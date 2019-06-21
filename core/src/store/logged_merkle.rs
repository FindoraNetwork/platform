//! # Logged Merkle Tree
//!
//! This module adds logging and snapshotting to the AppendOnlyMerkle
//! data structure.  The caller creates the tree and generates File
//! objects for the logs.  Thus, this module does not create or use
//! filesystem paths.  They are entirely the responsibility of the caller.
//!
//! This module writes a log file that can be replayed to update an
//! AppendOnlyMerkle structure.  Each "append" invocation generates a
//! log entry in the file provided by caller.
//!
//! When a snapshot is requested, the current log file is flushed to disk
//! and the new file provided by the caller is used for all subsequent
//! logging.  Managing the log files is the responsibility of the caller.
//! In addition to flushing the log, the snapshot invocation flushes
//! the AppendOnlyMerkle object, as well.

use super::append_only_merkle::{AppendOnlyMerkle, HashValue, Proof};
use sodiumoxide::crypto::hash::sha256;
use std::fs::File;
use std::io::BufWriter;
use std::io::Error;
use std::io::Write;
use std::slice::from_raw_parts;

const BUFFER_SIZE: usize = 32 * 1024;
const CHECK_SIZE: usize = 16;
const HASH_SIZE: usize = std::mem::size_of::<HashValue>();
const BUFFER_ENTRIES: u16 = ((BUFFER_SIZE / HASH_SIZE) - 1) as u16;
const BUFFER_MARKER: u32 = 0xabab_efe0;

/// This structure is used as the I/O buffer for the logs.  It consists
/// of a header and a series of HashValues passed to the "append" procedure
/// sequentially.  The header contains the transaction id for the first
/// hash, as assigned by the AppendOnlyMerkle object.  It also contains
/// a checksum and a valid count, so that each block is self-describing
/// and can be checked for consistency.
///
/// This structure is built as a C structure to allow zero-copy I/O and
/// easier checksumming.  Currently, the checksum must be first to make
/// the as_checksummed_region function valid.  Likewise, the marker field
/// must be next.
#[derive(Copy, Clone)]
#[repr(C)]
struct LogBuffer {
  check: [u8; CHECK_SIZE], // This entry must be first.
  marker: u32,             // This entry must be second.
  entry_count: u16,
  valid: u16,
  id: u64,
  hashes: [HashValue; BUFFER_ENTRIES as usize],
}

impl LogBuffer {
  // Create a new buffer.
  fn new(next_id: u64) -> LogBuffer {
    assert!(std::mem::size_of::<LogBuffer>() == BUFFER_SIZE);

    LogBuffer { marker: BUFFER_MARKER,
                entry_count: BUFFER_ENTRIES,
                valid: 0,
                id: next_id,
                check: [0_u8; CHECK_SIZE],
                hashes: [HashValue::new(); BUFFER_ENTRIES as usize] }
  }

  // Convert "self" to a slice for I/O.
  fn as_bytes(&self) -> &[u8] {
    unsafe {
      from_raw_parts((self as *const LogBuffer) as *const u8,
                     std::mem::size_of::<LogBuffer>())
    }
  }

  // Get a slice corresponding to the checksummed region.
  fn as_checksummed_region(&self) -> &[u8] {
    unsafe {
      from_raw_parts((&self.marker as *const u32) as *const u8,
                     std::mem::size_of::<LogBuffer>() - CHECK_SIZE)
    }
  }

  // Generate and set the checksum for this buffer.
  fn set_checksum(&mut self) {
    let digest = sha256::hash(self.as_checksummed_region());
    self.check.clone_from_slice(&digest[0..CHECK_SIZE]);
  }
}

/// This structure provides a logging and snapshot interface for
/// an underlying AppendOnlyMerkle tree.
pub struct LoggedMerkle {
  io_errors: u64,
  writer: BufWriter<File>,
  buffer: LogBuffer,
  next_id: u64,
  tree: AppendOnlyMerkle,
}

impl LoggedMerkle {
  /// Create a new logged Merkle tree object
  ///
  /// # Arguments
  ///
  /// * `tree` - an AppendOnlyMerkle object
  /// * `file` - a File for the log
  ///
  /// # Example
  ///````
  /// use crate::core::store::append_only_merkle::AppendOnlyMerkle;
  /// use crate::core::store::logged_merkle::LoggedMerkle;
  /// use std::fs::OpenOptions;
  ///
  /// let tree_path = "new_logged";
  /// let log_path = "new_logged-log.1";
  ///
  /// # let _ = std::fs::remove_file(&tree_path);
  /// # let _ = std::fs::remove_file(&log_path);
  /// let tree = AppendOnlyMerkle::create(&tree_path).unwrap();
  /// let file = OpenOptions::new().write(true).create(true).open(log_path).unwrap();
  /// let logged = LoggedMerkle::new(tree, file);
  /// println!("The tree state is {}", logged.state());
  /// # drop(logged);
  /// # let _ = std::fs::remove_file(&tree_path);
  /// # let _ = std::fs::remove_file(&log_path);
  ///````
  pub fn new(input_tree: AppendOnlyMerkle, file: File) -> LoggedMerkle {
    let id = input_tree.total_size();

    LoggedMerkle { io_errors: 0,
                   writer: BufWriter::new(file),
                   buffer: LogBuffer::new(id),
                   next_id: id,
                   tree: input_tree }
  }

  /// Append a hash value to the Merkle tree, returning the id
  /// if successful, and an error otherwise.
  ///
  /// # Argument
  ///
  /// `hash` - the hash value to append
  ///
  /// # Example
  ///
  /// let merkle_id =
  ///   match logged_merkle.append(&hash) {
  ///     Ok(id) => { id }
  ///     Err(x) => { return Err(x); }
  ///   };
  pub fn append(&mut self, hash: &HashValue) -> Result<u64, Error> {
    let id = self.tree.append_hash(hash)?;

    assert!(id == self.next_id);
    // TODO:  Handle assertion failures in some way.
    self.next_id += 1;

    self.buffer.hashes[self.buffer.valid as usize] = *hash;
    self.buffer.valid += 1;

    if self.buffer.valid == self.buffer.entry_count {
      self.write()?;
      assert!(self.buffer.id == self.next_id);
      assert!(self.buffer.valid == 0);
    }

    Ok(id)
  }

  /// Flush the current state to disk, generally for a snapshot.
  pub fn flush(&mut self) -> Result<(), Error> {
    if let Some(x) = self.tree.write() {
      return Err(x);
    }

    if self.buffer.valid > 0 {
      self.write()?;
    }

    Ok(())
  }

  /// Get a proof for the given transaction id from the underlying
  /// AppendOnlyMerkle object.
  ///
  /// # Arguments
  ///
  /// `transaction` - the Merkle tree id for the transaction
  /// `state_in` - the Merkle tree state for which the proof is wanted,
  ///              or zero, for the current state.
  pub fn get_proof(&self, transaction: u64, state_in: u64) -> Result<Proof, Error> {
    let state = if state_in != 0 {
      state_in
    } else {
      self.tree.total_size()
    };

    self.tree.generate_proof(transaction, state)
  }

  /// Generate a snapshot by flushing the current state to disk,
  /// and starting a new log to the given file.
  ///
  /// # Argument
  ///
  /// `file` - a file to which to write the log
  pub fn snapshot(&mut self, file: File) -> Result<(), Error> {
    self.flush()?;
    self.buffer = LogBuffer::new(self.next_id);
    self.writer = BufWriter::new(file);
    Ok(())
  }

  /// Return the current state of the underlying Merkle tree.
  /// This id currently is used to generate a log file name.
  pub fn state(&self) -> u64 {
    self.tree.total_size()
  }

  // Write the log buffer to the file, returning errors as needed.
  // When the write is done, create a new buffer.
  fn write(&mut self) -> Result<(), Error> {
    self.buffer.set_checksum();

    if let Err(x) = self.writer.write_all(self.buffer.as_bytes()) {
      self.io_errors += 1;
      return Err(x);
    }

    self.buffer = LogBuffer::new(self.next_id);
    Ok(())
  }
}

impl Drop for LoggedMerkle {
  fn drop(&mut self) {
    let _ = self.flush();
  }
}
