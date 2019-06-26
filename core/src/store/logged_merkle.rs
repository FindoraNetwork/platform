//! # Logged Merkle Tree
//!
//! This module adds logging and snapshotting to the AppendOnlyMerkle
//! data structure.  The caller creates the tree and generates File
//! objects for the logs.  Thus, this module does not create or use
//! filesystem paths.  They are entirely the responsibility of the
//! caller.
//!
//! This module writes a log file that can be replayed to update an
//! AppendOnlyMerkle structure.  Each "append" invocation generates a
//! log entry in the file provided by caller.
//!
//! When a snapshot is requested, the current log file is flushed to
//! disk and the new file provided by the caller is used for all
//! subsequent logging.  Managing the log files is the responsibility
//! of the caller.  In addition to flushing the log, the snapshot
//! invocation flushes the AppendOnlyMerkle object as well, to create
//! a complete, consistent state on disk.
//!
//! A log file contains a sequence of log buffers.  Each log buffer
//! is a fixed size given by the constant BUFFER_SIZE, and contains
//! a header describing the contents of the buffer.  The log files
//! are append-only.  See the LogBuffer struct for the full details
//! on the contents of a log buffer.
//!
use super::append_only_merkle::{AppendOnlyMerkle, HashValue, Proof};
use sodiumoxide::crypto::hash::sha256;
use std::fs::File;
use std::io::BufWriter;
use std::io::Error;
use std::io::ErrorKind;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom::Current;
use std::io::SeekFrom::End;
use std::io::SeekFrom::Start;
use std::io::Write;
use std::slice::from_raw_parts;
use std::slice::from_raw_parts_mut;

// Returns Err(Error::new...).
macro_rules! ser {
    ($($x:tt)+) => { Err(Error::new(ErrorKind::Other, format!($($x)+))) }
}

// Writes a log entry when enabled.
macro_rules! log {
    ($c:tt, $($x:tt)+) => { }
    // ($c:tt, $($x:tt)+) => { println!($($x)+); }
}

const BUFFER_SIZE: usize = 32 * 1024;
const CHECK_SIZE: usize = 16;
const HASH_SIZE: usize = std::mem::size_of::<HashValue>();
const BUFFER_ENTRIES: u16 = ((BUFFER_SIZE / HASH_SIZE) - 1) as u16;
const BUFFER_MARKER: u32 = 0xabab_efe0;

/// This structure is used as the I/O buffer for the logs.  It consists
/// of a header and a series of HashValues passed to the "append"
/// procedure sequentially.  The header contains the transaction id
/// for the first hash, as assigned by the AppendOnlyMerkle object.
/// It also contains a checksum and a valid count, so that each block
/// is self-describing and can be checked for consistency.
///
/// This structure is built as a C structure to allow zero-copy I/O and
/// easier checksumming.  Currently, the checksum must be first to make
/// the as_checksummed_region function valid.  Likewise, the marker
/// field must follow the checksum.
///
/// A flush operation causes the writing of a block whether that
/// block is full or not, so any block in the can be only partially
/// full.  Partially full blocks are written as a full-size block with
/// some number of empty (zero) entries.  All blocks should have at
/// least one valid entry.
///
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

  // Convert "self" to a slice for I/O.
  fn as_mut_bytes(&mut self) -> &mut [u8] {
    unsafe {
      from_raw_parts_mut((self as *mut LogBuffer) as *mut u8,
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
  closed: bool,
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
                   tree: input_tree,
                   closed: false }
  }

  /// Append a hash value to the Merkle tree, returning the id if
  /// successful, and an error otherwise.
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
    if self.closed {
      return ser!("This LoggedMerkle object is closed.");
    }

    let id = self.tree.append_hash(hash)?;

    assert!(id == self.next_id);
    // TODO:  Handle assertion failures in some way.
    self.next_id += 1;

    self.buffer.hashes[self.buffer.valid as usize] = *hash;
    self.buffer.valid += 1;

    // If this buffer is full, give it to the BufWriter code.
    if self.buffer.valid == self.buffer.entry_count {
      self.write()?;
      assert!(self.buffer.id == self.next_id);
      assert!(self.buffer.valid == 0);
    }

    Ok(id)
  }

  /// Flush the current state to disk, generally for a snapshot.  It's
  /// valid to call this at any time, though.  The log and the tree will
  /// be preserved on disk with the state as of the current point in time.
  pub fn flush(&mut self) -> Result<(), Error> {
    if let Some(x) = self.tree.write() {
      return Err(x);
    }

    if self.buffer.valid > 0 {
      self.write()?;
    }

    self.writer.flush()?;
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

  /// Apply a log file to the tree.
  ///
  /// Argument
  ///
  /// `file` - a file containing a log
  ///
  /// This procedure returns the number of records successfully processed,
  /// which is useful mostly for statistics reporting.  Entries already
  /// in the tree are ignored.  Logs must be applied in order, from the
  /// oldest to the newest.  A log file that contains only transactions
  /// too new to append (beyond the end of the tree + 1)  will cause an error.
  ///
  pub fn apply_log(&mut self, mut file: File) -> Result<u64, Error> {
    let mut state = self.tree.total_size();
    let mut buffer = LogBuffer::new(0);
    let mut processed = 0;

    self.find_block(&mut file)?;

    // Loop reading buffers.  Return on EOF.
    loop {
      if let Err(x) = file.read_exact(buffer.as_mut_bytes()) {
        if x.kind() == ErrorKind::UnexpectedEof {
          break;
        }

        return Err(x);
      }

      // A buffer always should have some valid entries, but let such
      // errors pass for now.
      if buffer.valid == 0 {
        // TODO:  report an error.
        continue;
      }

      if buffer.id > self.state() {
        return ser!("This log file starts too far in the future.");
      }

      // If there are entries in the current buffer that are not in
      // the tree, process them.
      // TODO:  Consider checking any hashes that allegedly are in
      // the tree to see that they match...
      let mut current = buffer.id;

      if current <= state && current + buffer.valid as u64 > state {
        let start_offset = (state - current) as usize;

        current += start_offset as u64;

        // Insert all the hashes in this buffer not already present
        // in the tree.
        for index in start_offset..buffer.valid as usize {
          match self.tree.append_hash(&buffer.hashes[index]) {
            Ok(n) => {
              assert!(n == current);
            }
            Err(x) => {
              return Err(x);
            }
          }

          current += 1;
          processed += 1;
        }

        state = self.tree.total_size();
        assert!(state == current);
      }
    }

    Ok(processed)
  }

  // Find a block in the log file that has records just
  // past the end of the tree, if possible.
  fn find_block(&mut self, mut file: &mut File) -> Result<(), Error> {
    let state = self.tree.total_size();
    let file_size = self.file_size(&mut file)?;
    let buffer_count = file_size / BUFFER_SIZE as u64;

    if buffer_count == 0 {
      return Ok(());
    }

    let mut buffer = LogBuffer::new(0);
    let mut base = 0;
    let mut top = buffer_count;
    let mut current = base;

    log!(find_block, "find_block:  state {}, top {}", state, top);

    loop {
      file.read_exact(buffer.as_mut_bytes())?;

      log!(find_block, "current: {}, id: {}, state {}", current, buffer.id, state);

      if buffer.id > state {
        // The buffer is in the future!  Move back, if possible.
        let gap = current - base;

        if gap <= 1 {
          log!(find_block, "exit:  current {}, base {}", current, base);
          break;
        }

        top = current;
        current -= gap / 2;
        log!(find_block, "move back {} to {}", gap / 2, current);
      } else if buffer.id + buffer.valid as u64 <= state {
        // The buffer is in the past.  Move forward!
        let gap = top - current;

        if gap <= 1 {
          log!(find_block, "exit:  current {}, top {}", current, top);
          break;
        }

        base = current;
        current += gap / 2;
        log!(find_block, "move forward {} to {}", gap / 2, current);
      } else {
        log!(find_block, "found id {}, valid {}", buffer.id, buffer.valid);
        break;
      }

      file.seek(Start(current * BUFFER_SIZE as u64))?;
    }

    log!(find_block, "find_block:  return {}", current);
    file.seek(Start(current * BUFFER_SIZE as u64))?;
    Ok(())
  }

  fn file_size(&self, file: &mut File) -> Result<u64, Error> {
    let start = file.seek(Current(0))?;
    let size  = file.seek(End(0))?;
    file.seek(Start(start))?;
    Ok(size)
  }

  /// Close the LoggedMerkle object.
  pub fn close(&mut self) -> Result<(), Error> {
    self.flush()?;
    self.closed = true;
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

  #[cfg(test)]
  fn leaf(&self, id: u64) -> HashValue {
    self.tree.leaf(id as usize)
  }
}

impl Drop for LoggedMerkle {
  fn drop(&mut self) {
    let _ = self.flush();
  }
}

#[cfg(test)]
mod tests {
  extern crate core;

  use crate::store::append_only_merkle::AppendOnlyMerkle;
  use crate::store::append_only_merkle::HashValue;
  use crate::store::logged_merkle::LoggedMerkle;
  use std::cmp::max;
  use std::fs::OpenOptions;

  #[test]
  fn test_basic() {
    let tree_path = "logged_tree";
    let (mut logged, mut logs) = create_test_tree(&tree_path);

    // Append some transactions and make sure that things work.
    for tid in 0..2048 {
      let hash = test_hash(tid);
      let assigned = logged.append(&hash).unwrap();
      assert!(assigned == tid);

      // Create a few partial files as a test of snapshotting.
      if tid % 37 == 0 && tid < 300 {
        let log_path = tree_path.to_owned() + "-log-" + &logged.state().to_string();
        logs.push(log_path.clone());
        let writer = OpenOptions::new().write(true)
                                       .create(true)
                                       .open(log_path)
                                       .unwrap();

        if let Err(x) = logged.snapshot(writer) {
          panic!("snapshot failed:  {}", x);
        }
      }

      // Try a flush now and then.
      if tid % 63 == 0 {
        if let Err(x) = logged.flush() {
          panic!("flush failed:  {}", x);
        }
      }
    }

    // Generate a couple of proofs.
    let proof_id = logged.state() - 2;

    match logged.get_proof(proof_id, logged.state()) {
      Ok(proof) => {
        assert!(proof.tx_id == proof_id);
      }
      Err(x) => {
        panic!("get_proof failed:  {}", x);
      }
    }

    // A state of zero should request a proof at the
    // current tree state.
    match logged.get_proof(proof_id, 0) {
      Ok(proof) => {
        assert!(proof.tx_id == proof_id);
        assert!(proof.state == logged.state());
      }
      Err(x) => {
        panic!("get_proof failed at state zero:  {}", x);
      }
    }

    // Close the object, and then test that append fails.
    if let Err(x) = logged.close() {
      panic!("close failed:  {}", x);
    }

    if let Ok(_) = logged.append(&test_hash(1)) {
      panic!("append after close worked");
    }

    drop(logged);

    for path in logs.iter() {
      let _ = std::fs::remove_file(&path);
    }

    let _ = std::fs::remove_file(&tree_path);
    let _ = std::fs::remove_file(tree_path.to_owned() + ".1");
  }

  #[test]
  fn test_apply_log() {
    let offsets =
      [0, 1, 217, 1021, 1022, 1023, 1024, 4817, 2048, 8190, 8191, 8192, 8193, 8194, 16322];

    for offset in offsets.iter() {
      println!("\n ==== Testing offset {}", offset);
      generate_apply_log(8192, *offset);
    }

    let offsets =
      [0, 1024, 4817, 2048, 8190, 8191, 8192, 8193, 8194, 16322, 45600, 230000];

    for offset in offsets.iter() {
      println!("\n ==== Testing offset {}", offset);
      generate_apply_log(64 * 1024, *offset);
    }
  }

  fn generate_apply_log(tree_size: usize, offset: usize) {
    let tree_path = "apply_tree";
    let (mut logged, mut logs) = create_test_tree(&tree_path);
    let mut id = 1;

    // Generate the tree contents.
    for tid in 0..tree_size as u64 {
      let hash = test_hash(tid);
      let assigned = logged.append(&hash).unwrap();
      assert!(assigned == tid);

      if tid % 465 == 0 && tid > 2048 {
        let writer_path = tree_path.to_owned() + "-log-" + &id.to_string();
        let writer = OpenOptions::new().write(true)
                                       .create(true)
                                       .truncate(true)
                                       .open(&writer_path)
                                       .unwrap();
        logs.push(writer_path);

        if let Err(x) = logged.snapshot(writer) {
          panic!("snapshot failed:  {}", x);
        }

        id += 1;
      }
    }

    if let Err(x) = logged.flush() {
      panic!("snapshot failed:  {}", x);
    }

    // Generate the tree to which we will apply the logs.
    let new_tree_path = "logged_new_tree";
    let _ = std::fs::remove_file(&new_tree_path);
    let mut new_tree = AppendOnlyMerkle::create(&new_tree_path).unwrap();

    // Insert some number of records to simulate a non-empty tree.
    for i in 0..offset {
      if let Err(x) = new_tree.append_hash(&test_hash(i as u64)) {
        panic!("append failed:  {}", x);
      }
    }

    // Create the new LoggedMerkle structure.
    let new_log_path = new_tree_path.to_owned() + "-log-0";
    let writer = OpenOptions::new().write(true)
                                   .create(true)
                                   .truncate(true)
                                   .open(&new_log_path)
                                   .unwrap();

    let mut new_logged = LoggedMerkle::new(new_tree, writer);

    // Try a log file out of order, if there might be one in the future.
    if offset < logged.state() as usize {
      let index = if offset < 1023 { 1 } else { logs.len() - 1 };

      let log_file = OpenOptions::new().read(true).open(&logs[index]).unwrap();

      match new_logged.apply_log(log_file) {
        Err(x) => {
          if x.to_string() != "This log file starts too far in the future." {
            panic!("apply_log failed:  {}", x);
          }
        }
        Ok(_n) => {
          if new_logged.state() > logged.state() {
            panic!("apply_log didn't detect a log in the future.");
          }
        }
      }
    }

    // Now apply all the logs.
    let mut total = 0;

    for path in logs.clone() {
      println!("Processing {}", path);

      let log_file = OpenOptions::new().read(true).open(&path).unwrap();

      match new_logged.apply_log(log_file) {
        Err(x) => {
          panic!("apply_log failed:  {}", x);
        }
        Ok(n) => {
          println!("    Processed {} entries.", n);
          total += n;
        }
      }
    }

    println!("Processed {} hashes from the log files.", total);

    // Check that the resulting tree is the correct size.
    let expected = max(offset as u64, logged.state());

    if new_logged.state() != expected {
      panic!("The sizes don't match:  got {} vs {} expected",
             new_logged.state(),
             logged.state());
    }

    for i in 0..logged.state() {
      assert!(new_logged.leaf(i) == logged.leaf(i));
    }

    // Check the contents of the reconstructed tree.
    for i in logged.state()..offset as u64 {
      assert!(new_logged.leaf(i) == test_hash(i));
    }

    let _ = std::fs::remove_file(&new_log_path);
    let _ = std::fs::remove_file(&new_tree_path);
    let _ = std::fs::remove_file(new_tree_path.to_owned() + ".1");
    let _ = std::fs::remove_file(&tree_path);
    let _ = std::fs::remove_file(tree_path.to_owned() + ".1");

    for log in logs {
      let _ = std::fs::remove_file(&log);
    }
  }

  fn create_test_tree(tree_path: &str) -> (LoggedMerkle, Vec<String>) {
    let log_path = tree_path.to_owned() + "-log-0";
    let _ = std::fs::remove_file(&tree_path);
    let _ = std::fs::remove_file(&log_path);
    let mut logs = Vec::new();

    let tree = AppendOnlyMerkle::create(tree_path).unwrap();
    logs.push(log_path.clone());
    let writer = OpenOptions::new().write(true)
                                   .create(true)
                                   .open(log_path)
                                   .unwrap();
    let logged = LoggedMerkle::new(tree, writer);
    (logged, logs)
  }

  // Create an insertable test hash.  The hash value of
  // all zeros is reserved, so add 1 to the tid before
  // slapping it into the hash array.
  fn test_hash(tid: u64) -> HashValue {
    let mut result = HashValue::new();
    let mut index = 0;
    let mut id = tid + 1;

    while id > 0 {
      result.hash[index] = (id % 256) as u8;
      index += 1;
      id /= 256;
    }

    result
  }
}
