use super::append_only_merkle::{AppendOnlyMerkle, HashValue, Proof};
use sodiumoxide::crypto::hash::sha256;
use std::fs::File;
use std::io::BufWriter;
use std::io::Error;
use std::io::Write;
use std::slice::from_raw_parts;

const BUFFER_SIZE: usize = 32 * 1024;
const CHECK_SIZE: usize = 16;
const BUFFER_ENTRIES: u16 = ((BUFFER_SIZE / HASH_SIZE) - 1) as u16;
const BUFFER_MARKER: u32 = 0xabab_efe0;
const HASH_SIZE: usize = std::mem::size_of::<HashValue>();

// Need to derive Debug, Deserialize, and Serialize after implementing it for hashes.
//
// This structure is build as a C structure to allow zero-copy I/O.  The checksum
// must be first to make the as_checksummed_region function valid.  Likewise, the
// marker field must be next.
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
  fn new(next_id: u64) -> LogBuffer {
    LogBuffer { marker: BUFFER_MARKER,
                entry_count: BUFFER_ENTRIES,
                valid: 0,
                id: next_id,
                check: [0_u8; CHECK_SIZE],
                hashes: [HashValue::new(); BUFFER_ENTRIES as usize] }
  }

  fn as_bytes(&self) -> &[u8] {
    unsafe {
      from_raw_parts((self as *const LogBuffer) as *const u8,
                     std::mem::size_of::<LogBuffer>())
    }
  }

  fn as_checksummed_region(&self) -> &[u8] {
    unsafe {
      from_raw_parts((&self.marker as *const u32) as *const u8,
                     std::mem::size_of::<LogBuffer>() - CHECK_SIZE)
    }
  }

  fn set_checksum(&mut self) {
    let digest = sha256::hash(self.as_checksummed_region());
    self.check.clone_from_slice(&digest[0..CHECK_SIZE]);
  }
}

pub struct LoggedMerkle {
  io_errors: u64,
  writer: BufWriter<File>,
  buffer: LogBuffer,
  next_id: u64,
  tree: AppendOnlyMerkle,
}

impl LoggedMerkle {
  pub fn new(input_tree: AppendOnlyMerkle, file: File) -> LoggedMerkle {
    let id = input_tree.total_size();

    LoggedMerkle { io_errors: 0,
                   writer: BufWriter::new(file),
                   buffer: LogBuffer::new(id),
                   next_id: id,
                   tree: input_tree }
  }

  pub fn append(&mut self, hash: &HashValue) -> Result<u64, Error> {
    let id = self.tree.append_hash(hash)?;

    assert!(id == self.next_id);
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

  fn write(&mut self) -> Result<(), Error> {
    self.buffer.set_checksum();

    if let Err(x) = self.writer.write_all(self.buffer.as_bytes()) {
      self.io_errors += 1;
      return Err(x);
    }

    self.buffer = LogBuffer::new(self.next_id);
    Ok(())
  }

  pub fn flush(&mut self) -> Result<(), Error> {
    if let Some(x) = self.tree.write() {
      return Err(x);
    }

    if self.buffer.valid > 0 {
      self.write()?;
    }

    Ok(())
  }

  pub fn get_proof(&self, transaction: u64, state_in: u64) -> Result<Proof, Error> {
    let state = if state_in != 0 {
      state_in
    } else {
      self.tree.total_size()
    };

    self.tree.generate_proof(transaction, state)
  }

  pub fn snapshot(&mut self, file: File) -> Result<(), Error> {
    self.flush()?;
    self.buffer = LogBuffer::new(self.next_id);
    self.writer = BufWriter::new(file);
    Ok(())
  }

  pub fn state(&self) -> u64 {
    self.tree.total_size()
  }
}

impl Drop for LoggedMerkle {
  fn drop(&mut self) {
    let _ = self.flush();
  }
}
