//! # A Simple BitMap Implementation
//!
//! This module implements a simple persistent bitmap.  The
//! bitmap currently is maintained in a single file.  The
//! caller is responsible for file creation and open operations.
//!
//! The bitmap is maintained in memory and on disk as a sequence
//! of blocks.  Each block is self-identifying and checksummed
//! to help handle problems with storage systems.  The BitBlock
//! and BlockHeader structures implement the disk (and memory)
//! structure of the bitmap.
//!
//! This bitmap is intended for the ledger, so it allows the
//! caller to append set bits, but not zero bits, as a minor
//! check of correctness.
//!

extern crate time;

use super::append_only_merkle::timestamp;
use sodiumoxide::crypto::hash::sha256;
use std::fs::File;
use std::io::Error;
use std::io::ErrorKind;
use std::io::Read;
use std::io::Result;
use std::io::Seek;
use std::io::SeekFrom::End;
use std::io::SeekFrom::Start;
use std::io::Write;
use std::mem;
use std::slice::from_raw_parts;
use std::slice::from_raw_parts_mut;

// Returns Err(Error::new...).
macro_rules! se {
    ($($x:tt)+) => { Err(Error::new(ErrorKind::Other, format!($($x)+))) }
}

// Write a log entry to stdout.
macro_rules! log {
  ($($x:tt)+) => { println!("{}    {}", timestamp(), format!($($x)+)); }
}

// Write a debug entry to stdout.
//
// This macro is used only for debugging simple problems
// with the basic mapping logic.
macro_rules! debug {
  ($($x:tt)+) => {}; // ($($x:tt)+) => { println!("{}    {}", timestamp(), format!($($x)+)); }
}

const CHECK_SIZE: usize = 16;

#[repr(C)]
#[derive(PartialEq)]
struct CheckBlock {
  bytes: [u8; CHECK_SIZE],
}

// A structure for a checksum on a block.
impl CheckBlock {
  fn new() -> CheckBlock {
    CheckBlock { bytes: [0_u8; CHECK_SIZE] }
  }
}

const HEADER_MAGIC: u32 = 0x0204_0600;
const BIT_INVALID: u16 = 0; // This value is used for testing.
const BIT_ARRAY: u16 = 1;
const BIT_DESC: u16 = 2;
const HEADER_SIZE: usize = 48;

// Define the layout for a block header.
//
// This structure occupies the first bytes of each disk
// block.  It also is maintained in memory.
//
// checksum  a checksum over the rest of the block
// magic     a magic number
// count     the count of valid bits in this block
// bit_id    the bit index corresponding to the first bit in the block
// offset    the offset in the file at which this block should appear
// contents  the contents type, currently always BIT_ARRAY
//
// The size of this structure must match the HEADER_SIZE
// constant.

#[repr(C)]
struct BlockHeader {
  checksum: CheckBlock, // must be first
  magic: u32,           // must be second
  count: u32,
  bit_id: u64,
  offset: u64,
  contents: u16,
  pad_1: u16,
  pad_2: u32,
}

impl BlockHeader {
  // Create a new block header.
  fn new(block_contents: u16, block_id: u64) -> Result<BlockHeader> {
    if block_contents != BIT_ARRAY && block_contents != BIT_DESC {
      return se!("That content type ({}) is invalid.", block_contents);
    }

    let result = BlockHeader { checksum: CheckBlock::new(),
                               magic: HEADER_MAGIC,
                               count: 0,
                               bit_id: block_id,
                               offset: block_id,
                               contents: block_contents,
                               pad_1: 0,
                               pad_2: 0 };

    Ok(result)
  }

  // Validate the contents of a block header, except for
  // the checksum, which might not be valid yet.
  fn validate(&self, contents: u16, id: u64) -> Result<()> {
    if self.magic != HEADER_MAGIC {
      return se!("Block {} has a bad magic number:  {:x}", id, self.magic);
    }

    if self.count > BLOCK_BITS as u32 {
      return se!("Block {} has a bad count:  {} vs {}",
                 id,
                 self.count,
                 BLOCK_BITS);
    }

    if self.bit_id != id {
      return se!("Block {} has a bad id:  the disk said {}.", id, self.bit_id);
    }

    if self.contents != contents {
      return se!("Block {} has a bad contents type:  {}", id, self.contents);
    }

    if self.pad_1 != 0 {
      return se!("Block {} has an invalid pad_1:  {}", id, self.pad_1);
    }

    if self.pad_2 != 0 {
      return se!("Block {} has an invalid pad_2:  {}", id, self.pad_2);
    }

    Ok(())
  }
}

const BLOCK_SIZE: usize = 32 * 1024;
const BITS_SIZE: usize = BLOCK_SIZE - HEADER_SIZE;
const BLOCK_BITS: usize = BITS_SIZE * 8;

// Define the layout of a block of a bitmap.  The on-disk
// and in-memory layouts are the same.
#[repr(C)]
struct BitBlock {
  header: BlockHeader,
  bits: [u8; BITS_SIZE],
}

impl BitBlock {
  // Create a new block header structure.
  fn new(block_contents: u16, block_id: u64) -> Result<BitBlock> {
    let result = BitBlock { header: BlockHeader::new(block_contents, block_id)?,
                            bits: [0_u8; BITS_SIZE] };

    Ok(result)
  }

  // Compute a checksum for the block.
  fn compute_checksum(&self) -> [u8; CHECK_SIZE] {
    let digest = sha256::hash(self.as_checksummed_region());
    let mut result: [u8; CHECK_SIZE] = Default::default();

    result.clone_from_slice(&digest[0..CHECK_SIZE]);
    result
  }

  // Set the block check bits with the current checksum for the block.
  fn set_checksum(&mut self) {
    self.header.checksum.bytes = self.compute_checksum();
  }

  // Create a slice for writing a block to disk.
  fn as_ref(&self) -> &[u8] {
    unsafe {
      from_raw_parts((self as *const BitBlock) as *const u8,
                     mem::size_of::<BitBlock>())
    }
  }

  // Create a mutable slice for reading a block from disk.
  fn as_mut(&mut self) -> &mut [u8] {
    unsafe {
      from_raw_parts_mut((self as *mut BitBlock) as *mut u8,
                         mem::size_of::<BitBlock>())
    }
  }

  // Create a slice corresponding to the part of the block
  // that is checksummed.
  fn as_checksummed_region(&self) -> &[u8] {
    unsafe {
      from_raw_parts((&self.header.magic as *const u32) as *const u8,
                     mem::size_of::<BitBlock>() - mem::size_of::<CheckBlock>())
    }
  }

  // Validate the contents of a block from the disk.  Here
  // we validate the checksum, since it should be set when
  // a block is sent to disk.
  fn validate(&self, contents: u16, id: u64) -> Result<()> {
    self.header.validate(contents, id)?;

    let checksum = self.compute_checksum();

    if self.header.checksum.bytes != checksum {
      return se!("Block {} has a bad checksum.", id);
    }

    Ok(())
  }
}

/// Define the structure for controlling a persistent bitmap.
pub struct BitMap {
  file: File,
  size: usize,
  blocks: Vec<BitBlock>,
  dirty: Vec<i64>, // the modification time for the block, or zero
  set: Vec<u32>,
  map: [u8; 256],
}

impl Drop for BitMap {
  fn drop(&mut self) {
    let _ = self.write();
  }
}

fn time() -> i64 {
  time::now().to_timespec().sec
}

fn count_bits(bits: &[u8], map: [u8; 256]) -> u32 {
  let mut result: u32 = 0;

  for i in 0..bits.len() {
    result += map[bits[i] as usize] as u32;
  }

  result
}

fn create_map() -> [u8; 256] {
  let mut result = [0_u8; 256];

  for i in 0..256 {
    result[i] = count_byte(i);
  }

  result
}

fn count_byte(mask: usize) -> u8 {
  let mut result = 0;

  for i in 0..8 {
    result += if mask & (1 << i) == 0 { 0 } else { 1 };
  }

  result
}

impl BitMap {
  /// Create a new bit map.  The caller should pass a File
  /// structure opened to an empty file.
  ///
  /// # Example
  ///````
  /// use std::fs::OpenOptions;
  /// use crate::core::store::bitmap::BitMap;
  ///
  /// let path = "sample_create_name";
  ///
  /// # let _ = std::fs::remove_file(&path);
  /// let file =
  ///   OpenOptions::new()
  ///     .read(true)
  ///     .write(true)
  ///     .create_new(true)
  ///     .open(&path)
  ///     .unwrap();
  ///
  /// let mut bitmap =
  ///   match BitMap::create(file) {
  ///     Ok(bitmap) => { bitmap }
  ///     Err(e) => { panic!("create failed:  {}", e); }
  ///   };
  ///
  /// bitmap.set(0);
  ///
  /// if let Err(e) = bitmap.write() {
  ///   panic!("Write failed:  {}", e);
  /// }
  /// # let _ = std::fs::remove_file(&path);
  ///````
  pub fn create(mut data: File) -> Result<BitMap> {
    let file_size = data.seek(End(0))?;

    if file_size != 0 {
      return se!("The file contains data!");
    }

    let result = BitMap { file: data,
                          size: 0,
                          blocks: Vec::new(),
                          dirty: Vec::new(),
                          set: Vec::new(),
                          map: create_map() };

    Ok(result)
  }

  /// Open an existing bitmap.  The caller is responsible
  /// for opening the file.
  ///
  /// # Example
  ///````
  /// use std::fs::OpenOptions;
  /// use crate::core::store::bitmap::BitMap;
  ///
  /// let path = "sample_open_name";
  ///
  /// # let _ = std::fs::remove_file(&path);
  /// # let file =
  /// #   OpenOptions::new()
  /// #     .read(true)
  /// #     .write(true)
  /// #     .create_new(true)
  /// #     .open(&path)
  /// #     .unwrap();
  /// # drop(file);
  /// let file =
  ///   OpenOptions::new()
  ///     .read(true)
  ///     .write(true)
  ///     .open(&path)
  ///     .unwrap();
  ///
  /// let mut bitmap =
  ///   match BitMap::open(file) {
  ///     Ok(bitmap) => { bitmap }
  ///     Err(e) => { panic!("open failed:  {}", e); }
  ///   };
  ///
  /// bitmap.set(0);
  /// bitmap.set(1);
  /// bitmap.set(2);
  /// bitmap.clear(1);
  ///
  /// if let Err(e) = bitmap.write() {
  ///   panic!("Write failed:  {}", e);
  /// }
  /// # let _ = std::fs::remove_file(&path);
  ///````
  pub fn open(mut data: File) -> Result<BitMap> {
    let (count, block_vector, state_vector, set_vector) = BitMap::read_file(&mut data)?;

    let result = BitMap { file: data,
                          size: count,
                          blocks: block_vector,
                          dirty: state_vector,
                          set: set_vector,
                          map: create_map() };

    assert!(result.validate());
    Ok(result)
  }

  // Read the contents of a file into memory, checking the
  // validity as we go.
  fn read_file(file: &mut File) -> Result<(usize, Vec<BitBlock>, Vec<i64>, Vec<u32>)> {
    let mut blocks = Vec::new();
    let mut dirty = Vec::new();
    let mut set = Vec::new();
    let mut count = 0;

    // Get a map to convert a byte value to a set bit count.
    let map = create_map();

    // Compute the number of blocks in the file.
    let file_size = file.seek(End(0))?;

    if file_size % BLOCK_SIZE as u64 != 0 {
      return se!("That file size ({}) is invalid.", file_size);
    }

    file.seek(Start(0))?;
    let total_blocks = file_size / BLOCK_SIZE as u64;

    // Reserve space in our vectors.
    blocks.reserve(total_blocks as usize);
    dirty.reserve(total_blocks as usize);

    // Read each block.
    for index in 0..total_blocks {
      let mut block = BitBlock::new(BIT_ARRAY, 0).unwrap();

      block.header.contents = BIT_INVALID;

      match file.read_exact(block.as_mut()) {
        Ok(_) => {
          block.validate(BIT_ARRAY, index)?;

          if index != total_blocks - 1 && block.header.count != BLOCK_BITS as u32 {
            return se!("Block {} is not full:  count {}", index, block.header.count);
          }

          let set_count = count_bits(&block.bits, map);
          count += block.header.count as usize;
          blocks.push(block);
          dirty.push(0 as i64);
          set.push(set_count);
        }
        Err(e) => {
          return Err(e);
        }
      }
    }

    Ok((count, blocks, dirty, set))
  }

  fn validate_count(&self, index: usize) -> bool {
    let result = count_bits(&self.blocks[index].bits, self.map) == self.set[index];

    if !result {
      log!("The count at block {} is {}, but should be {}",
           index,
           self.set[index],
           count_bits(&self.blocks[index].bits, self.map));
    }

    result
  }

  fn validate(&self) -> bool {
    let mut pass = true;

    for i in 0..self.blocks.len() {
      pass &= self.validate_count(i);
    }

    pass
  }

  /// Query the value of a bit in the map.
  pub fn query(&self, bit: usize) -> Result<bool> {
    if bit >= self.size {
      return se!("That index is out of range ({} vs {}).", bit, self.size);
    }

    let block = bit / BLOCK_BITS;
    let bit_id = bit % BLOCK_BITS;
    let index = bit_id / 8;
    let mask = 1 << (bit_id % 8);

    debug!("query({}) -> block {}, index {}, mask {}",
           bit, block, index, mask);
    let value = self.blocks[block].bits[index] & mask;
    Ok(value != 0)
  }

  /// Append a set bit, and return the index on success.
  pub fn append(&mut self) -> Result<u64> {
    let bit = self.size;

    if let Err(e) = self.mutate(bit, 1, true) {
      return Err(e);
    }

    Ok(bit as u64)
  }

  /// Set the given bit.
  pub fn set(&mut self, bit: usize) -> Result<()> {
    if bit > self.size {
      return se!("That index is too large to set ({} vs {}).", bit, self.size);
    }

    self.mutate(bit, 1, true)
  }

  /// Clear the given bit.
  pub fn clear(&mut self, bit: usize) -> Result<()> {
    if bit >= self.size {
      return se!("That index is too large to clear ({} vs {}).",
                 bit,
                 self.size);
    }

    self.mutate(bit, 0, false)
  }

  // Change the value of the given bit, as requested.
  fn mutate(&mut self, bit: usize, value: u8, extend: bool) -> Result<()> {
    if !extend && bit >= self.size {
      return se!("That index ({}) is out of the range [0, {}).",
                 bit,
                 self.size);
    }

    // Compute the various indices.
    let block = bit / BLOCK_BITS;
    let bit_id = bit % BLOCK_BITS;
    let index = bit_id / 8;
    let mask_shift = bit_id % 8;
    let mask = 1 << mask_shift;

    // We might need to create a new block.  If so,
    // push the new block and the dirty flag for it.
    if block >= self.blocks.len() {
      self.blocks.push(BitBlock::new(BIT_ARRAY, block as u64)?);
      self.dirty.push(time());
      self.set.push(0);
    } else {
      self.dirty[block] = time();
    }

    let mutate = if bit >= self.size {
      true
    } else {
      self.blocks[block].bits[index] & mask != value << mask_shift
    };

    debug!("mutate(bit = {}, value = {}, {}) -> block {}, index {}, mask {}, BLOCK_BITS {}, mutate {}",
           bit, value, extend, block, index, mask, BLOCK_BITS, mutate);

    if !mutate {
      return Ok(());
    }

    if value == 0 {
      self.blocks[block].bits[index] &= !mask;
      self.set[block] -= 1;
    } else {
      self.blocks[block].bits[index] |= mask;
      self.set[block] += 1;
    }

    // If we are extending the bit map, update all the various
    // counters.  Also, write the block if it is now full.  This
    // heuristics might help reduce long pauses for write()
    // operations.

    if bit >= self.size {
      self.size = bit + 1;
      self.blocks[block].header.count += 1;

      if self.blocks[block].header.count == BLOCK_BITS as u32 {
        if let Err(e) = self.write_block(block) {
          log!("Error writing block {}:  {}", block, e);
        }
      }
    }

    Ok(())
  }

  /// Return the number of bits in the map.
  pub fn size(&self) -> usize {
    self.size
  }

  /// Write the bitmap to disk.
  pub fn write(&mut self) -> Result<()> {
    for i in 0..self.blocks.len() {
      if self.dirty[i] != 0 {
        self.write_block(i)?;
      }
    }

    self.file.sync_all()?;
    Ok(())
  }

  /// Flush buffers that haven't been modified in "age" seconds
  /// to the operating system.  The write() method must be invoked
  /// if the caller wants a guarantee that the data has been moved
  /// to persistent store.
  pub fn flush_old(&mut self, age: i64) -> Result<()> {
    let now = time();

    for i in 0..self.blocks.len() {
      if self.dirty[i] <= now - age {
        self.write_block(i)?;
      }
    }

    Ok(())
  }

  // Write the given block to disk and clear the dirty flag.
  fn write_block(&mut self, index: usize) -> Result<()> {
    let offset = index as u64 * BLOCK_SIZE as u64;
    self.file.seek(Start(offset))?;
    self.blocks[index].set_checksum();
    self.file.write_all(self.blocks[index].as_ref())?;
    self.dirty[index] = 0;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;
  use std::fs::OpenOptions;
  use std::mem;

  #[test]
  fn test_header() {
    assert!(mem::size_of::<BlockHeader>() == HEADER_SIZE);

    let id = 24_000;
    let mut header = BlockHeader::new(BIT_ARRAY, id).unwrap();
    assert!(header.contents == BIT_ARRAY);
    assert!(header.bit_id == id);

    if let Err(e) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed:  {}", e);
    }

    header.magic ^= 1;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad magic number.");
    }

    header.magic ^= 1;

    header.count = (BLOCK_BITS + 1) as u32;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad count.");
    }

    header.count = 0;
    header.bit_id ^= 1;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad id.");
    }

    header.bit_id ^= 1;
    header.contents = BIT_INVALID;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad contents type.");
    }

    header.contents = BIT_ARRAY;
    header.pad_1 = 1;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad pad_1.");
    }

    header.pad_1 = 0;
    header.pad_2 = 1;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad pad_2.");
    }

    let header = BlockHeader::new(BIT_DESC, 0).unwrap();
    assert!(header.contents == BIT_DESC);
    assert!(header.bit_id == 0);

    assert!(header.count == 0);
    assert!(header.checksum == CheckBlock::new());

    if let Ok(_) = BlockHeader::new(BIT_INVALID, 0) {
      panic!("An invalid block type was accepted.");
    }
  }

  #[test]
  fn test_block() {
    println!("The block size is {}.", mem::size_of::<BitBlock>());
    println!("The header size is {}.", mem::size_of::<BlockHeader>());
    assert!(mem::size_of::<BlockHeader>() == HEADER_SIZE);
    assert!(mem::size_of::<BitBlock>() == BLOCK_SIZE);
    assert!(BLOCK_SIZE == BITS_SIZE + HEADER_SIZE);
    assert!(BLOCK_SIZE == BLOCK_BITS / 8 + HEADER_SIZE);

    let mut block = BitBlock::new(BIT_DESC, 32).unwrap();
    assert!(block.header.contents == BIT_DESC);
    assert!(block.header.bit_id == 32);

    block.set_checksum();

    if let Err(_) = block.validate(BIT_DESC, 32) {
      panic!("Block validation failed.");
    }

    block.header.checksum.bytes[0] ^= 1;

    if let Ok(_) = block.validate(BIT_DESC, 32) {
      panic!("Block validation didn't detect a bad checksum.");
    }
  }

  #[test]
  fn test_basic_bitmap() {
    log!("Run the basic bitmap test.");
    let path = "basic_bitmap";
    let _ = fs::remove_file(&path);

    let file = OpenOptions::new().read(true)
                                 .write(true)
                                 .create_new(true)
                                 .open(&path)
                                 .unwrap();

    let mut bitmap = BitMap::create(file).unwrap();

    if let Err(e) = bitmap.write() {
      panic!("Write failed:  {}", e);
    }

    if let Ok(_) = bitmap.set(1) {
      panic!("set worked with an invalid index.");
    }

    assert!(bitmap.validate());
    drop(bitmap);

    let file = OpenOptions::new().read(true)
                                 .write(true)
                                 .open(&path)
                                 .unwrap();

    let mut bitmap = BitMap::open(file).unwrap();
    assert!(bitmap.validate());

    for i in 0..2 * BLOCK_BITS + 2 {
      bitmap.set(i).unwrap();
      assert!(bitmap.query(i).unwrap() == true);
      assert!(bitmap.size() == i + 1);

      if let Ok(_) = bitmap.query(i + 1) {
        panic!("Index {} should be out of range.", i + 1);
      }

      if i % 4096 == 1 {
        println!("Validating the count at {}", i);
        assert!(bitmap.validate());
      }

      // Try a flush now and then.
      if i % (BLOCK_BITS / 2) == 0 {
        if let Err(e) = bitmap.flush_old(0) {
          panic!("flush_old failed:  {}", e);
        }
      }
    }

    for i in 0..bitmap.size() {
      if i & 1 == 0 {
        bitmap.clear(i).unwrap();
        assert!(bitmap.query(i).unwrap() == false);

        if i % 4096 == 0 {
          assert!(bitmap.validate());
        }
      }
    }

    for i in 0..bitmap.size() {
      assert!(bitmap.query(i).unwrap() == !(i & 1 == 0));
    }

    if let Err(_) = bitmap.write() {
      panic!("write failed.");
    }

    let bits_initialized = bitmap.size();

    if let Err(e) = bitmap.write() {
      panic!("write failed:  {}", e);
    }

    if let Err(e) = bitmap.flush_old(0) {
      panic!("flush_old failed:  {}", e);
    }

    assert!(bitmap.validate());
    drop(bitmap);

    let file = OpenOptions::new().read(true)
                                 .write(true)
                                 .open(&path)
                                 .unwrap();

    let mut bitmap = BitMap::open(file).unwrap();
    assert!(bits_initialized == bitmap.size());
    assert!(bits_initialized % BLOCK_BITS != 0);
    assert!(bitmap.validate());

    for i in 0..bits_initialized {
      assert!(bitmap.query(i).unwrap() == !(i & 1 == 0));

      if i % BLOCK_BITS == 0 {
        assert!(bitmap.validate());
      }
    }

    let bit = bitmap.append().unwrap();
    assert!(bit == bits_initialized as u64);
    assert!(bitmap.validate());

    let bit = bitmap.append().unwrap();
    assert!(bit == bits_initialized as u64 + 1);
    assert!(bitmap.validate());

    println!("Test code completed -- drop next");
    drop(bitmap);

    let _ = fs::remove_file(&path);
  }

  #[test]
  fn test_counting() {
    let map = create_map();

    for i in 0..8 {
      let value = 1 << i;
      println!("count_byte({}) = {}, map[{}] = {}",
               value,
               count_byte(value),
               value,
               map[value]);
      assert!(count_byte(value) == 1);
      assert!(map[value] == 1);
    }

    for i in 2..9 {
      let value = (1 << i) - 1;

      println!("count_byte({}) = {}, map[{}] = {}",
               value,
               count_byte(value),
               value,
               map[value]);
      assert!(count_byte(value) == i);
      assert!(map[value] == i);
    }

    for i in 1..8 {
      let value = (1 << i) + (1 << (i - 1));

      println!("count_byte({}) = {}, map[{}] = {}",
               value,
               count_byte(value),
               value,
               map[value]);
      assert!(count_byte(value) == 2);
      assert!(map[value] == 2);
    }
  }
}
