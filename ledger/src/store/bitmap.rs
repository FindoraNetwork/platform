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
use sodiumoxide::crypto::hash::sha256::Digest;
use sodiumoxide::crypto::hash::sha256::DIGESTBYTES;
use std::cmp::min;
use std::collections::HashMap;
use std::collections::HashSet;
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

// Constants for calling serialize_block.
// const HEADER_ONLY: bool = false;
const INCLUDE_BITS: bool = true;

const CHECK_SIZE: usize = 16;

#[repr(C)]
#[derive(Copy, Clone, Debug, Default, PartialEq)]
struct CheckBlock {
  bytes: [u8; CHECK_SIZE],
}

// A structure for a checksum on a block.  We use
// the first CHECK_SIZE bytes of the sha256 digest
// as the checksum.
impl CheckBlock {
  fn new() -> CheckBlock {
    CheckBlock { bytes: [0_u8; CHECK_SIZE] }
  }
}

const HEADER_MAGIC: u32 = 0x0204_0600;
const HEADER_SIZE: usize = 48;

// Define the types of headers.
const BIT_INVALID: u16 = 0; // This value is used for testing.
const BIT_ARRAY: u16 = 1;
const BIT_DESC_SET: u16 = 2;
const BIT_DESC_CLEAR: u16 = 3;
const BIT_HEADER: u16 = 4;

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

// For users who download the bitmap, this is what they
// get.
#[derive(Debug, Default)]
#[repr(C)]
pub struct BlockInfo {
  magic: u32, // Should be first to allow changes...
  count: u32,
  bit_id: u64,
  pad_1: u16,
  contents: u16,
  list_size: u32,
  checksum: CheckBlock,
}

impl BlockInfo {
  fn validate(&self) -> Result<()> {
    if self.magic != HEADER_MAGIC {
      return se!("Invalid magic value {:x}", self.magic);
    }

    if self.count > BLOCK_BITS as u32 {
      return se!("Invalid count {}", self.count);
    }

    if self.list_size as usize > BITS_SIZE / INDEX_SIZE {
      return se!("Invalid list size {}", self.list_size);
    }

    Ok(())
  }
}

impl BlockInfo {
  fn as_ref(&self) -> &[u8] {
    unsafe {
      from_raw_parts((self as *const BlockInfo) as *const u8,
                     mem::size_of::<BlockInfo>())
    }
  }

  fn as_mut(&mut self) -> &mut [u8] {
    unsafe {
      from_raw_parts_mut((self as *mut BlockInfo) as *mut u8,
                         mem::size_of::<BlockInfo>())
    }
  }
}

const BLOCK_INFO_SIZE: usize = mem::size_of::<BlockInfo>();

impl BlockHeader {
  // Create a new block header.
  fn new(block_contents: u16, block_id: u64) -> Result<BlockHeader> {
    if block_contents != BIT_ARRAY
       && block_contents != BIT_DESC_SET
       && block_contents != BIT_DESC_CLEAR
       && block_contents != BIT_HEADER
    {
      return se!("That content type ({}) is invalid.", block_contents);
    }

    let result = BlockHeader { checksum: CheckBlock::new(),
                               magic: HEADER_MAGIC,
                               count: 0,
                               bit_id: block_id * BLOCK_BITS as u64,
                               offset: block_id,
                               contents: block_contents,
                               pad_1: 0,
                               pad_2: 0 };

    Ok(result)
  }

  // Validate the contents of a block header, except for
  // the checksum, which might not be set yet.
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

    if self.bit_id != id * BLOCK_BITS as u64 {
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

// Define the tradeoff points for switching between compression
// modes.  The bits are stored as a full bit map, or a list of
// bits.  A list of bits can be those bits that are clear or
// those that are set.
const INDEX_SIZE: usize = 3;
const LOWER_LIMIT: u32 = BITS_SIZE as u32 / INDEX_SIZE as u32;
const UPPER_LIMIT: u32 = (BLOCK_BITS - BITS_SIZE / INDEX_SIZE) as u32;

// Define the layout of a block of a bitmap.  The on-disk
// and in-memory layouts are the same.
#[repr(C)]
struct BitBlock {
  header: BlockHeader,
  bits: [u8; BITS_SIZE],
}

// For deserialization, define an array for the bits.
pub type BlockBits = [u8; BITS_SIZE];

const BLOCK_BITS_SIZE: usize = mem::size_of::<BlockBits>();

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

// Define a type for the checksum operation.
type ChecksumData = [u8; CHECK_SIZE + DIGESTBYTES];
const EMPTY_CHECKSUM: ChecksumData = [0_u8; CHECK_SIZE + DIGESTBYTES];

/// Define the structure for controlling a persistent bitmap.
pub struct BitMap {
  file: File,
  size: usize,
  blocks: Vec<BitBlock>,
  checksum_data: Vec<ChecksumData>,
  checksum: Digest,
  first_invalid: usize,
  dirty: Vec<i64>, // the modification time for the block, or zero
  set_bits: Vec<u32>,
  map: [u8; 256],
}

// Clippy requires this declaration, otherwise the type is
// "too complicated".
type StoredState = (usize, Vec<BitBlock>, Vec<i64>, Vec<u32>);

impl Drop for BitMap {
  fn drop(&mut self) {
    let _ = self.write();
  }
}

// Get a time in seconds since the epoch.
fn time() -> i64 {
  time::now().to_timespec().sec
}

// Count the number of set bits in a given array using
// an existing map.  See create_map() for details on
// the map.
fn count_bits(bits: &[u8], map: [u8; 256]) -> u32 {
  let mut result: u32 = 0;

  for bit in bits.iter() {
    result += u32::from(map[*bit as usize]);
  }

  result
}

// Create a map of byte values to their corresponding
// population count (count of set bits)
fn create_map() -> [u8; 256] {
  let mut result = [0_u8; 256];

  for (i, res_pos) in result.iter_mut().enumerate() {
    *res_pos = count_byte(i);
  }

  result
}

// Get the population count for a byte value.
fn count_byte(mask: usize) -> u8 {
  let mut result = 0;

  for i in 0..8 {
    result += if mask & (1 << i) == 0 { 0 } else { 1 };
  }

  result
}

// Query the given bit in an array to see whether it is set.
fn bit_set(bits: &[u8], bit_index: usize) -> bool {
  let index = bit_index / 8;
  let shift = bit_index % 8;
  let mask = 1 << shift;

  bits[index] & mask != 0
}

// Convert a count into its serialized form.  Currently,
// that is little-endian, using INDEX_SIZE bytes.
fn encode(mut value: usize) -> [u8; INDEX_SIZE] {
  let mut result = [0u8; INDEX_SIZE];

  for res_pos in result.iter_mut() {
    *res_pos = (value & 0xff) as u8;
    value >>= 8;
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
  /// use crate::ledger::store::bitmap::BitMap;
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
                          checksum_data: Vec::new(),
                          checksum: Digest([0_u8; DIGESTBYTES]),
                          first_invalid: 0,
                          dirty: Vec::new(),
                          set_bits: Vec::new(),
                          map: create_map() };

    Ok(result)
  }

  /// Open an existing bitmap.  The caller is responsible
  /// for opening the file.
  ///
  /// # Example
  ///````
  /// use std::fs::OpenOptions;
  /// use crate::ledger::store::bitmap::BitMap;
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

    let mut result = BitMap { file: data,
                              size: count,
                              blocks: block_vector,
                              checksum_data: Vec::new(),
                              checksum: Digest([0_u8; DIGESTBYTES]),
                              first_invalid: 0,
                              dirty: state_vector,
                              set_bits: set_vector,
                              map: create_map() };

    result.checksum_data.reserve(result.blocks.len());

    // Reserve space for the checksum operation cache.
    for _ in 0..result.blocks.len() {
      result.checksum_data.push(EMPTY_CHECKSUM);
    }

    assert!(result.validate());
    Ok(result)
  }

  // Read the contents of a file into memory, checking the
  // validity as we go.
  fn read_file(file: &mut File) -> Result<StoredState> {
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

  // Check that the population count of bits for a given block
  // in the file matches the contents of the bit map itself.
  fn validate_count(&self, index: usize) -> bool {
    let result = count_bits(&self.blocks[index].bits, self.map) == self.set_bits[index];

    if !result {
      log!("The count at block {} is {}, but should be {}",
           index,
           self.set_bits[index],
           count_bits(&self.blocks[index].bits, self.map));
    }

    result
  }

  // Check that all the blocks in the bit map have accurate
  // population counts.
  fn validate(&self) -> bool {
    let mut pass = true;

    for i in 0..self.blocks.len() {
      pass &= self.validate_count(i);
    }

    pass
  }

  /// Query the value of a bit in the bitmap.
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
      self.checksum_data.push(EMPTY_CHECKSUM);
      self.dirty.push(time());
      self.set_bits.push(0);
    } else {
      self.dirty[block] = time();
      self.blocks[block].header.checksum = CheckBlock::new();
      self.first_invalid = min(self.first_invalid, block);
    }

    // Check whether the bit map state actually is going to
    // be changed.  We can skip the store if not.  Also, we
    // don't want to update the "set" count if nothing is
    // changing.
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

    // Change the actual value in the block.  Also,
    // update the population count.
    if value == 0 {
      self.blocks[block].bits[index] &= !mask;
      self.set_bits[block] -= 1;
    } else {
      self.blocks[block].bits[index] |= mask;
      self.set_bits[block] += 1;
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

  /// Compute a checksum for the entire bitmap.
  ///
  /// The checksum is defined currently recursively across
  /// the blocks where the checksum at block i is:
  ///
  ///    checksum[i] = sha256(check_block[i] | checksum[i - 1])
  ///    checksum[0] = sha256(check_block[0] | [0_u8; DIGESTBYTES])
  ///
  /// where "|" denotes byte array concatenation.  The checksum result
  /// for the entire map is defined as the final sha256 value computed,
  /// or an array of zeros, if no blocks exist in the bitmap.
  ///
  pub fn compute_checksum(&mut self) -> Digest {
    if self.first_invalid == self.blocks.len() {
      return self.checksum;
    }

    let mut result = Digest([0_u8; DIGESTBYTES]);

    // For the first time through the loop, the checksum in the
    // checksum_data array actually is valid, so make the first
    // clone_from_slice thus into a nop.
    if self.first_invalid > 0 {
      let previous_result = &self.checksum_data[self.first_invalid][CHECK_SIZE..];
      result.0.clone_from_slice(previous_result);
    }

    // For each block not yet computed.
    for i in self.first_invalid..self.blocks.len() {
      //
      // Get the checkblock for this block of the bitmap, and
      // clone the result of the previous checksum into it.
      //
      self.checksum_data[i][CHECK_SIZE..].clone_from_slice(result.as_ref());

      // Compute the next sha256 result.
      result = sha256::hash(&self.checksum_data[i]);
    }

    self.first_invalid = self.blocks.len();
    self.checksum = result;
    result
  }

  /// Serialize the bit map to a compressed representation.
  pub fn serialize(&self, version: usize) -> Vec<u8> {
    assert!(self.validate());
    // Reserve space for the version number as a u64.
    let mut bytes = 8;

    for i in 0..self.blocks.len() {
      bytes += self.serial_size(i);
    }

    let mut result = Vec::new();
    result.reserve(bytes);
    result.extend_from_slice(&(version as u64).to_le_bytes());

    for i in 0..self.blocks.len() {
      self.serialize_block(i, &mut result, INCLUDE_BITS);
    }

    assert!(result.len() == bytes);
    result
  }

  /// Serialize the bitmap to a compressed form that
  /// contains bit values only for a blocks containing a
  /// blocks containing an entry in a given list of bit
  /// ids.  Other blocks are represented only by a header
  /// with a checksum.
  pub fn serialize_partial(&self, bit_list: Vec<usize>, version: usize) -> Vec<u8> {
    assert!(self.validate());
    // Reserve space for the version number as a u64.
    let mut bytes = 8;
    let mut set = HashSet::new();

    for i in 0..bit_list.len() {
      set.insert(bit_list[i] / BLOCK_BITS);
    }

    // Add the length for each block.
    for i in 0..self.blocks.len() {
      if set.contains(&i) {
        bytes += self.serial_size(i);
      } else {
        bytes += BLOCK_INFO_SIZE;
      }
    }

    let mut result = Vec::new();
    result.reserve(bytes);
    result.extend_from_slice(&(version as u64).to_le_bytes());

    for i in 0..self.blocks.len() {
      self.serialize_block(i, &mut result, set.contains(&i));
    }

    assert!(result.len() == bytes);
    result
  }

  // Compute the expected size of the serialize form
  // of a given block.
  fn serial_size(&self, index: usize) -> usize {
    let set_bits = self.set_bits[index];
    let clear_bits = BLOCK_BITS as u32 - set_bits;

    if set_bits > LOWER_LIMIT && set_bits < UPPER_LIMIT {
      BLOCK_INFO_SIZE + BLOCK_BITS_SIZE
    } else if set_bits <= LOWER_LIMIT {
      BLOCK_INFO_SIZE + set_bits as usize * INDEX_SIZE
    } else {
      BLOCK_INFO_SIZE + clear_bits as usize * INDEX_SIZE
    }
  }

  // Append serialized form of a block to the Vec
  // representing the result.
  fn serialize_block(&self, index: usize, result: &mut Vec<u8>, include: bool) {
    let set_bits = self.set_bits[index];

    if !include {
      self.append_header(index, BIT_HEADER, 0, result);
    } else if set_bits > LOWER_LIMIT && set_bits < UPPER_LIMIT {
      self.append_block(index, result);
    } else if set_bits <= LOWER_LIMIT {
      self.append_set(index, result);
    } else {
      self.append_clear(index, result);
    }
  }

  // Append the header information as a BlockInfo.
  fn append_header(&self, index: usize, contents: u16, size: u32, result: &mut Vec<u8>) {
    debug!("append_header({}, {}, {}, ...)", index, contents, size);
    let info = BlockInfo { magic: HEADER_MAGIC,
                           checksum: self.blocks[index].header.checksum,
                           count: self.blocks[index].header.count,
                           pad_1: 0,
                           bit_id: self.blocks[index].header.bit_id,
                           list_size: size,
                           contents: contents };

    result.extend_from_slice(info.as_ref());
  }

  // Append an entire block to the serialized form.
  fn append_block(&self, index: usize, result: &mut Vec<u8>) {
    // Append the header to the serialization.
    self.append_header(index, BIT_ARRAY, 0, result);

    result.extend_from_slice(&self.blocks[index].bits[0..BITS_SIZE]);
  }

  // Append a list of the set bits to the serialization
  // results.
  fn append_set(&self, index: usize, result: &mut Vec<u8>) {
    let set_bits = self.set_bits[index] as u32;

    // Append the header to the serialization.
    self.append_header(index, BIT_DESC_SET, set_bits, result);

    // Get the bit map.
    let bits = &self.blocks[index].bits;
    let mut count = 0;

    for i in 0..self.blocks[index].header.count as usize {
      if bit_set(bits, i) {
        result.extend_from_slice(&encode(i));
        count += 1;
      }
    }

    assert!(count == set_bits);
    debug!("append_set: {} bits -> {} bytes ({} set)",
           self.set_bits[index],
           self.set_bits[index] * INDEX_SIZE as u32 + BLOCK_INFO_SIZE as u32,
           set_bits);
  }

  // Append a list of the clear bits to the serialization
  // results.
  fn append_clear(&self, index: usize, result: &mut Vec<u8>) {
    let clear_bits = BLOCK_BITS as u32 - self.set_bits[index];

    self.append_header(index, BIT_DESC_CLEAR, clear_bits, result);

    let mut count = 0;
    let bits = &self.blocks[index].bits;

    for i in 0..BLOCK_BITS {
      if !bit_set(bits, i) {
        result.extend_from_slice(&encode(i));
        count += 1;
      }
    }

    assert!(count == clear_bits);
    debug!("append_clear:  {} bits -> {} bytes ({} clear)",
           self.set_bits[index],
           (BLOCK_BITS - self.set_bits[index] as usize) * INDEX_SIZE + BLOCK_INFO_SIZE as usize,
           clear_bits);
  }

  pub fn deserialize(bytes: &[u8]) -> Result<(u64, Vec<BlockInfo>, HashMap<u64, BlockBits>)> {
    let mut info_vec = Vec::new();
    let mut bits_vec = HashMap::new();
    let mut index = 0;

    if bytes.len() < 8 {
      return se!("The input did not contain a version number.");
    }

    let mut version: u64 = 0;

    for i in 0..8 {
      version = (version << 8) | bytes[i] as u64;
    }

    index += 8;

    loop {
      if bytes.len() == index {
        break;
      } else if bytes.len() - index < BLOCK_INFO_SIZE {
        return se!("The input was too short.");
      }

      let mut info = BlockInfo::default();
      BitMap::clone_info(info.as_mut(), bytes, index);
      index += BLOCK_INFO_SIZE;
      info.validate()?;

      let mut bits: BlockBits;

      match info.contents {
        BIT_HEADER => {}
        BIT_ARRAY => {
          bits = [0_u8; BITS_SIZE];
          bits.clone_from_slice(&bytes[index..index + BITS_SIZE]);
          index += BITS_SIZE;
          bits_vec.insert(info.bit_id, bits);
        }
        BIT_DESC_SET => {
          let (next, ids) = BitMap::decode(info.list_size, bytes, index)?;
          bits = [0_u8; BITS_SIZE];

          for i in 0..ids.len() {
            assert!(ids[i] < info.count as usize);
            BitMap::mutate_bit(&mut bits, ids[i], true);
          }

          bits_vec.insert(info.bit_id, bits);
          index = next;
        }
        BIT_DESC_CLEAR => {
          let (next, ids) = BitMap::decode(info.list_size, bytes, index)?;
          bits = [0xff_u8; BITS_SIZE];

          for i in 0..ids.len() {
            assert!(ids[i] < info.count as usize);
            BitMap::mutate_bit(&mut bits, ids[i], false);
          }

          bits_vec.insert(info.bit_id, bits);
          index = next;
        }
        _ => {
          return se!("Invalid info contents type:  {}", info.contents);
        }
      }

      info_vec.push(info);
    }

    Ok((version, info_vec, bits_vec))
  }

  fn clone_info(info: &mut [u8], bytes: &[u8], index: usize) {
    info.clone_from_slice(&bytes[index..index + BLOCK_INFO_SIZE]);
  }

  fn decode(list_size: u32, bytes: &[u8], start: usize) -> Result<(usize, Vec<usize>)> {
    let mut index = start;
    let mut ids = Vec::new();
    let bytes_consumed = list_size as usize * INDEX_SIZE;

    if index + bytes_consumed > bytes.len() {
      return se!("An index list was too long:  {}", list_size);
    }

    for _ in 0..list_size {
      let mut id: usize = 0;

      for position in 0..INDEX_SIZE {
        id |= (bytes[index] as usize) << (8 * position);
        index += 1;
      }

      ids.push(id);
    }

    debug!("decode() -> ({}, {:?})", index, ids);
    Ok((index, ids))
  }

  fn mutate_bit(bytes: &mut BlockBits, id: usize, bit_set: bool) {
    let index = id / 8;
    let shift = id % 8;
    let mask = 1 << shift;

    if bit_set {
      bytes[index] |= mask
    } else {
      bytes[index] &= !mask;
    }
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
    assert!(header.bit_id == id * BLOCK_BITS as u64);

    if let Err(e) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed:  {}", e);
    }

    header.magic ^= 1;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad magic number.");
    }

    header.magic ^= 1;
    assert!(header.validate(BIT_ARRAY, id).is_ok());

    header.count = (BLOCK_BITS + 1) as u32;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad count.");
    }

    header.count = 0;
    assert!(header.validate(BIT_ARRAY, id).is_ok());
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
    assert!(header.validate(BIT_ARRAY, id).is_ok());
    header.pad_1 = 1;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad pad_1.");
    }

    header.pad_1 = 0;
    assert!(header.validate(BIT_ARRAY, id).is_ok());
    header.pad_2 = 1;

    if let Ok(_) = header.validate(BIT_ARRAY, id) {
      panic!("Validation failed to detect a bad pad_2.");
    }

    header.pad_2 = 0;
    assert!(header.validate(BIT_ARRAY, id).is_ok());

    let header = BlockHeader::new(BIT_DESC_SET, 0).unwrap();
    assert!(header.contents == BIT_DESC_SET);
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
    assert!(BLOCK_INFO_SIZE == 24 + CHECK_SIZE);

    let mut block = BitBlock::new(BIT_DESC_CLEAR, 32).unwrap();
    assert!(block.header.contents == BIT_DESC_CLEAR);
    assert!(block.header.bit_id == 32 * BLOCK_BITS as u64);

    block.set_checksum();

    if let Err(_) = block.validate(BIT_DESC_CLEAR, 32) {
      panic!("Block validation failed.");
    }

    block.header.checksum.bytes[0] ^= 1;

    if let Ok(_) = block.validate(BIT_DESC_CLEAR, 32) {
      panic!("Block validation didn't detect a bad checksum: {:?} vs {:?}.",
             block.header.checksum.bytes,
             block.compute_checksum());
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

    if bitmap.compute_checksum() != Digest([0_u8; DIGESTBYTES]) {
      panic!("compute_checksum() failed on an empty tree");
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

    let s1 = bitmap.serialize_partial(vec![0, BLOCK_BITS], 1);

    if let Err(x) = BitMap::deserialize(&s1) {
      panic!("deserialize(&s1) failed:  {}", x);
    }

    let s2 = bitmap.serialize(1);

    if let Err(x) = BitMap::deserialize(&s2) {
      panic!("deserialize(&s2) failed:  {}", x);
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
