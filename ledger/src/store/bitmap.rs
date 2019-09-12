//! # A Simple BitMap Implementation
//!
//! This module implements a simple persistent bitmap.  The
//! bitmap currently is stored persistently in a single file.
//! The caller is responsible for file creation and open
//! operations; thus this module does not manage or use file
//! paths.
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
//! This module supports a mildly-compressed bitmap version that
//! can be downloaded.  The serialize and serialize_partial
//! methods, q.v., produce a Vec<u8> that can be sent over the
//! network.  This Vec can be converted to a SparseMap structure.
//! The SparseMap structure allows various queries on the contents
//! of the map.

use findora::commas_u;
use findora::commas_us;
use findora::timestamp;
use findora::EnableMap;
use findora::DEFAULT_MAP;
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

#[allow(non_upper_case_globals)]
static bitmap_map: EnableMap = DEFAULT_MAP;

// Constants for calling serialize_block.
// const HEADER_ONLY: bool = false;
const INCLUDE_BITS: bool = true;

/// Define a structure for a checksum on a block.  We use
/// the first CHECK_SIZE bytes of the sha256 digest as the
/// checksum.
const CHECK_SIZE: usize = 16;

#[repr(C)]
#[derive(Copy, Clone, Debug, Default, PartialEq)]
struct CheckBlock {
  bytes: [u8; CHECK_SIZE],
}

impl CheckBlock {
  fn new() -> CheckBlock {
    CheckBlock { bytes: [0_u8; CHECK_SIZE] }
  }
}

// Define various constants for use in the file
// data block header.
const HEADER_MAGIC: u32 = 0x0204_0600;
const HEADER_SIZE: usize = 48;

// Define the types of headers.
const BIT_INVALID: u16 = 0; // This value is used for testing.
const BIT_ARRAY: u16 = 1;
const BIT_DESC_SET: u16 = 2;
const BIT_DESC_CLEAR: u16 = 3;
const BIT_HEADER: u16 = 4;

// For users who download the bitmap, this is what they
// get for a block header.
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
      return er!("Invalid magic value {:x}", self.magic);
    }

    if self.count > BLOCK_BITS as u32 {
      return er!("Invalid count {}", self.count);
    }

    if self.list_size as usize > BITS_SIZE / INDEX_SIZE {
      return er!("Invalid list size {}", self.list_size);
    }

    Ok(())
  }

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

// For deserialization, define an array for the valid bits
// of one block.
pub type BlockBits = [u8; BITS_SIZE];

// Define some constants for serialization.  DESCRIPTOR_SIZE
// defines the number of bytes of global data describing the
// serialized bitmap.  Currently, the descriptor contains an
// 8-byte version and a checksum for the tree.

const BLOCK_INFO_SIZE: usize = mem::size_of::<BlockInfo>();
const DESCRIPTOR_SIZE: usize = 8 + DIGESTBYTES;
const BLOCK_BITS_SIZE: usize = mem::size_of::<BlockBits>();

/// This structure can be initialized using a bitmap
/// downloaded from a server.  It allows queries and
/// checksum verification.

pub struct SparseMap {
  version: u64,
  checksum: Digest,
  headers: Vec<BlockInfo>,
  map: HashMap<u64, BlockBits>,
}

impl SparseMap {
  /// Create a new SparseMap from a serialized form.
  pub fn new(bytes: &[u8]) -> Result<SparseMap> {
    let (version, checksum, headers, map) = match BitMap::deserialize(bytes) {
      Ok((version, checksum, headers, map)) => (version, checksum, headers, map),
      Err(x) => {
        return Err(x);
      }
    };

    Ok(SparseMap { version,
                   checksum,
                   headers,
                   map })
  }

  /// Return the version of a SparseMap.
  pub fn version(&self) -> u64 {
    self.version
  }

  /// Return the checksum for the entire bitmap.
  pub fn checksum(&self) -> Digest {
    self.checksum
  }

  /// Query the value of a bit.  If it is present in the map,
  /// return a boolean.  Otherwise, return an error.
  pub fn query(&self, id: u64) -> Result<bool> {
    let block = (id / BLOCK_BITS as u64) as usize;
    let block_index = (id % BLOCK_BITS as u64) as usize;
    let index = block_index / 8;
    let shift = block_index % 8;
    let mask = 1 << shift;

    if block >= self.headers.len() {
      return er!("Bit {} in block {} is not present.", id, commas_us(block));
    }

    let info = &self.headers[block];
    debug!(bitmap_map,
           "query:  block_index {}, info {:?}", block_index, info);

    if block_index >= info.count as usize {
      return er!("Bit {} in block {} is not present.", id, commas_us(block));
    }

    match self.map.get(&(block as u64)) {
      Some(bits) => {
        return Ok(bits[index] & mask != 0);
      }
      None => {
        debug!(bitmap_map, "query:  no map for block {}", commas_us(block));
      }
    }

    er!("Bit {} in block {} is not present.", id, commas_us(block))
  }

  /// Validate that the tree actually matches the
  /// checksum in the download.  The checksum of
  /// complete blocks that were downloaded are
  /// checked as well, so that further query results
  /// are from validated blocks.
  pub fn validate_checksum(&self) -> bool {
    let mut checksum_data = EMPTY_CHECKSUM;
    let mut digest = Digest { 0: [0_u8; DIGESTBYTES] };

    // For each block, compute the checksum.
    for i in 0..self.headers.len() {
      let info = &self.headers[i];

      // If the bitmap is present, make sure that it matches
      // expectations.
      match self.map.get(&(i as u64)) {
        Some(bits) => {
          // Recreate the block as it should have been on disk...
          let mut block = BitBlock::new(BIT_ARRAY, i as u64).unwrap();
          block.header.count = info.count;
          block.bits.clone_from_slice(&bits[0..]);

          let checksum = block.compute_checksum();

          if checksum != info.checksum.bytes {
            debug!(bitmap_map, "validate_checksum:  header {:?}", block.header);
            log!(bitmap_map,
                 "Invalid checksum for block {}",
                 commas_u(info.bit_id / BLOCK_BITS as u64));
            return false;
          }

          debug!(bitmap_map,
                 "validate_checksum:  Block {} verifies.",
                 commas_us(i));
        }
        None => {
          debug!(bitmap_map, "validate_checksum:  phantom data");
        }
      }

      checksum_data[0..CHECK_SIZE].clone_from_slice(&info.checksum.bytes[0..CHECK_SIZE]);
      digest = sha256::hash(&checksum_data);
      debug!(bitmap_map,
             "validate_checksum:  checksum at block {} is {:?}",
             commas_us(i),
             info.checksum);
      debug!(bitmap_map,
             "validate_checksum:  checksum_data at block {} is {:?}",
             commas_us(i),
             show(&checksum_data));
      debug!(bitmap_map,
             "validate_checksum:  digest at block {} is {:?}",
             commas_us(i),
             digest);
      checksum_data[CHECK_SIZE..].clone_from_slice(&digest.0[0..]);
    }

    debug!(bitmap_map,
           "validate_checksum:  got final digest {:?}", digest);
    digest == self.checksum
  }
}

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

#[derive(Debug)]
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
    if block_contents != BIT_ARRAY
       && block_contents != BIT_DESC_SET
       && block_contents != BIT_DESC_CLEAR
       && block_contents != BIT_HEADER
    {
      return er!("That content type ({}) is invalid.", block_contents);
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
      return er!("Block {} has a bad magic number:  {:x}",
                 commas_u(id),
                 self.magic);
    }

    if self.count > BLOCK_BITS as u32 {
      return er!("Block {} has a bad count:  {} vs {}",
                 commas_u(id),
                 self.count,
                 BLOCK_BITS);
    }

    if self.bit_id != id * BLOCK_BITS as u64 {
      return er!("Block {} has a bad id:  the disk said {}.",
                 commas_u(id),
                 self.bit_id);
    }

    if self.contents != contents {
      return er!("Block {} has a bad contents type:  {}",
                 commas_u(id),
                 self.contents);
    }

    if self.pad_1 != 0 {
      return er!("Block {} has an invalid pad_1:  {}",
                 commas_u(id),
                 self.pad_1);
    }

    if self.pad_2 != 0 {
      return er!("Block {} has an invalid pad_2:  {}",
                 commas_u(id),
                 self.pad_2);
    }

    Ok(())
  }
}

// Define the block structure of the server version
// of the bitmap.
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
      return er!("Block {} has a bad checksum.", commas_u(id));
    }

    Ok(())
  }
}

// Define a type for the checksum operation.

type ChecksumData = [u8; CHECK_SIZE + DIGESTBYTES];
const EMPTY_CHECKSUM: ChecksumData = [0_u8; CHECK_SIZE + DIGESTBYTES];

// Provide a way to display a checksum computation
// buffer when debugging.  Used by the debug macro
// when it is enabled.
fn show(data: &ChecksumData) -> String {
  let mut result: String = "{ ".to_string();

  result = result + &format!("{}", data[0]);

  for i in 1..data.len() {
    result += &format!(", {}", data[i]);
  }

  result + " }"
}

/// Define the in-memory structure for managing a
/// persistent bitmap.
///
/// Data kept for the bitmap as a whole:
///   file            a file structure for the disk data
///   size            the number of bits in the bitmap
///   checksum        a checksum for the entire bitmap, if valid
///   first_invalid   the index to the first invalidated checksum term
///                     See the BitMap compute_checksum method for
///                     details.
///   map             a map from a byte value to the set bit count
///                     for that value, i.e., 0 maps to zero, 1 and 2
///                     map to 1, ...
///
/// Data kept per block:
///   blocks          the vector of data blocks in the map
///   checksum_data   the work area for computing the bitmap checksum
///   checksum_valid  a bit indicating whether the checksum_data field
///                     a valid block checksum present
///   dirty           the modification time for the block, or zero
///                     if the block is clean (valid on disk)
///   set_bits        the count of set bits in the block
///
pub struct BitMap {
  file: File,
  size: usize,
  checksum: Digest,
  first_invalid: usize,
  map: [u8; 256],

  // Data kept per block
  blocks: Vec<BitBlock>,
  checksum_data: Vec<ChecksumData>,
  checksum_valid: Vec<bool>,
  dirty: Vec<i64>,
  set_bits: Vec<u32>,
}

// Write any dirty blocks when a bitmap is dropped.
impl Drop for BitMap {
  fn drop(&mut self) {
    let _ = self.write();
  }
}

// Get a time in seconds since the epoch.  This
// value is used to tag dirty blocks.
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

// Clippy requires this declaration, otherwise a type is
// "too complicated".
type StoredState = (usize, Vec<BitBlock>, Vec<i64>, Vec<bool>, Vec<u32>);

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
      return er!("The file contains data!");
    }

    let result = BitMap { file: data,
                          size: 0,
                          blocks: Vec::new(),
                          checksum_data: Vec::new(),
                          checksum: Digest([0_u8; DIGESTBYTES]),
                          dirty: Vec::new(),
                          checksum_valid: Vec::new(),
                          set_bits: Vec::new(),
                          map: create_map(),
                          first_invalid: 0 };

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
    let (count, block_vector, state_vector, checksum_vector, set_vector) =
      BitMap::read_file(&mut data)?;

    let mut result = BitMap { file: data,
                              size: count,
                              blocks: block_vector,
                              checksum_data: Vec::new(),
                              checksum: Digest([0_u8; DIGESTBYTES]),
                              dirty: state_vector,
                              checksum_valid: checksum_vector,
                              set_bits: set_vector,
                              map: create_map(),
                              first_invalid: 0 };

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
    let mut checksum_valid = Vec::new();
    let mut set = Vec::new();
    let mut count = 0;

    // Get a map to convert a byte value to a set bit count.
    let map = create_map();

    // Compute the number of blocks in the file.
    let file_size = file.seek(End(0))?;

    if file_size % BLOCK_SIZE as u64 != 0 {
      return er!("That file size ({}) is invalid.", file_size);
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
            return er!("Block {} is not full:  count {}", index, block.header.count);
          }

          let set_count = count_bits(&block.bits, map);
          count += block.header.count as usize;
          blocks.push(block);
          dirty.push(0 as i64);
          checksum_valid.push(false);
          set.push(set_count);
        }
        Err(e) => {
          return Err(e);
        }
      }
    }

    Ok((count, blocks, dirty, checksum_valid, set))
  }

  // Check that the population count of bits for a given block
  // in the file matches the contents of the bit map itself.
  fn validate_count(&self, index: usize) -> bool {
    let result = count_bits(&self.blocks[index].bits, self.map) == self.set_bits[index];

    if !result {
      log!(bitmap_map,
           "The count at block {} is {}, but should be {}",
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
      return er!("That index is out of range ({} vs {}).", bit, self.size);
    }

    let block = bit / BLOCK_BITS;
    let bit_id = bit % BLOCK_BITS;
    let index = bit_id / 8;
    let mask = 1 << (bit_id % 8);

    debug!(bitmap_map,
           "query({}) -> block {}, index {}, mask {}", bit, block, index, mask);
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
      return er!("That index is too large to set ({} vs {}).", bit, self.size);
    }

    self.mutate(bit, 1, true)
  }

  /// Clear the given bit.
  pub fn clear(&mut self, bit: usize) -> Result<()> {
    if bit >= self.size {
      return er!("That index is too large to clear ({} vs {}).",
                 bit,
                 self.size);
    }

    self.mutate(bit, 0, false)
  }

  // Change the value of the given bit, as requested.
  // Bitmap extensions happen here, too.
  fn mutate(&mut self, bit: usize, value: u8, extend: bool) -> Result<()> {
    if !extend && bit >= self.size {
      return er!("That index ({}) is out of the range [0, {}).",
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
    // push the new block and all the metadata entries.
    if block >= self.blocks.len() {
      self.blocks.push(BitBlock::new(BIT_ARRAY, block as u64)?);
      self.checksum_data.push(EMPTY_CHECKSUM);
      self.dirty.push(time());
      self.checksum_valid.push(false);
      self.set_bits.push(0);
    } else {
      self.dirty[block] = time();
      self.checksum_valid[block] = false;
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
          log!(bitmap_map, "Error writing block {}:  {}", block, e);
        }
      }
    }

    Ok(())
  }

  /// Compute a sha256 checksum for the entire bitmap.
  ///
  /// The checksum is defined currently recursively across the
  /// blocks where the checksum at block i is:
  ///
  ///    checksum[i] = sha256(check_block[i] | checksum[i - 1])
  ///    checksum[0] = sha256(check_block[0] | [0_u8; DIGESTBYTES])
  ///
  /// where "|" denotes byte array concatenation, and check_block[i]
  /// is the CheckBlock for block i.
  ///
  /// The final checksum result for the entire map is defined as the
  /// final sha256 value computed, or an array of zeros, if no blocks
  /// exist in the bitmap.
  ///
  pub fn compute_checksum(&mut self) -> Digest {
    let mut digest = Digest([0_u8; DIGESTBYTES]);
    let mut first = false;

    // For each block not yet computed.
    for i in self.first_invalid..self.blocks.len() {
      if !first {
        self.checksum_data[i][CHECK_SIZE..].clone_from_slice(&digest[0..]);
        first = false;
      }

      //
      // Compute the checksum for this block if needed and insert
      // it into our checksum_data.
      //
      if !self.checksum_valid[i] {
        debug!(bitmap_map, "compute_checksum:  fixing block {}", i);
        self.blocks[i].set_checksum();
        let checksum = &self.blocks[i].header.checksum.bytes;
        self.checksum_data[i][0..CHECK_SIZE].clone_from_slice(checksum);
        self.checksum_valid[i] = true;
      }

      //
      // Get the previous digest into our checksum_data.
      //
      self.checksum_data[i][CHECK_SIZE..].clone_from_slice(digest.as_ref());

      // Compute the next sha256 digest.
      digest = sha256::hash(&self.checksum_data[i]);
      debug!(bitmap_map,
             "compute_checksum:  checksum at block {} is {:?}", i, self.blocks[i].header.checksum);
      debug!(bitmap_map,
             "compute_checksum:  checksum_data at block {} is {:?}",
             i,
             show(&self.checksum_data[i]));
      debug!(bitmap_map,
             "compute_checksum:  digest at block {} is {:?}", i, digest);
    }

    debug!(bitmap_map,
           "compute_checksum:  got final digest {:?}", digest);
    self.checksum = digest;
    digest
  }

  /// Serialize the bit map to a compressed representation.
  pub fn serialize(&mut self, version: usize) -> Vec<u8> {
    assert!(self.validate());
    // Reserve space for the version number as a u64.
    let mut bytes = DESCRIPTOR_SIZE;

    // Reserve space for each block.
    for i in 0..self.blocks.len() {
      bytes += self.serial_size(i);
    }

    let mut result = Vec::new();
    result.reserve(bytes);
    self.append_descriptor(version as u64, &mut result);

    for i in 0..self.blocks.len() {
      self.serialize_block(i, &mut result, INCLUDE_BITS);
    }

    assert!(result.len() == bytes);
    result
  }

  /// Serialize the bitmap to a compressed form that contains
  /// bit values only for a blocks containing a blocks
  /// containing an entry in a given list of bit ids.  Other
  /// blocks are represented only by a header with a checksum.
  pub fn serialize_partial(&mut self, bit_list: Vec<usize>, version: usize) -> Vec<u8> {
    assert!(self.validate());
    // Reserve space for the version number as a u64.
    let mut bytes = DESCRIPTOR_SIZE;
    let mut set = HashSet::new();

    for i in 0..bit_list.len() {
      set.insert(bit_list[i] / BLOCK_BITS);
    }

    // Add the space needed for each block.
    for i in 0..self.blocks.len() {
      if set.contains(&i) {
        bytes += self.serial_size(i);
      } else {
        bytes += BLOCK_INFO_SIZE;
      }
    }

    let mut result = Vec::new();
    result.reserve(bytes);
    self.append_descriptor(version as u64, &mut result);

    for i in 0..self.blocks.len() {
      self.serialize_block(i, &mut result, set.contains(&i));
    }

    assert!(result.len() == bytes);
    result
  }

  // Append the global data for the bitmap.  That data
  // currently consists of the version number from the
  // caller and the checksum of the tree.
  fn append_descriptor(&mut self, version: u64, bytes: &mut Vec<u8>) {
    let digest = self.compute_checksum();

    bytes.extend_from_slice(&version.to_le_bytes());
    bytes.extend_from_slice(&digest[0..]);
  }

  // Compute the expected size of the serialized form
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

  // Append the serialized form of a block to the Vec
  // representing the results.
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
  fn append_header(&self, index: usize, block_contents: u16, size: u32, result: &mut Vec<u8>) {
    debug!(bitmap_map,
           "append_header({}, {}, {}, ...)", index, block_contents, size);
    let info = BlockInfo { magic: HEADER_MAGIC,
                           checksum: self.blocks[index].header.checksum,
                           count: self.blocks[index].header.count,
                           pad_1: 0,
                           bit_id: self.blocks[index].header.bit_id,
                           list_size: size,
                           contents: block_contents };

    assert!(self.checksum_valid[index]);
    result.extend_from_slice(info.as_ref());
  }

  // Append an entire block to the serialized form.
  // This is called when the bitmap can't be compressed.
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
    debug!(bitmap_map,
           "append_set: {} bits -> {} bytes ({} set)",
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
    debug!(bitmap_map,
           "append_clear:  {} bits -> {} bytes ({} clear)",
           self.set_bits[index],
           (BLOCK_BITS - self.set_bits[index] as usize) * INDEX_SIZE + BLOCK_INFO_SIZE as usize,
           clear_bits);
  }

  // Convert a serialized bitmap back into structured data.
  fn deserialize(bytes: &[u8]) -> Result<(u64, Digest, Vec<BlockInfo>, HashMap<u64, BlockBits>)> {
    let mut info_vec = Vec::new();
    let mut bits_map = HashMap::new();
    let mut index = 0;

    // First, retrieve the global data, if possible.
    if bytes.len() < DESCRIPTOR_SIZE {
      return er!("The input did not contain a descriptor.");
    }

    let (next, version, checksum) = BitMap::decode_descriptor(bytes, index);
    index = next;

    // Now loop retrieving any blocks in the map.
    loop {
      if bytes.len() == index {
        break;
      } else if bytes.len() - index < BLOCK_INFO_SIZE {
        return er!("The input was too short.");
      }

      // Get the next BlockInfo structure from the stream.
      let mut info = BlockInfo::default();
      BitMap::clone_info(info.as_mut(), bytes, index);
      index += BLOCK_INFO_SIZE;
      info.validate()?;

      let block = info.bit_id / BLOCK_BITS as u64;
      let mut bits: BlockBits;

      // Now retrieve the block contents, if present, and
      // restore them to bitmaps.
      match info.contents {
        BIT_HEADER => {
          debug!(bitmap_map, "No contents for block {}", block);
        }
        BIT_ARRAY => {
          bits = [0_u8; BITS_SIZE];
          bits.clone_from_slice(&bytes[index..index + BITS_SIZE]);
          index += BITS_SIZE;
          bits_map.insert(block, bits);
          debug!(bitmap_map, "deserialize:  insert map block {}", block);
        }
        BIT_DESC_SET => {
          let (next, ids) = BitMap::decode(info.list_size, bytes, index)?;
          bits = [0_u8; BITS_SIZE];

          for i in 0..ids.len() {
            assert!(ids[i] < info.count as usize);
            BitMap::mutate_bit(&mut bits, ids[i], true);
          }

          bits_map.insert(block, bits);
          debug!(bitmap_map, "deserialize:  insert set block {}", block);
          index = next;
        }
        BIT_DESC_CLEAR => {
          let (next, ids) = BitMap::decode(info.list_size, bytes, index)?;
          bits = [0xff_u8; BITS_SIZE];

          for i in 0..ids.len() {
            assert!(ids[i] < info.count as usize);
            BitMap::mutate_bit(&mut bits, ids[i], false);
          }

          bits_map.insert(block, bits);
          debug!(bitmap_map, "deserialize:  insert clear block {}", block);
          index = next;
        }
        _ => {
          return er!("Invalid info contents type:  {}", info.contents);
        }
      }

      info_vec.push(info);
    }

    if index != bytes.len() {
      return er!("The vector contained {} extra bytes.", bytes.len() - index);
    }

    Ok((version, checksum, info_vec, bits_map))
  }

  // Decode the global information from the byte stream.
  fn decode_descriptor(bytes: &[u8], start: usize) -> (usize, u64, Digest) {
    // Pull the version number out of the slice.
    let mut index = start;
    let mut version: u64 = 0;

    // TODO:  There probably is a library routine for this.
    for i in 0..8 {
      version |= (bytes[index + i] as u64) << (8 * i);
    }

    index += 8;

    // Now pull the checksum for the tree out of the structure.
    let mut checksum = Digest([0u8; DIGESTBYTES]);
    checksum.0[..DIGESTBYTES].clone_from_slice(&bytes[index..index + DIGESTBYTES]);

    index += DIGESTBYTES;
    (index, version, checksum)
  }

  // Copy the BlockInfo structure from the byte stream into
  // an aligned structure.
  fn clone_info(info: &mut [u8], bytes: &[u8], index: usize) {
    info.clone_from_slice(&bytes[index..index + BLOCK_INFO_SIZE]);
  }

  // Decode a list of bit indices.  We get the number of entries
  // as an input, since it's kept in the BitInfo structure.
  fn decode(list_size: u32, bytes: &[u8], start: usize) -> Result<(usize, Vec<usize>)> {
    let mut index = start;
    let mut ids = Vec::new();
    let bytes_consumed = list_size as usize * INDEX_SIZE;

    if index + bytes_consumed > bytes.len() {
      return er!("An index list was too long:  {}", list_size);
    }

    for _ in 0..list_size {
      let mut id: usize = 0;

      for position in 0..INDEX_SIZE {
        id |= (bytes[index] as usize) << (8 * position);
        index += 1;
      }

      ids.push(id);
    }

    debug!(bitmap_map, "decode() -> ({}, {:?})", index, ids);
    Ok((index, ids))
  }

  // Given a bit index into an array, change that bit's
  // value to what's given.  This procedure is used when
  // creating a downloaded block.
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
  // We must set the checksum here since it's been changed,
  // although compute_checksum() actually might have fixed
  // it.  TODO:  use first_invalid to avoid an unnecessary
  // update of the checksum?
  fn write_block(&mut self, index: usize) -> Result<()> {
    self.blocks[index].set_checksum();
    let offset = index as u64 * BLOCK_SIZE as u64;
    self.file.seek(Start(offset))?;
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
    log!(bitmap_map, "Run the basic bitmap test.");
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

    // Pick a random version number.
    let sparse_version = 0x010203;
    let s2 = bitmap.serialize(sparse_version);

    if let Err(x) = BitMap::deserialize(&s2) {
      panic!("deserialize(&s2) failed:  {}", x);
    }

    let partial_map = SparseMap::new(&s1).unwrap();
    let sparse_map = SparseMap::new(&s2).unwrap();

    for (key, _value) in sparse_map.map.iter() {
      println!("Block {} mapped.", key);
    }

    assert!(partial_map.validate_checksum());
    assert!(sparse_map.validate_checksum());

    println!("sparse_version = {}, sparse_map.version() = {}",
             sparse_version,
             sparse_map.version());
    assert!(sparse_map.version() == sparse_version as u64);
    assert!(sparse_map.checksum() == bitmap.compute_checksum());

    for i in 0..bitmap.size() as u64 {
      let bitmap_result = bitmap.query(i as usize).unwrap();
      let sparse_result = sparse_map.query(i).unwrap();

      if let Ok(partial_result) = partial_map.query(i) {
        assert!(partial_result == bitmap_result);
      } else {
        assert!(i >= 2 * BLOCK_BITS as u64);
      }

      if bitmap_result != sparse_result {
        panic!("Sparse mismatch at {}", i);
      }
    }

    if let Ok(_) = bitmap.query(bitmap.size()) {
      panic!("bitmap query at size passed.");
    }

    if let Ok(_) = sparse_map.query(bitmap.size() as u64) {
      panic!("sparse_map query at size passed.");
    }

    if let Ok(_) = partial_map.query(bitmap.size() as u64) {
      panic!("partial_map query at size passed.");
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
