//!
//! # An Append-Only Merkle Tree Implementation
//!
//!  This module implements an append-only binary Merkle tree using
//!  SHA256 as the hash function. The tree currently is kept in memory,
//!  but this module will write the contents to disk when requested, and
//!  can initialize a tree using a disk image. Eventually, it should
//!  support a paged tree, i.e., one that is loaded on demand and paged
//!  to disk as needed.
//!

#![deny(warnings)]
#![deny(missing_docs)]
#![allow(clippy::needless_borrow)]

use {
    chrono::Utc,
    cryptohash::{hash_pair, hash_partial, sha256, HashValue, Proof, HASH_SIZE},
    globutils::Commas,
    ruc::*,
    serde::{Deserialize, Deserializer, Serialize, Serializer},
    std::{
        collections::HashMap,
        fmt,
        fs::{self, File, OpenOptions},
        io::{ErrorKind, Read, Seek, SeekFrom, Write},
        mem::{self, MaybeUninit},
        path,
        ptr::copy_nonoverlapping,
        result::Result as StdResult,
        slice,
    },
};

const BLOCK_SHIFT: u16 = 9;
const LEVELS_IN_BLOCK: usize = BLOCK_SHIFT as usize;
const HASHES_IN_BLOCK: usize = (1 << BLOCK_SHIFT) - 1;
const LEAVES_IN_BLOCK: usize = (HASHES_IN_BLOCK + 1) / 2;
const CHECK_SIZE: usize = 16;
const HEADER_VALUE: u32 = 0xabcd_0123;
const BLOCK_SIZE: usize = HASH_SIZE * (HASHES_IN_BLOCK + 1);
const MAX_BLOCK_LEVELS: usize = 64;
const PROOF_VERSION: u64 = 0;

#[repr(C)]
#[derive(PartialEq, Copy, Clone, Debug, Deserialize, Serialize)]
struct CheckBits {
    bits: [u8; CHECK_SIZE],
}

// Define a header structure for each block. It identifies the data
// in the block, and contains a checksum. This structure needs to
// be HASH_SIZE bytes in size. It must sit at the start of the block.
// The check_bits field must be first in the structure.
#[repr(C)]
#[derive(PartialEq, Copy, Clone, Debug, Deserialize, Serialize)]
struct BlockHeader {
    check_bits: CheckBits,
    header_mark: u32,
    level: u16,
    valid_leaves: u16,
    id: u64,
}

impl BlockHeader {
    #[inline(always)]
    fn new(level: u32, block_id: u64) -> BlockHeader {
        BlockHeader {
            check_bits: CheckBits {
                bits: [0; CHECK_SIZE],
            },
            header_mark: HEADER_VALUE,
            level: level as u16,
            valid_leaves: 0,
            id: block_id,
        }
    }

    // Do a simple consistency check on some fields in the header.
    fn check(&self, level: usize, id: u64) -> Result<()> {
        if self.header_mark != HEADER_VALUE {
            return Err(eg!(format!(
                "Block {} at level {} has a bad header ({:x}).",
                id, level, self.header_mark
            )));
        }

        if self.level != level as u16 {
            return Err(eg!(format!(
                "Block {} at level {} has a bad level ({}).",
                id, level, self.level
            )));
        }

        if self.id != id {
            return Err(eg!(format!(
                "Block {} at level {} has a bad id ({}).",
                id, level, self.id
            )));
        }

        if self.valid_leaves > LEAVES_IN_BLOCK as u16 {
            return Err(eg!(format!(
                "The entry count for block {} at level {} is too large ({}).",
                id, level, self.valid_leaves
            )));
        }

        Ok(())
    }
}

// Define a dictionary that will contain "completed"
// blocks used when generating a proof. The working
// copy of the tree is completed as blocks fill, but
// a proof requires that we complete partial blocks
// by defining the value of a parent node with one
// child as the hash of the child node. The dictionary
// contains such blocks when generating a proof so
// that the working copy is not modified.
struct Dictionary {
    max_level: usize,
    map: HashMap<usize, Entry>,
}

impl Dictionary {
    #[inline(always)]
    pub fn new() -> Dictionary {
        Dictionary {
            max_level: 0,
            map: HashMap::new(),
        }
    }

    // Retrieve an entry from the dictionary, or None.
    #[inline(always)]
    pub fn get(&self, level: usize, id: usize) -> Option<&Entry> {
        match self.map.get(&level) {
            Some(entry) => {
                if entry.id == id {
                    Some(&entry)
                } else {
                    None
                }
            }
            None => None,
        }
    }

    // Add an entry to the dictionary.
    #[inline(always)]
    pub fn insert(&mut self, level: usize, entry: Entry) {
        self.map.insert(level, entry);

        if level > self.max_level {
            self.max_level = level;
        }
    }

    #[inline(always)]
    pub fn max_level(&self) -> usize {
        self.max_level
    }
}

// An entry contains the data needed to describe
// the parts of a block used when generating a
// proof. We generate entries when the working
// copy of the tree doesn't match the "completed"
// form of a tree needed for a proof.
struct Entry {
    id: usize,
    hashes: [HashValue; HASHES_IN_BLOCK],
}

impl Entry {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(_level: usize, id: usize) -> Entry {
        Entry {
            id,
            hashes: [HashValue::new(); HASHES_IN_BLOCK],
        }
    }

    /// Push the hashes needed for a proof. The "id" parameter
    /// specifies the entry in this block that the proof checker
    /// will have generated, so we need to push the "partner" of
    /// this block, and so on up the tree.
    pub fn push(&self, hashes: &mut Vec<HashValue>, id: usize, _check: &[usize]) {
        let mut index = id;
        let mut base = 0;
        let mut interval = LEAVES_IN_BLOCK;

        for _ in 0..LEVELS_IN_BLOCK - 1 {
            let block = base + index;
            let partner = block ^ 1;

            if block > partner {
                hash_partial(&self.hashes[partner], &self.hashes[block])
            } else {
                hash_partial(&self.hashes[block], &self.hashes[partner])
            };

            hashes.push(self.hashes[partner]);

            index /= 2;
            base += interval;
            interval /= 2;
        }
    }

    /// Given an entry that has the non-empty leaf hashes added
    /// to it, generate the rest of the tree for this block.
    #[inline(always)]
    pub fn fill(&mut self) {
        for i in 0..HASHES_IN_BLOCK / 2 {
            self.hashes[LEAVES_IN_BLOCK + i] =
                hash_partial(&self.hashes[2 * i], &self.hashes[2 * i + 1]);
        }
    }

    /// Return the root hash of this block.
    #[inline(always)]
    pub fn root(&self) -> HashValue {
        let last = HASHES_IN_BLOCK - 1;
        self.hashes[last]
    }
}

// Provide the serialization help for the array of hashes in a block.
fn serialize_array<S, T>(array: &[T], serializer: S) -> StdResult<S::Ok, S::Error>
where
    S: Serializer,
    T: Serialize,
{
    array.serialize(serializer)
}

// Provide the deserialization helper for the hash array in a block.
fn deserialize_array<'de, D>(
    deserializer: D,
) -> StdResult<[HashValue; HASHES_IN_BLOCK], D::Error>
where
    D: Deserializer<'de>,
{
    let slice: Vec<HashValue> = Deserialize::deserialize(deserializer)?;

    if slice.len() != HASHES_IN_BLOCK {
        return Err(serde::de::Error::custom(format!(
            "The input slice has the wrong length:  {}",
            slice.len()
        )));
    }

    let result: [HashValue; HASHES_IN_BLOCK] = unsafe {
        let mut val = MaybeUninit::<[HashValue; HASHES_IN_BLOCK]>::uninit();

        copy_nonoverlapping(
            slice.as_ptr(),
            (*val.as_mut_ptr()).as_mut_ptr(),
            slice.len(),
        );

        val.assume_init()
    };
    Ok(result)
}

// A Merkle tree is represented by a collection of blocks. Blocks
// are used both in memory and on disk, and form a tree.
//
// A level-zero block contains HASHES_IN_BLOCK leaves, where each
// leaf corresponds to a transaction hash. The leaves then are
// hashed to form a binary tree. A level one block thus contains
// up to HASHES_IN_BLOCK interior nodes at that block's lowest level,
// with each such interior node being the parent of two level zero
// blocks.
#[repr(C)]
#[derive(Serialize, Deserialize)]
struct Block {
    header: BlockHeader,

    #[serde(serialize_with = "serialize_array")]
    #[serde(deserialize_with = "deserialize_array")]
    hashes: [HashValue; HASHES_IN_BLOCK],
}

impl fmt::Debug for Block {
    #[inline(always)]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.header.fmt(formatter)?;
        self.hashes[..].fmt(formatter)
    }
}

impl Block {
    #[inline(always)]
    fn new(level: u32, id: u64) -> Block {
        Block {
            header: BlockHeader::new(level, id),
            hashes: [HashValue::new(); HASHES_IN_BLOCK],
        }
    }

    // Set the hash value for a leaf for a block. When the last
    // available slot for leaves is full, form the upper levels of
    // the tree that fit in this block, and then set the checksum.
    fn set_hash(&mut self, hash_value: &HashValue) -> Result<()> {
        let index = self.header.valid_leaves as usize;

        if index >= LEAVES_IN_BLOCK {
            return Err(eg!("This block is full."));
        }

        if self.hashes[index] != HashValue::new() {
            return Err(eg!("That hash block is not empty."));
        }

        self.hashes[index] = *hash_value;
        self.header.valid_leaves += 1;

        // If the block is now full, form the subtree contained in it.
        // Also, set the checksum, as the block shouldn't change.
        if self.header.valid_leaves == LEAVES_IN_BLOCK as u16 {
            self.form_subtree();
            self.set_checksum();
        }

        Ok(())
    }

    // Compute the hashes that form the subtree represented by this
    // block. This is called when the block becomes full.
    fn form_subtree(&mut self) {
        let mut input = 0;

        for i in LEAVES_IN_BLOCK..HASHES_IN_BLOCK {
            let left = input;
            let right = input + 1;
            let hash = hash_pair(&self.hashes[left], &self.hashes[right]);

            self.hashes[i] = hash;

            input += 2;
        }
    }

    #[inline(always)]
    fn full(&self) -> bool {
        self.header.valid_leaves >= LEAVES_IN_BLOCK as u16
    }

    #[inline(always)]
    fn id(&self) -> usize {
        self.header.id as usize
    }

    #[inline(always)]
    fn level(&self) -> usize {
        self.header.level as usize
    }

    #[inline(always)]
    fn valid_leaves(&self) -> u64 {
        u64::from(self.header.valid_leaves)
    }

    // Return the hash that is the top level of the subtree
    // of this block, if the block is full. Otherwise, return
    // None.
    #[inline(always)]
    fn top_hash(&self) -> Option<&HashValue> {
        if self.full() {
            Some(&self.hashes[HASHES_IN_BLOCK - 1])
        } else {
            None
        }
    }

    // Return a slice representing the part of the block that is
    // checksummed, i.e., everything but the checksum area itself.
    #[inline(always)]
    fn as_checksummed_region(&self) -> &[u8] {
        unsafe {
            slice::from_raw_parts(
                (&self.header.header_mark as *const u32) as *const u8,
                mem::size_of::<Block>() - mem::size_of::<CheckBits>(),
            )
        }
    }

    // Compute a checksum for the block.
    #[inline(always)]
    fn compute_checksum(&self) -> [u8; CHECK_SIZE] {
        let digest = sha256::hash(self.as_checksummed_region());
        let mut result: [u8; CHECK_SIZE] = Default::default();

        result.clone_from_slice(&digest[0..CHECK_SIZE]);
        result
    }

    // Set the block check bits with the current checksum for the block.
    #[inline(always)]
    fn set_checksum(&mut self) {
        self.header.check_bits.bits = self.compute_checksum();
    }

    // Check the integrity of a block given an expected level and id.
    fn check(&self, level: usize, id: u64, disk_format: bool) -> Result<()> {
        // Check the checksum first, since a bad disk read would usually lead
        // to a bad checksum.
        if disk_format || self.full() {
            let hash = self.compute_checksum();

            if hash != self.header.check_bits.bits {
                return Err(eg!(format!(
                    "The header checksum for block {} ({} entries) at level {} is invalid.",
                    id,
                    self.valid_leaves(),
                    level
                )));
            }
        }

        // Validate the header so that we know that the overall
        // description is coherent.
        self.header.check(level, id).c(d!())?;

        // Check that the appropriate number of hash values has
        // been set.
        let limit = if !self.full() {
            self.header.valid_leaves as usize
        } else {
            HASHES_IN_BLOCK
        };

        for i in 0..limit {
            if self.hashes[i] == HashValue::new() {
                return Err(eg!(format!(
                    "Hash entry {} for block {} at level {} is invalid.",
                    i, id, level
                )));
            }
        }

        // The rest of the hashes should be in their initial state.
        for i in limit..HASHES_IN_BLOCK {
            if self.hashes[i] != HashValue::new() {
                return Err(eg!(format!(
                    "Hash entry {} for block {} at level {} was set, valid leaves {}.",
                    i,
                    id,
                    level,
                    self.valid_leaves()
                )));
            }
        }

        // If this level is full, the subtree should be valid. This check
        // is by far the most expensive.
        if self.full() {
            self.check_subtree().c(d!())?;
        }

        Ok(())
    }

    // Check the hashes of nodes formed inside this block. The block
    // must be full.
    fn check_subtree(&self) -> Result<()> {
        let mut input = 0;

        for i in LEAVES_IN_BLOCK..HASHES_IN_BLOCK {
            let left = &self.hashes[input];
            let right = &self.hashes[input + 1];
            let hash = hash_pair(left, right);

            if hash != self.hashes[i] {
                return Err(eg!(format!(
                    "hash[{}] in block {} at level {} is invalid.",
                    i,
                    self.id(),
                    self.level()
                )));
            }

            input += 2;
        }

        Ok(())
    }

    // Add the subtree for the given block into the proof. This code
    // handles full blocks. Here, "id" is the index within the block.
    fn push(&self, hashes: &mut Vec<HashValue>, id: usize, _check: &[usize]) {
        let mut index = id;
        let mut base = 0;
        let mut interval = LEAVES_IN_BLOCK;

        for _ in 0..LEVELS_IN_BLOCK - 1 {
            let block = base + index;
            let partner = block ^ 1;
            let hash = self.hashes[partner];

            if block > partner {
                hash_partial(&self.hashes[partner], &self.hashes[block])
            } else {
                hash_partial(&self.hashes[block], &self.hashes[partner])
            };

            hashes.push(hash);

            index /= 2;
            base += interval;
            interval /= 2;
        }
    }

    /// return the root hash
    #[inline(always)]
    pub fn root(&self) -> HashValue {
        let last = HASHES_IN_BLOCK - 1;
        self.hashes[last]
    }

    // Return a pointer to the raw bytes of the block for I/O.
    #[inline(always)]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            slice::from_raw_parts(
                (self as *const Block) as *const u8,
                mem::size_of::<Block>(),
            )
        }
    }
}

// Implement covered division, that is, round up fractions when dividing.
#[inline(always)]
fn covered(numerator: u64, denominator: u64) -> u64 {
    (numerator + denominator - 1) / denominator
}

// This structure is used only to pass data between internal
// routines when reading data from files.
struct LevelState {
    level: usize,
    leaves_at_this_level: u64,
    previous_leaves: u64,
    previous_blocks: u64,
    check_lower: bool,
}

// Compute the expected number of leaves in the next layer of the
// tree given the number of blocks at the current level, and whether
// the last block at the current layer is full.
#[inline(always)]
fn next_level_leaves(blocks: u64, last_full: bool) -> u64 {
    let full_blocks = if last_full { blocks } else { blocks - 1 };
    full_blocks / 2
}

/// Defines an append-ony Merkle tree that eventually will support
/// a sparse in-memory representation. We will need to use Box
/// for the blocks at that point.
#[derive(Debug, Serialize, Deserialize)]
pub struct AppendOnlyMerkle {
    entry_count: u64, // total entries in the tree
    entries_on_disk: u64,
    path: String, // the disk path for the stable store
    blocks: Vec<Vec<Block>>,

    #[serde(skip_serializing, skip_deserializing)]
    files: Vec<File>,

    #[serde(skip_serializing, skip_deserializing)]
    blocks_on_disk: Vec<u64>, // the number of entries on stable store
}

// When a tree is dropped, write it to disk.
impl Drop for AppendOnlyMerkle {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = self.write() {
            panic!("AppendOnlyMerkle: drop failed, err = {}", e);
        }
    }
}

impl AppendOnlyMerkle {
    // This constructor is private. Use open or create to get a
    // Merkle tree.
    #[inline(always)]
    fn new(path: &str, file: File) -> AppendOnlyMerkle {
        AppendOnlyMerkle {
            entry_count: 0,
            entries_on_disk: 0,
            path: path.to_string(),
            blocks: vec![Vec::new()],
            files: vec![file],
            blocks_on_disk: vec![0],
        }
    }

    /// Open an existing Merkle tree and and call read_files to
    /// initialize it from the contents on disk.
    ///
    /// # Argument
    ///
    /// * `path` - a string specifying the path to the base file
    #[inline(always)]
    pub fn open(path: &str) -> Result<AppendOnlyMerkle> {
        let check_path = OpenOptions::new().read(true).write(true).open(path);

        match check_path {
            Ok(file) => {
                let mut result = AppendOnlyMerkle::new(path, file);

                result.open_files().c(d!())?;
                result.read_files(false).c(d!())?;
                Ok(result)
            }
            Err(x) => Err(eg!(x)),
        }
    }

    /// Create a new Merkle tree at the given path. This routine returns
    /// an error if the tree exists.
    pub fn create(path: &str) -> Result<AppendOnlyMerkle> {
        let check_path = OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(path);

        match check_path {
            Ok(file) => {
                let result = AppendOnlyMerkle::new(path, file);

                // Remove any files left over from another tree with the same name.
                for i in 1..MAX_BLOCK_LEVELS {
                    let path = result.file_path(i);
                    let _ = fs::remove_file(&path);
                }

                Ok(result)
            }
            Err(x) => Err(eg!(x)),
        }
    }

    /// Rebuild a tree using only the file containing the leaves.
    ///
    /// # Argument
    ///
    /// `path` - the path to the Merkle tree
    ///
    /// If a previous rebuild has been attempted and not cleaned
    /// up by the upper levels, this rebuild will fail, whether or
    /// not the previous rebuild was successful. All files related
    /// to the tree with the suffix "rebuild_ext()" must be removed.
    /// The AppendOnlyMerkle code does not remove files:  the caller
    /// is responsible for such cleanup.
    ///
    /// If the rebuild is successful, a valid tree structure is
    /// returned, and this tree is guaranteed to be fully synchronized
    /// to disk.
    pub fn rebuild(path: &str) -> Result<AppendOnlyMerkle> {
        let input = OpenOptions::new().read(true).open(&path).c(d!())?;
        let ext = AppendOnlyMerkle::rebuild_ext();
        let save = path.to_owned() + &ext;

        if path::Path::new(&save).exists() {
            return Err(eg!(format!("Rebuild path {} already exists.", save)));
        }

        // Rename the level zero file out of the way and then create
        // a new one.
        fs::rename(&path, &save).c(d!())?;

        let output = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&path)
            .c(d!())?;

        let mut tree = AppendOnlyMerkle::new(&path, output);
        tree.rebuild_internal(input).c(d!())?;
        Ok(tree)
    }

    #[inline(always)]
    fn rebuild_ext() -> String {
        "-base".to_string()
    }

    #[inline(always)]
    fn rebuild_extension(&self) -> String {
        AppendOnlyMerkle::rebuild_ext()
    }

    // The rebuild method creates a skeleton tree that is empty. Now do
    // the work of recreating all the blocks for all the files.
    fn rebuild_internal(&mut self, mut input: File) -> Result<()> {
        // Compute the number of complete blocks there.
        let file_size = input.seek(SeekFrom::End(0)).c(d!())?;
        let block_count = file_size / BLOCK_SIZE as u64;

        input.seek(SeekFrom::Start(0)).c(d!())?;
        self.files[0] = input;

        let mut entries = 0;
        let mut last_block_full = false;

        // Read the level zero hashes if we can. If any level zero block
        // is corrupted, discard it and any following blocks.
        for block_id in 0..block_count {
            let block = if let Ok(b) = self
                .read_block(0, block_id, block_id == block_count - 1)
                .c(d!())
            {
                b
            } else {
                break;
            };

            last_block_full = block.full();
            entries += block.valid_leaves();
            self.blocks[0].push(block);
        }

        if entries == 0 {
            let _ = fs::remove_file(self.file_path(0));

            // Level 0 file contails no valid leaves, then release level 0 initial resources:
            // self.files / self.blocks / self.blocks_on_disk
            self.files.remove(0);
            self.blocks.remove(0);
            self.blocks_on_disk.remove(0);

            return Err(eg!("The level 0 file contains no valid leaves."));
        }

        // Set the size of the tree.
        self.entry_count = entries;

        // Now recover the upper level files.
        let mut leaves_at_this_level = next_level_leaves(block_count, last_block_full);
        let mut level = 1;

        // For each upper level that exists, move the old file
        // out of the way and create a new one, then reconstruct
        // the blocks in memory. The blocks are flushed to disk
        // if and when reconstruction succeeds.
        while leaves_at_this_level > 0 {
            let path = self.file_path(level);
            let ext = self.rebuild_extension();
            let _ = fs::rename(&path, path.to_owned() + &ext);

            let file = OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .truncate(true)
                .open(&path)
                .c(d!())?;

            self.push_file(file);
            let block_count = covered(leaves_at_this_level, LEAVES_IN_BLOCK as u64);
            last_block_full = self.rebuild_level(level, block_count).c(d!())?;
            leaves_at_this_level = next_level_leaves(block_count, last_block_full);
            level += 1;
        }

        // Rename any other files that might be in the way.
        for i in level..MAX_BLOCK_LEVELS {
            let path = self.file_path(i);
            let ext = self.rebuild_extension();
            let _ = fs::rename(&path, path.to_owned() + &ext);
        }

        // Okay, we have recovered all the upper level files. Point the
        // level zero file at the correct path.
        self.files[0] = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&self.path)
            .c(d!())?;

        self.check().c(d!())?;
        self.write().c(d!())?;

        Ok(())
    }

    // Rebuild all the blocks for the given level of the tree.
    fn rebuild_level(&mut self, level: usize, block_count: u64) -> Result<bool> {
        let mut last_block_full = false;

        for block_id in 0..block_count {
            let block = match self.reconstruct(level, block_id) {
                Ok(b) => b,
                Err(x) => {
                    return Err(eg!(format!(
                        "Reconstruction of block {} at level {} failed:  {}",
                        block_id, level, x
                    )));
                }
            };

            last_block_full = block.full();
            self.blocks[level].push(block);
        }

        Ok(last_block_full)
    }

    // Return the value of the hash for the given transaction id.
    // This function currently is only for testing.
    #[cfg(test)]
    fn leaf(&self, index: usize) -> HashValue {
        if index as u64 > self.entry_count {
            return HashValue::new();
        }

        let block_id = index / LEAVES_IN_BLOCK;
        let block_index = index % LEAVES_IN_BLOCK;

        self.blocks[0][block_id].hashes[block_index]
    }

    /// Make a deserialized tree ready for use. The derived
    /// deserializer doesn't open files or set up a correct
    /// vector for the blocks_on_disk field. Do that here,
    /// if possible.
    pub fn finish_deserialize(&mut self) -> Result<()> {
        self.blocks_on_disk = Vec::new();
        self.files = Vec::new();

        for i in 0..self.blocks.len() {
            let file_check = OpenOptions::new()
                .read(true)
                .write(true)
                .create_new(true)
                .open(&self.file_path(i));

            match file_check {
                Ok(file) => {
                    self.files.push(file);
                    self.blocks_on_disk.push(0);
                }
                Err(e) => {
                    return Err(eg!(format!(
                        "Open failed for {}:  {}",
                        self.file_path(i),
                        e
                    )));
                }
            }
        }

        Ok(())
    }

    // Open the files for each level of the tree from 1 upward.
    fn open_files(&mut self) -> Result<()> {
        let mut i = 1;

        loop {
            let path = self.file_path(i);
            let result = OpenOptions::new().read(true).write(true).open(&path);

            match result {
                Err(x) => {
                    if x.kind() == ErrorKind::NotFound {
                        return Ok(());
                    } else {
                        return Err(eg!(x));
                    }
                }
                Ok(file) => {
                    self.push_file(file);
                }
            }

            i += 1;

            // Enforce an arbitrary limit to the number of levels we can support.
            if i > MAX_BLOCK_LEVELS {
                return Err(eg!(format!(
                    "The tree at {} has more than {} levels.",
                    self.path,
                    i - 1
                )));
            }
        }
    }

    // Generate the path for the file for the given level.
    #[inline(always)]
    fn file_path(&self, level: usize) -> String {
        let path = self.path.clone();

        let extension = if level > 0 {
            format!(".{}", level)
        } else {
            "".to_string()
        };

        path + &extension
    }

    // Add a level to the tree's data structures. This function is called
    // to prepare for reading a file during open or when adding a new level.
    #[inline(always)]
    fn push_file(&mut self, file: File) {
        self.files.push(file);
        self.blocks_on_disk.push(0);
        self.blocks.push(Vec::new());
    }

    // Read the disk data into the Merkle tree.
    fn read_files(&mut self, check_lower: bool) -> Result<()> {
        let mut state = LevelState {
            level: 0,
            leaves_at_this_level: 0,
            previous_leaves: 0,
            previous_blocks: 0,
            check_lower,
        };

        // Read the file for each level of the tree.
        for level in 0..self.files.len() {
            state.level = level;
            self.read_level(&mut state).c(d!())?;
        }

        // Remove any leftover files. open_files() stops at the first missing
        // file, but there might be others there at higher levels.
        for i in self.files.len()..MAX_BLOCK_LEVELS {
            let path = self.file_path(i);
            let _ = fs::remove_file(&path);
        }

        Ok(())
    }

    // Read all the blocks from a single data file. Each data file
    // represents one level of the tree.
    #[allow(clippy::comparison_chain)]
    fn read_level(&mut self, state: &mut LevelState) -> Result<()> {
        let level = state.level;

        let file_size = if let Ok(n) = self.files[level].seek(SeekFrom::End(0)) {
            n
        } else {
            return self.recover_file(level);
        };

        if file_size % BLOCK_SIZE as u64 != 0 {
            return self.recover_file(level);
        }

        if self.files[level].seek(SeekFrom::Start(0)).is_err() {
            return self.recover_file(level);
        }

        // Compute the number of blocks that should exist at this
        // level, based on the number of leaves, and check that the
        // file size matches this expectation.
        let block_count = file_size / BLOCK_SIZE as u64;
        let expected = covered(state.leaves_at_this_level, LEAVES_IN_BLOCK as u64);

        if level != 0 && block_count != expected {
            return self.recover_file(level);
        }

        let mut last_block_full = true;
        let mut entries = 0;

        // Read each block, if possible.
        for i in 0..block_count {
            let last_block = i == block_count - 1;

            // Read one block and do some basic integrity checks.
            match self.read_block(level, i, last_block) {
                Ok(block) => {
                    last_block_full = block.full();
                    entries += block.valid_leaves();

                    // If we are above level zero, check that the hashes we
                    // have read agree with what's in the lower level.
                    if state.check_lower && level > 0 {
                        let lower_index = i as usize * LEAVES_IN_BLOCK * 2;
                        let lower_list = &self.blocks[level - 1];

                        self.check_lower(&block, lower_list, lower_index)?;
                    }

                    self.blocks[level].push(block);
                }
                Err(x) => {
                    // Loss of a level zero block is irrecoverable.
                    if level == 0 {
                        return Err(eg!(x));
                    }

                    // Rebuild the block if we can.
                    match self.reconstruct(level, i) {
                        Ok(block) => {
                            self.rewrite_block(&block).c(d!())?;
                            last_block_full = block.full();
                            entries += block.valid_leaves();
                            self.blocks[level].push(block);
                        }
                        Err(x) => {
                            return Err(eg!(x));
                        }
                    }
                }
            }
        }

        // If the size doesn't match the expected number of
        // leaves, try a recovery.
        if level > 0 && entries != state.leaves_at_this_level {
            return self.recover_file(level);
        }

        // If this is level zero, set the tree size.
        if level == 0 {
            self.entry_count = entries;
            self.entries_on_disk = entries;
        }

        self.blocks_on_disk[level] = block_count;

        // Compute the number of entries to expect at the next level as a
        // consistency check.
        state.leaves_at_this_level = next_level_leaves(block_count, last_block_full);

        state.previous_leaves = entries;
        state.previous_blocks = block_count;

        // So if there are leaves at the next level, and there's not a file,
        // we have a problem...
        let last_level = level == self.files.len() - 1;

        if last_level && state.leaves_at_this_level > 0 {
            return self.recover_file(level + 1);
        }

        Ok(())
    }

    // Recover the contents of a file by rebuilding the level it
    // represents. Open and truncate the file, as necessary.
    fn recover_file(&mut self, level: usize) -> Result<()> {
        if level == 0 {
            return Err(eg!("The level 0 file is corrupted."));
        }

        let path = self.file_path(level);

        let _ = fs::remove_file(&path);

        let result = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&path);

        let file = match result {
            Err(x) => {
                return Err(eg!(x));
            }
            Ok(file) => file,
        };

        // Add this file to the vector. We might need to
        // push a new entry into all of the Vecs.
        if level == self.files.len() {
            self.push_file(file);
        } else {
            self.files[level] = file;
            self.blocks[level] = Vec::new();
            self.blocks_on_disk[level] = 0;
        }

        let entry_count = self.blocks[level - 1].len() / 2;
        let block_count = covered(entry_count as u64, LEAVES_IN_BLOCK as u64);

        self.rebuild_level(level, block_count).c(d!())?;
        Ok(())
    }

    // Rewrite the given block to disk. This routine is called when a read
    // operation fails and the block has been reconstructed. If this routine
    // returns success, it leaves the file offset pointing at the next block
    // in the file.
    fn rewrite_block(&mut self, block: &Block) -> Result<()> {
        let offset = block.id() as u64 * BLOCK_SIZE as u64;
        let level = block.level();

        // Seek to the offset to which we will write.
        match self.files[level].seek(SeekFrom::Start(offset)) {
            Ok(n) => {
                if n != offset {
                    // Log error.
                    return Err(eg!(format!(
                        "Seek failed at level {}, block {} for rewrite:  {} vs {}",
                        level,
                        block.id().commas(),
                        n,
                        offset
                    )));
                }
            }
            Err(x) => {
                return Err(eg!(format!(
                    "Seek failed  at level {}, block {} for rewrite:  {}",
                    level,
                    block.id().commas(),
                    x
                )));
            }
        }

        match self.files[level].write_all(block.as_bytes()) {
            Ok(_) => Ok(()),
            Err(x) => Err(eg!(format!(
                "I/O failed for rewrite at level {}, block {}:  {}",
                level,
                block.id().commas(),
                x
            ))),
        }
    }

    // Reconstruct a block. Eventually, this routine might need to read
    // from the disk when we support paging, so allow an error return.
    fn reconstruct(&mut self, level: usize, block_id: u64) -> Result<Block> {
        if level == 0 {
            return Err(eg!("Level zero cannot be reconstructed."));
        }

        let mut lower_index = block_id as usize * LEAVES_IN_BLOCK * 2;
        let mut block = Block::new(level as u32, block_id);

        let last_lower = self.blocks[level - 1].len();

        // Go through as many hash pairs as there are. There are
        // "last_lower" blocks at "level" - 1, and we need two of
        // them to form a hash pair, so check against last_lower - 1.
        while lower_index < last_lower - 1 && !block.full() {
            let left = self.blocks[level - 1][lower_index].top_hash();
            let right = self.blocks[level - 1][lower_index + 1].top_hash();

            if let (Some(left), Some(right)) = (left, right) {
                block.set_hash(&hash_pair(left, right)).c(d!())?;
            } else {
                break;
            }

            lower_index += 2;
        }

        block.set_checksum();
        Ok(block)
    }

    /// Add a new level zero entry to the Merkle tree. This leaf will represent
    /// an actual transaction. The transaction id that is returned is used when
    /// generating a proof.
    ///
    /// # Argument
    ///
    /// * `hash_value` - a HashValue structure for the new transaction
    pub fn append_hash(&mut self, hash_value: &HashValue) -> Result<u64> {
        if self.entry_count == 0 {
            if !self.blocks[0].is_empty() {
                return Err(eg!(format!(
                    "Level zero should be empty, but it has {} blocks",
                    self.blocks[0].len()
                )));
            }

            self.blocks[0].push(Block::new(0, 0));
            self.blocks[0][0].set_hash(&hash_value).c(d!())?;
            self.entry_count = 1;
            return Ok(0);
        }

        // Loop up the levels of the tree, adding entries as needed.
        //
        // We might need to add a level, so include the next level in the count.
        // The loop will terminate via a break if there is no data for a new level.
        let levels = self.blocks.len() + 1;

        let mut current_hash = *hash_value;

        for level in 0..levels {
            if level == levels - 1 {
                self.add_level().c(d!())?;
            }

            // Pull what we need from the tree. That's the current
            // non-empty block, and the hash of the left subtree
            // for that block, if it has a left sub-stree.
            let items = {
                let block_list = &mut self.blocks[level];

                if block_list.last().c(d!())?.full() {
                    let block_id = block_list.len() as u64;
                    let block = Block::new(level as u32, block_id);

                    block_list.push(block);
                }

                let index = block_list.len() - 1;

                // Get the hash at the top of the older sibling of the
                // current node, if it exists. If it does exist, it will
                // be a left subtree, so the current index must be odd.
                let prev_top = if index & 1 != 0 {
                    let top_hash = match block_list[index - 1].top_hash() {
                        Some(x) => *x,
                        None => {
                            return Err(eg!(format!(
                                "No top hash for block {} at level {}",
                                index - 1,
                                level
                            )));
                        }
                    };

                    Some(top_hash)
                } else {
                    None
                };

                (&mut block_list[index], prev_top)
            };

            let (block, prev) = items;

            if let Err(x) = block.set_hash(&current_hash) {
                return Err(eg!(format!("The tree is corrupted:  set_hash:  {}", x)));
            }

            // If the block is full, and all the previous blocks are
            // on disk, write this block to disk and set the number
            // of blocks on disk to the correct value. If the block
            // already has been written by a tree.write operation, the
            // value in blocks_on_disk will remain the same. If the
            // tree.reset_disk method has been invoked, we can't write
            // this block without adding a lot of logic, so just wait
            // for the next tree.write invocation.
            if block.full() && self.blocks_on_disk[level] >= block.id() as u64 {
                let se = self.files[level]
                    .seek(SeekFrom::Start((block.id() * BLOCK_SIZE) as u64));

                if se.is_ok() {
                    let we = self.files[level].write_all(block.as_bytes());

                    if we.is_ok() {
                        self.blocks_on_disk[level] = block.id() as u64 + 1;
                    }
                }
            }

            // If this node of the tree is not full or doesn't
            // have a corresponding left subtree, we're done.
            if !block.full() || block.id() & 1 == 0 {
                break;
            }

            // Okay, we have another hash to add to the tree. Compute it.
            let left = &prev.c(d!())?;
            let right = block.top_hash().c(d!())?;

            current_hash = hash_pair(left, right);
        }

        // The entry count is for level zero (transaction) entries
        // only.
        self.entry_count += 1;
        Ok(self.entry_count - 1)
    }

    // Add a new level to the tree. This routine is called only
    // when there's data for the new layer, so allocate a block here.
    #[inline(always)]
    fn add_level(&mut self) -> Result<()> {
        let level = self.blocks.len();
        let path = self.file_path(level);

        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(path)
            .c(d!())?;

        self.push_file(file);

        self.blocks[level].push(Block::new(level as u32, 0));
        Ok(())
    }

    /// Append a transaction to the Merkle tree. It is encoded
    /// as a UTF-8 string. The returned transaction_id is required
    /// when generating a proof.
    ///
    /// # Argument
    ///
    /// * `value` - the string to insert
    #[inline(always)]
    pub fn append_str(&mut self, value: &str) -> Result<u64> {
        let mut hash_value = HashValue::new();

        let digest = sha256::hash(value.as_ref());

        hash_value.hash.clone_from_slice(digest.as_ref());
        self.append_hash(&hash_value)
    }

    /// Generate a proof given an index into the tree.
    ///
    /// This routine probably will end up taking a tree state id as
    /// an input so that verifiers don't need to keep a complete
    /// copy of the tree.
    ///
    /// # Arguments
    ///
    /// * `transaction_id`  - the transaction id for which a proof is required
    /// * `tree_version`    - the version of the tree for which the proof is wanted
    pub fn generate_proof(&self, transaction_id: u64, version: u64) -> Result<Proof> {
        if transaction_id >= self.entry_count {
            return Err(eg!(format!(
                "That transaction id ({}) does not exist.",
                transaction_id
            )));
        }

        if version != self.entry_count {
            return Err(eg!("Versioning is not yet supported."));
        }

        // Generate a dictionary of all the blocks that
        // would change or be added to make a complete
        // Merkle tree.
        let dictionary = self.generate_tree_completion();

        let mut level = 0;
        let mut hashes = Vec::new();
        let mut id = transaction_id as usize;
        let mut root;

        // Loop through each level of the tree building
        // the list of hashes.
        loop {
            root = self.append_proof_hashes(&mut hashes, level, id, &dictionary);

            if level == dictionary.max_level() {
                break;
            }

            // Now append the hash of the partner (sibling) for this block,
            // if one exists, or the empty hash.
            let block_id = id / LEAVES_IN_BLOCK;
            self.push_partner_hash(&mut hashes, level, block_id, &dictionary);

            level += 1;
            id /= LEAVES_IN_BLOCK * 2;
        }

        let result = Proof {
            version: PROOF_VERSION,
            ledger: self.path.clone(),
            state: self.entry_count,
            time: Utc::now().timestamp(),
            tx_id: transaction_id,
            root_hash: root,
            hash_array: hashes,
        };

        Ok(result)
    }

    /// Get a proof for the given transaction id from the underlying
    /// AppendOnlyMerkle object.
    ///
    /// # Arguments
    ///
    /// * `transaction` - the Merkle tree id for the transaction
    /// * `state` - the Merkle tree state for which the proof is wanted,
    ///              or zero, for the current state.
    pub fn get_proof(&self, transaction: u64, state: u64) -> Result<Proof> {
        let proof_state = if state != 0 { state } else { self.total_size() };

        if transaction >= proof_state {
            return Err(eg!(format!(
                "That id ({}) is not valid for state {}.",
                transaction.commas(),
                state
            )));
        }

        if !self.validate_transaction_id(transaction) {
            return Err(eg!(format!(
                "That id ({}) is not valid.",
                transaction.commas()
            )));
        }

        self.generate_proof(transaction, proof_state)
    }

    // Append the hash of the partner of the current block, or
    // the empty hash, if this block has no sibling in the tree.
    fn push_partner_hash(
        &self,
        hashes: &mut Vec<HashValue>,
        level: usize,
        block_id: usize,
        dictionary: &Dictionary,
    ) {
        let partner_id = block_id ^ 1;
        let block_hash = self.find_block_root(dictionary, level, block_id);
        let partner_hash = self.find_block_root(dictionary, level, partner_id);

        // Compute the hash of the parent of the block.
        // This is useful for debugging.
        if 0 == partner_id & 1 {
            hash_partial(&partner_hash, &block_hash)
        } else {
            hash_partial(&block_hash, &partner_hash)
        };

        hashes.push(partner_hash);
    }

    // Find the hash at the root of the given block. The
    // block might be in the dictionary, or not.
    #[inline(always)]
    fn find_block_root(
        &self,
        dictionary: &Dictionary,
        level: usize,
        block_id: usize,
    ) -> HashValue {
        let empty_hash = HashValue::new();

        match dictionary.get(level, block_id) {
            Some(entry) => entry.root(),
            None => {
                if level >= self.blocks.len() || block_id >= self.blocks[level].len() {
                    empty_hash
                } else {
                    let block = &self.blocks[level][block_id];
                    block.root()
                }
            }
        }
    }

    // The basic tree code only computes the upper tree elements
    // when a block becomes full. When we produce a proof, we need
    // to work on a "complete" tree. In a complete tree, nodes with
    // only one child contain a hash of that child's value. This
    // change from the working form of a tree ripples up to the root.
    //
    // The implementation of this modification is represented by a
    // dictionary that holds an entry for each block modified (or
    // created) by this rippling.
    //
    fn generate_tree_completion(&self) -> Dictionary {
        let empty_hash = HashValue::new();

        let mut dictionary = Dictionary::new();
        let mut level = 0;
        let mut carried_hash = HashValue::new();
        let mut carried = false;
        let mut solitary_block = false;

        // Iterate over each level of the tree that's present
        // in the working copy.
        while level < self.blocks.len() {
            let length = self.blocks[level].len();
            let last_id = length - 1;
            let last_block = &self.blocks[level][last_id];
            let count = last_block.valid_leaves() as usize;

            //
            // We have three important cases to consider:
            //   1) The current block is only partially full. Create
            //      a dictionary entry for it. Append the carried
            //      hash from the lower level to it. If we are at
            //      level zero, the carried hash will be the empty
            //      hash.
            //   2) The block is full, but we have a valid carried
            //      hash. Create a new entry and add it to the
            //      directory.
            //   3) There's no carried hash, the block is full, and
            //      the length of the block list at this level is
            //      odd. The carried hash becomes the hash of the
            //      root of the last block in this chain.
            //
            if count != LEAVES_IN_BLOCK {
                let mut entry = Entry::new(level, last_id);

                entry.hashes[0..count].clone_from_slice(&last_block.hashes[0..count]);
                entry.hashes[count] = carried_hash;
                entry.fill();
                carried_hash = entry.root();
                carried = true;
                dictionary.insert(level, entry);
                solitary_block = length == 1;

                // Compute the hash to carry upward. That is the
                // hash of the root of this block and the root of
                // its sibling, if it has a sibling. Otherwise,
                // the carried hash is the hash of the root of this
                // block and the empty hash.
                if last_id & 1 == 0 {
                    carried_hash = hash_partial(&carried_hash, &empty_hash);
                } else {
                    let left = self.blocks[level][last_id - 1].root();
                    carried_hash = hash_partial(&left, &carried_hash);
                }
            } else if carried_hash != empty_hash {
                let mut entry = Entry::new(level, length);
                let new_block_id = length;

                entry.hashes[0] = carried_hash;
                entry.fill();
                carried_hash = entry.root();
                carried = true;
                dictionary.insert(level, entry);
                solitary_block = level > self.blocks.len();

                // Similarly to the previous case, compute the
                // carried hash.
                if new_block_id & 1 == 0 {
                    carried_hash = hash_partial(&carried_hash, &empty_hash);
                } else {
                    let left = self.blocks[level][new_block_id - 1].root();
                    carried_hash = hash_partial(&left, &carried_hash);
                }
            } else if !carried && length % 2 == 1 {
                carried = true;
                carried_hash = hash_partial(&last_block.root(), &empty_hash);
                solitary_block = false;
            } else if !carried {
                solitary_block = length == 1;
            }

            level += 1;
        }

        //
        // Okay, we are at the top of the tree. We have a
        // couple of cases:
        //   1) The top of the tree is a solitary block.
        //      We have nothing to do.
        //   2) We have a carried hash. In this case, add
        //      a new dictionary entry to form the top of the
        //      tree.
        //
        if solitary_block {
            // The top of the tree is a single block. There's nothing
            // to do. The hash of the root of this block is the
            // hash of the completed tree.
        } else if carried_hash != empty_hash {
            let mut entry = Entry::new(level, 0);
            entry.hashes[0] = carried_hash;
            entry.fill();
            dictionary.insert(level, entry);
        }

        dictionary
    }

    //
    // Append the hashes for a given level in the block
    // structure. This amounts to adding the hierarchy
    // for one block, excluding the root of the block
    // itself. We either use a complete block from the
    // working tree, or we use a fake block from the tree
    // completion we generated.
    //
    // We return the root hash of the block since it might
    // be the root of the tree, and it's easy to get it.
    //
    fn append_proof_hashes(
        &self,
        hashes: &mut Vec<HashValue>,
        level: usize,
        id: usize,
        dictionary: &Dictionary,
    ) -> HashValue {
        let block_id = id / LEAVES_IN_BLOCK;
        let block_index = id % LEAVES_IN_BLOCK;
        let last = HASHES_IN_BLOCK - 1;

        let block_root_hash = match dictionary.get(level, block_id) {
            Some(entry) => {
                entry.push(hashes, block_index, &[]);
                entry.hashes[last]
            }
            None => {
                let block = &self.blocks[level][id / LEAVES_IN_BLOCK];
                block.push(hashes, block_index, &[]);
                block.hashes[last]
            }
        };

        block_root_hash
    }

    /// Compute the root hash of the Merkle tree.
    #[inline(always)]
    pub fn get_root_hash(&self) -> HashValue {
        if self.entry_count == 0 {
            return HashValue::default();
        }

        let proof = self.generate_proof(0, self.entry_count).unwrap();
        proof.root_hash
    }

    /// Check that a transaction id actually is present in the
    /// Merkle tree.
    #[inline(always)]
    pub fn validate_transaction_id(&self, transaction_id: u64) -> bool {
        transaction_id < self.entry_count
    }

    // Return the number of transaction entries in the tree.
    #[inline(always)]
    fn total_size(&self) -> u64 {
        self.entry_count
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn state(&self) -> u64 {
        self.entry_count
    }

    /// Save the tree to disk.
    /// At some point, flushes for transactional semantics might be important.
    pub fn write(&mut self) -> Result<()> {
        let mut entries_at_this_level = self.entry_count;

        // Write each block level of the tree to its file.
        for level in 0..self.blocks.len() {
            let total_blocks = covered(entries_at_this_level, LEAVES_IN_BLOCK as u64);

            if total_blocks != self.blocks[level].len() as u64 {
                return Err(eg!(format!(
                    "Level {} has {} blocks, but {} were expected",
                    level,
                    self.blocks[level].len(),
                    total_blocks
                )));
            }

            // Set the block at which to start writing. Always rewrite the
            // last disk block at this level (if any) because it might have
            // changed. No other block can change.
            let disk_block_count = self.blocks_on_disk[level] as usize;

            let start_block = if disk_block_count == 0 {
                disk_block_count
            } else {
                disk_block_count - 1
            };

            // Seek to the offset where we hope to put the block.
            // With some luck, this will help us recover from a
            // transient disk error.
            let start_offset = start_block as u64 * BLOCK_SIZE as u64;

            match self.files[level].seek(SeekFrom::Start(start_offset)) {
                Err(x) => {
                    return Err(eg!(x));
                }
                Ok(n) => {
                    if n != start_offset {
                        return Err(eg!(format!(
                            "A seek to {} returned {}.",
                            start_offset, n
                        )));
                    }
                }
            }

            let mut last_block_full = true;

            // Loop over each block on this level that needs to be sent to disk.
            for i in start_block as u64..total_blocks {
                let block = &mut self.blocks[level][i as usize];

                // Set the checksum if needed.
                if !block.full() {
                    block.set_checksum();
                }

                // Check consistency before writing the block to disk.
                if let Err(x) = block.check(level, i, true) {
                    return Err(eg!(x));
                }

                if i < total_blocks - 1 && !block.full() {
                    return Err(eg!(format!(
                        "Block {} at level {} should be full.",
                        i, level
                    )));
                }

                let result = self.files[level].write_all(block.as_bytes());

                if let Err(x) = result {
                    return Err(eg!(x));
                }

                last_block_full = block.full();
            }

            // Sync the file to detect any errors and give us a better shot
            // at decent semantics.
            let result = self.files[level].sync_all();

            // If there's an I/O error, truncate the file to try to get rid
            // of any possible bad blocks.
            if let Err(x) = result {
                let _ = self.files[level].set_len(0);
                self.blocks_on_disk[level] = 0;
                return Err(eg!(x));
            }

            // Save the number of blocks we have written to disk and
            // compute the entries at the next level.
            self.blocks_on_disk[level] = total_blocks;
            entries_at_this_level = next_level_leaves(total_blocks, last_block_full);
        }

        self.entries_on_disk = self.entry_count;
        Ok(())
    }

    /// Peform a consistency check of the disk representation of the tree.
    pub fn check_disk(&mut self, flushed: bool) -> Result<()> {
        let mut entries_at_this_level = self.entry_count;
        let mut lower = Vec::new();

        // Check the blocks at each level.
        for level in 0..self.blocks.len() {
            // First, get the file size check it against expectations.
            let disk_bytes = self.files[level].seek(SeekFrom::End(0)).c(d!())?;

            let blocks_on_disk = self.blocks_on_disk[level];
            let expected_size = blocks_on_disk * BLOCK_SIZE as u64;

            if disk_bytes != expected_size {
                return Err(eg!(format!(
                    "check_disk:  The file size ({}) at level {} should be {}.",
                    disk_bytes, level, expected_size
                )));
            }

            // If the disk image is up to date, check that the number
            // of blocks on disk and in the list match.
            let list_length = self.blocks[level].len() as u64;

            if flushed && blocks_on_disk != list_length {
                return Err(eg!(format!(
                    "check_disk:  The count {} at level {} should be {}.",
                    blocks_on_disk, level, list_length
                )));
            }

            if let Err(x) = self.files[level].seek(SeekFrom::Start(0)) {
                return Err(eg!(format!("check_disk:  The read seek failed:  {}", x)));
            }

            let mut entry_count = 0_u64;
            let mut last_block_full = true;
            let mut current = Vec::new();

            // Check each block on disk.
            for i in 0..blocks_on_disk {
                let last = i == blocks_on_disk - 1;

                // Read the next block and do some consistency checks.
                match self.read_block(level, i, last) {
                    Ok(block) => {
                        last_block_full = block.full();
                        entry_count += block.valid_leaves();

                        // If we have a lower level, check that the hashes in it
                        // match the hashes we have in the current level.
                        if !lower.is_empty() {
                            let lower_index = i as usize * LEAVES_IN_BLOCK * 2;

                            self.check_lower(&block, &lower, lower_index).c(d!())?;
                        }

                        current.push(block);
                    }
                    Err(x) => {
                        return Err(eg!(format!("check_disk:  A read failed:  {}", x)));
                    }
                }
            }

            lower = current;

            // Check that the disk contents match the in-memory
            // contents if the memory has been flushed.
            if flushed && entry_count != entries_at_this_level {
                return Err(eg!(format!(
                    "check_disk:  The entry counts ({}, {}) \
                           at level {} didn't match.",
                    entry_count, entries_at_this_level, level
                )));
            }

            // The first time through, we compare the in-memory entry count to the
            // count from disk, but for further iterations, there's no added value,
            // so just predict the number of blocks to expect based on the count on
            // disk. This value will be ignored unless the on-disk tree is expected
            // to match the in-memory version.
            entries_at_this_level = next_level_leaves(blocks_on_disk, last_block_full);

            let last_level = level == self.blocks.len() - 1;

            if last_level && entries_at_this_level > 0 {
                return Err(eg!(format!(
                    "There is at least one missing file (for level {}).",
                    level + 1
                )));
            }
        }

        Ok(())
    }

    // Read a block from disk and return its memory representation. Currently,
    // that is the same as the bytes on disk. This routine assumes that the
    // file offset is pointing to the block to be read.
    fn read_block(&mut self, level: usize, id: u64, last: bool) -> Result<Block> {
        let block = match self.read_struct(level) {
            Ok(block) => block,
            Err(x) => {
                return Err(eg!(x));
            }
        };

        if let Err(x) = block.check(level, id, true) {
            return Err(eg!(format!("Invalid disk block {}:  {}", id.commas(), x)));
        }

        if !last && !block.full() {
            return Err(eg!(format!(
                "Block {} at level {} from disk is not full.",
                id.commas(),
                level
            )));
        }

        Ok(block)
    }

    // Read the disk block directly into the memory result, if the
    // storage is working.
    fn read_struct(&mut self, level: usize) -> Result<Block> {
        unsafe {
            let mut s: MaybeUninit<Block> = MaybeUninit::uninit();

            let buffer =
                slice::from_raw_parts_mut(s.as_mut_ptr() as *mut u8, BLOCK_SIZE);

            match self.files[level].read_exact(buffer) {
                Ok(()) => Ok(mem::transmute::<_, Block>(s)),
                Err(e) => {
                    Err(eg!(e))
                }
            }
        }
    }

    /// Check the in-memory version of the Merkle tree for consistency.
    pub fn check(&self) -> Result<()> {
        let mut leaves_at_this_level = self.entry_count;
        let mut last_blocks = 0;
        let mut last_block_full = true;

        // Check each level.
        for level in 0..self.blocks.len() {
            let blocks_at_this_level =
                covered(leaves_at_this_level, LEAVES_IN_BLOCK as u64) as usize;
            let list_length = self.blocks[level].len();

            if list_length != blocks_at_this_level {
                return Err(eg!(format!(
                    "check:  The expected block count ({}) at level {} \
                    should be {}, last {}, full {}, entries {}",
                    blocks_at_this_level,
                    level,
                    list_length,
                    last_blocks,
                    last_block_full,
                    self.entry_count
                )));
            }

            let mut leaf_count = 0;
            last_block_full = true;

            // Now check each block at this level.
            for block_id in 0..blocks_at_this_level {
                let last = block_id == blocks_at_this_level - 1;
                let block = &self.blocks[level][block_id];
                last_block_full = block.full();

                if !last && !last_block_full {
                    return Err(eg!(format!(
                        "check:  Block {} at level {} should be full.",
                        block_id, level
                    )));
                }

                block.check(level, block_id as u64, false).c(d!())?;

                // If we are above level zero, check the hashes in the block
                // against the values in the lower level.
                if level > 0 {
                    let lower_index = block_id * LEAVES_IN_BLOCK * 2;
                    let lower_list = &self.blocks[level - 1];

                    self.check_lower(block, lower_list, lower_index).c(d!())?;
                }

                leaf_count += block.valid_leaves() as u64;
            }

            if leaf_count != leaves_at_this_level {
                return Err(eg!(format!(
                    "check:  The entry counts ({}, {}) at level {} did not match",
                    leaf_count, leaves_at_this_level, level
                )));
            }

            // Advance to the next level of the  tree. Compute the number
            // of entries that we expect to be there.
            last_blocks = blocks_at_this_level;
            leaves_at_this_level =
                next_level_leaves(last_blocks as u64, last_block_full);

            // Check that there's an entry in the vector for the next level.
            // If not, return an error.
            let last_level = level == self.blocks.len() - 1;

            if last_level && leaves_at_this_level > 0 {
                return Err(eg!(format!(
                    "Level {} has {} blocks, with {} upper leaves, \
                    but no levels remain.",
                    level, last_blocks, leaves_at_this_level
                )));
            }
        }

        Ok(())
    }

    // Check that a block contains the correct hashes based on the lower-level
    // blocks.
    fn check_lower(
        &self,
        block: &Block,
        lower: &[Block],
        start_block: usize,
    ) -> Result<()> {
        let mut block_index = start_block;

        for i in 0..block.valid_leaves() as usize {
            if block_index + 1 >= lower.len() {
                return Err(eg!(format!(
                    "Block {} at level {} has too many hashes:  {} vs {}.",
                    block.id(),
                    block.level(),
                    block.valid_leaves(),
                    lower.len()
                )));
            }

            let left = match lower[block_index].top_hash() {
                None => {
                    return Err(eg!(format!(
                        "The left lower hash at {}, level {} is missing.",
                        block_index,
                        block.level()
                    )));
                }
                Some(x) => x,
            };

            let right = match lower[block_index + 1].top_hash() {
                None => {
                    return Err(eg!(format!(
                        "The right lower hash at {}, level {} is missing.",
                        block_index + 1,
                        block.level()
                    )));
                }
                Some(x) => x,
            };

            let hash = hash_pair(left, right);

            if hash != block.hashes[i] {
                return Err(eg!(format!(
                    "hash[{}] for block {} at level {} didn't match.",
                    i,
                    block.id(),
                    block.level()
                )));
            }

            block_index += 2;
        }

        Ok(())
    }

    /// Reset the disk image to null.
    ///
    /// This action will cause the entire tree to be written to disk on
    /// the next write call, which can be useful in the presence of errors.
    /// For this reason, the code attempts to recreate all the files.
    #[inline(always)]
    pub fn reset_disk(&mut self) -> Result<()> {
        for i in 0..self.files.len() {
            self.blocks_on_disk[i] = 0;

            let path = self.file_path(i);
            let _ = fs::remove_file(&path);

            self.files[i] = OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .open(path)
                .c(d!())?;
        }

        Ok(())
    }

    /// Return the path for the tree as given to open.
    #[inline(always)]
    pub fn path(&self) -> String {
        self.path.clone()
    }
}

#[cfg(test)]
#[allow(missing_docs)]
mod tests {
    use {
        super::*,
        byteorder::{LittleEndian, WriteBytesExt},
        cryptohash::sha256,
        rand::{prelude::thread_rng, Rng},
    };

    #[test]
    fn test_info() {
        println!("The block size is {} bytes.", mem::size_of::<Block>());
        println!("A block contains {} leaves.", LEAVES_IN_BLOCK);
    }

    // Do some basic tests on the block header code to make sure that
    // it catches some forms of invalid data.
    #[test]
    fn test_header() {
        let mut header = BlockHeader::new(3, 5);

        if let Err(e) = header.check(3, 5) {
            panic!("new() returned an invalid header:  {}", e);
        }

        header.header_mark ^= 1;

        if header.check(3, 5).is_ok() {
            panic!("check didn't detect an invalid header.");
        }

        header = BlockHeader::new(3, 5);
        header.level += 1;

        if header.check(3, 5).is_ok() {
            panic!("check didn't detect an invalid level.");
        }

        header = BlockHeader::new(3, 5);
        header.id += 1;

        if header.check(3, 5).is_ok() {
            panic!("check didn't detect an invalid id.");
        }

        header = BlockHeader::new(3, 5);
        header.valid_leaves = LEAVES_IN_BLOCK as u16 + 1;

        if let Ok(()) = header.check(3, 5) {
            panic!("check didn't detect an invalid leaf count.");
        }
    }

    fn check_block(block: &Block, level: usize, id: u64, disk_format: bool) {
        if let Err(e) = block.check(level, id, disk_format) {
            panic!("Unexpected block check error:  {}", e);
        }
    }

    // Test some basic properties of the block-handling code.
    #[test]
    fn test_block() {
        let mut block = Block::new(1, 2);

        if block.header != BlockHeader::new(1, 2) {
            panic!("bad new header");
        }

        block.set_checksum();

        // Check that new and set_checksum create a valid block.
        check_block(&block, 1, 2, false);

        if block.valid_leaves() != 0 {
            panic!("valid_leaves() should be zero.");
        }

        // Now iterate and create a full block. Check that it
        // stays consistent as we go, and inject errors, as well.
        let mut hash = HashValue::new();

        for i in 0..LEAVES_IN_BLOCK {
            hash.hash[0] = 1;
            hash.hash[1] = (i & 0xff) as u8;

            if let Err(e) = block.set_hash(&hash) {
                panic!("Unexpected set_hash error:  {} at {}", e, i);
            }

            block.set_checksum();

            if let Err(e) = block.check(1, 2, false) {
                panic!("Block check failure at iteration {}:  {}", i, e);
            }

            // If the block is not full, there will still be hash entries
            // that are set to HashBlock::new. Pick one of them and corrupt
            // it.
            if !block.full() {
                let saved_hash = block.hashes[i + 1];
                block.hashes[i + 1].hash[0] ^= 1;
                block.set_checksum();

                if block.check(1, 2, false).is_ok() {
                    panic!(
                        "check didn't see a corrupted empty hash:  {:?}",
                        block.hashes[i + 1].hash
                    );
                }

                block.hashes[i + 1] = saved_hash;
                block.set_checksum();
                check_block(&block, 1, 2, false);
            }

            // Okay, now try corrupting a block that has been set with a value
            // from SHA256. We make the assumption here and elsewhere that SHA256
            // will not produce HashBlock::new.
            let index = if i > 0 { i - 1 } else { i };

            let saved_hash = block.hashes[index];

            block.hashes[index] = HashValue::new();
            block.set_checksum();

            if block.check(1, 2, false).is_ok() {
                panic!("check didn't see a corrupted full hash");
            }

            // Restore the block to a consistent state.
            block.hashes[index] = saved_hash;
            block.set_checksum();
            check_block(&block, 1, 2, false);

            if block.valid_leaves() != i as u64 + 1 {
                panic!("valid_leaves() should be {}", i);
            }

            if !block.full() {
                if let Some(_x) = block.top_hash() {
                    panic!("top_hash returned a hash.");
                }
            }
        }

        // Any further insertions should fail.
        for i in 0..4 {
            let error = block.set_hash(&hash);

            if let Ok(()) = error {
                panic!("Missing error at iteration {}.", i);
            }
        }

        if block.top_hash().is_none() {
            panic!("top_hash failed on a full block.");
        }

        // Now check that mismatched block ids and levels are
        // caught. First, make sure that the block is valid.
        if let Err(e) = block.check(1, 2, false) {
            panic!("The block was corrupted by set_hash:  {}", e);
        }

        if block.check(0, 2, false).is_ok() {
            panic!("Bad block id passed");
        }

        if block.check(1, 3, false).is_ok() {
            panic!("Bad level passed");
        }

        // Corrupt checksum[0] and see whether that's caught.
        block.header.check_bits.bits[0] ^= 1;

        if block.check(1, 2, false).is_ok() {
            panic!("Bad hash[0] passed");
        }

        block.header.check_bits.bits[0] ^= 1;

        if let Err(e) = block.check(1, 2, false) {
            panic!("Testing error:  {}", e);
        }

        // Now corrupt checksum[last] and do the checks.
        block.header.check_bits.bits[CHECK_SIZE - 1] ^= 1;

        if block.check(1, 2, false).is_ok() {
            panic!("Bad hash[last] passed");
        }

        block.header.check_bits.bits[CHECK_SIZE - 1] ^= 1;

        if let Err(e) = block.check(1, 2, false) {
            panic!("Testing error:  {}", e);
        }

        // Okay, corrupt a hash in the subtree.
        block.hashes[LEAVES_IN_BLOCK].hash[0] ^= 1;

        if block.check(1, 2, true).is_ok() {
            panic!("A corrupted subtree passed");
        }

        // Try redoing an insertion...
        block.header.valid_leaves = 0;

        if block.set_hash(&hash).is_ok() {
            panic!("set_hash overwrote a full hash");
        }
    }

    #[test]
    fn test_hash_pair() {
        let mut a = [0; 2 * HASH_SIZE];

        for (i, sa) in a.iter_mut().enumerate().take(2 * HASH_SIZE) {
            *sa = i as u8;
        }

        let digest = sha256::hash(&a[0..2 * HASH_SIZE]);
        let mut left = HashValue::new();
        let mut right = HashValue::new();

        left.hash.clone_from_slice(&a[0..HASH_SIZE]);
        right.hash.clone_from_slice(&a[HASH_SIZE..2 * HASH_SIZE]);

        let check = hash_pair(&left, &right);

        if check.hash != digest[0..HASH_SIZE] {
            panic!("hash_pair failed.");
        }
    }

    // Do some simple tests of small trees.
    #[test]
    fn test_basic_tree() {
        assert!(mem::size_of::<CheckBits>() == CHECK_SIZE);
        assert!(mem::size_of::<BlockHeader>() == HASH_SIZE);
        assert!(mem::size_of::<Block>() == BLOCK_SIZE);

        if AppendOnlyMerkle::open(&"no such file").is_ok() {
            panic!("Open found a non-existent tree.");
        }

        if AppendOnlyMerkle::create(&".").is_ok() {
            panic!("Created a tree from \".\".");
        }

        let path = "basic_test_tree".to_string();
        let _ = fs::remove_file(&path);
        let result = AppendOnlyMerkle::create(&path);

        let mut tree = match result {
            Ok(tree) => tree,
            Err(x) => {
                panic!("Unexpected error:  {}", x);
            }
        };

        write_tree(&mut tree);
        check_tree(&tree);

        if let Err(x) = AppendOnlyMerkle::open(&path) {
            panic!("read() got an error:  {}", x);
        }

        if tree.path() != path {
            panic!(
                "The path member returned {} instead of {}",
                tree.path(),
                path
            );
        }

        test_append(&mut tree, 0, false);
        test_append(&mut tree, 1, false);
        write_tree(&mut tree);
        let count = tree.total_size();
        let mut tid = 2;

        tree = match AppendOnlyMerkle::open(&path) {
            Err(x) => {
                panic!("Open failed:  {}", x);
            }
            Ok(x) => x,
        };

        if tree.total_size() != count {
            panic!(
                "The counts did not match ({}, {})",
                tree.total_size(),
                count
            );
        }

        // Now try deleting a file for an index level to see whether that
        // is detected. First, we make the tree large enough to need a
        // level 1 index block.
        for _i in 0..2 * LEAVES_IN_BLOCK as u64 {
            test_append(&mut tree, tid, false);
            tid += 1;
        }

        write_tree(&mut tree);

        // Check that the image can be read and is reasonable.
        match AppendOnlyMerkle::open(&path) {
            Err(x) => {
                panic!("The open failed:  {}", x);
            }
            Ok(new_tree) => {
                if new_tree.total_size() != tree.total_size() {
                    panic!("The tree sizes don't match.");
                }
            }
        }

        let level_path = tree.file_path(1);

        if let Err(x) = fs::remove_file(&level_path) {
            panic!("remove_file failed:  {}", x);
        }

        let mut tree = match AppendOnlyMerkle::open(&path) {
            Err(x) => {
                panic!("Open failed to rebuild:  {}", x);
            }
            Ok(tree) => tree,
        };

        check_tree(&tree);
        write_tree(&mut tree);
        check_disk_tree(&mut tree, true);

        // Now make sure that the new write path deals well with
        // disk reset operations.
        if let Err(e) = tree.reset_disk() {
            panic!("Unexpected error:  {}", e);
        }

        for _i in 0..2 * LEAVES_IN_BLOCK {
            test_append(&mut tree, tid, false);
            tid += 1;
        }

        check_tree(&tree);
        check_disk_tree(&mut tree, false);
        write_tree(&mut tree);
        check_disk_tree(&mut tree, true);

        let _ = fs::remove_file(&path);
        let _ = fs::remove_file(path.to_owned() + ".1");
    }

    fn create_test_hash(i: u64, verbose: bool) -> HashValue {
        let mut buffer = [0_u8; HASH_SIZE - 1];
        let mut hash_value = HashValue::new();

        for buf in &mut buffer {
            *buf = 0xfe;
        }

        buffer
            .as_mut()
            .write_u64::<LittleEndian>(i)
            .expect("le write");
        hash_value.hash[1..].clone_from_slice(&buffer[0..HASH_SIZE - 1]);

        if verbose {
            println!("Create hash {}", i.commas());
        }

        hash_value
    }

    fn check_tree(tree: &AppendOnlyMerkle) {
        if let Err(e) = tree.check() {
            panic!("Got check error:  {}", e);
        }
    }

    fn check_disk_tree(tree: &mut AppendOnlyMerkle, flushed: bool) {
        if let Err(e) = tree.check_disk(flushed) {
            panic!("Got disk check error:  {}", e);
        }
    }

    fn write_tree(tree: &mut AppendOnlyMerkle) {
        if let Err(e) = tree.write() {
            panic!("tree.write failed:  {}", e);
        }
    }

    fn reset_tree(tree: &mut AppendOnlyMerkle) {
        if let Err(e) = tree.reset_disk() {
            panic!("tree.reset_disk failed:  {}", e);
        }
    }

    fn test_append(tree: &mut AppendOnlyMerkle, i: u64, verbose: bool) -> u64 {
        let hash = create_test_hash(i, verbose);
        let result = tree.append_hash(&hash);

        if let Err(x) = result {
            panic!("append_hash failed:  {}", x);
        }

        result.unwrap()
    }

    // Test a larger tree.
    #[test]
    #[allow(clippy::mut_range_bound)]
    fn test_tree() {
        let path = "test_tree".to_string();
        let _ = fs::remove_file(&path);
        let _ = fs::remove_file(&(path.clone() + ".1-base"));
        let _ = fs::remove_file(&(path.clone() + ".2-base"));
        let result = AppendOnlyMerkle::create(&path);

        let mut tree = match result {
            Err(x) => {
                panic!("Unexpected error:  {}", x);
            }
            Ok(tree) => tree,
        };

        // Test an empty tree.
        write_tree(&mut tree);
        check_tree(&tree);
        check_disk_tree(&mut tree, true);

        // Now create a tree with some contents.
        let mut entry_id = 0;
        test_append(&mut tree, entry_id, true);
        entry_id += 1;

        check_tree(&tree);

        let leaves_in_block = LEAVES_IN_BLOCK as u64;
        let mut leaves_per_next = leaves_in_block;

        for _t in entry_id..2 * leaves_in_block * leaves_in_block + 1 {
            if entry_id % (64 * 1024) == 0 {
                println!("At entry {}", entry_id.commas());
            }

            test_append(&mut tree, entry_id, false);
            entry_id += 1;

            // Do an exponential-ish backoff on the checking, as it is expensive.
            if entry_id == leaves_per_next - 1
                || entry_id == leaves_per_next
                || entry_id == leaves_per_next + 1
                || entry_id == 2 * leaves_per_next - 1
                || entry_id == 2 * leaves_per_next
                || entry_id == 2 * leaves_per_next + 1
                || entry_id == 3 * leaves_per_next
            {
                println!("Checking the tree at {}.", entry_id.commas());
                check_tree(&tree);
                check_disk_tree(&mut tree, false);
                write_tree(&mut tree);
                check_disk_tree(&mut tree, false);

                if let Err(x) = AppendOnlyMerkle::open(&path) {
                    panic!("Open failed:  {}", x);
                }
            }

            if entry_id > 3 * leaves_per_next {
                leaves_per_next *= leaves_in_block;
            }
        }

        // The on-disk image should be fine, and so check_disk should pass.
        println!("Checking the disk image.");
        check_disk_tree(&mut tree, false);

        // Now sync the tree to disk and see whether everything checks.
        println!("Syncing the tree.");
        write_tree(&mut tree);
        check_tree(&tree);
        check_disk_tree(&mut tree, true);

        // Multiple writes should be okay.
        println!("Syncing the tree again.");
        write_tree(&mut tree);
        check_tree(&tree);
        check_disk_tree(&mut tree, true);

        // Check the tree every so often while we write a lot of hashes
        // to it.
        let mut countdown = thread_rng().gen::<u32>() % (256 * 1024);
        countdown += 1;

        // Try a reconstruct
        match tree.reconstruct(1, 0) {
            Err(x) => {
                panic!("reconstruct failed:  {}", x);
            }
            Ok(block) => {
                if let Err(e) = block.check(1, 0, true) {
                    panic!("The reconstruct block is bad:  {}", e);
                }

                for i in 0..HASHES_IN_BLOCK {
                    if block.hashes[i] != tree.blocks[1][0].hashes[i] {
                        panic!(
                            "Hash mismatch:  {}, {:?}, {:?}",
                            i, block.hashes[i], tree.blocks[1][0].hashes[i]
                        );
                    }
                }
            }
        }

        for _t in entry_id..512 * 1024 + leaves_in_block {
            if entry_id % (64 * 1024) == 0 {
                check_tree(&tree);
                println!("At entry {}", entry_id.commas());
            }

            test_append(&mut tree, entry_id, false);

            entry_id += 1;
            countdown -= 1;

            if countdown <= 1 {
                println!("Checking the tree and disk image at {}", entry_id.commas());
                check_tree(&tree);
                check_disk_tree(&mut tree, false);

                if tree.total_size() != entry_id {
                    panic!(
                        "Got {} nodes, but expected {}.",
                        tree.total_size(),
                        entry_id
                    );
                }

                // Sync the tree to disk.
                println!("Syncing the tree.");
                write_tree(&mut tree);

                if thread_rng().gen::<u32>() % 4 == 0 {
                    println!("Rechecking the in-memory tree.");
                    check_tree(&tree);
                    println!("Rechecking the disk.");
                    check_disk_tree(&mut tree, true);

                    if let Err(x) = AppendOnlyMerkle::open(&path) {
                        panic!("open failed:  {}", x);
                    }

                    reset_tree(&mut tree);
                    check_disk_tree(&mut tree, false);
                }

                println!("Done with checking.");

                // Restart the countdown.
                countdown = thread_rng().gen::<u32>() % (256 * 1024);
                countdown += 1;
            }
        }

        // Check the final tree.
        println!("Checking the final tree.");
        check_tree(&tree);
        check_disk_tree(&mut tree, false);

        // Sync the final tree to disk, and then check it.
        println!("Doing a final  sync.");
        write_tree(&mut tree);
        check_disk_tree(&mut tree, true);
        check_tree(&tree);

        // Save the current size, and try reopening the tree.
        let count = tree.total_size();
        let result = AppendOnlyMerkle::open(&path);

        match result {
            Err(x) => {
                panic!("The open failed:  {}", x);
            }
            Ok(x) => {
                tree = x;
            }
        }

        // The reopened tree should be okay.
        println!("Checking the reopened tree.");
        check_tree(&tree);
        check_disk_tree(&mut tree, true);

        // Its size should match the old tree.
        if count != tree.total_size() {
            panic!(
                "The sizes did not match, {} vs {}.",
                count,
                tree.total_size()
            );
        }

        // Okay, do a minimal test of the rewrite function.
        if let Err(e) = tree.reset_disk() {
            panic!("test_tree: failed at tree.reset_disk with error {}", e);
        }

        println!("Syncing and checking the reopened tree.");
        write_tree(&mut tree);
        check_disk_tree(&mut tree, true);

        drop(tree);
        println!("Trying a rebuild.");

        let tree = match AppendOnlyMerkle::rebuild(&path) {
            Err(x) => {
                panic!("Rebuild failed:  {}", x);
            }
            Ok(tree) => tree,
        };

        if tree.total_size() != count {
            panic!("The sizes did not match.");
        }

        println!("Checking the rebuilt tree.");

        for i in 0..count {
            if tree.leaf(i as usize) != create_test_hash(i, false) {
                panic!("Leaf {} does not match.", i);
            }
        }

        let ext = tree.rebuild_extension();
        let _ = fs::remove_file(&path);
        let _ = fs::remove_file(path.to_owned() + &ext);

        for i in 1..MAX_BLOCK_LEVELS {
            let path = tree.file_path(i);
            let _ = fs::remove_file(&path);
            let _ = fs::remove_file(path.to_owned() + &ext);
        }
    }

    //
    // Check that a proof is reasonably correct. Make sure the
    // fields are set, and that the hash fields merge to form
    // the root hash.
    //
    fn check_proof(tree: &AppendOnlyMerkle, proof: &Proof, tx_id: u64) {
        if proof.version != PROOF_VERSION {
            panic!("The proof version is not set.");
        }

        if proof.ledger != tree.path() {
            panic!("The proof ledger is {}.", proof.ledger);
        }

        if proof.state != tree.entry_count {
            panic!("The state state is {}.", proof.state);
        }

        if proof.time <= 0 {
            panic!("The proof time is invalid.");
        }

        if proof.tx_id != tx_id {
            panic!("The tx_id is {}, but it should be {}.", proof.tx_id, tx_id);
        }

        let mut id = tx_id;

        //
        // Now validate the actual hash values. We start with the
        // hash of the transaction, and hash it with each entry
        // in the hash_array field. At the end of this process,
        // we should have the value in the root_hash field.
        //
        // We need to decide whether the current result should be
        // first in the buffer to be hash (the "left" sibling in
        // our binary tree) or the second (the "right" sibling).
        // Checking the corresponding bit of the transaction id
        // gives us the order. The lowest-order bit gives the
        // answer for the first entry in the hash array, and so
        // on up the list.
        //
        let mut result = tree.leaf(tx_id as usize);

        for i in 0..proof.hash_array.len() {
            if id & 1 == 0 {
                result = hash_partial(&result, &proof.hash_array[i]);
            } else {
                result = hash_partial(&proof.hash_array[i], &result);
            }

            id /= 2;
        }

        // The result now had better match the root of the tree
        // at the time the proof was generated.
        if result != proof.root_hash {
            panic!(
                "Proof hash mismatch:  {} vs {}, id = {}, size = {}",
                result.desc(),
                proof.root_hash.desc(),
                tx_id,
                tree.total_size()
            );
        }

        // The result of "get_root_hash" should match,
        // as well.
        let tree_root_hash = tree.get_root_hash();

        if result != tree_root_hash {
            panic!(
                "Tree hash mismatch:  {} vs {}, id = {}, size = {}",
                result.desc(),
                tree_root_hash.desc(),
                tx_id,
                tree.total_size()
            );
        }
    }

    #[test]
    fn test_pieces() {
        // Test an entry that has one non-empty hash.
        let mut entry = Entry::new(0, 0);

        entry.hashes[0].hash[0] = 24;
        entry.hashes[0].hash[1] = 25;
        entry.hashes[0].hash[2] = 26;
        entry.hashes[0].hash[3] = 27;

        // Now compute the full tree of hashes for this block.

        entry.fill();

        // We should have 9 non-empty hashes in this entry's tree,
        // corresponding to the line up from the one valid hash.
        // We also know what their indices should be...

        let empty_hash = HashValue::new();
        let mut count = 0;

        let expected_entries = [0, 256, 384, 448, 480, 496, 504, 508, 510];

        for i in 0..HASHES_IN_BLOCK {
            if entry.hashes[i] != empty_hash {
                println!("hashes[{:3}] = {}", i, entry.hashes[i].desc());
                assert!(i == expected_entries[count]);
                count += 1;
            }
        }

        println!("Got {} hashes.", count);
        assert!(count == 9);
        let last = entry.hashes.len() - 1;
        println!("hashes[last = {}] = {}", last, entry.hashes[last].desc());

        // Okay, try pushing a piece of a proof. We should push a partner
        // for all but the topmost element of this entry's tree. We know
        // what indices should be pushed, too.

        let mut hashes = Vec::new();
        entry.push(&mut hashes, 0, &expected_entries);
        assert!(hashes.len() == LEVELS_IN_BLOCK - 1);

        // All of the partners should be empty hashes for
        // this subtree.
        for hash in hashes.iter() {
            assert!(*hash == empty_hash);
        }
    }

    #[test]
    fn test_basic_proof() {
        println!("Starting the basic proof test.");

        println!("Generating and checking the proofs.");

        let path = "basic_proof_tree".to_string();
        let _ = fs::remove_file(&path);
        let transactions = (LEAVES_IN_BLOCK + 2) as u64;

        let mut tree = match AppendOnlyMerkle::create(&path) {
            Ok(x) => x,
            Err(x) => {
                panic!("Error on open:  {}", x);
            }
        };

        for i in 0..transactions {
            test_append(&mut tree, i as u64, false);

            if i == 1
                || i == 2
                || (i as usize) % LEAVES_IN_BLOCK == LEAVES_IN_BLOCK - 1
                || (i as usize) % LEAVES_IN_BLOCK == 0
                || (i as usize) % LEAVES_IN_BLOCK == 1
                || (i as usize) % LEAVES_IN_BLOCK == 2
            {
                check_all_proofs(&tree);
            }
        }

        let _ = fs::remove_file(&path);
    }

    fn check_all_proofs(tree: &AppendOnlyMerkle) {
        for i in 0..tree.total_size() {
            match tree.generate_proof(i, tree.total_size()) {
                Err(x) => {
                    panic!("Error on proof for transaction {}:  {}", i, x);
                }
                Ok(proof) => {
                    check_proof(&tree, &proof, i);
                    validate_id(&tree, i);
                }
            }
        }
    }

    #[test]
    fn test_proof() {
        println!("Starting the proof test.");

        //
        // First, create a tree.
        //
        let path = "proof_tree".to_string();
        let _ = fs::remove_file(&path);

        let mut tree = match AppendOnlyMerkle::create(&path) {
            Ok(x) => x,
            Err(x) => {
                panic!("Error on open:  {}", x);
            }
        };

        //
        // Now add some transactions to it.
        //
        let transactions = (2 * LEAVES_IN_BLOCK * LEAVES_IN_BLOCK + 1) as u64;

        for i in 0..transactions {
            let id = test_append(&mut tree, i, false);

            // Try a proof of the appended id just as a bit
            // of a test.
            match tree.generate_proof(id, tree.total_size()) {
                Err(x) => {
                    panic!("Error on proof for transaction {}:  {}", i, x);
                }
                Ok(proof) => {
                    assert!(id == i);
                    check_proof(&tree, &proof, i);
                    validate_id(&tree, i);
                }
            }

            if i % (16 * 1024) == 0 {
                println!("Generated proof {}.", i.commas());
            }

            // Check that the proof for each element in the tree
            // is correct for this size of a tree.
            if tree.total_size() as usize == 2 * LEAVES_IN_BLOCK * LEAVES_IN_BLOCK {
                check_all_proofs(&tree);
            }
        }

        // Check that the proof for each entry in the
        // tree is generated properly.
        check_all_proofs(&tree);

        // Generating a proof for a non-existent transaction
        // should fail.
        if let Ok(_x) = tree.generate_proof(transactions, tree.total_size()) {
            panic!("Transaction {} does not exist.", transactions);
        }

        //
        // Generating a proof for a version other than the
        // current version should fail.
        //
        match tree.generate_proof(0, tree.total_size() - 1) {
            Err(e) => {
                if !e.msg_has_overloop(eg!("Versioning is not yet supported.").as_ref())
                {
                    panic!("The error for an invalid generation was not valid.");
                }
            }
            Ok(_x) => {
                panic!("An invalid tree version passed.");
            }
        }

        assert!(!tree.validate_transaction_id(tree.total_size()));

        //
        // Remove the test files.
        //
        let _ = fs::remove_file(&path);

        for i in 1..MAX_BLOCK_LEVELS {
            let path = tree.file_path(i);
            let _ = fs::remove_file(&path);
        }
        println!("Done with the proof test.");
    }

    fn validate_id(tree: &AppendOnlyMerkle, id: u64) {
        if !tree.validate_transaction_id(id) {
            panic!("Id {} is not valid.", id.commas());
        }
    }

    #[test]
    fn test_serde() {
        let path = "serde_tree".to_string();
        let _ = fs::remove_file(&path);
        let size = 2 * LEAVES_IN_BLOCK;

        let mut tree = match AppendOnlyMerkle::create(&path) {
            Ok(x) => x,
            Err(e) => {
                panic!("Create failed:  {}", e);
            }
        };

        for i in 0..size {
            test_append(&mut tree, i as u64, false);
        }

        let encoded = serde_json::to_string_pretty(&tree).unwrap();
        drop(tree);

        println!("Got JSON.");
        // println!("{}", encoded);

        println!("Removing files.");
        let _ = fs::remove_file(&path);

        for i in 1..MAX_BLOCK_LEVELS {
            let file = path.clone() + "." + &i.to_string();
            let _ = fs::remove_file(&file);
        }

        println!("Decoding.");

        let mut tree: AppendOnlyMerkle = match serde_json::from_str(&encoded) {
            Ok(x) => x,
            Err(e) => {
                panic!("from_str failed:  {}", e);
            }
        };

        if let Err(e) = tree.finish_deserialize() {
            panic!("test_serde: tree.finish_deserialize failed with err {}", e);
        }

        if let Err(e) = tree.check() {
            panic!("test_serde: tree.check failed with err {}", e);
        }

        if let Err(e) = tree.write() {
            panic!("test_serde: tree.write failed with err {}", e);
        }

        if let Err(e) = tree.check_disk(true) {
            panic!("test_serde: tree.check_disk failed with err {}", e);
        }

        if tree.total_size() != size as u64 {
            panic!("The size is wrong:  {} vs {}", size, tree.total_size());
        }

        drop(tree);

        let _ = fs::remove_file(&path);

        for i in 1..MAX_BLOCK_LEVELS {
            let file = path.clone() + "." + &i.to_string();
            let _ = fs::remove_file(&file);
        }
    }

    #[test]
    fn test_reconstruct() {
        let path = "reconstruct_tree".to_string();
        let _ = fs::remove_file(&path);
        let size = 2 * LEAVES_IN_BLOCK;

        let mut tree = match AppendOnlyMerkle::create(&path) {
            Ok(x) => x,
            Err(e) => {
                panic!("Create failed:  {}", e);
            }
        };

        for i in 0..size {
            test_append(&mut tree, i as u64, false);
        }

        if tree.reconstruct(0, 0).is_ok() {
            panic!("Reconstruct worked on level zero.");
        }

        match tree.reconstruct(1, 0) {
            Err(x) => {
                panic!("Reconstruct failed:  {}", x);
            }
            Ok(block) => {
                if let Err(e) = block.check(1, 0, true) {
                    panic!("block.check failed with {}", e);
                }

                for i in 0..HASHES_IN_BLOCK {
                    if block.hashes[i] != tree.blocks[1][0].hashes[i] {
                        panic!(
                            "Hash mismatch:  {}, {:?}, {:?}",
                            i, block.hashes[i], tree.blocks[1][0].hashes[i]
                        );
                    }
                }
            }
        }

        drop(tree);

        let _ = fs::remove_file(&path);

        for i in 1..MAX_BLOCK_LEVELS {
            let file = path.clone() + "." + &i.to_string();
            let _ = fs::remove_file(&file);
        }
    }

    #[test]
    fn test_corrupt_level0() {
        let path = "test_corrupt_level0";
        let base = path.to_owned() + &AppendOnlyMerkle::rebuild_ext();
        let _ = fs::remove_file(&path);
        let _ = fs::remove_file(&base);
        let buffer = [1_u8; 4];

        let mut file = OpenOptions::new()
            .create(true)
            .write(true)
            .open(&path)
            .unwrap();

        let _ = file.write(&buffer);

        if let Ok(_tree) = AppendOnlyMerkle::open(&path) {
            println!("Open worked with a corrupt tree.");
        }

        if let Ok(_tree) = AppendOnlyMerkle::rebuild(&path) {
            println!("Rebuild worked with a corrupt tree.");
        }

        let base = path.to_owned() + &AppendOnlyMerkle::rebuild_ext();
        let _ = fs::remove_file(&path);
        let _ = fs::remove_file(&base);
    }

    #[test]
    fn test_basic_rebuild() {
        let path = "rebuild_tree";
        let rebuild = path.to_owned() + &AppendOnlyMerkle::rebuild_ext();
        let _ = fs::remove_file(&path);
        let _ = fs::remove_file(&rebuild);

        let mut tree = match AppendOnlyMerkle::create(&path) {
            Ok(tree) => tree,
            Err(x) => {
                panic!("create failed:  {}", x);
            }
        };

        for tid in 0..4 * LEAVES_IN_BLOCK {
            test_append(&mut tree, tid as u64, false);
        }

        if let Err(e) = tree.write() {
            panic!("test_basic_rebuild: tree.write failed with {}", e);
        }

        let fake = path.to_owned() + "." + &tree.files.len().to_string();

        let result = OpenOptions::new().create(true).write(true).open(&fake);

        if let Err(x) = result {
            panic!("I cannot create {}:  {}", fake, x);
        } else {
            println!("Created {}.", fake);
        }

        let final_size = tree.total_size();
        drop(tree);

        let tree = match AppendOnlyMerkle::rebuild(&path) {
            Ok(tree) => tree,
            Err(x) => {
                panic!("rebuild failed:  {}", x);
            }
        };

        if tree.total_size() != final_size {
            panic!(
                "The sizes ({}. {}) do not match.",
                tree.total_size(),
                final_size
            );
        }

        for i in 0..4 * LEAVES_IN_BLOCK {
            if tree.leaf(i) != create_test_hash(i as u64, false) {
                panic!("Leaf {} does not match.", i);
            }
        }

        let ext = tree.rebuild_extension();
        //let expected = "Rebuild path ".to_owned() + path + &ext + " already exists.";

        if AppendOnlyMerkle::rebuild(&path).is_ok() {
            panic!("A double rebuild worked.");
        }

        drop(tree);

        let _ = fs::remove_file(path);
        let _ = fs::remove_file(path.to_owned() + &ext);
        let _ = fs::remove_file(path.to_owned() + ".1");
        let _ = fs::remove_file(path.to_owned() + ".1" + &ext);

        if fs::remove_file(&fake).is_ok() {
            panic!("File ... should have been moved.");
        }

        let fake_ext = fake + &ext;
        let result = fs::remove_file(&fake_ext);

        if let Err(x) = result {
            panic!("File {} was not deleted:  {}", fake_ext, x);
        }
    }
}
