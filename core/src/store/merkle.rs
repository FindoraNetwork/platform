//
//  This module implements an append-only binary Merkle tree using
//  SHA256 as the hash function.  The tree is kept in memory currently,
//  but this module will write the contents to disk if requested, and
//  can initialize a tree using a disk image.
//

#[macro_use]
pub mod append_only_merkle  {
    extern crate byteorder;
    extern crate rand;
    extern crate serde;
    extern crate sodiumoxide;

    use byteorder::LittleEndian;
    use byteorder::WriteBytesExt;
    use rand::prelude::thread_rng;
    use rand::Rng;
    use sodiumoxide::crypto::hash::sha256;
    use sodiumoxide::crypto::hash::sha256::Digest;
    use std::fs::File;
    use std::fs::OpenOptions;
    use std::io::Error;
    use std::io::ErrorKind;
    use std::io::Read;
    use std::io::Seek;
    use std::io::SeekFrom::End;
    use std::io::SeekFrom::Start;
    use std::io::Write;
    use std::mem;
    use std::option::Option;
    use std::slice::from_raw_parts;
    use std::slice::from_raw_parts_mut;

    const BLOCK_SHIFT:     u16   = 9;
    const HASHES_IN_BLOCK: usize = (1 << BLOCK_SHIFT) - 1;
    const LEAVES_IN_BLOCK: usize = (HASHES_IN_BLOCK + 1) / 2;
    const CHECK_SIZE:      usize = 16;
    const HEADER_VALUE:    u32   = 0xabcd0123;
    const HASH_SIZE:       usize = 32;
    const BLOCK_SIZE:      usize = HASH_SIZE * (HASHES_IN_BLOCK + 1);

    // Define a simple error macro to relieve some of the tedium of
    // creating an Error structure.
    macro_rules! sem {
        ($($x:tt)+) => { se(format!($($x)+)) }
    }

    fn se(why: String) -> Option<Error> {
        Some(Error::new(ErrorKind::Other, why))
    }

    #[derive(PartialEq, Copy, Clone, Debug)]
    #[repr(C)]
    struct CheckBits {
        bits: [u8; CHECK_SIZE],
    }
    
    // Define a header structure for each block.  It identifies the data
    // in the block, and contains a checksum.  This structure needs to
    // be HASH_SIZE bytes in size.  It must sit at the start of the block.
    // The check_bits field must be first in the structure.
    //
    // TODO:  Consider moving to Serialize-Deserialize for checksums and
    // I/O.
    #[derive(PartialEq, Copy, Clone, Debug)]
    #[repr(C)]
    struct BlockHeader {
        check_bits:    CheckBits,
        header_mark:   u32,
        level:         u16,
        valid_leaves:  u16,
        id:            u64,
    }

    impl BlockHeader {
        fn new(level: u32, id: u64) -> BlockHeader {
            return BlockHeader {
                check_bits:    CheckBits { bits: [0; CHECK_SIZE] },
                header_mark:   HEADER_VALUE,
                level:         level as u16,
                valid_leaves:  0,
                id:            id,
            };
        }

        // Do a simple consistency check on some fields in the header.
        fn check(&self, level: usize, id: u64) -> Option<Error> {
            if self.header_mark != HEADER_VALUE {
                return sem!("Block {} at level {} has a bad header ({:x}).", id, level,
                    self.header_mark);
            }
                
            if self.level != level as u16 {
                return sem!("Block {} at level {} has a bad level ({}).", id, level,
                    self.level);
            }

            if self.id != id {
                return sem!("Block {} at level {} has a bad id ({}).", id, level,
                    self.id);
            }
            
            if self.valid_leaves > LEAVES_IN_BLOCK as u16 {
                return sem!("The entry count for block {} at level {} is too large ({}).",
                    id, level, self.valid_leaves);
            }

            None
        }
    }

    // This structure must be HASH_SIZE bytes in size.  Each entry of this
    // type corresponds to a node in the Merkle tree.
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub struct HashValue {
        hash: [u8; HASH_SIZE],
    }

    impl HashValue {
        pub fn new() -> HashValue {
            return HashValue { hash: [0; HASH_SIZE] };
        }
    }

    // A Merkle tree is represented by a collection of blocks.  Blocks
    // are used both in memory and on disk.
    #[repr(C)]
    struct Block {
        header: BlockHeader,
        hashes: [HashValue; HASHES_IN_BLOCK],
    }

    impl Block {
        fn new(level: u32, id: u64) -> Block {
            return Block {
                header:  BlockHeader::new(level, id),
                hashes:  [HashValue::new(); HASHES_IN_BLOCK],
            }
        }

        // Set the hash value for a leaf for a block.  When the last
        // available slot for leaves is full, form the upper levels of
        // the tree that fit in this block, and then set the checksum.
        fn set_hash(&mut self, hash_value: &HashValue) -> Option<Error> {
            let index = self.header.valid_leaves as usize;

            if index >= LEAVES_IN_BLOCK {
                return Some(Error::new(ErrorKind::NotFound,
                    "This block is full."));
            }

            if self.hashes[index] != HashValue::new() {
                return Some(Error::new(ErrorKind::NotFound,
                    "That hash block is not enpty."));
            }

            self.hashes[index] = *hash_value;
            self.header.valid_leaves += 1;

            // If the block is now full, form the subtree contained in it.
            // Also, set the checksum, as the block shouldn't change.
            if self.header.valid_leaves == LEAVES_IN_BLOCK as u16 {
                self.form_subtree();
                self.set_checksum();
            }

            return None;
        }

        // Compute the hashes that form the subtree represented by this
        // block.
        fn form_subtree(&mut self) {
            let mut input = 0;

            for i in LEAVES_IN_BLOCK..HASHES_IN_BLOCK {
                let left  = input;
                let right = input + 1;
                let hash  = hash_pair(&self.hashes[left], &self.hashes[right]);

                self.hashes[i] = hash;

                input += 2;
            }
        }

        fn full(&self) -> bool {
            self.header.valid_leaves >= LEAVES_IN_BLOCK as u16
        }

        fn id(&self) -> usize {
            self.header.id as usize
        }

        fn level(&self) -> usize {
            self.header.level as usize
        }

        fn valid_leaves(&self) -> u64 {
            self.header.valid_leaves as u64
        }

        // Return the hash that is the top level of the subtree
        // of this block.
        fn top_hash(&self) -> Option<&HashValue> {
            if self.full() {
                Some(&self.hashes[HASHES_IN_BLOCK - 1])
            } else {
                None
            }
        }

        // Return a slice representing the part of the block that is
        // checksummed, i.e., everything but the checksum area itself.
        fn as_checksummed_region(&self) -> &[u8] {
            unsafe {
                from_raw_parts(
                    (&self.header.header_mark as *const u32) as *const u8,
                    mem::size_of::<Block>() - mem::size_of::<CheckBits>())
            }
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
            self.header.check_bits.bits = self.compute_checksum();
        }

        // Check the integrity of a block given an expected level and id.
        fn check(&self, level: usize, id: u64, disk_format: bool) -> Option<Error> {
            // Check the checksum first, since a bad disk read would usually lead
            // to a bad checksum.
            if disk_format || self.full() {
                let hash = self.compute_checksum();
                
                if hash != self.header.check_bits.bits {
                    return sem!(
                        "The header checksum for block {} at level {} is invalid.",
                        id, level);
                }
            }

            // Validate the header so that we know that the overall
            // description is coherent.
            if let Some(x) = self.header.check(level, id) {
                return Some(x);
            }

            // Check that the appropriate number of hash values has
            // been set.
            let limit =
                if !self.full() {
                    self.header.valid_leaves as usize
                } else {
                    HASHES_IN_BLOCK
                };

            for i in 0..limit {
                if self.hashes[i] == HashValue::new()  {
                    return sem!(
                        "Hash entry {} for block {} at level {} is invalid.",
                        i, id, level);
                }
            }

            // The rest of the hashes should be in their initial state.
            for i in limit..HASHES_IN_BLOCK {
                if self.hashes[i] != HashValue::new() {
                    return sem!(
                        "Hash entry {} for block {} at level {} was set, valid leaves {}.",
                        i, id, level, self.valid_leaves());
                }
            }

            // If this level is full, the subtree should be valid.  This check
            // is by far the most expensive.
            if self.full() {
                if let Some(x) = self.check_subtree() {
                    return Some(x);
                }
            }

            None
        }

        // Check the hashes of nodes formed inside this block.
        fn check_subtree(&self) -> Option<Error> {
            let mut input = 0;

            for i in LEAVES_IN_BLOCK..HASHES_IN_BLOCK {
                let left  = &self.hashes[input];
                let right = &self.hashes[input + 1];
                let hash  = hash_pair(left, right);

                if hash != self.hashes[i] {
                    return sem!("hash[{}] in block {} at level {} is invalid.",
                        i, self.id(), self.level());
                }

                input += 2;
            }

            None
        }
        
        // Return a pointer to the raw bytes of the block for I/O.
        fn as_bytes(&self) -> &[u8] {
            unsafe {
                from_raw_parts(
                    (self as *const Block) as *const u8,
                    mem::size_of::<Block>())
            }
        }
    }

    // Implement covered division, that is, round up fractions when dividing.
    fn covered(numerator: u64, denominator: u64) -> u64 {
        assert!(numerator <= std::u64::MAX - denominator);
        assert!(denominator > 0);
        (numerator + denominator - 1) / denominator
    }

    // Compute the hash of two hashes.  This Merkle tree is a binary
    // representation, so this is a common operation.
    fn hash_pair(left:  &HashValue, right:  &HashValue) -> HashValue {
        let mut data = [0_u8; 2 * HASH_SIZE];

        data[0..HASH_SIZE].clone_from_slice(&left.hash[0..HASH_SIZE]);
        data[HASH_SIZE..2 * HASH_SIZE].clone_from_slice(&right.hash[0..HASH_SIZE]);

        let digest = sha256::hash(&data);
        let mut result = HashValue::new();
        result.hash.clone_from_slice(&digest[0..HASH_SIZE]);
        result
    }

    // Define an append-ony Merkle tree that eventually will support
    // a sparse in-memory representation.  We will need to use Box
    // for the blocks at that point.
    pub struct AppendOnlyMerkle { 
        entry_count:      u64,          // total entries in the tree
        entries_on_disk:  u64,
        path:             String,       // the disk path for the stable store
        file:             Vec<File>,
        blocks:           Vec<Vec<Block>>,
        blocks_on_disk:   Vec<u64>,     // the number of entries on stable store
    }

    impl AppendOnlyMerkle {
        // This constructor is private.  Use open or create to get a
        // Merkle tree.
        fn new(path: &String, file: File) -> AppendOnlyMerkle {
            AppendOnlyMerkle {
                entry_count:      0,
                entries_on_disk:  0,
                path:             path.to_string(),
                file:             vec!(file),
                blocks:           vec!(Vec::new()),
                blocks_on_disk:   vec!(0),
            }
        }

        // Open an existing Merkle tree and and call read_files to
        // initialize it from the contents on disk.
        pub fn open(path: &String) -> Result<AppendOnlyMerkle, Error> {
            let check_path = OpenOptions::new().read(true).write(true).open(path);

            match check_path {
                Ok(file) => {
                    let mut result = AppendOnlyMerkle::new(path, file);

                    if let Some(x) = result.open_files() {
                        return Result::Err(x);
                    }

                    if let Some(x) = result.read_files() {
                        return Result::Err(x);
                    }

                    Result::Ok(result)
                }
                Err(x) => {
                    return Result::Err(x);
                }
            }
        }

        // Create a new Merkle tree at the given path.  The tree may not
        // exist.
        pub fn create(path: &String) -> Result<AppendOnlyMerkle, Error> {
            let check_path = OpenOptions::new().read(true).write(true).create_new(true).open(path);

            match check_path {
                Ok(file) => {
                    let result = AppendOnlyMerkle::new(path, file);

                    // Remove any files left over from another tree with the same name.
                    for i in 1..64 {
                        let path = result.file_path(i);
                        let _ = std::fs::remove_file(&path);
                    }

                    return Ok(result);
                }
                Err(x) => {
                    return Result::Err(x);
                }
            }
        }

        // Open the files for each level of the tree from 1 upward.
        fn open_files(&mut self) ->Option<Error> {
            let mut i = 1;

            loop {
                let path    = self.file_path(i);
                let result  = OpenOptions::new().read(true).write(true).open(&path);

                match result {
                    Err(x) => {
                        if x.kind() == ErrorKind::NotFound {
                            return None;
                        } else {
                            return Some(x);
                        }
                    }
                    Ok(file) => {
                        self.push_file(file);
                    }
                }

                i += 1;

                // Pick an arbitrary limit to the number of levels we can support. 2^64 is
                // too large to be real.
                if i > 64 - BLOCK_SHIFT as usize {
                    return sem!("The tree at {} has more than {} levels.",
                        self.path, i - 1);
                }
            }
        }

        // Generate the path for the file for the given level.
        fn file_path(&self, level: usize) -> String {
            let path      = self.path.clone();
            let extension = format!(".{}", level);

            path + &extension
        }

        // Add a level to the tree's data structures to prepare for reading
        // the file.
        fn push_file(&mut self, file: File) {
            self.file.push(file);
            self.blocks_on_disk.push(0);
            self.blocks.push(Vec::new());
        }

        // Read the disk data into the Merkle tree.
        fn read_files(&mut self) -> Option<Error> {
            let mut previous_level_size  = 0;
            let mut leaves_at_this_level = 0;

            // Read the file for each level of the tree.
            for level in 0..self.file.len() {
                let file_size = match self.file[level].seek(End(0)) {
                    Ok(n) => { n }
                    Err(x) => {
                        return Some(x);
                    }
                };

                if file_size % BLOCK_SIZE as u64 != 0 {
                    return sem!(
                        "The file contains a partial block (size {}) at level {}",
                        file_size, level);
                }

                if let Err(x) = self.file[level].seek(Start(0)) {
                    return Some(x);
                }

                let block_count = file_size / BLOCK_SIZE as u64;
                let expected    = covered(leaves_at_this_level, LEAVES_IN_BLOCK as u64);

                if level != 0 && block_count != expected {
                    return sem!(
                        "Level {} has {} blocks, but should have {}, last {}",
                        level, block_count, expected,
                        previous_level_size);
                }

                let mut last_block_full = true;
                let mut entries = 0;

                // Read each block, if possible.
                for i in 0..block_count {
                    match self.read_block(level, i, i == block_count - 1) {
                        Ok(block) => {
                            last_block_full = block.full();
                            entries += block.valid_leaves();

                            if level > 0 {
                                let lower_index =  i as usize * LEAVES_IN_BLOCK * 2;

                                if let Some(x) =
                                    self.check_lower(&block, &self.blocks[level - 1], lower_index) {
                                    return Some(x);
                                }
                            }

                            self.blocks[level].push(block);
                        }
                        Err(x) => {
                            return Some(x);
                        }
                    }
                }

                if level == 0 {
                    self.entry_count      = entries;
                    self.entries_on_disk  = entries;
                }

                self.blocks_on_disk[level] = block_count;

                // Compute the number of entries to expect at the next level as a
                // consistency check.
                previous_level_size   = block_count;
                leaves_at_this_level  = block_count;

                if !last_block_full {
                    leaves_at_this_level -= 1;
                }

                leaves_at_this_level /= 2;

                // So if there are leaves at the next level, and there's not a file,
                // we have a problem...
                let last_level = level == self.file.len() - 1;

                if last_level && leaves_at_this_level > 0 {
                    return sem!("There is at least one missing file (level {}).", level);
                }
            }

            None
        }

        // Add a new level zero entry to the Merkle tree.  This leaf will represent
        // an actual transaction.
        pub fn append_hash(&mut self, hash_value: HashValue) -> Option<Error> {
            if self.entry_count == 0 {
                if self.blocks[0].len() != 0 {
                    return sem!("Level zero should be empty, but it has {} blocks",
                        self.blocks[0].len());
                }

                self.blocks[0].push(Block::new(0, 0));
                self.blocks[0][0].set_hash(&hash_value);
                self.entry_count = 1;
                return None;
            }
            
            // We might need to add a level, so include the next level in the count.
            // The loop will terminate via a break if there is no data for a new level.
            let levels = self.blocks.len() + 1;

            let mut current_hash = hash_value;

            for level in 0..levels {
                if level == levels - 1 {
                    let result = self.add_level();

                    if let Some(x) = result {
                        return Some(x);
                    }
                }

                // Pull what we need from the tree.  That's the current
                // non-empty block, and the hash of the left subtree
                // for that block, if it has a left sub-stree.
                let items = {
                    let block_list = &mut self.blocks[level];

                    if block_list.last().unwrap().full() {
                        let block_id = block_list.len() as u64;
                        let block    = Block::new(level as u32, block_id);

                        block_list.push(block);
                    }

                    let index = block_list.len() - 1;

                    let prev_top = 
                        if index & 1 != 0 {
                            let top_hash =
                                match block_list[index - 1].top_hash() {
                                    Some(x) => { x.clone() }
                                    None => {
                                        return sem!("No top hash for block {} at level {}",
                                                index - 1, level);
                                    }
                                };

                            Some(top_hash) 
                        } else {
                            None
                        };

                        (&mut block_list[index], prev_top)
                    };
                
                let (block, prev) = items;

                if let Some(x) = block.set_hash(&current_hash) {
                    return sem!("Tree corrupted:  set_hash:  {}", x);
                }

                // If this node of the tree is not full or doesn't
                // have a corresponding left subtree, we're done.
                if !block.full() || block.id() & 1 == 0 {
                    break;
                }

                // Okay, we have another hash to add to the tree.
                let left   = &prev.unwrap();
                let right  = block.top_hash().unwrap();

                current_hash = hash_pair(left, right);
            }

            // The entry count is for level zero (transaction) entries
            // only.
            self.entry_count += 1;
            None
        }

        // Add a new level to the tree.  This routine is called only
        // when there's data for the new layer, so allocate a block here.
        fn add_level(&mut self) -> Option<Error> {
            let level   = self.blocks.len();
            let path    = self.file_path(level);
            let result  = OpenOptions::new().read(true).write(true).create(true).open(path);

            match result {
                Err(x) => {
                    return Some(x);
                }
                Ok(x) => {
                    self.push_file(x);
                }
            }

            self.blocks[level].push(Block::new(level as u32, 0));
            assert!(self.blocks[level].len() == 1 && self.blocks.len() == level + 1);
            None
        }

        // Append a transaction to the Merkle tree.  It is encoded
        // as a UTF-8 string.
        pub fn append_string(&mut self, value : &String) -> Option<Error> {
            let mut hash_value = HashValue {
                hash:  [0; HASH_SIZE],
            };

            hash_value.hash.clone_from_slice(sha256::hash(value.as_ref()).as_ref());
            return self.append_hash(hash_value);
        }

        pub fn total_size(&self) -> u64 {
            return self.entry_count;
        }

        // Save the tree to disk.  Call this before exiting a process
        // that can update the tree.  Also, at some point, flushes for
        // transactional semantics might be important.
        pub fn write(&mut self) -> Option<Error> {
            if self.entry_count == 0 {
                return None;
            }

            let mut entries_at_this_level = self.entry_count;

            // Write each block level of the tree to its file.
            for level in 0..self.blocks.len() {
                let total_blocks = covered(entries_at_this_level, LEAVES_IN_BLOCK as u64);

                if total_blocks != self.blocks[level].len() as u64 {
                    return sem!("Level {} has {} blocks, but {} were expected",
                            level, self.blocks[level].len(), total_blocks);
                }

                assert!(total_blocks >= self.blocks_on_disk[level]);

                // Compute the number of full blocks at this level, and
                // the first block that needs to be written.  Always
                // rewrite the last block at this level because it might
                // have changed.  
                let disk_block_count = self.blocks_on_disk[level] as usize;

                let start_block =
                    if disk_block_count == 0 {
                        disk_block_count
                    } else {
                        disk_block_count - 1
                    };

                // Seek to the offset where we hope to put the block.
                // With some luck, this will help us recover from a
                // transient disk error.
                let start_offset = (start_block * BLOCK_SIZE) as u64;

                match self.file[level].seek(Start(start_offset)) {
                    Err(x) => { return Some(x); },
                    Ok(n) => {
                        if n != start_offset {
                            return sem!("A seek to {} returned {}.",
                                start_offset, n);
                        }
                    }
                }

                let mut last_block_full = true;

                // Loop over each block on this level that needs to be sent to disk.
                for i in start_block as u64..total_blocks  {
                    let block = &mut self.blocks[level][i as usize];

                    // Set the checksum if needed.
                    if !block.full() {
                        block.set_checksum();
                    }

                    if let Some(x) = block.check(level, i, true) {
                        return Some(x);
                    }

                    if i < total_blocks - 1 && !block.full() {
                        return sem!("Block {} at level {} should be full.",
                            i, level);
                    }

                    let result = self.file[level].write_all(block.as_bytes());

                    if let Err(x) = result {
                        return Some(x);
                    }

                    last_block_full = block.full();
                }

                // Sync the file to detect any errors and give us a better
                // shot at decent semantics.
                //
                // TODO:  Truncate the file and reset blocks on disk if an
                // error occurs?
                let result = self.file[level].sync_all();

                if let Err(x) = result {
                    return Some(x);
                }

                // Save the number of blocks we have written to disk and
                // compute the entries at the next level.
                self.blocks_on_disk[level] = total_blocks;
                entries_at_this_level = total_blocks;

                if !last_block_full {
                    entries_at_this_level -= 1;
                }

                entries_at_this_level /= 2;
            }

            self.entries_on_disk = self.entry_count;
            None
        }

        // Peform a consistency check of the disk representation of the tree.
        pub fn check_disk(&mut self, flushed: bool) -> Option<Error> {
            let mut entries_at_this_level = self.entry_count;
            
            let mut lower = Vec::new();

            // Check the blocks at each level.
            for level in 0..self.blocks.len() {
                // First, get the file size check it against expectations.
                let disk_bytes = match self.file[level].seek(End(0)) {
                    Ok(n) => { n },
                    Err(x) => {
                        return sem!("check_disk:  The  size seek failed:  {}", x);
                    },
                };

                let blocks_on_disk = self.blocks_on_disk[level];
                let expected_size  = blocks_on_disk * BLOCK_SIZE as u64;

                if disk_bytes != expected_size {
                    return sem!("check_disk:  The file size ({}) should be {}.",
                        disk_bytes, expected_size);
                }

                // If the disk image is up to date, check that the number
                // of blocks on disk and in the list match.
                let list_length = self.blocks[level].len() as u64;

                if flushed && blocks_on_disk != list_length {
                    return sem!(
                        "check_disk:  The count {} at level {} should be {}.",
                         blocks_on_disk, level, list_length);
                }

                if let Err(x) = self.file[level].seek(Start(0)) {
                    return sem!("check_disk:  The read seek failed:  {}", x);
                }

                let mut entry_count     = 0_u64;
                let mut last_block_full = true;
                let mut current         = Vec::new();

                // Check each block on disk.
                for i in 0..blocks_on_disk {
                    let last = i == blocks_on_disk - 1;

                    match self.read_block(level, i, last) {
                        Ok(block) => {
                            last_block_full = block.full();
                            entry_count += block.valid_leaves();
                            
                            if lower.len() > 0 {
                                let lower_index =  i as usize * LEAVES_IN_BLOCK * 2;

                                if let Some(x) = self.check_lower(&block, &lower, lower_index) {
                                    return Some(x);
                                }
                            }

                            current.push(block);
                        },
                        Err(x) => {
                            return sem!("check_disk:  A read failed:  {}", x);
                        }
                    }
                }

                lower = current;

                if flushed && entry_count != entries_at_this_level {
                    return sem!("check_disk:  The entry counts ({}, {}) \
                        at level {} didn't match.",
                        entry_count, entries_at_this_level, level);
                }

                // The first time through, we compare the in-memory entry count to the
                // count from disk, but for further iterations, there's no added value,
                // so just predict the number of blocks to expect based on the count on
                // disk.
                entries_at_this_level = blocks_on_disk;

                if !last_block_full {
                    entries_at_this_level -= 1;
                }

                entries_at_this_level /= 2;

                let last_level = level == self.blocks.len() - 1;

                if last_level && entries_at_this_level > 0 {
                    return sem!("There is at least one missing file (for level {}).", level);
                }
            }

            None
        }

        // Read a block from disk and return its memory representation.  Currently,
        // that is the same as the bytes on disk.
        fn read_block(&mut self, level: usize, id: u64, last: bool) -> Result<Block, Error> {
            let block = match self.read_struct(level) {
                Ok(block) => { block },
                Err(x) => { return Err(x); }
            };

            if let Some(x) = block.check(level, id, true) {
                return Err(Error::new(ErrorKind::Other,
                    format!("Invalid disk block:  {}", x)));
            }

            if !last && !block.full() {
                return Err(Error::new(ErrorKind::Other,
                    format!("Block {} at level {} from disk is not full.",
                    id, level)));
            }

            Ok(block)
        }

        // Read the disk block directly into the memory result, if the disk
        // permits.
        //
        // TODO:  In theory, if the level is not zero, we could reconstruct
        // the block.
        fn read_struct(&mut self, level: usize) -> Result<Block, Error> {
            unsafe {
                let mut s = std::mem::uninitialized();

                let buffer =
                    from_raw_parts_mut(&mut s as *mut Block as *mut u8,
                        BLOCK_SIZE);

                match self.file[level].read_exact(buffer) {
                    Ok(()) => { return Ok(s) },
                    Err(e) => {
                        std::mem::forget(s);
                        return Err(e);
                    }
                }
            };
        }

        // Check the in-memory version of the Merkle tree for consistency.
        pub fn check(&self) -> Option<Error> {
            let mut leaves_at_this_level = self.entry_count;
            let mut last_blocks          = 0;
            let mut last_block_full      = true;

            // Check each level.
            for level in 0..self.blocks.len() {
                let blocks_at_this_level =
                    covered(leaves_at_this_level, LEAVES_IN_BLOCK as u64) as usize;
                let list_length = self.blocks[level].len();

                if list_length != blocks_at_this_level {
                    return sem!(
                        "check:  The expected block count ({}) at level {} \
                            should be {}, last {}, full {}, entries {}",
                        blocks_at_this_level, level, list_length,
                        last_blocks, last_block_full, self.entry_count);
                }

                let mut leaf_count = 0;
                last_block_full = true;

                // Now check each block at this level.
                for block_id in 0..blocks_at_this_level {
                    let last        = block_id == blocks_at_this_level - 1;
                    let block       = &self.blocks[level][block_id];
                    last_block_full = block.full();

                    if !last && !last_block_full {
                        return sem!("check:  Block {} at level {} should be full.",
                            block_id, level);
                    }

                    if let Some(x) = block.check(level, block_id as u64, false) {
                        return Some(x);
                    }

                    if level > 0 {
                        let lower_index =  block_id * LEAVES_IN_BLOCK * 2;

                        if let Some(x) = self.check_lower(block, &self.blocks[level - 1], lower_index) {
                            return Some(x);
                        }
                    }

                    leaf_count += block.valid_leaves() as u64;
                }
                
                if leaf_count != leaves_at_this_level {
                    return sem!(
                        "check:  The entry counts ({}, {}) at level {} did not match",
                        leaf_count, leaves_at_this_level, level);
                }

                // Advance to the next level of the  tree.  Compute the entries that
                // we expect to be there.
                last_blocks = blocks_at_this_level;
                leaves_at_this_level = last_blocks as u64;

                if !last_block_full {
                    leaves_at_this_level -= 1;
                }

                leaves_at_this_level /= 2;
            }

            None
        }

        // Check that a block contains the correct hashes from the lower-level
        // blocks.
        fn check_lower(&self, block: &Block, lower:  &Vec<Block>, start_block: usize) -> Option<Error> {
            let mut block_index = start_block;

            for i in 0..block.valid_leaves() as usize {
                let left = match lower[block_index].top_hash() {
                    None => {
                        return sem!("The lower hash at {} is missing.", block_index);
                    }
                    Some(x) => {
                        x
                    }
                };

                let right = match lower[block_index + 1].top_hash() {
                    None => {
                        return sem!("The lower hash at {} is missing.", block_index + 1);
                    }
                    Some(x) => {
                        x
                    }
                };

                let hash = hash_pair(left, right);

                if hash != block.hashes[i] {
                    return sem!("hash[{}] for block {} at level {} didn't match.",
                        i, block.id(), block.level());
                }

                block_index += 2;
            }

            None
        }

        // Reset the disk image to null.  This action will cause the
        // entire tree to be written to disk on the next write call,
        // which can be useful in the presence of errors.
        pub fn disk_reset(&mut self) {
            for i in 0..self.blocks_on_disk.len() {
                self.blocks_on_disk[i] = 0;
                let _ = self.file[i].set_len(0);
            }
        }

        pub fn path(&self) -> String {
            self.path.clone()
        }
    }
    
    pub fn test() {
        println!("The block size is {} bytes.", mem::size_of::<Block>());
        println!("A block contains {} leaves.", LEAVES_IN_BLOCK);
        test_header();
        test_block();
        test_hash_pair();
        test_basic_tree();
        test_tree();
        println!("Test passed.");
    }

    fn test_header() {
        let mut header = BlockHeader::new(3, 5);

        if let Some(x) = header.check(3, 5) {
            panic!("new() returned an invalid header:  {}", x);
        }

        header.header_mark ^= 1;

        if let None = header.check(3, 5) {
            panic!("check didn't detect an invalid header.");
        }

        header = BlockHeader::new(3, 5);
        header.level += 1;

        if let None = header.check(3, 5) {
            panic!("check didn't detect an invalid level.")
        }

        header = BlockHeader::new(3, 5);
        header.id += 1;

        if let None = header.check(3, 5) {
            panic!("check didn't detect an invalid id.")
        }

        header = BlockHeader::new(3, 5);
        header.valid_leaves = LEAVES_IN_BLOCK as u16 + 1;

        if let None = header.check(3, 5) {
            panic!("check didn't detect an invalid leaf count.")
        }
    }

    fn check_block(block: &Block, level: usize, id: u64, disk_format:  bool) {
        if let Some(x) = block.check(level, id, disk_format) {
            panic!("Unexpected block check error:  {}", x);
        }
    }

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

        // Now iterate and create a full block.  Check that it
        // stays consistent as we go, and inject errors, as well.
        let mut hash = HashValue::new();

        for i in 0..LEAVES_IN_BLOCK {
            hash.hash[0] = 1;
            hash.hash[1] = i as u8;

            let error = block.set_hash(&hash);

            if let Some(x) = error {
                panic!("Unexpected set_hash error:  {} at {}", x, i);
            }

            block.set_checksum();

            if let Some(x) = block.check(1, 2, false) {
                panic!("Block check failure at iteration {}:  {}",
                    i, x);
            }

            // If the block is not full, there will still be hash entries
            // that are set to HashBlock::new.  Pick one of them and corrupt
            // it.
            if !block.full() {
                let saved_hash = block.hashes[i + 1].clone();
                block.hashes[i + 1].hash[0] ^= 1;
                block.set_checksum();

                if let None = block.check(1, 2, false) {
                    panic!("check didn't see a corrupted empty hash:  {:?}",
                        block.hashes[i + 1].hash);
                }

                block.hashes[i + 1] = saved_hash;
                block.set_checksum();
                check_block(&block, 1, 2, false);
            }

            // Okay, now try corrupting a block that has been set with a value
            // from SHA256.  We make the assumption here and elsewhere that it
            // will not produce HashBlock::new.
            let index =
                if i > 0 {
                    i - 1
                } else {
                    i
                };

            let saved_hash = block.hashes[index];

            block.hashes[index] = HashValue::new();
            block.set_checksum();

            if let None = block.check(1, 2, false) {
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
        for i in 0..LEAVES_IN_BLOCK + 1 {
            let error = block.set_hash(&hash);

            if let None = error {
                panic!("Missing error at iteration {}.", i);
            }
        }

        if let None = block.top_hash() {
            panic!("top_hash failed on a full block.");
        }

        // Now check that mismatched block ids and levels are
        // caught.
        if let Some(x) = block.check(1, 2, false) {
            panic!("The block was corrupted by set_hash:  {}", x);
        }

        if let None = block.check(0, 2, false) {
            panic!("Bad block id passed");
        }

        if let None = block.check(1, 3, false) {
            panic!("Bad level passed");
        }

        // Corrupt checksum[0] and see whether that's caught.
        block.header.check_bits.bits[0] ^= 1;

        if let None = block.check(1, 2, false) {
            panic!("Bad hash[0] passed")
        }

        block.header.check_bits.bits[0] ^= 1;

        if let Some(_x) = block.check(1, 2, false) {
            panic!("Testing error");
        }

        // Now corrupt checksum[last] and do the checks.
        block.header.check_bits.bits[CHECK_SIZE - 1] ^= 1;

        if let None = block.check(1, 2, false) {
            panic!("Bad hash[last] passed")
        }

        block.header.check_bits.bits[CHECK_SIZE - 1] ^= 1;

        if let Some(_x) = block.check(1, 2, false) {
            panic!("Testing error");
        }

        // Okay, corrupt a hash in the subtree.
        block.hashes[LEAVES_IN_BLOCK].hash[0] ^= 1;

        if let None = block.check(1, 2, true) {
            panic!("A corrupted subtree passed.");
        }

        // Try redoing an insertion...
        block.header.valid_leaves = 0;
        
        if let None = block.set_hash(&hash) {
            panic!("set_hash overwrote a full hash.");
        }
    }

    fn test_hash_pair() {
        let mut a = [0; 2 * HASH_SIZE];

        for i in 0..2 * HASH_SIZE {
            a[i] = i as u8;
        }

        let digest     = sha256::hash(&a[0..2 * HASH_SIZE]);
        let mut left   = HashValue::new();
        let mut right  = HashValue::new();
        
        left.hash.clone_from_slice(&a[0..HASH_SIZE]);
        right.hash.clone_from_slice(&a[HASH_SIZE..2 * HASH_SIZE]);
        
        let check = hash_pair(&left, &right);

        if check.hash != digest[0..HASH_SIZE] {
            panic!("hash_pair failed.");
        }
    }

    fn test_basic_tree() {
        assert!(mem::size_of::<CheckBits>()   == CHECK_SIZE);
        assert!(mem::size_of::<BlockHeader>() == HASH_SIZE);
        assert!(mem::size_of::<Digest>()      == HASH_SIZE);
        assert!(mem::size_of::<Block>()       == BLOCK_SIZE);

        let result = AppendOnlyMerkle::open(&"no such file".to_string());

        match result {
            Ok(_tree) => {
                panic!("Open found a non-existent tree.")
            }
            Err(x) => {
                if x.kind() != ErrorKind::NotFound {
                    panic!("Error creating the tree:  {}", x);
                }
            }
        }

        let result = AppendOnlyMerkle::create(&".".to_string());

        match result {
            Ok(_tree) => {
                panic!("Created a tree from \".\".");
            }
            Err(x) => {
                if x.kind() != ErrorKind::AlreadyExists {
                    panic!("Unexpected error:  {}", x);
                }
            }
        }

        let path    = "test_tree".to_string();
        let _       = std::fs::remove_file(&path);
        let result  = AppendOnlyMerkle::create(&path);

        let mut tree =
            match result {
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
            panic!("The path member returned {} instead of {}",
                tree.path(), path);
        }

        test_append(&mut tree, 0, false);
        test_append(&mut tree, 1, false);
        write_tree(&mut tree);
        let count = tree.total_size();

        tree =
            match AppendOnlyMerkle::open(&path) {
                Err(x) => {
                    panic!("Open failed:  {}", x);
                }
                Ok(x) => {
                    x
                }
            };

        if tree.total_size() != count {
            panic!("The counts did not match ({}, {})", tree.total_size(), count);
        }

        // Now try deleting a file for an index level to see whether that is detected.
        // First, we make the tree large enough to need a level 1 index block.
        for i in 0..2 * LEAVES_IN_BLOCK as u64 {
            test_append(&mut tree, i + 2, false);
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

        let level_path = path.clone() + ".1";

        if let Err(x) =  std::fs::remove_file(&level_path) {
            panic!("remove_file failed:  {}", x);
        }

        let expected = "There is at least one missing file (level 0).";

        match AppendOnlyMerkle::open(&path) {
            Err(x) => {
                if x.to_string() != expected {
                    panic!("Unexpected error:  {}", x);
                }
            }
            Ok(_x) => {
                panic!("Open didn't fail.");
            }
        }

        let _ = std::fs::remove_file(&path);
    }

    fn create_test_hash(i: u64, verbose: bool) -> HashValue {
        let mut buffer      = [0_u8; HASH_SIZE];
        let mut hash_value  = HashValue::new();

        for i in 0..buffer.len() {
            buffer[i] = 0xfe;
        }

        buffer.as_mut().write_u64::<LittleEndian>(i).expect("le write");
        hash_value.hash.clone_from_slice(&buffer[0..HASH_SIZE]);

        if verbose {
            println!("Create hash {}", i);
        }

        hash_value
    }

    fn check_tree(tree: &AppendOnlyMerkle) {
        if let Some(x) = tree.check() {
            panic!("Got check error:  {}", x);
        }
    }

    fn check_disk_tree(tree: &mut AppendOnlyMerkle, flushed:  bool) {
        if let Some(x) = tree.check_disk(flushed) {
            panic!("Got disk check error:  {}", x);
        }
    }

    fn write_tree(tree: &mut AppendOnlyMerkle) {
        if let Some(x) = tree.write() {
            panic!("tree.write failed:  {}", x);
        }
    }

    fn test_append(tree: &mut AppendOnlyMerkle, i: u64, verbose: bool) {
        let hash   = create_test_hash(i, verbose);
        let result = tree.append_hash(hash);

        if let Some(x) = result {
            panic!("append_hash failed:  {}", x);
        }
    }

    fn test_tree() {
        let path   = "test_tree".to_string();
        let _      = std::fs::remove_file(&path);
        let result = AppendOnlyMerkle::create(&path);

        let mut tree =
            match result {
                Err(x) => {
                    panic!("Unexpected error:  {}", x);
                }
                Ok(tree) => {
                    tree
                }
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

        let leaves_in_block  = LEAVES_IN_BLOCK as u64;
        let mut leaves_per_next  = leaves_in_block;

        for _t in entry_id..1024 * 1024 {
            if entry_id % (4 * 1024) == 0 {
                println!("At entry {}", entry_id);
            }

            test_append(&mut tree, entry_id, false);
            entry_id += 1;

            // Do an exponential-ish backoff on the checking, as it is expensive.
            if  entry_id == leaves_per_next - 1
             || entry_id == leaves_per_next
             || entry_id == leaves_per_next + 1
             || entry_id == 2 * leaves_per_next
             || entry_id == 2 * leaves_per_next + 1
             || entry_id == 3 * leaves_per_next {
                println!("Checking the tree at {}.", entry_id);
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
        check_disk_tree(&mut tree, false);

        // Now sync the tree to disk and see whether everything checks.
        write_tree(&mut tree);
        check_tree(&tree);  
        check_disk_tree(&mut tree, true);
    
        // Multiple writes should be okay.
        write_tree(&mut tree);
        check_tree(&tree);
        check_disk_tree(&mut tree, true);

        // Check the tree every so often while we write a lot of hashes
        // to it.
        let mut countdown = thread_rng().gen::<u32>() % (1024 * 1024);

        for _t in entry_id..512 * 1024 * 1024 {
            if entry_id % (64 * 1024) == 0 {
                check_tree(&tree);
                println!("At entry {}", entry_id);
            }

            test_append(&mut tree, entry_id, false);

            entry_id  += 1;
            countdown -= 1;

            if countdown <= 0 {
                println!("Checking the disk image at {}", entry_id);
                check_tree(&tree);
                check_disk_tree(&mut tree, false);

                // Sync the tree to disk.
                write_tree(&mut tree);

                if thread_rng().gen::<u32>() % 4 == 0 {
                    println!("Rechecking the disk.");
                    check_tree(&tree);
                    check_disk_tree(&mut tree, true);

                    if let Err(x) = AppendOnlyMerkle::open(&path) {
                        panic!("open failed:  {}", x);
                    }
                }

                println!("Done with checking.");
                // Restart the countdown.
                countdown  = thread_rng().gen::<u32>() % (2 * 1024 * 1024);
                countdown += 1;
            }
        }

        // Check the final tree.
        check_tree(&tree);
        check_disk_tree(&mut tree, false);

        // Sync the final tree to disk, and then check it.
        write_tree(&mut tree);
        check_disk_tree(&mut tree, true);
        check_tree(&tree);

        // Save the current size, and try reopening the tree.
        let count  = tree.total_size();
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
        check_tree(&tree);
        check_disk_tree(&mut tree, true);

        // Its size should match the old tree.
        if count != tree.total_size() {
            panic!("The sizes did not match, {} vs {}.", count, tree.total_size());
        }
    
        // Okay, do a minimal test of the rewrite function.
        tree.disk_reset();
        write_tree(&mut tree);
        check_disk_tree(&mut tree, true);

        let _ = std::fs::remove_file(&path);
    }
}
