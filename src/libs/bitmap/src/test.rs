#![allow(missing_docs)]

use super::*;
use cryptohash::sha256::{Digest, DIGESTBYTES};
use rand::Rng;
use std::fs;
use std::fs::OpenOptions;
use std::mem;

fn validate_checksum(bitmap: &mut BitMap, location: String) {
    let checksum1 = bitmap.compute_checksum();
    bitmap.clear_checksum_cache();
    let checksum2 = bitmap.compute_checksum();

    if checksum1 != checksum2 {
        panic!(
            "validate_checksum: {} vs {}:  at {}",
            checksum1.0[0], checksum2.0[0], location
        );
    }
}

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

    if header.validate(BIT_ARRAY, id).is_ok() {
        panic!("Validation failed to detect a bad magic number.");
    }

    header.magic ^= 1;
    assert!(header.validate(BIT_ARRAY, id).is_ok());

    header.count = (BLOCK_BITS + 1) as u32;

    if header.validate(BIT_ARRAY, id).is_ok() {
        panic!("Validation failed to detect a bad count.");
    }

    header.count = 0;
    assert!(header.validate(BIT_ARRAY, id).is_ok());
    header.bit_id ^= 1;

    if header.validate(BIT_ARRAY, id).is_ok() {
        panic!("Validation failed to detect a bad id.");
    }

    header.bit_id ^= 1;
    header.contents = BIT_INVALID;

    if header.validate(BIT_ARRAY, id).is_ok() {
        panic!("Validation failed to detect a bad contents type.");
    }

    header.contents = BIT_ARRAY;
    assert!(header.validate(BIT_ARRAY, id).is_ok());
    header.pad_1 = 1;

    if header.validate(BIT_ARRAY, id).is_ok() {
        panic!("Validation failed to detect a bad pad_1.");
    }

    header.pad_1 = 0;
    assert!(header.validate(BIT_ARRAY, id).is_ok());
    header.pad_2 = 1;

    if header.validate(BIT_ARRAY, id).is_ok() {
        panic!("Validation failed to detect a bad pad_2.");
    }

    header.pad_2 = 0;
    assert!(header.validate(BIT_ARRAY, id).is_ok());

    let header = BlockHeader::new(BIT_DESC_SET, 0).unwrap();
    assert!(header.contents == BIT_DESC_SET);
    assert!(header.bit_id == 0);

    assert!(header.count == 0);
    assert!(header.checksum == CheckBlock::new());

    if BlockHeader::new(BIT_INVALID, 0).is_ok() {
        panic!("An invalid block type was accepted.");
    }
}

#[test]
fn test_block() {
    println!("The block size is {}.", mem::size_of::<BitBlock>());
    println!("The header size is {}.", mem::size_of::<BlockHeader>());
    assert!(mem::size_of::<BlockHeader>() == HEADER_SIZE);
    assert!(mem::size_of::<BitBlock>() == BLOCK_SIZE);
    let mut block = BitBlock::new(BIT_DESC_CLEAR, 32).unwrap();
    assert!(block.header.contents == BIT_DESC_CLEAR);
    assert!(block.header.bit_id == 32 * BLOCK_BITS as u64);

    block.set_checksum();

    if block.validate(BIT_DESC_CLEAR, 32).is_err() {
        panic!("Block validation failed.");
    }

    block.header.checksum.bytes[0] ^= 1;

    if block.validate(BIT_DESC_CLEAR, 32).is_ok() {
        panic!(
            "Block validation didn't detect a bad checksum: {:?} vs {:?}.",
            block.header.checksum.bytes,
            block.compute_checksum()
        );
    }
}

// Do a simple test of the bitmap-level functions.
#[test]
fn test_basic_bitmap() {
    let path = "basic_bitmap";
    let _ = fs::remove_file(path);

    // Create a new bitmap.
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create_new(true)
        .open(path)
        .unwrap();

    let mut bitmap = BitMap::create(file).unwrap();

    if let Err(e) = bitmap.write() {
        panic!("Write failed:  {}", e);
    }

    // Check our definition of the checksum of an empty tree.
    if bitmap.compute_checksum() != (Digest([0_u8; DIGESTBYTES])) {
        panic!("compute_checksum() failed on an empty tree");
    }

    if bitmap.set(1).is_ok() {
        panic!("set worked with an invalid index.");
    }

    assert!(bitmap.validate(true));

    // Close the bitmap.
    drop(bitmap);

    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(path)
        .unwrap();

    let mut bitmap = BitMap::open(file).unwrap();
    assert!(bitmap.validate(true));

    // Set some bits in the map.
    for i in 0..2 * BLOCK_BITS + 2 {
        bitmap.set(i).unwrap();
        assert!(bitmap.query(i).unwrap());
        assert!(bitmap.size() == i + 1);

        // Check a query beyond the end of the bitmap.
        if bitmap.query(i + 1).is_ok() {
            panic!("Index {} should be out of range.", i + 1);
        }

        // Validate the data structure from time to time.
        if i % 4096 == 1 {
            assert!(bitmap.validate(false));
            validate_checksum(&mut bitmap, "basic ".to_owned() + &i.to_string());
        }

        // Try a flush now and then.
        if i % (BLOCK_BITS / 2) == 0 {
            if let Err(e) = bitmap.flush_old(0) {
                panic!("flush_old failed:  {}", e);
            }
        }
    }

    let checksum1 = bitmap.compute_checksum();
    let checksum2 = bitmap.compute_checksum();
    println!("Checksum 1: {:?}", checksum1);
    println!("Checksum 2: {:?}", checksum2);
    assert!(checksum1 == checksum2);

    bitmap.clear_checksum_cache();
    let checksum3 = bitmap.compute_checksum();
    assert!(checksum1 == checksum3);

    // Serialize a part of the bitmap.
    let s1 = bitmap.serialize_partial(vec![0, BLOCK_BITS], 1);

    if let Err(x) = BitMap::deserialize(&s1) {
        panic!("deserialize(&s1) failed:  {}", x);
    }

    // Pick a random version number and serialize the entire
    // bitmap with that version applied.
    let sparse_version = 0x010203;
    let s2 = bitmap.serialize(sparse_version);

    if let Err(x) = BitMap::deserialize(&s2) {
        panic!("deserialize(&s2) failed:  {}", x);
    }

    // Create SparseMaps from the serialized forms.
    let partial_map = SparseMap::new(&s1).unwrap();
    let sparse_map = SparseMap::new(&s2).unwrap();

    assert!(partial_map.validate_checksum());
    assert!(sparse_map.validate_checksum());

    println!(
        "sparse_version = {}, sparse_map.version() = {}",
        sparse_version,
        sparse_map.version()
    );
    assert!(sparse_map.version() == sparse_version as u64);
    assert!(sparse_map.checksum() == bitmap.compute_checksum());

    // Check that the contents match expectations.
    for i in 0..bitmap.size() as u64 {
        let bitmap_result = bitmap.query(i as usize).unwrap();
        let sparse_result = sparse_map.query(i).unwrap();

        // Check that the sparse bitmap matches the source bitmap.
        if bitmap_result != sparse_result {
            panic!("Sparse mismatch at {}", i);
        }

        // Now check the partial bitmap.
        if let Ok(partial_result) = partial_map.query(i) {
            assert!(partial_result == bitmap_result);
        } else {
            assert!(i >= 2 * BLOCK_BITS as u64);
        }
    }

    if bitmap.query(bitmap.size()).is_ok() {
        panic!("bitmap query at size passed.");
    }

    if sparse_map.query(bitmap.size() as u64).is_ok() {
        panic!("sparse_map query at size passed.");
    }

    if partial_map.query(bitmap.size() as u64).is_ok() {
        panic!("partial_map query at size passed.");
    }

    // Test some more manipulations of the bitmap.
    for i in 0..bitmap.size() {
        if i & 1 == 0 {
            bitmap.clear(i).unwrap();
            assert!(!bitmap.query(i).unwrap());

            if i % 273 == 0 {
                assert!(bitmap.validate(false));
                validate_checksum(&mut bitmap, "rewrite ".to_owned() + &i.to_string());
            }
        }
    }

    for i in 0..bitmap.size() {
        assert!(bitmap.query(i).unwrap() != (i & 1 == 0));
    }

    if bitmap.write().is_err() {
        panic!("write failed.");
    }

    let bits_initialized = bitmap.size();

    if let Err(e) = bitmap.write() {
        panic!("write failed:  {}", e);
    }

    if let Err(e) = bitmap.flush_old(0) {
        panic!("flush_old failed:  {}", e);
    }

    assert!(bitmap.validate(true));
    drop(bitmap);

    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(path)
        .unwrap();

    let mut bitmap = BitMap::open(file).unwrap();
    assert!(bits_initialized == bitmap.size());
    assert!(bits_initialized % BLOCK_BITS != 0);
    assert!(bitmap.validate(false));
    validate_checksum(&mut bitmap, "reopen".to_owned());

    for i in 0..bits_initialized {
        assert!(bitmap.query(i).unwrap() != (i & 1 == 0));

        if i % BLOCK_BITS == 0 {
            assert!(bitmap.validate(false));
        }
    }

    let bit = bitmap.append().unwrap();
    assert!(bit == bits_initialized as u64);
    assert!(bitmap.validate(false));

    let bit = bitmap.append().unwrap();
    assert!(bit == bits_initialized as u64 + 1);
    bitmap.write().unwrap();
    assert!(bitmap.validate(true));

    // Expand the bitmap to allow for more testing.

    for _ in 0..4 * 1024 * 1024 {
        bitmap.append().unwrap();
    }

    let mut rng = rand::thread_rng();
    let mut countdown = rng.gen_range(0..50);

    // Manipulate some random bits and check the checksumming.
    for i in 0..4000 {
        let bit = rng.gen_range(0..bitmap.size() + 1);
        let current = if bit < bitmap.size {
            bitmap.query(bit).unwrap()
        } else {
            false
        };

        // Now flip the bit, or append a "true" bit.
        if current {
            bitmap.clear(bit).unwrap();
        } else {
            bitmap.set(bit).unwrap();
        }

        countdown -= 1;

        if countdown <= 0 {
            validate_checksum(&mut bitmap, "random ".to_owned() + &i.to_string());
            countdown = rng.gen_range(0..50);
        }
    }

    drop(bitmap);

    let _ = fs::remove_file(path);
}

#[test]
fn test_counting() {
    let map = create_map();

    for i in 0..8 {
        let value = 1 << i;
        println!(
            "count_byte({}) = {}, map[{}] = {}",
            value,
            count_byte(value),
            value,
            map[value]
        );
        assert!(count_byte(value) == 1);
        assert!(map[value] == 1);
    }

    for i in 2..9 {
        let value = (1 << i) - 1;

        println!(
            "count_byte({}) = {}, map[{}] = {}",
            value,
            count_byte(value),
            value,
            map[value]
        );
        assert!(count_byte(value) == i);
        assert!(map[value] == i);
    }

    for i in 1..8 {
        let value = (1 << i) + (1 << (i - 1));

        println!(
            "count_byte({}) = {}, map[{}] = {}",
            value,
            count_byte(value),
            value,
            map[value]
        );
        assert!(count_byte(value) == 2);
        assert!(map[value] == 2);
    }
}
