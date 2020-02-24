//! # An authenticated key value store mapping SHA-256 keys to values implemented
//! # as a Sparse Merkle Tree
//!
//!
use cryptohash::sha256;
use serde::{Deserialize, Serialize};
use sha256::DIGESTBYTES;
use std::collections::HashMap;
use std::io::Error;
use std::io::prelude::Read;
use std::fs::File;

pub fn get_bit(b: &[u8; 32], index: usize) -> bool {
  b[index >> 3] & (1_u8 << (index & 7)) != 0
}

pub fn set_bit(b: &mut [u8; 32], index: usize) {
  b[index >> 3] |= 1_u8 << (index & 7);
}

pub fn clear_bit(b: &mut [u8; 32], index: usize) {
  b[index >> 3] &= !(1_u8 << (index & 7));
}

pub fn flip_bit(b: &mut [u8; 32], index: usize) {
  b[index >> 3] ^= 1_u8 << (index & 7);
}

fn hash_256(value: impl AsRef<[u8]>) -> Hash256 {
  sha256::hash(value.as_ref()).0
}

// Compute the hash of two hashes.  This Merkle tree is a binary
// representation, so this is a common operation.
fn hash_pair(left: &Hash256, right: &Hash256) -> Hash256 {
  if left == &ZERO_DIGEST && right == &ZERO_DIGEST {
    ZERO_DIGEST
  } else {
    let mut data = [0_u8; 2 * DIGESTBYTES];

    data[0..DIGESTBYTES].clone_from_slice(left);
    data[DIGESTBYTES..2 * DIGESTBYTES].clone_from_slice(right);

    let digest = sha256::hash(&data);
    digest.0
  }
}

// 256 bit values are used both as keys, and as per node hashes. To avoid confusion,
// we've given different types to these.

pub type Key = [u8; DIGESTBYTES];
pub type Hash256 = [u8; DIGESTBYTES];

const ZERO_DIGEST: Hash256 = [0; DIGESTBYTES];
const ZERO_KEY: Key = [0; DIGESTBYTES];

// We apply the following optimization. Assuming 'H' is our hash function, we set H(leaf) = 0u256
// and H(0u256, 0u256) = 0u256.

/// Index of a node in a Sparse Merkle Tree.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
struct TreeNodeIndex {
  // The path starts from the first bit (the LSB of the first byte), and ends at
  // the bit indexed by `depth`. Bit 0 means left, and bit 1 means right. Bits beyond
  // the `depth`-th bit are irrelevant, and are always zeros.
  bit_path: Key,

  // The root has depth of 0, and the leaves have depth of 256.
  depth: usize,
}

impl TreeNodeIndex {
  /// Get a new TreeNodeIndex of the leaf corresponding to the given key.
  fn leaf(key: Key) -> Self {
    Self { bit_path: key,
           depth: 256 }
  }

  /// Index of the root.
  fn root() -> Self {
    Self { bit_path: ZERO_KEY,
           depth: 0 }
  }

  /// Whether this is the root.
  fn is_root(&self) -> bool {
    self.depth == 0
  }

  /// Whether this is a left subnode.
  fn is_left(&self) -> bool {
    self.depth > 0 && !get_bit(&self.bit_path, self.depth - 1)
  }

  /// Returns the index of the sibling of this node. Returns `None` if `self`
  // is the root.
  fn sibling(&self) -> Option<TreeNodeIndex> {
    if self.is_root() {
      return None;
    }

    let mut result = self.clone();
    flip_bit(&mut result.bit_path, result.depth - 1);
    Some(result)
  }

  /// Change `self` to the index of its parent node. Panics if `self` is the root.
  fn move_up(&mut self) {
    assert!(self.depth > 0, "Cannot move up from the root");
    clear_bit(&mut self.bit_path, self.depth - 1);
    self.depth -= 1;
  }
}

/// Merkle proof of a certain triple (SMT-merkle-root, key, value).
#[derive(PartialEq, Eq, Debug)]
pub struct MerkleProof {
  /// Whether the siblings along the path to the root are non-default hashes.
  pub bitmap: [u8; 32],

  pub hashes: Vec<Hash256>,
}

/// SmtMap256 is Sparse Merkle Tree Map from 256-bit keys to an optional value
//  and supports generating 256-bit merkle (non) inclusion proofs.
/// Initially, each  of the 2**256 possible keys has a default value of None,
/// which has a hash value of zero.
///
/// Each leaf corresponds to a key-value pair. The key is the bit-path from
/// the root to the leaf (see the documentation for TreeNodeIndex).
///
/// The hash of the leaf node is a 256 bit zero value. The hash of an non-leaf
/// node is calculated by hashing (using keccak-256) the concatenation of the
/// hashes of its two sub-nodes.
#[derive(Clone, Default, Debug, Serialize, Deserialize)]
pub struct SmtMap256<Value: AsRef<[u8]>> {
  kvs: HashMap<Key, Value>,

  // Hash values of both leaf and inner nodes.
  hashes: HashMap<TreeNodeIndex, Hash256>,
}

impl<Value: AsRef<[u8]>> SmtMap256<Value> {
  /// Returns a new SMT-Map where all keys have the default value (zero).
  pub fn new() -> Self {
    Self { kvs: HashMap::new(),
           hashes: HashMap::new() }
  }

  /// Sets the value of a key. Returns the previous value corresponding to the key.
  pub fn set(&mut self, key: impl AsRef<[u8]>, value: Option<Value>) -> Option<Value> {
    // Update the hash of the leaf.
    let key = hash_256(key.as_ref());
    let mut index = TreeNodeIndex::leaf(key);
    let mut hash: Hash256 = value.as_ref().map(hash_256).unwrap_or(ZERO_DIGEST);
    self.update_hash(&index, &hash);

    // Update the hashes of the inner nodes along the path.
    while !index.is_root() {
      let sibling_hash = self.get_hash(&index.sibling().unwrap());

      hash = if index.is_left() {
        hash_pair(&hash, &sibling_hash)
      } else {
        hash_pair(&sibling_hash, &hash)
      };
      index.move_up();
      self.update_hash(&index, &hash);
    }

    if let Some(v) = value {
      self.kvs.insert(key, v)
    } else {
      self.kvs.remove(&key)
    }
  }

  /// Returns a reference to the value of a key.
  pub fn get(&self, key: impl AsRef<[u8]>) -> Option<&Value> {
    let key = hash_256(key.as_ref());
    self.kvs.get(&key)
  }

  /// Returns a reference to the value of the key with merkle proof.
  pub fn get_with_proof(&self, key: impl AsRef<[u8]>) -> (Option<&Value>, MerkleProof) {
    let mut bitmap: Key = ZERO_KEY;
    let mut sibling_hashes = Vec::new();
    let key = hash_256(key.as_ref());
    let mut index = TreeNodeIndex::leaf(key);
    for i in 0..256 {
      if let Some(sibling_hash) = self.hashes.get(&index.sibling().unwrap()) {
        set_bit(&mut bitmap, i);
        sibling_hashes.push(*sibling_hash);
      }
      index.move_up();
    }
    (self.get(key),
     MerkleProof { bitmap,
                   hashes: sibling_hashes })
  }

  /// Returns the Merkle root
  pub fn merkle_root(&self) -> &Hash256 {
    self.get_hash(&TreeNodeIndex::root())
  }

  /// Returns `true` when proof is valid, `false` otherwise.
  pub fn check_merkle_proof(&self, key: impl AsRef<[u8]>, value: Option<&Value>, proof: &MerkleProof) -> bool {
    check_merkle_proof(self.merkle_root(), key, value, proof)
  }

  fn get_hash(&self, index: &TreeNodeIndex) -> &Hash256 {
    self.hashes.get(index).unwrap_or(&ZERO_DIGEST) // (&(*DEFAULT_HASHES)[256 - index.depth])
  }

  fn update_hash(&mut self, index: &TreeNodeIndex, hash: &Hash256) {
    if ZERO_DIGEST /* (*DEFAULT_HASHES)[256 - index.depth] */ == *hash {
      self.hashes.remove(index);
    } else {
      self.hashes.insert(index.clone(), *hash);
    }
  }
}

/// Check the Merkle proof of a key-value pair in a Merkle root.
/// whether the proof is valid.
pub fn check_merkle_proof<Value: AsRef<[u8]>>(merkle_root: &Hash256,
                                              key: impl AsRef<[u8]>,
                                              value: Option<&Value>,
                                              proof: &MerkleProof)
                                              -> bool {
  let key = hash_256(key.as_ref());
  let mut hash = value.map_or(ZERO_DIGEST, hash_256);
  let mut iter = proof.hashes.iter();
  for i in 0..256 {
    let sibling_hash = if !get_bit(&proof.bitmap, i) {
      &ZERO_DIGEST // &(*DEFAULT_HASHES)[i]
    } else if let Some(h) = iter.next() {
      h
    } else {
      return false;
    };

    let depth = 256 - i;
    hash = if get_bit(&key, depth - 1) {
      // sibling is at left
      hash_pair(sibling_hash, &hash)
    } else {
      // sibling is at right
      hash_pair(&hash, sibling_hash)
    };
  }

  iter.next() == None && hash == *merkle_root
}

pub fn open(path: &str) -> Result<SmtMap256<String>, Error> {
  let mut file = File::open(path)?;
  let mut contents = String::new();
  file.read_to_string(&mut contents)?;

  // Deserialize and print Rust data structure.
  let result: SmtMap256<String> = serde_json::from_str(&contents)?;
  Ok(result)
}

#[cfg(test)]
mod tests {
  use super::*;
  use hex::{encode, FromHex};
  use quickcheck::{quickcheck, TestResult};
  use std::string::ToString;

  // `hex` is the first a few bytes of the desired 32 bytes (the rest bytes are zeros).
  pub fn l256(hex: &str) -> [u8; 32] {
    assert!(hex.len() % 2 == 0 && hex.len() <= 64);
    let hex = hex.to_string() + &"0".repeat(64 - hex.len());
    <[u8; 32]>::from_hex(&hex).unwrap()
  }

  // `hex` is the last a few bytes of the desired 32 bytes (the rest bytes are zeros).
  pub fn r256(hex: &str) -> [u8; 32] {
    assert!(hex.len() % 2 == 0 && hex.len() <= 64);
    let hex = "0".repeat(64 - hex.len()) + hex;
    <[u8; 32]>::from_hex(&hex).unwrap()
  }

  // `hex` must be a 64-byte long hex string.
  pub fn b256(hex: &str) -> Hash256 {
    <[u8; 32]>::from_hex(hex).unwrap()
  }

  pub fn max256() -> [u8; 32] {
    b256("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
  }

  fn hex_from_digit(num: u8) -> char {
    if num < 10 {
      (b'0' + num) as char
    } else {
      (b'A' + num - 10) as char
    }
  }

  pub fn to_hex(blob: &[u8]) -> String {
    let mut buf: String = String::new();
    for ch in blob {
      buf.push(hex_from_digit(ch / 16));
      buf.push(hex_from_digit(ch % 16));
    }
    buf
  }

  fn mask_u8(x: u64, shift: usize) -> u8 {
    (x >> shift) as u8 & 0xffu8
  }

  fn u64_into_byteslice(slice: &mut [u8], u: u64) {
    for n in 0..8 {
      slice[n] = mask_u8(u, n * 8);
    }
  }

  fn make_u256(x0: u64, x1: u64, x2: u64, x3: u64) -> [u8; 32] {
    let args: [u64; 4] = [x0, x1, x2, x3];
    let mut result: [u8; 32] = [0u8; 32];

    for i in 0..4 {
      let l = i * 8;
      let r = l + 8;
      u64_into_byteslice(&mut result[l..r], args[i]);
    }

    return result;
  }

  #[test]
  fn test_bit_manipulation() {
    let mut u = [0_u8; 32];
    set_bit(&mut u, 0);
    assert_eq!(encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000000");
    set_bit(&mut u, 255);
    assert_eq!(encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000080");
    for i in 0..256 {
      set_bit(&mut u, i);
    }
    assert_eq!(encode(&u),
               "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");

    clear_bit(&mut u, 0);
    assert_eq!(encode(&u),
               "feffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
    clear_bit(&mut u, 255);
    assert_eq!(encode(&u),
               "feffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f");
    clear_bit(&mut u, 126);
    assert_eq!(encode(&u),
               "feffffffffffffffffffffffffffffbfffffffffffffffffffffffffffffff7f");
    for i in 0..256 {
      clear_bit(&mut u, i);
    }
    assert_eq!(encode(&u),
               "0000000000000000000000000000000000000000000000000000000000000000");

    flip_bit(&mut u, 0);
    assert_eq!(encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000000");
    flip_bit(&mut u, 255);
    assert_eq!(encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000080");
    flip_bit(&mut u, 255);
    assert_eq!(encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000000");
    for i in 0..256 {
      flip_bit(&mut u, i);
    }
    assert_eq!(encode(&u),
               "feffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
  }

  #[test]
  fn test_tree_node_index() {
    let mut index = TreeNodeIndex::leaf(r256("1234567890abcdef1234567890abcdef"));
    assert!(!index.is_left());
    for _ in 0..3 {
      index.move_up();
    }
    assert!(index.is_left());
    assert_eq!(index,
               TreeNodeIndex { bit_path: r256("1234567890abcdef1234567890abcd0f"),
                               depth: 256 - 3 });
    assert_eq!(index.sibling().unwrap(),
               TreeNodeIndex { bit_path: r256("1234567890abcdef1234567890abcd1f"),
                               depth: 256 - 3 });

    // Climb up from the left-most leaf.
    let mut index = TreeNodeIndex::leaf([0; 32]);
    for depth in (1..=256).rev() {
      assert_eq!(index.depth, depth);
      assert!(index.is_left());
      let mut sibling = index.sibling().unwrap();
      assert_eq!(sibling.depth, depth);
      assert!(!sibling.is_left());

      index.move_up();
      sibling.move_up();
      assert_eq!(index, sibling);
    }
    assert!(index.is_root());
    assert_eq!(index.bit_path, [0; 32]);
    assert_eq!(index.sibling(), None);

    // Climb up from the right-most leaf.
    let mut index = TreeNodeIndex::leaf(max256());
    for depth in (1..=256).rev() {
      assert_eq!(index.depth, depth);
      assert!(!index.is_left());
      let mut sibling = index.sibling().unwrap();
      assert_eq!(sibling.depth, depth);
      assert!(sibling.is_left());

      index.move_up();
      sibling.move_up();
      assert_eq!(index, sibling);
    }
    assert!(index.is_root());
    assert_eq!(index.bit_path, [0; 32]);
    assert_eq!(index.sibling(), None);
  }

  #[test]
  fn test_smt_map_256_kv() {
    let mut smt = SmtMap256::new();
    let merkle_root: Hash256 = *(&smt).merkle_root();

    assert_eq!(smt.get(&[0; 32]), None);

    let key = String::from("sJ4qHqSIcHbc725D_G_CQOPU3qAiZ8zDdAlA5192-TSqIAqcdcC4Hlq7A_phAM1B"); // b256("1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef");
    assert_eq!(smt.get(&key), None); // [0; 32]);

    let value1 = Some("ffeebbaa99887766554433221100");
    let value2 = Some("ffeebbaa99887766554433221199");
    let value3 = Some("cafebabecafebabecafebabecafebabecafebabecafebabecafebabecafebabe");

    assert_eq!(smt.set(&key, value1), None); // It starts out empty
    assert!(merkle_root != *(&smt).merkle_root());
    assert_eq!(smt.get(&key), value1.as_ref());
    assert_eq!(smt.set(&key, value2), value1);
    assert_eq!(smt.get(&key), value2.as_ref());
    assert_eq!(smt.set(&key, value3), value2);
    assert_eq!(smt.get(&key), value3.as_ref());
    assert_eq!(smt.set(&key, None), value3);
    assert_eq!(merkle_root, *(&smt).merkle_root());
    println!("retrieved value {} associated with key {}",
             &value3.unwrap(),
             &key);

    fn prop(key: String, value: String) -> TestResult {
      let mut smt = SmtMap256::new();
      // let (key, value) = (make_u256(x0, x1, x2, x3), Some(s.clone()));
      let prev = smt.set(&key, Some(value.clone()));
      match prev {
        Some(_) => TestResult::failed(),
        None => {
          if let Some(v) = smt.get(&key) {
            if v == &value {
              TestResult::passed()
            } else {
              TestResult::failed()
            }
          } else {
            TestResult::failed()
          }
        }
      }
    }
    quickcheck(prop as fn(String, String) -> TestResult);
  }

/*
  #[test]
  fn test_smt_map_256_merkle_proof() {
    let expected_default_root_hash =
      b256("0000000000000000000000000000000000000000000000000000000000000000");

    let mut smt = SmtMap256::new();

    // Verify proof of `key` when the values of all keys are default.
    let key = r256("C0");
    let (value, proof) = smt.get_with_proof(&key);
    println!("test_smt_map_256_merkle_proof: key={:?}, value={:?}, proof={:?}",
             &key, &value, &proof);
    assert_eq!(value, None); // [0; 32]);
    assert_eq!(proof,
               MerkleProof { bitmap: [0; 32],
                             hashes: Vec::new() });
    assert!(smt.check_merkle_proof(&key, value, &proof));
    assert!(check_merkle_proof(smt.merkle_root(), &key, value, &proof));

    // Verify the merkle proof of `key` when key 0x00 has a non-default value.
    smt.set(&[0; 32], Some(r256("AA")));
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, None); // [0; 32]);
    assert_eq!(
               proof,
               MerkleProof { bitmap: l256("0000000000000000000000000000000000000000000000000000000000000080"),
                             hashes: vec![b256("64f2216d3622e923f0ae2895c6346e135cdfedffbe9fae06eca0494be56f4127")],
                           },
    );
    assert_eq!(*smt.merkle_root(),
               b256("56c54f352367fabf41da238c4b8b498a9ed6ff0922ab6c2a3ac3570f245baf9d"));
    assert!(smt.check_merkle_proof(&key, value, &proof));
    assert!(check_merkle_proof(smt.merkle_root(), &key, value, &proof));

    // Verify the merkle proof of `key` again after setting a value at the max key (0xFF..FF).
    smt.set(&max256(), Some(r256("1234")));
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, None); // [0; 32]);
    assert_eq!(
      proof,
      MerkleProof {
        bitmap: b256("0200000000000000000000000000000000000000000000000000000000000080"),
        hashes: vec![
          b256("5031918db6776a678116ebe3352a3283f28983dbec4df0783c4988a7be461922"),
          b256("09e975535684248aafbf0d00824aadd496879c9e375e298fdd33e7adc09c5067"),
        ],
      },
    );
    assert_eq!(*smt.merkle_root(),
               b256("240a695aed4152b8f5b77aa9cb0e2b93b844ee0bc184fe898d28a29c348d57f6"));
    assert!(smt.check_merkle_proof(&key, value, &proof));
    assert!(check_merkle_proof(smt.merkle_root(), &key, value, &proof));

    // Verify the merkle proof of `key` again after setting a value at `key` itself.
    let value2 = Some(r256("0100000000000000000000000000000000"));
    smt.set(&key, value2);
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, value2.as_ref());
    assert_eq!(
      proof,
      MerkleProof {
        bitmap: b256("0200000000000000000000000000000000000000000000000000000000000080"),
        hashes: vec![
          b256("5031918db6776a678116ebe3352a3283f28983dbec4df0783c4988a7be461922"),
          b256("09e975535684248aafbf0d00824aadd496879c9e375e298fdd33e7adc09c5067"),
        ],
      },
    );
    assert_eq!(*smt.merkle_root(),
               b256("ada8f75819448c00025257d17bfd78c821487e60f9b31340bcf393e9c6acefa2"));

    // Reset the value of key 0x00..00 to the default, and verify the merkle proof of `key`.
    smt.set(&[0; 32], None); // [0; 32]);
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, value2.as_ref());
    assert_eq!(
      proof,
      MerkleProof {
        bitmap: b256("0000000000000000000000000000000000000000000000000000000000000080"),
        hashes: vec![b256(
          "09e975535684248aafbf0d00824aadd496879c9e375e298fdd33e7adc09c5067"
        ),],
      },
    );
    assert_eq!(*smt.merkle_root(),
               b256("46b50abc16c1f97e918186a18397082e02adb1f38dd79c765da71f37501f9277"));

    // Reset the value of the max key to the default, and verify the merkle proof of `key`.
    smt.set(&max256(), None); // [0; 32]);
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, value2.as_ref());
    assert_eq!(proof,
               MerkleProof { bitmap: [0; 32],
                             hashes: vec![] },);
    assert_eq!(*smt.merkle_root(),
               b256("41cf2d63e95cc4f4e430ffebb1c4c7d84adf81c6196348d6ea1f8afb9871599b"));

    // Reset the value of `key`, and verify that the merkle tree has been reset to the init state.
    smt.set(&key, None); // [0; 32]);
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, None); // [0; 32]);
    assert_eq!(proof,
               MerkleProof { bitmap: [0; 32],
                             hashes: vec![] },);
    assert_eq!(smt.merkle_root(), &expected_default_root_hash);
  }

  #[test]
  fn test_smt_map_256_merkle_proof_negative_cases() {
    let mut smt = SmtMap256::new();
    let (key, value) = (r256("C0"), Some(r256("0100000000000000000000000000000000")));
    smt.set(&key, value);
    smt.set(&[0; 32], Some(r256("AA")));
    smt.set(&max256(), Some(r256("1234")));

    let (v, p) = smt.get_with_proof(&key);
    // The correct merkle proof:
    assert!(smt.check_merkle_proof(&key, v, &p));
    // Negative cases of merkle proof verification:
    assert!(!smt.check_merkle_proof(
      &key,
      value.as_ref(),
      &MerkleProof {
        bitmap: b256("0200000000000000000000000000000000000000000000000000000000000080"),
        hashes: vec![
          b256("d6f751104ddfead9549c96fabdbd4d2fc6876c8cd9a49ea4a821de938f71a011"),
          b256("5a7ef746ad33334b4fbd7406a1a4ffa5c5f959199448d5ae6ed39b4a9d6ebe5a"),
          [0; 32], // extra hash
        ],
      }
    ));
    assert!(!smt.check_merkle_proof(
      &key,
      value.as_ref(),
      &MerkleProof {
        bitmap: b256("0200000000000000000000000000000000000000000000000000000000000080"),
        hashes: vec![
          b256("d6f751104ddfead9549c96fabdbd4d2fc6876c8cd9a49ea4a821de938f71a011"),
          // missing hash
        ],
      }
    ));
    assert!(!smt.check_merkle_proof(
      &key,
      value.as_ref(),
      &MerkleProof {
        // wrong bitmap - missing bit
        bitmap: b256("0200000000000000000000000000000000000000000000000000000000000000"),
        hashes: vec![
          b256("d6f751104ddfead9549c96fabdbd4d2fc6876c8cd9a49ea4a821de938f71a011"),
          b256("5a7ef746ad33334b4fbd7406a1a4ffa5c5f959199448d5ae6ed39b4a9d6ebe5a"),
        ],
      }
    ));
    assert!(!smt.check_merkle_proof(
      &key,
      value.as_ref(),
      &MerkleProof {
        // wrong bitmap - extra bit
        bitmap: b256("0200010000000000000000000000000000000000000000000000000000000080"),
        hashes: vec![
          b256("d6f751104ddfead9549c96fabdbd4d2fc6876c8cd9a49ea4a821de938f71a011"),
          b256("5a7ef746ad33334b4fbd7406a1a4ffa5c5f959199448d5ae6ed39b4a9d6ebe5a"),
        ],
      }
    ));
    assert!(!smt.check_merkle_proof(
      &key,
      value.as_ref(),
      &MerkleProof {
        // wrong bitmap - wrong bit
        bitmap: b256("0400000000000000000000000000000000000000000000000000000000000080"),
        hashes: vec![
          b256("d6f751104ddfead9549c96fabdbd4d2fc6876c8cd9a49ea4a821de938f71a011"),
          b256("5a7ef746ad33334b4fbd7406a1a4ffa5c5f959199448d5ae6ed39b4a9d6ebe5a"),
        ],
      }
    ));
  }
  */
}
