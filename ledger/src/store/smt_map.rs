use std::collections::HashMap;

use hex::FromHex;
use std::string::ToString;

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

pub type Key = [u8; 32];
// pub type Value = [u8; 32];
pub type Hash256 = [u8; 32];

lazy_static::lazy_static! {
  static ref DEFAULT_HASHES: [Hash256; 257] = {
    // The element at index `i` is the hash of a subtree with `2^i` default nodes.
    let mut hashes: [Hash256; 257] = [[0; 32]; 257];
    for i in 1..=256 {
      hashes[i] = merge_hashes(&hashes[i-1], &hashes[i-1]);
    }
    hashes
  };
}

/// Index of a node in a Sparse Merkle Tree.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct TreeNodeIndex {
  // The path starts from the first bit (the LSB of the first byte), and ends at
  // the bit indexed by `depth`. Bit 0 means left, and bit 1 means right. Bits beyond
  // the `depth`-th bit are irrelevant, and are always zeros.
  bit_path: [u8; 32],

  // The root has depth of 0, and the leaves have depth of 256.
  depth: usize,
}

impl TreeNodeIndex {
  /// Get a new TreeNodeIndex of the leaf corresponding to the given key.
  fn leaf(key: Key) -> Self {
    Self {
      bit_path: key,
      depth: 256,
    }
  }

  /// Index of the root.
  fn root() -> Self {
    Self {
      bit_path: [0; 32],
      depth: 0,
    }
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
#[derive(Clone, Default)]
pub struct SmtMap256<Value: AsRef<[u8]>> {
  kvs: HashMap<Key, Value>,

  // Hash values of both leaf and inner nodes.
  hashes: HashMap<TreeNodeIndex, Hash256>,
}

impl<Value: AsRef<[u8]>> SmtMap256<Value> {
  /// Returns a new SMT-Map where all keys have the default value (zero).
  pub fn new() -> Self {
    Self {
      kvs: HashMap::new(),
      hashes: HashMap::new(),
    }
  }

  /// Sets the value of a key. Returns the previous value corresponding to the key.
  pub fn set(&mut self, key: &Key, value: Option<Value>) -> Option<Value> {
    // Update the hash of the leaf.
    let mut index = TreeNodeIndex::leaf(*key);
    let mut hash: Hash256 = value.as_ref().map(|v| hash_256(v)).unwrap_or([0; 32]);
    self.update_hash(&index, &hash);

    // Update the hashes of the inner nodes along the path.
    while !index.is_root() {
      let sibling_hash = self.get_hash(&index.sibling().unwrap());

      hash = if index.is_left() {
        merge_hashes(&hash, &sibling_hash)
      } else {
        merge_hashes(&sibling_hash, &hash)
      };
      index.move_up();
      self.update_hash(&index, &hash);
    }

    if let Some(v) = value {
      self.kvs.insert(*key, v)
    } else {
      self.kvs.remove(key) // .unwrap_or([0; 32])
    }
  }

  /// Returns a reference to the value of a key.
  pub fn get(&self, key: &Key) -> Option<&Value> {
    self.kvs.get(key) // .unwrap_or(&[0; 32])
  }

  /// Returns a reference to the value of the key with merkle proof.
  pub fn get_with_proof(&self, key: &Key) -> (Option<&Value>, MerkleProof) {
    let mut bitmap = [0_u8; 32];
    let mut sibling_hashes = Vec::new();
    let mut index = TreeNodeIndex::leaf(*key);
    for i in 0..256 {
      if let Some(sibling_hash) = self.hashes.get(&index.sibling().unwrap()) {
        set_bit(&mut bitmap, i);
        sibling_hashes.push(*sibling_hash);
      }
      index.move_up();
    }
    (
      self.get(key),
      MerkleProof {
        bitmap,
        hashes: sibling_hashes,
      },
    )
  }

  /// Returns the Merkle root
  pub fn merkle_root(&self) -> &Hash256 {
    self.get_hash(&TreeNodeIndex::root())
  }

  /// Returns `true` when proof is valid, `false` otherwise.
  pub fn check_merkle_proof(&self,
                            key: &Key,
                            value: Option<&Value>, proof: &MerkleProof)
                            -> bool {
    check_merkle_proof(self.merkle_root(), key, value, proof)
  }

  fn get_hash(&self, index: &TreeNodeIndex) -> &Hash256 {
    self.hashes
      .get(index)
      .unwrap_or(&(*DEFAULT_HASHES)[256 - index.depth])
  }

  fn update_hash(&mut self, index: &TreeNodeIndex, hash: &Hash256) {
    if (*DEFAULT_HASHES)[256 - index.depth] == *hash {
      self.hashes.remove(index);
    } else {
      self.hashes.insert(index.clone(), *hash);
    }
  }
}

/// Check the Merkle proof of a key-value pair in a Merkle root.
/// whether the proof is valid.
pub fn check_merkle_proof<Value: AsRef<[u8]>>(
  merkle_root: &Hash256,
  key: &Key,
  value: Option<&Value>,
  proof: &MerkleProof)
  -> bool {
  let mut hash =
    if let Some(v) = value {
      hash_256(v)
    } else {
      [0; 32]
    };
  let mut iter = proof.hashes.iter();
  for i in 0..256 {
    let sibling_hash = if !get_bit(&proof.bitmap, i) {
      &(*DEFAULT_HASHES)[i]
    } else {
      if let Some(h) = iter.next() {
        h
      } else {
        return false;
      }
    };

    let depth = 256 - i;
    hash = if get_bit(key, depth - 1) {
      // sibling is at left
      merge_hashes(sibling_hash, &hash)
    } else {
      // sibling is at right
      merge_hashes(&hash, sibling_hash)
    };
  }

  iter.next() == None && hash == *merkle_root
}

fn hash_256(value: impl AsRef<[u8]>) -> Hash256 {
  use tiny_keccak::Keccak;
  let mut hasher = Keccak::new_keccak256();
  hasher.update(value.as_ref());
  let mut result: Hash256 = [0; 32];
  hasher.finalize(&mut result);
  result
}

fn merge_hashes(left: &Hash256, right: &Hash256) -> Hash256 {
  use tiny_keccak::Keccak;
  let mut hasher = Keccak::new_keccak256();
  hasher.update(&*left);
  hasher.update(&*right);
  let mut result: Hash256 = [0; 32];
  hasher.finalize(&mut result);
  result
}


#[cfg(test)]
mod tests {
  use super::*;
  use hex::encode as hex_encode;
  use quickcheck::{quickcheck, TestResult};

  fn mask_u8(x: u64, shift:usize) -> u8 {
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
    assert_eq!(hex_encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000000");
    set_bit(&mut u, 255);
    assert_eq!(hex_encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000080");
    for i in 0..256 {
      set_bit(&mut u, i);
    }
    assert_eq!(hex_encode(&u),
               "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");

    clear_bit(&mut u, 0);
    assert_eq!(hex_encode(&u),
               "feffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
    clear_bit(&mut u, 255);
    assert_eq!(hex_encode(&u),
               "feffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f");
    clear_bit(&mut u, 126);
    assert_eq!(hex_encode(&u),
               "feffffffffffffffffffffffffffffbfffffffffffffffffffffffffffffff7f");
    for i in 0..256 {
      clear_bit(&mut u, i);
    }
    assert_eq!(hex_encode(&u),
               "0000000000000000000000000000000000000000000000000000000000000000");

    flip_bit(&mut u, 0);
    assert_eq!(hex_encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000000");
    flip_bit(&mut u, 255);
    assert_eq!(hex_encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000080");
    flip_bit(&mut u, 255);
    assert_eq!(hex_encode(&u),
               "0100000000000000000000000000000000000000000000000000000000000000");
    for i in 0..256 {
      flip_bit(&mut u, i);
    }
    assert_eq!(hex_encode(&u),
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
    assert_eq!(
      index,
      TreeNodeIndex {
        bit_path: r256("1234567890abcdef1234567890abcd0f"),
        depth: 256 - 3,
      }
    );
    assert_eq!(
      index.sibling().unwrap(),
      TreeNodeIndex {
        bit_path: r256("1234567890abcdef1234567890abcd1f"),
        depth: 256 - 3,
      }
    );

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

    let key = b256("1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef");
    assert_eq!(smt.get(&key), None); // [0; 32]);

    let value1 = Some("ffeebbaa99887766554433221100");
    let value2 = Some("ffeebbaa99887766554433221199");
    let value3 = Some("cafebabecafebabecafebabecafebabecafebabecafebabecafebabecafebabe");

    assert_eq!(smt.set(&key, value1), None);
    assert!(merkle_root !=  *(&smt).merkle_root());
    assert_eq!(smt.get(&key), value1.as_ref());
    assert_eq!(smt.set(&key, value2), value1);
    assert_eq!(smt.get(&key), value2.as_ref());
    assert_eq!(smt.set(&key, value3), value2);
    assert_eq!(smt.get(&key), value3.as_ref());
    assert_eq!(smt.set(&key, None), value3);
    assert_eq!(merkle_root, *(&smt).merkle_root());
    println!("retrieved value {:?} associated with key {:?}", &value3.unwrap(), to_hex(&key));

    fn prop(x0: u64, x1: u64, x2: u64, x3: u64, s: String) -> TestResult {
      let mut smt = SmtMap256::new();
      let (key, value) = (make_u256(x0, x1, x2, x3), Some(s.clone()));
      let prev = smt.set(&key, value);
      match prev {
        Some(_) => TestResult::failed(),
        None => 
          if let Some(v) = smt.get(&key) {
            if v == &s {
              TestResult::passed()
            } else {
              TestResult::failed()
            }
          } else {
            TestResult::failed()
          }
      }
    }
    quickcheck(prop as fn(u64, u64, u64, u64, String) -> TestResult);
  }

  #[test]
  fn test_smt_map_256_merkle_proof() {
    assert_eq!((*DEFAULT_HASHES)[0], [0; 32]);

    let expected_default_root_hash =
      b256("a7ff9e28ffd3def443d324547688c2c4eb98edf7da757d6bfa22bff55b9ce24a");
    assert_eq!((*DEFAULT_HASHES)[256], expected_default_root_hash);

    let mut smt = SmtMap256::new();

    // Verify proof of `key` when the values of all keys are default.
    let key = r256("C0");
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, None); // [0; 32]);
    assert_eq!(
      proof,
      MerkleProof {
        bitmap: [0; 32],
        hashes: Vec::new(),
      }
    );
    assert!(smt.check_merkle_proof(&key, value, &proof));
    assert!(check_merkle_proof(smt.merkle_root(), &key, value, &proof));

    // Verify the merkle proof of `key` when key 0x00 has a non-default value.
    smt.set(&[0; 32], Some(r256("AA")));
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, None); // [0; 32]);
    assert_eq!(
      proof,
      MerkleProof {
        bitmap: l256("02"),
        hashes: vec![b256(
          "6be4392c7e7f5a762783f200425311e8f07cfa86b2bd786c8ef7a840c2dc5b56"
        )],
      },
    );
    assert_eq!(
      *smt.merkle_root(),
      b256("b1666beba7016c5e3d046246d8081ca0a9c26a2bd7a1aeb8b8ac5c741f696200")
    );
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
          b256("6be4392c7e7f5a762783f200425311e8f07cfa86b2bd786c8ef7a840c2dc5b56"),
          b256("0e7c0958e8b322a5612f450c03ff78fe9935826bb8c3907799e36c858c1c8179"),
        ],
      },
    );
    assert_eq!(
      *smt.merkle_root(),
      b256("c9749ac4eff2305f7bfec92499ddbcca10209267d41bd786e3a1d6bf908b73fe")
    );
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
          b256("6be4392c7e7f5a762783f200425311e8f07cfa86b2bd786c8ef7a840c2dc5b56"),
          b256("0e7c0958e8b322a5612f450c03ff78fe9935826bb8c3907799e36c858c1c8179"),
        ],
      },
    );
    assert_eq!(
      *smt.merkle_root(),
      b256("3603fac845821960d862459a5074ffd1ec5ec4fb0be98982b1922c36b5f48899")
    );

    // Reset the value of key 0x00..00 to the default, and verify the merkle proof of `key`.
    smt.set(&[0; 32], None); // [0; 32]);
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, value2.as_ref());
    assert_eq!(
      proof,
      MerkleProof {
        bitmap: b256("0000000000000000000000000000000000000000000000000000000000000080"),
        hashes: vec![b256(
          "0e7c0958e8b322a5612f450c03ff78fe9935826bb8c3907799e36c858c1c8179"
        ),],
      },
    );
    assert_eq!(
      *smt.merkle_root(),
      b256("4d15e9738affd99791768a9ab876abca8a8418387dccdd42d25b5ae9cadda833")
    );

    // Reset the value of the max key to the default, and verify the merkle proof of `key`.
    smt.set(&max256(), None); // [0; 32]);
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, value2.as_ref());
    assert_eq!(
      proof,
      MerkleProof {
        bitmap: [0; 32],
        hashes: vec![]
      },
    );
    assert_eq!(
      *smt.merkle_root(),
      b256("5385035f1a791313bed3b8023f19cab1b28a30e671c4e6e26bcb566dd8a5b842")
    );

    // Reset the value of `key`, and verify that the merkle tree has been reset to the init state.
    smt.set(&key, None); // [0; 32]);
    let (value, proof) = smt.get_with_proof(&key);
    assert_eq!(value, None); // [0; 32]);
    assert_eq!(
      proof,
      MerkleProof {
        bitmap: [0; 32],
        hashes: vec![]
      },
    );
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
}
