//!
//! # A Cryptohash Implementation
//!
//!

#![deny(warnings)]
#![deny(missing_docs)]

use serde::{Deserialize, Serialize};

/// HashValue size in byte
pub const HASH_SIZE: usize = 32;

#[allow(missing_docs)]
// #[cfg(target_arch = "wasm32")]
pub mod sha256 {
    use {
        arrayref::array_ref,
        serde::{Deserialize, Serialize},
        sha2::Digest as DigestTrait,
        sha2::Sha256,
        std::ops::{Index, Range, RangeFrom},
    };

    pub const DIGESTBYTES: usize = 32;

    #[derive(
        Clone,
        Copy,
        Debug,
        Default,
        Deserialize,
        Eq,
        Hash,
        Ord,
        PartialEq,
        PartialOrd,
        Serialize,
    )]
    pub struct Digest(pub [u8; DIGESTBYTES]);

    #[inline(always)]
    pub fn hash(m: &[u8]) -> Digest {
        let temp = Sha256::digest(m);
        Digest(*array_ref!(&temp, 0, 32))
    }

    impl AsRef<[u8]> for Digest {
        #[inline(always)]
        fn as_ref(&self) -> &[u8] {
            &self.0
        }
    }

    impl Index<Range<usize>> for Digest {
        type Output = [u8];

        #[inline(always)]
        fn index(&self, _index: std::ops::Range<usize>) -> &[u8] {
            self.0.index(_index)
        }
    }

    impl Index<RangeFrom<usize>> for Digest {
        type Output = [u8];

        #[inline(always)]
        fn index(&self, _index: RangeFrom<usize>) -> &[u8] {
            self.0.index(_index)
        }
    }

    impl Digest {
        #[inline(always)]
        pub fn from_slice(slice: &[u8]) -> Option<Self> {
            let mut buf = [0_u8; DIGESTBYTES];
            buf[0..DIGESTBYTES].clone_from_slice(slice);
            Some(Digest(buf))
        }
    }
}

// #[allow(missing_docs)]
// #[cfg(not(target_arch = "wasm32"))]
// pub mod sha256 {
//     use sodiumoxide::crypto::hash::sha256;
//     pub const DIGESTBYTES: usize = sha256::DIGESTBYTES;
//     pub use sha256::Digest;

//     #[inline(always)]
//     #[allow(missing_docs)]
//     pub fn hash(m: &[u8]) -> Digest {
//         sha256::hash(m)
//     }
// }

///
/// Define the structure containing a hash value for the tree.
///
/// This structure must be HASH_SIZE bytes in size. Each entry of this
/// type corresponds to a node in the Merkle tree.
///

#[repr(C)]
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct HashValue {
    pub hash: [u8; HASH_SIZE],
}

impl HashValue {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new() -> HashValue {
        HashValue {
            hash: [0; HASH_SIZE],
        }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn desc(&self) -> String {
        format!(
            "[ {:3}, {:3}, {:3}, {:3} ]",
            self.hash[0], self.hash[1], self.hash[2], self.hash[3]
        )
    }
}

impl From<sha256::Digest> for HashValue {
    #[inline(always)]
    fn from(d: sha256::Digest) -> Self {
        Self { hash: d.0 }
    }
}

///
/// This structure describes what is passed to the upper layers
/// for a proof of inclusion in the tree.
///
/// # Fields:
///
/// * version   a version for this proof structure
/// * ledger    the id of the tree as given at open or create
/// * state     a version id for the tree's state when the proof was created
/// * time      the time at which the proof was generated, in POSIX time
/// * tx_id     the transaction id to which this proof applies
/// * hashes    the set of hashes up the tree
///

#[allow(missing_docs)]
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Proof {
    pub version: u64,
    pub ledger: String,
    pub state: u64,
    pub time: i64,
    pub tx_id: u64,
    pub root_hash: HashValue,
    pub hash_array: Vec<HashValue>,
}

impl Proof {
    /// Check if this leaf is a valid proof
    pub fn is_valid_proof(&self, leaf: HashValue) -> bool {
        let mut result = leaf;
        let mut id = self.tx_id;
        for i in 0..self.hash_array.len() {
            if id & 1 == 0 {
                result = hash_partial(&result, &self.hash_array[i]);
            } else {
                result = hash_partial(&self.hash_array[i], &result);
            }

            id /= 2;
        }

        result == self.root_hash
    }
}

/// Compute the hash of two hashes. This Merkle tree is a binary
/// representation, so this is a common operation.
pub fn hash_pair(left: &HashValue, right: &HashValue) -> HashValue {
    let mut data = [0_u8; 2 * HASH_SIZE];

    data[0..HASH_SIZE].clone_from_slice(&left.hash[0..HASH_SIZE]);
    data[HASH_SIZE..2 * HASH_SIZE].clone_from_slice(&right.hash[0..HASH_SIZE]);

    let digest = sha256::hash(&data);
    let mut result = HashValue::new();
    result.hash.clone_from_slice(&digest[0..HASH_SIZE]);
    result
}

/// Compute the hash of a single hash value. This function is used
/// when generating proofs. Partially-filled nodes are constructed
/// using hashes of hashes.
#[inline(always)]
pub fn hash_single(hash: &HashValue) -> HashValue {
    let digest = sha256::hash(&hash.hash[0..HASH_SIZE]);

    let mut result = HashValue::new();

    result.hash.clone_from_slice(&digest[0..HASH_SIZE]);
    result
}

/// Compute a hash value for a node in a partially-filled block. The
/// right-hand side might not exist, in which case the value is just
/// the hash of the left side.
#[inline(always)]
pub fn hash_partial(left: &HashValue, right: &HashValue) -> HashValue {
    let empty_hash = HashValue::new();
    let left_present = *left != empty_hash;
    let right_present = *right != empty_hash;

    if left_present && right_present {
        hash_pair(left, right)
    } else if left_present {
        hash_single(left)
    } else {
        empty_hash
    }
}
