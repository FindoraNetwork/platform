//!
//! # Used to resist playback attack
//!
//! This module implements a sliding window mechanism. The module user can keep recent received
//! operations for replay attack prevention
//!

#![deny(warnings)]
#![deny(missing_docs)]

use {
    ruc::*,
    serde::{Deserialize, Serialize},
};

/// Define a sliding window
#[derive(Clone, Default, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub struct SlidingSet<T> {
    current: usize,
    width: usize,
    map: Vec<Vec<T>>,
}

impl<T> SlidingSet<T> {
    /// Create a new sliding window start with 0
    #[inline(always)]
    pub fn new(width: usize) -> Self {
        let mut map = Vec::with_capacity(width);
        for _ in 0..width {
            map.push(Vec::new());
        }
        let current = 0;
        SlidingSet {
            current,
            width,
            map,
        }
    }

    /// Advance current window index by 1
    /// and clear the sliding window
    #[inline(always)]
    pub fn incr_current(&mut self) {
        self.current += 1;
        let current_index = self.current % self.width;
        self.map[current_index].clear();
    }
}

impl<T: Eq + Copy + std::fmt::Debug> SlidingSet<T> {
    /// Check if a key with user-defined-type exists at specified index in current window
    #[inline(always)]
    pub fn has_key_at(&self, index: usize, key: T) -> bool {
        if index > self.current || index + self.width <= self.current {
            false
        } else {
            self.map[index % self.width].contains(&key)
        }
    }

    /// Insert a key to current window.
    /// Insertion should be failed if key has already existed at specified index.
    #[inline(always)]
    pub fn insert(&mut self, key: T, index: usize) -> Result<()> {
        if index <= self.current && index + self.width >= (self.current + 1) {
            if self.map[index % self.width].contains(&key) {
                Err(eg!(format!(
                    "SlidingSet::insert: ({:?}, {}) already in set",
                    key, index
                )))
            } else {
                self.map[index % self.width].push(key);
                Ok(())
            }
        } else {
            Err(eg!(format!("({:?}, {}) is out of range", key, index)))
        }
    }
}

#[cfg(test)]
#[allow(missing_docs)]
mod tests {
    use {
        super::*,
        cryptohash::sha256::{hash, Digest},
        rand::distributions::Alphanumeric,
        rand::{thread_rng, Rng},
    };

    #[test]
    fn test_basic() {
        let factor = 3;
        let width: usize = 4;
        let mut ss = SlidingSet::<Digest>::new(width);
        let mut names = Vec::new();
        for _ in 0..width * factor {
            let rand_string: String = thread_rng()
                .sample_iter(&Alphanumeric)
                .take(16)
                .map(|c| c.to_string())
                .collect();
            names.push(rand_string);
        }
        let digests: Vec<Digest> = names
            .iter()
            .map(|s| hash(&bincode::serialize(&s).unwrap()))
            .collect();

        // test the "sliding property".
        let mut n = 0;
        for i in 0..factor {
            for _ in 0..width {
                ss.insert(digests[n], n).unwrap();
                assert!(ss.has_key_at(n, digests[n]));
                ss.incr_current();
                n += 1;
            }
            assert!(!ss.has_key_at(i * width, digests[i * width]));
            assert!(ss.has_key_at((i + 1) * width - 1, digests[(i + 1) * width - 1]));
        }
    }
}
