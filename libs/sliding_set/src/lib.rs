#![feature(btree_drain_filter)]
use cryptohash::sha256::Digest;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Clone, Default, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub struct SlidingSet {
  map: HashMap<Digest, u64>,
  min: u64,
  current: u64,
  width: u64,
}

impl SlidingSet {
  pub fn new(min: u64, current: u64, width: u64) -> Self {
    assert!(min <= current);
    SlidingSet { min,
                 current,
                 width,
                 map: HashMap::new() }
  }
}

impl SlidingSet {
  pub fn contains_key(&self, key: Digest) -> bool {
    self.map.contains_key(&key)
  }

  pub fn get(&self, key: Digest) -> Option<&u64> {
    self.map.get(&key)
  }

  pub fn insert(&mut self, key: Digest, value: u64) {
    assert!(value <= self.current && value >= self.min);
    assert!(!self.map.contains_key(&key));
    self.map.insert(key, value);
  }

  pub fn incr_current(&mut self) {
    self.current += 1;
    let current = self.current;
    let width = self.width;
    if self.current == self.min + width {
      self.map.retain(|_k, v| *v + width >= current + 1);
      self.min += 1
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use cryptohash::sha256;
  use rand::distributions::Alphanumeric;
  use rand::{thread_rng, Rng};

  //
  #[test]
  fn test_basic() {
    let factor = 3;
    let width: usize = 4;
    let mut ss = SlidingSet::new(0, 0, width as u64);
    let mut names = Vec::new();
    for _ in 0..width * factor {
      let rand_string: String = thread_rng().sample_iter(&Alphanumeric).take(16).collect();
      names.push(rand_string);
    }
    let digests: Vec<Digest> = names.iter()
                                    .map(|s| sha256::hash(&bincode::serialize(&s).unwrap()))
                                    .collect();

    // test the "sliding property".
    let mut n = 0;
    for i in 0..factor {
      for _ in 0..width {
        ss.insert(digests[n], n as u64);
        assert!(ss.contains_key(digests[n]));
        ss.incr_current();
        n += 1;
      }
      assert!(!ss.contains_key(digests[i * width]));
      assert!(ss.contains_key(digests[(i + 1) * width - 1]));
    }
  }
}
