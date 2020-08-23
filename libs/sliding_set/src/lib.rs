#![feature(btree_drain_filter)]
use cryptohash::sha256::Digest;
use serde::{Deserialize, Serialize};

#[derive(Clone, Default, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub struct SlidingSet {
  current: usize,
  width: usize,
  map: Vec<Vec<Digest>>,
}

impl SlidingSet {
  pub fn new(width: usize) -> Self {
    let mut map = Vec::with_capacity(width as usize);
    for _ in 0..width {
      map.push(Vec::new());
    }
    let current = 0;
    SlidingSet { current,
                 width,
                 map }
  }
}

impl SlidingSet {
  pub fn contains_key(&self, key: Digest) -> bool {
    self.get(key).is_some()
  }

  pub fn get(&self, key: Digest) -> Option<usize> {
    for (index, vec) in self.map.iter().enumerate() {
      for k in vec.iter() {
        if *k == key {
          return Some(index);
        }
      }
    }
    None
  }

  pub fn insert(&mut self, key: Digest, value: usize) {
    assert!(value <= self.current && value + self.width >= (self.current + 1));
    assert!(!self.contains_key(key));

    self.map[value % self.width].push(key);
  }

  pub fn incr_current(&mut self) {
    self.current += 1;
    let current_index = self.current % self.width;
    self.map[current_index].clear();
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
    let mut ss = SlidingSet::new(width);
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
        ss.insert(digests[n], n);
        assert!(ss.contains_key(digests[n]));
        ss.incr_current();
        n += 1;
      }
      assert!(!ss.contains_key(digests[i * width]));
      assert!(ss.contains_key(digests[(i + 1) * width - 1]));
    }
  }
}
