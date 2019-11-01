#[cfg(target_arch = "wasm32")]
pub mod sha256 {
  use sha2::Digest as DigestTrait;
  use sha2::Sha256;
  use std::ops::{Index, Range, RangeFrom};
  pub const DIGESTBYTES: usize = 32;

  #[derive(Clone, Copy, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
  pub struct Digest(pub [u8; DIGESTBYTES]);

  pub fn hash(m: &[u8]) -> Digest {
    let temp = Sha256::digest(m);
    Digest { 0: *array_ref!(&temp, 0, 32) }
  }

  impl AsRef<[u8]> for Digest {
    fn as_ref(&self) -> &[u8] {
      &self.0
    }
  }

  impl Index<Range<usize>> for Digest {
    type Output = [u8];
    fn index(&self, _index: std::ops::Range<usize>) -> &[u8] {
      self.0.index(_index)
    }
  }

  impl Index<RangeFrom<usize>> for Digest {
    type Output = [u8];
    fn index(&self, _index: RangeFrom<usize>) -> &[u8] {
      self.0.index(_index)
    }
  }
}

#[cfg(not(target_arch = "wasm32"))]
pub mod sha256 {
  use sodiumoxide::crypto::hash::sha256;
  pub const DIGESTBYTES: usize = sha256::DIGESTBYTES;
  pub type Digest = sha256::Digest;
  pub fn hash(m: &[u8]) -> Digest {
    sha256::hash(m)
  }
}
