#![allow(dead_code)]
use serde::{Deserialize, Serialize};
use sparse_merkle_tree::{SmtMap256, digest, MerkleProof, check_merkle_proof as smt_check_proof};
use std::io::Error;
use std::io::prelude::Read;
use std::fs::File;

pub use sparse_merkle_tree::Digest;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AIR(SmtMap256<String>);
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AIRMerkleProof(MerkleProof);
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AIRResult {
  pub merkle_root: Digest,
  pub key: String,
  pub value: Option<String>, 
  pub merkle_proof: MerkleProof
}

impl AIR {
  pub fn new() -> Self {
    Self { 0: SmtMap256::<String>::new()}
  }

  pub fn key_of_byteref(key: impl AsRef<[u8]>) -> Digest {
    digest(key.as_ref())
  }

  pub fn set(&mut self, key: impl AsRef<[u8]>, value: Option<String>) -> Option<String> {
    let hashed_key = digest(key.as_ref());
    self.0.set(&hashed_key, value)
  }

  pub fn get(&self, key: impl AsRef<[u8]>) -> Option<&String> {
    let hashed_key = digest(key.as_ref());
    self.0.get(&hashed_key)
  }

  pub fn get_with_proof(&self, key: impl AsRef<[u8]>) -> (Option<&String>, MerkleProof) {
    let hashed_key = digest(key.as_ref());
    self.0.get_with_proof(&hashed_key)
  }

  pub fn merkle_root(&self) -> &Digest {
    self.0.merkle_root()
  }

  pub fn check_merkle_proof(&self, key: impl AsRef<[u8]>, value: Option<&String>, proof: &MerkleProof) -> bool {
    let hashed_key = digest(key.as_ref());
    self.0.check_merkle_proof(&hashed_key, value, proof)
  }
}

pub fn check_merkle_proof<String: AsRef<[u8]>>(merkle_root: &Digest,
                                               key: impl AsRef<[u8]>,
                                               value: Option<&String>,
                                               proof: &MerkleProof)
                                               -> bool {
  let hashed_key = digest(key.as_ref());
  smt_check_proof(merkle_root, &hashed_key, value, proof)
}

pub fn open(path: &str) -> Result<AIR, Error> {
  let mut file = File::open(path)?;
  let mut contents = String::new();
  file.read_to_string(&mut contents)?;

  // Deserialize and print Rust data structure.
  let result: AIR = serde_json::from_str(&contents)?;
  Ok(result)
}
