#[allow(dead_code)]
use serde::{Deserialize, Serialize};
use sparse_merkle_tree::{MerkleProof, SmtMap256, Hash256, hash_256, check_merkle_proof as smt_check_proof};
use std::io::Error;
use std::io::prelude::Read;
use std::fs::File;

#[derive(Clone, Default, Debug, Serialize, Deserialize)]
pub struct AIR<Value: AsRef<[u8]>>(SmtMap256<Value>);

impl <Value: AsRef<[u8]>> AIR<Value> {
  pub fn new() -> Self {
    Self { 0: SmtMap256::<Value>::new()}
  }

  pub fn set(&mut self, key: impl AsRef<[u8]>, value: Option<Value>) -> Option<Value> {
    let hashed_key = hash_256(key.as_ref());
    self.0.set(&hashed_key, value)
  }

  pub fn get(&self, key: impl AsRef<[u8]>) -> Option<&Value> {
    let hashed_key = hash_256(key.as_ref());
    self.0.get(&hashed_key)
  }

  pub fn get_with_proof(&self, key: impl AsRef<[u8]>) -> (Option<&Value>, MerkleProof) {
    let hashed_key = hash_256(key.as_ref());
    self.0.get_with_proof(&hashed_key)
  }

  pub fn merkle_root(&self) -> &Hash256 {
    self.0.merkle_root()
  }

  pub fn check_merkle_proof(&self, key: impl AsRef<[u8]>, value: Option<&Value>, proof: &MerkleProof) -> bool {
    let hashed_key = hash_256(key.as_ref());
    self.0.check_merkle_proof(&hashed_key, value, proof)
  }
}

pub fn check_merkle_proof<Value: AsRef<[u8]>>(merkle_root: &Hash256,
                                              key: impl AsRef<[u8]>,
                                              value: Option<&Value>,
                                              proof: &MerkleProof)
                                              -> bool {
  let hashed_key = hash_256(key.as_ref());
  smt_check_proof(merkle_root, &hashed_key, value, proof)
}

pub fn open(path: &str) -> Result<AIR<String>, Error> {
  let mut file = File::open(path)?;
  let mut contents = String::new();
  file.read_to_string(&mut contents)?;

  // Deserialize and print Rust data structure.
  let result: AIR<String> = serde_json::from_str(&contents)?;
  Ok(result)
}
