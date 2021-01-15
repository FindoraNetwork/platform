#![deny(warnings)]
#![allow(dead_code)]
use serde::{Deserialize, Serialize};
use sparse_merkle_tree::{
    check_merkle_proof as smt_check_proof, Key, MerkleProof, SmtMap256,
};
use std::fs;
use std::io::Error;

pub use sparse_merkle_tree::Digest;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AIR(SmtMap256<String>);
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AIRMerkleProof(MerkleProof);
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AIRResult {
    pub merkle_root: Digest,
    pub key: String,
    pub value: Option<String>,
    pub merkle_proof: MerkleProof,
}

impl Default for AIR {
    fn default() -> Self {
        AIR {
            0: SmtMap256::<String>::new(),
        }
    }
}

impl AIR {
    pub fn key_of_byteref(key: impl AsRef<[u8]>) -> Key {
        Key::hash(key)
    }

    pub fn set(
        &mut self,
        key: impl AsRef<[u8]>,
        value: Option<String>,
    ) -> Option<String> {
        let hashed_key = Key::hash(key);
        self.0.set(&hashed_key, value)
    }

    pub fn get(&self, key: impl AsRef<[u8]>) -> Option<&String> {
        let hashed_key = Key::hash(key);
        self.0.get(&hashed_key)
    }

    pub fn get_with_proof(
        &self,
        key: impl AsRef<[u8]>,
    ) -> (Option<&String>, MerkleProof) {
        let hashed_key = Key::hash(key);
        self.0.get_with_proof(&hashed_key)
    }

    pub fn merkle_root(&self) -> &Digest {
        self.0.merkle_root()
    }

    pub fn check_merkle_proof(
        &self,
        key: impl AsRef<[u8]>,
        value: Option<&String>,
        proof: &MerkleProof,
    ) -> bool {
        let hashed_key = Key::hash(key.as_ref());
        self.0.check_merkle_proof(&hashed_key, value, proof)
    }
}

pub fn check_merkle_proof<String: AsRef<[u8]>>(
    merkle_root: &Digest,
    key: impl AsRef<[u8]>,
    value: Option<&String>,
    proof: &MerkleProof,
) -> bool {
    let hashed_key = Key::hash(key.as_ref());
    smt_check_proof(merkle_root, &hashed_key, value, proof)
}

pub fn open(path: &str) -> Result<AIR, Error> {
    let contents = fs::read_to_string(path)?;

    // Deserialize and print Rust data structure.
    let result: AIR = serde_json::from_str(&contents)?;
    Ok(result)
}
