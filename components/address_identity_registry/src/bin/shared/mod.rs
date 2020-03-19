#![deny(warnings)]
#![allow(clippy::module_inception)]
use serde_derive::{Deserialize, Serialize};
use credentials::{CredIssuerPublicKey, CredUserPublicKey};

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct PubCreds {
  pub name: String,
  pub issuer_pk: CredIssuerPublicKey,
}

// pub use shared::*;
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct UserCreds {
  pub credname: String,
  pub user_pk: CredUserPublicKey,
  pub attrs: Vec<(String, String)>,
}
