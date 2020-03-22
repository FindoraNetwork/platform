#![deny(warnings)]
#![allow(clippy::module_inception)]
use credentials::{CredIssuerPublicKey, CredPoK, CredUserPublicKey};
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct PubCreds {
  pub name: String,
  pub issuer_pk: CredIssuerPublicKey,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct UserCreds {
  pub credname: String,
  pub user_pk: CredUserPublicKey,
  pub attrs: Vec<(String, String)>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Bitmap {
  pub bits: Vec<bool>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct AIRAddressAndPoK {
  pub addr: String,
  pub pok: CredPoK,
}
