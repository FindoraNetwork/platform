#![deny(warnings)]
#![allow(clippy::module_inception)]
use serde_derive::{Deserialize, Serialize};
use zei::api::anon_creds::{ACPoK, ACIssuerPublicKey, ACUserPublicKey};

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct PubCreds {
  pub name: String,
  pub num_attrs: u64,
  pub issuer_pk: ACIssuerPublicKey,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct UserCreds {
  pub credname: String,
  pub user_pk: ACUserPublicKey,
  pub attrs: Vec<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Bitmap {
  pub bits: Vec<bool>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct AIRAddressAndPoK {
  pub addr: String,
  pub pok: ACPoK,
}

