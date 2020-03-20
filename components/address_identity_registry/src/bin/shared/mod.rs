use percent_encoding::{AsciiSet, CONTROLS, utf8_percent_encode };
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

/// https://url.spec.whatwg.org/#fragment-percent-encode-set
const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');

pub fn urlencode(input: &str) -> String {
  let iter = utf8_percent_encode(input, FRAGMENT);
  iter.collect()
}

const PROTOCOL: &str = "http";
const SERVER_HOST: &str = "localhost";

/// Port for querying values.
pub const QUERY_PORT: &str = "8668";
/// Port for submitting transactions.
pub const SUBMIT_PORT: &str = "8669";

/// Sets the protocol and host.
///
/// Environment variables `PROTOCOL` and `SERVER_HOST` set the protocol and host,
///
/// By default, the protocol is `http` and the host is `testnet.findora.org`.
pub fn protocol_host() -> (&'static str, &'static str) {
  (std::option_env!("PROTOCOL").unwrap_or(PROTOCOL), // "https"
   std::option_env!("SERVER_HOST").unwrap_or(SERVER_HOST)) // "testnet.findora.org"
}
