use credentials::{CredIssuerPublicKey, CredPoK, CredUserPublicKey};
use percent_encoding::{percent_decode, utf8_percent_encode, AsciiSet, CONTROLS};
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct PubCreds {
  pub name: String,
  pub attrs_sizes: Vec<(String, usize)>,
  pub issuer_pk: CredIssuerPublicKey,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct UserCreds {
  pub credname: String,
  pub user_pk: CredUserPublicKey,
  pub attrs: Vec<(String, String)>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct RevealFields {
  pub fields: Vec<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct AIRAddressAndPoK {
  pub addr: String,
  pub pok: CredPoK,
}

/// https://url.spec.whatwg.org/#fragment-percent-encode-set
const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');

pub fn urldecode(s: &str) -> String {
  let iter = percent_decode(s.as_bytes());
  iter.decode_utf8().unwrap().to_string()
}

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
