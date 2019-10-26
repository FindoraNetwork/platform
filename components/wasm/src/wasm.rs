// Interface for issuing transactions that can be compiled to Wasm.
// Allows web clients to issue transactions from a browser contexts.
// For now, forwards transactions to a ledger hosted locally.
// To compile wasm package, run wasm-pack build in the wasm directory
#![deny(warnings)]
extern crate ledger;
extern crate serde;
extern crate zei;

use js_sys::Promise;
use ledger::data_model::*;
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use serde_json::from_str;
use txn_builder::{BuildsTransactions, TransactionBuilder};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::future_to_promise;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode};
use zei::basic_crypto::signatures::{XfrKeyPair, XfrPublicKey, XfrSecretKey};
use zei::xfr::structs::BlindAssetRecord;

const HOST: &'static str = "localhost";
const PORT: &'static str = "8668";

#[wasm_bindgen]
#[derive(Debug)]
// Keypair representing serialized private and public keys.
pub struct KeyPair {
  pub_key: String,
  priv_key: String,
}

#[wasm_bindgen]
impl KeyPair {
  pub fn get_pub_key(&self) -> String {
    self.pub_key.clone()
  }

  pub fn get_priv_key(&self) -> String {
    self.priv_key.clone()
  }

  pub fn new() -> KeyPair {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([0u8; 32]);
    let keypair = XfrKeyPair::generate(&mut prng);
    KeyPair { priv_key: serde_json::to_string(keypair.get_sk_ref()).unwrap(),
              pub_key: serde_json::to_string(keypair.get_pk_ref()).unwrap() }
  }
}

fn split_key_pair(key_pair: &KeyPair) -> Result<(XfrPublicKey, XfrSecretKey), serde_json::error::Error> {
  let public_key = from_str::<XfrPublicKey>(&key_pair.get_pub_key())?;
  let secret_key = from_str::<XfrSecretKey>(&key_pair.get_priv_key())?;
  Ok((public_key, secret_key))
}

// GENERATED CODE INSERTED HERE
include!(concat!(env!("OUT_DIR"), "/wasm.rs"));

// Ensures that the transaction serialization is valid URI text
fn encode_uri(to_encode: &str) -> String {
  const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ')
                                       .add(b'"')
                                       .add(b'`')
                                       .add(b'{')
                                       .add(b'/')
                                       .add(b'}');

  utf8_percent_encode(&to_encode, FRAGMENT).to_string()
}

#[wasm_bindgen]
// Submit transation to the ledger at HOST and PORT.
pub fn submit_transaction(transaction_str: String) -> Promise {
  let mut opts = RequestInit::new();
  opts.method("POST");
  opts.mode(RequestMode::Cors);

  let req_string = format!("http://{}:{}/submit_transaction/{}",
                           HOST,
                           PORT,
                           encode_uri(&transaction_str));

  create_query_promise(&opts, &req_string)
}

#[wasm_bindgen]
pub fn test_deserialize(str: String) -> bool {
  let blind_asset_record = serde_json::from_str::<BlindAssetRecord>(&str);
  return blind_asset_record.is_ok();
}

#[wasm_bindgen]
// Get txo by index
pub fn get_txo(index: u64) -> Promise {
  let mut opts = RequestInit::new();
  opts.method("GET");
  opts.mode(RequestMode::Cors);

  let req_string = format!("http://{}:{}/utxo_sid/{}", HOST, PORT, format!("{}", index));

  create_query_promise(&opts, &req_string)
}

// Given a request string and a request init object, constructs
// the JS promise to be returned to the client
fn create_query_promise(opts: &RequestInit, req_string: &str) -> Promise {
  let request = Request::new_with_str_and_init(&req_string, &opts).unwrap();
  let window = web_sys::window().unwrap();
  let request_promise = window.fetch_with_request(&request);
  future_to_promise(JsFuture::from(request_promise))
}

#[cfg(test)]
mod tests {
  use super::*;

  // Test to ensure that define transaction is being constructed correctly
  #[test]
  fn test_wasm_define_transaction() {
    let key_pair = KeyPair::new();
    create_asset(key_pair, String::from("abcd"), true, true).unwrap();
  }
  #[test]
  // Test to ensure that issue transaction is being constructed correctly
  fn test_wasm_issue_transaction() {
    let key_pair = KeyPair::new();
    issue_asset(key_pair, String::from("abcd"), 1, 5).unwrap();
  }
}
