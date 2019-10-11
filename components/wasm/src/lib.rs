// Interface for issuing transactions that can be compiled to Wasm.
// Allows web clients to issue transactions from a browser contexts.
// For now, forwards transactions to a ledger hosted locally.
extern crate ledger;
extern crate serde;
extern crate zei;
#[macro_use]
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use txn_builder::{BuildsTransactions, TransactionBuilder};

use futures::{future, Future};

use js_sys::Promise;
use ledger::data_model::{AssetTokenCode, IssuerPublicKey, Transaction};

use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

use wasm_bindgen_futures::future_to_promise;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode, Response};
use zei::basic_crypto::signatures::{XfrKeyPair, XfrPublicKey, XfrSecretKey};

const HOST: &'static str = "localhost";
const PORT: &'static str = "8668";

#[wasm_bindgen]
// Keypair representing serialized private and public keys.
pub struct KeyPair(String, String);

// Wasm API to build transactions from browser
#[wasm_bindgen]
pub fn new_key_pair() -> KeyPair {
  let mut prng: ChaChaRng;
  prng = ChaChaRng::from_seed([0u8; 32]);
  let keypair = XfrKeyPair::generate(&mut prng);
  KeyPair(serde_json::to_string(keypair.get_sk_ref()).unwrap(),
          serde_json::to_string(keypair.get_pk_ref()).unwrap())
}

// Defines an asset on the ledger using the serialized strings in KeyPair and a couple of boolean policies
#[wasm_bindgen]
pub fn create_asset(key_pair: KeyPair,
                    updatable: bool,
                    traceable: bool)
                    -> Result<String, JsValue> {
  let public_key;
  let secret_key;

  if let Ok(pub_key) = serde_json::from_str::<XfrPublicKey>(&key_pair.1) {
    public_key = pub_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize private key."));
  }
  if let Ok(sec_key) = serde_json::from_str::<XfrSecretKey>(&key_pair.0) {
    secret_key = sec_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize private key."));
  }

  let asset_token = AssetTokenCode::new_from_base64(&String::from("test")).unwrap();

  let mut txn_builder = TransactionBuilder::default();
  match txn_builder.add_operation_create_asset(&IssuerPublicKey { key: public_key },
                                               &secret_key,
                                               Some(asset_token),
                                               updatable,
                                               traceable,
                                               &String::from("{}"),
                                               true)
  {
    Ok(_) => return Ok(txn_builder.serialize_str().unwrap()),
    Err(_) => return Err(JsValue::from_str("Could not build transaction")),
  }
}

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
  opts.method("GET");
  opts.mode(RequestMode::Cors);

  let req_string = format!("http://{}:{}/submit_transaction/{}",
                           HOST,
                           PORT,
                           encode_uri(&transaction_str));
  let request = Request::new_with_str_and_init(&req_string, &opts).unwrap();
  let window = web_sys::window().unwrap();
  let request_promise = window.fetch_with_request(&request);
  future_to_promise(JsFuture::from(request_promise))
}

mod tests {
  use super::*;
  #[test]
  fn test_wasm_define_transaction() {
    let key_pair = new_key_pair();
    let transaction = create_asset(key_pair, true, true);
  }
}
