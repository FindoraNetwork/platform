// Interface for issuing transactions that can be compiled to Wasm.
// Allows web clients to issue transactions from a browser contexts.
// For now, forwards transactions to a ledger hosted locally.
// To compile wasm package, run wasm-pack build in the wasm directory
extern crate ledger;
extern crate serde;
extern crate zei;
use hex;
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use std::str;
use txn_builder::{BuildsTransactions, TransactionBuilder};

use js_sys::Promise;
use ledger::data_model::{
  AccountAddress, AssetTypeCode, IssuerPublicKey, TxOutput, TxoRef, TxoSID, Utxo,
};

use bulletproofs::PedersenGens;
use ledger::utils::sha256;
use rand::prelude::thread_rng;
use rand::Rng;
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use wasm_bindgen::prelude::*;

use wasm_bindgen_futures::future_to_promise;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode};
use zei::algebra::ristretto::RistPoint;
use zei::basic_crypto::elgamal::{elgamal_decrypt, ElGamalSecretKey};
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
pub fn test_thread_rng() {
  let countdown = thread_rng().gen::<u32>() % (256 * 1024);
}

#[wasm_bindgen]
impl KeyPair {
  pub fn get_pub_key(&self) -> String {
    self.pub_key.clone()
  }

  pub fn get_priv_key(&self) -> String {
    self.priv_key.clone()
  }

  pub fn new(rand_seed: &str) -> KeyPair {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([rand_seed.as_bytes()[0]; 32]);
    let keypair = XfrKeyPair::generate(&mut prng);
    KeyPair { priv_key: serde_json::to_string(keypair.get_sk_ref()).unwrap(),
              pub_key: serde_json::to_string(keypair.get_pk_ref()).unwrap() }
  }
}

// Defines an asset on the ledger using the serialized strings in KeyPair and a couple of boolean policies
#[wasm_bindgen]
pub fn create_asset(public_key_str: String,
                    private_key_str: String,
                    memo: String,
                    token_code: String,
                    updatable: bool,
                    traceable: bool)
                    -> Result<String, JsValue> {
  let public_key;
  let secret_key;

  if let Ok(pub_key) = serde_json::from_str::<XfrPublicKey>(&public_key_str) {
    public_key = pub_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize public key."));
  }
  if let Ok(sec_key) = serde_json::from_str::<XfrSecretKey>(&private_key_str) {
    secret_key = sec_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize private key."));
  }

  let asset_token = AssetTypeCode::new_from_base64(&token_code).unwrap();

  let mut txn_builder = TransactionBuilder::default();
  match txn_builder.add_operation_create_asset(&IssuerPublicKey { key: public_key },
                                               &secret_key,
                                               Some(asset_token),
                                               updatable,
                                               traceable,
                                               &memo,
                                               true)
  {
    Ok(_) => return Ok(txn_builder.serialize_str().unwrap()),
    Err(_) => return Err(JsValue::from_str("Could not build transaction")),
  }
}

#[wasm_bindgen]
pub fn sha256str(str: &str) -> String {
  let digest = sha256::hash(&str.as_bytes());
  hex::encode(digest).into()
}

#[wasm_bindgen]
pub fn sign(private_key_str: String,
            public_key_str: String,
            message: String)
            -> Result<String, JsValue> {
  let secret_key;
  let public_key;

  if let Ok(sec_key) = serde_json::from_str::<XfrSecretKey>(&private_key_str) {
    secret_key = sec_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize private key."));
  }
  if let Ok(pub_key) = serde_json::from_str::<XfrPublicKey>(&public_key_str) {
    public_key = pub_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize public key."));
  }

  let signature = secret_key.sign(&message.as_bytes(), &public_key);
  let mut smaller_signature: [u8; 32] = Default::default();
  smaller_signature.copy_from_slice(&signature.0.to_bytes()[0..32]);
  Ok(hex::encode(smaller_signature))
}

fn u8_littleendian_slice_to_u32(array: &[u8]) -> u32 {
  u32::from(array[0])
  | u32::from(array[1]) << 8
  | u32::from(array[2]) << 16
  | u32::from(array[3]) << 24
}

fn u32_pair_to_u64(x: (u32, u32)) -> u64 {
  (x.1 as u64) << 32 ^ (x.0 as u64)
}

#[wasm_bindgen]
pub fn get_tracked_amount(blind_asset_record: String,
                          issuer_private_key_point: String)
                          -> Result<String, JsValue> {
  let pc_gens = PedersenGens::default();
  let blind_asset_record = serde_json::from_str::<BlindAssetRecord>(&blind_asset_record).map_err(|e| {
                             JsValue::from_str("Could not deserialize blind asset record")
                           })?;
  let issuer_private_key = serde_json::from_str(&issuer_private_key_point).map_err(|e| {
                             JsValue::from_str("Could not deserialize issuer private key")
                           })?;
  if let Some(lock_amount) = blind_asset_record.issuer_lock_amount {
    match (elgamal_decrypt(&RistPoint(pc_gens.B), &(lock_amount.0), &issuer_private_key),
           elgamal_decrypt(&RistPoint(pc_gens.B), &(lock_amount.1), &issuer_private_key))
    {
      (Ok(s1), Ok(s2)) => {
        let amount = u32_pair_to_u64((u8_littleendian_slice_to_u32(s1.0.as_bytes()),
                                      u8_littleendian_slice_to_u32(s1.0.as_bytes())));
        return Ok(amount.to_string());
      }
      (_, _) => return Err(JsValue::from_str("Unable to decrypt amount")),
    }
  } else {
    return Err(JsValue::from_str("Asset record does not contain decrypted lock amount"));
  }
}

#[wasm_bindgen]
pub fn issue_asset(public_key_str: String,
                   private_key_str: String,
                   token_code: String,
                   seq_num: u64,
                   amount: u64)
                   -> Result<String, JsValue> {
  let public_key;
  let secret_key;

  if let Ok(pub_key) = serde_json::from_str::<XfrPublicKey>(&public_key_str) {
    public_key = pub_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize public key."));
  }
  if let Ok(sec_key) = serde_json::from_str::<XfrSecretKey>(&private_key_str) {
    secret_key = sec_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize private key."));
  }

  let asset_token = AssetTypeCode::new_from_base64(&token_code).unwrap();

  let mut txn_builder = TransactionBuilder::default();
  match txn_builder.add_basic_issue_asset(&IssuerPublicKey { key: public_key },
                                          &secret_key,
                                          &asset_token,
                                          seq_num,
                                          amount)
  {
    Ok(_) => return Ok(txn_builder.serialize_str().unwrap()),
    Err(_) => return Err(JsValue::from_str("Could not build transaction")),
  }
}

#[wasm_bindgen]
pub fn transfer_asset(public_key_str: String,
                      secret_key_str: String,
                      txo_sid: u64,
                      amount: u64,
                      blind_asset_record_str: String)
                      -> Result<String, JsValue> {
  let public_key;
  let secret_key;
  let blind_asset_record;

  if let Ok(pub_key) = serde_json::from_str::<XfrPublicKey>(&public_key_str) {
    public_key = pub_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize public key."));
  }

  if let Ok(sec_key) = serde_json::from_str::<XfrSecretKey>(&secret_key_str) {
    secret_key = sec_key;
  } else {
    return Err(JsValue::from_str("Could not deserialize private key."));
  }

  if let Ok(asset_record) = serde_json::from_str::<BlindAssetRecord>(&blind_asset_record_str) {
    blind_asset_record = asset_record;
  } else {
    return Err(JsValue::from_str("Could not deserialize blind asset record."));
  }

  let mut txn_builder = TransactionBuilder::default();
  match txn_builder.add_basic_transfer_asset(&[(&TxoRef::Absolute(TxoSID(txo_sid)),
                                                &blind_asset_record,
                                                amount,
                                                &secret_key)],
                                             &[(amount, &AccountAddress { key: public_key })])
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

#[wasm_bindgen]
// Get txo by index
pub fn get_asset_token(name: String) -> Promise {
  let mut opts = RequestInit::new();
  opts.method("GET");
  opts.mode(RequestMode::Cors);

  let req_string = format!("http://{}:{}/asset_token/{}",
                           HOST,
                           PORT,
                           format!("{}", name));

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
  use ledger::data_model::TxoSID;
  use ledger::store::{LedgerAccess, LedgerState, LedgerUpdate};
  use txn_builder::{BuildsTransactions, TransactionBuilder};

  // Test to ensure that define transaction is being constructed correctly
  #[test]
  fn test_wasm_define_transaction() {
    let key_pair = KeyPair::new(&String::from("test"));
    create_asset(key_pair, String::from("abcd"), true, true);
  }
  #[test]
  // Test to ensure that issue transaction is being constructed correctly
  fn test_wasm_issue_transaction() {
    let key_pair = KeyPair::new(&String::from("test"));
    issue_asset(key_pair.get_pub_key(),
                key_pair.get_priv_key(),
                String::from("abcd"),
                1,
                5);
  }
}
