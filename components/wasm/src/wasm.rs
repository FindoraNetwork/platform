// Interface for issuing transactions that can be compiled to Wasm.
// Allows web clients to issue transactions from a browser contexts.
// For now, forwards transactions to a ledger hosted locally.
// To compile wasm package, run wasm-pack build in the wasm directory
#![deny(warnings)]
extern crate ledger;
extern crate rand;
extern crate rand_chacha;
extern crate serde;
extern crate wasm_bindgen;
extern crate zei;
<<<<<<< HEAD
=======
use hex;
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use std::str;
use txn_builder::{BuildsTransactions, TransactionBuilder};
use wasm_bindgen_test::*;
>>>>>>> master

use bulletproofs::PedersenGens;
use hex;
use js_sys::Promise;
use ledger::data_model::{AccountAddress, AssetTypeCode, IssuerPublicKey, TxoRef, TxoSID};
use ledger::utils::sha256;
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use rand::FromEntropy;
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use serde::{Deserialize, Serialize};
use std::str;
use txn_builder::{BuildsTransactions, TransactionBuilder};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::future_to_promise;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode};
use zei::algebra::bls12_381::{BLSGt, BLSScalar, BLSG1, BLSG2};
use zei::algebra::groups::Group;
use zei::algebra::groups::Scalar;
use zei::algebra::ristretto::{RistPoint, RistScalar};
use zei::basic_crypto::elgamal::{
  elgamal_decrypt, elgamal_derive_public_key, elgamal_generate_secret_key, ElGamalPublicKey,
};
use zei::basic_crypto::signatures::XfrKeyPair;
use zei::crypto::anon_creds::{
  ac_keygen_issuer, ac_keygen_user, ac_reveal, ac_sign, ac_verify, ACIssuerPublicKey,
  ACIssuerSecretKey, ACRevealSig, ACSignature, ACUserPublicKey, ACUserSecretKey,
};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::structs::{AssetIssuerPubKeys, BlindAssetRecord};

const HOST: &str = "localhost";
const PORT: &str = "8668";

#[wasm_bindgen]
pub fn get_pub_key_str(key_pair: &XfrKeyPair) -> String {
  serde_json::to_string(key_pair.get_pk_ref()).unwrap()
}

#[wasm_bindgen]
pub fn get_priv_key_str(key_pair: &XfrKeyPair) -> String {
  serde_json::to_string(key_pair.get_sk_ref()).unwrap()
}

#[wasm_bindgen]
pub fn new_keypair(rand_seed: &str) -> XfrKeyPair {
  let mut prng: ChaChaRng;
  prng = ChaChaRng::from_seed([rand_seed.as_bytes()[0]; 32]);
  XfrKeyPair::generate(&mut prng)
}

#[wasm_bindgen]
pub fn keypair_to_str(key_pair: &XfrKeyPair) -> String {
  hex::encode(key_pair.zei_to_bytes())
}

#[wasm_bindgen]
pub fn keypair_from_str(str: String) -> XfrKeyPair {
  XfrKeyPair::zei_from_bytes(&hex::decode(str).unwrap())
}

#[wasm_bindgen]
pub fn generate_elgamal_secret_key() -> JsValue {
  let mut small_rng = ChaChaRng::from_entropy();
  let sk = elgamal_generate_secret_key::<_, RistScalar>(&mut small_rng);
  JsValue::from_serde(&sk).unwrap()
}

#[wasm_bindgen]
pub fn derive_elgamal_public_key(elgamal_secret_key_jsvalue: JsValue) -> Result<JsValue, JsValue> {
  let pc_gens = PedersenGens::default();
  let sk = elgamal_secret_key_jsvalue.into_serde().unwrap();
  let pk = elgamal_derive_public_key(&RistPoint(pc_gens.B), &sk);
  return Ok(JsValue::from_serde(&pk).unwrap());
}

// Defines an asset on the ledger using the serialized strings in KeyPair and a couple of boolean policies
#[wasm_bindgen]
pub fn create_asset(key_pair: &XfrKeyPair,
                    memo: String,
                    token_code: String,
                    updatable: bool,
                    traceable: bool)
                    -> Result<String, JsValue> {
  let asset_token = AssetTypeCode::new_from_base64(&token_code).unwrap();

  let mut txn_builder = TransactionBuilder::default();
  match txn_builder.add_operation_create_asset(&IssuerPublicKey { key: *key_pair.get_pk_ref() },
                                               &key_pair.get_sk_ref(),
                                               Some(asset_token),
                                               updatable,
                                               traceable,
                                               &memo)
  {
    Ok(_) => Ok(txn_builder.serialize_str().unwrap()),
    Err(_) => Err(JsValue::from_str("Could not build transaction")),
  }
}

#[wasm_bindgen]
pub fn sha256str(str: &str) -> String {
  let digest = sha256::hash(&str.as_bytes());
  hex::encode(digest)
}

#[wasm_bindgen]
pub fn sign(key_pair: &XfrKeyPair, message: String) -> Result<JsValue, JsValue> {
  let signature = key_pair.get_sk_ref()
                          .sign(&message.as_bytes(), key_pair.get_pk_ref());
  let mut smaller_signature: [u8; 32] = Default::default();
  smaller_signature.copy_from_slice(&signature.0.to_bytes()[0..32]);
  Ok(JsValue::from_serde(&smaller_signature).unwrap())
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
  let blind_asset_record = serde_json::from_str::<BlindAssetRecord>(&blind_asset_record).map_err(|_e| {
                             JsValue::from_str("Could not deserialize blind asset record")
                           })?;
  let issuer_private_key = serde_json::from_str(&issuer_private_key_point).map_err(|_e| {
                             JsValue::from_str("Could not deserialize issuer private key")
                           })?;
  if let Some(lock_amount) = blind_asset_record.issuer_lock_amount {
    match (elgamal_decrypt(&RistPoint(pc_gens.B), &(lock_amount.0), &issuer_private_key),
           elgamal_decrypt(&RistPoint(pc_gens.B), &(lock_amount.1), &issuer_private_key))
    {
      (Ok(s1), Ok(s2)) => {
        let amount = u32_pair_to_u64((u8_littleendian_slice_to_u32(s1.0.as_bytes()),
                                      u8_littleendian_slice_to_u32(s2.0.as_bytes())));
        Ok(amount.to_string())
      }
      (_, _) => Err(JsValue::from_str("Unable to decrypt amount")),
    }
  } else {
    Err(JsValue::from_str("Asset record does not contain decrypted lock amount"))
  }
}

#[wasm_bindgen]
pub fn issue_asset(key_pair: &XfrKeyPair,
                   elgamal_pub_key: String,
                   token_code: String,
                   seq_num: u64,
                   amount: u64)
                   -> Result<String, JsValue> {
  let asset_token = AssetTypeCode::new_from_base64(&token_code).unwrap();

  let mut txn_builder = TransactionBuilder::default();
  // construct asset tracking keys
  let issuer_keys;
  if elgamal_pub_key.is_empty() {
    issuer_keys = None
  } else {
    let pk = serde_json::from_str::<ElGamalPublicKey<RistPoint>>(&elgamal_pub_key).map_err(|_e| return JsValue::from_str("could not deserialize elgamal key"))?;
    let mut small_rng = ChaChaRng::from_entropy();
    let sk = elgamal_generate_secret_key::<_, BLSScalar>(&mut small_rng);
    // For now, zei expecs both id reveal key and tracking decryption key, so we construct a dummy
    // id reveal key
    let id_reveal_pub_key = elgamal_derive_public_key(&BLSG1::get_base(), &sk);
    issuer_keys = Some(AssetIssuerPubKeys { eg_ristretto_pub_key: pk,
                                            eg_blsg1_pub_key: id_reveal_pub_key });
  }

  match txn_builder.add_basic_issue_asset(&IssuerPublicKey { key: *key_pair.get_pk_ref() },
                                          key_pair.get_sk_ref(),
                                          &issuer_keys,
                                          &asset_token,
                                          seq_num,
                                          amount)
  {
    Ok(_) => Ok(txn_builder.serialize_str().unwrap()),
    Err(_) => Err(JsValue::from_str("Could not build transaction")),
  }
}

#[wasm_bindgen]
pub fn transfer_asset(transfer_from: &XfrKeyPair,
                      transfer_to: &XfrKeyPair,
                      txo_sid: u64,
                      amount: u64,
                      blind_asset_record: String)
                      -> Result<String, JsValue> {
  let blind_asset_record = serde_json::from_str::<BlindAssetRecord>(&blind_asset_record).map_err(|_e| {
                             JsValue::from_str("Could not deserialize blind asset record")
                           })?;

  let mut txn_builder = TransactionBuilder::default();
  match txn_builder.add_basic_transfer_asset(&transfer_from,
                                             &[(&TxoRef::Absolute(TxoSID(txo_sid)),
                                                &blind_asset_record,
                                                amount)],
                                             &[(amount,
                                                &AccountAddress { key:
                                                                    *transfer_to.get_pk_ref() })])
  {
    Ok(_) => Ok(txn_builder.serialize_str().unwrap()),
    Err(_) => Err(JsValue::from_str("Could not build transaction")),
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
  blind_asset_record.is_ok()
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

  let req_string = format!("http://{}:{}/asset_token/{}", HOST, PORT, name);

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

//
// Issuer, User and Prover structs for credentialing
// Currently support verifying the lower bound of the credit score
//

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct Issuer {
  public_key: ACIssuerPublicKey<BLSG1, BLSG2>,
  secret_key: ACIssuerSecretKey<BLSG1, BLSScalar>,
}

#[wasm_bindgen]
impl Issuer {
  // Create a new issuer, generating the key pair with the knowledge of the number of attributes
  // TODO (Keyao):
  //  Make sure we can tell which attribute is which, possibly by fixing the order of attributes
  //  Then pass all the attributes to sign_min_credit_score and sign the lower bound of credit score only
  pub fn new(num_attr: usize) -> Issuer {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([0u8; 32]);
    let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_, BLSGt>(&mut prng, num_attr);

    Issuer { public_key: issuer_pk,
             secret_key: issuer_sk }
  }

  // Convert an Issuer to JsValue
  pub fn jsvalue(&mut self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }

  // Sign the low bound of the credit score
  pub fn sign_min_credit_score(&self, user_jsvalue: &JsValue, min_credit_score: u64) -> JsValue {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([0u8; 32]);
    let user: User = user_jsvalue.into_serde().unwrap();

    let attrs = [BLSScalar::from_u64(min_credit_score)];
    let sig = ac_sign::<_, BLSGt>(&mut prng, &self.secret_key, &user.public_key, &attrs);

    JsValue::from_serde(&sig).unwrap()
  }
}

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct User {
  public_key: ACUserPublicKey<BLSG1>,
  secret_key: ACUserSecretKey<BLSScalar>,
}

#[wasm_bindgen]
impl User {
  // Create a new user, generating the key pair using the issuer's public key
  pub fn new(issuer: &Issuer, rand_seed: &str) -> User {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([rand_seed.as_bytes()[0]; 32]);
    let (user_pk, user_sk) = ac_keygen_user::<_, BLSGt>(&mut prng, &issuer.public_key);

    User { public_key: user_pk,
           secret_key: user_sk }
  }

  // Convert a User to JsValue
  pub fn jsvalue(&mut self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }

  // Commit the lower bound of the credit score with the issuer's signature
  pub fn commit_min_credit_score(&self,
                                 issuer_jsvalue: &JsValue,
                                 sig: &JsValue,
                                 min_credit_score: u64,
                                 reveal_credit_score: bool)
                                 -> JsValue {
    let issuer: Issuer = issuer_jsvalue.into_serde().unwrap();
    let sig: ACSignature<BLSG1> = sig.into_serde().unwrap();
    let mut prng = ChaChaRng::from_seed([0u8; 32]);

    let attrs = [BLSScalar::from_u64(min_credit_score)];
    let bitmap = [reveal_credit_score];

    let proof = ac_reveal::<_, BLSGt>(&mut prng,
                                      &self.secret_key,
                                      &issuer.public_key,
                                      &sig,
                                      &attrs,
                                      &bitmap).unwrap();

    JsValue::from_serde(&proof).unwrap()
  }
}

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct Prover;

#[wasm_bindgen]
impl Prover {
  // Verify the lower bound of credit score
  fn verify_min_credit_score(proof_jsvalue: &JsValue,
                             issuer_jsvalue: &JsValue,
                             min_credit_score: u64,
                             reveal_min_credit_score: bool)
                             -> bool {
    let proof: ACRevealSig<BLSG1, BLSG2, BLSScalar> = proof_jsvalue.into_serde().unwrap();
    let issuer: Issuer = issuer_jsvalue.into_serde().unwrap();

    let attrs = [BLSScalar::from_u64(min_credit_score)];
    let bitmap = [reveal_min_credit_score];
    ac_verify::<BLSGt>(&issuer.public_key, &attrs, &bitmap, &proof).is_ok()
  }

  // Prove that the credit score meets the requirement
  pub fn prove_min_credit_score(proof_jsvalue: &JsValue,
                                issuer_jsvalue: &JsValue,
                                min_credit_score: u64,
                                reveal_min_credit_score: bool,
                                min_requirement: u64)
                                -> bool {
    if min_credit_score < min_requirement {
      return false;
    }
    Prover::verify_min_credit_score(proof_jsvalue,
                                    issuer_jsvalue,
                                    min_credit_score,
                                    reveal_min_credit_score)
  }
}

#[wasm_bindgen]
// Create a credit score credentialing secenario
// with the lower bound of the user's credit score and the minimum requirement
pub fn attest_credit_score(min_credit_score: u64, min_requirement: u64) -> bool {
  let mut issuer = Issuer::new(1);
  let issuer_jsvalue = issuer.jsvalue();
  let mut user = User::new(&issuer, "user");
  let user_jsvalue = user.jsvalue();

  let sig_jsvalue = issuer.sign_min_credit_score(&user_jsvalue, min_credit_score);
  let proof_jsvalue =
    user.commit_min_credit_score(&issuer_jsvalue, &sig_jsvalue, min_credit_score, true);

  Prover::prove_min_credit_score(&proof_jsvalue,
                                 &issuer_jsvalue,
                                 min_credit_score,
                                 true,
                                 min_requirement)
}

//
// Test section
//
// wasm-bindgen-test must be placed in the root of the crate or in a pub mod
// To test, run wasm-pack test --node in the wasm directory
//
// TODO (Keyao):
// 1. Make wasm_bindgen_test work in the wasm directory
//    It's currently not working due to dependency issue
// 2. Once it's working, convert the cargo tests below to wasm_bindgen_test
//

extern crate wasm_bindgen_test;
use wasm_bindgen_test::*;

// #[cfg(test)]
// mod tests {
//   use super::*;
//   use rand_chacha;
//   use zei::basic_crypto::signatures::XfrKeyPair;

//   // Test to ensure that define transaction is being constructed correctly
//   #[test]
//   fn test_wasm_define_transaction() {
//     let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

//     let keypair = XfrKeyPair::generate(&mut prng);
//     let txn = create_asset(&keypair,
//                            String::from("abcd"),
//                            String::from("test"),
//                            true,
//                            true);
//     assert!(txn.is_ok());
//   }
//   #[test]
//   // Test to ensure that issue transaction is being constructed correctly
//   fn test_wasm_issue_transaction() {
//     let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

//     let keypair = XfrKeyPair::generate(&mut prng);
//     let txn = issue_asset(&keypair, String::from(""), String::from("abcd"), 1, 5);
//     assert!(txn.is_ok());
//   }

//   #[test]
//   fn test_elgamal_serialization() {
//     let sk = generate_elgamal_secret_key();
//     let pk = derive_elgamal_public_key(sk);
//     assert!(pk.is_ok());
//   }
// }

#[wasm_bindgen_test]
// Test to ensure that credit score is checked correctly
fn test_credit_score_proof() {
  let mut issuer = Issuer::new(10);
  let issuer_jsvalue = issuer.jsvalue();
  let mut user = User::new(&issuer, "user");
  let user_jsvalue = user.jsvalue();

  let min_credit_score = 520;
  let fake_credit_score = 620;

  let sig_jsvalue = issuer.sign_min_credit_score(&user_jsvalue, min_credit_score);
  let proof_jsvalue =
    user.commit_min_credit_score(&issuer_jsvalue, &sig_jsvalue, min_credit_score, true);
  let fake_proof_jsvalue =
    user.commit_min_credit_score(&issuer_jsvalue, &sig_jsvalue, fake_credit_score, true);

  let requirement_low = 500;
  let requirement_high = 600;
  let incorrect_credit_score = 700;

  // Verify that prove_min_credit_score succeedes
  assert!(Prover::prove_min_credit_score(&proof_jsvalue,
                                         &issuer_jsvalue,
                                         min_credit_score,
                                         true,
                                         requirement_low));

  // Verify that prove_min_credit_score fails if:
  //  1. The lower bound of the credit score doesn't meet the requirement
  assert!(!Prover::prove_min_credit_score(&proof_jsvalue,
                                          &issuer_jsvalue,
                                          min_credit_score,
                                          true,
                                          requirement_high));

  // 2. The prover uses the incorrect credit score for the verification
  assert!(!Prover::prove_min_credit_score(&proof_jsvalue,
                                          &issuer_jsvalue,
                                          incorrect_credit_score,
                                          true,
                                          requirement_high));

  // 3. The user provides a fake proof
  assert!(!Prover::prove_min_credit_score(&fake_proof_jsvalue,
                                          &issuer_jsvalue,
                                          min_credit_score,
                                          true,
                                          requirement_high));

  // 4. reveal_min_credit_score isn't consistant
  assert!(!Prover::prove_min_credit_score(&proof_jsvalue,
                                          &issuer_jsvalue,
                                          min_credit_score,
                                          false,
                                          requirement_low));
}
