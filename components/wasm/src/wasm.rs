// Interface for issuing transactions that can be compiled to Wasm.
// Allows web clients to issue transactions from a browser contexts.
// For now, forwards transactions to a ledger hosted locally.
// To compile wasm package, run wasm-pack build in the wasm directory
#![deny(warnings)]
extern crate ledger;
extern crate serde;
extern crate wasm_bindgen;
extern crate wasm_bindgen_test;
extern crate zei;
use bulletproofs::PedersenGens;
use cryptohash::sha256;
use hex;
use js_sys::Promise;
use ledger::data_model::{
  AccountAddress, AssetTypeCode, Operation, Serialized, TransferType, TxoRef, TxoSID,
};
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::{Deserialize, Serialize};
use std::str;
use txn_builder::{BuildsTransactions, TransactionBuilder, TransferOperationBuilder};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::future_to_promise;
use wasm_bindgen_futures::JsFuture;
use wasm_bindgen_test::*;
use web_sys::{Request, RequestInit, RequestMode};
use zei::algebra::ristretto::RistPoint;
use zei::api::anon_creds::{
  ac_keygen_issuer, ac_keygen_user, ac_reveal, ac_sign, ac_verify, ACIssuerPublicKey,
  ACIssuerSecretKey, ACRevealSig, ACSignature, ACUserPublicKey, ACUserSecretKey,
};
use zei::api::conf_cred_reveal::cac_gen_encryption_keys;
use zei::basic_crypto::elgamal::{elgamal_decrypt, elgamal_keygen, ElGamalPublicKey};
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{AssetIssuerPubKeys, AssetRecord, BlindAssetRecord, OpenAssetRecord};

const HOST: &str = "localhost";
const PORT: &str = "8668";
const SUBMISSION_PORT: &str = "8669";

/////////// TRANSACTION BUILDING ////////////////

//Random Helpers
#[wasm_bindgen]
pub fn create_txo_ref(idx: u64, relative: bool) -> String {
  let txo_ref;
  if relative {
    txo_ref = TxoRef::Relative(idx)
  } else {
    txo_ref = TxoRef::Absolute(TxoSID(idx))
  }
  serde_json::to_string(&txo_ref).unwrap()
}

#[wasm_bindgen]
pub fn create_blind_asset_record(amount: u64,
                                 code: String,
                                 pk: &XfrPublicKey,
                                 conf_amount: bool,
                                 conf_type: bool)
                                 -> Result<String, JsValue> {
  let params = PublicParams::new();
  let code = AssetTypeCode::new_from_base64(&code).map_err(|_e| {
      JsValue::from_str("Could not deserialize asset token code")})?;
  let mut small_rng = ChaChaRng::from_entropy();
  Ok(serde_json::to_string(&build_blind_asset_record(&mut small_rng,
                                                     &params.pc_gens,
                                                     &AssetRecord::new(amount, code.val, *pk).unwrap(),
                                                     conf_amount,
                                                     conf_type,
                                                     &None)).unwrap())
}

#[wasm_bindgen]
pub fn open_blind_asset_record(blind_asset_record: String,
                               key: &XfrKeyPair)
                               -> Result<String, JsValue> {
  let blind_asset_record = serde_json::from_str::<BlindAssetRecord>(&blind_asset_record).map_err(|_e| {
                             JsValue::from_str("Could not deserialize blind asset record")
                           })?;
  let open_asset_record = open_asset_record(&blind_asset_record, key.get_sk_ref()).map_err(|_e| JsValue::from_str("could not open asset record"))?;
  let bincode_encoded =
    bincode::serialize(&open_asset_record).map_err(|_e| {
                                            JsValue::from_str("could not encode open asset record")
                                          })?;
  Ok(base64::encode(&bincode_encoded))
}

// Wrapper around TransactionBuilder that does necessary serialization.
#[wasm_bindgen]
#[derive(Default)]
pub struct WasmTransactionBuilder {
  transaction_builder: Serialized<TransactionBuilder>,
}

#[wasm_bindgen]
impl WasmTransactionBuilder {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_operation_create_asset(&self,
                                    key_pair: &XfrKeyPair,
                                    memo: String,
                                    token_code: String,
                                    updatable: bool,
                                    traceable: bool)
                                    -> Result<WasmTransactionBuilder, JsValue> {
    let asset_token = AssetTypeCode::new_from_base64(&token_code).unwrap();

    Ok(WasmTransactionBuilder { transaction_builder: Serialized::new(&*self.transaction_builder.deserialize().add_operation_create_asset(&key_pair,
                                              Some(asset_token),
                                              updatable,
                                              traceable,
                                              &memo)
                  .map_err(|_e| JsValue::from_str("Could not build transaction"))?)})
  }

  pub fn add_operation_issue_asset(&self,
                                   key_pair: &XfrKeyPair,
                                   elgamal_pub_key: String,
                                   asset_token: String,
                                   seq_num: u64,
                                   amount: u64)
                                   -> Result<WasmTransactionBuilder, JsValue> {
    let asset_token = AssetTypeCode::new_from_base64(&asset_token).map_err(|_e| {
      JsValue::from_str("Could not deserialize asset token code")})?;

    let mut txn_builder = self.transaction_builder.deserialize();
    // construct asset tracking keys
    let issuer_keys;
    if elgamal_pub_key.is_empty() {
      issuer_keys = None
    } else {
      let pk = serde_json::from_str::<ElGamalPublicKey<RistPoint>>(&elgamal_pub_key).map_err(|_e| JsValue::from_str("could not deserialize elgamal key"))?;
      let mut small_rng = ChaChaRng::from_entropy();
      let (_, id_reveal_pub_key) = cac_gen_encryption_keys(&mut small_rng);
      issuer_keys = Some(AssetIssuerPubKeys { eg_ristretto_pub_key: pk,
                                              eg_blsg1_pub_key: id_reveal_pub_key });
    }

    Ok(WasmTransactionBuilder { transaction_builder: Serialized::new(&*txn_builder.add_basic_issue_asset(&key_pair,
                                            &issuer_keys,
                                            &asset_token,
                                            seq_num,
                                            amount).map_err(|_e| JsValue::from_str("could not build transaction"))?)})
  }

  pub fn add_operation(&mut self, op: String) -> Result<WasmTransactionBuilder, JsValue> {
    let op =
      serde_json::from_str::<Operation>(&op).map_err(|_e| {
                                              JsValue::from_str("Could not deserialize operation")
                                            })?;
    Ok(WasmTransactionBuilder { transaction_builder: Serialized::new(&*self.transaction_builder
                                                                           .deserialize()
                                                                           .add_operation(op)) })
  }

  pub fn transaction(&mut self) -> Result<String, JsValue> {
    Ok(self.transaction_builder
           .deserialize()
           .serialize_str()
           .map_err(|_e| JsValue::from_str("could not serialize transaction"))?)
  }
}

// Wrapper around TransferOperationBuilder that does necessary serialization.
#[wasm_bindgen]
#[derive(Default)]
pub struct WasmTransferOperationBuilder {
  op_builder: Serialized<TransferOperationBuilder>,
}
#[wasm_bindgen]
impl WasmTransferOperationBuilder {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_input(&mut self,
                   txo_ref: String,
                   oar: String,
                   amount: u64)
                   -> Result<WasmTransferOperationBuilder, JsValue> {
    let txo_sid =
      serde_json::from_str::<TxoRef>(&txo_ref).map_err(|_e| {
                                                JsValue::from_str("Could not deserialize txo sid")
                                              })?;
    let oar = serde_json::from_str::<OpenAssetRecord>(&oar).map_err(|_e| {
                             JsValue::from_str("Could not deserialize open asset record")
                           })?;
    Ok(WasmTransferOperationBuilder { op_builder:
                                        Serialized::new(&*self.op_builder
                                                              .deserialize()
                                                              .add_input(txo_sid, oar, amount)
                                                              .map_err(|e| {
                                                                JsValue::from_str(&format!("{}", e))
                                                              })?) })
  }

  pub fn add_output(&mut self,
                    amount: u64,
                    recipient: &XfrPublicKey,
                    code: String)
                    -> Result<WasmTransferOperationBuilder, JsValue> {
    let code = AssetTypeCode::new_from_base64(&code).map_err(|_e| {
      JsValue::from_str("Could not deserialize asset token code")})?;

    let new_builder = Serialized::new(&*self.op_builder
                                            .deserialize()
                                            .add_output(amount, recipient, code)
                                            .map_err(|e| JsValue::from_str(&format!("{}", e)))?);
    Ok(WasmTransferOperationBuilder { op_builder: new_builder })
  }

  pub fn balance(&mut self) -> Result<WasmTransferOperationBuilder, JsValue> {
    Ok(WasmTransferOperationBuilder { op_builder: Serialized::new(&*self.op_builder
                                                                        .deserialize()
                                                                        .balance().map_err(|_e| JsValue::from_str("Error balancing txn"))?) })
  }

  pub fn create(&mut self, transfer_type: String) -> Result<WasmTransferOperationBuilder, JsValue> {
    let transfer_type =
      serde_json::from_str::<TransferType>(&transfer_type).map_err(|_e| {
                                                JsValue::from_str("Could not deserialize transfer type")
                                              })?;
    let new_builder = Serialized::new(&*self.op_builder
                                            .deserialize()
                                            .create(transfer_type)
                                            .map_err(|e| JsValue::from_str(&format!("{}", e)))?);

    Ok(WasmTransferOperationBuilder { op_builder: new_builder })
  }

  pub fn sign(&mut self, kp: &XfrKeyPair) -> Result<WasmTransferOperationBuilder, JsValue> {
    let new_builder = Serialized::new(&*self.op_builder
                                            .deserialize()
                                            .sign(&kp)
                                            .map_err(|e| JsValue::from_str(&format!("{}", e)))?);

    Ok(WasmTransferOperationBuilder { op_builder: new_builder })
  }

  pub fn transaction(&self) -> Result<String, JsValue> {
    let transaction = self.op_builder
                          .deserialize()
                          .transaction()
                          .map_err(|e| JsValue::from_str(&format!("{}", e)))?;

    Ok(serde_json::to_string(&transaction).unwrap())
  }
}

///////////// CRYPTO //////////////////////

#[wasm_bindgen]
pub fn get_pub_key_str(key_pair: &XfrKeyPair) -> String {
  serde_json::to_string(key_pair.get_pk_ref()).unwrap()
}

#[wasm_bindgen]
pub fn get_priv_key_str(key_pair: &XfrKeyPair) -> String {
  serde_json::to_string(key_pair.get_sk_ref()).unwrap()
}

#[wasm_bindgen]
pub fn new_keypair() -> XfrKeyPair {
  let mut small_rng = rand::thread_rng();
  XfrKeyPair::generate(&mut small_rng)
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
pub fn generate_elgamal_keys() -> String {
  let mut small_rng = ChaChaRng::from_entropy();
  let pc_gens = PedersenGens::default();
  serde_json::to_string(&elgamal_keygen(&mut small_rng, &RistPoint(pc_gens.B))).unwrap()
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
  match txn_builder.add_operation_create_asset(&key_pair,
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
    let pk = serde_json::from_str::<ElGamalPublicKey<RistPoint>>(&elgamal_pub_key).map_err(|_e| JsValue::from_str("could not deserialize elgamal key"))?;
    let mut small_rng = ChaChaRng::from_entropy();
    let (_, id_reveal_pub_key) = cac_gen_encryption_keys(&mut small_rng);
    issuer_keys = Some(AssetIssuerPubKeys { eg_ristretto_pub_key: pk,
                                            eg_blsg1_pub_key: id_reveal_pub_key });
  }

  match txn_builder.add_basic_issue_asset(&key_pair, &issuer_keys, &asset_token, seq_num, amount) {
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
pub fn submit_transaction(transaction_str: String) -> Result<Promise, JsValue> {
  let mut opts = RequestInit::new();
  opts.method("POST");
  opts.mode(RequestMode::Cors);

  let req_string = format!("http://{}:{}/submit_transaction/{}",
                           HOST,
                           SUBMISSION_PORT,
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
pub fn get_txo(index: u64) -> Result<Promise, JsValue> {
  let mut opts = RequestInit::new();
  opts.method("GET");
  opts.mode(RequestMode::Cors);

  let req_string = format!("http://{}:{}/utxo_sid/{}", HOST, PORT, format!("{}", index));

  create_query_promise(&opts, &req_string)
}

#[wasm_bindgen]
// Get txo by index
pub fn get_asset_token(name: String) -> Result<Promise, JsValue> {
  let mut opts = RequestInit::new();
  opts.method("GET");
  opts.mode(RequestMode::Cors);

  let req_string = format!("http://{}:{}/asset_token/{}", HOST, PORT, name);

  create_query_promise(&opts, &req_string)
}

pub fn get_txn_status(handle: String) -> Result<Promise, JsValue> {
  let mut opts = RequestInit::new();
  opts.method("GET");
  opts.mode(RequestMode::Cors);

  let req_string = format!("http://{}:{}/txn_status/{}", HOST, PORT, handle);

  create_query_promise(&opts, &req_string)
}

// Given a request string and a request init object, constructs
// the JS promise to be returned to the client
fn create_query_promise(opts: &RequestInit, req_string: &str) -> Result<Promise, JsValue> {
  let request = Request::new_with_str_and_init(&req_string, &opts)?;
  let window = web_sys::window().unwrap();
  let request_promise = window.fetch_with_request(&request);
  Ok(future_to_promise(JsFuture::from(request_promise)))
}

//
// Credentialing section
//

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct Issuer {
  public_key: ACIssuerPublicKey,
  secret_key: ACIssuerSecretKey,
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
    let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_>(&mut prng, num_attr);

    Issuer { public_key: issuer_pk,
             secret_key: issuer_sk }
  }

  // Convert an Issuer to JsValue
  pub fn jsvalue(&mut self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }

  // Sign an attribute
  // E.g. sign the lower bound of the credit score
  pub fn sign_attribute(&self, user_jsvalue: &JsValue, attribute: u64) -> JsValue {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([0u8; 32]);
    let user: User = user_jsvalue.into_serde().unwrap();

    let attrs = [attribute.to_le_bytes()];
    let sig = ac_sign(&mut prng, &self.secret_key, &user.public_key, &attrs);

    JsValue::from_serde(&sig).unwrap()
  }
}

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct User {
  public_key: ACUserPublicKey,
  secret_key: ACUserSecretKey,
}

#[wasm_bindgen]
impl User {
  // Create a new user, generating the key pair using the issuer's public key
  pub fn new(issuer: &Issuer, rand_seed: &str) -> User {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([rand_seed.as_bytes()[0]; 32]);
    let (user_pk, user_sk) = ac_keygen_user::<_>(&mut prng, &issuer.public_key);

    User { public_key: user_pk,
           secret_key: user_sk }
  }

  // Convert a User to JsValue
  pub fn jsvalue(&mut self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }

  // Commit an attribute with the issuer's signature
  // E.g. commit the lower bound of the credit score
  pub fn commit_attribute(&self,
                          issuer_jsvalue: &JsValue,
                          sig: &JsValue,
                          attribute: u64,
                          reveal_attribute: bool)
                          -> JsValue {
    let issuer: Issuer = issuer_jsvalue.into_serde().unwrap();
    let sig: ACSignature = sig.into_serde().unwrap();
    let mut prng = ChaChaRng::from_seed([0u8; 32]);

    let attrs = [attribute.to_le_bytes()];
    let bitmap = [reveal_attribute];

    let proof = ac_reveal(&mut prng,
                          &self.secret_key,
                          &issuer.public_key,
                          &sig,
                          &attrs,
                          &bitmap).unwrap();

    JsValue::from_serde(&proof).unwrap()
  }
}

#[wasm_bindgen]
#[derive(PartialEq)]
pub enum RequirementType {
  // Requirement: attribute value == requirement
  Equal = 0,

  // Requirement: attribute value >= requirement
  AtLeast = 1,
}

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct Prover;

#[wasm_bindgen]
impl Prover {
  // Prove that an attribute meets the requirement and is true
  pub fn prove_attribute(proof_jsvalue: &JsValue,
                         issuer_jsvalue: &JsValue,
                         attribute: u64,
                         reveal_attribute: bool,
                         requirement: u64,
                         requirement_type: RequirementType)
                         -> bool {
    // 1. Prove that the attribut meets the requirement
    match requirement_type {
      //    Case 1. "Equal" requirement
      //    E.g. prove that the country code is the same as the requirement
      RequirementType::Equal => {
        if attribute != requirement {
          return false;
        }
      }
      //    Case 2. "AtLeast" requirement
      //    E.g. prove that the credit score is at least the required value
      RequirementType::AtLeast => {
        if attribute < requirement {
          return false;
        }
      }
    }

    // 2. Prove that the attribute is true
    //    E.g. verify the lower bound of the credit score
    let issuer: Issuer = issuer_jsvalue.into_serde().unwrap();
    let attrs = [attribute.to_le_bytes()];
    let bitmap = [reveal_attribute];
    let proof: ACRevealSig = proof_jsvalue.into_serde().unwrap();
    ac_verify(&issuer.public_key, &attrs, &bitmap, &proof).is_ok()
  }
}

#[wasm_bindgen]
// Create a proving secenario
pub fn get_proof(attribute: u64) -> JsValue {
  let mut issuer = Issuer::new(1);
  let issuer_jsvalue = issuer.jsvalue();
  let mut user = User::new(&issuer, "user");
  let user_jsvalue = user.jsvalue();

  let sig_jsvalue = issuer.sign_attribute(&user_jsvalue, attribute);
  user.commit_attribute(&issuer_jsvalue, &sig_jsvalue, attribute, true)
}

// In the P2P Lending app, the user has the option to save the proof for future use
// 1. If the proof exists, use attest_with_proof for credentialing
// 2. Else, use attest_without_proof for credentialing

#[wasm_bindgen]
// 1. Create a credentialing secenario with proof as an input
pub fn attest_with_proof(attribute: u64,
                         requirement: u64,
                         requirement_type: RequirementType,
                         proof_jsvalue: JsValue)
                         -> bool {
  Prover::prove_attribute(&proof_jsvalue,
                          &Issuer::new(1).jsvalue(),
                          attribute,
                          true,
                          requirement,
                          requirement_type)
}

#[wasm_bindgen]
// 2. Create a credentialing secenario without proof as an input
pub fn attest_without_proof(attribute: u64,
                            requirement: u64,
                            requirement_type: RequirementType)
                            -> bool {
  let mut issuer = Issuer::new(1);
  let issuer_jsvalue = issuer.jsvalue();
  let mut user = User::new(&issuer, "user");
  let user_jsvalue = user.jsvalue();

  let sig_jsvalue = issuer.sign_attribute(&user_jsvalue, attribute);
  let proof_jsvalue = user.commit_attribute(&issuer_jsvalue, &sig_jsvalue, attribute, true);

  Prover::prove_attribute(&proof_jsvalue,
                          &issuer_jsvalue,
                          attribute,
                          true,
                          requirement,
                          requirement_type)
}

//
// Test section
//
// wasm-bindgen-test must be placed in the root of the crate or in a pub mod
// To test, run wasm-pack test --node in the wasm directory
//

// Test to ensure that define transaction is being constructed correctly
#[wasm_bindgen_test]
fn test_wasm_define_transaction() {
  let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

  let keypair = XfrKeyPair::generate(&mut prng);
  let txn = create_asset(&keypair,
                         String::from("abcd"),
                         String::from("test"),
                         true,
                         true);
  assert!(txn.is_ok());
}

#[wasm_bindgen_test]
// Test to ensure that "Equal" requirement is checked correctly
// E.g. citizenship requirement
fn test_citizenship_proof() {
  let mut issuer = Issuer::new(10);
  let issuer_jsvalue = issuer.jsvalue();
  let mut user = User::new(&issuer, "user");
  let user_jsvalue = user.jsvalue();

  let citizenship_code = 1;
  let fake_citizenship_code = 86;

  let sig_jsvalue = issuer.sign_attribute(&user_jsvalue, citizenship_code);
  let proof_jsvalue = user.commit_attribute(&issuer_jsvalue, &sig_jsvalue, citizenship_code, true);
  let fake_proof_jsvalue =
    user.commit_attribute(&issuer_jsvalue, &sig_jsvalue, fake_citizenship_code, true);

  let requirement_usa = 1;
  let requirement_china = 86;
  let incorrect_credit_score = 11;

  // Verify that prove_attribute succeedes
  assert!(Prover::prove_attribute(&proof_jsvalue,
                                  &issuer_jsvalue,
                                  citizenship_code,
                                  true,
                                  requirement_usa,
                                  RequirementType::Equal));

  // Verify that prove_attribute fails if:
  //  1. The attribute doesn't equal the requirement
  assert!(!Prover::prove_attribute(&proof_jsvalue,
                                   &issuer_jsvalue,
                                   citizenship_code,
                                   true,
                                   requirement_china,
                                   RequirementType::Equal));

  // 2. The prover uses the incorrect credit score for the verification
  assert!(!Prover::prove_attribute(&proof_jsvalue,
                                   &issuer_jsvalue,
                                   incorrect_credit_score,
                                   true,
                                   requirement_usa,
                                   RequirementType::Equal));

  // 3. The user provides a fake proof
  assert!(!Prover::prove_attribute(&fake_proof_jsvalue,
                                   &issuer_jsvalue,
                                   citizenship_code,
                                   true,
                                   requirement_china,
                                   RequirementType::Equal));

  // 4. reveal_min_credit_score isn't consistant
  assert!(!Prover::prove_attribute(&proof_jsvalue,
                                   &issuer_jsvalue,
                                   citizenship_code,
                                   false,
                                   requirement_usa,
                                   RequirementType::Equal));
}

#[wasm_bindgen_test]
// Test to ensure that issue transaction is being constructed correctly
fn test_wasm_issue_transaction() {
  let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

  let keypair = XfrKeyPair::generate(&mut prng);
  let txn = issue_asset(&keypair, String::from(""), String::from("abcd"), 1, 5);
  assert!(txn.is_ok());
}

#[wasm_bindgen_test]
// Test to ensure that "AtLeast" requirement is checked correctly
// E.g. minimun credit score requirement
fn test_min_credit_score_proof() {
  let mut issuer = Issuer::new(10);
  let issuer_jsvalue = issuer.jsvalue();
  let mut user = User::new(&issuer, "user");
  let user_jsvalue = user.jsvalue();

  let min_credit_score = 520;
  let fake_credit_score = 620;

  let sig_jsvalue = issuer.sign_attribute(&user_jsvalue, min_credit_score);
  let proof_jsvalue = user.commit_attribute(&issuer_jsvalue, &sig_jsvalue, min_credit_score, true);
  let fake_proof_jsvalue =
    user.commit_attribute(&issuer_jsvalue, &sig_jsvalue, fake_credit_score, true);

  let requirement_low = 500;
  let requirement_high = 600;
  let incorrect_credit_score = 700;

  // Verify that prove_attribute succeedes
  assert!(Prover::prove_attribute(&proof_jsvalue,
                                  &issuer_jsvalue,
                                  min_credit_score,
                                  true,
                                  requirement_low,
                                  RequirementType::AtLeast));

  // Verify that prove_attribute fails if:
  //  1. The lower bound of the credit score doesn't meet the requirement
  assert!(!Prover::prove_attribute(&proof_jsvalue,
                                   &issuer_jsvalue,
                                   min_credit_score,
                                   true,
                                   requirement_high,
                                   RequirementType::AtLeast));

  // 2. The prover uses the incorrect credit score for the verification
  assert!(!Prover::prove_attribute(&proof_jsvalue,
                                   &issuer_jsvalue,
                                   incorrect_credit_score,
                                   true,
                                   requirement_low,
                                   RequirementType::AtLeast));

  // 3. The user provides a fake proof
  assert!(!Prover::prove_attribute(&fake_proof_jsvalue,
                                   &issuer_jsvalue,
                                   min_credit_score,
                                   true,
                                   requirement_high,
                                   RequirementType::AtLeast));

  // 4. reveal_min_credit_score isn't consistant
  assert!(!Prover::prove_attribute(&proof_jsvalue,
                                   &issuer_jsvalue,
                                   min_credit_score,
                                   false,
                                   requirement_low,
                                   RequirementType::AtLeast));
}
