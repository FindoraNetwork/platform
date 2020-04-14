#![deny(warnings)]
use credentials::{
  CredCommitment, CredIssuerPublicKey, CredIssuerSecretKey, CredPoK, CredRevealSig, CredSignature,
  CredUserPublicKey, CredUserSecretKey, Credential as PlatformCredential,
};
use ledger::data_model::{
  TransferType as PlatformTransferType, TxOutput, TxoRef as PlatformTxoRef, TxoSID,
};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;
use zei::xfr::structs::{BlindAssetRecord, OwnerMemo as ZeiOwnerMemo};

#[wasm_bindgen]
pub struct TxoRef {
  pub(crate) txo_ref: PlatformTxoRef,
}

#[wasm_bindgen]
impl TxoRef {
  /// Creates a relative txo reference as a JSON string. Relative txo references are offset
  /// backwards from the operation they appear in -- 0 is the most recent, (n-1) is the first output
  /// of the transaction.
  ///
  /// Use relative txo indexing when referring to outputs of intermediate operations (e.g. a
  /// transaction containing both an issuance and a transfer).
  ///
  /// # Arguments
  /// @param {BigInt} idx -  Relative Txo (transaction output) SID.
  pub fn relative(idx: u64) -> Self {
    TxoRef { txo_ref: PlatformTxoRef::Relative(idx) }
  }

  /// Creates an absolute transaction reference as a JSON string.
  ///
  /// Use absolute txo indexing when referring to an output that has been assigned a utxo index (i.e.
  /// when the utxo has been committed to the ledger in an earlier transaction).
  ///
  /// # Arguments
  /// @param {BigInt} idx -  Txo (transaction output) SID.
  pub fn absolute(idx: u64) -> Self {
    TxoRef { txo_ref: PlatformTxoRef::Absolute(TxoSID(idx)) }
  }
}

impl TxoRef {
  pub fn get_txo(&self) -> &PlatformTxoRef {
    &self.txo_ref
  }
}

#[wasm_bindgen]
pub struct TransferType {
  transfer_type: PlatformTransferType,
}

#[wasm_bindgen]
impl TransferType {
  /// Standard TransferType variant for txn builder.
  /// Returns a token as a string signifying that the Standard policy should be used when evaluating the transaction.
  pub fn standard_transfer_type() -> Self {
    TransferType { transfer_type: PlatformTransferType::Standard }
  }

  /// Debt swap TransferType variant for txn builder.
  /// Returns a token as a string signifying that the DebtSwap policy should be used when evaluating the transaction.
  pub fn debt_transfer_type() -> Self {
    TransferType { transfer_type: PlatformTransferType::DebtSwap }
  }
}

impl TransferType {
  pub fn get_type(&self) -> &PlatformTransferType {
    &self.transfer_type
  }
}

#[wasm_bindgen]
pub struct ClientAssetRecord {
  pub(crate) output: TxOutput,
}

#[wasm_bindgen]
pub struct OwnerMemo {
  pub(crate) memo: ZeiOwnerMemo,
}

impl ClientAssetRecord {
  pub fn get_bar_ref(&self) -> &BlindAssetRecord {
    &self.output.0
  }
}

#[wasm_bindgen]
impl ClientAssetRecord {
  /// Builds a client record from an asset record fetched from the ledger server.
  /// @param {record} - JSON asset record fetched from server.
  pub fn from_json_record(record: &JsValue) -> Self {
    ClientAssetRecord { output: TxOutput(record.into_serde().unwrap()) }
  }
}

impl OwnerMemo {
  pub fn get_memo_ref(&self) -> &ZeiOwnerMemo {
    &self.memo
  }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct AttributeDefinition {
  pub name: String,
  pub size: usize,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct AttributeAssignment {
  pub name: String,
  pub val: String,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct CredentialUserKeyPair {
  pub(crate) pk: CredUserPublicKey,
  pub(crate) sk: CredUserSecretKey,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct CredentialIssuerKeyPair {
  pub(crate) pk: CredIssuerPublicKey,
  pub(crate) sk: CredIssuerSecretKey,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct CredentialSignature {
  pub(crate) sig: CredSignature,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct CredentialRevealSig {
  pub(crate) sig: CredRevealSig,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct CredentialCommitment {
  pub(crate) commitment: CredCommitment,
  pub(crate) pok: CredPoK,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Credential {
  pub(crate) credential: PlatformCredential,
}

impl CredentialCommitment {
  pub fn get_commitment_ref(&self) -> &CredCommitment {
    &self.commitment
  }
  pub fn get_pok_ref(&self) -> &CredPoK {
    &self.pok
  }
}

impl CredentialSignature {
  pub fn get_sig_ref(&self) -> &CredSignature {
    &self.sig
  }
}

impl Credential {
  pub fn get_cred_ref(&self) -> &PlatformCredential {
    &self.credential
  }
}

impl CredentialRevealSig {
  pub fn get_sig_ref(&self) -> &CredRevealSig {
    &self.sig
  }
}

#[wasm_bindgen]
impl CredentialIssuerKeyPair {
  pub fn get_pk(&self) -> CredIssuerPublicKey {
    self.pk.clone()
  }
  pub fn get_sk(&self) -> CredIssuerSecretKey {
    self.sk.clone()
  }
  pub fn to_jsvalue(&self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }
  pub fn from_jsvalue(val: &JsValue) -> Self {
    val.into_serde().unwrap()
  }
}

#[wasm_bindgen]
impl CredentialUserKeyPair {
  pub fn get_pk(&self) -> CredUserPublicKey {
    self.pk.clone()
  }
  pub fn get_sk(&self) -> CredUserSecretKey {
    self.sk.clone()
  }
  pub fn serialize(&self) -> String {
    serde_json::to_string(&self).unwrap()
  }
  pub fn to_jsvalue(&self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }
  pub fn from_jsvalue(val: &JsValue) -> Self {
    val.into_serde().unwrap()
  }
}
