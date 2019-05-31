use chrono::prelude::*;
use curve25519_dalek::ristretto::CompressedRistretto;
use rand::rngs::SmallRng;
use rand::{CryptoRng, FromEntropy, Rng};
use std::boxed::Box;
use std::collections::HashMap;
use std::convert::TryFrom;
use zei::basic_crypto::signatures::{XfrKeyPair, XfrPublicKey, XfrSecretKey, XfrSignature};
use zei::xfr::lib::gen_xfr_note;
use zei::xfr::structs::{AssetRecord, BlindAssetRecord, OpenAssetRecord, XfrNote};
pub mod errors;

pub const TXN_SEQ_ID_PLACEHOLDER: u64 = 0xD000_0000_0000_0000u64;

// Unique Identifier for AssetTokens
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct AssetTokenCode {
  // User-supplied code, system guarantees uniqueness
  pub val: [u8; 16],
}

impl AssetTokenCode {
  pub fn gen_random() -> AssetTokenCode {
    let mut small_rng = SmallRng::from_entropy();
    let mut buf: [u8; 16] = [0u8; 16];
    small_rng.fill(&mut buf);
    AssetTokenCode { val: buf }
  }
  pub fn new_from_str(s: &str) -> AssetTokenCode {
    let mut as_vec = s.to_string().into_bytes();
    as_vec.resize(16, 0u8);
    let buf = <[u8; 16]>::try_from(as_vec.as_slice()).unwrap();
    AssetTokenCode { val: buf }
  }
}

#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct AssetDigest {
  // Generated from the asset definition, also unique
  pub val: [u8; 32],
}

// TODO: Define Memo
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct Memo;
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct ConfidentialMemo;
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct Commitment([u8; 32]);

#[derive(Default, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct XfrAddress {
  pub key: XfrPublicKey,
}

#[derive(Default, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct IssuerPublicKey {
  pub key: XfrPublicKey,
}

#[derive(Default, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct AccountAddress {
  pub key: XfrPublicKey,
}

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct SignedAddress {
  pub address: XfrAddress,
  pub signature: XfrSignature,
}

impl SignedAddress {
  pub fn verify(&self, message: &[u8]) -> bool {
    self.address.key.verify(message, &self.signature).is_ok()
  }
}

#[derive(Default, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Asset {
  pub code: AssetTokenCode,
  pub issuer: IssuerPublicKey,
  pub memo: Memo,
  pub confidential_memo: ConfidentialMemo,
  pub updatable: bool,
}

#[derive(Default, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct AssetToken {
  pub properties: Asset,
  pub digest: [u8; 32],
  pub units: u64,
  pub confidential_units: Commitment,
}

//impl AssetToken {
//    pub fn create_empty() -> AssetToken {
//        AssetToken {
//            code: AssetTokenCode{val:[0;16]},
//            digest: [0;32],
//            issuer: Address{key:[0;32]},
//            memo: Memo{},
//            confidential_memo: ConfidentialMemo{},
//            updatable: false,
//            units: 0,
//            confidential_units: [0;32],
//        }
//    }
//}

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct AssetPolicyKey([u8; 16]);

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct CustomAssetPolicy {
  policy: Vec<u8>, // serialized policy, underlying form TBD.
}

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct CredentialProofKey([u8; 16]);

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct CredentialProof {
  pub key: CredentialProofKey,
}

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct SmartContractKey([u8; 16]);

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct SmartContract;

//TODO(Kevin): define types
#[derive(Clone, Serialize, Deserialize)]
pub struct Variable;

#[derive(Default, Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct TxoSID {
  pub(crate) index: u64,
}

#[derive(Default, Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct TxnSID {
  pub(crate) index: u64,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct AssetSpecification {
  pub code: AssetTokenCode,
  pub amount: u64,
}

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct PrivateAssetSpecification {
  amount_commitment: Option<CompressedRistretto>,
  asset_type_commitment: Option<CompressedRistretto>,
}

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum AssetType {
  // TODO: need to not include amount/ammount_commitment in zei type generation
  Normal(AssetSpecification),
  Private(PrivateAssetSpecification),
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum TxOutput {
  BlindAssetRecord(BlindAssetRecord),
} // needs to be a generic view on an Operation, specifying one output record of a specific type...

#[derive(Eq, PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct Utxo {
  // digest is a hash of the TxoSID and the operation output
  pub digest: [u8; 32],
  pub output: TxOutput,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AssetTransferBody {
  //pub nonce: u128,
  pub inputs: Vec<TxoSID>,    // ledger address of inputs
  pub outputs: Vec<TxoSID>,   // computed in check?
  pub transfer: Box<XfrNote>, //TODO: ZEI. XfrNote,
}

impl AssetTransferBody {
  pub fn new<R: CryptoRng + Rng>(prng: &mut R,
                                 input_sids: Vec<TxoSID>,
                                 input_records: &[OpenAssetRecord],
                                 output_records: &[AssetRecord],
                                 input_keys: &[XfrKeyPair],
                                 offset: &mut u64)
                                 -> Result<AssetTransferBody, errors::PlatformError> {
    let id_proofs = vec![];
    let note = Box::new(gen_xfr_note(prng, input_records, output_records, input_keys, &id_proofs).or_else(|_| Err(errors::PlatformError::ZeiError))?);
    let mut txos = Vec::new();
    txos.resize_with(output_records.len(), || {
          let tmp = *offset;
          *offset += 1;
          TxoSID { index: tmp }
        });
    Ok(AssetTransferBody { inputs: input_sids,
                           outputs: txos,
                           transfer: note })
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AssetIssuanceBody {
  pub code: AssetTokenCode,
  pub seq_num: u64,
  pub outputs: Vec<TxoSID>,
  pub records: Vec<TxOutput>,
}

impl AssetIssuanceBody {
  pub fn new(token_code: &AssetTokenCode,
             seq_num: u64,
             records: &[TxOutput],
             offset: &mut u64)
             -> Result<AssetIssuanceBody, errors::PlatformError> {
    let mut txos = Vec::new();
    txos.resize_with(records.len(), || {
          let tmp = *offset;
          *offset += 1;
          TxoSID { index: tmp }
        });
    Ok(AssetIssuanceBody { code: *token_code,
                           seq_num,
                           outputs: txos,
                           records: records.to_vec() })
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AssetCreationBody {
  pub asset: Asset,
}

impl AssetCreationBody {
  pub fn new(token_code: &AssetTokenCode,
             issuer_key: &IssuerPublicKey, // TODO: require private key check somehow?
             updatable: bool,
             memo: Option<Memo>,
             confidential_memo: Option<ConfidentialMemo>)
             -> Result<AssetCreationBody, errors::PlatformError> {
    let mut asset_def: Asset = Default::default();
    asset_def.code = *token_code;
    asset_def.issuer = *issuer_key;
    asset_def.updatable = updatable;

    if memo.is_some() {
      asset_def.memo = *memo.as_ref().unwrap();
    } else {
      asset_def.memo = Memo {};
    }

    if confidential_memo.is_some() {
      asset_def.confidential_memo = *confidential_memo.as_ref().unwrap();
    } else {
      asset_def.confidential_memo = ConfidentialMemo {};
    }
    Ok(AssetCreationBody { asset: asset_def })
  }
}

fn compute_signature<T>(secret_key: &XfrSecretKey,
                        public_key: &XfrPublicKey,
                        operation_body: &T)
                        -> XfrSignature
  where T: serde::Serialize
{
  secret_key.sign(&serde_json::to_vec(&operation_body).unwrap(), &public_key)
}

// TODO: UTXO Addresses must be included in Transfer Signature
#[derive(Debug, Serialize, Deserialize)]
pub struct AssetTransfer {
  //pub nonce: u128,
  pub body: AssetTransferBody,
  pub body_signatures: Vec<SignedAddress>, // not yet supported
}

impl AssetTransfer {
  pub fn new(transfer_body: AssetTransferBody) -> Result<AssetTransfer, errors::PlatformError> {
    Ok(AssetTransfer { body: transfer_body,
                       body_signatures: Vec::new() })
  }
}

//Tells the storage layer what to do the list of active asset records (i.e. UTXO set)
#[derive(Debug, Serialize, Deserialize)]
pub struct AssetTransferResult {
  pub success: bool,
}

// TODO: Include mechanism for replay attacks
#[derive(Debug, Serialize, Deserialize)]
pub struct AssetIssuance {
  pub body: AssetIssuanceBody,
  pub pubkey: IssuerPublicKey,
  pub signature: XfrSignature,
}

impl AssetIssuance {
  pub fn new(issuance_body: AssetIssuanceBody,
             public_key: &IssuerPublicKey,
             secret_key: &XfrSecretKey)
             -> Result<AssetIssuance, errors::PlatformError> {
    let sign = compute_signature(&secret_key, &public_key.key, &issuance_body);
    Ok(AssetIssuance { body: issuance_body,
                       pubkey: *public_key,
                       signature: sign })
  }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AssetIssuanceResult {
  pub success: bool,
}

// ... etc...
#[derive(Debug, Serialize, Deserialize)]
pub struct AssetCreation {
  pub body: AssetCreationBody,
  pub pubkey: IssuerPublicKey,
  pub signature: XfrSignature,
}

impl AssetCreation {
  pub fn new(creation_body: AssetCreationBody,
             public_key: &IssuerPublicKey,
             secret_key: &XfrSecretKey)
             -> Result<AssetCreation, errors::PlatformError> {
    let sign = compute_signature(&secret_key, &public_key.key, &creation_body);
    Ok(AssetCreation { body: creation_body,
                       pubkey: *public_key,
                       signature: sign })
  }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AssetCreationResult {
  pub success: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Operation {
  AssetTransfer(AssetTransfer),
  AssetIssuance(AssetIssuance),
  AssetCreation(AssetCreation),
  // ... etc...
}

#[derive(Debug, Serialize, Deserialize)]
pub enum OperationResult {
  AssetTransferResult(AssetTransferResult),
  AssetIssuanceResult(AssetIssuanceResult),
  AssetCreationResult(AssetCreationResult),
  // ... etc...
}

#[derive(Debug)]
pub struct TimeBounds {
  pub start: DateTime<Utc>,
  pub end: DateTime<Utc>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Transaction {
  pub operations: Vec<Operation>,
  pub variable_utxos: Vec<TxoSID>, // TODO: precondition support
  pub credentials: Vec<CredentialProof>,
  pub memos: Vec<Memo>,
  pub sid: TxoSID,
  pub outputs: u64,
  //pub time_bounds: TimeBounds,
  // ... etc...
}

impl Transaction {
  pub fn add_operation(&mut self, op: Operation) {
    self.operations.push(op);
  }
}

impl Default for Transaction {
  fn default() -> Self {
    Transaction { operations: Vec::new(),
                  variable_utxos: Vec::new(),
                  credentials: Vec::new(),
                  memos: Vec::new(),
                  sid: TxoSID { index: TXN_SEQ_ID_PLACEHOLDER },
                  outputs: 0 }
  }
}

pub struct TransactionResult {
  pub op_results: Vec<OperationResult>,
}

#[derive(Default, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct AccountID {
  pub val: String,
}

#[derive(Default, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Account {
  pub id: AccountID,
  pub access_control_list: Vec<AccountAddress>,
  pub key_value: HashMap<String, String>, //key value storage...
}
