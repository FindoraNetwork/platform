use super::errors;
use base64::decode as b64dec;
use base64::encode as b64enc;
use chrono::prelude::*;
use rand::rngs::SmallRng;
use rand::{FromEntropy, Rng};
use rand_core::{CryptoRng, RngCore};
use std::boxed::Box;
use std::collections::HashMap;
use std::convert::TryFrom;
use zei::xfr::lib::gen_xfr_note;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey, XfrSignature};
use zei::xfr::structs::{AssetRecord, BlindAssetRecord, OpenAssetRecord, XfrNote};

// Unique Identifier for ledger objects
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Code {
  pub val: [u8; 16],
}

pub type AssetTypeCode = Code;
pub type AssetPolicyKey = Code;
pub type SmartContractKey = Code;

impl Code {
  pub fn gen_random() -> Self {
    let mut small_rng = SmallRng::from_entropy();
    let mut buf: [u8; 16] = [0u8; 16];
    small_rng.fill(&mut buf);
    Self { val: buf }
  }
  pub fn new_from_str(s: &str) -> Self {
    let mut as_vec = s.to_string().into_bytes();
    as_vec.resize(16, 0u8);
    let buf = <[u8; 16]>::try_from(as_vec.as_slice()).unwrap();
    Self { val: buf }
  }
  pub fn new_from_base64(b64: &str) -> Result<Self, errors::PlatformError> {
    if let Ok(mut bin) = b64dec(b64) {
      bin.resize(16, 0u8);
      let buf = <[u8; 16]>::try_from(bin.as_slice()).unwrap();
      Ok(Self { val: buf })
    } else {
      Err(errors::PlatformError::DeserializationError)
    }
  }
  pub fn to_base64(&self) -> String {
    b64enc(&self.val)
  }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct AssetDigest {
  // Generated from the asset definition, also unique
  pub val: [u8; 32],
}

// TODO: Define Memo
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Memo(pub String);
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ConfidentialMemo;
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Commitment([u8; 32]);

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct XfrAddress {
  pub key: XfrPublicKey,
}

// TODO(joe): Better name! There's more than one thing that gets issued.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct IssuerPublicKey {
  pub key: XfrPublicKey,
  // TODO(joe): possibly include other keys, pending zei interface updates.
  // eg. encryption key
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct AccountAddress {
  pub key: XfrPublicKey,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SignedAddress {
  pub address: XfrAddress,
  pub signature: XfrSignature,
}

impl SignedAddress {
  pub fn verify(&self, message: &[u8]) -> bool {
    self.address.key.verify(message, &self.signature).is_ok()
  }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct Asset {
  pub code: AssetTypeCode,
  pub issuer: IssuerPublicKey,
  pub memo: Memo,
  pub confidential_memo: ConfidentialMemo,
  pub updatable: bool,
  pub traceable: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct AssetType {
  pub properties: Asset,
  pub digest: [u8; 32],
  pub units: u64,
  pub confidential_units: Commitment,
}

//impl AssetType {
//    pub fn create_empty() -> AssetType {
//        AssetType {
//            code: AssetTypeCode{val:[0;16]},
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

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CustomAssetPolicy {
  policy: Vec<u8>, // serialized policy, underlying form TBD.
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CredentialProofKey([u8; 16]);

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CredentialProof {
  pub key: CredentialProofKey,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct SmartContract;

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct TxoSID(pub u64);

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct TxnSID(pub usize);

// An ephemeral index for a transaction (with a different newtype so that
// it's harder to mix up)
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct TxnTempSID(pub usize);

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TxOutput(pub BlindAssetRecord);

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Utxo(pub TxOutput);
// TODO(joe): the digest is currently unused -- should it be put back?
// pub struct Utxo {
//   // digest is a hash of the TxoSID and the operation output
//   pub digest: [u8; 32],
//   pub output: TxOutput,
// }

#[derive(Copy, Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum TxoRef {
  // Offset backwards from this operation (within a txn) -- 0 is the most recent, (n-1) (if there
  // are n outputs so far) is the first output of the transaction
  Relative(u64),
  // Absolute Txo address to a location outside this txn
  Absolute(TxoSID),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TransferAssetBody {
  pub inputs: Vec<TxoRef>, // Ledger address of inputs
  pub num_outputs: usize,  // How many output TXOs?
  // TODO(joe): we probably don't need the whole XfrNote with input records
  // once it's on the chain
  pub transfer: Box<XfrNote>, // Encrypted transfer note
}

impl TransferAssetBody {
  pub fn new<R: CryptoRng + RngCore>(prng: &mut R,
                                     input_refs: Vec<TxoRef>,
                                     input_records: &[OpenAssetRecord],
                                     output_records: &[AssetRecord],
                                     input_keys: &[XfrKeyPair])
                                     -> Result<TransferAssetBody, errors::PlatformError> {
    let id_proofs = vec![];
    if input_records.is_empty() {
      return Err(errors::PlatformError::InputsError);
    }
    let note = Box::new(gen_xfr_note(prng, input_records, output_records, input_keys, &id_proofs)?);
    Ok(TransferAssetBody { inputs: input_refs,
                           num_outputs: output_records.len(),
                           transfer: note })
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct IssueAssetBody {
  pub code: AssetTypeCode,
  pub seq_num: u64,
  pub num_outputs: usize,
  pub records: Vec<TxOutput>,
}

impl IssueAssetBody {
  pub fn new(token_code: &AssetTypeCode,
             seq_num: u64,
             records: &[TxOutput])
             -> Result<IssueAssetBody, errors::PlatformError> {
    Ok(IssueAssetBody { code: *token_code,
                        seq_num,
                        num_outputs: records.len(),
                        records: records.to_vec() })
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct DefineAssetBody {
  pub asset: Asset,
}

impl DefineAssetBody {
  pub fn new(token_code: &AssetTypeCode,
             issuer_key: &IssuerPublicKey, // TODO: require private key check somehow?
             updatable: bool,
             traceable: bool,
             memo: Option<Memo>,
             confidential_memo: Option<ConfidentialMemo>)
             -> Result<DefineAssetBody, errors::PlatformError> {
    let mut asset_def: Asset = Default::default();
    asset_def.code = *token_code;
    asset_def.issuer = *issuer_key;
    asset_def.updatable = updatable;
    asset_def.traceable = traceable;

    if let Some(memo) = memo {
      asset_def.memo = Memo(memo.0);
    } else {
      asset_def.memo = Memo(String::from(""));
    }

    if let Some(confidential_memo) = confidential_memo {
      asset_def.confidential_memo = confidential_memo;
    } else {
      asset_def.confidential_memo = ConfidentialMemo {};
    }
    Ok(DefineAssetBody { asset: asset_def })
  }
}

pub fn compute_signature<T>(secret_key: &XfrSecretKey,
                            public_key: &XfrPublicKey,
                            operation_body: &T)
                            -> XfrSignature
  where T: serde::Serialize
{
  secret_key.sign(&serde_json::to_vec(&operation_body).unwrap(), &public_key)
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum TransferType {
  Standard,
  DebtSwap,
}

// TODO: UTXO Addresses must be included in Transfer Signature
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TransferAsset {
  pub body: TransferAssetBody,
  pub transfer_type: TransferType,
  pub body_signatures: Vec<SignedAddress>,
}

impl TransferAsset {
  pub fn new(transfer_body: TransferAssetBody,
             input_keys: &[&XfrKeyPair],
             transfer_type: TransferType)
             -> Result<TransferAsset, errors::PlatformError> {
    let mut body_signatures = Vec::new();

    for key in input_keys {
      let sig = key.get_sk_ref()
                   .sign(&serde_json::to_vec(&transfer_body).unwrap(),
                         key.get_pk_ref());

      body_signatures.push(SignedAddress { signature: sig,
                                           address: XfrAddress { key: *key.get_pk_ref() } });
    }

    Ok(TransferAsset { body: transfer_body,
                       body_signatures,
                       transfer_type })
  }
}

// TODO: Include mechanism for replay attacks
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct IssueAsset {
  pub body: IssueAssetBody,
  pub pubkey: IssuerPublicKey,
  pub signature: XfrSignature,
}

impl IssueAsset {
  pub fn new(issuance_body: IssueAssetBody,
             public_key: &IssuerPublicKey,
             secret_key: &XfrSecretKey)
             -> Result<IssueAsset, errors::PlatformError> {
    let sign = compute_signature(&secret_key, &public_key.key, &issuance_body);
    Ok(IssueAsset { body: issuance_body,
                    pubkey: *public_key,
                    signature: sign })
  }
}

// ... etc...
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct DefineAsset {
  pub body: DefineAssetBody,

  // TODO(joe?): Why is there a distinct public key used for signing?
  // Should this be the same as the issuer key in `body`? Is it *dangerous*
  // to have a distinct public key for this? Is it *beneficial* to have a
  // distinct public key?
  pub pubkey: IssuerPublicKey,
  pub signature: XfrSignature,
}

impl DefineAsset {
  pub fn new(creation_body: DefineAssetBody,
             public_key: &IssuerPublicKey,
             secret_key: &XfrSecretKey)
             -> Result<DefineAsset, errors::PlatformError> {
    let sign = compute_signature(&secret_key, &public_key.key, &creation_body);
    Ok(DefineAsset { body: creation_body,
                     pubkey: *public_key,
                     signature: sign })
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Operation {
  TransferAsset(TransferAsset),
  IssueAsset(IssueAsset),
  DefineAsset(DefineAsset),
  // ... etc...
}

#[derive(Clone, Debug)]
pub struct TimeBounds {
  pub start: DateTime<Utc>,
  pub end: DateTime<Utc>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Transaction {
  pub operations: Vec<Operation>,
  pub credentials: Vec<CredentialProof>,
  pub memos: Vec<Memo>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FinalizedTransaction {
  pub txn: Transaction,
  pub tx_id: TxnSID,
  pub merkle_id: u64,
}

impl Transaction {
  pub fn add_operation(&mut self, op: Operation) {
    self.operations.push(op);
  }

  pub fn serialize_bincode(&self, sid: TxnSID) -> Vec<u8> {
    let mut serialized = bincode::serialize(&self).unwrap();
    serialized.extend(bincode::serialize(&sid).unwrap());
    serialized
  }
}

impl Default for Transaction {
  fn default() -> Self {
    Transaction { operations: Vec::new(),
                  credentials: Vec::new(),
                  memos: Vec::new() }
  }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct AccountID {
  pub val: String,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct Account {
  pub id: AccountID,
  pub access_control_list: Vec<AccountAddress>,
  pub key_value: HashMap<String, String>, //key value storage...
}

#[cfg(test)]
mod tests {
  use super::*;
  use rand_core::SeedableRng;
  use std::cmp::min;
  use zei::xfr::structs::{AssetAmountProof, XfrBody, XfrProofs};

  #[test]
  fn test_gen_random() {
    let mut sum: u64 = 0;
    let mut sample_size = 0;

    for _ in 0..1000 {
      let code = AssetTypeCode::gen_random();
      let mut failed = true;

      for byte in code.val.iter() {
        if *byte != 0 {
          failed = false;
        }

        sum += *byte as u64;
        sample_size += 1;
      }

      assert!(!failed);
    }

    // Use the central limit theorem.  The standard deviation of the
    // sample mean should be normal(127.5, uniform variance).  Work
    // from the standard deviation of uniform(0, 1), sqrt(1/12).  The
    // expected average (mu) is 127.5 if the random number generator
    // is unbiased.
    let uniform_stddev = 1.0 / (12.0 as f64).sqrt();
    let average = sum as f64 / sample_size as f64;
    let stddev = (uniform_stddev * 255.0) / (sample_size as f64).sqrt();
    println!("Average {}, stddev {}", average, stddev);
    assert!(average > 127.5 - 3.0 * stddev);
    assert!(average < 127.5 + 3.0 * stddev);
  }

  #[test]
  fn test_new_from_str() {
    let value = "1";
    let mut input = "".to_string();

    for i in 0..64 {
      let code = AssetTypeCode::new_from_str(&input);
      let mut checked = 0;

      for j in 0..min(i, code.val.len()) {
        assert!(code.val[j] == value.as_bytes()[0]);
        checked = checked + 1;
      }

      for j in i..code.val.len() {
        assert!(code.val[j] == 0);
        checked = checked + 1;
      }

      assert!(checked == code.val.len());
      input = input + &value;
    }
  }

  #[test]
  fn test_new_from_base64() {
    let base64 = "ZGVmZ2hpamtsbW5vcHFycw==";
    let result = Code::new_from_base64(base64);

    assert_eq!(result.ok(),
               Some(Code { val: [100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
                                 112, 113, 114, 115] }));
  }

  #[test]
  fn test_code_to_base64() {
    let code = Code { val: [100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
                            113, 114, 115] };
    assert_eq!(code.to_base64(), "ZGVmZ2hpamtsbW5vcHFycw==");
  }

  #[test]
  fn test_verify() {
    let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

    let keypair = XfrKeyPair::generate(&mut prng);
    let message: &[u8] = b"test";

    let signed_address = SignedAddress { address: XfrAddress { key: *keypair.get_pk_ref() },
                                         signature: keypair.sign(message) };

    assert!(signed_address.verify(message));
  }

  // Test Transaction::add_operation
  // Below are not directly tested but called:
  //   TransferAssetBody::new
  //   IssueAssetBody::new
  //   DefineAssetBody::new
  //   TransferAsset::new
  //   IssueAsset::new
  //   DefineAsset::new
  #[test]
  fn test_add_operation() {
    // Create values to be used to instantiate operations
    let mut transaction: Transaction = Default::default();

    let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

    let keypair = XfrKeyPair::generate(&mut prng);
    let message: &[u8] = b"test";

    let public_key = *keypair.get_pk_ref();
    let signature = keypair.sign(message);

    // Instantiate an TransferAsset operation
    let xfr_note = XfrNote { body: XfrBody { inputs: Vec::new(),
                                             outputs: Vec::new(),
                                             proofs: XfrProofs { asset_amount_proof:
                                                                   AssetAmountProof::NoProof,
                                                                 asset_tracking_proof:
                                                                   Default::default() } },
                             multisig: Default::default() };

    let assert_transfer_body = TransferAssetBody { inputs: Vec::new(),
                                                   num_outputs: 0,
                                                   transfer: Box::new(xfr_note) };

    let asset_transfer = TransferAsset { body: assert_transfer_body,
                                         body_signatures: Vec::new(),
                                         transfer_type: TransferType::Standard };

    let transfer_operation = Operation::TransferAsset(asset_transfer.clone());

    // Instantiate an IssueAsset operation
    let asset_issuance_body = IssueAssetBody { code: Default::default(),
                                               seq_num: 0,
                                               num_outputs: 0,
                                               records: Vec::new() };

    let asset_issurance = IssueAsset { body: asset_issuance_body,
                                       pubkey: IssuerPublicKey { key: public_key },
                                       signature: signature.clone() };

    let issurance_operation = Operation::IssueAsset(asset_issurance.clone());

    // Instantiate an DefineAsset operation
    let asset = Default::default();

    let asset_creation = DefineAsset { body: DefineAssetBody { asset },
                                       pubkey: IssuerPublicKey { key: public_key },
                                       signature };

    let creation_operation = Operation::DefineAsset(asset_creation.clone());

    // Add operations to the transaction
    transaction.add_operation(transfer_operation);
    transaction.add_operation(issurance_operation);
    transaction.add_operation(creation_operation);

    // Verify operatoins
    assert_eq!(transaction.operations.len(), 3);

    assert_eq!(transaction.operations.get(0),
               Some(&Operation::TransferAsset(asset_transfer)));
    assert_eq!(transaction.operations.get(1),
               Some(&Operation::IssueAsset(asset_issurance)));
    assert_eq!(transaction.operations.get(2),
               Some(&Operation::DefineAsset(asset_creation)));
  }

  // Verify that the hash values of two transactions:
  //   are the same if the transactions differ only in merkle_id
  //   are different if the transactions differ in other fields
  // TODO(joe): determine a good test to replace this
  // #[test]
  // fn test_compute_merkle_hash() {
  //   let transaction_default: Transaction = Default::default();

  //   let transaction_different_merkle_id =
  //     Transaction { operations: Vec::new(),
  //                   credentials: Vec::new(),
  //                   memos: Vec::new() };

  //   let transaction_other_differences = Transaction { operations: Vec::new(),
  //                                                     credentials: Vec::new(),
  //                                                     memos: Vec::new(),
  //                                                     };

  //   let hash_value_default = transaction_default.compute_merkle_hash();
  //   let hash_value_different_merkle_id = transaction_different_merkle_id.compute_merkle_hash();
  //   let hash_value_other_differences = transaction_other_differences.compute_merkle_hash();

  //   assert_eq!(hash_value_different_merkle_id, hash_value_default);
  //   assert_ne!(hash_value_other_differences, hash_value_default);
  // }
}
