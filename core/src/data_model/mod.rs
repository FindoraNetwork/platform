use chrono::prelude::*;
use curve25519_dalek::ristretto::CompressedRistretto;
use std::collections::HashMap;
use zei::basic_crypto::signatures::{XfrPublicKey, XfrSignature};
use zei::transfers::{BlindAssetRecord, XfrNote};

// Unique Identifier for AssetTokens
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct AssetTokenCode {
    pub val: [u8; 16],
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
    pub asset_type: String,
    pub issuer: IssuerPublicKey,
    pub memo: Memo,
    pub confidential_memo: ConfidentialMemo,
    pub updatable: bool,
}

#[derive(Default, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct AssetToken {
    pub properties: Asset, //TODO: ZEI. change to asset_record from zei...
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
pub struct CustomAssetPolicy;

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

// #[derive(Default, Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
// pub struct TxSequenceNumber {
//     pub val: u64,
// }

#[derive(Default, Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct TxoSID {
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
    pub inputs: Vec<TxoSID>, // ledger address of inputs
    pub outputs: Vec<TxoSID>, // computed in check?
    pub transfer: Box<XfrNote>,        //TODO: ZEI. XfrNote,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AssetIssuanceBody {
    pub code: AssetTokenCode,
    pub seq_num: u64,
    pub outputs: Vec<TxoSID>,
    pub records: Vec<TxOutput>,
    // pub outputs: Vec<TxoSID>, // computed in check?
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AssetCreationBody {
    pub asset: Asset,
    pub outputs: Vec<TxoSID>, // computed in check?
}

// TODO: UTXO Addresses must be included in Transfer Signature
#[derive(Debug, Serialize, Deserialize)]
pub struct AssetTransfer {
    //pub nonce: u128,
    pub body: AssetTransferBody,
    pub body_signatures: Vec<SignedAddress>,
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
    pub variable_utxos: Vec<TxoSID>,
    pub credentials: Vec<CredentialProof>,
    pub memos: Vec<Memo>,
    //pub time_bounds: TimeBounds,
    // ... etc...
}

impl Transaction {
    pub fn create_empty() -> Transaction {
        Transaction { operations: Vec::new(),
                      variable_utxos: Vec::new(),
                      credentials: Vec::new(),
                      memos: Vec::new() }
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
