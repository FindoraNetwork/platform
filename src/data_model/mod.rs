
use chrono::prelude::*;
use zei::utxo_transaction::{Tx, TxOutput, ZeiSignature};
use schnorr::{PublicKey, SecretKey, Signature};
use serde::{Serialize, Deserialize};
use serde::{Serializer, Deserializer};
use blake2::{Blake2b,Digest};

// Unique Identifier for AssetTokens
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct AssetTokenCode {
    pub val: [u8; 16],
}

// TODO: Define Memo
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct Proof {}
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct Memo {}
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct ConfidentialMemo {}
pub type Commitment = [u8; 32];

#[derive(Default, Serialize, Deserialize, Eq, PartialEq, Clone, Copy, Debug)]
pub struct Address {
    pub key: PublicKey
}

#[derive(Serialize, Deserialize, Eq, PartialEq, Clone, Copy, Debug)]
pub struct LedgerSignature {
    pub signature: Signature,
}

impl LedgerSignature {
    pub fn verify(&self, message: &[u8], address: &Address) -> bool {
        // let pk = address.key;
        // if pk.is_err() {
        //     return false;
        // }
        // let pk = pk.unwrap();
        // if pk.verify::<blake2::Blake2b>(message, &self.signature).is_err(){
        //     return false;
        // }
        true
    }
}

#[derive(Default, Serialize, Deserialize, Eq, PartialEq, Copy, Clone, Debug)]
pub struct AssetTokenProperties {
    pub code: AssetTokenCode,
    pub issuer: Address,
    pub memo: Memo,
    pub confidential_memo: ConfidentialMemo,
    pub updatable: bool,
}

#[derive(Default, Eq, PartialEq, Copy, Clone, Debug)]
pub struct AssetToken {
    pub properties: AssetTokenProperties,
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

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct AssetPolicyKey {
    pub val: [u8; 16],
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct CustomAssetPolicy {}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct CredentialKey {
    pub val: [u8; 16],
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Credential {
    pub key: CredentialKey,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct SmartContractKey {
    pub val: [u8; 16],
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct SmartContract {}

//TODO(Kevin): define types
#[derive(Clone)]
pub struct Variable {}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct TxSequenceNumber {
    pub val: u64,
}

pub struct UtxoIndex{
    pub val: u64,
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct UtxoAddress {
    pub transaction_id: TxSequenceNumber,
    pub operation_index: u16,
    pub output_index: u16,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct Asset {
    pub code: AssetTokenCode,
    pub amount: u64,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct PrivateAsset {
    pub hidden: [u8; 32],
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum AssetType {
    Normal(Asset),
    Private(PrivateAsset),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Utxo {
    pub key: UtxoAddress,
    pub digest: [u8; 32],
    pub output: TxOutput,
}

pub struct TransactionKey {
    pub val: [u8; 32],
}

#[derive(Clone, Debug)]
pub struct AssetTransferBody {
    //pub nonce: u128,
    pub utxo_addresses: Vec<UtxoAddress>,
    pub transfer: Tx,
    pub signatures: Vec<LedgerSignature>,
}

// TODO: UTXO Addresses must be included in Transfer Signature
#[derive(Clone, Debug)]
pub struct AssetTransfer {
    //pub nonce: u128,
    pub body: AssetTransferBody,
    pub signatures: Vec<LedgerSignature>,
}

#[derive(Clone, Debug)]
pub struct AssetIssuanceBody {
    pub seq_num: u128,
    pub code: AssetTokenCode,
    pub outputs: Vec<TxOutput>,
}

// TODO: Include mechanism for replay attacks
#[derive(Clone, Debug)]
pub struct AssetIssuance {
    pub body: AssetIssuanceBody,
    pub signature: LedgerSignature,
}

// ... etc...
#[derive(Clone, Debug)]
pub struct CreateAssetToken {
    pub properties: AssetTokenProperties,
    pub signature: LedgerSignature,
}

#[derive(Clone, Debug)]
pub enum Operation {
    asset_transfer(AssetTransfer),
    asset_issuance(AssetIssuance),
    create_token(CreateAssetToken),
    // ... etc...
}

#[derive(Clone, Debug)]
pub struct TimeBounds {
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
}

#[derive(Clone, Debug)]
pub struct Transaction {
    pub operations: Vec<Operation>,
    pub utxos: Vec<Utxo>,
    pub proofs: Vec<Proof>,
    pub memos: Vec<Memo>,
    //pub time_bounds: TimeBounds,
    // ... etc...
}

impl Transaction {
    pub fn create_empty() -> Transaction {
        Transaction {
            operations: Vec::new(),
            utxos: Vec::new(),
            proofs: Vec::new(),
            memos: Vec::new(),
        }
    }
}
