use chrono::prelude::*;

#[derive(Default, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct AssetTokenCode {
    pub val: [u8; 16],
}

// TODO: Define Memo
#[derive(Default, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct Proof {}
#[derive(Default, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct Memo {}
#[derive(Default, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct ConfidentialMemo {}
pub type Commitment = [u8; 32];


#[derive(Default, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct AssetTokenProperties {
    pub code: AssetTokenCode,
    pub issuer: Address,
    pub memo: Memo,
    pub confidential_memo: ConfidentialMemo,
    pub updatable: bool,
}

#[derive(Default, Hash, Eq, PartialEq, Copy, Clone, Debug)]
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

#[derive(Default, Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct Address {
    pub key: [u8; 32],
}

//TODO(Kevin): define types
#[derive(Clone)]
pub struct Variable {}
pub type Signature = [u8; 32];

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct TxSequenceNumber {
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

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct Utxo {
    pub key: UtxoAddress,
    pub digest: [u8; 32],
    pub address: Address,
    pub asset: AssetType,
}

pub struct TransactionKey {
    pub val: [u8; 32],
}

#[derive(Clone)]
pub struct TxOutput {
    pub address: Address,
    pub asset: AssetType,
}

#[derive(Clone)]
pub struct AssetTransfer {
    pub nonce: u128,
    pub variables: Vec<Variable>,
    pub confidential_asset_flag: bool,
    pub confidential_amount_flag: bool,
    pub input_utxos: Vec<Utxo>,
    pub outputs: Vec<TxOutput>,
    pub signatures: Vec<Signature>,
}

#[derive(Clone)]
pub struct AssetIssuance {
    pub nonce: u128,
    pub code: AssetTokenCode,
    pub outputs: Vec<TxOutput>,
    pub signature: Signature,
}

// ... etc...
#[derive(Clone)]
pub struct CreateAssetToken {
    pub properties: AssetTokenProperties,
    pub signature: Signature,
}

#[derive(Clone)]
pub enum Operation {
    asset_transfer(AssetTransfer),
    asset_issuance(AssetIssuance),
    create_token(CreateAssetToken),
    // ... etc...
}

#[derive(Clone)]
pub struct TimeBounds {
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
}

#[derive(Clone)]
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
