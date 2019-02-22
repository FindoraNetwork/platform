use chrono::prelude::*;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct AssetTokenCode {
    val: [u8; 16],
}

// TODO: Define Memo
#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Proof {}
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct Memo {}
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct ConfidentialMemo {}
pub type Commitment = [u8; 32];

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct AssetToken {
    pub code: AssetTokenCode,
    pub digest: [u8; 32],
    pub issuer: Address,
    pub memo: Memo,
    pub confidential_memo: ConfidentialMemo,
    pub updatable: bool,
    pub units: u128,
    pub confidential_units: Commitment,
}

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

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct Address {
    pub key: [u8; 32],
}

//TODO(Kevin): define types
pub struct Variable {}
pub type Signature = [u8; 32];

pub type TxSequenceNumber = u64;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct UtxoAddress {
    pub transaction_id: TxSequenceNumber,
    pub operation_index: u16,
    pub output_index: u16,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct Asset {
    pub name: String,
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
    pub asset_type: AssetType,
}

pub struct TransactionKey {
    pub val: [u8; 32],
}

pub struct TxOutput {
    pub address: Address,
    pub asset_type: AssetType,
}

pub struct AssetTransfer {
    pub nonce : u128,
    pub variables : Vec<Variable>,
    pub confidential_asset_flag : bool,
    pub confidential_amount_flag : bool,
    pub input_utxos: Vec<Utxo>,
    pub outputs : Vec<TxOutput>,
    pub signatures : Vec<Signature>,
}

pub struct AssetIssuance {
    pub nonce: u128,
    pub asset: Asset,
}

// ... etc...
pub struct CreateAssetToken {}

pub struct OperationReference {
    pub tx_index: TxSequenceNumber,
    pub op_index: u16,
}

pub enum Operation {
    asset_transfer(AssetTransfer),
    asset_issuance(AssetIssuance),
    create_token(CreateAssetToken),
    // ... etc...
}

pub struct TimeBounds {
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
}

pub struct Transaction {
    pub operations: Vec<Operation>,
    pub utxos: Vec<Utxo>,
    pub time_bounds: TimeBounds,
    pub proofs: Vec<Proof>,
    pub memos: Vec<Memo>,
    // ... etc...
}
