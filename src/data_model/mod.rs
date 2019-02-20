use chrono::prelude::*;

pub struct AssetTokenCode {
    val: [u8; 16],
}

// TODO: Define Memo
pub struct Proof{}
pub struct Memo {}
pub struct ConfidentialMemo {}
pub type Commitment = [u8; 32];

pub struct AssetToken {
    code: AssetTokenCode,
    digest: [u8; 32],
    issuer: Address,
    memo: Memo,
    confidential_memo: ConfidentialMemo,
    updatable: bool,
    units: u128,
    confidential_units: Commitment,
}

pub struct AssetPolicyKey {
    val: [u8; 16],
}

pub struct CustomAssetPolicy {
    key: AssetPolicyKey,
}

pub struct CredentialKey {
    val: [u8; 16],
}

pub struct Credential {
    key: CredentialKey,
}

pub struct SmartContractKey {
    val: [u8; 16],
}

pub struct SmartContract {
    key: SmartContractKey,
}

pub struct Address {
    key: [u8; 32],
}

//TODO(Kevin): define types
pub struct Variable{}
pub type Signature = [u8; 32];

pub struct TxSequenceNumber {
    val: u64,
}

pub struct UtxoAddress {
    transaction_id: TxSequenceNumber,
    operation_index: u16,
    output_index: u16,
}

pub struct Asset {
    typey: String,
    amount: u64,
}

pub struct PrivateAsset {
    hidden: [u8; 32],
}

pub enum AssetType {
    Asset,
    PrivateAsset,
}

pub struct Utxo {
    key: UtxoAddress,
    digest: [u8; 32],
    address: Address,
    asset_type: AssetType,
}

pub struct TransactionKey {
    val: [u8; 32],
}

pub struct TransferOutput {
    address : Address,
    token_type : AssetTokenCode,
    amount : u64,
}


pub struct AssetTransfer {
    nonce : u128,
    variables : Vec<Variable>,
    confidential_asset_flag : bool,
    confidential_amount_flag : bool,
    utxo_input_references : Vec<Utxo>,
    outputs : Vec<TransferOutput>,
    signatures : Vec<Signature>,
}

pub struct AssetIssuance {

}

// ... etc...
pub struct CreateAssetToken {

}

pub enum Operation {
    AssetTransfer,
    AssetIssuance,
    CreateAssetToken,
    // ... etc...
}

pub struct TimeBounds {
    start : DateTime<Utc>,
    end : DateTime<Utc>,
}

pub struct Transaction {
    key: TransactionKey,
    operations: Vec<Operation>,
    utxos: Vec<Utxo>,
    time_bounds: TimeBounds,
    proofs: Vec<Proof>,
    memos: Vec<Memo>,
    // ... etc...
}
