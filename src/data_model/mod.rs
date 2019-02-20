pub struct AssetTokenCode {
    val: [u8; 16],
}

// TODO(Kevin): define Digest and Memo
pub struct Digest {}
pub struct Memo {}

pub struct AssetToken {
    code: AssetTokenCode,
    digest: [u8; 32],
    issuer: Address,
    memo: Memo,
    updatable: bool,
    units: u128,
    //TODO(Kevin): Determine confidential memo and confidential_units types
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

pub struct TxSequenceNumber {
    val: u64,
}

pub struct UtxoAddress {
    transaction_id: TxSequenceNumber,
    operation_index: u16,
    output_index: u16,
}

pub struct Asset {
    type: String,
    amount: u64,
}

pub struct PrivateAsset {
    hidden: [u8, 32],
}

pub enum AssetType {
    normal: Asset,
    private: PrivateAsset,
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

pub struct AssetTransfer {

}

pub struct AssetIssuance {

}

// ... etc...
pub struct CreateAssetToken {

}

pub enum Operation {
    asset_transfer: AssetTransfer,
    asset_issuance: AssetIssuance,
    create_asset_token: CreateAssetToken,
    // ... etc...
}

pub struct Transaction {
    key: TransactionKey,
    operations: Vec<Operation>,
    // ... etc...
}
