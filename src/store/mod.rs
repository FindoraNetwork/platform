use data_model::{UtxoAddress, Utxo, AssetTokenCode, AssetToken, AssetPolicyKey, CustomAssetPolicy, SmartContractKey, SmartContract};

pub trait LedgerAccess {
    fn check_utxo(&self, addr: &UtxoAddress) -> Option<Utxo>;
    fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken>;
    fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy>;
    fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract>;
}

pub trait LedgerUpdate {
    fn apply_transaction(&mut self, txn: &Transaction) -> ();
}

// TODO(Kevin): implement LedgerAccess and LedgerUpdate for a new struct that can be added to the main App struct