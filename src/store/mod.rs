use data_model::{UtxoAddress, Utxo, AssetTokenCode, AssetToken, AssetPolicyKey, CustomAssetPolicy, SmartContractKey, SmartContract, Transaction};
use std::collections::HashMap;

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

pub struct LedgerState {
    txs: Vec<Transaction>,
    utxos: HashMap<UtxoAddress, Utxo>,
    contracts: HashMap<SmartContractKey, SmartContract>,
    policies: HashMap<AssetPolicyKey, AssetPolicyKey>,
    tokens: HashMap<AssetTokenCode, AssetToken>,
}

impl LedgerAccess for LedgerState {

    fn check_utxo(&self, addr:&UtxoAddress) ->Option<Utxo> {
        self.utxos.get(addr)
    }

    fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken> {
        self.tokens.get(code)
    }

    fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy> {
        self.policies.get(key)
    }

    fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract> {
        self.contracts.get(key)
    }
}
