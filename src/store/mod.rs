use crate::data_model;
use data_model::{
    AssetIssuance, AssetPolicyKey, AssetToken, AssetTokenCode, AssetTransfer, CustomAssetPolicy,
    Operation, SmartContract, SmartContractKey, Transaction, TxSequenceNumber, Utxo, UtxoAddress,
};
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

pub struct NextIndex {
    tx_index: TxSequenceNumber,
    op_index: u16,
    utxo_index: u16,
}

impl NextIndex {
    pub fn new() -> NextIndex {
        NextIndex{
            tx_index: 0,
            op_index: 0,
            utxo_index: 0,
        }
    }
}

pub struct LedgerState {
    txs: Vec<Transaction>,
    utxos: HashMap<UtxoAddress, Utxo>,
    contracts: HashMap<SmartContractKey, SmartContract>,
    policies: HashMap<AssetPolicyKey, CustomAssetPolicy>,
    tokens: HashMap<AssetTokenCode, AssetToken>,
    next_index: NextIndex,
}

impl LedgerState {
    pub fn new() -> LedgerState {
        LedgerState {
            txs: Vec::new(),
            utxos: HashMap::new(),
            contracts: HashMap::new(),
            policies: HashMap::new(),
            tokens: HashMap::new(),
            next_index: NextIndex::new(),
        }
    }

    fn apply_asset_transfer(&mut self, asset_transfer: &AssetTransfer) -> () {
        for utxo in &asset_transfer.input_utxos {
            self.utxos.remove(&utxo.key);
        }
        for out in &asset_transfer.outputs {
            let utxo_addr = UtxoAddress {
                transaction_id: self.next_index.tx_index,
                operation_index: self.next_index.op_index,
                output_index: self.next_index.utxo_index,
            };
            let utxo_ref = Utxo {
                key: utxo_addr,
                digest: [0; 32], // TODO(Kevin): determine hash
                address: out.address,
                asset_type: out.asset_type.clone(),
            };
            self.utxos.insert(utxo_addr, utxo_ref);
            self.next_index.utxo_index += 1;
        }
    }

    fn apply_asset_issuance(&mut self, asset_issuance: &AssetIssuance) -> () {}

    fn apply_operation(&mut self, op: &Operation) -> () {
        self.next_index.utxo_index = 0;
        match op {
            Operation::asset_transfer(transfer)=> self.apply_asset_transfer(transfer),
            Operation::asset_issuance(issuance)=> self.apply_asset_issuance(issuance),
            _ => println!("Warning: operation not valid!"), // etc ...
        }
        self.next_index.op_index += 1;
    }
}

impl LedgerUpdate for LedgerState {
    fn apply_transaction(&mut self, txn: &Transaction) -> () {
        self.next_index.op_index = 0;
        // Apply the operations
        for op in &txn.operations {
            self.apply_operation(op);
        }
        self.next_index.tx_index += 1;
        //TODO: (Kevin) Determine where to add tx to the tx_log
        //self.txs.push(txn.copy());
    }
}

impl LedgerAccess for LedgerState {
    fn check_utxo(&self, addr: &UtxoAddress) -> Option<Utxo> {
        match self.utxos.get(addr) {
            Some(utxo) => Some(utxo.clone()),
            None => None
        }
    }

    fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken> {
        match self.tokens.get(code) {
            Some(token) => Some(token.clone()),
            None => None
        }
    }

    fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy> {
        match self.policies.get(key) {
            Some(policy) => Some(policy.clone()),
            None => None
        }
    }

    fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract> {
        match self.contracts.get(key) {
            Some(contract) => Some(contract.clone()),
            None => None
        }
    }
}
