use crate::data_model;
use crate::data_model::CreateAssetToken;
use crate::data_model::TxOutput;
use chrono::format::Pad;
use data_model::{
    Asset, AssetIssuance, AssetPolicyKey, AssetToken, AssetTokenCode, AssetTransfer, AssetType,
    CustomAssetPolicy, Operation, PrivateAsset, SmartContract, SmartContractKey, Transaction,
    TxSequenceNumber, Utxo, UtxoAddress,
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

pub struct NextIndex {
    tx_index: TxSequenceNumber,
    op_index: u16,
    utxo_index: u16,
}

impl NextIndex {
    pub fn new() -> NextIndex {
        NextIndex {
            tx_index: TxSequenceNumber { val: 0 },
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

    fn add_txo(&mut self, txo: &TxOutput) {
        let utxo_addr = UtxoAddress {
            transaction_id: TxSequenceNumber {
                val: self.next_index.tx_index.val,
            },
            operation_index: self.next_index.op_index,
            output_index: self.next_index.utxo_index,
        };
        let utxo_ref = Utxo {
            key: utxo_addr,
            digest: [0; 32], // TODO(Kevin): determine hash
            address: txo.address,
            asset: txo.asset.clone(),
        };
        self.utxos.insert(utxo_addr, utxo_ref);
        self.next_index.utxo_index += 1;
    }

    fn apply_asset_transfer(&mut self, asset_transfer: &AssetTransfer) -> () {
        for utxo in &asset_transfer.input_utxos {
            self.utxos.remove(&utxo.key);
        }
        for out in &asset_transfer.outputs {
            self.add_txo(out);
        }
    }

    fn apply_asset_issuance(&mut self, asset_issuance: &AssetIssuance) -> () {
        let sum: u128 = 0;
        for out in &asset_issuance.outputs {
            self.add_txo(out);
            match &out.asset {
                AssetType::Normal(a) => {
                    if let Some(token) = self.tokens.get_mut(&a.code) {
                        token.units += a.amount as u128; //TODO: Are we defining amounts as u64 or u128?
                    }
                    //TODO: We should never have the if statement above fail, but should we write something if it does
                }
                //TODO: (Kevin) Implement Private Asset Issuance
                AssetType::Private(p) => println!("Private Issuance Not Implemented!"),
            }
        }
    }

    fn apply_asset_creation(&mut self, create_asset_token: &CreateAssetToken) -> () {
        self.tokens.insert(
            create_asset_token.asset_token.code.clone(),
            create_asset_token.asset_token.clone(),
        );
    }

    fn apply_operation(&mut self, op: &Operation) -> () {
        self.next_index.utxo_index = 0;
        match op {
            Operation::asset_transfer(transfer) => self.apply_asset_transfer(transfer),
            Operation::asset_issuance(issuance) => self.apply_asset_issuance(issuance),
            Operation::create_token(token) => self.apply_asset_creation(token),
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
        self.next_index.tx_index.val += 1;
        self.txs.push(txn.clone());
    }
}

impl LedgerAccess for LedgerState {
    fn check_utxo(&self, addr: &UtxoAddress) -> Option<Utxo> {
        match self.utxos.get(addr) {
            Some(utxo) => Some(utxo.clone()),
            None => None,
        }
    }

    fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken> {
        match self.tokens.get(code) {
            Some(token) => Some(token.clone()),
            None => None,
        }
    }

    fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy> {
        match self.policies.get(key) {
            Some(policy) => Some(policy.clone()),
            None => None,
        }
    }

    fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract> {
        match self.contracts.get(key) {
            Some(contract) => Some(contract.clone()),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data_model::Address;

    #[test]
    fn asset_creation() {
        let mut state = LedgerState::new();
        let token_code1 = AssetTokenCode { val: [1; 16] };
        assert_eq!(true, state.get_asset_token(&token_code1).is_none());
        // Using empty token because we aren't testing the validation step
        let mut token = AssetToken::create_empty();
        token.code = token_code1;

        let mut tx = Transaction::create_empty();
        let create_asset = CreateAssetToken {
            asset_token: token,
            signature: [0; 32],
        };
        let create_op = Operation::create_token(create_asset);
        tx.operations.push(create_op);
        state.apply_transaction(&tx);
        assert_eq!(Some(token), state.get_asset_token(&token_code1));
    }

    #[test]
    fn asset_issued() {
        let mut state = LedgerState::new();
        let token_code1 = AssetTokenCode { val: [1; 16] };
        let mut token = AssetToken::create_empty();
        token.code = token_code1;

        let mut tx = Transaction::create_empty();
        let create_asset = CreateAssetToken {
            asset_token: token,
            signature: [0; 32],
        };

        let create_op = Operation::create_token(create_asset);
        tx.operations.push(create_op);
        let issued = TxOutput {
            address: Address { key: [0; 32] },
            asset: AssetType::Normal(Asset {
                code: AssetTokenCode { val: [1; 16] },
                amount: 100,
            }),
        };
        let issue_op = Operation::asset_issuance(AssetIssuance {
            nonce: 0,
            code: token_code1,
            outputs: vec![issued.clone()],
            signature: [0; 32], //Empty signature
        });
        tx.operations.push(issue_op);
        //let issue_op = O
        state.apply_transaction(&tx);
        // Update units as would be done once asset is issued
        token.units = 100;
        assert_eq!(Some(token), state.get_asset_token(&token_code1));
        let utxo_loc = UtxoAddress {
            transaction_id: TxSequenceNumber { val: 0 },
            operation_index: 1,
            output_index: 0,
        };
        assert_eq!(true, state.check_utxo(&utxo_loc).is_some());
        assert_eq!(issued.address, state.check_utxo(&utxo_loc).unwrap().address);
        assert_eq!(issued.asset, state.check_utxo(&utxo_loc).unwrap().asset);
    }

    #[test]
    fn asset_transferred() {
        let mut state = LedgerState::new();
        let token_code1 = AssetTokenCode { val: [1; 16] };
        let mut token = AssetToken::create_empty();
        token.code = token_code1;

        let mut tx = Transaction::create_empty();
        let create_asset = CreateAssetToken {
            asset_token: token,
            signature: [0; 32],
        };

        let create_op = Operation::create_token(create_asset);
        tx.operations.push(create_op);
        let issued = TxOutput {
            address: Address { key: [5; 32] },
            asset: AssetType::Normal(Asset {
                code: AssetTokenCode { val: [1; 16] },
                amount: 100,
            }),
        };
        let issue_op = Operation::asset_issuance(AssetIssuance {
            nonce: 0,
            code: token_code1,
            outputs: vec![issued.clone()],
            signature: [0; 32], //Empty signature
        });
        tx.operations.push(issue_op);
        //let issue_op = O
        state.apply_transaction(&tx);
        // Update units as would be done once asset is issued
        token.units = 100;
        assert_eq!(Some(token), state.get_asset_token(&token_code1));
        let utxo_loc = UtxoAddress {
            transaction_id: TxSequenceNumber { val: 0 },
            operation_index: 1,
            output_index: 0,
        };
        assert_eq!(true, state.check_utxo(&utxo_loc).is_some());
        assert_eq!(issued.address, state.check_utxo(&utxo_loc).unwrap().address);
        assert_eq!(issued.asset, state.check_utxo(&utxo_loc).unwrap().asset);

        let mut tx2 = Transaction::create_empty();
        let transfer_to = TxOutput {
            address: Address { key: [7; 32] },
            asset: AssetType::Normal(Asset {
                code: AssetTokenCode { val: [1; 16] },
                amount: 100,
            }),
        };
        let transfer_op = Operation::asset_transfer(AssetTransfer {
            nonce: 0,
            variables: Vec::new(),
            confidential_asset_flag: false,
            confidential_amount_flag: false,
            input_utxos: vec![state.check_utxo(&utxo_loc).unwrap()],
            outputs: vec![transfer_to.clone()],
            signatures: vec![[0; 32]],
        });
        tx2.operations.push(transfer_op);
        state.apply_transaction(&tx2);
        assert_eq!(true, state.check_utxo(&utxo_loc).is_none());
        let utxo_loc = UtxoAddress {
            transaction_id: TxSequenceNumber { val: 1 },
            operation_index: 0,
            output_index: 0,
        };
        assert_eq!(true, state.check_utxo(&utxo_loc).is_some());
        assert_eq!(
            transfer_to.address,
            state.check_utxo(&utxo_loc).unwrap().address
        );
        assert_eq!(
            transfer_to.asset,
            state.check_utxo(&utxo_loc).unwrap().asset
        );
    }
}
