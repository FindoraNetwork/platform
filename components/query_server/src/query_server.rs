#![deny(warnings)]
#![allow(clippy::needless_borrow)]

use lazy_static::lazy_static;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::*;
use ledger::staking::ops::mint_fra::MintEntry;
use ledger::staking::BlockHeight;
use ledger::store::*;
use parking_lot::{Condvar, Mutex, RwLock};
use ruc::*;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::sync::Arc;
use utils::MetricsRenderer;
use zei::xfr::structs::OwnerMemo;

lazy_static! {
    pub static ref BLOCK_CREATED: Arc<(Mutex<bool>, Condvar)> =
        Arc::new((Mutex::new(false), Condvar::new()));
}

pub type TxnIDHash = (TxnSID, String);
type Issuances = Vec<Arc<(TxOutput, Option<OwnerMemo>)>>;

macro_rules! fail {
    () => {
        PlatformError::QueryServerError(None)
    };
    ($s:expr) => {
        PlatformError::QueryServerError(Some($s.to_string()))
    };
}

pub struct QueryServer<U>
where
    U: MetricsRenderer,
{
    committed_state: Arc<RwLock<LedgerState>>,
    addresses_to_utxos: HashMap<XfrAddress, HashSet<TxoSID>>,
    related_transactions: HashMap<XfrAddress, HashSet<TxnSID>>, // Set of transactions related to a ledger address
    related_transfers: HashMap<AssetTypeCode, HashSet<TxnSID>>, // Set of transfer transactions related to an asset code
    claim_hist_txns: HashMap<XfrAddress, Vec<TxnSID>>, // List of claim transactions related to a ledger address
    coinbase_oper_hist: HashMap<XfrAddress, Vec<(BlockHeight, MintEntry)>>,
    created_assets: HashMap<IssuerPublicKey, Vec<DefineAsset>>,
    traced_assets: HashMap<IssuerPublicKey, Vec<AssetTypeCode>>, // List of assets traced by a ledger address
    issuances: HashMap<IssuerPublicKey, Issuances>, // issuance mapped by public key
    token_code_issuances: HashMap<AssetTypeCode, Issuances>, // issuance mapped by token code
    owner_memos: HashMap<TxoSID, OwnerMemo>,
    utxos_to_map_index: HashMap<TxoSID, XfrAddress>,
    txo_to_txnid: HashMap<TxoSID, TxnIDHash>, // txo(spent, unspent) to authenticated txn (sid, hash)
    txn_sid_to_hash: HashMap<TxnSID, String>, // txn sid to txn hash
    txn_hash_to_sid: HashMap<String, TxnSID>, // txn hash to txn sid
    metrics_renderer: U,
    app_block_cnt: usize,
}

impl<U> QueryServer<U>
where
    U: MetricsRenderer,
{
    pub fn new(ledger: Arc<RwLock<LedgerState>>, metrics_renderer: U) -> QueryServer<U> {
        QueryServer {
            committed_state: ledger,
            addresses_to_utxos: map! {},
            related_transactions: map! {},
            related_transfers: map! {},
            claim_hist_txns: map! {},
            coinbase_oper_hist: map! {},
            owner_memos: map! {},
            created_assets: map! {},
            traced_assets: map! {},
            issuances: map! {},
            token_code_issuances: map! {},
            utxos_to_map_index: map! {},
            txo_to_txnid: map! {},
            txn_sid_to_hash: map! {},
            txn_hash_to_sid: map! {},
            metrics_renderer,
            app_block_cnt: 0,
        }
    }

    pub fn render(&self) -> String {
        self.metrics_renderer.rendered()
    }

    // Returns the set of records issued by a certain key.
    pub fn get_issued_records(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.issuances
            .get(issuer)
            .map(|recs| recs.iter().map(|rec| rec.deref().clone()).collect())
    }

    // Returns the set of records issued by a certain token code.
    pub fn get_issued_records_by_code(
        &self,
        code: &AssetTypeCode,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.token_code_issuances
            .get(code)
            .map(|recs| recs.iter().map(|rec| rec.deref().clone()).collect())
    }

    pub fn get_created_assets(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<&Vec<DefineAsset>> {
        self.created_assets.get(issuer)
    }

    // Returns the list of assets traced by a certain key.
    pub fn get_traced_assets(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<&Vec<AssetTypeCode>> {
        self.traced_assets.get(issuer)
    }

    pub fn get_coinbase_entries(
        &self,
        address: &XfrAddress,
        start: usize,
        end: usize,
        order_desc: bool,
    ) -> Result<(u64, Vec<(u64, MintEntry)>)> {
        if let Some(hist) = self.coinbase_oper_hist.get(address) {
            let len = hist.len();
            if len > start {
                let slice = match order_desc {
                    false => {
                        let mut new_end = len;
                        if len > end {
                            new_end = end;
                        }
                        hist[start..new_end].to_vec()
                    }
                    true => {
                        let mut new_start = 0;
                        if len > end {
                            new_start = len - end;
                        }
                        let mut tmp = hist[new_start..len - start].to_vec();
                        tmp.reverse();
                        tmp
                    }
                };
                return Ok((len as u64, slice));
            } else if len == 0 {
                return Ok((0, vec![]));
            } else {
                return Err(eg!("Index out of range"));
            }
        }

        Ok((0, vec![]))
    }

    // Returns a list of claim transactions of a given ledger address
    pub fn get_claim_transactions(
        &self,
        address: &XfrAddress,
        start: usize,
        end: usize,
        order_desc: bool,
    ) -> Result<Vec<Option<Transaction>>> {
        if let Some(hist) = self.claim_hist_txns.get(address) {
            let len = hist.len();
            if len > start {
                let slice = match order_desc {
                    false => {
                        let mut new_end = len;
                        if len > end {
                            new_end = end;
                        }
                        hist[start..new_end].to_vec()
                    }
                    true => {
                        let mut new_start = 0;
                        if len > end {
                            new_start = len - end;
                        }
                        let mut tmp = hist[new_start..len - start].to_vec();
                        tmp.reverse();
                        tmp
                    }
                };

                let ledger = Arc::clone(&self.committed_state);
                let ledger = ledger.read();

                return Ok(slice
                    .iter()
                    .map(|h| {
                        if let Some(txn) = ledger.get_transaction(*h) {
                            Some(txn.finalized_txn.txn)
                        } else {
                            None
                        }
                    })
                    .collect());
            }
        }

        Err(eg!("Record not found"))
    }

    // Returns the set of transactions that are in some way related to a given ledger address.
    // An xfr address is related to a transaction if it is one of the following:
    // 1. Owner of a transfer output
    // 2. Transfer signer (owner of input or co-signer)
    // 3. Signer of a an issuance txn
    // 4. Signer of a kv_update txn
    // 5. Signer of a memo_update txn
    pub fn get_related_transactions(
        &self,
        address: &XfrAddress,
    ) -> Option<&HashSet<TxnSID>> {
        self.related_transactions.get(&address)
    }

    // Returns the set of transfer transactions that are associated with a given asset.
    // The asset type must be nonconfidential.
    pub fn get_related_transfers(
        &self,
        code: &AssetTypeCode,
    ) -> Option<&HashSet<TxnSID>> {
        self.related_transfers.get(&code)
    }

    // Returns the set of TxoSIDs that are the indices of records owned by a given address.
    pub fn get_owned_utxo_sids(&self, address: &XfrAddress) -> Option<&HashSet<TxoSID>> {
        self.addresses_to_utxos.get(&address)
    }

    // Returns the owner of a given txo_sid.
    pub fn get_address_of_sid(&self, txo_sid: TxoSID) -> Option<&XfrAddress> {
        self.utxos_to_map_index.get(&txo_sid)
    }

    // Returns the authenticated txn (id, hash) of a given txo_sid.
    pub fn get_authenticated_txnid(&self, txo_sid: TxoSID) -> Option<&TxnIDHash> {
        self.txo_to_txnid.get(&txo_sid)
    }

    // Returns the transaction hash of a given txn_sid.
    pub fn get_transaction_hash(&self, txn_sid: TxnSID) -> Option<&String> {
        self.txn_sid_to_hash.get(&txn_sid)
    }

    // Returns the transaction sid of a given txn_hash.
    pub fn get_transaction_sid(&self, txn_hash: String) -> Option<&TxnSID> {
        self.txn_hash_to_sid.get(&txn_hash)
    }

    // Returns most recent commits at query_server side.
    pub fn get_commits(&self) -> u64 {
        self.committed_state.read().get_block_commit_count()
    }

    // Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
    pub fn get_owner_memo(&self, txo_sid: TxoSID) -> Option<&OwnerMemo> {
        self.owner_memos.get(&txo_sid)
    }

    // Add created asset
    pub fn add_created_asset(&mut self, creation: &DefineAsset) {
        let issuer = creation.pubkey;
        let set = self.created_assets.entry(issuer).or_insert_with(Vec::new);

        set.push(creation.clone());
        set.sort_by_key(|i| i.pubkey);
        set.dedup_by_key(|i| i.body.asset.code);
    }

    // Add traced asset
    pub fn add_traced_asset(&mut self, creation: &DefineAsset) {
        let tracing_policies = &creation.body.asset.asset_rules.tracing_policies;
        if !tracing_policies.is_empty() {
            let issuer = creation.pubkey;
            let new_asset_code = creation.body.asset.code;
            let set = self.traced_assets.entry(issuer).or_insert_with(Vec::new);

            set.push(new_asset_code);
            set.sort_by_key(|i| i.val);
            set.dedup_by_key(|i| i.val);
        }
    }

    // Cache issuance records
    pub fn cache_issuance(&mut self, issuance: &IssueAsset) {
        let new_records: Vec<Arc<(TxOutput, Option<OwnerMemo>)>> = issuance
            .body
            .records
            .iter()
            .map(|rec| Arc::new(rec.clone()))
            .collect();

        macro_rules! save_issuance {
            ($maps: tt, $key: tt) => {
                let records = $maps.entry($key).or_insert_with(Vec::new);
                records.extend_from_slice(&new_records);
            };
        }

        let key_issuances = &mut self.issuances;
        let pubkey = issuance.pubkey;
        save_issuance!(key_issuances, pubkey);

        let token_issuances = &mut self.token_code_issuances;
        let token_code = issuance.body.code;
        save_issuance!(token_issuances, token_code);
    }

    fn remove_spent_utxos(&mut self, transfer: &TransferAsset) -> Result<()> {
        for input in &transfer.body.inputs {
            match input {
                TxoRef::Relative(_) => {} // Relative utxos were never cached so no need to do anything here
                TxoRef::Absolute(txo_sid) => {
                    let address = self.utxos_to_map_index.get(&txo_sid).c(d!(fail!(
                        "Attempting to remove owned txo of address that isn't cached"
                    )))?;
                    let hash_set = self
                        .addresses_to_utxos
                        .get_mut(&address)
                        .c(d!(fail!("No txos stored for this address")))?;
                    hash_set.remove(&txo_sid);
                }
            }
        }
        Ok(())
    }

    // Updates query server cache with new transactions from a block.
    // Each new block must be consistent with the state of the cached ledger up until this point
    fn apply_new_blocks(&mut self) -> Result<()> {
        let ledger = Arc::clone(&self.committed_state);
        let ledger = ledger.read();

        if self.app_block_cnt == ledger.blocks.len() {
            return Ok(());
        }

        // Should be unreachable !!
        if self.app_block_cnt > ledger.blocks.len() {
            return Err(eg!("The fucking world is over!"));
        }

        for block in ledger.blocks.iter().skip(self.app_block_cnt) {
            // Update ownership status
            for (txn_sid, txo_sids) in
                block.txns.iter().map(|v| (v.tx_id, v.txo_ids.as_slice()))
            {
                let curr_txn =
                    ledger.get_transaction(txn_sid).c(d!())?.finalized_txn.txn;
                // get the transaction, ownership addresses, and memos associated with each transaction
                let (addresses, owner_memos) = {
                    let addresses: Vec<XfrAddress> = txo_sids
                        .iter()
                        .map(|sid| XfrAddress {
                            key: ((ledger
                                .get_utxo(*sid)
                                .or_else(|| ledger.get_spent_utxo(*sid))
                                .unwrap()
                                .utxo)
                                .0)
                                .record
                                .public_key,
                        })
                        .collect();

                    let owner_memos = curr_txn.get_owner_memos_ref();

                    (addresses, owner_memos)
                };

                let classify_op = |op: &Operation| {
                    match op {
                        Operation::Claim(i) => {
                            let key = i.get_claim_publickey();
                            let hist = self
                                .claim_hist_txns
                                .entry(XfrAddress { key })
                                .or_insert_with(Vec::new);

                            // keep it in ascending order to reduce memory movement count
                            match hist.binary_search_by(|a| a.0.cmp(&txn_sid.0)) {
                                Ok(_) => { /*skip if txn_sid already exists*/ }
                                Err(idx) => hist.insert(idx, txn_sid),
                            }
                        }
                        Operation::MintFra(i) => i.entries.iter().for_each(|me| {
                            let key = me.utxo.record.public_key;
                            let hist = self
                                .coinbase_oper_hist
                                .entry(XfrAddress { key })
                                .or_insert_with(Vec::new);
                            hist.push((i.height, me.clone()));
                        }),
                        _ => { /* filter more operations before this line */ }
                    };
                };

                // Update related addresses
                // Apply classify_op for each operation in curr_txn
                let related_addresses = get_related_addresses(&curr_txn, classify_op);
                for address in &related_addresses {
                    self.related_transactions
                        .entry(*address)
                        .or_insert_with(HashSet::new)
                        .insert(txn_sid);
                }

                // Update transferred nonconfidential assets
                let transferred_assets =
                    get_transferred_nonconfidential_assets(&curr_txn);
                for asset in &transferred_assets {
                    self.related_transfers
                        .entry(*asset)
                        .or_insert_with(HashSet::new)
                        .insert(txn_sid);
                }

                // Add created asset and remove spent utxos
                for op in &curr_txn.body.operations {
                    match op {
                        Operation::DefineAsset(define_asset) => {
                            self.add_created_asset(&define_asset);
                            self.add_traced_asset(&define_asset);
                        }
                        Operation::IssueAsset(issue_asset) => {
                            self.cache_issuance(&issue_asset)
                        }
                        Operation::TransferAsset(transfer_asset) => {
                            self.remove_spent_utxos(&transfer_asset).c(d!())?
                        }
                        _ => {}
                    };
                }

                // Add new utxos (this handles both transfers and issuances)
                for (txo_sid, (address, owner_memo)) in txo_sids
                    .iter()
                    .zip(addresses.iter().zip(owner_memos.iter()))
                {
                    self.addresses_to_utxos
                        .entry(*address)
                        .or_insert_with(HashSet::new)
                        .insert(*txo_sid);
                    self.utxos_to_map_index.insert(*txo_sid, *address);
                    let hash = curr_txn.hash_tm().hex().to_uppercase();
                    self.txo_to_txnid.insert(*txo_sid, (txn_sid, hash.clone()));
                    self.txn_sid_to_hash.insert(txn_sid, hash.clone());
                    self.txn_hash_to_sid.insert(hash.clone(), txn_sid);
                    if let Some(owner_memo) = owner_memo {
                        self.owner_memos.insert(*txo_sid, (*owner_memo).clone());
                    }
                }
            }
        }

        self.app_block_cnt = ledger.blocks.len();
        Ok(())
    }

    pub fn update(&mut self) {
        pnk!(self.apply_new_blocks());
    }
}
// An xfr address is related to a transaction if it is one of the following:
// 1. Owner of a transfer output
// 2. Transfer signer (owner of input or co-signer)
// 3. Signer of a an issuance txn
// 4. Signer of a kv_update txn
// 5. Signer of a memo_update txn
fn get_related_addresses<F>(txn: &Transaction, mut classify: F) -> HashSet<XfrAddress>
where
    F: FnMut(&Operation),
{
    let mut related_addresses = HashSet::new();

    macro_rules! staking_gen {
        ($op: expr) => {
            $op.get_related_pubkeys().into_iter().for_each(|pk| {
                related_addresses.insert(XfrAddress { key: pk });
            });
        };
    }

    for op in &txn.body.operations {
        classify(op);
        match op {
            Operation::UpdateStaker(i) => staking_gen!(i),
            Operation::Delegation(i) => staking_gen!(i),
            Operation::UnDelegation(i) => staking_gen!(i),
            Operation::Claim(i) => staking_gen!(i),
            Operation::UpdateValidator(i) => staking_gen!(i),
            Operation::Governance(i) => staking_gen!(i),
            Operation::FraDistribution(i) => staking_gen!(i),
            Operation::MintFra(i) => staking_gen!(i),

            Operation::TransferAsset(transfer) => {
                for input in transfer.body.transfer.inputs.iter() {
                    related_addresses.insert(XfrAddress {
                        key: input.public_key,
                    });
                }

                for output in transfer.body.transfer.outputs.iter() {
                    related_addresses.insert(XfrAddress {
                        key: output.public_key,
                    });
                }
            }
            Operation::IssueAsset(issue_asset) => {
                related_addresses.insert(XfrAddress {
                    key: issue_asset.pubkey.key,
                });
            }
            Operation::DefineAsset(define_asset) => {
                related_addresses.insert(XfrAddress {
                    key: define_asset.pubkey.key,
                });
            }
            Operation::UpdateMemo(update_memo) => {
                related_addresses.insert(XfrAddress {
                    key: update_memo.pubkey,
                });
            }
        }
    }
    related_addresses
}

// Returns the set of nonconfidential assets transferred in a transaction.
fn get_transferred_nonconfidential_assets(txn: &Transaction) -> HashSet<AssetTypeCode> {
    let mut transferred_assets = HashSet::new();
    for op in &txn.body.operations {
        if let Operation::TransferAsset(transfer) = op {
            for input in transfer.body.transfer.inputs.iter() {
                if let Some(asset_type) = input.asset_type.get_asset_type() {
                    transferred_assets.insert(AssetTypeCode { val: asset_type });
                }
            }
        }
    }
    transferred_assets
}
