//!
//! data sources for the query api
//!

use lazy_static::lazy_static;
use ledger::{
    data_model::{
        AssetTypeCode, DefineAsset, IssueAsset, IssuerPublicKey, Operation, Transaction,
        TransferAsset, TxOutput, TxnSID, TxoRef, TxoSID, XfrAddress,
    },
    staking::{ops::mint_fra::MintEntry, BlockHeight},
    store::{
        bnc::{new_mapx, Mapx},
        LedgerState,
    },
};
use parking_lot::{Condvar, Mutex, RwLock};
use ruc::*;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashSet, env, fs, io::ErrorKind, ops::Deref, path::Path, sync::Arc,
};
use zei::xfr::structs::OwnerMemo;

lazy_static! {
    /// the query_server will be notified every time
    /// a block is added to the ledgerState to update the data
    pub static ref BLOCK_CREATED: Arc<(Mutex<bool>, Condvar)> =
        Arc::new((Mutex::new(false), Condvar::new()));
}

/// (sid, hash)
pub type TxnIDHash = (TxnSID, String);

type Issuances = Vec<(TxOutput, Option<OwnerMemo>)>;

/// data from ledgerState
#[derive(Serialize, Deserialize)]
pub struct QueryServer {
    snapshot_path: String,
    #[serde(skip)]
    committed_state: Option<Arc<RwLock<LedgerState>>>,
    addresses_to_utxos: Mapx<XfrAddress, HashSet<TxoSID>>,
    related_transactions: Mapx<XfrAddress, HashSet<TxnSID>>, // Set of transactions related to a ledger address
    related_transfers: Mapx<AssetTypeCode, HashSet<TxnSID>>, // Set of transfer transactions related to an asset code
    claim_hist_txns: Mapx<XfrAddress, Vec<TxnSID>>, // List of claim transactions related to a ledger address
    coinbase_oper_hist: Mapx<XfrAddress, Vec<(BlockHeight, MintEntry)>>,
    created_assets: Mapx<IssuerPublicKey, Vec<DefineAsset>>,
    issuances: Mapx<IssuerPublicKey, Issuances>, // issuance mapped by public key
    token_code_issuances: Mapx<AssetTypeCode, Issuances>, // issuance mapped by token code
    owner_memos: Mapx<TxoSID, OwnerMemo>,
    utxos_to_map_index: Mapx<TxoSID, XfrAddress>,
    txo_to_txnid: Mapx<TxoSID, TxnIDHash>, // txo(spent, unspent) to authenticated txn (sid, hash)
    txn_sid_to_hash: Mapx<TxnSID, String>, // txn sid to txn hash
    txn_hash_to_sid: Mapx<String, TxnSID>, // txn hash to txn sid
    app_block_cnt: usize,
}

impl QueryServer {
    /// create query server
    pub fn new(
        ledger: Arc<RwLock<LedgerState>>,
        base_dir: Option<&Path>,
    ) -> Result<QueryServer> {
        let base_dir = if let Some(path) = base_dir {
            path.to_str().unwrap().to_string() + "/query_server"
        } else {
            pnk!(env::var("tmp_dir").c(d!())) + "/test_query_server"
        };

        let snapshot_path = base_dir.to_string() + "/query_server";

        match fs::read_to_string(&snapshot_path) {
            Ok(s) => serde_json::from_str(&s).c(d!()).map(|mut r: QueryServer| {
                r.committed_state = Some(ledger);
                r
            }),
            Err(e) => {
                if ErrorKind::NotFound != e.kind() {
                    Err(eg!(e))
                } else {
                    Ok(Self::create(ledger, base_dir, snapshot_path))
                }
            }
        }
    }

    fn create(
        ledger: Arc<RwLock<LedgerState>>,
        base_dir: String,
        snapshot_path: String,
    ) -> QueryServer {
        let addresses_to_utxos_path = base_dir.to_string() + "/addresses_to_utxos";
        let related_transactions_path = base_dir.to_string() + "/related_transactions";
        let related_transfers_path = base_dir.to_string() + "/related_transfers";
        let claim_hist_txns_path = base_dir.to_string() + "/claim_hist_txns";
        let coinbase_oper_hist_path = base_dir.to_string() + "/coinbase_oper_hist";
        let owner_memos_path = base_dir.to_string() + "/owner_memos";
        let created_assets_path = base_dir.to_string() + "/created_assets";
        let issuances_path = base_dir.to_string() + "/issuances";
        let token_code_issuances_path = base_dir.to_string() + "/token_code_issuances";
        let utxos_to_map_index_path = base_dir.to_string() + "/utxos_to_map_index";
        let txo_to_txnid_path = base_dir.to_string() + "/txo_to_txnid";
        let txn_sid_to_hash_path = base_dir.to_string() + "/txn_sid_to_hash";
        let txn_hash_to_sid_path = base_dir + "/txn_hash_to_sid";

        QueryServer {
            committed_state: Some(ledger),
            snapshot_path,
            addresses_to_utxos: new_mapx!(addresses_to_utxos_path.as_str()),
            related_transactions: new_mapx!(related_transactions_path.as_str()),
            related_transfers: new_mapx!(related_transfers_path.as_str()),
            claim_hist_txns: new_mapx!(claim_hist_txns_path.as_str()),
            coinbase_oper_hist: new_mapx!(coinbase_oper_hist_path.as_str()),
            owner_memos: new_mapx!(owner_memos_path.as_str()),
            created_assets: new_mapx!(created_assets_path.as_str()),
            issuances: new_mapx!(issuances_path.as_str()),
            token_code_issuances: new_mapx!(token_code_issuances_path.as_str()),
            utxos_to_map_index: new_mapx!(utxos_to_map_index_path.as_str()),
            txo_to_txnid: new_mapx!(txo_to_txnid_path.as_str()),
            txn_sid_to_hash: new_mapx!(txn_sid_to_hash_path.as_str()),
            txn_hash_to_sid: new_mapx!(txn_hash_to_sid_path.as_str()),
            app_block_cnt: 0,
        }
    }

    /// Returns the set of records issued by a certain key.
    pub fn get_issued_records(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.issuances
            .get(issuer)
            .map(|recs| recs.deref().iter().map(|rec| rec.deref().clone()).collect())
    }

    /// Returns the set of records issued by a certain token code.
    pub fn get_issued_records_by_code(
        &self,
        code: &AssetTypeCode,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.token_code_issuances
            .get(code)
            .map(|recs| recs.deref().iter().map(|rec| rec.deref().clone()).collect())
    }

    /// return `DefineAsset` according to `IssuerPublicKey`
    pub fn get_created_assets(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<Vec<DefineAsset>> {
        self.created_assets.get(issuer)
    }

    /// get coinbase based on address and sorting rules and start and end position
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

    /// Returns a list of claim transactions of a given ledger address
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

                let ledger = Arc::clone(self.committed_state.as_ref().unwrap());
                let ledger = ledger.read();

                return Ok(slice
                    .iter()
                    .map(|h| {
                        if let Ok(tx) = ruc::info!(ledger.get_transaction_light(*h)) {
                            Some(tx.txn)
                        } else {
                            None
                        }
                    })
                    .collect());
            }
        }

        Err(eg!("Record not found"))
    }

    /// Returns the set of transactions that are in some way related to a given ledger address.
    /// An xfr address is related to a transaction if it is one of the following:
    /// 1. Owner of a transfer output
    /// 2. Transfer signer (owner of input or co-signer)
    /// 3. Signer of a an issuance txn
    /// 4. Signer of a kv_update txn
    /// 5. Signer of a memo_update txn
    pub fn get_related_transactions(
        &self,
        address: &XfrAddress,
    ) -> Option<HashSet<TxnSID>> {
        self.related_transactions.get(&address)
    }

    /// Returns the set of transfer transactions that are associated with a given asset.
    /// The asset type must be nonconfidential.
    pub fn get_related_transfers(
        &self,
        code: &AssetTypeCode,
    ) -> Option<HashSet<TxnSID>> {
        self.related_transfers.get(&code)
    }

    /// Returns the set of TxoSIDs that are the indices of records owned by a given address.
    pub fn get_owned_utxo_sids(&self, address: &XfrAddress) -> Option<HashSet<TxoSID>> {
        self.addresses_to_utxos.get(&address)
    }

    /// Returns the owner of a given txo_sid.
    pub fn get_address_of_sid(&self, txo_sid: TxoSID) -> Option<XfrAddress> {
        self.utxos_to_map_index.get(&txo_sid)
    }

    /// Returns the authenticated txn (id, hash) of a given txo_sid.
    pub fn get_authenticated_txnid(&self, txo_sid: TxoSID) -> Option<TxnIDHash> {
        self.txo_to_txnid.get(&txo_sid)
    }

    /// Returns the transaction hash of a given txn_sid.
    pub fn get_transaction_hash(&self, txn_sid: TxnSID) -> Option<String> {
        self.txn_sid_to_hash.get(&txn_sid)
    }

    /// Returns the transaction sid of a given txn_hash.
    pub fn get_transaction_sid(&self, txn_hash: String) -> Option<TxnSID> {
        self.txn_hash_to_sid.get(&txn_hash)
    }

    /// Returns most recent commits at query_server side.
    pub fn get_commits(&self) -> u64 {
        self.committed_state
            .as_ref()
            .unwrap()
            .read()
            .get_block_commit_count()
    }

    /// Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
    pub fn get_owner_memo(&self, txo_sid: TxoSID) -> Option<OwnerMemo> {
        self.owner_memos.get(&txo_sid)
    }

    /// Add created asset
    pub fn add_created_asset(&mut self, creation: &DefineAsset) {
        let issuer = creation.pubkey;
        #[allow(unused_mut)]
        let mut set = self.created_assets.entry(issuer).or_insert_with(Vec::new);

        set.push(creation.clone());
        set.sort_by_key(|i| i.pubkey);
        set.dedup_by_key(|i| i.body.asset.code);
    }

    /// Cache issuance records
    pub fn cache_issuance(&mut self, issuance: &IssueAsset) {
        let new_records = issuance.body.records.to_vec();

        macro_rules! save_issuance {
            ($maps: tt, $key: tt) => {
                #[allow(unused_mut)]
                let mut records = $maps.entry($key).or_insert_with(Vec::new);
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

    fn remove_spent_utxos(&mut self, transfer: &TransferAsset) {
        for input in &transfer.body.inputs {
            match input {
                TxoRef::Relative(_) => {} // Relative utxos were never cached so no need to do anything here
                TxoRef::Absolute(txo_sid) => {
                    if let Some(addr) = self.utxos_to_map_index.get(&txo_sid) {
                        #[allow(unused_mut)]
                        if let Some(mut hash_set) =
                            self.addresses_to_utxos.get_mut(&addr)
                        {
                            hash_set.remove(&txo_sid);
                        } else {
                            ruc::pd!("No txos stored for this address");
                        }
                    } else {
                        ruc::pd!("Attempting to remove owned txo of address that isn't cached");
                    }
                }
            }
        }
    }

    /// Updates query server cache with new transactions from a block.
    /// Each new block must be consistent with the state of the cached ledger up until this point
    fn apply_new_blocks(&mut self) -> Result<()> {
        let ledger = Arc::clone(self.committed_state.as_ref().unwrap());
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
                let curr_txn = ledger.get_transaction_light(txn_sid).c(d!())?.txn;
                // get the transaction, ownership addresses, and memos associated with each transaction
                let (addresses, owner_memos) = {
                    let addresses: Vec<XfrAddress> = txo_sids
                        .iter()
                        .map(|sid| XfrAddress {
                            key: ((ledger
                                .get_utxo_light(*sid)
                                .or_else(|| ledger.get_spent_utxo_light(*sid))
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
                            #[allow(unused_mut)]
                            let mut hist = self
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
                            #[allow(unused_mut)]
                            let mut hist = self
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
                        }
                        Operation::IssueAsset(issue_asset) => {
                            self.cache_issuance(&issue_asset);
                        }
                        Operation::TransferAsset(transfer_asset) => {
                            self.remove_spent_utxos(&transfer_asset);
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
        self.flush();

        // snapshot them finally
        serde_json::to_vec(&self)
            .c(d!())
            .and_then(|s| fs::write(&self.snapshot_path, s).c(d!()))
    }

    fn flush(&mut self) {
        self.addresses_to_utxos.flush_data();
        self.related_transactions.flush_data();
        self.related_transfers.flush_data();
        self.claim_hist_txns.flush_data();
        self.coinbase_oper_hist.flush_data();
        self.created_assets.flush_data();
        self.issuances.flush_data();
        self.token_code_issuances.flush_data();
        self.owner_memos.flush_data();
        self.utxos_to_map_index.flush_data();
        self.txo_to_txnid.flush_data();
        self.txn_sid_to_hash.flush_data();
        self.txn_hash_to_sid.flush_data();
    }

    /// update data of query server
    /// call update when the block into end_block and commit to ledgerState
    pub fn update(&mut self) {
        pnk!(self.apply_new_blocks());
    }
}

/// An xfr address is related to a transaction if it is one of the following:
/// 1. Owner of a transfer output
/// 2. Transfer signer (owner of input or co-signer)
/// 3. Signer of a an issuance txn
/// 4. Signer of a kv_update txn
/// 5. Signer of a memo_update txn
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

/// Returns the set of nonconfidential assets transferred in a transaction.
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
