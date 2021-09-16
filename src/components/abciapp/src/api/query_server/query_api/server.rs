//!
//! data sources for the query api
//!

use globutils::wallet;
use lazy_static::lazy_static;
use ledger::{
    data_model::{
        AssetTypeCode, DefineAsset, IssueAsset, IssuerPublicKey, Operation, Transaction,
        TransferAsset, TxOutput, TxnSID, TxoRef, TxoSID, XfrAddress,
    },
    staking::{
        ops::mint_fra::MintEntry, Amount, BlockHeight, DelegationRwdDetail,
        CHAN_D_AMOUNT_HIST, CHAN_D_RWD_HIST, CHAN_GLOB_RATE_HIST, CHAN_V_SELF_D_HIST,
    },
    store::{
        bnc::{self, new_mapx, Mapx},
        flush_data, LedgerState,
    },
};
use parking_lot::{Condvar, Mutex, RwLock};
use ruc::*;
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, fs, io::ErrorKind, sync::Arc};
use zei::xfr::{sig::XfrPublicKey, structs::OwnerMemo};

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
    pub(crate) state: Option<Arc<RwLock<LedgerState>>>,
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

    // global rate history
    pub(crate) staking_global_rate_hist: Mapx<BlockHeight, [u128; 2]>,

    // - self-delegation amount history
    //   - `NonConfidential` FRAs amount
    //   - only valid for validators
    pub(crate) staking_self_delegation_hist:
        Mapx<XfrPublicKey, Mapx<BlockHeight, Amount>>,

    // - delegation amount per block height
    // - only valid for a validator
    pub(crate) staking_delegation_amount_hist:
        Mapx<XfrPublicKey, Mapx<BlockHeight, Amount>>,

    // rewards history, used on some pulic nodes, such as fullnode
    pub(crate) staking_delegation_rwd_hist:
        Mapx<XfrPublicKey, Mapx<BlockHeight, DelegationRwdDetail>>,

    app_block_cnt: usize,
}

impl QueryServer {
    /// create query server
    pub fn new(
        ledger: Arc<RwLock<LedgerState>>,
        base_dir: Option<&str>,
    ) -> Result<QueryServer> {
        let base_dir = if let Some(path) = base_dir {
            format!("{}/query_server", path)
        } else {
            bnc::clear();
            globutils::fresh_tmp_dir()
                .join("/test_query_server")
                .to_string_lossy()
                .into_owned()
        };

        fs::create_dir_all(&base_dir).c(d!())?;
        let snapshot_path = base_dir + "/query_server";

        match fs::read_to_string(&snapshot_path) {
            Ok(s) => serde_json::from_str(&s).c(d!()).map(|mut r: QueryServer| {
                r.state = Some(ledger);
                r
            }),
            Err(e) => {
                if ErrorKind::NotFound != e.kind() {
                    Err(eg!(e))
                } else {
                    Ok(Self::create(ledger, snapshot_path))
                }
            }
        }
    }

    fn create(ledger: Arc<RwLock<LedgerState>>, snapshot_path: String) -> QueryServer {
        QueryServer {
            state: Some(ledger),
            snapshot_path,
            addresses_to_utxos: new_mapx!("addresses_to_utxos"),
            related_transactions: new_mapx!("related_transactions"),
            related_transfers: new_mapx!("related_transfers"),
            claim_hist_txns: new_mapx!("claim_hist_txns"),
            coinbase_oper_hist: new_mapx!("coinbase_oper_hist"),
            owner_memos: new_mapx!("owner_memos"),
            created_assets: new_mapx!("created_assets"),
            issuances: new_mapx!("issuances"),
            token_code_issuances: new_mapx!("token_code_issuances"),
            utxos_to_map_index: new_mapx!("utxos_to_map_index"),
            txo_to_txnid: new_mapx!("txo_to_txnid"),
            txn_sid_to_hash: new_mapx!("txn_sid_to_hash"),
            txn_hash_to_sid: new_mapx!("txn_hash_to_sid"),
            staking_global_rate_hist: new_mapx!("staking_rate_hist"),
            staking_self_delegation_hist: new_mapx!("staking_self_delegation_hist"),
            staking_delegation_amount_hist: new_mapx!("staking_delegation_amount_hist"),
            staking_delegation_rwd_hist: new_mapx!("staking_rwd_hist"),
            app_block_cnt: 0,
        }
    }

    /// Returns the set of records issued by a certain key.
    #[inline(always)]
    pub fn get_issued_records(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.issuances.get(issuer)
    }

    /// Returns the set of records issued by a certain token code.
    #[inline(always)]
    pub fn get_issued_records_by_code(
        &self,
        code: &AssetTypeCode,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.token_code_issuances.get(code)
    }

    /// return `DefineAsset` according to `IssuerPublicKey`
    #[inline(always)]
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

                let ledger = Arc::clone(self.state.as_ref().unwrap());
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
    #[inline(always)]
    pub fn get_related_transactions(
        &self,
        address: &XfrAddress,
    ) -> Option<HashSet<TxnSID>> {
        self.related_transactions.get(&address)
    }

    /// Returns the set of transfer transactions that are associated with a given asset.
    /// The asset type must be nonconfidential.
    #[inline(always)]
    pub fn get_related_transfers(
        &self,
        code: &AssetTypeCode,
    ) -> Option<HashSet<TxnSID>> {
        self.related_transfers.get(&code)
    }

    /// Returns the set of TxoSIDs that are the indices of records owned by a given address.
    #[inline(always)]
    pub fn get_owned_utxo_sids(&self, address: &XfrAddress) -> Option<HashSet<TxoSID>> {
        self.addresses_to_utxos.get(&address)
    }

    /// Returns the owner of a given txo_sid.
    #[inline(always)]
    pub fn get_address_of_sid(&self, txo_sid: TxoSID) -> Option<XfrAddress> {
        self.utxos_to_map_index.get(&txo_sid)
    }

    /// Returns the authenticated txn (id, hash) of a given txo_sid.
    #[inline(always)]
    pub fn get_authenticated_txnid(&self, txo_sid: TxoSID) -> Option<TxnIDHash> {
        self.txo_to_txnid.get(&txo_sid)
    }

    /// Returns the transaction hash of a given txn_sid.
    #[inline(always)]
    pub fn get_transaction_hash(&self, txn_sid: TxnSID) -> Option<String> {
        self.txn_sid_to_hash.get(&txn_sid)
    }

    /// Returns the transaction sid of a given txn_hash.
    #[inline(always)]
    pub fn get_transaction_sid(&self, txn_hash: String) -> Option<TxnSID> {
        self.txn_hash_to_sid.get(&txn_hash)
    }

    /// Returns most recent commits at query_server side.
    #[inline(always)]
    pub fn get_commits(&self) -> u64 {
        self.state.as_ref().unwrap().read().get_block_commit_count()
    }

    /// Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
    #[inline(always)]
    pub fn get_owner_memo(&self, txo_sid: TxoSID) -> Option<OwnerMemo> {
        self.owner_memos.get(&txo_sid)
    }

    /// Add created asset
    #[inline(always)]
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

    fn cache_hist_data(&mut self) {
        CHAN_GLOB_RATE_HIST.1.lock().try_iter().for_each(|(h, r)| {
            self.staking_global_rate_hist.insert(h, r);
        });

        CHAN_V_SELF_D_HIST
            .1
            .lock()
            .try_iter()
            .for_each(|(pk, h, r)| {
                self.staking_self_delegation_hist
                    .entry(pk)
                    .or_insert(new_mapx!(format!(
                        "staking_self_delegation_hist_subdir/{}",
                        wallet::public_key_to_base64(&pk)
                    )))
                    .insert(h, r);
            });

        CHAN_D_AMOUNT_HIST
            .1
            .lock()
            .try_iter()
            .for_each(|(pk, h, r)| {
                self.staking_delegation_amount_hist
                    .entry(pk)
                    .or_insert(new_mapx!(format!(
                        "staking_delegation_amount_hist_subdir/{}",
                        wallet::public_key_to_base64(&pk)
                    )))
                    .insert(h, r);
            });

        CHAN_D_RWD_HIST.1.lock().try_iter().for_each(|(pk, h, r)| {
            #[allow(unused_mut)]
            let mut dd =
                self.staking_delegation_rwd_hist
                    .entry(pk)
                    .or_insert(new_mapx!(format!(
                        "staking_delegation_rwd_hist_subdir/{}",
                        wallet::public_key_to_base64(&pk)
                    )));
            let mut dd = dd.entry(h).or_insert_with(DelegationRwdDetail::default);

            dd.block_height = r.block_height;
            dd.amount += r.amount;
            dd.penalty_amount += r.penalty_amount;

            alt!(0 < r.bond, dd.bond = r.bond);
            alt!(r.return_rate.is_some(), dd.return_rate = r.return_rate);
            alt!(
                r.commission_rate.is_some(),
                dd.commission_rate = r.commission_rate
            );
            alt!(
                r.global_delegation_percent.is_some(),
                dd.global_delegation_percent = r.global_delegation_percent
            );
        });
    }

    /// Updates query server cache with new transactions from a block.
    /// Each new block must be consistent with the state of the cached ledger up until this point
    fn apply_new_blocks(&mut self) -> Result<()> {
        self.cache_hist_data();

        let ledger = Arc::clone(self.state.as_ref().unwrap());
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

        flush_data();

        // snapshot them finally
        serde_json::to_vec(&self).c(d!()).and_then(|s| {
            fs::write(&self.snapshot_path, s).c(d!(self.snapshot_path.clone()))
        })
    }

    /// update data of query server
    /// call update when the block into end_block and commit to ledgerState
    #[inline(always)]
    pub fn update(&mut self) {
        pnk!(self.apply_new_blocks());
    }

    /// retrieve block reward rate at specified block height
    #[inline(always)]
    pub fn query_block_rewards_rate(&self, height: &BlockHeight) -> Option<[u128; 2]> {
        self.staking_global_rate_hist.get(height)
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

            Operation::ConvertAccount(i) => {
                related_addresses.insert(XfrAddress {
                    key: i.get_related_address(),
                });
            }
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
