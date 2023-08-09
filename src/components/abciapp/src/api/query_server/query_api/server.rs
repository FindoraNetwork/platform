//!
//! data sources for the query api
//!

use {
    globutils::HashOf,
    lazy_static::lazy_static,
    ledger::{
        data_model::{
            ATxoSID, AssetTypeCode, DefineAsset, IssuerPublicKey, StateCommitmentData,
            Transaction, TxOutput, TxnIDHash, TxnSID, TxoSID, XfrAddress,
        },
        staking::{ops::mint_fra::MintEntry, BlockHeight},
        store::LedgerState,
    },
    parking_lot::{Condvar, Mutex, RwLock},
    ruc::*,
    std::{collections::HashSet, sync::Arc},
    zei::{
        noah_api::anon_xfr::structs::{AxfrOwnerMemo, Commitment, MTLeafInfo},
        OwnerMemo
    },
};

lazy_static! {
    /// the query_server will be notified every time
    /// a block is added to the ledgerState to update the data
    pub static ref BLOCK_CREATED: Arc<(Mutex<bool>, Condvar)> =
        Arc::new((Mutex::new(false), Condvar::new()));
}

/// A data container for API
pub struct QueryServer {
    pub(crate) ledger: Arc<RwLock<LedgerState>>,
    pub(crate) ledger_cloned: LedgerState,
}

impl QueryServer {
    /// create query server
    pub fn new(ledger: Arc<RwLock<LedgerState>>) -> QueryServer {
        let ledger_cloned = ledger.read().clone();
        QueryServer {
            ledger,
            ledger_cloned,
        }
    }

    /// Returns the set of records issued by a certain key.
    #[inline(always)]
    pub fn get_issued_records(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .issuances
            .get(issuer)
    }

    /// Returns the set of records issued by a certain token code.
    #[inline(always)]
    pub fn get_issued_records_by_code(
        &self,
        code: &AssetTypeCode,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .token_code_issuances
            .get(code)
    }

    /// return `DefineAsset` according to `IssuerPublicKey`
    #[inline(always)]
    pub fn get_created_assets(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<Vec<DefineAsset>> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .created_assets
            .get(issuer)
            .map(|d| d.iter().map(|(_, v)| v).collect())
    }

    /// get coinbase based on address and sorting rules and start and end position
    pub fn get_coinbase_entries(
        &self,
        address: &XfrAddress,
        start: usize,
        end: usize,
        order_desc: bool,
    ) -> Result<(u64, Vec<(u64, MintEntry)>)> {
        if let Some(hist) = self
            .ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .coinbase_oper_hist
            .get(address)
        {
            let len = hist.len();
            if len > start {
                let slice = match order_desc {
                    false => {
                        let mut new_end = len;
                        if len > end {
                            new_end = end;
                        }
                        hist.iter()
                            .skip(start.saturating_sub(1))
                            .take((new_end + 1) - start)
                            .collect()
                    }
                    true => {
                        let mut new_start = 0;
                        if len > end {
                            new_start = len - end;
                        }
                        let mut tmp = hist
                            .iter()
                            .skip(new_start.saturating_sub(1))
                            .take((len - start + 1) - new_start)
                            .collect::<Vec<_>>();
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
        if let Some(hist) = self
            .ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .claim_hist_txns
            .get(address)
        {
            let len = hist.len();
            if len > start {
                let slice = match order_desc {
                    false => {
                        let mut new_end = len;
                        if len > end {
                            new_end = end;
                        }
                        hist.iter()
                            .skip(start.saturating_sub(1))
                            .take((new_end + 1) - start)
                            .map(|(k, _)| k)
                            .collect()
                    }
                    true => {
                        let mut new_start = 0;
                        if len > end {
                            new_start = len - end;
                        }
                        let mut tmp = hist
                            .iter()
                            .skip(new_start.saturating_sub(1))
                            .take((len - start + 1) - new_start)
                            .map(|(k, _)| k)
                            .collect::<Vec<_>>();
                        tmp.reverse();
                        tmp
                    }
                };

                return Ok(slice
                    .iter()
                    .map(|h| {
                        if let Ok(tx) =
                            ruc::info!(self.ledger_cloned.get_transaction_light(*h))
                        {
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
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .related_transactions
            .get(&address)
            .map(|d| d.iter().map(|(k, _)| k).collect())
    }

    /// Returns the set of transfer transactions that are associated with a given asset.
    /// The asset type must be nonconfidential.
    #[inline(always)]
    pub fn get_related_transfers(
        &self,
        code: &AssetTypeCode,
    ) -> Option<HashSet<TxnSID>> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .related_transfers
            .get(&code)
            .map(|d| d.iter().map(|(k, _)| k).collect())
    }

    /// Returns the owner of a given txo_sid.
    #[inline(always)]
    pub fn get_address_of_sid(&self, txo_sid: TxoSID) -> Option<XfrAddress> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .utxos_to_map_index
            .get(&txo_sid)
    }

    /// Returns the authenticated txn (id, hash) of a given txo_sid.
    #[inline(always)]
    pub fn get_authenticated_txnid(&self, txo_sid: TxoSID) -> Option<TxnIDHash> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .txo_to_txnid
            .get(&txo_sid)
    }

    /// Returns the transaction hash of a given txn_sid.
    #[inline(always)]
    pub fn get_transaction_hash(&self, txn_sid: TxnSID) -> Option<String> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .txn_sid_to_hash
            .get(&txn_sid)
    }

    /// Returns the transaction sid of a given txn_hash.
    #[inline(always)]
    pub fn get_transaction_sid(&self, txn_hash: String) -> Option<TxnSID> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .txn_hash_to_sid
            .get(&txn_hash)
    }

    /// Returns most recent commits at query_server side.
    #[inline(always)]
    pub fn get_commits(&self) -> u64 {
        self.ledger_cloned.get_block_commit_count()
    }

    /// Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
    #[inline(always)]
    pub fn get_owner_memo(&self, txo_sid: TxoSID) -> Option<OwnerMemo> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .owner_memos
            .get(&txo_sid)
    }

    /// Returns the abar owner memo required to decrypt the asset record stored at given index, if it exists.
    #[inline(always)]
    pub fn get_abar_memo(&self, atxo_sid: ATxoSID) -> Option<AxfrOwnerMemo> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .and_then(|api| api.abar_memos.get(&atxo_sid))
    }

    /// Returns the owner memos required to decrypt the asset record stored at between start and end,
    /// include start and end, limit 100.
    #[inline(always)]
    pub fn get_abar_memos(&self, start: u64, end: u64) -> Vec<(u64, AxfrOwnerMemo)> {
        let mut memos = vec![];
        let cache = self.ledger_cloned.api_cache.as_ref().unwrap();
        for i in start..=end {
            if let Some(memo) = cache.abar_memos.get(&ATxoSID(i)) {
                memos.push((i, memo));
            }
        }
        memos
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_state_commitment_from_api_cache(
        &self,
    ) -> (HashOf<Option<StateCommitmentData>>, u64) {
        let block_count = self.ledger_cloned.get_block_commit_count();
        let commitment = self
            .ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .state_commitment_version
            .clone()
            .unwrap_or_else(|| HashOf::new(&None));
        (commitment, block_count)
    }

    /// Returns the abar commitment by given index, if it exists.
    pub fn get_abar_commitment(&self, atxo_sid: ATxoSID) -> Option<Commitment> {
        self.ledger_cloned.get_abar(&atxo_sid)
    }

    /// Returns the merkle proof from the given ATxoSID
    #[inline(always)]
    pub fn get_abar_proof(&self, atxo_sid: ATxoSID) -> Option<MTLeafInfo> {
        self.ledger_cloned.get_abar_proof(atxo_sid).ok()
    }

    /// Returns a bool value from the given hash
    #[inline(always)]
    pub fn check_nullifier_hash(&self, null_hash: String) -> Option<bool> {
        self.ledger_cloned.check_nullifier_hash(null_hash).ok()
    }

    /// Returns an int value for the max ATxoSid
    #[inline(always)]
    pub fn max_atxo_sid(&self) -> Option<usize> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .and_then(|api| api.abar_memos.len().checked_sub(1))
    }

    /// Returns an int value for the max ATxoSid at a given block height
    #[inline(always)]
    pub fn max_atxo_sid_at_height(&self, height: BlockHeight) -> Option<usize> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .and_then(|api| api.height_to_max_atxo.get(&height).unwrap_or(None))
    }

    /// retrieve block reward rate at specified block height
    #[inline(always)]
    pub fn query_block_rewards_rate(&self, height: &BlockHeight) -> Option<[u128; 2]> {
        self.ledger_cloned
            .api_cache
            .as_ref()
            .unwrap()
            .staking_global_rate_hist
            .get(height)
    }

    /// update after a new block is created
    #[inline(always)]
    pub fn update(&mut self) {
        if let Some(l) = self.ledger.try_read() {
            self.ledger_cloned = l.clone();
        }
    }
}
