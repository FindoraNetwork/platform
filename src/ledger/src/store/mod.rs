//!
//! # Findora ledger store implementation
//!
//!

pub mod helpers;
mod test;
pub mod utils;

pub use bnc;

use crate::{
    data_model::{
        AssetType, AssetTypeCode, AuthenticatedBlock, AuthenticatedTransaction,
        AuthenticatedUtxo, AuthenticatedUtxoStatus, BlockEffect, BlockSID,
        FinalizedBlock, FinalizedTransaction, IssuerKeyPair, IssuerPublicKey,
        OutputPosition, StateCommitmentData, Transaction, TransferType, TxnEffect,
        TxnSID, TxnTempSID, TxoSID, UnAuthenticatedUtxo, Utxo, UtxoStatus,
        BLACK_HOLE_PUBKEY,
    },
    staking::{Amount, Power, Staking, TendermintAddrRef, FF_PK_LIST, FRA_TOTAL_AMOUNT},
};
use bitmap::{BitMap, SparseMap};
use bnc::{new_mapx, new_vecx, Mapx, Vecx};
use cryptohash::sha256::Digest as BitDigest;
use globutils::{HashOf, ProofOf};
use merkle_tree::AppendOnlyMerkle;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ruc::*;
use serde::{Deserialize, Serialize};
use sliding_set::SlidingSet;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fs::{self, File, OpenOptions},
    io::{BufRead, BufReader, ErrorKind},
    mem,
    ops::{Deref, DerefMut},
};
use zei::xfr::{
    lib::XfrNotePolicies,
    sig::XfrPublicKey,
    structs::{OwnerMemo, TracingPolicies, TracingPolicy, XfrAmount},
};

const TRANSACTION_WINDOW_WIDTH: u64 = 128;

type TmpSidMap = HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>;

/// The findora ledger in-memory representative.
pub struct LedgerState {
    // major part of State
    status: LedgerStatus,
    // Merkle tree tracing the sequence of transaction hashes in the block
    // Each appended hash is the hash of transactions in the same block
    block_merkle: AppendOnlyMerkle,
    // Merkle tree tracing the sequence of all transaction hashes
    // Each appended hash is the hash of a transaction
    txn_merkle: AppendOnlyMerkle,
    /// The `FinalizedTransaction`s consist of a Transaction and an index into
    /// `merkle` representing its hash.
    pub blocks: Vecx<FinalizedBlock>,
    /// <tx id> => [<block id>, <tx idx in block>]
    pub tx_to_block_location: Mapx<TxnSID, [usize; 2]>,
    // Bitmap tracing all the live TXOs
    utxo_map: BitMap,
    // current block effect (middle cache)
    block_ctx: Option<BlockEffect>,

    prng: ChaChaRng,
}

impl LedgerState {
    #[inline(always)]
    fn fast_invariant_check(&self) -> Result<()> {
        self.status.fast_invariant_check().c(d!())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_prng(&mut self) -> &mut ChaChaRng {
        &mut self.prng
    }

    /// Consume a block context and assemble a BlockEffect
    #[inline(always)]
    pub fn start_block(&mut self) -> Result<BlockEffect> {
        self.block_ctx
            .take()
            .map(|mut b| {
                *b.get_staking_simulator_mut() = self.get_staking().clone();
                b
            })
            .c(d!())
    }

    /// Check tx of a block context, and apply it to current block
    pub fn apply_transaction(
        &self,
        block: &mut BlockEffect,
        txe: TxnEffect,
        is_loading: bool,
    ) -> Result<TxnTempSID> {
        let tx = txe.txn.clone();
        self.status
            .check_txn_effects(&txe)
            .c(d!())
            .and_then(|_| block.add_txn_effect(txe, is_loading).c(d!()))
            .and_then(|tmpid| {
                // NOTE: set at the last position
                if is_loading {
                    Ok(tmpid)
                } else {
                    block
                        .staking_simulator
                        .coinbase_check_and_pay(&tx)
                        .c(d!())
                        .map(|_| tmpid)
                }
            })
    }

    // Update the UTXO bitmap
    fn update_utxo_map(
        &mut self,
        base_sid: u64,
        max_sid: u64,
        txn_temp_sids: &[TxnTempSID],
        tsm: &TmpSidMap,
    ) -> Result<()> {
        // This is, unfortunately, some horrible index-walking messiness.
        // The core idea is that we walk over every new TXO SID (ix), tracing:
        //  - by `temp_sid_ix`, which transaction we're in
        //  - by `txo_sid_ix`, which UTXO within that transaction is next.
        let mut temp_sid_ix = 0;
        let mut txo_sid_ix = 0;

        // Find the first index that matters
        while temp_sid_ix < txn_temp_sids.len()
            && (tsm[&txn_temp_sids[temp_sid_ix]].1).is_empty()
        {
            temp_sid_ix += 1;
        }

        for ix in base_sid..max_sid {
            let temp_sid = txn_temp_sids[temp_sid_ix];
            let utxo_sids = &tsm[&temp_sid].1;

            // Only .set() extends the bitmap, so to append a 0 we currently
            // nead to .set() then .clear().
            self.utxo_map.set(ix as usize).c(d!())?;
            if let Some(TxoSID(utxo_sid)) = utxo_sids.get(txo_sid_ix) {
                if *utxo_sid != ix {
                    self.utxo_map.clear(ix as usize).c(d!())?;
                } else {
                    txo_sid_ix += 1;

                    // We've reached the end of this UTXO list, search for the next
                    // relevant one
                    if txo_sid_ix == utxo_sids.len() {
                        txo_sid_ix = 0;

                        temp_sid_ix += 1;
                        while temp_sid_ix < txn_temp_sids.len()
                            && (tsm[&txn_temp_sids[temp_sid_ix]].1).is_empty()
                        {
                            temp_sid_ix += 1;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn update_state(&mut self, mut block: BlockEffect, tsm: &TmpSidMap) -> Result<()> {
        let mut tx_block = Vec::new();

        // Update the transaction Merkle tree
        // Store the location of each utxo so we can create authenticated utxo proofs
        for (tmp_sid, txn) in block.temp_sids.iter().zip(block.txns.iter()) {
            let txn = txn.clone();
            let txo_sid_map = tsm.get(&tmp_sid).c(d!())?;
            let txn_sid = txo_sid_map.0;
            let txo_sids = &txo_sid_map.1;

            let merkle_id = self
                .txn_merkle
                .append_hash(&txn.hash(txn_sid).0.hash.into())
                .c(d!())?;

            tx_block.push(FinalizedTransaction {
                txn: txn.clone(),
                tx_id: txn_sid,
                txo_ids: txo_sids.clone(),
                merkle_id,
            });

            for (position, sid) in txo_sids.iter().enumerate() {
                self.status
                    .txo_to_txn_location
                    .insert(*sid, (txn_sid, OutputPosition(position)));
            }
        }

        // Checkpoint
        let block_merkle_id = self.checkpoint(&block).c(d!())?;
        block.temp_sids.clear();
        block.txns.clear();

        let block_idx = self.blocks.len();
        tx_block.iter().enumerate().for_each(|(tx_idx, tx)| {
            self.tx_to_block_location
                .insert(tx.tx_id, [block_idx, tx_idx]);
        });

        self.blocks.push(FinalizedBlock {
            txns: tx_block,
            merkle_id: block_merkle_id,
            state: self.status.state_commitment_data.clone().c(d!())?,
        });

        mem::swap(
            &mut block.staking_simulator,
            self.get_staking_mut().deref_mut(),
        );

        self.block_ctx = Some(block);

        Ok(())
    }

    /// Finish current block, peform following operations:
    ///    Invalid current input utxos
    ///    Apply current block to ledger status
    ///    Update Utxo map
    pub fn finish_block(&mut self, mut block: BlockEffect) -> Result<TmpSidMap> {
        for (inp_sid, _) in block.input_txos.iter() {
            self.utxo_map.clear(inp_sid.0 as usize).c(d!())?;
        }

        let (tsm, base_sid, max_sid) = self.status.apply_block_effects(&mut block);

        self.update_utxo_map(base_sid, max_sid, &block.temp_sids, &tsm)
            .c(d!())
            .and_then(|_| self.update_state(block, &tsm).c(d!()))
            .map(|_| tsm)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_staking_mut(&mut self) -> &mut Staking {
        &mut self.status.staking
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_tendermint_commit(&mut self, tendermint_h: u64) {
        self.status.td_commit_height = tendermint_h;
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_tendermint_height(&self) -> u64 {
        self.status.td_commit_height
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_next_txn(&self) -> TxnSID {
        self.status.next_txn
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_next_txo(&self) -> TxoSID {
        self.status.next_txo
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_status(&self) -> &LedgerStatus {
        &self.status
    }

    /// create a tmp ledger for testing purpose
    pub fn tmp_ledger() -> LedgerState {
        bnc::clear();
        let tmp_dir = globutils::fresh_tmp_dir().to_string_lossy().into_owned();
        LedgerState::new(&tmp_dir, Some(String::from("test"))).unwrap()
    }

    fn load_transaction_log(path: &str) -> Result<Vec<LoggedBlock>> {
        let file = File::open(path).c(d!())?;
        let reader = BufReader::new(file);
        let mut v = Vec::new();
        for l in reader.lines() {
            let l = l.c(d!())?;
            match serde_json::from_str::<LoggedBlock>(&l) {
                Ok(next_block) => {
                    v.push(next_block);
                }
                Err(e) => {
                    if !l.is_empty() {
                        return Err(eg!(format!("{:?} (deserializing '{:?}')", e, &l)));
                    }
                }
            }
        }
        Ok(v)
    }

    // In this functionn:
    //  1. Compute the hash of transactions in the block and update txns_in_block_hash
    //  2. Append txns_in_block_hash to block_merkle
    #[inline(always)]
    fn compute_and_append_txns_hash(&mut self, block: &BlockEffect) -> u64 {
        // 1. Compute the hash of transactions in the block and update txns_in_block_hash
        let txns_in_block_hash = block.compute_txns_in_block_hash();
        self.status.txns_in_block_hash = Some(txns_in_block_hash.clone());

        // 2. Append txns_in_block_hash to block_merkle
        //  2.1 Update the block Merkle tree
        let ret = self
            .block_merkle
            .append_hash(&txns_in_block_hash.0.hash.into())
            .unwrap();

        ret
    }

    fn compute_and_save_state_commitment_data(&mut self, pulse_count: u64) {
        let state_commitment_data = StateCommitmentData {
            bitmap: self.utxo_map.compute_checksum(),
            block_merkle: self.block_merkle.get_root_hash(),
            transaction_merkle_commitment: self.txn_merkle.get_root_hash(),
            txns_in_block_hash: self
                .status
                .txns_in_block_hash
                .as_ref()
                .cloned()
                .unwrap(),
            previous_state_commitment: HashOf::new(&self.status.state_commitment_data),
            air_commitment: BitDigest::from_slice(&[0; 32][..]).unwrap(),
            txo_count: self.get_next_txo().0,
            pulse_count,
            staking: alt!(
                self.get_staking().has_been_inited(),
                Some(HashOf::new(self.get_staking())),
                None
            ),
        };

        self.status
            .state_commitment_versions
            .push(state_commitment_data.compute_commitment());
        self.status.state_commitment_data = Some(state_commitment_data);
        self.status.incr_block_commit_count();
    }

    // Initialize a logged Merkle tree for the ledger.
    // We might be creating a new tree or opening an existing one.
    #[inline(always)]
    fn init_merkle_log(path: &str) -> Result<AppendOnlyMerkle> {
        AppendOnlyMerkle::open(path)
            .c(d!())
            .or_else(|e| AppendOnlyMerkle::create(path).c(d!(e)))
    }

    // Initialize a bitmap to track the unspent utxos.
    #[inline(always)]
    fn init_utxo_map(path: &str) -> Result<BitMap> {
        let mut file = OpenOptions::new();
        let f = file.read(true).write(true);

        f.open(path)
            .c(d!())
            .or_else(|e| f.create(true).truncate(true).open(path).c(d!(e)))
            .and_then(|f| BitMap::open(f).c(d!()))
    }

    /// Initialize a new Ledger structure.
    pub fn new(base_dir: &str, prefix: Option<String>) -> Result<LedgerState> {
        let prefix = prefix.unwrap_or_default();

        let block_merkle_path = format!("{}/{}_block_merkle", base_dir, &prefix);
        let txn_merkle_path = format!("{}/{}_txn_merkle", base_dir, &prefix);
        let utxo_map_path = format!("{}/{}_utxo_map", base_dir, &prefix);

        // These iterms will be set under ${BNC_DATA_DIR}
        fs::create_dir_all(&base_dir).c(d!())?;
        let snapshot_path = format!("{}/{}_ledger_status", base_dir, &prefix);
        let snapshot_entries_dir = prefix.clone() + "_ledger_status_subdata";
        let blocks_path = prefix.clone() + "_blocks";
        let tx_to_block_location_path = prefix + "_tx_to_block_location";

        let ledger = LedgerState {
            status: LedgerStatus::new(&snapshot_path, &snapshot_entries_dir).c(d!())?,
            prng: ChaChaRng::from_entropy(),
            block_merkle: LedgerState::init_merkle_log(&block_merkle_path).c(d!())?,
            txn_merkle: LedgerState::init_merkle_log(&txn_merkle_path).c(d!())?,
            blocks: new_vecx!(&blocks_path),
            tx_to_block_location: new_mapx!(&tx_to_block_location_path),
            utxo_map: LedgerState::init_utxo_map(&utxo_map_path).c(d!())?,
            block_ctx: Some(BlockEffect::default()),
        };

        Ok(ledger)
    }

    // Load an existing one OR create a new one.
    fn load_from_log(base_dir: &str) -> Result<LedgerState> {
        let mut ledger = LedgerState::new(base_dir, None).c(d!())?;

        if ledger.blocks.is_empty() {
            let txn_log_path = format!("{}/txn_log", base_dir);
            if let Ok(old_blocks) =
                LedgerState::load_transaction_log(&txn_log_path).c(d!())
            {
                for logged_block in old_blocks.into_iter() {
                    let mut be = ledger.start_block().c(d!())?;
                    for txn in logged_block.block {
                        let te = TxnEffect::compute_effect(txn).c(d!())?;
                        ledger.apply_transaction(&mut be, te, true).c(d!())?;
                    }
                    ledger.finish_block(be).c(d!())?;
                }
            }
        }

        let h = ledger.get_tendermint_height() as u64;
        ledger.get_staking_mut().set_custom_block_height(h);
        omit!(ledger.utxo_map.compute_checksum());
        ledger.fast_invariant_check().c(d!())?;

        flush_data();

        Ok(ledger)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn load_or_init(base_dir: &str) -> Result<LedgerState> {
        LedgerState::load_from_log(base_dir)
    }

    /// Perform checkpoint of current ledger state
    pub fn checkpoint(&mut self, block: &BlockEffect) -> Result<u64> {
        let merkle_id = self.compute_and_append_txns_hash(&block);
        let pulse_count = block
            .staking_simulator
            .cur_height()
            .saturating_sub(self.get_block_commit_count() + 1);
        self.compute_and_save_state_commitment_data(pulse_count);
        self.utxo_map.write().c(d!())?;
        self.txn_merkle.write().c(d!())?;
        self.block_merkle.write().c(d!())?;

        Ok(merkle_id)
    }

    /// A helper for setting block rewards in ABCI.
    pub fn staking_set_last_block_rewards(
        &mut self,
        addr: TendermintAddrRef,
        block_vote_percent: Option<[Power; 2]>,
    ) -> Result<()> {
        let gdp = self.staking_get_global_delegation_percent();
        let return_rate = self.staking_get_block_rewards_rate();

        let pk = self
            .get_staking()
            .validator_td_addr_to_app_pk(addr)
            .c(d!())?;

        self.get_staking_mut()
            .record_block_rewards_rate(return_rate);

        let commission_rate = if let Some(Some(v)) = self
            .get_staking()
            .validator_get_current()
            .map(|vd| vd.body.get(&pk))
        {
            v.commission_rate
        } else {
            return Err(eg!("not validator"));
        };

        let h = self.get_staking().cur_height;
        let commissions = self
            .get_staking_mut()
            .di
            .addr_map
            .values_mut()
            .filter(|d| d.validator_entry_exists(&pk))
            .map(|d| {
                d.set_delegation_rewards(&pk, h, return_rate, commission_rate, gdp, true)
            })
            .collect::<Result<Vec<_>>>()
            .c(d!())?;

        if let Some(v) = self.get_staking_mut().delegation_get_mut(&pk) {
            v.rwd_amount = v.rwd_amount.saturating_add(commissions.into_iter().sum());
        }

        if let Some(vote_percent) = block_vote_percent {
            self.get_staking_mut()
                .set_proposer_rewards(&pk, vote_percent)
                .c(d!())?;
        }

        Ok(())
    }

    /// Return rate definition for delegation rewards.
    #[inline(always)]
    pub fn staking_get_block_rewards_rate(&self) -> [u128; 2] {
        let p = self.staking_get_global_delegation_percent();
        let p = [p[0] as u128, p[1] as u128];

        // This is an equal conversion of `1 / p% * 0.0201`
        let mut a0 = p[1] * 201;
        let mut a1 = p[0] * 10000;

        if a0 * 100 > a1 * 105 {
            // max value: 105%
            a0 = 105;
            a1 = 100;
        } else if a0 * 50 < a1 {
            // min value: 2%
            a0 = 2;
            a1 = 100;
        }

        [a0, a1]
    }

    // Total amount of all freed FRAs, aka 'are not being locked in any way'.
    #[inline(always)]
    fn staking_get_global_unlocked_amount(&self) -> Amount {
        FRA_TOTAL_AMOUNT
            - FF_PK_LIST
                .iter()
                .chain([*BLACK_HOLE_PUBKEY].iter())
                .map(|pk| self.staking_get_nonconfidential_balance(pk).unwrap_or(0))
                .sum::<Amount>()
            - self.get_staking().coinbase_balance()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn staking_get_global_delegation_percent(&self) -> [u64; 2] {
        [
            self.get_staking().get_global_delegation_amount(),
            self.staking_get_global_unlocked_amount(),
        ]
    }

    fn staking_get_nonconfidential_balance(&self, addr: &XfrPublicKey) -> Result<u64> {
        self.get_owned_utxos(addr).c(d!()).map(|o| {
            o.values()
                .map(|(utxo, _)| {
                    if let XfrAmount::NonConfidential(am) = utxo.0.record.amount {
                        am
                    } else {
                        0
                    }
                })
                .sum()
        })
    }

    /// Get a utxo along with the transaction, spent status and commitment data which it belongs
    pub fn get_utxo(&self, id: TxoSID) -> Option<AuthenticatedUtxo> {
        if let Some(utxo) = self.status.get_utxo(id) {
            let txn_location = self.status.txo_to_txn_location.get(&id).unwrap();
            let authenticated_txn = self.get_transaction(txn_location.0).unwrap();
            let authenticated_spent_status = self.get_utxo_status(id);
            let state_commitment_data =
                self.status.state_commitment_data.as_ref().unwrap().clone();
            let utxo_location = txn_location.1;
            Some(AuthenticatedUtxo {
                utxo,
                authenticated_txn,
                authenticated_spent_status,
                utxo_location,
                state_commitment_data,
            })
        } else {
            None
        }
    }

    /// Get a utxo along with the transaction which it belongs
    /// Avoid ledger query operation to reduce latency
    pub fn get_utxo_light(&self, id: TxoSID) -> Option<UnAuthenticatedUtxo> {
        let utxo = self.status.get_utxo(id);
        if let Some(utxo) = utxo {
            let txn_location = self.status.txo_to_txn_location.get(&id).unwrap();
            let txn = self.get_transaction_light(txn_location.0).unwrap();
            let utxo_location = txn_location.1;
            Some(UnAuthenticatedUtxo {
                utxo,
                txn,
                utxo_location,
            })
        } else {
            None
        }
    }

    /// Get a spent utxo along with the transaction, spent status and commitment data which it belongs
    pub fn get_spent_utxo(&self, addr: TxoSID) -> Option<AuthenticatedUtxo> {
        let utxo = self.status.get_spent_utxo(addr);
        if let Some(utxo) = utxo {
            let txn_location = self.status.txo_to_txn_location.get(&addr).unwrap();
            let authenticated_txn = self.get_transaction(txn_location.0).unwrap();
            let authenticated_spent_status = self.get_utxo_status(addr);
            let state_commitment_data =
                self.status.state_commitment_data.as_ref().unwrap().clone();
            let utxo_location = txn_location.1;
            Some(AuthenticatedUtxo {
                utxo,
                authenticated_txn,
                authenticated_spent_status,
                utxo_location,
                state_commitment_data,
            })
        } else {
            None
        }
    }

    /// Get a spent utxo along with the transaction which it belongs
    /// Avoid ledger query operation to reduce latency
    pub fn get_spent_utxo_light(&self, addr: TxoSID) -> Option<UnAuthenticatedUtxo> {
        let utxo = self.status.get_spent_utxo(addr);
        if let Some(utxo) = utxo {
            let txn_location = self.status.txo_to_txn_location.get(&addr).unwrap();
            let txn = self.get_transaction_light(txn_location.0).unwrap();
            let utxo_location = txn_location.1;
            Some(UnAuthenticatedUtxo {
                utxo,
                txn,
                utxo_location,
            })
        } else {
            None
        }
    }

    #[allow(missing_docs)]
    pub fn get_utxos(&self, sid_list: &[TxoSID]) -> Vec<Option<AuthenticatedUtxo>> {
        let mut utxos = vec![];
        for sid in sid_list.iter() {
            let utxo = self.status.get_utxo(*sid);
            if let Some(utxo) = utxo {
                let txn_location = self.status.txo_to_txn_location.get(sid).unwrap();
                let authenticated_txn = self.get_transaction(txn_location.0).unwrap();
                let authenticated_spent_status = self.get_utxo_status(*sid);
                let state_commitment_data =
                    self.status.state_commitment_data.as_ref().unwrap().clone();
                let utxo_location = txn_location.1;
                let auth_utxo = AuthenticatedUtxo {
                    utxo,
                    authenticated_txn,
                    authenticated_spent_status,
                    utxo_location,
                    state_commitment_data,
                };
                utxos.push(Some(auth_utxo))
            } else {
                utxos.push(None)
            }
        }

        utxos
    }

    #[allow(missing_docs)]
    pub fn get_utxos_light(
        &self,
        sid_list: &[TxoSID],
    ) -> Result<Vec<Option<UnAuthenticatedUtxo>>> {
        let mut utxos = vec![];
        for sid in sid_list.iter() {
            let utxo = self.status.get_utxo(*sid);
            if let Some(utxo) = utxo {
                let txn_location = self.status.txo_to_txn_location.get(sid).c(d!())?;
                let txn = self.get_transaction_light(txn_location.0).c(d!())?;
                let utxo_location = txn_location.1;
                let auth_utxo = UnAuthenticatedUtxo {
                    utxo,
                    txn,
                    utxo_location,
                };
                utxos.push(Some(auth_utxo))
            } else {
                utxos.push(None)
            }
        }
        Ok(utxos)
    }

    /// Get unspent utxos owned by a findora account
    pub fn get_owned_utxos(
        &self,
        addr: &XfrPublicKey,
    ) -> Result<BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)>> {
        let sids = self.status.get_owned_utxos(addr);
        let aus = self.get_utxos_light(&sids).c(d!())?;

        let res = sids
            .into_iter()
            .zip(aus.into_iter())
            .filter_map(|(sid, au)| au.map(|au| (sid, au)))
            .map(|(sid, au)| {
                (
                    sid,
                    (
                        au.utxo,
                        au.txn
                            .txn
                            .get_owner_memos_ref()
                            .get(au.utxo_location.0)
                            .map(|i| i.cloned())
                            .flatten(),
                    ),
                )
            })
            .collect();

        Ok(res)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_issuance_num(&self, code: &AssetTypeCode) -> Option<u64> {
        self.status.get_issuance_num(code)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_asset_type(&self, code: &AssetTypeCode) -> Option<AssetType> {
        self.status.get_asset_type(code)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_block_commit_count(&self) -> u64 {
        self.status.block_commit_count
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_state_commitment(&self) -> (HashOf<Option<StateCommitmentData>>, u64) {
        let block_count = self.status.block_commit_count;
        let commitment = self
            .status
            .state_commitment_versions
            .last()
            .unwrap_or_else(|| HashOf::new(&None));
        (commitment, block_count)
    }

    /// Get utxo status and its proof data
    pub fn get_utxo_status(&self, addr: TxoSID) -> AuthenticatedUtxoStatus {
        let state_commitment_data = self.status.state_commitment_data.as_ref().unwrap();
        let utxo_map_bytes;
        let status;
        if addr.0 < state_commitment_data.txo_count {
            utxo_map_bytes = Some(self.utxo_map.serialize(0));
            let utxo_map =
                SparseMap::new(&utxo_map_bytes.as_ref().unwrap().clone()).unwrap();
            status = if utxo_map.query(addr.0).unwrap() {
                UtxoStatus::Unspent
            } else {
                UtxoStatus::Spent
            };
        } else {
            status = UtxoStatus::Nonexistent;
            utxo_map_bytes = None;
        }

        AuthenticatedUtxoStatus {
            status,
            state_commitment_data: state_commitment_data.clone(),
            state_commitment: state_commitment_data.compute_commitment(),
            utxo_sid: addr,
            utxo_map_bytes,
        }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_staking(&self) -> &Staking {
        &self.status.staking
    }

    /// Query the transaction by a TxnSID along with its proof data
    pub fn get_transaction(&self, id: TxnSID) -> Result<AuthenticatedTransaction> {
        self.get_transaction_light(id).c(d!()).and_then(|tx| {
            let state_commitment_data =
                self.status.state_commitment_data.as_ref().c(d!())?.clone();
            let merkle = &self.txn_merkle;
            let proof = ProofOf::new(merkle.get_proof(tx.merkle_id, 0).c(d!())?);

            Ok(AuthenticatedTransaction {
                finalized_txn: tx,
                txn_inclusion_proof: proof,
                state_commitment_data: state_commitment_data.clone(),
                state_commitment: state_commitment_data.compute_commitment(),
            })
        })
    }

    /// Query the transaction by a TxnSID without its proof data to reduce latency
    pub fn get_transaction_light(&self, id: TxnSID) -> Result<FinalizedTransaction> {
        self.tx_to_block_location
            .get(&id)
            .c(d!())
            .and_then(|[block_idx, tx_idx]| {
                self.blocks
                    .get(block_idx)
                    .c(d!())
                    .and_then(|b| b.txns.get(tx_idx).cloned().c(d!()))
            })
    }

    /// Query the Block by a BlockSID along with its proof data
    pub fn get_block(&self, addr: BlockSID) -> Option<AuthenticatedBlock> {
        match self.blocks.get(addr.0) {
            None => None,
            Some(finalized_block) => {
                let block_inclusion_proof = ProofOf::new(
                    self.block_merkle
                        .get_proof(finalized_block.merkle_id, 0)
                        .unwrap(),
                );
                let state_commitment_data =
                    self.status.state_commitment_data.as_ref().unwrap().clone();
                Some(AuthenticatedBlock {
                    block: finalized_block,
                    block_inclusion_proof,
                    state_commitment_data: state_commitment_data.clone(),
                    state_commitment: state_commitment_data.compute_commitment(),
                })
            }
        }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_block_count(&self) -> usize {
        self.blocks.len()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_transaction_count(&self) -> usize {
        self.get_next_txn().0
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_state_commitment_at_block_height(
        &self,
        block_height: u64,
    ) -> Option<HashOf<Option<StateCommitmentData>>> {
        self.status
            .state_commitment_versions
            .get((block_height - 1) as usize)
    }
}

/// The main LedgerStatus of findora ledger
#[derive(Deserialize, Serialize, PartialEq, Debug)]
pub struct LedgerStatus {
    /// the file path of the snapshot
    pub snapshot_path: String,
    utxos: Mapx<TxoSID, Utxo>, // all currently-unspent TXOs
    owned_utxos: Mapx<XfrPublicKey, HashSet<TxoSID>>,
    /// all spent TXOs
    pub spent_utxos: Mapx<TxoSID, Utxo>,
    // Map a TXO to its output position in a transaction
    txo_to_txn_location: Mapx<TxoSID, (TxnSID, OutputPosition)>,
    // State commitment history.
    // The BitDigest at index i is the state commitment of the ledger at block height  i + 1.
    state_commitment_versions: Vecx<HashOf<Option<StateCommitmentData>>>,
    // Registered asset types
    asset_types: Mapx<AssetTypeCode, AssetType>,
    // Issuance number is always increasing
    issuance_num: Mapx<AssetTypeCode, u64>,
    // Issuance amounts for assets with limits
    issuance_amounts: Mapx<AssetTypeCode, u64>,
    // Should be equal to the count of transactions
    next_txn: TxnSID,
    // Should be equal to the count of TXOs
    next_txo: TxoSID,
    // Each block corresponds to such a summary structure
    state_commitment_data: Option<StateCommitmentData>,
    // number of non-empty blocks, equal to: <block count of tendermint> - <pulse count>
    block_commit_count: u64,
    // Hash of the transactions in the most recent block
    txns_in_block_hash: Option<HashOf<Vec<Transaction>>>,
    // Sliding window of operations for replay attack prevention
    sliding_set: SlidingSet<[u8; 8]>,
    // POS-related implementations
    staking: Staking,
    // tendermint commit height
    td_commit_height: u64,

    // An obsolete feature, ignore it!
    tracing_policies: HashMap<AssetTypeCode, TracingPolicy>,
}

impl LedgerStatus {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_owned_utxos(&self, addr: &XfrPublicKey) -> Vec<TxoSID> {
        self.owned_utxos
            .get(addr)
            .map(|v| v.iter().cloned().collect())
            .unwrap_or_default()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    fn get_utxo(&self, id: TxoSID) -> Option<Utxo> {
        self.utxos.get(&id)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    fn get_spent_utxo(&self, addr: TxoSID) -> Option<Utxo> {
        self.spent_utxos.get(&addr)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    fn get_issuance_num(&self, code: &AssetTypeCode) -> Option<u64> {
        self.issuance_num.get(code)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    fn get_asset_type(&self, code: &AssetTypeCode) -> Option<AssetType> {
        self.asset_types.get(code)
    }

    fn fast_invariant_check(&self) -> Result<()> {
        let cnt_eq =
            self.block_commit_count == self.state_commitment_versions.len() as u64;
        let state_eq = self
            .state_commitment_data
            .as_ref()
            .map(|x| x.compute_commitment())
            == self.state_commitment_versions.last();

        if cnt_eq && state_eq {
            Ok(())
        } else {
            Err(eg!(format!(
                "{}: {}",
                self.block_commit_count,
                self.state_commitment_versions.len()
            )))
        }
    }

    /// Load or init LedgerStatus from snapshot
    #[inline(always)]
    pub fn new(snapshot_path: &str, snapshot_entries_dir: &str) -> Result<LedgerStatus> {
        match fs::read_to_string(snapshot_path) {
            Ok(s) => serde_json::from_str(&s).c(d!()),
            Err(e) => {
                if ErrorKind::NotFound != e.kind() {
                    Err(eg!(e))
                } else {
                    Self::create(snapshot_path, snapshot_entries_dir).c(d!())
                }
            }
        }
    }

    fn create(snapshot_path: &str, snapshot_entries_dir: &str) -> Result<LedgerStatus> {
        let utxos_path = snapshot_entries_dir.to_owned() + "/utxo";
        let spent_utxos_path = snapshot_entries_dir.to_owned() + "/spent_utxos";
        let txo_to_txn_location_path =
            snapshot_entries_dir.to_owned() + "/txo_to_txn_location";
        let issuance_amounts_path =
            snapshot_entries_dir.to_owned() + "/issuance_amounts";
        let state_commitment_versions_path =
            snapshot_entries_dir.to_owned() + "/state_commitment_versions";
        let asset_types_path = snapshot_entries_dir.to_owned() + "/asset_types";
        let issuance_num_path = snapshot_entries_dir.to_owned() + "/issuance_num";
        let owned_utxos_path = snapshot_entries_dir.to_owned() + "/owned_utxos";

        let ledger = LedgerStatus {
            snapshot_path: snapshot_path.to_owned(),
            sliding_set: SlidingSet::<[u8; 8]>::new(TRANSACTION_WINDOW_WIDTH as usize),
            utxos: new_mapx!(utxos_path.as_str()),
            owned_utxos: new_mapx!(owned_utxos_path.as_str()),
            spent_utxos: new_mapx!(spent_utxos_path.as_str()),
            txo_to_txn_location: new_mapx!(txo_to_txn_location_path.as_str()),
            issuance_amounts: new_mapx!(issuance_amounts_path.as_str()),
            state_commitment_versions: new_vecx!(state_commitment_versions_path.as_str()),
            asset_types: new_mapx!(asset_types_path.as_str()),
            tracing_policies: map! {},
            issuance_num: new_mapx!(issuance_num_path.as_str()),
            next_txn: TxnSID(0),
            next_txo: TxoSID(0),
            txns_in_block_hash: None,
            state_commitment_data: None,
            block_commit_count: 0,
            staking: Staking::new(),
            td_commit_height: 1,
        };

        Ok(ledger)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn incr_block_commit_count(&mut self) {
        self.block_commit_count += 1;
        self.sliding_set.incr_current();
    }

    // Check that `txn` can be safely applied to the current ledger.
    //
    // Returns the same TxnEffect (unchanged) if it is safe. Consumes `txn`
    // if it would be invalid to prevent a simple case of forgetting error
    // handling, such as:
    //
    //  ledger.check_txn_effects(txn_effect);
    //  block.add_txn_effect(txn_effect);
    fn check_txn_effects(&self, txn_effect: &TxnEffect) -> Result<()> {
        // The current transactions seq_id must be within the sliding window over seq_ids
        let (rand, seq_id) = (
            txn_effect.txn.body.no_replay_token.get_rand(),
            txn_effect.txn.body.no_replay_token.get_seq_id(),
        );
        if seq_id > self.block_commit_count {
            return Err(eg!(("Transaction seq_id ahead of block_count")));
        } else if seq_id + (TRANSACTION_WINDOW_WIDTH as u64) < self.block_commit_count {
            return Err(eg!(("Transaction seq_id too far behind block_count")));
        } else {
            // Check to see that this nrpt has not been seen before
            if self.sliding_set.has_key_at(seq_id as usize, rand) {
                return Err(eg!(format!(
                    "No replay token ({:?}, {})seen before at  possible replay",
                    rand, seq_id
                )));
            }
        }

        // 1. Each input must be unspent and correspond to the claimed record
        // 2. Inputs with transfer restrictions can only be owned by the asset issuer
        for (inp_sid, inp_record) in txn_effect.input_txos.iter() {
            // (1)
            let inp_utxo = self.utxos.get(inp_sid).c(d!("Input must be unspent"))?;
            let record = &(inp_utxo.0);
            if record != inp_record {
                return Err(eg!((format!(
                    "Input must correspond to claimed record: {} != {}",
                    serde_json::to_string(&record).c(d!())?,
                    serde_json::to_string(inp_record).unwrap()
                ))));
            }
            // (2)
            if let Some(code) = record
                .record
                .asset_type
                .get_asset_type()
                .map(|v| AssetTypeCode { val: v })
            {
                let asset_type = self
                    .asset_types
                    .get(&code)
                    .or_else(|| txn_effect.new_asset_codes.get(&code).cloned())
                    .c(d!())?;
                if !asset_type.properties.asset_rules.transferable
                    && asset_type.properties.issuer.deref() != &record.record.public_key
                {
                    return Err(eg!(
                        ("Non-transferable asset type must be owned by asset issuer")
                    ));
                }
            }
        }

        // Internally spend inputs with transfer restrictions can only be owned by the asset issuer
        for record in txn_effect.internally_spent_txos.iter() {
            if let Some(code) = record
                .record
                .asset_type
                .get_asset_type()
                .map(|v| AssetTypeCode { val: v })
            {
                let asset_type = self
                    .asset_types
                    .get(&code)
                    .or_else(|| txn_effect.new_asset_codes.get(&code).cloned())
                    .c(d!())?;
                if !asset_type.properties.asset_rules.transferable
                    && asset_type.properties.issuer.deref() != &record.record.public_key
                {
                    return Err(eg!(
                        ("Non-transferable asset type must be owned by asset issuer")
                    ));
                }
            }
        }

        // New asset types must not already exist
        for (code, _asset_type) in txn_effect.new_asset_codes.iter() {
            if self.asset_types.contains_key(&code) {
                return Err(eg!(format!("Asset type {:?} already defined", &code)));
            }
            if self.issuance_num.contains_key(&code) {
                return Err(eg!(format!(
                    "Asset type {:?} is being defined after issue",
                    &code
                )));
            }

            // Asset issuance should match the currently registered key
        }

        // New issuance numbers
        // (1) Must refer to a created asset type
        //  - NOTE: if the asset type is created in this transaction, this
        //    function is assuming that the ordering within the transaction is
        //    already valid.
        // (2) Must not be below the current asset cap
        //  - NOTE: this relies on the sequence numbers appearing in sorted
        //    order
        for (code, seq_nums) in txn_effect.new_issuance_nums.iter() {
            let iss_key = txn_effect.issuance_keys.get(&code).c(d!())?;
            let asset_type = self
                .asset_types
                .get(&code)
                .or_else(|| txn_effect.new_asset_codes.get(&code).cloned())
                .c(d!())?;
            let proper_key = asset_type.properties.issuer;
            if *iss_key != proper_key {
                return Err(eg!(
                    ("Issuance key is not the same as key of properties issuer")
                ));
            }

            if seq_nums.is_empty() {
                if !txn_effect.new_asset_codes.contains_key(&code) {
                    return Err(eg!(("Code is not contained in new asset codes")));
                }
            // We could re-check that self.issuance_num doesn't contain `code`,
            // but currently it's redundant with the new-asset-type checks
            } else {
                let curr_seq_num_limit = self.issuance_num.get(&code).unwrap_or(0);
                let min_seq_num = seq_nums.first().c(d!())?;
                if *min_seq_num < curr_seq_num_limit {
                    return Err(eg!(("Minimum seq num is less than limit")));
                }
            }
        }

        // Asset Caps
        // (1) New issuance amounts cannot exceed asset cap
        // (2) No confidential issuances allowed for assets with issuance restrictions
        for (code, amount) in txn_effect.issuance_amounts.iter() {
            let asset_type = self
                .asset_types
                .get(&code)
                .or_else(|| txn_effect.new_asset_codes.get(&code).cloned())
                .c(d!())?;
            // (1)
            if let Some(cap) = asset_type.properties.asset_rules.max_units {
                let current_amount = self.issuance_amounts.get(code).unwrap_or(0);
                if current_amount.checked_add(*amount).c(d!())? > cap {
                    return Err(eg!(("Amount exceeds asset cap")));
                }
            }
        }

        // (2)
        for code in txn_effect.confidential_issuance_types.iter() {
            let asset_type = self
                .asset_types
                .get(&code)
                .or_else(|| txn_effect.new_asset_codes.get(&code).cloned())
                .c(d!())?;
            if asset_type.has_issuance_restrictions() {
                return Err(eg!(("This asset type has issuance restrictions")));
            }
        }

        // Memo updates
        // Multiple memo updates for the same asset are allowed, but only the last one will be applied.
        for memo_update in txn_effect.memo_updates.iter() {
            let asset = self.asset_types.get(&memo_update.0).c(d!())?;
            // Asset must be updatable and key must be correct
            if !asset.properties.asset_rules.updatable
                || asset.properties.issuer != (IssuerPublicKey { key: memo_update.1 })
            {
                return Err(eg!(("Non updatable asset or issuer mismatch")));
            }
        }

        // Until we can distinguish assets that have policies that invoke transfer restrictions
        // from those that don't, prevent any non-confidential assets with transfer restrictions
        // from becoming confidential
        for code in txn_effect.confidential_transfer_inputs.iter() {
            let asset_type = self
                .asset_types
                .get(&code)
                .or_else(|| txn_effect.new_asset_codes.get(&code).cloned())
                .c(d!())?;
            if asset_type.has_transfer_restrictions() {
                return Err(eg!(
                    ("non-confidential assets with transfer restrictions can't become confidential")
                ));
            }
        }

        Ok(())
    }

    // This function assumes that `block` is COMPLETELY CONSISTENT with the
    // ledger state. Calling `check_txn_effects` for each TxnEffect getting
    // mixed into the BlockEffect *should* be enough to guarantee that (if
    // that is ever false, it's a bug).
    //
    // This drains every field of `block` except `txns` and `temp_sids`.
    fn apply_block_effects(&mut self, block: &mut BlockEffect) -> (TmpSidMap, u64, u64) {
        let base_sid = self.next_txo.0;

        for no_replay_token in block.no_replay_tokens.iter() {
            let (rand, seq_id) = (
                no_replay_token.get_rand(),
                no_replay_token.get_seq_id() as usize,
            );
            if let Err(e) = self.sliding_set.insert(rand, seq_id) {
                pd!(format!("Error inserting into window: {}", e));
            }
        }
        block.no_replay_tokens.clear();

        // Remove consumed UTXOs
        for (inp_sid, utxo) in block.input_txos.drain() {
            if let Some(mut v) = self.owned_utxos.get_mut(&utxo.record.public_key) {
                v.deref_mut().remove(&inp_sid);
            }
            if let Some(v) = self.utxos.remove(&inp_sid) {
                self.spent_utxos.insert(inp_sid, v);
            }
        }

        // Apply memo updates
        for (code, memo) in block.memo_updates.drain() {
            let mut asset = self.asset_types.get_mut(&code).unwrap();
            (*asset).properties.memo = memo;
        }

        for (code, amount) in block.issuance_amounts.drain() {
            let mut amt = self.issuance_amounts.entry(code).or_insert(0);
            *amt.deref_mut() += amount;
        }

        // Add new UTXOs
        // Each transaction gets a TxnSID, and each of its unspent TXOs gets
        // a TxoSID. TxoSID assignments are based on the order TXOs appear in
        // the transaction.
        let mut new_utxo_sids: HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)> = map! {};
        {
            let mut next_txn = self.next_txn.0;
            let mut next_txo = self.next_txo.0;

            for (ix, txos) in block.temp_sids.iter().zip(block.txos.drain(..)) {
                let txn_sid = next_txn;
                next_txn += 1;

                let mut txn_utxo_sids: Vec<TxoSID> = vec![];

                for txo in txos {
                    let txo_sid = next_txo;
                    next_txo += 1;
                    if let Some(tx_output) = txo {
                        self.owned_utxos
                            .entry(tx_output.record.public_key)
                            .or_insert_with(HashSet::new)
                            .insert(TxoSID(txo_sid));
                        self.utxos.insert(TxoSID(txo_sid), Utxo(tx_output));
                        txn_utxo_sids.push(TxoSID(txo_sid));
                    }
                }

                new_utxo_sids.insert(*ix, (TxnSID(txn_sid), txn_utxo_sids));
            }
            self.next_txn = TxnSID(next_txn);
            self.next_txo = TxoSID(next_txo);
        }

        // Update issuance sequence number limits
        for (code, seq_nums) in block.new_issuance_nums.drain() {
            // One more than the greatest sequence number, or 0
            let new_max_seq_num = seq_nums.last().map(|x| x + 1).unwrap_or(0);
            self.issuance_num.insert(code, new_max_seq_num);
        }

        // Register new asset types
        for (code, asset_type) in block.new_asset_codes.drain() {
            self.asset_types.insert(code, asset_type.clone());
        }

        // issuance_keys should already have been checked
        block.issuance_keys.clear();

        let max_sid = self.next_txo.0;
        (new_utxo_sids, base_sid, max_sid)
    }

    /// Check if an txo_sid is unspent.
    #[inline(always)]
    pub fn is_unspent_txo(&self, addr: TxoSID) -> bool {
        self.utxos.contains_key(&addr)
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct LoggedBlock {
    pub block: Vec<Transaction>,
    pub state: StateCommitmentData,
}

/// Flush data to disk
pub fn flush_data() {
    bnc::flush_data();
}
