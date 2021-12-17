//!
//! # Findora ledger store implementation
//!

pub mod api_cache;
pub mod helpers;
mod test;
pub mod utils;

pub use fbnc;

use {
    crate::{
        data_model::{
            ATxoSID, AnonStateCommitmentData, AssetType, AssetTypeCode,
            AuthenticatedBlock, AuthenticatedTransaction, AuthenticatedUtxo,
            AuthenticatedUtxoStatus, BlockEffect, BlockSID, FinalizedBlock,
            FinalizedTransaction, IssuerKeyPair, IssuerPublicKey, Operation,
            OutputPosition, StateCommitmentData, Transaction, TransferType, TxnEffect,
            TxnSID, TxnTempSID, TxoSID, UnAuthenticatedUtxo, Utxo, UtxoStatus,
            BLACK_HOLE_PUBKEY,
        },
        staking::{
            Amount, BlockHeight, Power, Staking, TendermintAddrRef,
            FF_PK_EXTRA_120_0000, FF_PK_LIST, FRA_TOTAL_AMOUNT, KEEP_HIST,
        },
        LSSED_VAR, SNAPSHOT_ENTRIES_DIR,
    },
    api_cache::ApiCache,
    bitmap::{BitMap, SparseMap},
    cryptohash::sha256::Digest as BitDigest,
    fbnc::{new_mapx, new_mapxnk, new_vecx, Mapx, Mapxnk, Vecx},
    globutils::{HashOf, ProofOf},
    merkle_tree::AppendOnlyMerkle,
    parking_lot::RwLock,
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    serde::{Deserialize, Serialize},
    sliding_set::SlidingSet,
    sparse_merkle_tree::{Key, SmtMap256},
    std::{
        collections::{BTreeMap, HashMap, HashSet},
        env,
        fs::{self, OpenOptions},
        io::ErrorKind,
        mem,
        ops::{Deref, DerefMut},
        sync::Arc,
    },
    storage::{
        db::RocksDB,
        state::{ChainState, State},
        store::{ImmutablePrefixedStore, PrefixedStore},
    },
    zei::{
        anon_xfr::{
            hash_abar,
            keys::AXfrPubKey,
            structs::{AnonBlindAssetRecord, MTLeafInfo, MTNode, MTPath, Nullifier},
            verify_anon_xfr_body,
        },
        serialization::ZeiFromToBytes,
        setup::{NodeParams, UserParams, DEFAULT_BP_NUM_GENS},
        xfr::{
            lib::XfrNotePolicies,
            sig::XfrPublicKey,
            structs::{OwnerMemo, TracingPolicies, TracingPolicy},
        },
    },
    zei_accumulators::merkle_tree::{
        ImmutablePersistentMerkleTree, PersistentMerkleTree, Proof,
    },
    zeialgebra::bls12_381::BLSScalar,
};

const TRANSACTION_WINDOW_WIDTH: u64 = 128;

type TmpSidMap = HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>;

/// findora ledger
#[derive(Clone)]
pub struct LedgerState {
    // major part of State
    status: LedgerStatus,

    /// The `FinalizedTransaction`s consist of a Transaction and an index into
    /// `merkle` representing its hash.
    pub blocks: Vecx<FinalizedBlock>,
    /// <tx id> => [<block id>, <tx idx in block>]
    pub tx_to_block_location: Mapxnk<TxnSID, [usize; 2]>,
    /// cache used in APIs
    pub api_cache: Option<ApiCache>,

    // current block effect (middle cache)
    block_ctx: Option<BlockEffect>,

    // Merkle tree tracing the sequence of transaction hashes in the block
    // Each appended hash is the hash of transactions in the same block
    block_merkle: Arc<RwLock<AppendOnlyMerkle>>,
    // Merkle tree tracing the sequence of all transaction hashes
    // Each appended hash is the hash of a transaction
    txn_merkle: Arc<RwLock<AppendOnlyMerkle>>,
    // Bitmap tracing all the live TXOs
    utxo_map: Arc<RwLock<BitMap>>,
    // Merkle Tree with all the ABARs created till now
    abar_state: State<RocksDB>,
    // Merkle Tree with all the ABARs created till now
    abar_query_state: State<RocksDB>,
    // Sparse Merkle Tree to hold nullifier Set
    nullifier_set: SmtMap256<RocksDB>,
}

impl LedgerState {
    #[inline(always)]
    fn fast_invariant_check(&self) -> Result<()> {
        self.status.fast_invariant_check().c(d!())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_prng(&mut self) -> ChaChaRng {
        ChaChaRng::from_entropy()
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
    ) -> Result<TxnTempSID> {
        let tx = txe.txn.clone();
        self.status
            .check_txn_effects(&txe)
            .c(d!())
            .and_then(|_| block.add_txn_effect(txe).c(d!()))
            .map(|tmpid| {
                // NOTE: set at the last position
                block.staking_simulator.coinbase_check_and_pay(&tx);
                tmpid
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

        let mut utxo_map = self.utxo_map.write();
        for ix in base_sid..max_sid {
            let temp_sid = txn_temp_sids[temp_sid_ix];
            let utxo_sids = &tsm[&temp_sid].1;

            // Only .set() extends the bitmap, so to append a 0 we currently
            // nead to .set() then .clear().
            utxo_map.set(ix as usize).c(d!())?;
            if let Some(TxoSID(utxo_sid)) = utxo_sids.get(txo_sid_ix) {
                if *utxo_sid != ix {
                    utxo_map.clear(ix as usize).c(d!())?;
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

    fn update_state(
        &mut self,
        mut block: BlockEffect,
        tsm: &TmpSidMap,
        next_txn_sid: usize,
    ) -> Result<()> {
        let mut tx_block = Vec::new();

        // Update the transaction Merkle tree
        // Store the location of each utxo so we can create authenticated utxo proofs
        let mut txn_merkle = self.txn_merkle.write();
        for (tmp_sid, txn) in block.temp_sids.iter().zip(block.txns.iter()) {
            let txn = txn.clone();
            let txo_sid_map = tsm.get(&tmp_sid).c(d!())?;
            let txn_sid = txo_sid_map.0;
            let txo_sids = &txo_sid_map.1;

            let merkle_id = txn_merkle
                .append_hash(&txn.hash(txn_sid).0.hash.into())
                .c(d!())?;

            tx_block.push(FinalizedTransaction {
                txn: txn.clone(),
                tx_id: txn_sid,
                txo_ids: txo_sids.clone(),
                atxo_ids: vec![],
                merkle_id,
            });

            for (position, sid) in txo_sids.iter().enumerate() {
                self.status
                    .txo_to_txn_location
                    .insert(*sid, (txn_sid, OutputPosition(position)));
            }
        }
        drop(txn_merkle);

        tx_block = self
            .update_anon_stores(
                block.new_nullifiers.clone(),
                block.output_abars.clone(),
                next_txn_sid,
                tx_block,
            )
            .c(d!())?;

        // Checkpoint
        let block_merkle_id = self.checkpoint(&block).c(d!())?;
        block.temp_sids.clear();
        block.txns.clear();
        block.output_abars.clear();
        block.new_nullifiers.clear();

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
        {
            let mut utxo_map = self.utxo_map.write();
            for (inp_sid, _) in block.input_txos.iter() {
                utxo_map.clear(inp_sid.0 as usize).c(d!())?;
            }
        }

        let backup_next_txn_sid = self.status.next_txn.0;
        let (tsm, base_sid, max_sid) = self.status.apply_block_effects(&mut block);

        self.update_utxo_map(base_sid, max_sid, &block.temp_sids, &tsm)
            .c(d!())
            .and_then(|_| self.update_state(block, &tsm, backup_next_txn_sid).c(d!()))
            .map(|_| tsm)
    }

    /// Apply the changes from current block
    /// to the merkle trees holding anonymous data
    pub fn update_anon_stores(
        &mut self,
        new_nullifiers: Vec<Nullifier>,
        output_abars: Vec<Vec<AnonBlindAssetRecord>>,
        backup_next_txn_sid: usize,
        mut tx_block: Vec<FinalizedTransaction>,
    ) -> Result<Vec<FinalizedTransaction>> {
        for n in new_nullifiers.iter() {
            let str =
                base64::encode_config(&n.get_scalar().to_bytes(), base64::URL_SAFE);
            let d: Key = Key::from_base64(&str).c(d!())?;

            // if the nullifier hash is present in our nullifier set, fail the block
            if self.nullifier_set.get(&d).c(d!())?.is_some() {
                return Err(eg!("Nullifier hash already present in set"));
            }
            self.nullifier_set.set(&d, Some(n.zei_to_bytes())).c(d!())?;
        }

        let mut txn_sid = TxnSID(backup_next_txn_sid);
        for (txn_abars, txn) in output_abars.iter().zip(tx_block.iter_mut()) {
            let mut op_position = OutputPosition(0);
            let mut atxo_ids: Vec<ATxoSID> = vec![];
            for abar in txn_abars {
                println!(
                    "ABAR setting in LedgerStatus: {}",
                    base64::encode_config(
                        abar.public_key.zei_to_bytes().as_slice(),
                        base64::URL_SAFE
                    )
                );
                let uid = self.add_abar(&abar).c(d!())?;
                self.status.ax_utxos.insert(uid, abar.clone());
                self.status
                    .owned_ax_utxos
                    .entry(abar.public_key)
                    .or_insert_with(HashSet::new)
                    .insert(uid);
                self.status
                    .ax_txo_to_txn_location
                    .insert(uid, (txn_sid, op_position));

                atxo_ids.push(uid);
                self.status.next_atxo = ATxoSID(uid.0 + 1);
                op_position = OutputPosition(op_position.0 + 1);
            }
            txn.atxo_ids = atxo_ids;
            txn_sid = TxnSID(txn_sid.0 + 1);
        }

        Ok(tx_block)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_staking_mut(&mut self) -> &mut Staking {
        &mut self.status.staking
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_tendermint_height(&mut self, tendermint_h: u64) {
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
        fbnc::clear();
        let tmp_dir = globutils::fresh_tmp_dir().to_string_lossy().into_owned();
        LedgerState::new(&tmp_dir, Some("test")).unwrap()
    }

    // In this functionn:
    //  1. Compute the hash of transactions in the block and update txns_in_block_hash
    //  2. Append txns_in_block_hash to block_merkle
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn compute_and_append_txns_hash(&mut self, block: &BlockEffect) -> u64 {
        // 1. Compute the hash of transactions in the block and update txns_in_block_hash
        let txns_in_block_hash = block.compute_txns_in_block_hash();
        self.status.txns_in_block_hash = Some(txns_in_block_hash.clone());

        // 2. Append txns_in_block_hash to block_merkle
        //  2.1 Update the block Merkle tree
        let ret = self
            .block_merkle
            .write()
            .append_hash(&txns_in_block_hash.0.hash.into())
            .unwrap();

        ret
    }

    #[allow(missing_docs)]
    pub fn compute_and_save_state_commitment_data(&mut self, pulse_count: u64) {
        let state_commitment_data = StateCommitmentData {
            bitmap: self.utxo_map.write().compute_checksum(),
            block_merkle: self.block_merkle.read().get_root_hash(),
            transaction_merkle_commitment: self.txn_merkle.read().get_root_hash(),
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

        // Commit Anon tree changes here following Tendermint protocol
        pnk!(self.commit_anon_changes().c(d!()));
        pnk!(self.commit_nullifier_changes().c(d!()));

        let abar_root_hash =
            self.get_abar_root_hash().expect("failed to read root hash");
        self.status.abar_commitment_versions.push(abar_root_hash);
        let anon_state_commitment_data = AnonStateCommitmentData {
            abar_root_hash,
            nullifier_root_hash: self
                .nullifier_set
                .merkle_root()
                .unwrap_or(sparse_merkle_tree::ZERO_DIGEST),
        };
        self.status
            .anon_state_commitment_versions
            .push(anon_state_commitment_data.compute_commitment());
        self.status.anon_state_commitment_data = Some(anon_state_commitment_data);

        self.status.incr_block_commit_count();
    }

    #[inline(always)]
    /// Adds a new abar to session cache and updates merkle hashes of ancestors
    pub fn add_abar(&mut self, abar: &AnonBlindAssetRecord) -> Result<ATxoSID> {
        let store = PrefixedStore::new("abar_store", &mut self.abar_state);
        let mut mt = PersistentMerkleTree::new(store)?;

        mt.add_commitment_hash(hash_abar(mt.entry_count(), abar))
            .map(ATxoSID)
    }

    #[inline(always)]
    /// writes the changes from session cache to the RocksDB store
    pub fn commit_anon_changes(&mut self) -> Result<u64> {
        let store = PrefixedStore::new("abar_store", &mut self.abar_state);
        let mut mt = PersistentMerkleTree::new(store)?;

        mt.commit()
    }

    #[inline(always)]
    /// writes the changes from session cache to the RocksDB store
    pub fn commit_nullifier_changes(&mut self) -> Result<u64> {
        self.nullifier_set.commit()
    }

    #[inline(always)]
    /// Fetches the root hash of the committed merkle tree of abar commitments directly from committed
    /// state and ignore session cache
    pub fn get_abar_root_hash(&self) -> Result<BLSScalar> {
        let store = ImmutablePrefixedStore::new("abar_store", &self.abar_query_state);
        let mt = ImmutablePersistentMerkleTree::new(store)?;

        mt.get_current_root_hash().c(d!(
            "probably due to badly constructed tree or data corruption"
        ))
    }

    #[inline(always)]
    /// Generates a MTLeafInfo from the latest committed version of tree from committed state and
    /// ignore session cache
    pub fn get_abar_proof(&self, id: ATxoSID) -> Result<MTLeafInfo> {
        let store = ImmutablePrefixedStore::new("abar_store", &self.abar_query_state);
        let mt = ImmutablePersistentMerkleTree::new(store)?;

        let mut t = mt.generate_proof(id.0)?;
        t.root_version = self.status.get_current_abar_version();

        Ok(create_mt_leaf_info(t))
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

    // Initialize a persistent merkle tree for ABAR store.
    #[inline(always)]
    fn init_abar_state(path: &str) -> Result<(State<RocksDB>, State<RocksDB>)> {
        let fdb = RocksDB::open(path).c(d!("failed to open db"))?;
        let cs = Arc::new(RwLock::new(ChainState::new(fdb, "abar_db".to_string(), 0)));
        Ok((State::new(cs.clone(), false), State::new(cs, false)))
    }

    // Initialize persistent Sparse Merkle tree for the Nullifier set
    #[inline(always)]
    fn init_nullifier_smt(path: &str) -> Result<SmtMap256<RocksDB>> {
        let rdb = RocksDB::open(path).c(d!("failed to open db"))?;
        Ok(SmtMap256::new(rdb))
    }

    /// Initialize a new Ledger structure.
    pub fn new(basedir: &str, prefix: Option<&str>) -> Result<LedgerState> {
        let prefix = if let Some(p) = prefix {
            format!("{}_", p)
        } else {
            "".to_owned()
        };

        let block_merkle_path = format!("{}/{}block_merkle", basedir, &prefix);
        let txn_merkle_path = format!("{}/{}txn_merkle", basedir, &prefix);
        let utxo_map_path = format!("{}/{}utxo_map", basedir, &prefix);
        let abar_store_path = format!("{}/{}abar_store", basedir, &prefix);
        let nullifier_store_path = format!("{}/{}nullifier_store", basedir, &prefix);

        // These items will be set under ${BNC_DATA_DIR}
        fs::create_dir_all(&basedir).c(d!())?;
        let snapshot_file = format!("{}ledger_status", &prefix);

        let snapshot_entries_dir = prefix.clone() + "ledger_status_subdata";
        env::set_var(LSSED_VAR, &snapshot_entries_dir);

        let blocks_path = prefix.clone() + "blocks";
        let tx_to_block_location_path = prefix.clone() + "tx_to_block_location";

        let (mut abar_state, abar_query_state) =
            LedgerState::init_abar_state(&abar_store_path).c(d!())?;

        // Initializing Merkle tree to set Empty tree root hash, which is a hash of null children
        let store = PrefixedStore::new("abar_store", &mut abar_state);
        let _ = PersistentMerkleTree::new(store)?;

        let mut ledger = LedgerState {
            status: LedgerStatus::new(&basedir, &snapshot_file).c(d!())?,
            block_merkle: Arc::new(RwLock::new(
                LedgerState::init_merkle_log(&block_merkle_path).c(d!())?,
            )),
            txn_merkle: Arc::new(RwLock::new(
                LedgerState::init_merkle_log(&txn_merkle_path).c(d!())?,
            )),
            blocks: new_vecx!(&blocks_path),
            tx_to_block_location: new_mapxnk!(&tx_to_block_location_path),
            utxo_map: Arc::new(RwLock::new(
                LedgerState::init_utxo_map(&utxo_map_path).c(d!())?,
            )),
            block_ctx: Some(BlockEffect::default()),
            api_cache: alt!(*KEEP_HIST, Some(ApiCache::new(&prefix)), None),
            abar_state: abar_state,
            abar_query_state: abar_query_state,
            nullifier_set: LedgerState::init_nullifier_smt(&nullifier_store_path)
                .c(d!())?,
        };

        ledger.status.refresh_data();

        Ok(ledger)
    }

    /// Load an existing one OR create a new one.
    #[inline(always)]
    pub fn load_or_init(basedir: &str) -> Result<LedgerState> {
        let mut ledger = LedgerState::new(basedir, None).c(d!())?;

        let h = ledger.get_tendermint_height();
        ledger.get_staking_mut().set_custom_block_height(h);
        omit!(ledger.utxo_map.write().compute_checksum());
        ledger.fast_invariant_check().c(d!())?;

        flush_data();

        Ok(ledger)
    }

    /// Perform checkpoint of current ledger state
    pub fn checkpoint(&mut self, block: &BlockEffect) -> Result<u64> {
        let merkle_id = self.compute_and_append_txns_hash(&block);
        let pulse_count = block
            .staking_simulator
            .cur_height()
            .saturating_sub(self.get_block_commit_count() + 1);
        self.compute_and_save_state_commitment_data(pulse_count);
        self.utxo_map.write().write().c(d!())?;
        self.txn_merkle.write().write().c(d!())?;
        self.block_merkle.write().write().c(d!())?;

        Ok(merkle_id)
    }

    /// A helper for setting block rewards in ABCI.
    // This function is called from end_block
    pub fn staking_set_last_block_rewards(
        &mut self,
        addr: TendermintAddrRef,
        block_vote_percent: Option<[Power; 2]>,
    ) -> Result<()> {
        // Get Staking Ratio
        let gdp = self.staking_get_global_delegation_percent();
        // Realtime_network_APY (modifier has been included)
        let return_rate = self.staking_get_block_rewards_rate();

        // Record RT_APY for this block in Historical Data
        self.get_staking_mut()
            .record_block_rewards_rate(return_rate);

        let s = self.get_staking();

        // Fetch public key for current nodes (The node on which the binary is running), Tendermint key .
        let pk = s.validator_td_addr_to_app_pk(addr).c(d!())?;

        // Check validator and fetch commission_rate
        let commission_rate = if let Some(v) = s.validator_get_current_one_by_id(&pk) {
            v.commission_rate
        } else {
            return Err(eg!("not validator"));
        };

        let h = s.cur_height;

        // Total balance for coinbase ( Staking rewards distribution address )
        let cbl = s.coinbase_balance();

        // Fetch total delegation amount for this validator ( Self stake + Stake from delegators )
        let total_delegation_amount_of_validator = s
            .delegation_get(&pk)
            .map(|d| d.delegations.get(&pk))
            .flatten()
            .copied()
            .unwrap_or(0)
            + s.validator_get_current_one_by_id(&pk)
                .c(d!())?
                .delegators
                .values()
                .sum::<Amount>();

        // Get total delegation amount
        let gda = s.get_global_delegation_amount();

        // Iterate over every delegation , check if it has an entry for this validator .
        // Set commission rewards for the validator , if a valid delegation to this validator exists
        let commissions = self
            .get_staking_mut()
            .delegation_info
            .global_delegation_records_map
            .values_mut()
            .filter(|d| d.validator_entry_exists(&pk))
            .map(|d| {
                d.set_delegation_rewards(
                    &pk,
                    h,
                    return_rate,
                    commission_rate,
                    gdp,
                    total_delegation_amount_of_validator,
                    gda,
                    true,
                    cbl,
                )
            })
            .collect::<Result<Vec<_>>>()
            .c(d!())?;

        // Add total commission to the Validators own delegation
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
        // p contains ( total staked and total unlocked i.e staking ratio)
        let p = self.staking_get_global_delegation_percent();
        let p = [p[0] as u128, p[1] as u128];

        #[cfg(feature = "debug_env")]
        const APY_V7_UPGRADE_HEIGHT: BlockHeight = 0;

        #[cfg(not(feature = "debug_env"))]
        const APY_V7_UPGRADE_HEIGHT: BlockHeight = 139_8000;

        if APY_V7_UPGRADE_HEIGHT < self.get_tendermint_height() {
            // This is an equal conversion of `1 / p% * 0.0536`
            let mut a0 = p[1] * 536;
            let mut a1 = p[0] * 10000;

            if a0 * 100 > a1 * 268 {
                // max value: 268%
                a0 = 268;
                a1 = 100;
            } else if a0 * 1000 < a1 * 54 {
                // min value: 5.4%
                a0 = 54;
                a1 = 1000;
            }
            [a0, a1]
        } else {
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
    }

    /// Total amount of all freed FRAs, aka 'are not being locked'.
    #[inline(always)]
    pub fn staking_get_global_unlocked_amount(&self) -> Amount {
        #[cfg(feature = "debug_env")]
        const FF_ADDR_EXTRA_FIX_HEIGHT: BlockHeight = 0;

        #[cfg(not(feature = "debug_env"))]
        const FF_ADDR_EXTRA_FIX_HEIGHT: BlockHeight = 120_0000;

        let s = self.get_staking();

        let extras = if FF_ADDR_EXTRA_FIX_HEIGHT < s.cur_height {
            vec![*BLACK_HOLE_PUBKEY, *FF_PK_EXTRA_120_0000]
        } else {
            vec![*BLACK_HOLE_PUBKEY]
        };

        FRA_TOTAL_AMOUNT
            - FF_PK_LIST
                .iter()
                .chain(extras.iter())
                .map(|pk| self.staking_get_nonconfidential_balance(pk).unwrap_or(0))
                .sum::<Amount>()
            - s.coinbase_balance()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    /// Returns amount staked (by all validators across the network and total amount of FRA unlocked )
    /// Returns the latest value from LedgerStatus
    pub fn staking_get_global_delegation_percent(&self) -> [u64; 2] {
        [
            self.get_staking().get_global_delegation_amount(),
            self.staking_get_global_unlocked_amount(),
        ]
    }

    #[inline(always)]
    fn staking_get_nonconfidential_balance(&self, addr: &XfrPublicKey) -> Result<u64> {
        #[cfg(feature = "debug_env")]
        const NONCONFIDENTIAL_BALANCE_FIX_HEIGHT: BlockHeight = 0;

        #[cfg(not(feature = "debug_env"))]
        const NONCONFIDENTIAL_BALANCE_FIX_HEIGHT: BlockHeight = 121_0000;

        if NONCONFIDENTIAL_BALANCE_FIX_HEIGHT < self.get_tendermint_height() {
            self.get_nonconfidential_balance(addr).c(d!())
        } else {
            Ok(0)
        }
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

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_nonconfidential_balance(&self, addr: &XfrPublicKey) -> Option<u64> {
        self.status.nonconfidential_balances.get(addr)
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

    /// Get all abars with sid which are associated with a diversified public key
    #[allow(dead_code)]
    pub fn get_owned_abars(
        &self,
        addr: &AXfrPubKey,
    ) -> Vec<(ATxoSID, AnonBlindAssetRecord)> {
        println!(
            "Public Key: {:?}",
            base64::encode_config(addr.zei_to_bytes().as_slice(), base64::URL_SAFE)
        );

        if let Some(set) = self.status.owned_ax_utxos.get(addr) {
            return set
                .iter()
                .map(|sid| {
                    let abar = self.status.ax_utxos.get(sid).unwrap();
                    (*sid, abar)
                })
                .collect();
        }

        vec![]
    }

    /// Get the owner memo of a abar by ATxoSID
    #[allow(dead_code)]
    pub fn get_abar_memo(&self, ax_id: ATxoSID) -> Option<OwnerMemo> {
        let txn_location = self.status.ax_txo_to_txn_location.get(&ax_id).unwrap();
        let authenticated_txn = self.get_transaction(txn_location.0).unwrap();
        let mut memo: Vec<OwnerMemo> = authenticated_txn
            .finalized_txn
            .txn
            .body
            .operations
            .iter()
            .filter_map(|o| match o {
                Operation::BarToAbar(body) => Some(body.note.body.memo.clone()),
                _ => None,
            })
            .collect::<Vec<OwnerMemo>>();

        memo.append(
            &mut authenticated_txn
                .finalized_txn
                .txn
                .body
                .operations
                .iter()
                .filter_map(|o| match o {
                    Operation::TransferAnonAsset(body) => {
                        body.note.body.owner_memos.get(txn_location.1 .0).cloned()
                    }
                    _ => None,
                })
                .collect::<Vec<OwnerMemo>>(),
        );

        if memo.is_empty() {
            return None;
        }
        Some(memo.get(txn_location.1 .0).unwrap().clone())
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

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_anon_state_commitment(
        &self,
    ) -> (HashOf<Option<AnonStateCommitmentData>>, u64) {
        let block_count = self.status.block_commit_count;
        let commitment = self
            .status
            .anon_state_commitment_versions
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
            utxo_map_bytes = Some(self.utxo_map.read().serialize(0));
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
            let proof = ProofOf::new(merkle.read().get_proof(tx.merkle_id, 0).c(d!())?);

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
                        .read()
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
#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
pub struct LedgerStatus {
    /// the file path of the snapshot
    pub snapshot_file: String,
    // all currently-unspent TXOs
    utxos: Mapxnk<TxoSID, Utxo>,
    nonconfidential_balances: Mapx<XfrPublicKey, u64>,
    owned_utxos: Mapx<XfrPublicKey, HashSet<TxoSID>>,
    /// all existing ax_utxos
    ax_utxos: Mapx<ATxoSID, AnonBlindAssetRecord>,
    /// all owned abars
    owned_ax_utxos: Mapx<AXfrPubKey, HashSet<ATxoSID>>,
    /// all spent TXOs
    pub spent_utxos: Mapxnk<TxoSID, Utxo>,
    // Map a TXO to its output position in a transaction
    txo_to_txn_location: Mapxnk<TxoSID, (TxnSID, OutputPosition)>,
    /// Map a Anonymous TXO to its output position in a transaction
    ax_txo_to_txn_location: Mapx<ATxoSID, (TxnSID, OutputPosition)>,
    // State commitment history.
    // The BitDigest at index i is the state commitment of the ledger at block height  i + 1.
    state_commitment_versions: Vecx<HashOf<Option<StateCommitmentData>>>,
    // Abar commitment versions for verifying proofs
    abar_commitment_versions: Vecx<BLSScalar>,
    // Anon state commitment versions
    anon_state_commitment_versions: Vecx<HashOf<Option<AnonStateCommitmentData>>>,
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
    // Should be equal to the count of ABARs
    next_atxo: ATxoSID,
    // Each block corresponds to such a summary structure
    state_commitment_data: Option<StateCommitmentData>,
    // Anon state commitment
    anon_state_commitment_data: Option<AnonStateCommitmentData>,
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
    pub fn get_owned_abars_ids(&self, addr: &AXfrPubKey) -> Vec<ATxoSID> {
        self.ax_utxos
            .iter()
            .filter(|(_, axutxo)| &axutxo.public_key == addr)
            .map(|(sid, _)| sid.clone().to_owned())
            .collect()
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

    #[inline(always)]
    #[allow(missing_docs)]
    #[allow(dead_code)]
    fn get_latest_abar_hash(&self) -> Option<BLSScalar> {
        self.abar_commitment_versions.last()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    #[allow(dead_code)]
    pub fn add_abar_commitment(&mut self, hash: BLSScalar) {
        self.abar_commitment_versions.push(hash)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    #[allow(dead_code)]
    fn get_versioned_abar_hash(&self, version: usize) -> Option<BLSScalar> {
        self.abar_commitment_versions.get(version)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    #[allow(dead_code)]
    fn get_current_abar_version(&self) -> usize {
        self.abar_commitment_versions.len() - 1
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
    pub fn new(basedir: &str, snapshot_file: &str) -> Result<LedgerStatus> {
        let path = format!("{}/{}", basedir, snapshot_file);
        match fs::read_to_string(path) {
            Ok(s) => serde_json::from_str(&s).c(d!()),
            Err(e) => {
                if ErrorKind::NotFound != e.kind() {
                    Err(eg!(e))
                } else {
                    Self::create(snapshot_file).c(d!())
                }
            }
        }
    }

    fn create(snapshot_file: &str) -> Result<LedgerStatus> {
        let utxos_path = SNAPSHOT_ENTRIES_DIR.to_owned() + "/utxo";
        let ax_utxos_path = SNAPSHOT_ENTRIES_DIR.to_owned() + "/ax_utxos";
        let nonconfidential_balances_path =
            SNAPSHOT_ENTRIES_DIR.to_owned() + "/nonconfidential_balances";
        let spent_utxos_path = SNAPSHOT_ENTRIES_DIR.to_owned() + "/spent_utxos";
        let txo_to_txn_location_path =
            SNAPSHOT_ENTRIES_DIR.to_owned() + "/txo_to_txn_location";
        let atxo_to_txn_location_path =
            SNAPSHOT_ENTRIES_DIR.to_owned() + "/atxo_to_txn_location";
        let issuance_amounts_path =
            SNAPSHOT_ENTRIES_DIR.to_owned() + "/issuance_amounts";
        let state_commitment_versions_path =
            SNAPSHOT_ENTRIES_DIR.to_owned() + "/state_commitment_versions";
        let abar_commitment_versions_path =
            SNAPSHOT_ENTRIES_DIR.to_owned() + "/abar_commitment_versions";
        let anon_state_commitment_versions_path =
            SNAPSHOT_ENTRIES_DIR.to_owned() + "/anon_state_commitment_versions";
        let asset_types_path = SNAPSHOT_ENTRIES_DIR.to_owned() + "/asset_types";
        let issuance_num_path = SNAPSHOT_ENTRIES_DIR.to_owned() + "/issuance_num";
        let owned_utxos_path = SNAPSHOT_ENTRIES_DIR.to_owned() + "/owned_utxos";
        let owned_ax_utxos_path = SNAPSHOT_ENTRIES_DIR.to_owned() + "/owned_ax_utxos";

        let ledger = LedgerStatus {
            snapshot_file: snapshot_file.to_owned(),
            sliding_set: SlidingSet::<[u8; 8]>::new(TRANSACTION_WINDOW_WIDTH as usize),
            utxos: new_mapxnk!(utxos_path.as_str()),
            nonconfidential_balances: new_mapx!(nonconfidential_balances_path.as_str()),
            owned_utxos: new_mapx!(owned_utxos_path.as_str()),
            ax_utxos: new_mapx!(ax_utxos_path.as_str()),
            owned_ax_utxos: new_mapx!(owned_ax_utxos_path.as_str()),
            spent_utxos: new_mapxnk!(spent_utxos_path.as_str()),
            txo_to_txn_location: new_mapxnk!(txo_to_txn_location_path.as_str()),
            issuance_amounts: new_mapx!(issuance_amounts_path.as_str()),
            state_commitment_versions: new_vecx!(state_commitment_versions_path.as_str()),
            abar_commitment_versions: new_vecx!(abar_commitment_versions_path.as_str()),
            anon_state_commitment_versions: new_vecx!(
                anon_state_commitment_versions_path.as_str()
            ),
            asset_types: new_mapx!(asset_types_path.as_str()),
            tracing_policies: map! {},
            issuance_num: new_mapx!(issuance_num_path.as_str()),
            next_txn: TxnSID(0),
            next_txo: TxoSID(0),
            next_atxo: ATxoSID(0),
            txns_in_block_hash: None,
            state_commitment_data: None,
            anon_state_commitment_data: None,
            block_commit_count: 0,
            staking: Staking::new(),
            td_commit_height: 0,
            ax_txo_to_txn_location: new_mapx!(atxo_to_txn_location_path.as_str()),
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

        // An axfr_body requires abar merkle root hash for AxfrNote verification. This is done
        // here with LedgerStatus available.
        for axfr_body in txn_effect.axfr_bodies.iter() {
            // TODO: maybe load user_params in memory for inp_op mapping
            let user_params = UserParams::from_file_if_exists(
                axfr_body.inputs.len(),
                axfr_body.outputs.len(),
                Some(41),
                DEFAULT_BP_NUM_GENS,
                None,
            )
            .unwrap();
            let node_params = NodeParams::from(user_params);
            let abar_version = axfr_body.proof.merkle_root_version;
            verify_anon_xfr_body(
                &node_params,
                axfr_body,
                // Unwrap Now to get the code to compile . Change in Zei later to accept Option<BLSScalar>
                &self.get_versioned_abar_hash(abar_version as usize).unwrap(),
            )
            .c(d!())?;
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
                #[allow(unused_mut)]
                if let Some(mut bl) = self
                    .nonconfidential_balances
                    .get_mut(&v.0.record.public_key)
                {
                    *bl -= v.get_nonconfidential_balance();
                }
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
                        let utxo = Utxo(tx_output);
                        *self
                            .nonconfidential_balances
                            .entry(utxo.0.record.public_key)
                            .or_insert(0) += utxo.get_nonconfidential_balance();
                        self.utxos.insert(TxoSID(txo_sid), utxo);
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

    fn refresh_data(&mut self) {
        if self.nonconfidential_balances.is_empty() {
            self.utxos
                .iter()
                .collect::<Vec<_>>()
                .into_iter()
                .for_each(|(_, txo)| {
                    *self
                        .nonconfidential_balances
                        .entry(txo.0.record.public_key)
                        .or_insert(0) += txo.get_nonconfidential_balance();
                });
        }
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
    fbnc::flush_data();
}

/// convert merkle tree proof to Zei compatible proofs
pub fn create_mt_leaf_info(proof: Proof) -> MTLeafInfo {
    MTLeafInfo {
        path: MTPath {
            nodes: proof
                .nodes
                .iter()
                .map(|e| MTNode {
                    siblings1: e.siblings1,
                    siblings2: e.siblings2,
                    is_left_child: e.is_left_child,
                    is_right_child: e.is_right_child,
                })
                .collect(),
        },
        root: proof.root,
        root_version: proof.root_version,
        uid: proof.uid,
    }
}
