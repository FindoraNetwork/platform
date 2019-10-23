extern crate bincode;
extern crate byteorder;
extern crate findora;
extern crate tempdir;

use crate::data_model::errors::PlatformError;
use crate::data_model::{
  AssetPolicyKey, AssetType, AssetTypeCode, CustomAssetPolicy, DefineAsset, FinalizedTransaction,
  IssueAsset, Operation, SmartContract, SmartContractKey, Transaction, TransferAsset, TxOutput,
  TxnSID, TxnTempSID, TxoRef, TxoSID, Utxo, TXN_SEQ_ID_PLACEHOLDER,
};
use crate::utils::sha256;
use crate::utils::sha256::Digest as BitDigest;
use append_only_merkle::{AppendOnlyMerkle, Proof};
use bitmap::BitMap;
use findora::HasInvariants;
use logged_merkle::LoggedMerkle;
use rand::SeedableRng;
use rand::{CryptoRng, Rng};
use rand_chacha::ChaChaRng;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::fs::OpenOptions;
use std::io::BufReader;
use std::slice::from_raw_parts;
use std::sync::{Arc, RwLock};
use std::u64;
use tempdir::TempDir;
use zei::xfr::structs::{BlindAssetRecord, EGPubKey};

use super::append_only_merkle;
use super::bitmap;
use super::effects::*;
use super::logged_merkle;

pub struct SnapshotId {
  pub id: u64,
}

pub trait LedgerAccess {
  // Look up a currently unspent TXO
  fn get_utxo(&self, addr: TxoSID) -> Option<&Utxo>;

  // The most recently-issued sequence number for the `code`-labelled asset
  // type
  fn get_issuance_num(&self, code: &AssetTypeCode) -> Option<u64>;

  // Retrieve asset type metadata
  fn get_asset_type(&self, code: &AssetTypeCode) -> Option<&AssetType>;

  // TODO(joe): figure out what to do for these.
  // See comments about asset policies and tracked SIDs in LedgerStatus
  // fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy>;
  //  // Asset issuers can query ids of UTXOs of assets they are tracking
  // fn get_tracked_sids(&self, key: &EGPubKey)       -> Option<Vec<TxoSID>>;
}

pub trait LedgerUpdate<RNG: Rng + CryptoRng> {
  // Each Block represents a collection of transactions which have been
  // validated and confirmed to be unconditionally consistent with the
  // ledger and with each other.
  type Block;

  fn get_prng(&mut self) -> &mut RNG;

  // Returns a Block object representing an in-construction group of
  // transactions.
  //
  // NOTE: To ensure that each Block represents an *unconditionally*
  // consistent collection of transactions, there should only be one live
  // Block allowed at a time, unless you have a very good, carefully
  // researched, checked-by-someone-else reason.
  fn start_block(&mut self) -> Result<Self::Block, PlatformError>;

  // Update the Block state, validating the *external* properties of
  // the TxnEffect against the current block and state of the ledger.
  //
  // Returns:
  //   If valid: An identifier representing the transaction within this
  //             Block
  //   If invalid: Err(...)
  //
  // When Err is returned, no modifications are made to the Block.
  //
  // NOTE: This function is allowed to assume that the TxnEffect is
  // internally consistent, and matches its internal Transaction
  // object, so any caller of this *must* validate the TxnEffect
  // properly first.
  fn apply_transaction(&self,
                       block: &mut Self::Block,
                       txn: TxnEffect)
                       -> Result<TxnTempSID, PlatformError>;

  // Abort an in-development block. No effects of the block are reflected
  // in the ledger.
  //
  // Returns:
  //   Map of (temp identifier -> original transaction) for applied
  //   transactions in this block.
  fn abort_block(&mut self, block: Self::Block) -> HashMap<TxnTempSID, Transaction>;

  // Record a block into the ledger, applying the effects of all included
  // transactions and making those effects externally visible.
  //
  // Returns:
  //   If valid: Map from temporary IDs to the finalized Transaction SID
  //      and the finalized TXO SIDs of that transaction's UTXOs. UTXO
  //      SIDs for each transaction will be in increasing order.
  //   If invalid: Err(...)
  //
  // When Err(...) is returned, no modifications are made to the ledger.
  fn finish_block(&mut self, block: Self::Block) -> HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>;
}

pub trait ArchiveAccess {
  // Number of transactions available
  fn get_transaction_count(&self) -> usize;
  // Look up transaction in the log
  fn get_transaction(&self, addr: TxnSID) -> Option<&FinalizedTransaction>;
  // Get consistency proof for TxnSID `addr`
  fn get_proof(&self, addr: TxnSID) -> Option<Proof>;

  // This previously did the serialization at the call to this, and
  // unconditionally returned Some(...).
  // fn get_utxo_map     (&mut self)                   -> Vec<u8>;
  // I (joe) think returning &BitMap matches the intended usage a bit more
  // closely
  fn get_utxo_map(&self) -> &BitMap;

  // Since serializing the bitmap requires mutation access, I'm (joe)
  // making this a separate method
  fn serialize_utxo_map(&mut self) -> Vec<u8>;

  // TODO(joe): figure out what interface this should have -- currently
  // there isn't anything to handle out-of-bounds indices from `list`
  // fn get_utxos        (&mut self, list: Vec<usize>) -> Option<Vec<u8>>;

  // Get the bitmap's hash at version `version`, if such a hash is
  // available.
  fn get_utxo_checksum(&self, version: u64) -> Option<BitDigest>;

  // Get the hash of the most recent checkpoint, and its sequence number.
  fn get_global_hash(&self) -> (BitDigest, u64);
}

#[repr(C)]
struct GlobalHashData {
  pub bitmap: BitDigest,
  pub merkle: append_only_merkle::HashValue,
  pub block: u64,
  pub global_hash: BitDigest,
}

impl GlobalHashData {
  fn as_ref(&self) -> &[u8] {
    unsafe {
      from_raw_parts((self as *const GlobalHashData) as *const u8,
                     std::mem::size_of::<GlobalHashData>())
    }
  }
}

const MAX_VERSION: usize = 100;

// Parts of the current ledger state which can be restored from a snapshot
// without replaying a log
#[derive(Deserialize, Serialize)]
pub struct LedgerStatus {
  // Paths to archival logs for the merkle tree and transaction history
  merkle_path: String,
  txn_path: String,
  utxo_map_path: String,

  // TODO(joe): The old version of LedgerState had this field but it didn't
  // seem to be used for anything -- so we should figure out what it's
  // supposed to be for and whether or not having a reference to what file
  // the state is loaded from in the state itself is a good idea.
  // snapshot_path:       String,

  // All currently-unspent TXOs
  utxos: HashMap<TxoSID, Utxo>,

  // Digests of the UTXO bitmap to (I think -joe) track recent states of
  // the UTXO map
  // TODO(joe): should this be an ordered map of some sort?
  utxo_map_versions: VecDeque<(TxnSID, BitDigest)>,

  // TODO(joe): This field should probably exist, but since it is not
  // currently used by anything I'm leaving it commented out. We should
  // figure out (a) whether it should exist and (b) what it should do
  // policies:            HashMap<AssetPolicyKey, CustomAssetPolicy>,

  // TODO(joe): Similar to `policies`, but possibly more grave. The prior
  // implementation updated this map in `add_txo`, but there doesn't seem
  // to be any logic to actually apply or verify the tracking proofs.
  // Specifically, there are several tests which check that the right
  // TxoSIDs get added to this map under the right EGPubKey, but all
  // tracking proofs appear to be implemented with Default::default() and
  // no existing code attempts to check the asset tracking proof through
  // some `zei` interface.
  //
  // tracked_sids:        HashMap<EGPubKey,       Vec<TxoSID>>,

  // Registered asset types, and one-more-than the most recently issued
  // sequence number. Issuance numbers must be increasing over time to
  // prevent replays, but (as far as I know -joe) need not be strictly
  // sequential.
  asset_types: HashMap<AssetTypeCode, AssetType>,
  issuance_num: HashMap<AssetTypeCode, u64>,

  // Should be equal to the count of transactions
  next_txn: TxnSID,
  // Should be equal to the count of TXOs
  next_txo: TxoSID,

  // Hash and sequence number of the most recent "full checkpoint" of the
  // ledger -- committing to the whole ledger history up to the most recent
  // such checkpoint.
  global_hash: BitDigest,
  global_commit_count: u64,
}

pub struct LedgerState {
  status: LedgerStatus,

  // PRNG used for transaction validation
  prng: ChaChaRng,

  // Merkle tree tracking the sequence of transaction hashes
  merkle: LoggedMerkle,

  // The `FinalizedTransaction`s consist of a Transaction and an index into
  // `merkle` representing its hash.
  // TODO(joe): should this be in-memory?
  txs: Vec<FinalizedTransaction>,

  // Bitmap tracking all the live TXOs
  utxo_map: BitMap,

  // TODO(joe): use this file handle to actually record transactions
  txn_log: File,

  block_ctx: Option<BlockEffect>,
}

// TODO(joe): fill these in
impl HasInvariants<PlatformError> for LedgerStatus {
  fn fast_invariant_check(&self) -> Result<(), PlatformError> {
    Ok(())
  }

  fn deep_invariant_check(&self) -> Result<(), PlatformError> {
    Ok(())
  }
}

// TODO(joe): fill these in
impl HasInvariants<PlatformError> for LedgerState {
  fn fast_invariant_check(&self) -> Result<(), PlatformError> {
    Ok(())
  }

  fn deep_invariant_check(&self) -> Result<(), PlatformError> {
    Ok(())
  }
}

impl LedgerStatus {
  pub fn new(merkle_path: &str,
             txn_path: &str,
             // TODO(joe): should this do something?
             // snapshot_path: &str,
             utxo_map_path: &str)
             -> Result<LedgerStatus, std::io::Error> {
    let ledger = LedgerStatus { merkle_path: merkle_path.to_owned(),
                                txn_path: txn_path.to_owned(),
                                utxo_map_path: utxo_map_path.to_owned(),
                                utxos: HashMap::new(),
                                utxo_map_versions: VecDeque::new(),
                                asset_types: HashMap::new(),
                                issuance_num: HashMap::new(),
                                next_txn: TxnSID(0),
                                next_txo: TxoSID(0),
                                global_hash: BitDigest { 0: [0_u8; 32] },
                                global_commit_count: 0 };

    Ok(ledger)
  }

  // Check that `txn` can be safely applied to the current ledger.
  //
  // Returns the same TxnEffect (unchanged) if it is safe. Consumes `txn`
  // if it would be invalid to prevent a simple case of forgetting error
  // handling, such as:
  //
  //  ledger.check_txn_effects(txn_effect);
  //  block.add_txn_effect(txn_effect);
  //
  fn check_txn_effects(&self, txn: TxnEffect) -> Result<TxnEffect, PlatformError> {
    // Each input must be unspent and correspond to the claimed record
    for (inp_sid, inp_record) in txn.input_txos.iter() {
      let inp_utxo = self.utxos
                         .get(inp_sid)
                         .map_or(Err(PlatformError::InputsError), Ok)?;
      let record = &(inp_utxo.0).0;
      if record != inp_record {
        return Err(PlatformError::InputsError);
      }
    }

    // New asset types must not already exist
    for (code, asset_type) in txn.new_asset_codes.iter() {
      if self.asset_types.contains_key(&code) {
        return Err(PlatformError::InputsError);
      }
      if self.issuance_num.contains_key(&code) {
        return Err(PlatformError::InputsError);
      }
      debug_assert!(txn.new_issuance_nums.contains_key(&code));

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
    for (code, seq_nums) in txn.new_issuance_nums.iter() {
      debug_assert!(txn.issuance_keys.contains_key(&code));

      let iss_key = txn.issuance_keys.get(&code).unwrap();
      let proper_key = self.asset_types
                           .get(&code)
                           .or_else(|| txn.new_asset_codes.get(&code))
                           .ok_or(PlatformError::InputsError)?
                           .properties
                           .issuer;
      if *iss_key != proper_key {
        return Err(PlatformError::InputsError);
      }

      if seq_nums.is_empty() {
        if !txn.new_asset_codes.contains_key(&code) {
          return Err(PlatformError::InputsError);
        }
      // We could re-check that self.issuance_num doesn't contain `code`,
      // but currently it's redundant with the new-asset-type checks
      } else {
        let curr_seq_num_limit = self.issuance_num.get(&code).unwrap();
        let min_seq_num = seq_nums.first().unwrap();
        if min_seq_num < curr_seq_num_limit {
          return Err(PlatformError::InputsError);
        }
      }
    }

    Ok(txn)
  }

  // This function assumes that `block` is COMPLETELY CONSISTENT with the
  // ledger state. Calling `check_txn_effects` for each TxnEffect getting
  // mixed into the BlockEffect *should* be enough to guarantee that (if
  // that is ever false, it's a bug).
  //
  // This drains every field of `block` except `txns` and `temp_sids`.
  fn apply_block_effects(&mut self,
                         block: &mut BlockEffect)
                         -> HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)> {
    // Remove consumed UTXOs
    for (inp_sid, _) in block.input_txos.drain() {
      debug_assert!(self.utxos.contains_key(&inp_sid));
      self.utxos.remove(&inp_sid);
    }

    // Add new UTXOs
    // Each transaction gets a TxnSID, and each of its unspent TXOs gets
    // a TxoSID. TxoSID assignments are based on the order TXOs appear in
    // the transaction.
    let mut new_utxo_sids: HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)> = HashMap::new();
    {
      let next_txn = &mut self.next_txn;
      let next_txo = &mut self.next_txo;

      debug_assert!(block.txos.len() == block.txns.len());
      debug_assert!(block.txos.len() == block.temp_sids.len());
      for (ix, txos) in block.temp_sids.iter().zip(block.txos.drain(..)) {
        let txn_sid = *next_txn;
        next_txn.0 += 1;

        let mut txn_utxo_sids: Vec<TxoSID> = vec![];

        for txo in txos {
          let txo_sid = *next_txo;
          next_txo.0 += 1;
          if let Some(tx_output) = txo {
            self.utxos.insert(txo_sid, Utxo(tx_output));
            txn_utxo_sids.push(txo_sid);
          }
        }

        new_utxo_sids.insert(*ix, (txn_sid, txn_utxo_sids));
      }
    }

    // Update issuance sequence number limits
    for (code, seq_nums) in block.new_issuance_nums.drain() {
      // One more than the greatest sequence number, or 0
      let new_max_seq_num = seq_nums.last().map(|x| x + 1).unwrap_or(0);
      self.issuance_num.insert(code, new_max_seq_num);
    }

    // Register new asset types
    for (code, asset_type) in block.new_asset_codes.drain() {
      debug_assert!(!self.asset_types.contains_key(&code));
      self.asset_types.insert(code, asset_type.clone());
    }

    // issuance_keys should already have been checked
    block.issuance_keys.clear();

    debug_assert!(block.temp_sids.len() == block.txns.len());
    debug_assert!(block.txos.is_empty());
    debug_assert!(block.input_txos.is_empty());
    debug_assert!(block.new_asset_codes.is_empty());
    debug_assert!(block.new_issuance_nums.is_empty());
    debug_assert!(block.issuance_keys.is_empty());

    new_utxo_sids
  }
}

impl LedgerUpdate<ChaChaRng> for LedgerState {
  type Block = BlockEffect;

  fn get_prng(&mut self) -> &mut ChaChaRng {
    &mut self.prng
  }

  fn start_block(&mut self) -> Result<BlockEffect, PlatformError> {
    let mut block_ctx = None;
    std::mem::swap(&mut self.block_ctx, &mut block_ctx);
    match block_ctx {
      None => Err(PlatformError::InputsError), // Probably should be a more relevant error
      Some(block) => Ok(block),
    }
  }

  fn apply_transaction(&self,
                       block: &mut BlockEffect,
                       txn: TxnEffect)
                       -> Result<TxnTempSID, PlatformError> {
    block.add_txn_effect(self.status.check_txn_effects(txn)?)
  }

  fn abort_block(&mut self, block: BlockEffect) -> HashMap<TxnTempSID, Transaction> {
    let mut block = block;
    let txns = block.txns.drain(..);
    let ret: HashMap<TxnTempSID, Transaction> = block.temp_sids.drain(..).zip(txns).collect();

    block.txos.clear();
    block.input_txos.clear();
    block.new_asset_codes.clear();
    block.new_issuance_nums.clear();
    block.issuance_keys.clear();

    debug_assert!(block.temp_sids.is_empty());
    debug_assert!(block.txns.is_empty());
    debug_assert!(block.txos.is_empty());
    debug_assert!(block.input_txos.is_empty());
    debug_assert!(block.new_asset_codes.is_empty());
    debug_assert!(block.new_issuance_nums.is_empty());
    debug_assert!(block.issuance_keys.is_empty());

    return ret;
  }

  fn finish_block(&mut self, block: BlockEffect) -> HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)> {
    let mut block = block;

    let base_sid = self.status.next_txo.0;
    let txn_temp_sids = block.temp_sids.clone();
    let temp_sid_map = self.status.apply_block_effects(&mut block);
    let max_sid = self.status.next_txo.0; // mutated by apply_txn_effects

    // debug_assert!(utxo_sids.is_sorted());

    {
      // Update the UTXO bitmap
      // This is, unfortunately, some horrible index-walking messiness.
      // The core idea is that we walk over every new TXO SID (ix), tracking:
      //  - by `temp_sid_ix`, which transaction we're in
      //  - by `txo_sid_ix`, which UTXO within that transaction is next.
      let mut temp_sid_ix = 0;
      let mut txo_sid_ix = 0;

      // Find the first index that matters
      while temp_sid_ix < txn_temp_sids.len()
            && (temp_sid_map[&txn_temp_sids[temp_sid_ix]].1).is_empty()
      {
        temp_sid_ix += 1;
      }

      for ix in base_sid..max_sid {
        debug_assert!(temp_sid_ix < txn_temp_sids.len());

        let temp_sid = txn_temp_sids[temp_sid_ix];
        let utxo_sids = &temp_sid_map[&temp_sid].1;

        debug_assert!(txo_sid_ix < utxo_sids.len());

        // Only .set() extends the bitmap, so to append a 0 we currently
        // nead to .set() then .clear().
        //
        // TODO(joe): are these unwraps okay?
        self.utxo_map.set(ix as usize).unwrap();
        if let Some(TxoSID(utxo_sid)) = utxo_sids.get(txo_sid_ix) {
          if *utxo_sid != ix {
            self.utxo_map.clear(ix as usize).unwrap();
          } else {
            txo_sid_ix += 1;

            // We've reached the end of this UTXO list, search for the next
            // relevant one
            if txo_sid_ix == utxo_sids.len() {
              txo_sid_ix = 0;

              temp_sid_ix += 1;
              while temp_sid_ix < txn_temp_sids.len()
                    && (temp_sid_map[&txn_temp_sids[temp_sid_ix]].1).is_empty()
              {
                temp_sid_ix += 1;
              }
            }
          }
        }
      }
      debug_assert!(temp_sid_ix == txn_temp_sids.len());
      debug_assert!(txo_sid_ix == 0);
    }

    // Update the Merkle tree and transaction log
    for (tmp_sid, txn) in block.temp_sids.drain(..).zip(block.txns.drain(..)) {
      let txn_sid = temp_sid_map.get(&tmp_sid).unwrap().0;
      let hash = txn.compute_merkle_hash(txn_sid);

      // TODO(joe/jonathan): Since errors in the merkle tree are things like
      // corruption and I/O failure, we don't have a good recovery story. Is
      // panicking acceptable?
      let merkle_id = self.merkle.append(&hash).unwrap();

      self.txs.push(FinalizedTransaction { txn: txn,
                                           tx_id: txn_sid,
                                           merkle_id });
    }

    // Compute hash against history
    self.checkpoint();

    // TODO(joe): asset tracing?

    debug_assert!(block.temp_sids.is_empty());
    debug_assert!(block.txns.is_empty());
    debug_assert!(block.txos.is_empty());
    debug_assert!(block.input_txos.is_empty());
    debug_assert!(block.new_asset_codes.is_empty());
    debug_assert!(block.new_issuance_nums.is_empty());

    self.block_ctx = Some(block);

    temp_sid_map
  }
}

impl LedgerState {
  // Create a ledger for use by a unit test.
  pub fn test_ledger() -> LedgerState {
    let tmp_dir = TempDir::new("test").unwrap();

    let merkle_buf = tmp_dir.path().join("test_ledger_merkle");
    let merkle_path = merkle_buf.to_str().unwrap();

    let txn_buf = tmp_dir.path().join("test_ledger_txns");
    let txn_path = txn_buf.to_str().unwrap();

    // let snap_buf      = tmp_dir.path().join("test_ledger_snap");
    // let snap_path     = snap_buf.to_str().unwrap();

    let utxo_map_buf = tmp_dir.path().join("test_ledger_utxo_map");
    let utxo_map_path = utxo_map_buf.to_str().unwrap();

    LedgerState::new(&merkle_path, &txn_path, &utxo_map_path, None, true).unwrap()
  }

  fn load_transaction_log(path: &str) -> Result<Vec<FinalizedTransaction>, std::io::Error> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut v = Vec::new();
    while let Ok(next_txn) =
      bincode::deserialize_from::<&mut BufReader<File>, FinalizedTransaction>(&mut reader)
    {
      v.push(next_txn);
    }
    Ok(v)
  }

  fn save_utxo_map_version(&mut self) {
    if self.status.utxo_map_versions.len() >= MAX_VERSION {
      self.status.utxo_map_versions.pop_front();
    }

    self.status
        .utxo_map_versions
        .push_back((self.status.next_txn, self.utxo_map.compute_checksum()));
  }

  fn save_global_hash(&mut self) {
    let data = GlobalHashData { bitmap: self.utxo_map.compute_checksum(),
                                merkle: self.merkle.get_root_hash(),
                                block: self.status.global_commit_count,
                                global_hash: self.status.global_hash };

    self.status.global_hash = sha256::hash(data.as_ref());
    self.status.global_commit_count += 1;
  }

  // Initialize a logged Merkle tree for the ledger.  We might
  // be creating a new tree or opening an existing one.  We
  // always start a new log file.
  fn init_merkle_log(path: &str, create: bool) -> Result<LoggedMerkle, std::io::Error> {
    // Create a merkle tree or open an existing one.
    let result = if create {
      AppendOnlyMerkle::create(path)
    } else {
      AppendOnlyMerkle::open(path)
    };

    log!(Store, "Using path {} for the Merkle tree.", path);

    let tree = match result {
      Err(x) => {
        return Err(x);
      }
      Ok(tree) => tree,
    };

    // Create a log for the tree.  The tree size ("state") is appended to
    // the end of the path.
    let next_id = tree.total_size();
    let writer = LedgerState::create_merkle_log(path.to_owned(), next_id)?;
    Ok(LoggedMerkle::new(tree, writer))
  }

  // Initialize a bitmap to track the unspent utxos.
  fn init_utxo_map(path: &str, create: bool) -> Result<BitMap, std::io::Error> {
    let file = OpenOptions::new().read(true)
                                 .write(true)
                                 .create_new(create)
                                 .open(path)?;

    if create {
      BitMap::create(file)
    } else {
      BitMap::open(file)
    }
  }

  // Initialize a new Ledger structure.
  pub fn new(merkle_path: &str,
             txn_path: &str,
             // snapshot_path: &str,
             utxo_map_path: &str,
             prng_seed: Option<[u8; 32]>,
             create: bool)
             -> Result<LedgerState, std::io::Error> {
    let ledger = LedgerState { status: LedgerStatus::new(merkle_path, txn_path, utxo_map_path)?,
                               // TODO(joe): is this safe?
                               prng:
                                 rand_chacha::ChaChaRng::from_seed(prng_seed.unwrap_or([0u8; 32])),
                               merkle: LedgerState::init_merkle_log(merkle_path, create)?,
                               txs: Vec::new(),
                               utxo_map: LedgerState::init_utxo_map(utxo_map_path, create)?,
                               txn_log: std::fs::OpenOptions::new().create(create)
                                                                   .append(true)
                                                                   .open(txn_path)?,
                               block_ctx: Some(BlockEffect::new()) };

    Ok(ledger)
  }

  // Load a ledger given the paths to the various storage elements.
  pub fn load(merkle_path: &str,
              txn_path: &str,
              utxo_map_path: &str,
              prng_seed: Option<[u8; 32]>,
              snapshot_path: &str)
              -> Result<LedgerState, std::io::Error> {
    let merkle = LedgerState::init_merkle_log(merkle_path, false)?;
    let utxo_map = LedgerState::init_utxo_map(utxo_map_path, false)?;
    let txs = LedgerState::load_transaction_log(txn_path)?;
    let ledger_file = File::open(snapshot_path)?;
    let status      = bincode::deserialize_from
                             ::<BufReader<File>, LedgerStatus>(
                                  BufReader::new(ledger_file)
                             ).map_err(|e|
                                std::io::Error::new(
                                  std::io::ErrorKind::Other, e)
                             )?;
    let txn_log = OpenOptions::new().append(true).open(txn_path)?;

    // TODO(joe): thoughts about write-ahead transaction log so that
    // recovery can happen between snapshots.
    // for txn in &txs[ledger.txn_count..] {
    //   ledger.apply_transaction(&txn);
    // }

    let prng =
        // TODO(joe): is this safe?
        rand_chacha::ChaChaRng::from_seed(prng_seed.unwrap_or([0u8;32]));

    let ledger = LedgerState { status,
                               prng,
                               merkle,
                               txs,
                               utxo_map,
                               txn_log,
                               block_ctx: Some(BlockEffect::new()) };
    assert!(ledger.txs.len() == ledger.status.next_txn.0);
    Ok(ledger)
  }

  // Snapshot the ledger state.  This involves synchronizing
  // the durable data structures to the disk and starting a
  // new log file for the logged Merkle tree.
  //
  // TODO(joe): Actually serialize the active ledger state.
  pub fn snapshot(&mut self) -> Result<SnapshotId, std::io::Error> {
    let state = self.merkle.state();
    let writer = LedgerState::create_merkle_log(self.status.merkle_path.clone(), state)?;
    self.merkle.snapshot(writer)?;

    Ok(SnapshotId { id: state })
  }

  // pub fn begin_commit(&mut self) {
  //   self.txn_base_sid.0 = self.max_applied_sid.0 + 1;
  // }

  pub fn checkpoint(&mut self) {
    self.save_utxo_map_version();
    self.save_global_hash();
  }

  // Create a file structure for a Merkle tree log.
  // Mostly just make a path of the form:
  //
  //     <tree_path>-log-<Merkle tree state>
  //
  fn create_merkle_log(base_path: String, next_id: u64) -> Result<File, std::io::Error> {
    let log_path = base_path.to_owned() + "-log-" + &next_id.to_string();
    println!("merkle log:  {}", log_path);
    let result = OpenOptions::new().write(true)
                                   .create(true)
                                   .truncate(true)
                                   .open(&log_path);

    let file = match result {
      Ok(file) => file,
      Err(error) => {
        println!("File open failed for {}", log_path);
        return Err(error);
      }
    };

    Ok(file)
  }
}

impl LedgerAccess for LedgerStatus {
  fn get_utxo(&self, addr: TxoSID) -> Option<&Utxo> {
    self.utxos.get(&addr)
  }

  fn get_issuance_num(&self, code: &AssetTypeCode) -> Option<u64> {
    self.issuance_num.get(code).map(|x| *x)
  }

  fn get_asset_type(&self, code: &AssetTypeCode) -> Option<&AssetType> {
    self.asset_types.get(code)
  }
}

impl LedgerAccess for LedgerState {
  fn get_utxo(&self, addr: TxoSID) -> Option<&Utxo> {
    self.status.get_utxo(addr)
  }

  fn get_issuance_num(&self, code: &AssetTypeCode) -> Option<u64> {
    self.status.get_issuance_num(code)
  }

  fn get_asset_type(&self, code: &AssetTypeCode) -> Option<&AssetType> {
    self.status.get_asset_type(code)
  }
}

impl ArchiveAccess for LedgerState {
  fn get_transaction(&self, addr: TxnSID) -> Option<&FinalizedTransaction> {
    self.txs.get(addr.0)
  }

  fn get_proof(&self, addr: TxnSID) -> Option<Proof> {
    match self.get_transaction(addr) {
      None => None,
      Some(txn) => {
        let merkle = &self.merkle;
        // TODO log error and recover?
        Some(merkle.get_proof(txn.merkle_id, 0).unwrap())
      }
    }
  }

  fn get_transaction_count(&self) -> usize {
    self.txs.len()
  }
  fn get_utxo_map(&self) -> &BitMap {
    &self.utxo_map
  }
  fn serialize_utxo_map(&mut self) -> Vec<u8> {
    self.utxo_map.serialize(self.txs.len())
  }

  // TODO(joe): see notes in ArchiveAccess about these
  // fn get_utxo_map(&mut self) -> Option<Vec<u8>> {
  //   Some(self.utxo_map.as_mut().unwrap().serialize(self.txn_count))
  // }
  // fn get_utxos(&mut self, utxo_list: Vec<usize>) -> Option<Vec<u8>> {
  //   Some(self.utxo_map
  //            .as_mut()
  //            .unwrap()
  //            .serialize_partial(utxo_list, self.txn_count))
  // }

  fn get_utxo_checksum(&self, version: u64) -> Option<BitDigest> {
    // TODO:  This could be done via a hashmap to support more versions
    // efficiently.
    for pair in self.status.utxo_map_versions.iter() {
      if (pair.0).0 as u64 == version {
        return Some(pair.1);
      }
    }

    None
  }

  fn get_global_hash(&self) -> (BitDigest, u64) {
    (self.status.global_hash, self.status.global_commit_count)
  }
}

pub mod helpers {
  use super::*;
  use crate::data_model::{Asset, ConfidentialMemo, DefineAssetBody, IssuerPublicKey, Memo};
  use zei::basic_crypto::signatures::{XfrKeyPair, XfrPublicKey, XfrSecretKey, XfrSignature};

  pub fn build_keys<R: CryptoRng + Rng>(prng: &mut R) -> (XfrPublicKey, XfrSecretKey) {
    let keypair = XfrKeyPair::generate(prng);

    (*keypair.get_pk_ref(), keypair.get_sk())
  }

  pub fn compute_signature<T>(secret_key: &XfrSecretKey,
                              public_key: &XfrPublicKey,
                              asset_body: &T)
                              -> XfrSignature
    where T: serde::Serialize
  {
    secret_key.sign(&serde_json::to_vec(&asset_body).unwrap(), &public_key)
  }

  pub fn asset_creation_body(token_code: &AssetTypeCode,
                             issuer_key: &XfrPublicKey,
                             updatable: bool,
                             traceable: bool,
                             memo: Option<Memo>,
                             confidential_memo: Option<ConfidentialMemo>)
                             -> DefineAssetBody {
    let mut token_properties: Asset = Default::default();
    token_properties.code = *token_code;
    token_properties.issuer = IssuerPublicKey { key: *issuer_key };
    token_properties.updatable = updatable;
    token_properties.traceable = traceable;

    if memo.is_some() {
      token_properties.memo = memo.unwrap();
    } else {
      token_properties.memo = Memo {};
    }

    if confidential_memo.is_some() {
      token_properties.confidential_memo = confidential_memo.unwrap();
    } else {
      token_properties.confidential_memo = ConfidentialMemo {};
    }

    DefineAssetBody { asset: token_properties }
  }

  pub fn asset_creation_operation(asset_body: &DefineAssetBody,
                                  public_key: &XfrPublicKey,
                                  secret_key: &XfrSecretKey)
                                  -> DefineAsset {
    let sign = compute_signature(&secret_key, &public_key, &asset_body);
    DefineAsset { body: asset_body.clone(),
                  pubkey: IssuerPublicKey { key: *public_key },
                  signature: sign }
  }
}

#[cfg(test)]
mod tests {
  use super::helpers::*;
  use super::*;
  use crate::data_model::{
    DefineAssetBody, IssueAssetBody, IssuerPublicKey, TransferAsset, TransferAssetBody,
  };
  use bulletproofs::PedersenGens;
  use curve25519_dalek::scalar::Scalar;
  use rand::SeedableRng;
  use std::fs;
  use std::io::BufWriter;
  use tempfile::{tempdir, tempfile};
  use zei::algebra::bls12_381::{BLSScalar, BLSG1};
  use zei::algebra::groups::Group;
  use zei::algebra::ristretto::RistPoint;
  use zei::basic_crypto::elgamal::{
    elgamal_derive_public_key, elgamal_generate_secret_key, ElGamalPublicKey,
  };
  use zei::basic_crypto::signatures::XfrKeyPair;
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record};
  use zei::xfr::structs::{
    AssetAmountProof, AssetIssuerPubKeys, AssetRecord, XfrBody, XfrNote, XfrProofs,
  };

  #[test]
  fn test_load_transaction_log() {
    // Verify that loading transaction fails with incorrect path
    let result_err = LedgerState::load_transaction_log("incorrect/path");
    assert!(result_err.is_err());

    // TODO(joe): replace/update this test

    //     // Create values to be used to instantiate operations
    //     let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

    //     let keypair = XfrKeyPair::generate(&mut prng);
    //     let message: &[u8] = b"test";

    //     let public_key = *keypair.get_pk_ref();
    //     let signature = keypair.sign(message);

    //     // Instantiate an IssueAsset operation
    //     let asset_issuance_body = IssueAssetBody { code: Default::default(),
    //                                                seq_num: 0,
    //                                                records: Vec::new() };

    //     let asset_issurance = IssueAsset { body: asset_issuance_body,
    //                                        pubkey: IssuerPublicKey { key: public_key },
    //                                        signature: signature.clone() };

    //     let issurance_operation = Operation::IssueAsset(asset_issurance);

    //     // Instantiate an DefineAsset operation
    //     let asset = Default::default();

    //     let asset_creation = DefineAsset { body: DefineAssetBody { asset },
    //                                        pubkey: IssuerPublicKey { key: public_key },
    //                                        signature: signature };

    //     let creation_operation = Operation::DefineAsset(asset_creation);

    //     // Verify that loading transaction succeeds with correct path
    //     let transaction_0: Transaction = Default::default();

    //     let transaction_1 = Transaction { operations: vec![issurance_operation.clone()],
    //                                       variable_utxos: Vec::new(),
    //                                       credentials: Vec::new(),
    //                                       memos: Vec::new(),
    //                                       tx_id: TxnSID { index: TXN_SEQ_ID_PLACEHOLDER as usize },
    //                                       merkle_id: TXN_SEQ_ID_PLACEHOLDER,
    //                                       outputs: 1 };

    //     let transaction_2 = Transaction { operations: vec![issurance_operation, creation_operation],
    //                                       variable_utxos: Vec::new(),
    //                                       credentials: Vec::new(),
    //                                       memos: Vec::new(),
    //                                       tx_id: TxnSID { index: TXN_SEQ_ID_PLACEHOLDER as usize },
    //                                       merkle_id: TXN_SEQ_ID_PLACEHOLDER,
    //                                       outputs: 2 };

    //     let tmp_dir = tempdir().unwrap();
    //     let buf = tmp_dir.path().join("test_transactions");
    //     let path = buf.to_str().unwrap();

    //     {
    //       let file = File::create(path).unwrap();
    //       let mut writer = BufWriter::new(file);

    //       bincode::serialize_into::<&mut BufWriter<File>, Transaction>(&mut writer, &transaction_0).unwrap();
    //       bincode::serialize_into::<&mut BufWriter<File>, Transaction>(&mut writer, &transaction_1).unwrap();
    //       bincode::serialize_into::<&mut BufWriter<File>, Transaction>(&mut writer, &transaction_2).unwrap();
    //     }

    //     let result_ok = LedgerState::load_transaction_log(&path);
    //     assert_eq!(result_ok.ok(),
    //                Some(vec![transaction_0, transaction_1, transaction_2]));

    //     tmp_dir.close().unwrap();
  }

  #[test]
  fn test_save_utxo_map_version() {
    let mut ledger_state = LedgerState::test_ledger();
    let digest = BitDigest { 0: [0_u8; 32] };
    ledger_state.status.utxo_map_versions = vec![(TxnSID(0), digest); MAX_VERSION - 1].into_iter()
                                                                                      .collect();

    // Verify that save_utxo_map_version increases the size of utxo_map_versions by 1 if its length < MAX_VERSION
    ledger_state.save_utxo_map_version();
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION);

    // Verify that save_utxo_map_version doesn't change the size of utxo_map_versions if its length >= MAX_VERSION
    ledger_state.status
                .utxo_map_versions
                .push_back((TxnSID(0), digest));
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION + 1);
    ledger_state.save_utxo_map_version();
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION + 1);

    // Verify that the element pushed to the back is as expected
    let back = ledger_state.status.utxo_map_versions.get(MAX_VERSION);
    assert_eq!(back,
               Some(&(ledger_state.status.next_txn, ledger_state.utxo_map.compute_checksum())));
  }

  #[test]
  fn test_save_global_hash() {
    let mut ledger_state = LedgerState::test_ledger();

    let data = GlobalHashData { bitmap: ledger_state.utxo_map.compute_checksum(),
                                merkle: ledger_state.merkle.get_root_hash(),
                                block: ledger_state.status.global_commit_count,
                                global_hash: ledger_state.status.global_hash };

    let count_original = ledger_state.status.global_commit_count;

    ledger_state.save_global_hash();

    assert_eq!(ledger_state.status.global_hash, sha256::hash(data.as_ref()));
    assert_eq!(ledger_state.status.global_commit_count, count_original + 1);
  }

  #[test]
  fn test_init_merkle_log() {
    let tmp_dir = tempdir().unwrap();
    let buf = tmp_dir.path().join("test_merkle");
    let path = buf.to_str().unwrap();

    // Verify that opening a non-existing Merkle tree fails
    let result_open_err = LedgerState::init_merkle_log(path, false);
    assert_eq!(result_open_err.err().unwrap().kind(),
               std::io::ErrorKind::NotFound);

    // Verify that creating a non-existing Merkle tree succeeds
    let result_create_ok = LedgerState::init_merkle_log(path, true);
    assert!(result_create_ok.is_ok());

    // Verify that opening an existing Merkle tree succeeds
    let result_open_ok = LedgerState::init_merkle_log(path, false);
    assert!(result_open_ok.is_ok());

    // Verify that creating an existing Merkle tree fails
    let result_create_err = LedgerState::init_merkle_log(path, true);
    assert_eq!(result_create_err.err().unwrap().kind(),
               std::io::ErrorKind::AlreadyExists);

    tmp_dir.close().unwrap();
  }

  #[test]
  fn test_init_utxo_map() {
    let tmp_dir = tempdir().unwrap();
    let buf = tmp_dir.path().join("test_init_bitmap");
    let path = buf.to_str().unwrap();

    // Verify that opening a non-existing bitmap fails
    let result_open_err = LedgerState::init_utxo_map(path, false);
    assert_eq!(result_open_err.err().unwrap().kind(),
               std::io::ErrorKind::NotFound);

    // Verify that creating a non-existing bitmap succeeds
    let result_create_ok = LedgerState::init_utxo_map(path, true);
    assert!(result_create_ok.is_ok());

    // Verify that creating an existing bitmap succeeds
    let result_open_ok = LedgerState::init_utxo_map(path, false);
    assert!(result_open_ok.is_ok());

    // Verify that opening an existing bitmap fails
    let result_create_err = LedgerState::init_utxo_map(path, true);
    assert_eq!(result_create_err.err().unwrap().kind(),
               std::io::ErrorKind::AlreadyExists);

    tmp_dir.close().unwrap();
  }

  #[test]
  fn test_snapshot() {
    let tmp_dir = tempdir().unwrap();
    let buf = tmp_dir.path().join("test_snapshot");
    let path = buf.to_str().unwrap();

    let mut ledger_state = LedgerState::test_ledger();
    ledger_state.status.merkle_path = path.to_string();
    let result = ledger_state.snapshot();

    // Verify that the SnapshotId is correct
    assert_eq!(result.ok().unwrap().id, 0);

    tmp_dir.close().unwrap();
  }

  #[test]
  fn test_checkpoint() {
    let mut ledger_state = LedgerState::test_ledger();

    let digest = BitDigest { 0: [0_u8; 32] };
    ledger_state.status.utxo_map_versions = vec![(TxnSID(0), digest); MAX_VERSION - 1].into_iter()
                                                                                      .collect();

    // Verify that checkpoint increases the size of utxo_map_versions by 1 if its length < MAX_VERSION
    ledger_state.checkpoint();
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION);

    let count_original = ledger_state.status.global_commit_count;
    let data = GlobalHashData { bitmap: ledger_state.utxo_map.compute_checksum(),
                                merkle: ledger_state.merkle.get_root_hash(),
                                block: count_original,
                                global_hash: ledger_state.status.global_hash };

    // Verify that end_commit doesn't change the size of utxo_map_versions if its length >= MAX_VERSION
    ledger_state.status
                .utxo_map_versions
                .push_back((TxnSID(0), digest));
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION + 1);
    ledger_state.checkpoint();
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION + 1);

    // Verify that the element pushed to the back is as expected
    let back = ledger_state.status.utxo_map_versions.get(MAX_VERSION);
    assert_eq!(back,
               Some(&(ledger_state.status.next_txn, ledger_state.utxo_map.compute_checksum())));

    // Verify that the global hash is saved as expected
    assert_eq!(ledger_state.status.global_hash, sha256::hash(data.as_ref()));
    assert_eq!(ledger_state.status.global_commit_count, count_original + 1);
  }

  // TODO(joe): should this be replaced?
  // #[test]
  // fn test_add_txo() {
  //   // Instantiate a BlindAssetRecord
  //   let mut prng = ChaChaRng::from_seed([0u8; 32]);
  //   let pc_gens = PedersenGens::default();

  //   let sk = elgamal_generate_secret_key::<_, Scalar>(&mut prng);
  //   let xfr_pub_key = elgamal_derive_public_key(&pc_gens.B, &sk);
  //   let elgamal_public_key = ElGamalPublicKey(RistPoint(xfr_pub_key.get_point()));

  //   let sk = elgamal_generate_secret_key::<_, BLSScalar>(&mut prng);
  //   let id_reveal_pub_key = elgamal_derive_public_key(&BLSG1::get_base(), &sk);

  //   let asset_issuer_pub_key = AssetIssuerPubKeys { eg_ristretto_pub_key:
  //                                                     elgamal_public_key.clone(),
  //                                                   eg_blsg1_pub_key: id_reveal_pub_key };

  //   let record = zei::xfr::structs::BlindAssetRecord { issuer_public_key:
  //                                                        Some(asset_issuer_pub_key),
  //                                                      issuer_lock_type: None,
  //                                                      issuer_lock_amount: None,
  //                                                      amount: None,
  //                                                      asset_type: None,
  //                                                      public_key: Default::default(),
  //                                                      amount_commitments: None,
  //                                                      asset_type_commitment: None,
  //                                                      blind_share: Default::default(),
  //                                                      lock: None };

  //   // Instantiate a transaction output
  //   let sid = TxoSID::default();
  //   let txo = (&sid, TxOutput(record));

  //   // Instantiate a LedgerState
  //   let mut ledger_state = LedgerState::test_ledger();
  //   ledger_state.add_txo(txo.clone());

  //   // Verify that add_txo sets values correctly
  //   let utxo_addr = TxoSID(0);

  //   assert_eq!(ledger_state.tracked_sids.get(&elgamal_public_key),
  //              Some(&vec![utxo_addr]));

  //   let utxo_ref = Utxo { digest: sha256::hash(&serde_json::to_vec(&txo.1).unwrap()).0,
  //                         output: txo.1 };
  //   assert_eq!(ledger_state.utxos.get(&utxo_addr).unwrap(), &utxo_ref);

  //   assert_eq!(ledger_state.max_applied_sid, utxo_addr)
  // }

  // TODO(joe): these tests with and without tracking should be fleshed out
  // #[test]
  // fn test_apply_asset_transfer_no_tracking() {
  //   // Instantiate an TransferAsset
  //   let xfr_note = XfrNote { body: XfrBody { inputs: Vec::new(),
  //                                            outputs: Vec::new(),
  //                                            proofs: XfrProofs { asset_amount_proof:
  //                                                                  AssetAmountProof::NoProof,
  //                                                                asset_tracking_proof:
  //                                                                  Default::default() } },
  //                            multisig: Default::default() };

  //   let assert_transfer_body =
  //     TransferAssetBody { inputs: vec![],
  //                         num_outputs: 0,
  //                         transfer: Box::new(xfr_note) };

  //   let asset_transfer = TransferAsset { body: assert_transfer_body,
  //                                        body_signatures: Vec::new() };

  //   // Instantiate a LedgerState
  //   let mut ledger_state = LedgerState::test_ledger();

  //   let map_file = tempfile().unwrap();

  //   let mut bitmap = BitMap::create(map_file).unwrap();
  //   bitmap.append().unwrap();

  //   ledger_state.utxo_map = bitmap;

  //   ledger_state.apply_asset_transfer(&asset_transfer);

  //   assert!(ledger_state.tracked_sids.is_empty());
  // }

  // #[test]
  // fn test_apply_asset_transfer_with_tracking() {
  //   // Instantiate a BlindAssetRecord
  //   let mut prng = ChaChaRng::from_seed([0u8; 32]);
  //   let pc_gens = PedersenGens::default();

  //   let sk = elgamal_generate_secret_key::<_, Scalar>(&mut prng);
  //   let xfr_pub_key = elgamal_derive_public_key(&pc_gens.B, &sk);
  //   let elgamal_public_key = ElGamalPublicKey(RistPoint(xfr_pub_key.get_point()));

  //   let sk = elgamal_generate_secret_key::<_, BLSScalar>(&mut prng);
  //   let id_reveal_pub_key = elgamal_derive_public_key(&BLSG1::get_base(), &sk);

  //   let asset_issuer_pub_key = AssetIssuerPubKeys { eg_ristretto_pub_key:
  //                                                     elgamal_public_key.clone(),
  //                                                   eg_blsg1_pub_key: id_reveal_pub_key };

  //   let record = zei::xfr::structs::BlindAssetRecord { issuer_public_key:
  //                                                        Some(asset_issuer_pub_key),
  //                                                      issuer_lock_type: None,
  //                                                      issuer_lock_amount: None,
  //                                                      amount: None,
  //                                                      asset_type: None,
  //                                                      public_key: Default::default(),
  //                                                      amount_commitments: None,
  //                                                      asset_type_commitment: None,
  //                                                      blind_share: Default::default(),
  //                                                      lock: None };

  //   // Instantiate an TransferAsset
  //   let xfr_note = XfrNote { body: XfrBody { inputs: Vec::new(),
  //                                            outputs: vec![record],
  //                                            proofs: XfrProofs { asset_amount_proof:
  //                                                                  AssetAmountProof::NoProof,
  //                                                                asset_tracking_proof:
  //                                                                  Default::default() } },
  //                            multisig: Default::default() };

  //   let assert_transfer_body =
  //     TransferAssetBody { inputs: vec![TxoSID { index: TXN_SEQ_ID_PLACEHOLDER }],
  //                         outputs: vec![TxoSID { index: TXN_SEQ_ID_PLACEHOLDER }],
  //                         transfer: Box::new(xfr_note) };

  //   let asset_transfer = TransferAsset { body: assert_transfer_body,
  //                                        body_signatures: Vec::new() };

  //   // Instantiate a LedgerState
  //   let mut ledger_state = LedgerState::test_ledger();

  //   let map_file = tempfile().unwrap();

  //   let mut bitmap = BitMap::create(map_file).unwrap();
  //   bitmap.append().unwrap();

  //   ledger_state.utxo_map = Some(bitmap);

  //   ledger_state.apply_asset_transfer(&asset_transfer);

  //   assert_eq!(ledger_state.tracked_sids.get(&elgamal_public_key),
  //              Some(&vec![TxoSID { index: 0 }]));
  // }

  // TODO(joe): apply_* functions are no longer relevant, so need to be
  // replaced with tests generating Transactions
  //
  // #[test]
  // fn test_apply_asset_issuance() {
  //   // Instantiate an IssueAsset
  //   let mut prng = ChaChaRng::from_seed([0u8; 32]);
  //   let keypair = XfrKeyPair::generate(&mut prng);
  //   let message: &[u8] = b"test";
  //   let public_key = *keypair.get_pk_ref();
  //   let signature = keypair.sign(message);

  //   let asset_issuance_body = IssueAssetBody { code: Default::default(),
  //                                              seq_num: 0,
  //                                              num_outputs: 0
  //                                              records: Vec::new() };

  //   let asset_issurance = IssueAsset { body: asset_issuance_body,
  //                                      pubkey: IssuerPublicKey { key: public_key },
  //                                      signature: signature };

  //   // Instantiate a LedgerState and apply the IssueAsset
  //   let mut ledger_state = LedgerState::test_ledger();
  //   ledger_state.apply_asset_issuance(&asset_issurance);

  //   // Verify that apply_asset_issuance correctly adds each txo to tracked_sids
  //   // TODO(joe): fix this
  //   // for output in asset_issurance.body
  //   //                              .outputs
  //   //                              .iter()
  //   //                              .zip(asset_issurance.body.records.iter().map(|ref o| (*o)))
  //   // {
  //   //   let record = &(output.0).0;
  //   //   match &output.1 {
  //   //     BlindAssetRecord(record) => {
  //   //       assert!(ledger_state.tracked_sids
  //   //                           .get(&record.issuer_public_key
  //   //                                       .as_ref()
  //   //                                       .unwrap()
  //   //                                       .eg_ristretto_pub_key)
  //   //                           .unwrap()
  //   //                           .contains(output.0));
  //   //     }
  //   //   }
  //   // }

  //   // Verify that issuance_num is correctly set
  //   assert_eq!(ledger_state.issuance_num.get(&asset_issurance.body.code),
  //              Some(&asset_issurance.body.seq_num));
  // }

  // #[test]
  // fn test_apply_asset_creation() {
  //   let mut ledger_state = LedgerState::test_ledger();

  //   let mut prng = ChaChaRng::from_seed([0u8; 32]);
  //   let keypair = XfrKeyPair::generate(&mut prng);
  //   let message: &[u8] = b"test";
  //   let public_key = *keypair.get_pk_ref();
  //   let signature = keypair.sign(message);

  //   let asset_creation = DefineAsset { body: DefineAssetBody { asset: Default::default() },
  //                                      pubkey: IssuerPublicKey { key: public_key },
  //                                      signature: signature };

  //   let token = AssetType { properties: asset_creation.body.asset.clone(),
  //                            ..Default::default() };

  //   ledger_state.apply_asset_creation(&asset_creation);

  //   assert_eq!(ledger_state.asset_types.get(&token.properties.code),
  //              Some(&token));
  // }

  // #[test]
  // fn test_apply_operation() {
  //   // Create values to be used to instantiate operations
  //   let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

  //   let keypair = XfrKeyPair::generate(&mut prng);
  //   let message: &[u8] = b"test";

  //   let public_key = *keypair.get_pk_ref();
  //   let signature = keypair.sign(message);

  //   // Instantiate an TransferAsset operation
  //   let xfr_note = XfrNote { body: XfrBody { inputs: Vec::new(),
  //                                            outputs: Vec::new(),
  //                                            proofs: XfrProofs { asset_amount_proof:
  //                                                                  AssetAmountProof::NoProof,
  //                                                                asset_tracking_proof:
  //                                                                  Default::default() } },
  //                            multisig: Default::default() };

  //   let assert_transfer_body = TransferAssetBody { inputs: Vec::new(),
  //                                                  outputs: Vec::new(),
  //                                                  transfer: Box::new(xfr_note) };

  //   let asset_transfer = TransferAsset { body: assert_transfer_body,
  //                                        body_signatures: Vec::new() };

  //   let transfer_operation = Operation::TransferAsset(asset_transfer.clone());

  //   // Instantiate an IssueAsset operation
  //   let asset_issuance_body = IssueAssetBody { code: Default::default(),
  //                                              seq_num: 0,
  //                                              outputs: Vec::new(),
  //                                              records: Vec::new() };

  //   let asset_issurance = IssueAsset { body: asset_issuance_body,
  //                                      pubkey: IssuerPublicKey { key: public_key },
  //                                      signature: signature.clone() };

  //   let issurance_operation = Operation::IssueAsset(asset_issurance.clone());

  //   // Instantiate an DefineAsset operation
  //   let asset = Default::default();

  //   let asset_creation = DefineAsset { body: DefineAssetBody { asset },
  //                                      pubkey: IssuerPublicKey { key: public_key },
  //                                      signature: signature };

  //   let creation_operation = Operation::DefineAsset(asset_creation.clone());

  //   // Test apply_operation
  //   let mut ledger_state = LedgerState::test_ledger();

  //   assert_eq!(ledger_state.apply_operation(&transfer_operation),
  //              ledger_state.apply_asset_transfer(&asset_transfer));
  //   assert_eq!(ledger_state.apply_operation(&issurance_operation),
  //              ledger_state.apply_asset_issuance(&asset_issurance));
  //   assert_eq!(ledger_state.apply_operation(&creation_operation),
  //              ledger_state.apply_asset_creation(&asset_creation));
  // }

  #[test]
  fn test_create_merkle_log() {
    let tmp_dir = tempdir().unwrap();
    let buf = tmp_dir.path().join("merkle_log");
    let base_path = buf.to_str().unwrap();

    let result = LedgerState::create_merkle_log(base_path.to_string(), 0);
    assert!(result.is_ok());

    let path = base_path.to_owned() + "-log-0";
    assert!(fs::metadata(path).is_ok());

    tmp_dir.close().unwrap();
  }

  // TODO (Keyao): Add unit tests for
  //   TxnContext::new
  //   TxnContext::apply_operation
  //   LedgerAccess for TxnContext
  //     LedgerAccess::check_utxo
  //     LedgerAccess::get_asset_token
  //     LedgerAccess::get_asset_policy
  //     LedgerAccess::get_smart_contract
  //     LedgerAccess::get_issuance_num
  //     LedgerAccess::get_tracked_sids
  //   LedgerUpdate for LedgerState
  //     LedgerUpdate::apply_transaction
  //   ArchiveUpdate for LedgerState
  //     ArchiveUpdate::append_transaction
  //   LedgerAccess for LedgerState
  //     LedgerAccess::check_utxo
  //     LedgerAccess::get_asset_token
  //     LedgerAccess::get_asset_policy
  //     LedgerAccess::get_smart_contract
  //     LedgerAccess::get_issuance_num
  //     LedgerAccess::get_tracked_sids
  //   ArchiveAccess for LedgerState
  //     ArchiveAccess::get_transaction
  //     ArchiveAccess::get_proof
  //     ArchiveAccess::get_utxo_map
  //     ArchiveAccess::get_utxos
  //     ArchiveAccess::get_utxo_checksum
  //     ArchiveAccess::get_global_hash

  #[test]
  fn test_asset_creation_valid() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::test_ledger();
    let mut tx = Transaction::default();

    let token_code1 = AssetTypeCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::DefineAsset(asset_create));

    let effect = TxnEffect::compute_effect(&mut prng, tx).unwrap();
    {
      let mut block = state.start_block().unwrap();
      state.apply_transaction(&mut block, effect).unwrap();
      state.finish_block(block);
    }

    assert!(state.get_asset_type(&token_code1).is_some());

    assert_eq!(asset_body.asset,
               state.get_asset_type(&token_code1).unwrap().properties);

    assert_eq!(0, state.get_asset_type(&token_code1).unwrap().units);
  }

  // Change the signature to have the wrong public key
  #[test]
  fn test_asset_creation_invalid_public_key() {
    // Create a valid asset creation operation
    let mut state = LedgerState::test_ledger();
    let mut tx = Transaction::default();
    let token_code1 = AssetTypeCode { val: [1; 16] };
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let (public_key1, secret_key1) = build_keys(&mut prng);
    let asset_body = asset_creation_body(&token_code1, &public_key1, true, false, None, None);
    let mut asset_create = asset_creation_operation(&asset_body, &public_key1, &secret_key1);

    // Now re-sign the operation with the wrong key.
    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let (public_key2, _secret_key2) = build_keys(&mut prng);

    asset_create.pubkey.key = public_key2;
    tx.operations.push(Operation::DefineAsset(asset_create));

    assert!(TxnEffect::compute_effect(&mut prng, tx).is_err());
  }

  // Sign with the wrong key.
  #[test]
  fn test_asset_creation_invalid_signature() {
    // Create a valid operation.
    let mut state = LedgerState::test_ledger();
    let mut tx = Transaction::default();
    let token_code1 = AssetTypeCode { val: [1; 16] };

    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let (public_key1, secret_key1) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key1, true, false, None, None);
    let mut asset_create = asset_creation_operation(&asset_body, &public_key1, &secret_key1);

    // Re-sign the operation with the wrong key.
    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let (public_key2, _secret_key2) = build_keys(&mut prng);

    asset_create.pubkey.key = public_key2;
    tx.operations.push(Operation::DefineAsset(asset_create));

    assert!(TxnEffect::compute_effect(&mut prng, tx).is_err());
  }

  #[test]
  fn asset_issued() {
    let tmp_dir = TempDir::new("test").unwrap();
    let merkle_buf = tmp_dir.path().join("test_merkle");
    let merkle_path = merkle_buf.to_str().unwrap();
    let txn_buf = tmp_dir.path().join("test_txnlog");
    let txn_path = txn_buf.to_str().unwrap();
    let ledger_buf = tmp_dir.path().join("test_ledger");
    let ledger_path = ledger_buf.to_str().unwrap();
    let utxo_map_buf = tmp_dir.path().join("test_utxo_map");
    let utxo_map_path = utxo_map_buf.to_str().unwrap();

    let mut ledger = LedgerState::new(&merkle_path, &txn_path, &utxo_map_path, None, true).unwrap();

    assert!(ledger.get_global_hash() == (BitDigest { 0: [0_u8; 32] }, 0));
    let mut tx = Transaction::default();
    let token_code1 = AssetTypeCode { val: [1; 16] };
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::DefineAsset(asset_create));

    let effect = TxnEffect::compute_effect(&mut prng, tx).unwrap();
    {
      let mut block = ledger.start_block().unwrap();
      ledger.apply_transaction(&mut block, effect).unwrap();
      ledger.finish_block(block);
    }

    let mut tx = Transaction::default();

    let ar = AssetRecord::new(100, token_code1.val, public_key).unwrap();
    let params = PublicParams::new();
    let ba = build_blind_asset_record(&mut prng, &params.pc_gens, &ar, false, false, &None);

    let asset_issuance_body = IssueAssetBody::new(&token_code1, 0, &[TxOutput(ba)]).unwrap();

    let sign = compute_signature(&secret_key, &public_key, &asset_issuance_body);

    let asset_issuance_operation = IssueAsset { body: asset_issuance_body,
                                                pubkey: IssuerPublicKey { key:
                                                                            public_key.clone() },
                                                signature: sign };

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    tx.operations.push(issue_op);
    let effect = TxnEffect::compute_effect(&mut prng, tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (txn_sid, txos) = ledger.finish_block(block).remove(&temp_sid).unwrap();

    let sid = txn_sid;
    let transaction = ledger.txs[txn_sid.0].clone();
    let txn_id = transaction.tx_id;

    println!("utxos = {:?}", ledger.status.utxos);
    // TODO assert!(ledger.utxos.contains_key(&sid));

    match ledger.get_proof(txn_id) {
      Some(proof) => {
        assert!(proof.tx_id == ledger.txs[txn_id.0].merkle_id);
      }
      None => {
        panic!("get_proof failed for tx_id {}, merkle_id {}, state {}",
               transaction.tx_id.0,
               transaction.merkle_id,
               ledger.merkle.state());
      }
    }

    // We don't actually have anything to commmit yet,
    // but this will save the empty checksum, which is
    // enough for a bit of a test.
    assert!(ledger.get_global_hash() == (ledger.status.global_hash, 2));
    let query_result = ledger.get_utxo_checksum(ledger.status.next_txn.0 as u64)
                             .unwrap();
    let compute_result = ledger.utxo_map.compute_checksum();
    println!("query_result = {:?}, compute_result = {:?}",
             query_result, compute_result);

    assert!(query_result == compute_result);

    match ledger.snapshot() {
      Ok(n) => {
        assert!(n.id == 2);
      }
      Err(x) => {
        panic!("snapshot failed:  {}", x);
      }
    }
  }
}
