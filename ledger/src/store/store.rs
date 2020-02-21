#![deny(warnings)]
extern crate bincode;
extern crate byteorder;
extern crate findora;
extern crate tempdir;

use crate::data_model::errors::PlatformError;
use crate::data_model::*;
use crate::policies::{calculate_fee, DebtMemo};
use crate::policy_script::policy_check_txn;
use bitmap;
use bitmap::BitMap;
use cryptohash::sha256;
use cryptohash::sha256::Digest as BitDigest;
use findora::HasInvariants;
use merkle_tree::append_only_merkle::{AppendOnlyMerkle, HashValue, Proof};
use merkle_tree::logged_merkle::LoggedMerkle;
use rand_chacha::ChaChaRng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::fs::OpenOptions;
use std::io::BufReader;
use std::path::Path;
use std::u64;

use super::effects::*;

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

pub trait LedgerUpdate<RNG: RngCore + CryptoRng> {
  // Each Block represents a collection of transactions which have been
  // validated and confirmed to be unconditionally consistent with the
  // ledger and with each other.
  type Block: Sync + Send;

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
  //   On i/o failure: Err(...)
  //   Otherwise: Map from temporary IDs to the finalized Transaction SID
  //     and the finalized TXO SIDs of that transaction's UTXOs. UTXO SIDs
  //     for each transaction will be in increasing order.
  //
  // When Err(...) is returned, no modifications are made to the ledger.
  fn finish_block(&mut self,
                  block: Self::Block)
                  -> Result<HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>, std::io::Error>;
}

pub trait ArchiveAccess {
  // Number of blocks committed
  fn get_block_count(&self) -> usize;
  // Number of transactions available
  fn get_transaction_count(&self) -> usize;
  // Look up transaction in the log
  fn get_transaction(&self, addr: TxnSID) -> Option<&FinalizedTransaction>;
  // Look up block in the log
  fn get_block(&self, addr: BlockSID) -> Option<&Vec<FinalizedTransaction>>;
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
  fn get_global_block_hash(&self) -> (BitDigest, u64);
}

#[repr(C)]
#[derive(Serialize)]
// TODO (Keyao):
// Are the four fields below all necessary?
// Can we remove one of txns_in_block_hash and global_block_hash?
// Both of them contain the information of the previous state
pub struct BlockHashData {
  pub bitmap: BitDigest,             // The checksum of the utxo_map
  pub block_merkle: HashValue,       // The root hash of the block Merkle tree
  pub txns_in_block_hash: BitDigest, // The hash of the transactions in the block
  pub global_block_hash: BitDigest,  // The prior global block hash
}

const MAX_VERSION: usize = 100;

// Parts of the current ledger state which can be restored from a snapshot
// without replaying a log
#[derive(Deserialize, Serialize, PartialEq)]
pub struct LedgerStatus {
  // Paths to archival logs for the merkle tree and transaction history
  block_merkle_path: String,
  txn_merkle_path: String,
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
  global_block_hash: BitDigest,
  block_commit_count: u64, // TODO (Keyao): Remove this if not needed

  // Hash of the transactions in the most recent block
  txns_in_block_hash: BitDigest,
}

pub struct LedgerState {
  status: LedgerStatus,

  // PRNG used for transaction validation
  prng: ChaChaRng,

  // Merkle tree tracking the sequence of transaction hashes in the block
  // Each appended hash is the hash of transactions in the same block
  block_merkle: LoggedMerkle,

  // Merkle tree tracking the sequence of all transaction hashes
  // Each appended hash is the hash of a transaction
  txn_merkle: LoggedMerkle,

  // The `FinalizedTransaction`s consist of a Transaction and an index into
  // `merkle` representing its hash.
  // TODO(joe): should this be in-memory?
  blocks: Vec<Vec<FinalizedTransaction>>,

  // Bitmap tracking all the live TXOs
  utxo_map: BitMap,

  // TODO(joe): use this file handle to actually record transactions
  #[allow(unused)]
  txn_log: Option<File>,

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
    if let Some(txn_log_fd) = &self.txn_log {
      txn_log_fd.sync_data().unwrap();
      let tmp_dir = {
        let base_dir = std::env::temp_dir();
        let base_dirname = "findora_ledger";
        let mut i = 0;
        let mut dirname = None;
        while dirname.is_none() {
          let name = std::format!("{}_{}", base_dirname, i);
          let path = base_dir.join(name);
          match std::fs::create_dir(&path) {
            Ok(()) => {
              dirname = Some(path);
            }
            Err(_) => {
              i += 1;
            }
          }
        }
        dirname.unwrap()
      };
      let block_merkle_buf = tmp_dir.join("test_block_merkle");
      let other_block_merkle_path = block_merkle_buf.to_str().unwrap();

      let txn_merkle_buf = tmp_dir.join("test_txn_merkle");
      let other_txn_merkle_path = txn_merkle_buf.to_str().unwrap();

      let txn_buf = tmp_dir.join("test_txnlog");
      let other_txn_path = txn_buf.to_str().unwrap();

      let utxo_map_buf = tmp_dir.join("test_utxo_map");
      let other_utxo_map_path = utxo_map_buf.to_str().unwrap();

      dbg!(&self.status.txn_path);
      dbg!(std::fs::metadata(&self.status.txn_path).unwrap());
      dbg!(&other_txn_path);
      std::fs::copy(&self.status.txn_path, &other_txn_path).unwrap();

      let state2 = Box::new(LedgerState::load_from_log(&other_block_merkle_path,
                                                       &other_txn_merkle_path,
                                                       &other_txn_path,
                                                       &other_utxo_map_path,
                                                       None).unwrap());

      let mut status2 = Box::new(state2.status);
      status2.block_merkle_path = self.status.block_merkle_path.clone();
      status2.txn_merkle_path = self.status.txn_merkle_path.clone();
      status2.txn_path = self.status.txn_path.clone();
      status2.utxo_map_path = self.status.utxo_map_path.clone();

      assert!(*status2 == self.status);

      std::fs::remove_dir_all(tmp_dir).unwrap();
    }
    Ok(())
  }
}

impl LedgerStatus {
  pub fn new(block_merkle_path: &str,
             txn_merkle_path: &str,
             txn_path: &str,
             // TODO(joe): should this do something?
             // snapshot_path: &str,
             utxo_map_path: &str)
             -> Result<LedgerStatus, std::io::Error> {
    let ledger = LedgerStatus { block_merkle_path: block_merkle_path.to_owned(),
                                txn_merkle_path: txn_merkle_path.to_owned(),
                                txn_path: txn_path.to_owned(),
                                utxo_map_path: utxo_map_path.to_owned(),
                                utxos: HashMap::new(),
                                utxo_map_versions: VecDeque::new(),
                                asset_types: HashMap::new(),
                                issuance_num: HashMap::new(),
                                next_txn: TxnSID(0),
                                next_txo: TxoSID(0),
                                txns_in_block_hash: BitDigest { 0: [0_u8; 32] },
                                global_block_hash: BitDigest { 0: [0_u8; 32] },
                                block_commit_count: 0 };

    Ok(ledger)
  }

  #[cfg(feature = "TESTING")]
  #[allow(non_snake_case)]
  pub fn TESTING_check_txn_effects(&self, txn: TxnEffect) -> Result<TxnEffect, PlatformError> {
    self.check_txn_effects(txn)
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
    for (code, _asset_type) in txn.new_asset_codes.iter() {
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
      let asset_type = self.asset_types
                           .get(&code)
                           .or_else(|| txn.new_asset_codes.get(&code))
                           .ok_or(PlatformError::InputsError)?;
      let proper_key = asset_type.properties.issuer;
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
        let curr_seq_num_limit = self.issuance_num
                                     .get(&code)
                                     // If a transaction defines and then issues, it should pass.
                                     // However, if there is a bug elsewhere in validation, panicking
                                     // is better than allowing incorrect issuances to pass through.
                                     .or_else(|| {
                                       assert!(txn.new_asset_codes.contains_key(&code));
                                       Some(&0)
                                     })
                                     .unwrap();
        let min_seq_num = seq_nums.first().unwrap();
        if min_seq_num < curr_seq_num_limit {
          return Err(PlatformError::InputsError);
        }
      }
    }

    // Debt swaps
    // (1) Fiat code must match debt asset memo
    // (2) fee must be correct
    for (code, debt_swap_effects) in txn.debt_effects.iter() {
      let debt_type = &self.asset_types
                           .get(&code)
                           .or_else(|| txn.new_asset_codes.get(&code))
                           .ok_or(PlatformError::InputsError)?
                           .properties;

      let debt_memo = serde_json::from_str::<DebtMemo>(&debt_type.memo.0)?;
      let correct_fee = calculate_fee(debt_swap_effects.initial_balance, debt_memo.interest_rate);

      // (1), (2)
      if debt_swap_effects.fiat_code != debt_memo.fiat_code
         || debt_swap_effects.fiat_paid != debt_swap_effects.debt_burned + correct_fee
      {
        return Err(PlatformError::InputsError);
      }
    }

    // Policy checking
    // TODO(joe): Currently the policy language can't validate transactions
    //   which include DefineAsset, so it's safe to assume that any valid
    //   policy usage involves an asset whose definition is already in the
    //   committed state. However, it may not always be that way, and this
    //   code will lead to erroneous validation failures when that change
    //   arrives.
    for code in txn.asset_types_involved.iter() {
      let asset = self.asset_types
                      .get(code)
                      .ok_or(PlatformError::InputsError)?;
      if let Some((ref pol, ref globals)) = asset.properties.policy {
        let globals = globals.clone();
        policy_check_txn(code, globals, &pol, &txn.txn)?;
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
  #[allow(clippy::cognitive_complexity)]
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
      None => Err(PlatformError::InputsError),
      // Probably should be a more relevant error
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

    ret
  }

  #[allow(clippy::cognitive_complexity)]
  fn finish_block(&mut self,
                  block: BlockEffect)
                  -> Result<HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>, std::io::Error> {
    let mut block = block;

    let base_sid = self.status.next_txo.0;
    let txn_temp_sids = block.temp_sids.clone();

    if let Some(txn_log_fd) = &self.txn_log {
      bincode::serialize_into(txn_log_fd, &block.txns)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other,e))?;
      txn_log_fd.sync_data()?;
    }

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

    {
      let mut tx_block = Vec::new();
      // Update the transaction Merkle tree and transaction log
      for (tmp_sid, txn) in block.temp_sids.drain(..).zip(block.txns.drain(..)) {
        let txn_sid = temp_sid_map.get(&tmp_sid).unwrap().0;

        let digest = sha256::hash(&txn.serialize_bincode(txn_sid));
        let mut hash = HashValue::new();
        hash.hash.clone_from_slice(&digest.0);

        // TODO(joe/jonathan): Since errors in the merkle tree are things like
        // corruption and I/O failure, we don't have a good recovery story. Is
        // panicking acceptable?
        let merkle_id = self.txn_merkle.append(&hash).unwrap();

        tx_block.push(FinalizedTransaction { txn,
                                             tx_id: txn_sid,
                                             merkle_id });
      }
      self.blocks.push(tx_block);
    }

    // Compute hash against history
    self.checkpoint(&block);

    // TODO(joe): asset tracing?

    debug_assert!(block.temp_sids.is_empty());
    debug_assert!(block.txns.is_empty());
    debug_assert!(block.txos.is_empty());
    debug_assert!(block.input_txos.is_empty());
    debug_assert!(block.new_asset_codes.is_empty());
    debug_assert!(block.new_issuance_nums.is_empty());

    self.block_ctx = Some(block);

    Ok(temp_sid_map)
  }
}

impl LedgerState {
  #[cfg(feature = "TESTING")]
  #[allow(non_snake_case)]
  pub fn TESTING_get_status(&self) -> &LedgerStatus {
    &self.status
  }

  // Create a ledger for use by a unit test.
  pub fn test_ledger() -> LedgerState {
    let tmp_dir = {
      let base_dir = std::env::temp_dir();
      let base_dirname = "findora_ledger";
      let mut i = 0;
      let mut dirname = None;
      while dirname.is_none() {
        let name = std::format!("{}_{}", base_dirname, i);
        let path = base_dir.join(name);
        match std::fs::create_dir(&path) {
          Ok(()) => {
            dirname = Some(path);
          }
          Err(_) => {
            i += 1;
          }
        }
      }
      dirname.unwrap()
    };

    let block_merkle_buf = tmp_dir.join("test_block_merkle");
    let block_merkle_path = block_merkle_buf.to_str().unwrap();

    let txn_merkle_buf = tmp_dir.join("test_txn_merkle");
    let txn_merkle_path = txn_merkle_buf.to_str().unwrap();

    let txn_buf = tmp_dir.join("test_txnlog");
    let txn_path = txn_buf.to_str().unwrap();

    // let snap_buf      = tmp_dir.join("test_ledger_snap");
    // let snap_path     = snap_buf.to_str().unwrap();

    let utxo_map_buf = tmp_dir.join("test_utxo_map");
    let utxo_map_path = utxo_map_buf.to_str().unwrap();

    LedgerState::new(&block_merkle_path,
                     &txn_merkle_path,
                     &txn_path,
                     &utxo_map_path,
                     None).unwrap()
  }

  // TODO(joe): Make this an iterator of some sort so that we don't have to load the whole log
  // into memory
  fn load_transaction_log(path: &str) -> Result<Vec<Vec<Transaction>>, std::io::Error> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut v = Vec::new();
    while let Ok(next_block) =
      bincode::deserialize_from::<&mut BufReader<File>, Vec<Transaction>>(&mut reader)
    {
      v.push(next_block);
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

  // In this functionn:
  //  1. Compute the hash of transactions in the block and update txns_in_block_hash
  //  2. Append txns_in_block_hash to block_merkle
  fn compute_and_append_txns_hash(&mut self, block: &BlockEffect) {
    // 1. Compute the hash of transactions in the block and update txns_in_block_hash
    self.status.txns_in_block_hash = block.compute_txns_in_block_hash();

    // 2. Append txns_in_block_hash to block_merkle
    //  2.1 Convert txns_in_block_hash from BitDigest to HashValue
    let mut txns_in_block_hash = HashValue::new();
    txns_in_block_hash.hash
                      .clone_from_slice(&self.status.txns_in_block_hash.0);

    //  2.2 Update the block Merkle tree
    self.block_merkle.append(&txns_in_block_hash).unwrap();
  }

  fn compute_and_save_block_hash(&mut self) {
    let data = BlockHashData { bitmap: self.utxo_map.compute_checksum(),
                               block_merkle: self.block_merkle.get_root_hash(),
                               txns_in_block_hash: self.status.txns_in_block_hash,
                               global_block_hash: self.status.global_block_hash };

    let serialized = bincode::serialize(&data).unwrap();

    self.status.global_block_hash = sha256::hash(&serialized);
    self.status.block_commit_count += 1;
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
  pub fn new(block_merkle_path: &str,
             txn_merkle_path: &str,
             txn_path: &str,
             utxo_map_path: &str,
             prng_seed: Option<[u8; 32]>)
             -> Result<LedgerState, std::io::Error> {
    let ledger =
      LedgerState { status: LedgerStatus::new(block_merkle_path,
                                              txn_merkle_path,
                                              txn_path,
                                              utxo_map_path)?,
                    // TODO(joe): is this safe?
                    prng: rand_chacha::ChaChaRng::from_seed(prng_seed.unwrap_or([0u8; 32])),
                    block_merkle: LedgerState::init_merkle_log(block_merkle_path, true)?,
                    txn_merkle: LedgerState::init_merkle_log(txn_merkle_path, true)?,
                    blocks: Vec::new(),
                    utxo_map: LedgerState::init_utxo_map(utxo_map_path, true)?,
                    txn_log: Some(std::fs::OpenOptions::new().create_new(true)
                                                             .append(true)
                                                             .open(txn_path)?),
                    block_ctx: Some(BlockEffect::new()) };

    ledger.txn_log.as_ref().unwrap().sync_all()?;

    Ok(ledger)
  }

  pub fn load_from_log(block_merkle_path: &str,
                       txn_merkle_path: &str,
                       txn_path: &str,
                       utxo_map_path: &str,
                       prng_seed: Option<[u8; 32]>)
                       -> Result<LedgerState, std::io::Error> {
    let blocks = LedgerState::load_transaction_log(txn_path)?;
    let txn_log = std::fs::OpenOptions::new().append(true).open(txn_path)?;
    let mut ledger =
      LedgerState { status: LedgerStatus::new(block_merkle_path,
                                              txn_merkle_path,
                                              txn_path,
                                              utxo_map_path)?,
                    // TODO(joe): is this safe?
                    prng: rand_chacha::ChaChaRng::from_seed(prng_seed.unwrap_or([0u8; 32])),
                    block_merkle: LedgerState::init_merkle_log(block_merkle_path, true)?,
                    txn_merkle: LedgerState::init_merkle_log(txn_merkle_path, true)?,
                    blocks: Vec::new(),
                    utxo_map: LedgerState::init_utxo_map(utxo_map_path, true)?,
                    txn_log: None,
                    block_ctx: Some(BlockEffect::new()) };

    for block in blocks {
      let mut block_builder = ledger.start_block().unwrap();
      for txn in block {
        let eff = TxnEffect::compute_effect(ledger.get_prng(), txn)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other,e))?;
        ledger.apply_transaction(&mut block_builder, eff)
              .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
      }
      ledger.finish_block(block_builder).unwrap();
    }

    ledger.txn_log = Some(txn_log);

    Ok(ledger)
  }

  pub fn load_or_init(base_dir: &Path) -> Result<LedgerState, std::io::Error> {
    let block_merkle = base_dir.join("block_merkle");
    let block_merkle = block_merkle.to_str().unwrap();
    let txn_merkle = base_dir.join("txn_merkle");
    let txn_merkle = txn_merkle.to_str().unwrap();
    let txn_log = base_dir.join("txn_log");
    let txn_log = txn_log.to_str().unwrap();
    let utxo_map = base_dir.join("utxo_map");
    let utxo_map = utxo_map.to_str().unwrap();

    // TODO(joe): distinguish between the transaction log not existing
    // and it being corrupted
    LedgerState::load_from_log(&block_merkle, &txn_merkle, &txn_log,
                &utxo_map, None)
              .or_else(|_| LedgerState::new(&block_merkle, &txn_merkle, &txn_log,
                &utxo_map, None))
  }

  // Load a ledger given the paths to the various storage elements.
  #[allow(unused_variables)]
  pub fn load_from_snapshot(block_merkle_path: &str,
                            merkle_path: &str,
                            txn_path: &str,
                            utxo_map_path: &str,
                            prng_seed: Option<[u8; 32]>,
                            snapshot_path: &str)
                            -> Result<LedgerState, std::io::Error> {
    unimplemented!();

    // let block_merkle = LedgerState::init_merkle_log(block_merkle_path, false)?;
    // let txn_merkle = LedgerState::init_merkle_log(merkle_path, false)?;
    // let utxo_map = LedgerState::init_utxo_map(utxo_map_path, false)?;
    // let txs = LedgerState::load_transaction_log(txn_path)?;
    // let ledger_file = File::open(snapshot_path)?;
    // let status      = bincode::deserialize_from
    //                          ::<BufReader<File>, LedgerStatus>(
    //                               BufReader::new(ledger_file)
    //                          ).map_err(|e|
    //                             std::io::Error::new(
    //                               std::io::ErrorKind::Other, e)
    //                          )?;
    // let txn_log = OpenOptions::new().append(true).open(txn_path)?;

    // // TODO(joe): thoughts about write-ahead transaction log so that
    // // recovery can happen between snapshots.
    // // for txn in &txs[ledger.txn_count..] {
    // //   ledger.apply_transaction(&txn);
    // // }

    // let prng =
    //     // TODO(joe): is this safe?
    //     rand_chacha::ChaChaRng::from_seed(prng_seed.unwrap_or([0u8;32]));

    // let ledger = LedgerState { status,
    //                            prng,
    //                            block_merkle,
    //                            txn_merkle,
    //                            txs,
    //                            utxo_map,
    //                            txn_log,
    //                            block_ctx: Some(BlockEffect::new()) };
    // assert!(ledger.txs.len() == ledger.status.next_txn.0);
    // Ok(ledger)
  }

  // Snapshot the block ledger state
  pub fn snapshot_block(&mut self) -> Result<SnapshotId, std::io::Error> {
    let state = self.block_merkle.state();
    let writer = LedgerState::create_merkle_log(self.status.block_merkle_path.clone(), state)?;
    self.block_merkle.snapshot(writer)?;

    Ok(SnapshotId { id: state })
  }

  // Snapshot the ledger state.  This involves synchronizing
  // the durable data structures to the disk and starting a
  // new log file for the logged Merkle tree.
  //
  // TODO(joe): Actually serialize the active ledger state.
  pub fn snapshot_txns(&mut self) -> Result<SnapshotId, std::io::Error> {
    let state = self.txn_merkle.state();
    let writer = LedgerState::create_merkle_log(self.status.txn_merkle_path.clone(), state)?;
    self.txn_merkle.snapshot(writer)?;

    Ok(SnapshotId { id: state })
  }

  // pub fn begin_commit(&mut self) {
  //   self.txn_base_sid.0 = self.max_applied_sid.0 + 1;
  // }

  pub fn checkpoint(&mut self, block: &BlockEffect) {
    self.save_utxo_map_version();
    self.compute_and_append_txns_hash(&block);
    self.compute_and_save_block_hash();
  }

  // Create a file structure for a Merkle tree log.
  // Mostly just make a path of the form:
  //
  //     <tree_path>-log-<Merkle tree state>
  //
  fn create_merkle_log(base_path: String, next_id: u64) -> Result<File, std::io::Error> {
    let log_path = base_path + "-log-" + &next_id.to_string();
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
    self.issuance_num.get(code).copied()
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
    let mut ix: usize = addr.0;
    for b in self.blocks.iter() {
      match b.get(ix) {
        None => {
          debug_assert!(ix >= b.len());
          ix -= b.len();
        }
        v => return v,
      }
    }
    None
  }
  fn get_block(&self, addr: BlockSID) -> Option<&Vec<FinalizedTransaction>> {
    self.blocks.get(addr.0)
  }

  fn get_proof(&self, addr: TxnSID) -> Option<Proof> {
    match self.get_transaction(addr) {
      None => None,
      Some(txn) => {
        let merkle = &self.txn_merkle;
        // TODO log error and recover?
        Some(merkle.get_proof(txn.merkle_id, 0).unwrap())
      }
    }
  }

  fn get_block_count(&self) -> usize {
    self.blocks.len()
  }
  fn get_transaction_count(&self) -> usize {
    self.status.next_txn.0
  }
  fn get_utxo_map(&self) -> &BitMap {
    &self.utxo_map
  }
  fn serialize_utxo_map(&mut self) -> Vec<u8> {
    self.utxo_map.serialize(self.get_transaction_count())
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

  fn get_global_block_hash(&self) -> (BitDigest, u64) {
    (self.status.global_block_hash, self.status.block_commit_count)
  }
}

pub mod helpers {
  use super::*;
  use crate::data_model::{
    Asset, ConfidentialMemo, DefineAsset, DefineAssetBody, IssuerPublicKey, Memo,
  };
  use zei::serialization::ZeiFromToBytes;
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::AssetRecordType;
  use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record};
  use zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey};
  use zei::xfr::structs::AssetRecord;

  pub fn create_definition_transaction(code: &AssetTypeCode,
                                       public_key: &XfrPublicKey,
                                       secret_key: &XfrSecretKey,
                                       memo: Option<Memo>)
                                       -> Result<Transaction, PlatformError> {
    let issuer_key = IssuerPublicKey { key: *public_key };
    let mut tx = Transaction::default();
    let asset_body = DefineAssetBody::new(&code, &issuer_key, false, false, memo, None, None)?;
    let asset_create = DefineAsset::new(asset_body, &issuer_key, &secret_key)?;
    tx.operations.push(Operation::DefineAsset(asset_create));
    Ok(tx)
  }

  pub fn build_keys<R: CryptoRng + RngCore>(prng: &mut R) -> (XfrPublicKey, XfrSecretKey) {
    let keypair = XfrKeyPair::generate(prng);

    (*keypair.get_pk_ref(), keypair.get_sk())
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

    if let Some(memo) = memo {
      token_properties.memo = memo;
    } else {
      token_properties.memo = Memo(String::from(""));
    }

    if let Some(confidential_memo) = confidential_memo {
      token_properties.confidential_memo = confidential_memo;
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

  pub fn apply_transaction(ledger: &mut LedgerState, tx: Transaction) -> (TxnSID, Vec<TxoSID>) {
    let effect = TxnEffect::compute_effect(&mut ledger.get_prng(), tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();
    ledger.finish_block(block)
          .unwrap()
          .remove(&temp_sid)
          .unwrap()
  }

  pub fn create_issue_and_transfer_txn(ledger: &mut LedgerState,
                                       params: &PublicParams,
                                       code: &AssetTypeCode,
                                       amount: u64,
                                       issuer_keys: &XfrKeyPair,
                                       recipient_pk: &XfrPublicKey)
                                       -> Transaction {
    let mut tx = Transaction::default();
    let _issuer_key_copy = XfrKeyPair::zei_from_bytes(&issuer_keys.zei_to_bytes());

    // issue operation
    let ar = AssetRecord::new(amount, code.val, *issuer_keys.get_pk_ref()).unwrap();
    let art = AssetRecordType::PublicAmount_PublicAssetType;
    let ba = build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &ar, art, &None);

    let asset_issuance_body = IssueAssetBody::new(&code, 0, &[TxOutput(ba.clone())]).unwrap();
    let asset_issuance_operation = IssueAsset::new(asset_issuance_body,
                                                   &IssuerPublicKey { key:
                                                                        *issuer_keys.get_pk_ref() },
                                                   issuer_keys.get_sk_ref()).unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    tx.operations.push(issue_op);

    // transfer operation
    let ar = AssetRecord::new(amount, code.val, *recipient_pk).unwrap();
    let mut transfer =
      TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                             vec![TxoRef::Relative(0)],
                             &[open_asset_record(&ba, &issuer_keys.get_sk_ref()).unwrap()],
                             &[ar]).unwrap(), TransferType::Standard).unwrap();

    transfer.sign(&issuer_keys);
    tx.operations.push(Operation::TransferAsset(transfer));
    tx
  }
}

#[cfg(test)]
mod tests {
  use super::helpers::*;
  use super::*;
  use crate::policies::{calculate_fee, Fraction};
  use rand_core::SeedableRng;
  use std::fs;
  use tempdir::TempDir;
  use tempfile::tempdir;
  use zei::serialization::ZeiFromToBytes;
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record, AssetRecordType};
  use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
  use zei::xfr::structs::AssetRecord;

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
  fn test_compute_and_save_block_hash() {
    let mut ledger_state = LedgerState::test_ledger();
    ledger_state.block_ctx = Some(BlockEffect::new());

    let data = BlockHashData { bitmap: ledger_state.utxo_map.compute_checksum(),
                               block_merkle: ledger_state.block_merkle.get_root_hash(),
                               txns_in_block_hash: ledger_state.status.txns_in_block_hash,
                               global_block_hash: ledger_state.status.global_block_hash };

    let serialized = bincode::serialize(&data).unwrap();
    let count_original = ledger_state.status.block_commit_count;

    ledger_state.compute_and_save_block_hash();

    assert_eq!(ledger_state.status.global_block_hash,
               sha256::hash(&serialized));
    assert_eq!(ledger_state.status.block_commit_count, count_original + 1);
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
    let block_buf = tmp_dir.path().join("test_snapshot_block");
    let txn_buf = tmp_dir.path().join("test_snapshot_txns");
    let block_path = block_buf.to_str().unwrap();
    let txn_path = txn_buf.to_str().unwrap();

    let mut ledger_state = LedgerState::test_ledger();

    ledger_state.status.block_merkle_path = block_path.to_string();
    let block_result = ledger_state.snapshot_block();

    ledger_state.status.txn_merkle_path = txn_path.to_string();
    let txn_result = ledger_state.snapshot_txns();

    // Verify that the SnapshotId is correct
    assert_eq!(block_result.ok().unwrap().id, 0);
    assert_eq!(txn_result.ok().unwrap().id, 0);

    tmp_dir.close().unwrap();
  }

  #[test]
  fn test_checkpoint() {
    let mut ledger_state = LedgerState::test_ledger();
    ledger_state.block_ctx = Some(BlockEffect::new());

    let digest = BitDigest { 0: [0_u8; 32] };
    ledger_state.status.utxo_map_versions = vec![(TxnSID(0), digest); MAX_VERSION - 1].into_iter()
                                                                                      .collect();

    // Verify that checkpoint increases the size of utxo_map_versions by 1 if its length < MAX_VERSION
    ledger_state.checkpoint(&BlockEffect::new());
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION);

    let count_original = ledger_state.status.block_commit_count;

    // Verify that end_commit doesn't change the size of utxo_map_versions if its length >= MAX_VERSION
    ledger_state.status
                .utxo_map_versions
                .push_back((TxnSID(0), digest));
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION + 1);
    ledger_state.checkpoint(&BlockEffect::new());
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION + 1);

    // Verify that the element pushed to the back is as expected
    let back = ledger_state.status.utxo_map_versions.get(MAX_VERSION);
    assert_eq!(back,
               Some(&(ledger_state.status.next_txn, ledger_state.utxo_map.compute_checksum())));

    // Verify that the status is saved as expected
    assert_eq!(ledger_state.status.txns_in_block_hash,
               BlockEffect::new().compute_txns_in_block_hash());
    assert_eq!(ledger_state.status.block_commit_count, count_original + 1);
  }

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
  //     ArchiveAccess::get_global_block_hash

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
      state.finish_block(block).unwrap();
    }

    assert!(state.get_asset_type(&token_code1).is_some());

    assert_eq!(asset_body.asset,
               state.get_asset_type(&token_code1).unwrap().properties);

    assert_eq!(0, state.get_asset_type(&token_code1).unwrap().units);
  }

  // Change the signature to have the wrong public key
  #[test]
  fn test_asset_creation_invalid_public_key() {
    // Create a valid asset creation operation.
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

  #[test]
  fn test_asset_transfer() {
    let mut ledger = LedgerState::test_ledger();
    let params = PublicParams::new();

    let code = AssetTypeCode { val: [1; 16] };
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let key_pair = XfrKeyPair::generate(&mut prng);
    let key_pair_adversary = XfrKeyPair::generate(ledger.get_prng());

    let tx = create_definition_transaction(&code,
                                           key_pair.get_pk_ref(),
                                           key_pair.get_sk_ref(),
                                           None).unwrap();

    let effect = TxnEffect::compute_effect(&mut ledger.get_prng(), tx).unwrap();
    {
      let mut block = ledger.start_block().unwrap();
      ledger.apply_transaction(&mut block, effect).unwrap();
      ledger.finish_block(block).unwrap();
    }

    // Issuance with two outputs
    let mut tx = Transaction::default();

    let ar = AssetRecord::new(100, code.val, key_pair.get_pk_ref().clone()).unwrap();
    let art = AssetRecordType::PublicAmount_PublicAssetType;
    let ba = build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &ar, art, &None);
    let second_ba = ba.clone();

    let asset_issuance_body =
      IssueAssetBody::new(&code, 0, &[TxOutput(ba), TxOutput(second_ba)]).unwrap();
    let asset_issuance_operation =
      IssueAsset::new(asset_issuance_body,
                      &IssuerPublicKey { key: key_pair.get_pk_ref().clone() },
                      key_pair.get_sk_ref()).unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    tx.operations.push(issue_op);

    // Commit issuance to block
    let effect = TxnEffect::compute_effect(ledger.get_prng(), tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (_txn_sid, txos) = ledger.finish_block(block)
                                 .unwrap()
                                 .remove(&temp_sid)
                                 .unwrap();

    // Store txo_sids for subsequent transfers
    let txo_sid = txos[0];
    let second_txo_id = txos[1];

    // Construct transfer operation
    let bar = ((ledger.get_utxo(txo_sid).unwrap().0).0).clone();
    let ar = AssetRecord::new(100, code.val, key_pair_adversary.get_pk_ref().clone()).unwrap();

    let mut tx = Transaction::default();
    let mut transfer =
      TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                             vec![TxoRef::Absolute(txo_sid)],
                             &[open_asset_record(&bar, &key_pair.get_sk_ref()).unwrap()],
                             &[ar]).unwrap(), TransferType::Standard).unwrap();

    let mut second_transfer = transfer.clone();
    transfer.sign(&key_pair);
    tx.operations.push(Operation::TransferAsset(transfer));

    // Commit first transfer
    let effect = TxnEffect::compute_effect(ledger.get_prng(), tx).unwrap();
    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (_txn_sid, _txos) = ledger.finish_block(block)
                                  .unwrap()
                                  .remove(&temp_sid)
                                  .unwrap();

    // Adversary will attempt to spend the same blind asset record at another index
    let mut tx = Transaction::default();
    second_transfer.body.inputs = vec![TxoRef::Absolute(second_txo_id)];

    // Submit spend of same asset at second sid without signature
    second_transfer.body_signatures = Vec::new();
    tx.operations
      .push(Operation::TransferAsset(second_transfer));

    let effect = TxnEffect::compute_effect(ledger.get_prng(), tx);
    assert!(effect.is_err());
  }

  // Sign with the wrong key.
  #[test]
  fn test_asset_creation_invalid_signature() {
    // Create a valid operation.
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
    let block_merkle_buf = tmp_dir.path().join("test_block_merkle");
    let block_merkle_path = block_merkle_buf.to_str().unwrap();
    let txn_merkle_buf = tmp_dir.path().join("test_txn_merkle");
    let txn_merkle_path = txn_merkle_buf.to_str().unwrap();
    let txn_buf = tmp_dir.path().join("test_txnlog");
    let txn_path = txn_buf.to_str().unwrap();
    let utxo_map_buf = tmp_dir.path().join("test_utxo_map");
    let utxo_map_path = utxo_map_buf.to_str().unwrap();

    let mut ledger = LedgerState::new(&block_merkle_path,
                                      &txn_merkle_path,
                                      &txn_path,
                                      &utxo_map_path,
                                      None).unwrap();

    let params = PublicParams::new();

    assert!(ledger.get_global_block_hash() == (BitDigest { 0: [0_u8; 32] }, 0));
    let token_code1 = AssetTypeCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut ledger.get_prng());

    let tx = create_definition_transaction(&token_code1, &public_key, &secret_key, None).unwrap();

    let effect = TxnEffect::compute_effect(&mut ledger.get_prng(), tx).unwrap();
    {
      let mut block = ledger.start_block().unwrap();
      ledger.apply_transaction(&mut block, effect).unwrap();
      ledger.finish_block(block).unwrap();
    }

    let mut tx = Transaction::default();
    let ar = AssetRecord::new(100, token_code1.val, public_key).unwrap();
    let art = AssetRecordType::PublicAmount_PublicAssetType;
    let ba = build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &ar, art, &None);
    let asset_issuance_body = IssueAssetBody::new(&token_code1, 0, &[TxOutput(ba)]).unwrap();
    let asset_issuance_operation = IssueAsset::new(asset_issuance_body,
                                                   &IssuerPublicKey { key: public_key },
                                                   &secret_key).unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    tx.operations.push(issue_op);
    let second_tx = tx.clone();

    let effect = TxnEffect::compute_effect(ledger.get_prng(), tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (txn_sid, txos) = ledger.finish_block(block)
                                .unwrap()
                                .remove(&temp_sid)
                                .unwrap();

    // shouldn't be able to replay issuance
    let effect = TxnEffect::compute_effect(ledger.get_prng(), second_tx).unwrap();
    let mut block = ledger.start_block().unwrap();
    let result = ledger.apply_transaction(&mut block, effect);
    assert!(result.is_err());
    ledger.abort_block(block);

    let transaction = ledger.get_transaction(txn_sid).unwrap().clone();
    let txn_id = transaction.tx_id;

    println!("utxos = {:?}", ledger.status.utxos);
    for txo_id in txos {
      assert!(ledger.status.utxos.contains_key(&txo_id));
    }

    match ledger.get_proof(txn_id) {
      Some(proof) => {
        assert!(proof.tx_id == ledger.get_transaction(txn_id).unwrap().merkle_id);
      }
      None => {
        panic!("get_proof failed for tx_id {}, merkle_id {}, block state {}, transaction state {}",
               transaction.tx_id.0,
               transaction.merkle_id,
               ledger.block_merkle.state(),
               ledger.txn_merkle.state());
      }
    }

    // We don't actually have anything to commmit yet,
    // but this will save the empty checksum, which is
    // enough for a bit of a test.
    assert!(ledger.get_global_block_hash() == (ledger.status.global_block_hash, 2));
    let query_result = ledger.get_utxo_checksum(ledger.status.next_txn.0 as u64)
                             .unwrap();
    let compute_result = ledger.utxo_map.compute_checksum();
    println!("query_result = {:?}, compute_result = {:?}",
             query_result, compute_result);

    assert!(query_result == compute_result);

    match ledger.snapshot_txns() {
      Ok(n) => {
        assert!(n.id == 2);
      }
      Err(x) => {
        panic!("snapshot failed:  {}", x);
      }
    }

    match ledger.snapshot_block() {
      Ok(n) => {
        assert!(n.id == 2);
      }
      Err(x) => {
        panic!("snapshot failed:  {}", x);
      }
    }
  }

  #[test]
  pub fn test_debt_transfer() {
    // Setup
    let mut ledger = LedgerState::test_ledger();
    let params = PublicParams::new();
    let interest_rate = Fraction::new(1, 10); // Interest rate of 10%
    let loan_amount = 1000;
    let loan_burn_amount = 200;
    let payment_amount = calculate_fee(loan_amount, interest_rate) + loan_burn_amount;
    let fiat_refund = 1000;
    let fiat_amount = payment_amount + fiat_refund;

    // Users
    let fiat_issuer_key_pair = XfrKeyPair::generate(&mut ledger.get_prng());
    let borrower_key_pair = XfrKeyPair::generate(&mut ledger.get_prng());
    let lender_key_pair = XfrKeyPair::generate(&mut ledger.get_prng());

    // Define fiat token
    let fiat_code = AssetTypeCode { val: [1; 16] };
    let tx = create_definition_transaction(&fiat_code,
                                           fiat_issuer_key_pair.get_pk_ref(),
                                           fiat_issuer_key_pair.get_sk_ref(),
                                           Some(Memo("fiat".to_string()))).unwrap();
    apply_transaction(&mut ledger, tx);

    // Define debt token
    let debt_code = AssetTypeCode { val: [2; 16] };
    let debt_memo = DebtMemo { interest_rate,
                               fiat_code,
                               loan_amount: loan_amount as u64 };
    let tx =
      create_definition_transaction(&debt_code,
                                    borrower_key_pair.get_pk_ref(),
                                    borrower_key_pair.get_sk_ref(),
                                    Some(Memo(serde_json::to_string(&debt_memo).unwrap()))).unwrap();
    apply_transaction(&mut ledger, tx);

    // Issue and transfer fiat tokens to lender
    let tx = create_issue_and_transfer_txn(&mut ledger,
                                           &params,
                                           &fiat_code,
                                           fiat_amount,
                                           &fiat_issuer_key_pair,
                                           lender_key_pair.get_pk_ref());

    let (_txn_sid, txo_sids) = apply_transaction(&mut ledger, tx);
    let fiat_sid = txo_sids[0];

    // Issue and transfer debt tokens to borrower
    let tx = create_issue_and_transfer_txn(&mut ledger,
                                           &params,
                                           &debt_code,
                                           loan_amount,
                                           &borrower_key_pair,
                                           borrower_key_pair.get_pk_ref());
    let (_txn_sid, txo_sids) = apply_transaction(&mut ledger, tx);
    let debt_sid = txo_sids[0];

    let loan_transfer_record = AssetRecord::new(loan_amount,
                                                debt_code.val,
                                                lender_key_pair.get_pk_ref().clone()).unwrap();
    let fiat_transfer_record = AssetRecord::new(fiat_amount,
                                                fiat_code.val,
                                                borrower_key_pair.get_pk_ref().clone()).unwrap();
    let fiat_bar = ((ledger.get_utxo(fiat_sid).unwrap().0).0).clone();
    let debt_bar = ((ledger.get_utxo(debt_sid).unwrap().0).0).clone();

    let mut tx = Transaction::default();

    let mut transfer = TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                             vec![TxoRef::Absolute(fiat_sid), TxoRef::Absolute(debt_sid)],
                             &[open_asset_record(&fiat_bar, &lender_key_pair.get_sk_ref()).unwrap(),
                             open_asset_record(&debt_bar, &borrower_key_pair.get_sk_ref()).unwrap()],
                               &[fiat_transfer_record, loan_transfer_record]).unwrap(), TransferType::Standard).unwrap();
    transfer.sign(&lender_key_pair);
    transfer.sign(&borrower_key_pair);
    tx.operations.push(Operation::TransferAsset(transfer));

    let (_txn_sid, txo_sids) = apply_transaction(&mut ledger, tx);
    let fiat_sid = txo_sids[0];
    let debt_sid = txo_sids[1];

    // Attempt to pay off debt with correct interest payment
    let null_public_key = XfrPublicKey::zei_from_bytes(&[0; 32]);
    let mut tx = Transaction::default();
    let mut block = ledger.start_block().unwrap();
    let fiat_bar = ((ledger.get_utxo(fiat_sid).unwrap().0).0).clone();
    let debt_bar = ((ledger.get_utxo(debt_sid).unwrap().0).0).clone();

    let payment_record = AssetRecord::new(payment_amount,
                                          fiat_code.val,
                                          lender_key_pair.get_pk_ref().clone()).unwrap();
    let burned_debt_record =
      AssetRecord::new(loan_burn_amount, debt_code.val, null_public_key).unwrap();
    let returned_debt_record = AssetRecord::new(loan_amount - loan_burn_amount,
                                                debt_code.val,
                                                lender_key_pair.get_pk_ref().clone()).unwrap();
    let returned_fiat_record = AssetRecord::new(fiat_amount - payment_amount,
                                                fiat_code.val,
                                                borrower_key_pair.get_pk_ref().clone()).unwrap();

    let transfer_body = TransferAssetBody::new(ledger.get_prng(),
                             vec![TxoRef::Absolute(debt_sid), TxoRef::Absolute(fiat_sid)],
                             &[open_asset_record(&debt_bar, &lender_key_pair.get_sk_ref()).unwrap(),
                               open_asset_record(&fiat_bar, &borrower_key_pair.get_sk_ref()).unwrap()],
                               &[payment_record, burned_debt_record, returned_debt_record, returned_fiat_record]).unwrap();

    tx.operations
      .push(Operation::TransferAsset(TransferAsset::new(transfer_body,
                                                        TransferType::DebtSwap).unwrap()));

    let effect = TxnEffect::compute_effect(ledger.get_prng(), tx).unwrap();
    let result = ledger.apply_transaction(&mut block, effect);
    assert!(result.is_ok());
  }
}
