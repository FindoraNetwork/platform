#![deny(warnings)]
extern crate byteorder;
extern crate tempdir;

use crate::data_model::errors::PlatformError;
use crate::data_model::*;
use crate::policies::{calculate_fee, DebtMemo};
use crate::policy_script::policy_check_txn;
use crate::{add_location, error_location, inp_fail, inv_fail};
use air::{AIRResult, AIR};
use bitmap::{BitMap, SparseMap};
use cryptohash::sha256::Digest as BitDigest;
use log::info;
use merkle_tree::append_only_merkle::AppendOnlyMerkle;
use rand_chacha::ChaChaRng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use serde::{Deserialize, Serialize};
use sliding_set::SlidingSet;
use sparse_merkle_tree::{Key, SmtMap256};
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::fs::OpenOptions;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;
use std::path::PathBuf;
use std::u64;
use utils::HasInvariants;
use utils::{HashOf, ProofOf, Serialized, SignatureOf};
use zei::xfr::lib::XfrNotePolicies;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{AssetTracingPolicies, AssetTracingPolicy, XfrAssetType};

use super::effects::*;

pub struct SnapshotId {
  pub id: u64,
}

pub trait LedgerAccess {
  // Look up a currently unspent TXO
  fn get_utxo(&mut self, addr: TxoSID) -> Option<AuthenticatedUtxo>;

  // The most recently-issued sequence number for the `code`-labelled asset
  // type
  fn get_issuance_num(&self, code: &AssetTypeCode) -> Option<u64>;

  // Retrieve asset type metadata
  fn get_asset_type(&self, code: &AssetTypeCode) -> Option<&AssetType>;

  // Get the sequence number of the most recent checkpoint.
  fn get_block_commit_count(&self) -> u64;

  // Get NoReplayToken
  fn get_no_replay_token(&mut self) -> NoReplayToken;

  // Get the hash of the most recent checkpoint, and its sequence number.
  fn get_state_commitment(&self) -> (HashOf<Option<StateCommitmentData>>, u64);

  // Get the authenticated status of a UTXO (Spent, Unspent, NonExistent).
  fn get_utxo_status(&mut self, addr: TxoSID) -> AuthenticatedUtxoStatus;

  // Get the authenticated KV entry
  fn get_kv_entry(&self, addr: Key) -> AuthenticatedKVLookup;

  // The public signing key this ledger provides
  fn public_key(&self) -> &XfrPublicKey;

  // Sign a message with the ledger's signing key
  fn sign_message<T: Serialize + serde::de::DeserializeOwned>(&self, msg: &T) -> SignatureOf<T>;

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

  // kludge for consensus with heartbeat
  fn pulse_block(block: &mut Self::Block) -> u64;
  fn block_pulse_count(block: &Self::Block) -> u64;
}

// TODO(joe/keyao): which of these methods should be in `LedgerAccess`?
pub trait ArchiveAccess {
  // Number of blocks committed
  fn get_block_count(&self) -> usize;
  // Number of transactions available
  fn get_transaction_count(&self) -> usize;
  // Look up transaction in the log
  fn get_transaction(&self, addr: TxnSID) -> Option<AuthenticatedTransaction>;
  // Look up block in the log
  fn get_block(&self, addr: BlockSID) -> Option<AuthenticatedBlock>;

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

  // Get the ledger state commitment at a specific block height.
  fn get_state_commitment_at_block_height(&self,
                                          height: u64)
                                          -> Option<HashOf<Option<StateCommitmentData>>>;

  // Key-value lookup in AIR
  fn get_air_data(&self, address: &str) -> AuthenticatedAIRResult;
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct LoggedBlock {
  pub block: Vec<Transaction>,
  pub state: StateCommitmentData,
}

const MAX_VERSION: usize = 100;

// Parts of the current ledger state which can be restored from a snapshot
// without replaying a log
#[derive(Deserialize, Serialize, PartialEq, Debug)]
pub struct LedgerStatus {
  // Paths to archival logs for the merkle tree and transaction history
  block_merkle_path: String,
  air_path: String,
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

  // Map a TXO to its output position in a transaction
  txo_to_txn_location: HashMap<TxoSID, (TxnSID, OutputPosition)>,

  // Digests of the UTXO bitmap to (I think -joe) track recent states of
  // the UTXO map
  // TODO(joe): should this be an ordered map of some sort?
  utxo_map_versions: VecDeque<(TxnSID, BitDigest)>,

  // State commitment history. The BitDigest at index i is the state commitment of the ledger at block height  i + 1.
  state_commitment_versions: Vec<HashOf<Option<StateCommitmentData>>>,

  // Arbitrary custom data
  custom_data: SmtMap256<Serialized<(u64, Option<KVEntry>)>>,

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
  // Tracing policy for each asset type
  tracing_policies: HashMap<AssetTypeCode, AssetTracingPolicy>,
  issuance_num: HashMap<AssetTypeCode, u64>,
  // Issuance amounts for assets with limits
  issuance_amounts: HashMap<AssetTypeCode, u64>,

  // Should be equal to the count of transactions
  next_txn: TxnSID,
  // Should be equal to the count of TXOs
  next_txo: TxoSID,

  // Hash and sequence number of the most recent "full checkpoint" of the
  // ledger -- committing to the whole ledger history up to the most recent
  // such checkpoint.
  state_commitment_data: Option<StateCommitmentData>,
  block_commit_count: u64,

  // cumulative consensus specific counter, up to the current block.
  // Updated when applying next block. Always 0 if consensus does not need it,
  // for tendermint with no empty blocks flag, it will go up by exactly 1
  // each time there is a lull in transactions. For tendermint without the flag,
  // it will go up by 1 once a second (by default) unless there is a transaction.
  pulse_count: u64,

  // Hash of the transactions in the most recent block
  txns_in_block_hash: Option<HashOf<Vec<Transaction>>>,

  // Sparse Merkle Tree for Address Identity Registry
  air: AIR,

  // Sliding window of operations for replay attack prevention
  ops_seen: SlidingSet,
}

pub struct LedgerState {
  status: LedgerStatus,

  // PRNG used for transaction validation
  prng: ChaChaRng,

  // Key pair used for signing the state commitment
  // TODO(joe): update this to the generic zei signing API when it exists
  signing_key: XfrKeyPair,

  // Merkle tree tracking the sequence of transaction hashes in the block
  // Each appended hash is the hash of transactions in the same block
  block_merkle: AppendOnlyMerkle,
  // Merkle tree tracking the sequence of all transaction hashes
  // Each appended hash is the hash of a transaction
  txn_merkle: AppendOnlyMerkle,

  // The `FinalizedTransaction`s consist of a Transaction and an index into
  // `merkle` representing its hash.
  // TODO(joe): should this be in-memory?
  blocks: Vec<FinalizedBlock>,

  // Bitmap tracking all the live TXOs
  utxo_map: BitMap,

  txn_log: Option<(PathBuf, File)>,

  block_ctx: Option<BlockEffect>,
}

struct LedgerStateChecker(pub LedgerState);

// TODO(joe): fill these in
impl HasInvariants<PlatformError> for LedgerStatus {
  fn fast_invariant_check(&self) -> Result<(), PlatformError> {
    if self.block_commit_count != self.state_commitment_versions.len() as u64 {
      println!("{}: {}",
               self.block_commit_count,
               self.state_commitment_versions.len());
      return Err(inv_fail!());
    }
    if self.state_commitment_data
           .as_ref()
           .map(|x| x.compute_commitment())
       != self.state_commitment_versions.last().cloned()
    {
      return Err(inv_fail!());
    }
    Ok(())
  }

  fn deep_invariant_check(&self) -> Result<(), PlatformError> {
    self.fast_invariant_check().map_err(add_location!())?;
    Ok(())
  }
}

// TODO(joe): fill these in
impl HasInvariants<PlatformError> for LedgerState {
  fn fast_invariant_check(&self) -> Result<(), PlatformError> {
    self.status
        .fast_invariant_check()
        .map_err(add_location!())?;
    Ok(())
  }

  fn deep_invariant_check(&self) -> Result<(), PlatformError> {
    self.fast_invariant_check().map_err(add_location!())?;
    self.status
        .deep_invariant_check()
        .map_err(add_location!())?;
    let mut txn_sid = 0;
    for (ix, block) in self.blocks.iter().enumerate() {
      let fin_txns = block.txns.to_vec();
      let txns = block.txns
                      .iter()
                      .cloned()
                      .map(|x| x.txn)
                      .collect::<Vec<_>>();

      let proof =
        ProofOf::<Vec<Transaction>>::new(self.block_merkle.get_proof(ix as u64, 0).unwrap());
      if !proof.verify(&txns) {
        return Err(inv_fail!(format!("Bad block proof at {}", ix)));
      }

      for fin_txn in fin_txns.iter() {
        let ix = txn_sid;
        if ix != fin_txn.tx_id.0 {
          return Err(inv_fail!());
        }
        txn_sid += 1;
        let proof =
          ProofOf::<(TxnSID, Transaction)>::new(self.txn_merkle.get_proof(ix as u64, 0).unwrap());
        if !proof.0.verify(fin_txn.hash().0) {
          return Err(inv_fail!(format!("Bad txn proof at {}", ix)));
        }
      }
    }
    if let Some((_, txn_log_fd)) = &self.txn_log {
      txn_log_fd.sync_data().unwrap();
      let tmp_dir = utils::fresh_tmp_dir();

      let other_block_merkle_buf = tmp_dir.join("test_block_merkle");
      let other_block_merkle_path = other_block_merkle_buf.to_str().unwrap();

      let other_air_buf = tmp_dir.join("test_air");
      let other_air_path = other_air_buf.to_str().unwrap();

      let other_txn_merkle_buf = tmp_dir.join("test_txn_merkle");
      let other_txn_merkle_path = other_txn_merkle_buf.to_str().unwrap();

      let other_txn_buf = tmp_dir.join("test_txnlog");
      let other_txn_path = other_txn_buf.to_str().unwrap();

      let other_utxo_map_buf = tmp_dir.join("test_utxo_map");
      let other_utxo_map_path = other_utxo_map_buf.to_str().unwrap();

      // dbg!(&self.status.txn_path);
      // dbg!(std::fs::metadata(&self.status.txn_path).unwrap());
      // dbg!(&other_txn_path);
      std::fs::copy(&self.status.txn_path, &other_txn_path).unwrap();
      std::fs::copy(&self.status.block_merkle_path, &other_block_merkle_path).unwrap();
      std::fs::copy(&self.status.txn_merkle_path, &other_txn_merkle_path).unwrap();
      std::fs::copy(&self.status.utxo_map_path, &other_utxo_map_path).unwrap();

      let state2 = Box::new(LedgerState::load_checked_from_log(&other_block_merkle_path,
                                                               &other_air_path,
                                                               &other_txn_merkle_path,
                                                               &other_txn_path,
                                                               &other_utxo_map_path,
                                                               None,
                                                               None).unwrap());

      let mut status2 = Box::new(state2.status);
      status2.block_merkle_path = self.status.block_merkle_path.clone();
      status2.air_path = self.status.air_path.clone();
      status2.txn_merkle_path = self.status.txn_merkle_path.clone();
      status2.txn_path = self.status.txn_path.clone();
      status2.utxo_map_path = self.status.utxo_map_path.clone();
      status2.utxo_map_versions = self.status.utxo_map_versions.clone();

      dbg!(&status2);
      dbg!(&self.status);
      assert!(*status2 == self.status);

      std::fs::remove_dir_all(tmp_dir).unwrap();
    }
    Ok(())
  }
}

impl LedgerStatus {
  pub fn new(block_merkle_path: &str,
             air_path: &str,
             txn_merkle_path: &str,
             txn_path: &str,
             // TODO(joe): should this do something?
             // snapshot_path: &str,
             utxo_map_path: &str)
             -> Result<LedgerStatus, std::io::Error> {
    let ledger = LedgerStatus { block_merkle_path: block_merkle_path.to_owned(),
                                air_path: air_path.to_owned(),
                                txn_merkle_path: txn_merkle_path.to_owned(),
                                air: LedgerState::init_air_log(air_path, true)?,
                                ops_seen: SlidingSet::new(0, 0, TRANSACTION_WINDOW_WIDTH),
                                txn_path: txn_path.to_owned(),
                                utxo_map_path: utxo_map_path.to_owned(),
                                utxos: HashMap::new(),
                                custom_data: SmtMap256::new(),
                                txo_to_txn_location: HashMap::new(),
                                issuance_amounts: HashMap::new(),
                                utxo_map_versions: VecDeque::new(),
                                state_commitment_versions: Vec::new(),
                                asset_types: HashMap::new(),
                                tracing_policies: HashMap::new(),
                                issuance_num: HashMap::new(),
                                next_txn: TxnSID(0),
                                next_txo: TxoSID(0),
                                txns_in_block_hash: None,
                                state_commitment_data: None,
                                block_commit_count: 0,
                                pulse_count: 0 };

    Ok(ledger)
  }

  pub fn incr_block_commit_count(&mut self) {
    self.block_commit_count += 1;
    self.ops_seen.incr_current();
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
  #[allow(clippy::clone_double_ref)]
  #[allow(clippy::cognitive_complexity)]
  fn check_txn_effects(&self, txn_effect: TxnEffect) -> Result<TxnEffect, PlatformError> {
    // The current transactions seq_id must be within the sliding window over seq_ids
    if txn_effect.txn.body.no_replay_token.get_seq_id() > self.block_commit_count {
      return Err(PlatformError::InputsError(format!("Transaction seq_id ahead of block_count: {}",
                                                    error_location!())));
    } else if txn_effect.txn.body.no_replay_token.get_seq_id() + TRANSACTION_WINDOW_WIDTH
              < self.block_commit_count
    {
      return Err(PlatformError::InputsError(format!("Transaction seq_id too far behind block_count: {}",
                                                    error_location!())));
    } else {
      // None of the operations in the current transaction have been seen before in the window
      for op_digest in txn_effect.op_digests.iter() {
        if self.ops_seen.contains_key(*op_digest) {
          return Err(PlatformError::InputsError(format!("Operation seen before, possible replay: {}",
                                                        error_location!())));
        }
      }
    }

    // Key-Value updates must be
    // 1. Signed by the previous owner of that key, if one exists
    // 2. The generation number starts at 0 or increments
    // 3. Signed by the new owner of that key, if one exists
    // (2) is checked for all but the first value in local validation
    // (3) is already handled in local validation
    for (k, update) in txn_effect.kv_updates.iter() {
      let (sig, gen_num, update) = update.first().unwrap();
      if let Some(ent) = self.custom_data.get(&k) {
        let (prev_gen_num, ent) = ent.deserialize();
        // (2)
        if prev_gen_num + 1 != *gen_num {
          return Err(PlatformError::InputsError(error_location!()));
        }
        if let Some(ent) = ent {
          // (1)
          KVUpdate { body: (*k, *gen_num, update.clone()),
                     signature: sig.clone() }.check_signature(&ent.0)
                                             .map_err(add_location!())?;
        }
      } else {
        // (2)
        if *gen_num != 0 {
          return Err(PlatformError::InputsError(error_location!()));
        }
      }
    }

    // 1. Each input must be unspent and correspond to the claimed record
    // 2. Inputs with transfer restrictions can only be owned by the asset issuer
    for (inp_sid, inp_record) in txn_effect.input_txos.iter() {
      // (1)
      let inp_utxo = self.utxos
                         .get(inp_sid)
                         .map_or(Err(PlatformError::InputsError(error_location!())), Ok)?;
      let record = &(inp_utxo.0).0;
      if record != inp_record {
        return Err(PlatformError::InputsError(error_location!()));
      }
      // (2)
      if let Some(code) = record.asset_type
                                .get_asset_type()
                                .map(|v| AssetTypeCode { val: v })
      {
        let asset_type = self.asset_types
                             .get(&code)
                             .or_else(|| txn_effect.new_asset_codes.get(&code))
                             .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        if !asset_type.properties.asset_rules.transferable
           && asset_type.properties.issuer.key != record.public_key
        {
          return Err(PlatformError::InputsError(error_location!()));
        }
      }
    }

    // Internally spend inputs with transfer restrictions can only be owned by the asset issuer
    for record in txn_effect.internally_spent_txos.iter() {
      if let Some(code) = record.asset_type
                                .get_asset_type()
                                .map(|v| AssetTypeCode { val: v })
      {
        // dbg!(&self.asset_types);
        let asset_type = self.asset_types
                             .get(&code)
                             .or_else(|| txn_effect.new_asset_codes.get(&code))
                             .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        if !asset_type.properties.asset_rules.transferable
           && asset_type.properties.issuer.key != record.public_key
        {
          return Err(PlatformError::InputsError(error_location!()));
        }
      }
    }

    // dbg!("records work");

    // New asset types must not already exist
    for (code, _asset_type) in txn_effect.new_asset_codes.iter() {
      if self.asset_types.contains_key(&code) {
        return Err(inp_fail!());
      }
      if self.issuance_num.contains_key(&code) {
        return Err(inp_fail!());
      }
      debug_assert!(txn_effect.new_issuance_nums.contains_key(&code));

      // Asset issuance should match the currently registered key
    }

    // dbg!("new types work");

    // New issuance numbers
    // (1) Must refer to a created asset type
    //  - NOTE: if the asset type is created in this transaction, this
    //    function is assuming that the ordering within the transaction is
    //    already valid.
    // (2) Must not be below the current asset cap
    //  - NOTE: this relies on the sequence numbers appearing in sorted
    //    order
    for (code, seq_nums) in txn_effect.new_issuance_nums.iter() {
      debug_assert!(txn_effect.issuance_keys.contains_key(&code));
      // dbg!(&(code, seq_nums));

      let iss_key = txn_effect.issuance_keys.get(&code).unwrap();
      let asset_type = self.asset_types
                           .get(&code)
                           .or_else(|| txn_effect.new_asset_codes.get(&code))
                           .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
      let proper_key = asset_type.properties.issuer;
      if *iss_key != proper_key {
        return Err(inp_fail!());
      }

      if seq_nums.is_empty() {
        if !txn_effect.new_asset_codes.contains_key(&code) {
          return Err(PlatformError::InputsError(error_location!()));
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
                                       assert!(txn_effect.new_asset_codes.contains_key(&code));
                                       Some(&0)
                                     })
                                     .unwrap();
        let min_seq_num = seq_nums.first().unwrap();
        if min_seq_num < curr_seq_num_limit {
          return Err(inp_fail!());
        }
      }
    }

    // Asset Caps
    // (1) New issuance amounts cannot exceed asset cap
    // (2) No confidential issuances allowed for assets with issuance restrictions
    for (code, amount) in txn_effect.issuance_amounts.iter() {
      let asset_type = self.asset_types
                           .get(&code)
                           .or_else(|| txn_effect.new_asset_codes.get(&code))
                           .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
      // (1)
      if let Some(cap) = asset_type.properties.asset_rules.max_units {
        let current_amount = self.issuance_amounts
                                 .get(code)
                                 .or_else(|| Some(&0))
                                 .unwrap();
        if current_amount.checked_add(*amount)
                         .ok_or_else(|| PlatformError::InputsError(error_location!()))?
           > cap
        {
          return Err(inp_fail!());
        }
      }
    }

    // (2)
    for code in txn_effect.confidential_issuance_types.iter() {
      let asset_type = self.asset_types
                           .get(&code)
                           .or_else(|| txn_effect.new_asset_codes.get(&code))
                           .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
      if asset_type.has_issuance_restrictions() {
        return Err(inp_fail!());
      }
    }

    // Assets with cosignature requirements must have enough signatures
    for ((op_idx, input_idx), key_set) in txn_effect.cosig_keys.iter() {
      let op = &txn_effect.txn.body.operations[*op_idx];
      if let Operation::TransferAsset(xfr) = op {
        if let XfrAssetType::NonConfidential(val) = xfr.body.transfer.inputs[*input_idx].asset_type
        {
          let code = AssetTypeCode { val };
          let signature_rules = &self.asset_types
                                     .get(&code)
                                     .or_else(|| txn_effect.new_asset_codes.get(&code))
                                     .ok_or_else(|| PlatformError::InputsError(error_location!()))?
                                     .properties
                                     .asset_rules
                                     .transfer_multisig_rules;
          if let Some(rules) = signature_rules {
            rules.check_signature_set(key_set)
                 .map_err(add_location!())?;
          }
        }
      } else {
        return Err(inp_fail!());
      }
    }

    // Check that asset types were validated under the correct tracing policies
    for (code, tracing_policies) in txn_effect.tracing_policies.iter() {
      let definition_policies = &self.asset_types
                                     .get(&code)
                                     .or_else(|| txn_effect.new_asset_codes.get(&code))
                                     .ok_or_else(|| PlatformError::InputsError(error_location!()))?
                                     .properties
                                     .asset_rules
                                     .tracing_policies;

      if definition_policies != tracing_policies {
        return Err(inp_fail!());
      }
    }

    // Debt swaps
    // (1) Fiat code must match debt asset memo
    // (2) fee must be correct
    for (code, debt_swap_effects) in txn_effect.debt_effects.iter() {
      // dbg!(&(code, debt_swap_effects));
      let debt_type = &self.asset_types
                           .get(&code)
                           .or_else(|| txn_effect.new_asset_codes.get(&code))
                           .ok_or_else(|| PlatformError::InputsError(error_location!()))?
                           .properties;

      let debt_memo = serde_json::from_str::<DebtMemo>(&debt_type.memo.0).map_err(add_location!())?;
      let correct_fee = calculate_fee(debt_swap_effects.initial_balance, debt_memo.interest_rate);

      // (1), (2)
      if debt_swap_effects.fiat_code != debt_memo.fiat_code
         || debt_swap_effects.fiat_paid != debt_swap_effects.debt_burned + correct_fee
      {
        return Err(PlatformError::InputsError(error_location!()));
      }
    }

    // Memo updates
    // Multiple memo updates for the same asset are allowed, but only the last one will be applied.
    for memo_update in txn_effect.memo_updates.iter() {
      let asset = self.asset_types
                      .get(&memo_update.0)
                      .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
      // Asset must be updatable and key must be correct
      if !asset.properties.asset_rules.updatable
         || asset.properties.issuer != (IssuerPublicKey { key: memo_update.1 })
      {
        return Err(PlatformError::InputsError(error_location!()));
      }
    }

    // Until we can distinguish assets that have policies that invoke transfer restrictions
    // from those that don't, prevent any non-confidential assets with transfer restrictions
    // from becoming confidnetial
    for code in txn_effect.confidential_transfer_inputs.iter() {
      let asset_type = self.asset_types
                           .get(&code)
                           .or_else(|| txn_effect.new_asset_codes.get(&code))
                           .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
      if asset_type.has_transfer_restrictions() {
        PlatformError::InputsError(error_location!());
      }
    }

    // Policy checking
    // TODO(joe): Currently the policy language can't validate transactions
    //   which include DefineAsset, so it's safe to assume that any valid
    //   policy usage involves an asset whose definition is already in the
    //   committed state. However, it may not always be that way, and this
    //   code will lead to erroneous validation failures when that change
    //   arrives.
    for code in txn_effect.asset_types_involved.iter() {
      if txn_effect.custom_policy_asset_types.contains_key(code) {
        let asset = self.asset_types
                        .get(code)
                        .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        if let Some((ref pol, ref globals)) = asset.properties.policy {
          let globals = globals.clone();
          policy_check_txn(code, globals, &pol, &txn_effect.txn).map_err(add_location!())?;
        }
      }
    }

    Ok(txn_effect)
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
    for (digest, seq_id) in block.opseqs.iter() {
      self.ops_seen.insert(*digest, *seq_id);
    }
    block.opseqs.clear();

    // KV updates
    for (k, ent) in block.kv_updates.drain() {
      // safe unwrap since entries in kv_updates should be non-empty
      let final_val = ent.last().unwrap();
      self.custom_data.set(&k,
                           Some(Serialized::new(&(final_val.1, final_val.2.clone()))));
    }

    // Remove consumed UTXOs
    for (inp_sid, _) in block.input_txos.drain() {
      // Remove from ledger status
      debug_assert!(self.utxos.contains_key(&inp_sid));
      self.utxos.remove(&inp_sid);
    }

    // Apply AIR updates
    for (addr, data) in block.air_updates.drain() {
      debug_assert!(self.air.get(&addr).is_none());
      self.air.set(&addr, Some(data));
    }

    // Apply memo updates
    for (code, memo) in block.memo_updates.drain() {
      let mut asset = self.asset_types.get_mut(&code).unwrap();
      (*asset).properties.memo = memo;
    }

    for (code, amount) in block.issuance_amounts.drain() {
      let amt = self.issuance_amounts.entry(code).or_insert(0);
      *amt += amount;
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

    debug_assert_eq!(block.clone(), {
      let mut def: BlockEffect = Default::default();
      def.txns = block.txns.clone();
      def.temp_sids = block.temp_sids.clone();

      def
    });

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
      None => Err(PlatformError::InputsError(error_location!())),
      // Probably should be a more relevant error
      Some(block) => Ok(block),
    }
  }

  fn apply_transaction(&self,
                       block: &mut BlockEffect,
                       txn: TxnEffect)
                       -> Result<TxnTempSID, PlatformError> {
    block.add_txn_effect(self.status
                             .check_txn_effects(txn)
                             .map_err(add_location!())?)
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
    block.air_updates.clear();

    debug_assert_eq!(block.clone(), Default::default());

    ret
  }

  #[allow(clippy::cognitive_complexity)]
  fn finish_block(&mut self,
                  block: BlockEffect)
                  -> Result<HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>, std::io::Error> {
    let mut block = block;

    let base_sid = self.status.next_txo.0;
    let txn_temp_sids = block.temp_sids.clone();

    let block_txns = block.txns.clone();

    for (inp_sid, _) in block.input_txos.iter() {
      // Remove from bitmap
      debug_assert!(self.utxo_map.query(inp_sid.0 as usize).unwrap());
      self.utxo_map.clear(inp_sid.0 as usize).unwrap();
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

      // TODO(joe/keyao): reorder these so that we can drain things

      // Update the transaction Merkle tree and transaction log
      // Store the location of each utxo so we can create authenticated utxo proofs
      for (tmp_sid, txn) in block.temp_sids.iter().zip(block.txns.iter()) {
        let txn = txn.clone();
        let txo_sid_map = temp_sid_map.get(&tmp_sid).unwrap();
        let txn_sid = txo_sid_map.0;
        let txo_sids = &txo_sid_map.1;

        // TODO(joe/jonathan): Since errors in the merkle tree are things like
        // corruption and I/O failure, we don't have a good recovery story. Is
        // panicking acceptable?
        let merkle_id = self.txn_merkle
                            .append_hash(&txn.hash(txn_sid).0.hash.into())
                            .unwrap();

        tx_block.push(FinalizedTransaction { txn: txn.clone(),
                                             tx_id: txn_sid,
                                             merkle_id });

        let outputs = txn.get_outputs_ref(false);
        debug_assert!(txo_sids.len() == outputs.len());

        for (position, sid) in txo_sids.iter().enumerate() {
          self.status
              .txo_to_txn_location
              .insert(*sid, (txn_sid, OutputPosition(position)));
        }
      }
      // this feels like the wrong place for this, but there's no other good place unless
      // we move the pulse into the StateCommitment, which seems like the wrong answer to
      // a consensus-specific hack.
      // The entirety of the checkpoint function seems wrong, though, because the BlockEffect is
      // applied to the LedgerState in a way that renders the BlockEffect obsolete, without
      // moving it.
      self.status.pulse_count += block.pulse_count;
      block.pulse_count = 0;

      // Checkpoint
      let block_merkle_id = self.checkpoint(&block);
      block.temp_sids.clear();
      block.txns.clear();

      //Add block to txn history
      //TODO(joe/nathan): This ordering feels bad -- the txn log should probably be write-ahead,
      //but the state commitment you need doesn't exist yet! maybe these should be two different
      //logs, or the writing should be staggered in some way.
      if let Some((_, txn_log_fd)) = &mut self.txn_log {
        writeln!(txn_log_fd,"{}",serde_json::to_string(&LoggedBlock { block: block_txns, state: self.status.state_commitment_data.clone().unwrap() }).unwrap())
              .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other,e)).unwrap();
        txn_log_fd.sync_data().unwrap();
      }

      self.blocks.push(FinalizedBlock { txns: tx_block,
                                        merkle_id: block_merkle_id });
    }

    debug_assert_eq!(block, Default::default());

    self.block_ctx = Some(block);

    Ok(temp_sid_map)
  }
  fn pulse_block(block: &mut BlockEffect) -> u64 {
    block.add_pulse()
  }
  fn block_pulse_count(block: &Self::Block) -> u64 {
    block.get_pulse_count()
  }
}

impl LedgerUpdate<ChaChaRng> for LedgerStateChecker {
  type Block = BlockEffect;

  fn get_prng(&mut self) -> &mut ChaChaRng {
    self.0.get_prng()
  }

  fn start_block(&mut self) -> Result<BlockEffect, PlatformError> {
    self.0.start_block()
  }

  fn apply_transaction(&self,
                       block: &mut BlockEffect,
                       txn: TxnEffect)
                       -> Result<TxnTempSID, PlatformError> {
    // inputs must be listed as spent in the bitmap
    for (inp_sid, _) in txn.input_txos.iter() {
      if self.0.utxo_map.query(inp_sid.0 as usize).unwrap() {
        return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                             std::file!(),
                                                             std::line!(),
                                                             std::column!())));
      }
    }

    let base_ix = self.0.status.next_txo.0 + (block.txos.len() as u64);

    // internally-spent outputs must be listed as spent
    for (ix, txo) in txn.txos.iter().enumerate() {
      let live = self.0
                     .utxo_map
                     .query((base_ix + (ix as u64)) as usize)
                     .map_err(add_location!())?;
      if txo.is_none() && live {
        return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                             std::file!(),
                                                             std::line!(),
                                                             std::column!())));
      }
    }

    /*
     * NOTE: validity checking of the bitmap is a little bit subtle
     *
     * The approach this code takes is:
     *  - Every TXO which is spent by some transaction is 0 in the bitmap
     *  - Once the log has been replayed, every remaining unspent TXO is 1
     *    in the bitmap
     *
     * This is basically the exact spec of bitmap correctness, but it
     * relies on the UTXO set in `LedgerStatus` being authoritative -- if
     * there is a bug in updating that set, then a bug in updating the UTXO
     * bitmap won't be detected by this code.
     */

    // The transaction must match its spot in the txn merkle tree
    let txn_sid = self.0.status.next_txn.0 + block.txns.len();
    let proof = ProofOf::<(TxnSID, Transaction)>::new(self.0
                                                          .txn_merkle
                                                          .get_proof(txn_sid as u64, 0)
                                                          .map_err(add_location!())?);

    if !proof.0.verify(txn.txn.hash(TxnSID(txn_sid)).0) {
      return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                           std::file!(),
                                                           std::line!(),
                                                           std::column!())));
    }

    self.0.apply_transaction(block, txn)
  }

  // this shouldn't ever be called, since this type should only be used for
  // replaying a log, and there isn't a reason to abort
  fn abort_block(&mut self, _block: BlockEffect) -> HashMap<TxnTempSID, Transaction> {
    unimplemented!()
  }

  fn finish_block(&mut self,
                  block: BlockEffect)
                  -> Result<HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>, std::io::Error> {
    let mut block = block;

    let block_id = self.0.blocks.len();

    let temp_sid_map = self.0.status.apply_block_effects(&mut block);

    {
      let mut tx_block = Vec::new();

      // TODO(joe/keyao): reorder these so that we can drain things

      // Update the transaction Merkle tree and transaction log
      for (tmp_sid, txn) in block.temp_sids.iter().zip(block.txns.iter()) {
        let txn = txn.clone();
        let txo_sid_map = temp_sid_map.get(&tmp_sid).unwrap();
        let txn_sid = txo_sid_map.0;
        let txo_sids = &txo_sid_map.1;

        let outputs = txn.get_outputs_ref(false);
        debug_assert!(txo_sids.len() == outputs.len());

        for (position, sid) in txo_sids.iter().enumerate() {
          self.0
              .status
              .txo_to_txn_location
              .insert(*sid, (txn_sid, OutputPosition(position)));
        }

        tx_block.push(FinalizedTransaction { txn,
                                             tx_id: txn_sid,
                                             merkle_id: txn_sid.0 as u64 });
      }

      self.0.blocks.push(FinalizedBlock { txns: tx_block,
                                          merkle_id: block_id as u64 });
    }

    block.txns.clear();
    block.temp_sids.clear();

    debug_assert_eq!(block, Default::default());

    self.0.block_ctx = Some(block);

    Ok(temp_sid_map)
  }
  fn pulse_block(block: &mut BlockEffect) -> u64 {
    block.add_pulse()
  }
  fn block_pulse_count(block: &Self::Block) -> u64 {
    block.get_pulse_count()
  }
}

impl LedgerStateChecker {
  pub fn check_block(self, ix: u64, block: &BlockEffect) -> Result<Self, PlatformError> {
    log::debug!("Checking block {}", ix);

    // The block must match its spot in the block merkle tree
    let proof = ProofOf::<Vec<Transaction>>::new(self.0
                                                     .block_merkle
                                                     .get_proof(ix, 0)
                                                     .map_err(add_location!())?);

    let block = block;

    let block_merkle_hash = block.compute_txns_in_block_hash();

    // dbg!(&self.0.block_merkle.state());
    // dbg!(&proof);
    // dbg!(&block_merkle_hash);
    if !proof.0.verify(block_merkle_hash.clone().0) {
      return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                           std::file!(),
                                                           std::line!(),
                                                           std::column!())));
    }

    let comm = (&self.0.status.state_commitment_data).as_ref().unwrap();
    if comm.txns_in_block_hash != block_merkle_hash {
      return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                           std::file!(),
                                                           std::line!(),
                                                           std::column!())));
    }

    // dbg!(&comm.txo_count);
    // dbg!(&self.0.status.next_txo.0);
    if comm.txo_count != self.0.status.next_txo.0 + block.txos.iter().flatten().count() as u64 {
      return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                           std::file!(),
                                                           std::line!(),
                                                           std::column!())));
    }

    Ok(self)
  }

  pub fn finish_check(mut self) -> Result<LedgerState, PlatformError> {
    // Check the UTXO set is all "on" in the bitmap, and check the top
    // level state commitment.

    for (ix, _) in self.0.status.utxos.iter() {
      let live = self.0
                     .utxo_map
                     .query(ix.0 as usize)
                     .map_err(add_location!())?;
      if !live {
        return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                             std::file!(),
                                                             std::line!(),
                                                             std::column!())));
      }
    }

    match self.0.status.state_commitment_data.as_ref() {
      Some(comm) => {
        if self.0.utxo_map.compute_checksum() != comm.bitmap {
          return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                               std::file!(),
                                                               std::line!(),
                                                               std::column!())));
        }
        if self.0.block_merkle.get_root_hash() != comm.block_merkle {
          return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                               std::file!(),
                                                               std::line!(),
                                                               std::column!())));
        }
        if self.0.txn_merkle.get_root_hash() != comm.transaction_merkle_commitment {
          return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                               std::file!(),
                                                               std::line!(),
                                                               std::column!())));
        }
      }
      None => {
        if self.0.status.block_commit_count != 0 {
          return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                               std::file!(),
                                                               std::line!(),
                                                               std::column!())));
        }
      }
    }

    self.0.fast_invariant_check().unwrap();

    Ok(self.0)
  }
}

impl LedgerState {
  #[cfg(feature = "TESTING")]
  #[allow(non_snake_case)]
  pub fn TESTING_get_status(&self) -> &LedgerStatus {
    &self.status
  }

  pub fn txn_log_path(&self) -> Option<PathBuf> {
    Some(self.txn_log.as_ref()?.0.clone())
  }

  // Create a ledger for use by a unit test.
  pub fn test_ledger() -> LedgerState {
    let tmp_dir = utils::fresh_tmp_dir();

    let block_merkle_buf = tmp_dir.join("test_block_merkle");
    let block_merkle_path = block_merkle_buf.to_str().unwrap();

    let air_buf = tmp_dir.join("test_air");
    let air_path = air_buf.to_str().unwrap();

    let txn_merkle_buf = tmp_dir.join("test_txn_merkle");
    let txn_merkle_path = txn_merkle_buf.to_str().unwrap();

    let txn_buf = tmp_dir.join("test_txnlog");
    let txn_path = txn_buf.to_str().unwrap();

    // let snap_buf      = tmp_dir.join("test_ledger_snap");
    // let snap_path     = snap_buf.to_str().unwrap();

    let utxo_map_buf = tmp_dir.join("test_utxo_map");
    let utxo_map_path = utxo_map_buf.to_str().unwrap();

    let ret = LedgerState::new(&block_merkle_path,
                               &air_path,
                               &txn_merkle_path,
                               &txn_path,
                               &utxo_map_path,
                               None,
                               None).unwrap();

    let key_buf = tmp_dir.join("test_sig_key");
    let key_path = key_buf.to_str().unwrap();
    {
      let file = File::create(key_path).unwrap();
      {
        let mut writer = BufWriter::new(file);

        serde_json::to_writer::<&mut BufWriter<File>, XfrKeyPair>(&mut writer, &ret.signing_key).unwrap();
      }
    }

    ret
  }

  // TODO(joe): Make this an iterator of some sort so that we don't have to load the whole log
  // into memory
  fn load_transaction_log(path: &str) -> Result<Vec<LoggedBlock>, PlatformError> {
    let file = File::open(path).map_err(add_location!())?;
    let reader = BufReader::new(file);
    let mut v = Vec::new();
    for l in reader.lines() {
      let l = l.map_err(add_location!())?;
      match serde_json::from_str::<LoggedBlock>(&l) {
        Ok(next_block) => {
          v.push(next_block);
        }
        Err(e) => {
          if l != "" {
            return Err(PlatformError::DeserializationError(format!("[{}]: {:?}",
                                                                   &error_location!(),
                                                                   e)));
          }
        }
      }
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
  fn compute_and_append_txns_hash(&mut self, block: &BlockEffect) -> u64 {
    // 1. Compute the hash of transactions in the block and update txns_in_block_hash
    let txns_in_block_hash = block.compute_txns_in_block_hash();
    self.status.txns_in_block_hash = Some(txns_in_block_hash.clone());

    // 2. Append txns_in_block_hash to block_merkle
    //  2.1 Update the block Merkle tree
    let ret = self.block_merkle
                  .append_hash(&txns_in_block_hash.0.hash.into())
                  .unwrap();
    // dbg!(&block.txns);
    // dbg!(&txns_in_block_hash);
    debug_assert!(ProofOf::<Vec<Transaction>>::new(self.block_merkle
                                                       .get_proof(self.status.block_commit_count, 0)
                                                       .unwrap()).0
                                                                 .verify(txns_in_block_hash.0));
    ret
  }

  fn compute_and_save_state_commitment_data(&mut self) {
    let prev_commitment = HashOf::new(&self.status.state_commitment_data);
    self.status.state_commitment_data =
      Some(StateCommitmentData { bitmap: self.utxo_map.compute_checksum(),
                                 block_merkle: self.block_merkle.get_root_hash(),
                                 transaction_merkle_commitment: self.txn_merkle.get_root_hash(),
                                 air_commitment: *self.status.air.merkle_root(),
                                 txns_in_block_hash: self.status
                                                         .txns_in_block_hash
                                                         .as_ref()
                                                         .cloned()
                                                         .unwrap(),
                                 previous_state_commitment: prev_commitment,
                                 txo_count: self.status.next_txo.0,
                                 pulse_count: self.status.pulse_count,
                                 kv_store: *self.status.custom_data.merkle_root() });
    self.status.state_commitment_versions.push(self.status
                                                   .state_commitment_data
                                                   .as_ref()
                                                   .unwrap()
                                                   .compute_commitment());
    self.status.incr_block_commit_count();
  }

  // Initialize a logged Merkle tree for the ledger.  We might
  // be creating a new tree or opening an existing one.  We
  // always start a new log file.
  fn init_merkle_log(path: &str, create: bool) -> Result<AppendOnlyMerkle, std::io::Error> {
    // Create a merkle tree or open an existing one.
    let result = if create {
      AppendOnlyMerkle::create(path)
    } else {
      AppendOnlyMerkle::open(path)
    };

    info!("Using path {} for the Merkle tree.", path);

    let tree = match result {
      Err(x) => {
        return Err(x);
      }
      Ok(tree) => tree,
    };

    // Create a log for the tree.  The tree size ("state") is appended to
    // the end of the path.
    // TODO: START https://github.com/findoraorg/platform/issues/307
    // let next_id = tree.total_size();
    // let writer = LedgerState::create_merkle_log(path.to_owned(), next_id)?;
    // TODO: END This is being disabled as we decide what to do about about logging, archival, etc
    Ok(tree)
    // Ok(LoggedMerkle::new(tree, writer))
  }

  fn init_air_log(path: &str, create: bool) -> Result<AIR, std::io::Error> {
    // Create a merkle tree or open an existing one.
    let result = if create {
      Ok(AIR::default())
    } else {
      air::open(path)
    };

    info!("Using path {} for the Address Identity Registry.", path);

    let tree = match result {
      Err(x) => {
        return Err(x);
      }
      Ok(tree) => tree,
    };
    Ok(tree)
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
             air_path: &str,
             txn_merkle_path: &str,
             txn_path: &str,
             utxo_map_path: &str,
             keypair: Option<XfrKeyPair>,
             prng_seed: Option<[u8; 32]>)
             -> Result<LedgerState, std::io::Error> {
    let mut prng = prng_seed.map(rand_chacha::ChaChaRng::from_seed)
                            .unwrap_or_else(ChaChaRng::from_entropy);
    let signing_key = keypair.unwrap_or_else(|| XfrKeyPair::generate(&mut prng));
    let ledger = LedgerState { status: LedgerStatus::new(block_merkle_path,
                                                         air_path,
                                                         txn_merkle_path,
                                                         txn_path,
                                                         utxo_map_path)?,
                               prng,
                               signing_key,
                               block_merkle: LedgerState::init_merkle_log(block_merkle_path,
                                                                          true)?,
                               txn_merkle: LedgerState::init_merkle_log(txn_merkle_path, true)?,
                               blocks: Vec::new(),
                               utxo_map: LedgerState::init_utxo_map(utxo_map_path, true)?,
                               txn_log: Some((txn_path.into(),
                                              std::fs::OpenOptions::new().create_new(true)
                                                                         .append(true)
                                                                         .open(txn_path)?)),
                               block_ctx: Some(BlockEffect::new()) };

    ledger.txn_log.as_ref().unwrap().1.sync_all()?;

    Ok(ledger)
  }

  pub fn load_checked_from_log(block_merkle_path: &str,
                               air_path: &str,
                               txn_merkle_path: &str,
                               txn_path: &str,
                               utxo_map_path: &str,
                               signing_key_path: Option<&str>,
                               prng_seed: Option<[u8; 32]>)
                               -> Result<LedgerState, PlatformError> {
    let mut prng = prng_seed.map(rand_chacha::ChaChaRng::from_seed)
                            .unwrap_or_else(ChaChaRng::from_entropy);
    let signing_key = match signing_key_path {
      Some(path) => {
        let ret = {
          let file = File::open(path).map_err(add_location!())?;
          let mut reader = BufReader::new(file);
          serde_json::from_reader::<&mut BufReader<File>, XfrKeyPair>(&mut reader)
        };
        ret.or_else::<PlatformError,_>(|_| {
          let key = XfrKeyPair::generate(&mut prng);
          let file = File::create(path).map_err(add_location!())?;
          let mut writer = BufWriter::new(file);

          serde_json::to_writer::<&mut BufWriter<File>, XfrKeyPair>(&mut writer, &key)
            .map_err(|e| PlatformError::SerializationError(format!("[{}]: {:?}",&error_location!(),e)))?;
          Ok(key)
        }).map_err(add_location!())?
      }
      None => XfrKeyPair::generate(&mut prng),
    };

    let blocks = LedgerState::load_transaction_log(txn_path).map_err(add_location!())?;
    // dbg!(&blocks);
    let txn_log = (txn_path.into(),
                   std::fs::OpenOptions::new().append(true)
                                              .open(txn_path)
                                              .map_err(add_location!())?);
    // dbg!(&txn_log);
    let mut ledger =
      LedgerStateChecker(LedgerState { status: LedgerStatus::new(block_merkle_path,
                                                                 air_path,
                                                                 txn_merkle_path,
                                                                 txn_path,
                                                                 utxo_map_path).map_err(add_location!())?,
                                       prng,
                                       signing_key,
                                       block_merkle:
                                         LedgerState::init_merkle_log(block_merkle_path, false).map_err(add_location!())?,
                                       txn_merkle:
                                         LedgerState::init_merkle_log(txn_merkle_path, false).map_err(add_location!())?,
                                       blocks: Vec::new(),
                                       utxo_map: LedgerState::init_utxo_map(utxo_map_path,
                                                                            false).map_err(add_location!())?,
                                       txn_log: None,
                                       block_ctx: Some(BlockEffect::new()) });

    // dbg!(blocks.len());
    for (ix, logged_block) in blocks.into_iter().enumerate() {
      // dbg!(&ix);
      // dbg!(&logged_block);
      let (comm, block) = (logged_block.state, logged_block.block);
      let prev_commitment = HashOf::new(&ledger.0.status.state_commitment_data);
      if prev_commitment != comm.previous_state_commitment {
        return Err(PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                             std::file!(),
                                                             std::line!(),
                                                             std::column!())));
      }

      ledger.0.status.txns_in_block_hash = Some(comm.txns_in_block_hash.clone());
      ledger.0
            .status
            .state_commitment_versions
            .push(comm.compute_commitment());
      ledger.0.status.pulse_count = comm.pulse_count;
      ledger.0.status.incr_block_commit_count();
      ledger.0.status.state_commitment_data = Some(comm);

      let mut block_builder = ledger.start_block().unwrap();
      for txn in block {
        let eff = TxnEffect::compute_effect(txn).map_err(|e| {
                                                  std::io::Error::new(std::io::ErrorKind::Other, e)
                                                })
                                                .map_err(add_location!())?;
        ledger.apply_transaction(&mut block_builder, eff)
              .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
              .map_err(add_location!())?;
      }
      ledger = ledger.check_block(ix as u64, &block_builder)
                     .map_err(add_location!())?;
      ledger.finish_block(block_builder).unwrap();

      ledger.0.fast_invariant_check().map_err(add_location!())?;
    }

    ledger.0.txn_log = Some(txn_log);

    ledger.finish_check()
  }

  pub fn load_from_log(block_merkle_path: &str,
                       air_path: &str,
                       txn_merkle_path: &str,
                       txn_path: &str,
                       utxo_map_path: &str,
                       signing_key_path: Option<&str>,
                       prng_seed: Option<[u8; 32]>)
                       -> Result<LedgerState, PlatformError> {
    let mut prng = prng_seed.map(rand_chacha::ChaChaRng::from_seed)
                            .unwrap_or_else(ChaChaRng::from_entropy);
    let signing_key = match signing_key_path {
      Some(path) => {
        let ret = File::open(path).and_then(|file| {
                    let mut reader = BufReader::new(file);
                    serde_json::from_reader::<&mut BufReader<File>, XfrKeyPair>(&mut reader)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other,e))
                  });
        ret.or_else::<PlatformError, _>(|_| {
             let key = XfrKeyPair::generate(&mut prng);
             File::create(path).and_then(|file| {
               let mut writer = BufWriter::new(file);

               serde_json::to_writer::<&mut BufWriter<File>, XfrKeyPair>(&mut writer, &key)
              .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other,e))
              .map(|_| key)
             })
             .map_err(|e| {
               PlatformError::SerializationError(format!("[{}]: {:?}", &error_location!(), e))
             })
           })
           .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
           .map_err(add_location!())?
      }
      None => XfrKeyPair::generate(&mut prng),
    };

    let blocks = LedgerState::load_transaction_log(txn_path).map_err(add_location!())?;
    let txn_log = (txn_path.into(),
                   std::fs::OpenOptions::new().append(true)
                                              .open(txn_path)
                                              .map_err(add_location!())?);
    let mut ledger =
      LedgerState { status: LedgerStatus::new(block_merkle_path,
                                              air_path,
                                              txn_merkle_path,
                                              txn_path,
                                              utxo_map_path).map_err(add_location!())?,
                    prng,
                    signing_key,
                    block_merkle: LedgerState::init_merkle_log(block_merkle_path, true).map_err(add_location!())?,
                    txn_merkle: LedgerState::init_merkle_log(txn_merkle_path, true).map_err(add_location!())?,
                    blocks: Vec::new(),
                    utxo_map: LedgerState::init_utxo_map(utxo_map_path, true).map_err(add_location!())?,
                    txn_log: None,
                    block_ctx: Some(BlockEffect::new()) };

    for logged_block in blocks.into_iter() {
      let block = logged_block.block;
      let mut block_builder = ledger.start_block().unwrap();
      for txn in block {
        let eff = TxnEffect::compute_effect(txn).map_err(|e| {
                                                  std::io::Error::new(std::io::ErrorKind::Other, e)
                                                })
                                                .map_err(add_location!())?;
        ledger.apply_transaction(&mut block_builder, eff)
              .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
              .map_err(add_location!())?;
      }
      ledger.status.pulse_count = logged_block.state.pulse_count;
      ledger.finish_block(block_builder).unwrap();
    }

    ledger.txn_log = Some(txn_log);
    ledger.fast_invariant_check().map_err(add_location!())?;

    Ok(ledger)
  }

  pub fn load_or_init(base_dir: &Path) -> Result<LedgerState, PlatformError> {
    let block_buf = base_dir.join("block_merkle");
    let block_merkle = block_buf.to_str().unwrap();

    let air_buf = base_dir.join("air");
    let air = air_buf.to_str().unwrap();

    let txn_merkle_buf = base_dir.join("txn_merkle");
    let txn_merkle = txn_merkle_buf.to_str().unwrap();

    let txn_log_buf = base_dir.join("txn_log");
    let txn_log = txn_log_buf.to_str().unwrap();

    let utxo_map_buf = base_dir.join("utxo_map");
    let utxo_map = utxo_map_buf.to_str().unwrap();

    let sig_key_file_buf = base_dir.join("sig_key");
    let sig_key_file = sig_key_file_buf.to_str().unwrap();

    // TODO(joe): distinguish between the transaction log not existing
    // and it being corrupted
    LedgerState::load_from_log(&block_merkle, &air, &txn_merkle, &txn_log,
                &utxo_map, Some(sig_key_file), None)
    .or_else(|e| {
        log::info!("Replaying without merkle trees failed: {}",e);
        LedgerState::load_checked_from_log(&block_merkle, &air, &txn_merkle, &txn_log,
                &utxo_map, Some(sig_key_file), None)
    }).or_else(|e| {
        log::info!("Checking log against merkle trees failed: {}",e);
        let ret = LedgerState::new(&block_merkle, &air, &txn_merkle, &txn_log,
            &utxo_map, None, None).map_err(add_location!())?;

        {
            let file = File::create(sig_key_file).map_err(add_location!())?;
            let mut writer = BufWriter::new(file);

            serde_json::to_writer::<&mut BufWriter<File>, XfrKeyPair>(&mut writer, &ret.signing_key)
              .map_err(|e| PlatformError::SerializationError(format!("[{}]: {:?}",&error_location!(),e)))?;
        }

        Ok(ret)
    })
  }

  // Load a ledger given the paths to the various storage elements.
  #[allow(unused_variables)]
  pub fn load_from_snapshot(block_merkle_path: &str,
                            air_path: &str,
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
    // let status      = serde_json::from_reader
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
    // TODO: START https://github.com/findoraorg/platform/issues/307
    // let writer = LedgerState::create_merkle_log(self.status.block_merkle_path.clone(), state)?;
    // self.block_merkle.snapshot(writer)?;
    // TODO: END This is being disabled as we decide what to do about about logging, archival, etc
    Ok(SnapshotId { id: state })
  }

  // Snapshot the ledger state.  This involves synchronizing
  // the durable data structures to the disk and starting a
  // new log file for the logged Merkle tree.
  //
  // TODO(joe): Actually serialize the active ledger state.
  pub fn snapshot_txns(&mut self) -> Result<SnapshotId, std::io::Error> {
    let state = self.txn_merkle.state();
    // TODO: START https://github.com/findoraorg/platform/issues/307
    // let writer = LedgerState::create_merkle_log(self.status.txn_merkle_path.clone(), state)?;
    // self.txn_merkle.snapshot(writer)?;
    // TODO: END This is being disabled as we decide what to do about about logging, archival, etc

    Ok(SnapshotId { id: state })
  }

  // pub fn begin_commit(&mut self) {
  //   self.txn_base_sid.0 = self.max_applied_sid.0 + 1;
  // }
  //

  pub fn checkpoint(&mut self, block: &BlockEffect) -> u64 {
    self.save_utxo_map_version();
    let merkle_id = self.compute_and_append_txns_hash(&block);
    self.compute_and_save_state_commitment_data();
    self.utxo_map.write().unwrap();
    self.txn_merkle.write().unwrap();
    self.block_merkle.write().unwrap();
    // TODO: START https://github.com/findoraorg/platform/issues/307
    // self.txn_merkle.flush().unwrap();
    // self.block_merkle.flush().unwrap();
    // TODO: END This is being disabled as we decide what to do about about logging, archival, etc
    merkle_id
  }

  // Create a file structure for a Merkle tree log.
  // Mostly just make a path of the form:
  //
  //     <tree_path>-log-<Merkle tree state>
  //
  /* TODO: Leaving this code here while https://github.com/findoraorg/platform/issues/307 gets worked out
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
  */

  pub fn get_pulse_count(&self) -> u64 {
    self.status.pulse_count
  }
}

impl LedgerStatus {
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
  fn get_utxo(&mut self, addr: TxoSID) -> Option<AuthenticatedUtxo> {
    let utxo = self.status.get_utxo(addr);
    if let Some(utxo) = utxo.cloned() {
      let txn_location = *self.status.txo_to_txn_location.get(&addr).unwrap();
      let authenticated_txn = self.get_transaction(txn_location.0).unwrap();
      let authenticated_spent_status = self.get_utxo_status(addr);
      let state_commitment_data = self.status.state_commitment_data.as_ref().unwrap().clone();
      let utxo_location = txn_location.1;
      Some(AuthenticatedUtxo { utxo,
                               authenticated_txn,
                               authenticated_spent_status,
                               state_commitment_data,
                               utxo_location })
    } else {
      None
    }
  }

  fn get_issuance_num(&self, code: &AssetTypeCode) -> Option<u64> {
    self.status.get_issuance_num(code)
  }

  fn get_asset_type(&self, code: &AssetTypeCode) -> Option<&AssetType> {
    self.status.get_asset_type(code)
  }

  fn get_block_commit_count(&self) -> u64 {
    self.status.block_commit_count
  }

  fn get_no_replay_token(&mut self) -> NoReplayToken {
    NoReplayToken::new(&mut self.prng, self.status.block_commit_count)
  }

  fn get_state_commitment(&self) -> (HashOf<Option<StateCommitmentData>>, u64) {
    let block_count = self.status.block_commit_count;
    let commitment = self.status
                         .state_commitment_versions
                         .last()
                         .cloned()
                         .unwrap_or_else(|| HashOf::new(&None));
    (commitment, block_count)
  }

  fn public_key(&self) -> &XfrPublicKey {
    self.signing_key.get_pk_ref()
  }

  fn sign_message<T: Serialize + serde::de::DeserializeOwned>(&self, msg: &T) -> SignatureOf<T> {
    SignatureOf::new(&self.signing_key, msg)
  }

  fn get_utxo_status(&mut self, addr: TxoSID) -> AuthenticatedUtxoStatus {
    let state_commitment_data = self.status.state_commitment_data.as_ref().unwrap();
    let utxo_map_bytes: Option<SparseMapBytes>;
    let status;
    if addr.0 < state_commitment_data.txo_count {
      utxo_map_bytes = Some(self.utxo_map.serialize(0));
      let utxo_map = SparseMap::new(&utxo_map_bytes.as_ref().unwrap().clone()).unwrap();
      status = if utxo_map.query(addr.0).unwrap() {
        UtxoStatus::Unspent
      } else {
        UtxoStatus::Spent
      };
    } else {
      status = UtxoStatus::Nonexistent;
      utxo_map_bytes = None;
    }

    AuthenticatedUtxoStatus { status,
                              state_commitment_data: state_commitment_data.clone(),
                              state_commitment: state_commitment_data.compute_commitment(),
                              utxo_sid: addr,
                              utxo_map_bytes }
  }

  fn get_kv_entry(&self, addr: Key) -> AuthenticatedKVLookup {
    let (result, proof) = self.status.custom_data.get_with_proof(&addr);
    AuthenticatedKVLookup { key: addr,
                            result: result.cloned(),
                            state_commitment_data: self.status.state_commitment_data.clone(),
                            merkle_root: *self.status.custom_data.merkle_root(),
                            merkle_proof: proof,
                            state_commitment: self.get_state_commitment().0 }
  }
}

impl ArchiveAccess for LedgerState {
  fn get_transaction(&self, addr: TxnSID) -> Option<AuthenticatedTransaction> {
    let mut ix: usize = addr.0;
    for b in self.blocks.iter() {
      match b.txns.get(ix) {
        None => {
          debug_assert!(ix >= b.txns.len());
          ix -= b.txns.len();
        }
        v => {
          // Unwrap is safe because if transaction is on ledger there must be a state commitment
          let state_commitment_data = self.status.state_commitment_data.as_ref().unwrap().clone();
          let merkle = &self.txn_merkle;
          // TODO log error and recover?
          let proof = ProofOf::new(merkle.get_proof(v.unwrap().merkle_id, 0).unwrap());
          return Some(AuthenticatedTransaction { finalized_txn: v.unwrap().clone(),
                                                 txn_inclusion_proof: proof,
                                                 state_commitment_data:
                                                   state_commitment_data.clone(),
                                                 state_commitment:
                                                   state_commitment_data.compute_commitment() });
        }
      }
    }
    None
  }
  fn get_block(&self, addr: BlockSID) -> Option<AuthenticatedBlock> {
    match self.blocks.get(addr.0) {
      None => None,
      Some(finalized_block) => {
        debug_assert_eq!(addr.0 as u64, finalized_block.merkle_id);
        let block_inclusion_proof = ProofOf::new(self.block_merkle
                                                     .get_proof(finalized_block.merkle_id, 0)
                                                     .unwrap());
        let state_commitment_data = self.status.state_commitment_data.as_ref().unwrap().clone();
        Some(AuthenticatedBlock { block: finalized_block.clone(),
                                  block_inclusion_proof,
                                  state_commitment_data: state_commitment_data.clone(),
                                  state_commitment: state_commitment_data.compute_commitment() })
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
    for pair in self.status.utxo_map_versions.iter() {
      if (pair.0).0 as u64 == version {
        return Some(pair.1);
      }
    }

    None
  }

  fn get_state_commitment_at_block_height(&self,
                                          block_height: u64)
                                          -> Option<HashOf<Option<StateCommitmentData>>> {
    self.status
        .state_commitment_versions
        .get((block_height - 1) as usize)
        .cloned()
  }

  fn get_air_data(&self, key: &str) -> AuthenticatedAIRResult {
    let merkle_root = self.status.air.merkle_root();
    let (value, merkle_proof) = self.status.air.get_with_proof(key);
    let air_result = AIRResult { merkle_root: *merkle_root,
                                 key: key.to_string(),
                                 value: value.map(|s| s.to_string()),
                                 merkle_proof };
    AuthenticatedAIRResult { air_result,
                             state_commitment_data: self.status.state_commitment_data.clone(),
                             state_commitment: self.get_state_commitment().0 }
  }
}

pub mod helpers {
  use super::*;
  use crate::data_model::{
    Asset, AssetRules, ConfidentialMemo, DefineAsset, DefineAssetBody, IssuerPublicKey, Memo,
  };
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::AssetRecordType;
  use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record};
  use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
  use zei::xfr::structs::{AssetRecord, AssetRecordTemplate};

  pub fn create_definition_transaction(code: &AssetTypeCode,
                                       keypair: &XfrKeyPair,
                                       asset_rules: AssetRules,
                                       memo: Option<Memo>,
                                       no_replay_token: NoReplayToken)
                                       -> Result<Transaction, PlatformError> {
    let issuer_key = IssuerPublicKey { key: *keypair.get_pk_ref() };
    let asset_body =
      DefineAssetBody::new(&code, &issuer_key, asset_rules, memo, None, None).map_err(add_location!())?;
    let asset_create =
      DefineAsset::new(asset_body, &IssuerKeyPair { keypair: &keypair }).map_err(add_location!())?;
    Ok(Transaction::from_operation(Operation::DefineAsset(asset_create), no_replay_token))
  }

  pub fn build_keys<R: CryptoRng + RngCore>(prng: &mut R) -> XfrKeyPair {
    XfrKeyPair::generate(prng)
  }

  pub fn asset_creation_body(token_code: &AssetTypeCode,
                             issuer_key: &XfrPublicKey,
                             asset_rules: AssetRules,
                             memo: Option<Memo>,
                             confidential_memo: Option<ConfidentialMemo>)
                             -> DefineAssetBody {
    let mut token_properties: Asset = Default::default();
    token_properties.code = *token_code;
    token_properties.issuer = IssuerPublicKey { key: *issuer_key };
    token_properties.asset_rules = asset_rules;

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
                                  iss_key: &XfrKeyPair)
                                  -> DefineAsset {
    let signature = SignatureOf::new(iss_key, asset_body);
    DefineAsset { body: asset_body.clone(),
                  pubkey: IssuerPublicKey { key: *iss_key.get_pk_ref() },
                  signature }
  }

  pub fn apply_transaction(ledger: &mut LedgerState, tx: Transaction) -> (TxnSID, Vec<TxoSID>) {
    let effect = TxnEffect::compute_effect(tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();
    ledger.finish_block(block)
          .unwrap()
          .remove(&temp_sid)
          .unwrap()
  }

  #[allow(clippy::too_many_arguments)]
  pub fn create_issue_and_transfer_txn(ledger: &mut LedgerState,
                                       params: &PublicParams,
                                       code: &AssetTypeCode,
                                       amount: u64,
                                       issuer_keys: &XfrKeyPair,
                                       recipient_pk: &XfrPublicKey,
                                       seq_num: u64,
                                       no_replay_token: NoReplayToken)
                                       -> (Transaction, AssetRecord) {
    // issue operation
    let ar_template = AssetRecordTemplate::with_no_asset_tracking(amount, code.val, AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType, issuer_keys.get_pk());
    let (ba, _tracer_memo, owner_memo) =
      build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &ar_template, vec![]);

    let asset_issuance_body =
      IssueAssetBody::new(&code, seq_num, &[(TxOutput(ba.clone()), None)]).unwrap();
    let asset_issuance_operation =
      IssueAsset::new(asset_issuance_body,
                      &IssuerKeyPair { keypair: &issuer_keys }).unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    // transfer operation
    let ar_template = AssetRecordTemplate::with_no_asset_tracking(amount, code.val, AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType, *recipient_pk);
    let ar =
      AssetRecord::from_template_no_identity_tracking(ledger.get_prng(), &ar_template).unwrap();
    let mut transfer =
      TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                                                vec![TxoRef::Relative(0)],
                                                &[AssetRecord::from_open_asset_record_no_asset_tracking(open_blind_asset_record(&ba, &owner_memo, &issuer_keys.get_sk_ref()).unwrap())],
                                                &[ar.clone()],
                                                None,
                                                TransferType::Standard).unwrap()
                         ).unwrap();

    transfer.sign(&issuer_keys);
    let mut tx = Transaction::from_operation(issue_op, no_replay_token);
    tx.add_operation(Operation::TransferAsset(transfer));
    (tx, ar)
  }

  #[allow(clippy::too_many_arguments)]
  pub fn create_issue_and_transfer_txn_with_asset_tracing(ledger: &mut LedgerState,
                                                          params: &PublicParams,
                                                          code: &AssetTypeCode,
                                                          amount: u64,
                                                          issuer_keys: &XfrKeyPair,
                                                          recipient_pk: &XfrPublicKey,
                                                          seq_num: u64,
                                                          tracing_policy: AssetTracingPolicy)
                                                          -> (Transaction, AssetRecord) {
    let tracing_policies = AssetTracingPolicies::from_policy(tracing_policy);
    let xfr_note_policies = XfrNotePolicies::new(vec![tracing_policies.clone()],
                                                 vec![None],
                                                 vec![tracing_policies.clone()],
                                                 vec![None]);
    // issue operation
    let ar_template = AssetRecordTemplate::with_asset_tracking(amount, code.val, AssetRecordType::ConfidentialAmount_NonConfidentialAssetType, issuer_keys.get_pk(), tracing_policies.clone());
    let (ba, _tracer_memo, owner_memo) =
      build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &ar_template, vec![None]);

    let asset_issuance_body =
      IssueAssetBody::new(&code, seq_num, &[(TxOutput(ba.clone()), None)]).unwrap();
    let asset_issuance_operation =
      IssueAsset::new(asset_issuance_body,
                      &IssuerKeyPair { keypair: &issuer_keys }).unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    // transfer operation
    let ar_template = AssetRecordTemplate::with_asset_tracking(amount, code.val, AssetRecordType::ConfidentialAmount_NonConfidentialAssetType, *recipient_pk, tracing_policies.clone());
    let ar =
      AssetRecord::from_template_no_identity_tracking(ledger.get_prng(), &ar_template).unwrap();
    let mut transfer =
TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
             vec![TxoRef::Relative(0)],
             &[AssetRecord::from_open_asset_record_with_asset_tracking_but_no_identity(open_blind_asset_record(&ba, &owner_memo, &issuer_keys.get_sk_ref()).unwrap(), tracing_policies).unwrap()],
             &[ar.clone()],
             Some(xfr_note_policies), TransferType::Standard).unwrap()
).unwrap();

    transfer.sign(&issuer_keys);
    let mut tx = Transaction::from_operation(issue_op, seq_num);
    tx.add_operation(Operation::TransferAsset(transfer));
    (tx, ar)
  }

  pub fn create_issuance_txn(ledger: &mut LedgerState,
                             params: &PublicParams,
                             code: &AssetTypeCode,
                             amount: u64,
                             seq_num: u64,
                             record_type: AssetRecordType,
                             issuer_keys: &XfrKeyPair)
                             -> Transaction {
    // issue operation
    let ar_template = AssetRecordTemplate::with_no_asset_tracking(amount,
                                                                  code.val,
                                                                  record_type,
                                                                  issuer_keys.get_pk());
    let (ba, _tracer_memo, _owner_memo) =
      build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &ar_template, vec![]);

    let asset_issuance_body = IssueAssetBody::new(&code, seq_num, &[(TxOutput(ba), None)]).unwrap();
    let asset_issuance_operation =
      IssueAsset::new(asset_issuance_body,
                      &IssuerKeyPair { keypair: &issuer_keys }).unwrap();

    Transaction::from_operation(Operation::IssueAsset(asset_issuance_operation), NoReplayToken::default())
  }
}

#[cfg(test)]
mod tests {
  use super::helpers::*;
  use super::*;
  use crate::policies::{calculate_fee, Fraction};
  use credentials::{
    credential_commit, credential_issuer_key_gen, credential_sign, credential_user_key_gen,
    Credential,
  };
  use rand_core::SeedableRng;
  use tempfile::tempdir;
  use zei::serialization::ZeiFromToBytes;
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::{
    build_blind_asset_record, open_blind_asset_record, AssetRecordType,
  };
  use zei::xfr::asset_tracer::gen_asset_tracer_keypair;
  use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
  use zei::xfr::structs::{AssetRecord, AssetRecordTemplate};

  #[test]
  fn test_load_fake_transaction_log() {
    // Verify that loading transaction fails with incorrect path
    let result_err = LedgerState::load_transaction_log("incorrect/path");
    assert!(result_err.is_err());
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
    let mut data =
      StateCommitmentData { bitmap: ledger_state.utxo_map.compute_checksum(),
                            block_merkle: ledger_state.block_merkle.get_root_hash(),
                            txns_in_block_hash: HashOf::new(&vec![]),
                            previous_state_commitment: HashOf::new(&None),
                            transaction_merkle_commitment: ledger_state.txn_merkle
                                                                       .get_root_hash(),
                            air_commitment: *ledger_state.status.air.merkle_root(),
                            kv_store: *ledger_state.status.custom_data.merkle_root(),
                            txo_count: 0,
                            pulse_count: 0 };

    // dbg!(&data);
    let count_original = ledger_state.status.block_commit_count;

    let b = ledger_state.start_block().unwrap();
    ledger_state.finish_block(b).unwrap();
    data.block_merkle = ledger_state.block_merkle.get_root_hash();

    let first_hash = data.compute_commitment();

    // dbg!(&ledger_state.status.state_commitment_data);

    assert_eq!(ledger_state.status
                           .state_commitment_data
                           .clone()
                           .unwrap()
                           .compute_commitment(),
               first_hash);
    assert_eq!(ledger_state.get_state_commitment_at_block_height(1)
                           .unwrap(),
               first_hash);
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

    let digest = BitDigest { 0: [0_u8; 32] };
    ledger_state.status.utxo_map_versions = vec![(TxnSID(0), digest); MAX_VERSION - 1].into_iter()
                                                                                      .collect();

    // Verify that checkpoint increases the size of utxo_map_versions by 1 if its length < MAX_VERSION
    ledger_state.checkpoint(&BlockEffect::new());
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION);

    let count_original = ledger_state.status.block_commit_count;
    let (commitment1, v1) = ledger_state.get_state_commitment();

    // Verify that end_commit doesn't change the size of utxo_map_versions if its length >= MAX_VERSION
    ledger_state.status
                .utxo_map_versions
                .push_back((TxnSID(0), digest));
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION + 1);
    ledger_state.checkpoint(&BlockEffect::new());
    assert_eq!(ledger_state.status.utxo_map_versions.len(), MAX_VERSION + 1);
    let (commitment2, v2) = ledger_state.get_state_commitment();

    // Verify that the element pushed to the back is as expected
    let back = ledger_state.status.utxo_map_versions.get(MAX_VERSION);
    assert_eq!(back,
               Some(&(ledger_state.status.next_txn, ledger_state.utxo_map.compute_checksum())));

    // Verify that the status is saved as expected
    assert_eq!(ledger_state.status.txns_in_block_hash.clone().unwrap(),
               BlockEffect::new().compute_txns_in_block_hash());
    assert_eq!(ledger_state.status.block_commit_count, count_original + 1);
    // Check state commitment history
    assert_eq!(ledger_state.get_state_commitment_at_block_height(v1)
                           .unwrap(),
               commitment1);
    assert_eq!(ledger_state.get_state_commitment_at_block_height(v2)
                           .unwrap(),
               commitment2);
  }

  /*
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
  */

  #[test]
  fn test_asset_creation_valid() {
    let mut prng = ChaChaRng::from_entropy();
    let mut state = LedgerState::test_ledger();

    let token_code1 = AssetTypeCode::from_identical_byte(1);
    let keypair = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1,
                                         keypair.get_pk_ref(),
                                         AssetRules::default(),
                                         None,
                                         None);
    let asset_create = asset_creation_operation(&asset_body, &keypair);
    let tx = Transaction::from_operation(Operation::DefineAsset(asset_create),
                                         state.get_no_replay_token());

    let effect = TxnEffect::compute_effect(tx).unwrap();
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

  #[test]
  fn test_kv_store() {
    let mut prng = ChaChaRng::from_entropy();
    let mut ledger = LedgerState::test_ledger();
    let kp1 = XfrKeyPair::generate(&mut prng);
    let kp2 = XfrKeyPair::generate(&mut prng);

    let data1 = [0u8, 16];

    let key1 = Key::gen_random(&mut prng);

    let hash = KVHash::new(&data1, None);
    let update = KVUpdate::new((key1, Some(hash)), 0, &kp1);
    let tx = Transaction::from_operation(Operation::KVStoreUpdate(update.clone()),
                                         ledger.get_no_replay_token());

    {
      let effect = TxnEffect::compute_effect(tx).unwrap();
      let mut block = ledger.start_block().unwrap();
      ledger.apply_transaction(&mut block, effect).unwrap();
      ledger.finish_block(block).unwrap();
    }

    let auth_entry = ledger.get_kv_entry(key1);
    assert!(auth_entry.is_valid(ledger.get_state_commitment().0));

    let entry = auth_entry.result.unwrap().deserialize().1;
    assert!(&entry == update.get_entry());

    // Assert that nobody else can update that key and that reply isn't possible
    let bad_seq_update = KVUpdate::new((key1, None), 0, &kp1);
    let bad_seq_tx = Transaction::from_operation(Operation::KVStoreUpdate(bad_seq_update.clone()),
                                                 ledger.get_no_replay_token());
    let wrong_key_update = KVUpdate::new((key1, None), 1, &kp2);
    let wrong_key_tx =
      Transaction::from_operation(Operation::KVStoreUpdate(wrong_key_update.clone()),
                                  ledger.get_no_replay_token());

    let mut block = ledger.start_block().unwrap();
    {
      let effect = TxnEffect::compute_effect(bad_seq_tx).unwrap();
      let res = ledger.apply_transaction(&mut block, effect);
      assert!(res.is_err());

      let effect = TxnEffect::compute_effect(wrong_key_tx).unwrap();
      let res = ledger.apply_transaction(&mut block, effect);
      assert!(res.is_err());
    }

    // Now update, this time with a blind
    let data2 = [0u8, 16];
    let hash = KVHash::new(&data2, Some(&KVBlind::gen_random()));
    let update = KVUpdate::new((key1, Some(hash)), 1, &kp1);
    let tx = Transaction::from_operation(Operation::KVStoreUpdate(update.clone()),
                                         ledger.get_no_replay_token());

    {
      let effect = TxnEffect::compute_effect(tx).unwrap();
      ledger.apply_transaction(&mut block, effect).unwrap();
      ledger.finish_block(block).unwrap();
    }

    let auth_entry = ledger.get_kv_entry(key1);
    assert!(auth_entry.is_valid(ledger.get_state_commitment().0));

    let entry = auth_entry.result.unwrap().deserialize().1;
    assert!(&entry == update.get_entry());
  }

  // Change the signature to have the wrong public key
  #[test]
  fn test_asset_creation_invalid_public_key() {
    // Create a valid asset creation operation.
    let token_code1 = AssetTypeCode::from_identical_byte(1);
    let mut prng = ChaChaRng::from_entropy();
    let keypair = build_keys(&mut prng);
    let asset_body = asset_creation_body(&token_code1,
                                         keypair.get_pk_ref(),
                                         AssetRules::default(),
                                         None,
                                         None);
    let mut asset_create = asset_creation_operation(&asset_body, &keypair);

    // Now re-sign the operation with the wrong key.
    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let keypair = build_keys(&mut prng);

    asset_create.pubkey.key = *keypair.get_pk_ref();
    let tx = Transaction::from_operation(Operation::DefineAsset(asset_create),
                                         NoReplayToken::default());

    assert!(TxnEffect::compute_effect(tx).is_err());
  }

  #[test]
  fn test_asset_transfer() {
    let mut ledger = LedgerState::test_ledger();
    let params = PublicParams::new();

    let code = AssetTypeCode::from_identical_byte(1);
    let mut prng = ChaChaRng::from_entropy();
    let key_pair = XfrKeyPair::generate(&mut prng);
    let key_pair_adversary = XfrKeyPair::generate(ledger.get_prng());

    let tx = create_definition_transaction(&code,
                                           &key_pair,
                                           AssetRules::default(),
                                           None,
                                           ledger.get_no_replay_token()).unwrap();

    let effect = TxnEffect::compute_effect(tx).unwrap();
    {
      let mut block = ledger.start_block().unwrap();
      ledger.apply_transaction(&mut block, effect).unwrap();
      ledger.finish_block(block).unwrap();
    }

    // Issuance with two outputs
    let art = AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
    let template =
      AssetRecordTemplate::with_no_asset_tracking(100, code.val, art, key_pair.get_pk());
    let (ba, _, _) =
      build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &template, vec![]);
    let second_ba = ba.clone();

    let asset_issuance_body = IssueAssetBody::new(&code,
                                                  0,
                                                  &[(TxOutput(ba), None),
                                                    (TxOutput(second_ba), None)]).unwrap();
    let asset_issuance_operation =
      IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &key_pair }).unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    let tx = Transaction::from_operation(issue_op, ledger.get_no_replay_token());

    // Commit issuance to block
    let effect = TxnEffect::compute_effect(tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (_txn_sid, txos) = ledger.finish_block(block)
                                 .unwrap()
                                 .remove(&temp_sid)
                                 .unwrap();
    let state_commitment = ledger.get_state_commitment().0;

    for txo_id in &txos {
      assert!(ledger.status.utxos.contains_key(&txo_id));
      let utxo_status = ledger.get_utxo_status(*txo_id);
      assert!(utxo_status.is_valid(state_commitment.clone()));
      assert!(utxo_status.status == UtxoStatus::Unspent);
    }

    // Store txo_sids for subsequent transfers
    let txo_sid = txos[0];
    let second_txo_id = txos[1];

    // Construct transfer operation
    let input_bar_proof = ledger.get_utxo(txo_sid).unwrap();
    let input_bar = (input_bar_proof.clone().utxo.0).0;
    let input_oar = open_blind_asset_record(&input_bar, &None, &key_pair.get_sk_ref()).unwrap();
    assert!(input_bar_proof.is_valid(state_commitment.clone()));

    let output_template =
      AssetRecordTemplate::with_no_asset_tracking(100, code.val, art, key_pair_adversary.get_pk());
    let output_ar =
      AssetRecord::from_template_no_identity_tracking(ledger.get_prng(), &output_template).unwrap();
    let input_ar = AssetRecord::from_open_asset_record_no_asset_tracking(input_oar.clone());

    let mut transfer =
      TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                                                vec![TxoRef::Absolute(txo_sid)],
                                                &[input_ar],
                                                &[output_ar],
                                                None,
                                                TransferType::Standard).unwrap()).unwrap();

    let mut second_transfer = transfer.clone();
    transfer.sign(&key_pair);
    let tx = Transaction::from_operation(Operation::TransferAsset(transfer),
                                         ledger.get_no_replay_token());

    // Commit first transfer
    let effect = TxnEffect::compute_effect(tx).unwrap();
    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (_txn_sid, _txos) = ledger.finish_block(block)
                                  .unwrap()
                                  .remove(&temp_sid)
                                  .unwrap();
    // Ensure that previous txo is now spent
    let state_commitment = ledger.get_state_commitment().0;
    let utxo_status = ledger.get_utxo_status(TxoSID(0));
    assert!(utxo_status.is_valid(state_commitment.clone()));
    assert!(!input_bar_proof.is_valid(state_commitment));
    assert!(utxo_status.status == UtxoStatus::Spent);

    // Adversary will attempt to spend the same blind asset record at another index
    second_transfer.body.inputs = vec![TxoRef::Absolute(second_txo_id)];

    // Submit spend of same asset at second sid without signature
    second_transfer.body_signatures = Vec::new();
    let tx = Transaction::from_operation(Operation::TransferAsset(second_transfer),
                                         ledger.get_no_replay_token());

    let effect = TxnEffect::compute_effect(tx);
    assert!(effect.is_err());
  }

  // Sign with the wrong key.
  #[test]
  fn test_asset_creation_invalid_signature() {
    // Create a valid operation.
    let token_code1 = AssetTypeCode::from_identical_byte(1);

    let mut prng = ChaChaRng::from_entropy();
    let keypair1 = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1,
                                         keypair1.get_pk_ref(),
                                         AssetRules::default(),
                                         None,
                                         None);
    let mut asset_create = asset_creation_operation(&asset_body, &keypair1);

    // Re-sign the operation with the wrong key.
    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let keypair2 = build_keys(&mut prng);

    asset_create.pubkey.key = *keypair2.get_pk_ref();
    let tx = Transaction::from_operation(Operation::DefineAsset(asset_create),
                                         NoReplayToken::default()); // OK because no ledger interaction

    assert!(TxnEffect::compute_effect(tx).is_err());
  }

  #[test]
  fn asset_issued() {
    let mut ledger = LedgerState::test_ledger();

    let params = PublicParams::new();

    assert!(ledger.get_state_commitment() == (HashOf::new(&None), 0));
    let token_code1 = AssetTypeCode::from_identical_byte(1);
    let keypair = build_keys(&mut ledger.get_prng());

    let tx = create_definition_transaction(&token_code1,
                                           &keypair,
                                           AssetRules::default(),
                                           None,
                                           ledger.get_no_replay_token()).unwrap();

    let effect = TxnEffect::compute_effect(tx).unwrap();
    {
      let mut block = ledger.start_block().unwrap();
      ledger.apply_transaction(&mut block, effect).unwrap();
      ledger.finish_block(block).unwrap();
    }

    let art = AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
    let ar =
      AssetRecordTemplate::with_no_asset_tracking(100, token_code1.val, art, *keypair.get_pk_ref());

    let (ba, _, _) = build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &ar, vec![]);
    let asset_issuance_body =
      IssueAssetBody::new(&token_code1, 0, &[(TxOutput(ba), None)]).unwrap();
    let asset_issuance_operation =
      IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &keypair }).unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    let tx = Transaction::from_operation(issue_op, ledger.get_no_replay_token());
    let second_tx = tx.clone();

    let effect = TxnEffect::compute_effect(tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (txn_sid, txos) = ledger.finish_block(block)
                                .unwrap()
                                .remove(&temp_sid)
                                .unwrap();

    // shouldn't be able to replay issuance
    let effect = TxnEffect::compute_effect(second_tx).unwrap();
    let mut block = ledger.start_block().unwrap();
    let result = ledger.apply_transaction(&mut block, effect);
    assert!(result.is_err());
    ledger.abort_block(block);

    let transaction = ledger.get_transaction(txn_sid).unwrap();
    let txn_id = transaction.finalized_txn.tx_id;
    let state_commitment_and_version = ledger.get_state_commitment();

    println!("utxos = {:?}", ledger.status.utxos);
    for txo_id in txos {
      assert!(ledger.status.utxos.contains_key(&txo_id));
      let utxo_status = ledger.get_utxo_status(txo_id);
      assert!(utxo_status.is_valid(state_commitment_and_version.0.clone()));
      assert!(utxo_status.status == UtxoStatus::Unspent);
    }

    match ledger.get_block(BlockSID(0)) {
      Some(authenticated_block) => {
        assert!(authenticated_block.is_valid(state_commitment_and_version.0.clone()));
      }
      None => panic!("get_proof failed for block id 0"),
    }

    match ledger.get_transaction(txn_id) {
      Some(authenticated_txn) => {
        assert!(authenticated_txn.txn_inclusion_proof.0.proof.tx_id
                == authenticated_txn.finalized_txn.merkle_id);
        assert!(authenticated_txn.is_valid(state_commitment_and_version.0.clone()));
        assert!(transaction.finalized_txn == authenticated_txn.finalized_txn);
      }
      None => {
        panic!("get_proof failed for tx_id {}, merkle_id {}, block state {}, transaction state {}",
               transaction.finalized_txn.tx_id.0,
               transaction.finalized_txn.merkle_id,
               ledger.block_merkle.state(),
               ledger.txn_merkle.state());
      }
    }

    // We don't actually have anything to commmit yet,
    // but this will save the empty checksum, which is
    // enough for a bit of a test.
    assert!(&state_commitment_and_version
            == &(ledger.status
                       .state_commitment_data
                       .clone()
                       .unwrap()
                       .compute_commitment(),
                 2));
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
  /// Tests that a valid AIR credential can be appended to the AIR with the air_assign operation.
  pub fn test_air_assign_operation() {
    let mut ledger = LedgerState::test_ledger();
    let dl = String::from("dl");
    let cred_issuer_key = credential_issuer_key_gen(&mut ledger.get_prng(), &[(dl.clone(), 8)]);
    let cred_user_key = credential_user_key_gen(&mut ledger.get_prng(), &cred_issuer_key.0);
    let user_kp = XfrKeyPair::generate(&mut ledger.get_prng());

    // Construct credential
    let dl_attr = b"A1903479";
    let attr_map = vec![(dl.clone(), dl_attr.to_vec())];
    let attributes = [(dl.clone(), &dl_attr[..])];
    let signature = credential_sign(&mut ledger.get_prng(),
                                    &cred_issuer_key.1,
                                    &cred_user_key.0,
                                    &attributes).unwrap();
    let credential = Credential { signature: signature.clone(),
                                  attributes: attr_map,
                                  issuer_pub_key: cred_issuer_key.0.clone() };
    let (commitment, pok, _key) = credential_commit(&mut ledger.get_prng(),
                                                    &cred_user_key.1,
                                                    &credential,
                                                    user_kp.get_pk_ref().as_bytes()).unwrap();
    let no_replay_token = ledger.get_no_replay_token();
    let air_assign_op = AIRAssign::new(AIRAssignBody::new(cred_user_key.0.clone(),
                                                          commitment,
                                                          cred_issuer_key.0,
                                                          pok,
                                                          no_replay_token).unwrap(),
                                       &user_kp).unwrap();
    let mut adversarial_op = air_assign_op.clone();
    let air_assign_op2 = air_assign_op.clone();
    adversarial_op.pubkey = XfrKeyPair::generate(&mut ledger.get_prng()).get_pk();
    let tx = Transaction::from_operation(Operation::AIRAssign(air_assign_op),
                                         no_replay_token);
    apply_transaction(&mut ledger, tx);

    let tx2 = Transaction::from_operation(Operation::AIRAssign(air_assign_op2),
                                          no_replay_token);

    let effect = TxnEffect::compute_effect(tx2).unwrap();
    assert!(ledger.status.check_txn_effects(effect).is_err()); // This fails due to replay protection

    let tx = Transaction::from_operation(Operation::AIRAssign(adversarial_op),
                                         no_replay_token);
    let effect = TxnEffect::compute_effect(tx);
    assert!(effect.is_err());
    let authenticated_air_res =
      ledger.get_air_data(&serde_json::to_string(&cred_user_key.0).unwrap());
    assert!(authenticated_air_res.is_valid(ledger.get_state_commitment().0));
  }

  #[test]
  pub fn test_transferable() {
    let mut ledger = LedgerState::test_ledger();
    let params = PublicParams::new();
    let issuer = XfrKeyPair::generate(&mut ledger.get_prng());
    let alice = XfrKeyPair::generate(&mut ledger.get_prng());
    let bob = XfrKeyPair::generate(&mut ledger.get_prng());

    // Define fiat token
    let code = AssetTypeCode::from_identical_byte(1);
    let tx = create_definition_transaction(&code,
                                           &issuer,
                                           AssetRules::default().set_transferable(false).clone(),
                                           Some(Memo("test".to_string())),
                                           ledger.get_no_replay_token()).unwrap();
    apply_transaction(&mut ledger, tx);
    let no_replay_token = ledger.get_no_replay_token();
    let (tx, _) = create_issue_and_transfer_txn(&mut ledger,
                                                &params,
                                                &code,
                                                100,
                                                &issuer,
                                                alice.get_pk_ref(),
                                                0,
                                                no_replay_token);
    let (_, sids) = apply_transaction(&mut ledger, tx);
    let sid = sids[0];

    let bar = ((ledger.get_utxo(sid).unwrap().utxo.0).0).clone();

    let transfer_template= AssetRecordTemplate::with_no_asset_tracking(100,
                                                                             code.val,
                                                                             AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                                             bob.get_pk_ref().clone());
    let record = AssetRecord::from_template_no_identity_tracking(ledger.get_prng(),
                                                                 &transfer_template).unwrap();

    // Cant transfer non-transferable asset
    let mut transfer = TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                                                                 vec![TxoRef::Absolute(sid)],
                                                                 &[AssetRecord::from_open_asset_record_no_asset_tracking(open_blind_asset_record(&bar, &None, &alice.get_sk_ref()).unwrap())],
                                                                 &[record.clone()],
                                                                 None, TransferType::Standard).unwrap()
                                          ).unwrap();
    transfer.sign(&alice);
    let tx = Transaction::from_operation(Operation::TransferAsset(transfer), no_replay_token);
    let effect = TxnEffect::compute_effect(tx.clone()).unwrap();

    let mut block = ledger.start_block().unwrap();
    let res = ledger.apply_transaction(&mut block, effect);
    assert!(res.is_err());
    // Cant transfer by making asset confidential
    let transfer_template = AssetRecordTemplate::with_no_asset_tracking(100, code.val,
                                                                             AssetRecordType::ConfidentialAmount_ConfidentialAssetType,
                                                                             bob.get_pk_ref().clone());
    let record = AssetRecord::from_template_no_identity_tracking(ledger.get_prng(),
                                                                 &transfer_template).unwrap();

    // Cant transfer non-transferable asset
    let mut transfer = TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                             vec![TxoRef::Absolute(sid)],
                             &[AssetRecord::from_open_asset_record_no_asset_tracking(open_blind_asset_record(&bar, &None, &alice.get_sk_ref()).unwrap())],
                             &[record.clone()],
                             None, TransferType::Standard).unwrap()).unwrap();
    transfer.sign(&alice);
    let tx = Transaction::from_operation(Operation::TransferAsset(transfer), no_replay_token);
    let effect = TxnEffect::compute_effect(tx.clone()).unwrap();

    let res = ledger.apply_transaction(&mut block, effect);
    assert!(res.is_err());
    // Cant transfer non-transferable asset through some intermediate operation
    // In this case, alice attempts to spend her non-transferable asset in the same transaction it
    // was issued.
    let second_transfer_template= AssetRecordTemplate::with_no_asset_tracking(100,
                                                                              code.val,
                                                                              AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                                              bob.get_pk_ref().clone());
    let second_record = AssetRecord::from_template_no_identity_tracking(ledger.get_prng(),
                                                                        &second_transfer_template).unwrap();
    let no_replay_token = ledger.get_no_replay_token();
    let (mut tx, ar) = create_issue_and_transfer_txn(&mut ledger,
                                                     &params,
                                                     &code,
                                                     100,
                                                     &issuer,
                                                     alice.get_pk_ref(),
                                                     1,
                                                     no_replay_token);
    let mut transfer = TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                                                                 vec![TxoRef::Relative(0)],
                                                                 &[AssetRecord::from_open_asset_record_no_asset_tracking(ar.open_asset_record)],
                                                                 &[second_record],
                                                                 None,
                                          TransferType::Standard).unwrap()).unwrap();
    transfer.sign(&alice);
    tx.body.operations.push(Operation::TransferAsset(transfer));
    let effect = TxnEffect::compute_effect(tx).unwrap();
    let res = ledger.apply_transaction(&mut block, effect);
    assert!(res.is_err());
  }

  #[test]
  pub fn test_tracing_policy() {
    let mut ledger = LedgerState::test_ledger();
    let params = PublicParams::new();

    let issuer = XfrKeyPair::generate(&mut ledger.get_prng());
    let recipient = XfrKeyPair::generate(&mut ledger.get_prng());

    // Set tracing policies
    let tracer_kp = gen_asset_tracer_keypair(&mut ledger.get_prng());
    let tracing_policy = AssetTracingPolicy { enc_keys: tracer_kp.enc_key.clone(),
                                              asset_tracking: true,
                                              identity_tracking: None };
    let unmatched_tracing_policy = AssetTracingPolicy { enc_keys: tracer_kp.enc_key.clone(),
                                                        asset_tracking: false,
                                                        identity_tracking: None };

    // Define an asset without a tracing policy
    let code = AssetTypeCode::from_identical_byte(0);
    let tx = create_definition_transaction(&code,
                                           &issuer,
                                           AssetRules::default(),
                                           Some(Memo("test".to_string())),
                                           ledger.get_block_commit_count()).unwrap();
    apply_transaction(&mut ledger, tx);

    // Issue and transfer the asset without a tracing policy
    // Should succeed
    let (tx, _) = create_issue_and_transfer_txn(&mut ledger,
                                                &params,
                                                &code,
                                                100,
                                                &issuer,
                                                recipient.get_pk_ref(),
                                                0);
    apply_transaction(&mut ledger, tx);

    // Define an asset with the tracing policy
    let code = AssetTypeCode::from_identical_byte(1);
    let tx = create_definition_transaction(&code,
                                           &issuer,
                                           AssetRules::default().add_tracing_policy(tracing_policy.clone()).clone(),
                                           Some(Memo("test".to_string())),
                                           ledger.get_block_commit_count()).unwrap();
    apply_transaction(&mut ledger, tx);

    // Issue and transfer the asset without a tracing policy
    // Should fail
    let (tx, _) = create_issue_and_transfer_txn(&mut ledger,
                                                &params,
                                                &code,
                                                100,
                                                &issuer,
                                                recipient.get_pk_ref(),
                                                0);
    let mut block = ledger.start_block().unwrap();
    let effect = TxnEffect::compute_effect(tx.clone()).unwrap();
    let res = ledger.apply_transaction(&mut block, effect);
    assert!(res.is_err());

    // Issue and transfer the asset to with the unmatched tracing policy
    // Should fail
    let (tx, _) = create_issue_and_transfer_txn_with_asset_tracing(&mut ledger,
                                                                   &params,
                                                                   &code,
                                                                   100,
                                                                   &issuer,
                                                                   recipient.get_pk_ref(),
                                                                   0,
                                                                   unmatched_tracing_policy);
    let effect = TxnEffect::compute_effect(tx.clone()).unwrap();
    let res = ledger.apply_transaction(&mut block, effect);
    assert!(res.is_err());

    // Issue and transfer the asset with the correct tracing policy
    // Should pass
    let (tx, _) = create_issue_and_transfer_txn_with_asset_tracing(&mut ledger,
                                                                   &params,
                                                                   &code,
                                                                   100,
                                                                   &issuer,
                                                                   recipient.get_pk_ref(),
                                                                   0,
                                                                   tracing_policy);
    let effect = TxnEffect::compute_effect(tx.clone()).unwrap();
    let res = ledger.apply_transaction(&mut block, effect);
    // dbg!(&res);
    assert!(res.is_ok());
  }

  #[test]
  pub fn test_max_units() {
    let mut ledger = LedgerState::test_ledger();
    let params = PublicParams::new();

    let issuer = XfrKeyPair::generate(&mut ledger.get_prng());

    // Define fiat token
    let code = AssetTypeCode::from_identical_byte(1);
    let tx = create_definition_transaction(&code,
                                           &issuer,
                                           AssetRules::default().set_max_units(Some(100)).clone(),
                                           Some(Memo("test".to_string())),
                                           ledger.get_no_replay_token()).unwrap();
    apply_transaction(&mut ledger, tx);
    let tx = create_issuance_txn(&mut ledger,
                                 &params,
                                 &code,
                                 50,
                                 0,
                                 AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                 &issuer);
    apply_transaction(&mut ledger, tx);
    {
      // Ensure that a single overlfowing transaction fails
      let tx = create_issuance_txn(&mut ledger,
                                   &params,
                                   &code,
                                   51,
                                   1,
                                   AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                   &issuer);
      let effect = TxnEffect::compute_effect(tx).unwrap();

      let mut block = ledger.start_block().unwrap();
      let res = ledger.apply_transaction(&mut block, effect);
      assert!(res.is_err());

      // Ensure that cap can be reached
      let tx = create_issuance_txn(&mut ledger,
                                   &params,
                                   &code,
                                   50,
                                   1,
                                   AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                   &issuer);
      let effect = TxnEffect::compute_effect(tx).unwrap();
      ledger.apply_transaction(&mut block, effect).unwrap();
      ledger.finish_block(block).unwrap();

      // Cant try to exceed asset cap by issuing confidentially
      let tx = create_issuance_txn(&mut ledger,
                                   &params,
                                   &code,
                                   1,
                                   2,
                                   AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                   &issuer);
      let effect = TxnEffect::compute_effect(tx).unwrap();
      let mut block = ledger.start_block().unwrap();
      let res = ledger.apply_transaction(&mut block, effect);
      assert!(res.is_err());
    }
  }

  // Co_signers is a array of (signs, weight) pairs representing cosigners. If signs is true, that cosigner signs the
  // transaction.
  fn cosignature_transfer_succeeds(co_signers: &[(bool, u64)],
                                   threshold: u64,
                                   confidential: bool)
                                   -> bool {
    let mut ledger = LedgerState::test_ledger();
    let params = PublicParams::new();

    let code = AssetTypeCode::from_identical_byte(1);
    let mut prng = ChaChaRng::from_entropy();
    let keys: Vec<XfrKeyPair> = (0..co_signers.len()).map(|_| XfrKeyPair::generate(&mut prng))
                                                     .collect();
    let alice = XfrKeyPair::generate(&mut prng); // Asset owner
    let bob = XfrKeyPair::generate(&mut prng); // Asset recipient

    let sig_rules =
      SignatureRules { threshold,
                       weights: co_signers.iter()
                                          .zip(keys.iter())
                                          .map(|((_, weight), kp)| (*kp.get_pk_ref(), *weight))
                                          .collect() };

    let tx =
      create_definition_transaction(&code,
                                    &alice,
                                    AssetRules::default().set_transfer_multisig_rules(Some(sig_rules))
                                                         .clone(),
                                    None, ledger.get_no_replay_token()).unwrap();

    let effect = TxnEffect::compute_effect(tx).unwrap();
    {
      let mut block = ledger.start_block().unwrap();
      ledger.apply_transaction(&mut block, effect).unwrap();
      ledger.finish_block(block).unwrap();
    }

    // Issuance with two outputs
    let art = if let true = confidential {
      AssetRecordType::ConfidentialAmount_ConfidentialAssetType
    } else {
      AssetRecordType::NonConfidentialAmount_ConfidentialAssetType
    };
    let template = AssetRecordTemplate::with_no_asset_tracking(100, code.val, AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType, alice.get_pk());
    let (ba, _, _) =
      build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &template, vec![]);

    let asset_issuance_body = IssueAssetBody::new(&code, 0, &[(TxOutput(ba), None)]).unwrap();
    let asset_issuance_operation =
      IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &alice }).unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    let tx = Transaction::from_operation(issue_op, ledger.get_no_replay_token());

    // Commit issuance to block
    let effect = TxnEffect::compute_effect(tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (_txn_sid, txos) = ledger.finish_block(block)
                                 .unwrap()
                                 .remove(&temp_sid)
                                 .unwrap();
    let txo_sid = txos[0];

    // Construct transfer operation
    let mut block = ledger.start_block().unwrap();
    let input_bar = ((ledger.get_utxo(txo_sid).unwrap().utxo.0).0).clone();
    let input_oar = open_blind_asset_record(&input_bar, &None, &alice.get_sk_ref()).unwrap();

    let output_template =
      AssetRecordTemplate::with_no_asset_tracking(100, code.val, art, bob.get_pk());
    let output_ar =
      AssetRecord::from_template_no_identity_tracking(ledger.get_prng(), &output_template).unwrap();

    let mut transfer = TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                                                                 vec![TxoRef::Absolute(txo_sid)],
                                                                 &[AssetRecord::from_open_asset_record_no_asset_tracking(input_oar)],
                                                                 &[output_ar],
                                                                 None,
                                          TransferType::Standard).unwrap()).unwrap();

    transfer.sign(&alice);
    for (i, (signs, _)) in co_signers.iter().enumerate() {
      if *signs {
        transfer.sign_cosignature(&keys[i], 0);
      }
    }
    let tx = Transaction::from_operation(Operation::TransferAsset(transfer),
                                         ledger.get_no_replay_token());
    let effect = TxnEffect::compute_effect(tx).unwrap();
    ledger.apply_transaction(&mut block, effect).is_ok()
  }

  #[test]
  pub fn test_update_memo() {
    let mut ledger = LedgerState::test_ledger();
    let _params = PublicParams::new();

    let creator = XfrKeyPair::generate(&mut ledger.get_prng());
    let adversary = XfrKeyPair::generate(&mut ledger.get_prng());

    // Define fiat token
    let code = AssetTypeCode::from_identical_byte(1);
    let tx = create_definition_transaction(&code,
                                           &creator,
                                           AssetRules::default().set_updatable(true).clone(),
                                           Some(Memo("test".to_string())),
                                           ledger.get_no_replay_token()).unwrap();
    apply_transaction(&mut ledger, tx);
    let mut block = ledger.start_block().unwrap();
    let new_memo = Memo("new_memo".to_string());
    let mut memo_update = UpdateMemo::new(UpdateMemoBody { no_replay_token:
                                                             ledger.get_no_replay_token(),
                                                           new_memo: new_memo.clone(),
                                                           asset_type: code },
                                          &creator);
    // Ensure that invalid signature fails
    memo_update.pubkey = adversary.get_pk();
    let tx = Transaction::from_operation(Operation::UpdateMemo(memo_update.clone()),
                                         ledger.get_no_replay_token());
    assert!(TxnEffect::compute_effect(tx).is_err());

    // Only the asset creator can change the memo
    let no_replay_token = ledger.get_no_replay_token();
    let memo_update_wrong_creator = UpdateMemo::new(UpdateMemoBody { no_replay_token,
                                                                     new_memo: new_memo.clone(),
                                                                     asset_type: code },
                                                    &adversary);
    let tx = Transaction::from_operation(Operation::UpdateMemo(memo_update_wrong_creator),
                                         no_replay_token);
    let effect = TxnEffect::compute_effect(tx).unwrap();
    assert!(ledger.apply_transaction(&mut block, effect).is_err());

    // Cant change memo more than once in the same block
    memo_update.pubkey = creator.get_pk();
    let tx = Transaction::from_operation(Operation::UpdateMemo(memo_update.clone()),
                                         memo_update.body.no_replay_token);
    let tx_clone = tx.clone();
    let effect = TxnEffect::compute_effect(tx).unwrap();
    ledger.apply_transaction(&mut block, effect.clone())
      .unwrap();
    assert!(ledger.apply_transaction(&mut block, effect).is_err());
    ledger.finish_block(block).unwrap();

    // Ensure memo is updated
    assert!(ledger.get_asset_type(&code).unwrap().properties.memo == new_memo);

    // Test replay defense
    let mut block2 = ledger.start_block().unwrap();
    let effect2 = TxnEffect::compute_effect(tx_clone).unwrap();
    assert!(ledger.apply_transaction(&mut block2, effect2).is_err());
    ledger.finish_block(block2).unwrap();
  }

  #[test]
  pub fn test_cosignature_restrictions() {
    // Simple
    assert!(!cosignature_transfer_succeeds(&[(false, 1), (false, 1)], 1, false));
    assert!(!cosignature_transfer_succeeds(&[(false, 1), (false, 1)], 1, true));
    assert!(cosignature_transfer_succeeds(&[(false, 1), (true, 1)], 1, false));
    assert!(cosignature_transfer_succeeds(&[(true, 1)], 1, false));
    assert!(cosignature_transfer_succeeds(&[], 0, false));

    // More complex
    assert!(!cosignature_transfer_succeeds(&[(false, 1),
                                             (true, 1),
                                             (false, 5),
                                             (true, 10),
                                             (false, 18)],
                                           16,
                                           false));
    assert!(cosignature_transfer_succeeds(&[(false, 1),
                                            (true, 1),
                                            (true, 5),
                                            (true, 10),
                                            (false, 18)],
                                          16,
                                          false));
    // Needlessly complex
    assert!(cosignature_transfer_succeeds(&[(false, 18888888),
                                            (true, 1),
                                            (true, 5),
                                            (false, 12320),
                                            (true, 13220),
                                            (true, 100000),
                                            (true, 12320),
                                            (true, 134440),
                                            (false, 18)],
                                          232323,
                                          false));
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
    let fiat_code = AssetTypeCode::from_identical_byte(1);
    let tx = create_definition_transaction(&fiat_code,
                                           &fiat_issuer_key_pair,
                                           AssetRules::default(),
                                           Some(Memo("fiat".to_string())),
                                           ledger.get_no_replay_token()).unwrap();
    apply_transaction(&mut ledger, tx);

    // Define debt token
    let debt_code = AssetTypeCode::from_identical_byte(2);
    let debt_memo = DebtMemo { interest_rate,
                               fiat_code,
                               loan_amount: loan_amount as u64 };
    let tx = create_definition_transaction(&debt_code,
                                           &borrower_key_pair,
                                           AssetRules::default(),
                                           Some(Memo(serde_json::to_string(&debt_memo).unwrap())),
                                           ledger.get_no_replay_token()).unwrap();
    apply_transaction(&mut ledger, tx);

    // Issue and transfer fiat tokens to lender
    let no_replay_token = ledger.get_no_replay_token();
    let (tx, _) = create_issue_and_transfer_txn(&mut ledger,
                                                &params,
                                                &fiat_code,
                                                fiat_amount,
                                                &fiat_issuer_key_pair,
                                                lender_key_pair.get_pk_ref(),
                                                0,
                                                no_replay_token);

    let (_txn_sid, txo_sids) = apply_transaction(&mut ledger, tx);
    let fiat_sid = txo_sids[0];

    // Issue and transfer debt tokens to borrower
    let no_replay_token = ledger.get_no_replay_token();
    let (tx, _) = create_issue_and_transfer_txn(&mut ledger,
                                                &params,
                                                &debt_code,
                                                loan_amount,
                                                &borrower_key_pair,
                                                borrower_key_pair.get_pk_ref(),
                                                0,
                                                no_replay_token);
    let (_txn_sid, txo_sids) = apply_transaction(&mut ledger, tx);
    let debt_sid = txo_sids[0];

    let loan_transfer_template = AssetRecordTemplate::with_no_asset_tracking(loan_amount,
                                                                             debt_code.val,
                                                                             AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                                             lender_key_pair.get_pk_ref().clone());
    let fiat_transfer_template = AssetRecordTemplate::with_no_asset_tracking(fiat_amount,
                                                                             fiat_code.val,
                                                                             AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                                             borrower_key_pair.get_pk_ref().clone());

    let loan_transfer_record = AssetRecord::from_template_no_identity_tracking(
      ledger.get_prng(), &loan_transfer_template).unwrap();

    let fiat_transfer_record = AssetRecord::from_template_no_identity_tracking(
      ledger.get_prng(), &fiat_transfer_template).unwrap();

    let fiat_bar = ((ledger.get_utxo(fiat_sid).unwrap().utxo.0).0).clone();
    let debt_bar = ((ledger.get_utxo(debt_sid).unwrap().utxo.0).0).clone();

    let mut transfer = TransferAsset::new(TransferAssetBody::new(ledger.get_prng(),
                                          vec![TxoRef::Absolute(fiat_sid), TxoRef::Absolute(debt_sid)],
                                          &[AssetRecord::from_open_asset_record_no_asset_tracking(open_blind_asset_record(&fiat_bar, &None, &lender_key_pair.get_sk_ref()).unwrap()),
                                          AssetRecord::from_open_asset_record_no_asset_tracking(open_blind_asset_record(&debt_bar, &None, &borrower_key_pair.get_sk_ref()).unwrap())],
                                          &[fiat_transfer_record, loan_transfer_record],
                                          None,
                       TransferType::Standard).unwrap()).unwrap();
    transfer.sign(&lender_key_pair);
    transfer.sign(&borrower_key_pair);
    let tx = Transaction::from_operation(Operation::TransferAsset(transfer),
                                         ledger.get_no_replay_token());

    let (_txn_sid, txo_sids) = apply_transaction(&mut ledger, tx);
    let fiat_sid = txo_sids[0];
    let debt_sid = txo_sids[1];

    // Attempt to pay off debt with correct interest payment
    let null_public_key = XfrPublicKey::zei_from_bytes(&[0; 32]).unwrap();
    let mut block = ledger.start_block().unwrap();
    let fiat_bar = ((ledger.get_utxo(fiat_sid).unwrap().utxo.0).0).clone();
    let debt_bar = ((ledger.get_utxo(debt_sid).unwrap().utxo.0).0).clone();

    let payment_template = AssetRecordTemplate::with_no_asset_tracking(
      payment_amount,
      fiat_code.val,
      AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
      lender_key_pair.get_pk_ref().clone());
    let payment_record = AssetRecord::from_template_no_identity_tracking(
      ledger.get_prng(),
      &payment_template
    ).unwrap();

    let burned_debt_template = AssetRecordTemplate::with_no_asset_tracking(
      loan_burn_amount,
      debt_code.val,
      AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
      null_public_key);
    let burned_debt_record = AssetRecord::from_template_no_identity_tracking(
      ledger.get_prng(),
      &burned_debt_template
    ).unwrap();

    let returned_debt_template = AssetRecordTemplate::with_no_asset_tracking(
      loan_amount - loan_burn_amount,
      debt_code.val,
      AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
      lender_key_pair.get_pk_ref().clone());
    let returned_debt_record = AssetRecord::from_template_no_identity_tracking(
      ledger.get_prng(),
      &returned_debt_template
    ).unwrap();

    let returned_fiat_template = AssetRecordTemplate::with_no_asset_tracking(
      fiat_amount - payment_amount,
      fiat_code.val,
      AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
      borrower_key_pair.get_pk_ref().clone());

    let returned_fiat_record = AssetRecord::from_template_no_identity_tracking(
      ledger.get_prng(),
      &returned_fiat_template
    ).unwrap();

    let transfer_body =
      TransferAssetBody::new(ledger.get_prng(),
                             vec![TxoRef::Absolute(debt_sid), TxoRef::Absolute(fiat_sid)],
                             &[AssetRecord::from_open_asset_record_no_asset_tracking(open_blind_asset_record(&debt_bar,
                                                       &None,
                                                       &lender_key_pair.get_sk_ref()).unwrap()),
                               AssetRecord::from_open_asset_record_no_asset_tracking(open_blind_asset_record(&fiat_bar,
                                                       &None,
                                                       &borrower_key_pair.get_sk_ref()).unwrap())],
                             &[payment_record,
                               burned_debt_record,
                               returned_debt_record,
                               returned_fiat_record],
                             None,
                                                        TransferType::DebtSwap).unwrap();

    let tx = Transaction::from_operation(Operation::TransferAsset(TransferAsset::new(transfer_body).unwrap()), ledger.get_no_replay_token());

    let effect = TxnEffect::compute_effect(tx).unwrap();
    let result = ledger.apply_transaction(&mut block, effect);
    assert!(result.is_ok());
  }
}
