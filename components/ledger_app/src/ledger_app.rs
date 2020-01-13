#![deny(warnings)]
extern crate ledger;

use cryptohash::sha256;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{Transaction, TxnSID, TxnTempSID, TxoSID};
use ledger::store::*;
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

// Query handle for user
#[derive(Debug, Hash, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct TxnHandle(String);

impl TxnHandle {
  pub fn new(txn: &Transaction) -> Self {
    let digest = sha256::hash(&txn.serialize_bincode(TxnSID(0)));
    TxnHandle(hex::encode(digest))
  }
}

// Indicates whether a transaction has been committed to the ledger
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum TxnStatus {
  Committed((TxnSID, Vec<TxoSID>)),
  Pending,
}

pub struct LedgerApp<RNG, LU>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG>
{
  committed_state: Arc<RwLock<LU>>,
  block: Option<LU::Block>,
  pending_txns: Vec<(TxnTempSID, TxnHandle)>,
  txn_status: HashMap<TxnHandle, TxnStatus>,
  block_capacity: usize,
  prng: RNG,
}

impl<RNG, LU> LedgerApp<RNG, LU>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG>
{
  pub fn new(prng: RNG,
             ledger_state: Arc<RwLock<LU>>,
             block_capacity: usize)
             -> Result<LedgerApp<RNG, LU>, PlatformError> {
    Ok(LedgerApp { committed_state: ledger_state,
                   block: None,
                   txn_status: HashMap::new(),
                   pending_txns: vec![],
                   prng,
                   block_capacity })
  }

  pub fn get_txn_status(&self, txn_handle: &TxnHandle) -> Option<TxnStatus> {
    self.txn_status
        .get(&txn_handle)
        .map(|handle| handle.clone())
  }

  pub fn all_commited(&self) -> bool {
    self.pending_txns.is_empty()
  }

  // TODO (Keyao): Determine the condition
  // Commit for a certain number of transactions or time duration?
  pub fn eligible_to_commit(&self) -> bool {
    self.pending_txns.len() == self.block_capacity
  }

  pub fn get_prng(&mut self) -> &mut RNG {
    &mut self.prng
  }

  pub fn borrowable_ledger_state(&self) -> Arc<RwLock<LU>> {
    self.committed_state.clone()
  }

  pub fn get_committed_state(&self) -> &RwLock<LU> {
    &self.committed_state
  }

  // TODO(joe): what should these do?
  pub fn begin_commit(&mut self) {}
  pub fn end_commit(&mut self) {}

  pub fn begin_block(&mut self) {
    assert!(self.block.is_none());
    if let Ok(mut ledger) = self.committed_state.write() {
      self.block = Some(ledger.start_block().unwrap());
    } // What should happen in failure? -joe
  }

  pub fn end_block(&mut self) {
    let mut block = None;
    std::mem::swap(&mut self.block, &mut block);
    if let Some(block) = block {
      if let Ok(mut ledger) = self.committed_state.write() {
        let finalized_txns = ledger.finish_block(block);
        // Update status of all committed transactions
        for (txn_temp_sid, handle) in self.pending_txns.drain(..) {
          self.txn_status
              .insert(handle,
                      TxnStatus::Committed(finalized_txns.get(&txn_temp_sid).unwrap().clone()));
        }
      } // What should happen in failure? -joe
    } // What should happen in failure? -joe

    // Empty temp_sids after the block is finished
    // If begin_commit or end_commit is no longer empty, move this line to the end of end_commit
    self.pending_txns = Vec::new();
  }

  pub fn cache_transaction(&mut self, txn: Transaction) -> Result<TxnHandle, PlatformError> {
    if let Some(block) = &mut self.block {
      if let Ok(ledger) = self.committed_state.read() {
        let handle = TxnHandle::new(&txn);
        let txn_effect = TxnEffect::compute_effect(&mut self.prng, txn)?;
        self.pending_txns
            .push((ledger.apply_transaction(block, txn_effect)?, handle.clone()));
        self.txn_status.insert(handle.clone(), TxnStatus::Pending);

        return Ok(handle);
      }
    }

    Err(PlatformError::InputsError)
  }

  pub fn abort_block(&mut self) {
    let mut block = None;
    std::mem::swap(&mut self.block, &mut block);
    if let Some(block) = block {
      if let Ok(mut ledger) = self.committed_state.write() {
        ledger.abort_block(block);
      }
    }
  }

  // Handle the whole process when there's a new transaction
  // TODO noah propagate error
  pub fn handle_transaction(&mut self, txn: Transaction) -> TxnHandle {
    // Begin a block if the previous one has been commited
    if self.all_commited() {
      self.begin_block();
    }

    let handle = self.cache_transaction(txn)
                     .map_err(actix_web::error::ErrorBadRequest)
                     .unwrap();

    // End the current block if it's eligible to commit
    if self.eligible_to_commit() {
      self.end_block();

      // If begin_commit and end_commit are no longer empty, call them here
    }
    handle
  }
}

// Convert incoming tx data to the proper Transaction format
pub fn convert_tx(tx: &[u8]) -> Option<Transaction> {
  let transaction: Option<Transaction> = serde_json::from_slice(tx).ok();

  transaction
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::{AssetTypeCode, IssuerPublicKey};
  use rand_core::SeedableRng;
  use txn_builder::{BuildsTransactions, TransactionBuilder};
  use zei::xfr::sig::XfrKeyPair;

  #[test]
  fn test_cache_transaction() {
    // Create a LedgerApp
    let block_capacity = 8;
    let ledger_state = LedgerState::test_ledger();
    let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
    let mut ledger_app = LedgerApp::new(prng.clone(),
                                        Arc::new(RwLock::new(ledger_state)),
                                        block_capacity).unwrap();

    ledger_app.begin_block();

    // Create values to be used to build transactions
    let keypair = XfrKeyPair::generate(&mut prng);
    let public_key = *keypair.get_pk_ref();
    let secret_key = keypair.get_sk_ref();
    let token_code = "test";
    let asset_token = AssetTypeCode::new_from_base64(&token_code).unwrap();

    // Build transactions
    let mut txn_builder_0 = TransactionBuilder::default();
    let mut txn_builder_1 = TransactionBuilder::default();

    txn_builder_0.add_operation_create_asset(&IssuerPublicKey { key: public_key },
                                             &secret_key,
                                             Some(asset_token),
                                             true,
                                             true,
                                             &String::from("{}"))
                 .unwrap();

    txn_builder_1.add_operation_create_asset(&IssuerPublicKey { key: public_key },
                                             &secret_key,
                                             None,
                                             true,
                                             true,
                                             "test")
                 .unwrap();

    // Cache transactions
    ledger_app.cache_transaction(txn_builder_0.transaction().clone())
              .unwrap();
    ledger_app.cache_transaction(txn_builder_1.transaction().clone())
              .unwrap();

    // Verify temp_sids
    let temp_sid_0 = &*ledger_app.pending_txns.get(0).unwrap();
    let temp_sid_1 = &*ledger_app.pending_txns.get(1).unwrap();

    assert_eq!(temp_sid_0.0, TxnTempSID(0));
    assert_eq!(temp_sid_1.0, TxnTempSID(1));
  }

  #[test]
  fn test_eligible_to_commit() {
    // Create a LedgerApp
    let block_capacity = 8;
    let ledger_state = LedgerState::test_ledger();
    let prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
    let mut ledger_app =
      LedgerApp::new(prng, Arc::new(RwLock::new(ledger_state)), block_capacity).unwrap();

    ledger_app.begin_block();

    let transaction = Transaction::default();

    // Verify that it's ineligible to commit if #transactions < BLOCK_CAPACITY
    for _i in 0..(block_capacity - 1) {
      ledger_app.cache_transaction(transaction.clone()).unwrap();
      assert!(!ledger_app.eligible_to_commit());
    }

    // Verify that it's eligible to commit if #transactions == BLOCK_CAPACITY
    ledger_app.cache_transaction(transaction).unwrap();
    assert!(ledger_app.eligible_to_commit());
  }

  #[test]
  fn test_txn_status() {
    let block_capacity = 2;
    let ledger_state = LedgerState::test_ledger();
    let prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
    let mut ledger_app =
      LedgerApp::new(prng, Arc::new(RwLock::new(ledger_state)), block_capacity).unwrap();

    // Submit the first transcation. Ensure that the txn is pending.
    let transaction = Transaction::default();
    let txn_handle = ledger_app.handle_transaction(transaction.clone());
    let status = ledger_app.txn_status
                           .get(&txn_handle)
                           .expect("handle should be in map")
                           .clone();
    assert_eq!(status, TxnStatus::Pending);

    // Submit a second transaction and ensure that it is tracked as committed
    ledger_app.handle_transaction(transaction.clone());
    // In this case, both transactions have the same handle. Because transactions are unique and
    // We are using a collision resistant hash function, this will not occur on a live ledger.
    let status = ledger_app.txn_status
                           .get(&txn_handle)
                           .expect("handle should be in map")
                           .clone();
    assert_eq!(status, TxnStatus::Committed((TxnSID(1), Vec::new())));
  }
}
