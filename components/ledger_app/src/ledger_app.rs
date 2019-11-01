extern crate ledger;

use ledger::data_model::errors::PlatformError;
use ledger::data_model::{Transaction, TxnTempSID};
use ledger::store::*;
use rand::{CryptoRng, Rng};
use std::sync::{Arc, RwLock};

// Maximum number of transactions in a block
static BLOCK_CAPACITY: usize = 8;

pub struct LedgerApp<RNG, LU>
  where RNG: Rng + CryptoRng,
        LU: LedgerUpdate<RNG>
{
  committed_state: Arc<RwLock<LU>>,
  pub(crate) block: Option<LU::Block>,
  pub(crate) temp_sids: Vec<TxnTempSID>,
  prng: RNG,
}

impl<RNG, LU> LedgerApp<RNG, LU>
  where RNG: Rng + CryptoRng,
        LU: LedgerUpdate<RNG>
{
  pub fn new(prng: RNG, ledger_state: LU) -> Result<LedgerApp<RNG, LU>, PlatformError> {
    Ok(LedgerApp { committed_state: Arc::new(RwLock::new(ledger_state)),
                   block: None,
                   temp_sids: vec![],
                   prng })
  }

  pub fn all_commited(&self) -> bool {
    self.temp_sids.len() == 0
  }

  // TODO (Keyao): Determine the condition
  // Commit for a certain number of transactions or time duration?
  pub fn eligible_to_commit(&self) -> bool {
    self.temp_sids.len() == BLOCK_CAPACITY
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
        ledger.finish_block(block);
      } // What should happen in failure? -joe
    } // What should happen in failure? -joe

    // Empty temp_sids after the block is finished
    // If begin_commit or end_commit is no longer empty, move this line to the end of end_commit
    self.temp_sids = Vec::new();
  }

  pub fn cache_transaction(&mut self, txn: Transaction) -> Result<(), PlatformError> {
    if let Some(block) = &mut self.block {
      if let Ok(ledger) = self.committed_state.read() {
        let txn_effect = TxnEffect::compute_effect(&mut self.prng, txn)?;
        self.temp_sids
            .push(ledger.apply_transaction(block, txn_effect)?);

        return Ok(());
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
  pub fn handle_transaction(&mut self, txn: Transaction) {
    // Begin a block if the previous one has been commited
    if self.all_commited() {
      self.begin_block();
    }

    self.cache_transaction(txn)
        .map_err(|e| actix_web::error::ErrorBadRequest(e))
        .unwrap();

    // End the current block if it's eligible to commit
    if self.eligible_to_commit() {
      self.end_block();

      // If begin_commit and end_commit are no longer empty, call them here
    }
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
  use rand::SeedableRng;
  use txn_builder::{BuildsTransactions, TransactionBuilder};
  use zei::basic_crypto::signatures::XfrKeyPair;

  #[test]
  fn test_cache_transaction() {
    // Create a LedgerApp
    let ledger_state = LedgerState::test_ledger();
    let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
    let mut ledger_app = LedgerApp::new(prng.clone(), ledger_state).unwrap();

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
                                             &String::from("{}"),
                                             true)
                 .unwrap();

    txn_builder_1.add_operation_create_asset(&IssuerPublicKey { key: public_key },
                                             &secret_key,
                                             None,
                                             true,
                                             true,
                                             "test",
                                             true)
                 .unwrap();

    // Cache transactions
    ledger_app.cache_transaction(txn_builder_0.transaction().clone())
              .unwrap();
    ledger_app.cache_transaction(txn_builder_1.transaction().clone())
              .unwrap();

    // Verify temp_sids
    let temp_sid_0 = *ledger_app.temp_sids.get(0).unwrap();
    let temp_sid_1 = *ledger_app.temp_sids.get(1).unwrap();

    assert_eq!(temp_sid_0, TxnTempSID(0));
    assert_eq!(temp_sid_1, TxnTempSID(1));
  }

  #[test]
  fn test_eligible_to_commit() {
    // Create a LedgerApp
    let ledger_state = LedgerState::test_ledger();
    let prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
    let mut ledger_app = LedgerApp::new(prng, ledger_state).unwrap();

    ledger_app.begin_block();

    let transaction = Transaction::default();

    // Verify that it's ineligible to commit if #transactions < BLOCK_CAPACITY
    for _i in 0..(BLOCK_CAPACITY - 1) {
      ledger_app.cache_transaction(transaction.clone()).unwrap();
      assert!(!ledger_app.eligible_to_commit());
    }

    // Verify that it's eligible to commit if #transactions == BLOCK_CAPACITY
    ledger_app.cache_transaction(transaction).unwrap();
    assert!(ledger_app.eligible_to_commit());
  }
}
