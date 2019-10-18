extern crate api_service;
extern crate ledger;

use ledger::data_model::errors::PlatformError;
use ledger::data_model::Transaction;
use ledger::store::*;
use std::sync::{Arc, RwLock};

pub struct LedgerApp {
  committed_state: Arc<RwLock<LedgerState>>,
  txns: Vec<TxnEffect>,
}

impl LedgerApp {
  pub fn new(ledger_state: LedgerState) -> Result<LedgerApp, PlatformError> {
    Ok(LedgerApp { committed_state: Arc::new(RwLock::new(ledger_state)),
                   txns: Vec::new() })
  }

  pub fn borrowable_ledger_state(&self) -> Arc<RwLock<LedgerState>> {
    self.committed_state.clone()
  }

  pub fn get_committed_state(&self) -> &RwLock<LedgerState> {
    &self.committed_state
  }

  // TODO(joe): what should these do?
  pub fn begin_block(&mut self) {}
  pub fn end_block(&mut self) {}
  pub fn begin_commit(&mut self) {}

  pub fn end_commit(&mut self) {
    let txns = &mut self.txns;
    if let Ok(mut ledger) = self.committed_state.write() {
      for tx in txns.drain(..) {
        // TODO(joe): should error recovery be more graceful?
        ledger.apply_transaction(tx).unwrap();
      }
    }
  }
  pub fn cache_transaction(&mut self, txn: Transaction) -> Result<(),PlatformError> {
    if let Ok(mut ledger) = self.committed_state.write() {
        let txn_effect = TxnEffect::compute_effect(ledger.get_prng(), txn)?;
        self.txns.push(txn_effect);
        return Ok(());
    }
    Err(PlatformError::InputsError)
  }
}

// Convert incoming tx data to the proper Transaction format
pub fn convert_tx(tx: &[u8]) -> Option<Transaction> {
  let transaction: Option<Transaction> = serde_json::from_slice(tx).ok();

  transaction
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
