extern crate core;
extern crate api_service;

use core::data_model::errors::PlatformError;
use core::data_model::Transaction;
use core::store::*;
use std::sync::{Arc, RwLock};

pub struct LedgerApp {
  committed_state: Arc<RwLock<LedgerState>>,
  pending_state: Option<BlockContext<LedgerState>>,
  txns: Vec<Transaction>,
}

impl LedgerApp {
  pub fn new(ledger_state: LedgerState) -> Result<LedgerApp, PlatformError> {
    Ok(LedgerApp { committed_state: Arc::new(RwLock::new(ledger_state)),
                   pending_state: None,
                   txns: Vec::new(),
                 })
  }

  pub fn borrowable_ledger_state(&self) -> Arc<RwLock<LedgerState>> {
    self.committed_state.clone()
  }

  pub fn get_committed_state(&self) -> &RwLock<LedgerState> {
    &self.committed_state
  }

  pub fn get_pending_state(&self) -> &Option<BlockContext<LedgerState>> {
    &self.pending_state
  }

  pub fn get_mut_pending_state(&mut self) -> &mut Option<BlockContext<LedgerState>> {
    &mut self.pending_state
  }

  pub fn begin_block(&mut self) {
    self.pending_state = BlockContext::new(&self.committed_state).ok();
    self.txns.clear();
  }

  pub fn end_block(&mut self) {}
  pub fn begin_commit(&mut self) {}

  pub fn end_commit(&mut self) {
    let txns = &mut self.txns;
    if let Ok(mut writer) = self.committed_state.write() {
      for tx in txns.drain(..) {
        writer.apply_transaction(&tx);
        writer.append_transaction(tx);
      }
    }
    self.pending_state = None;
  }
  pub fn cache_transaction(&mut self, txn: Transaction) {
    self.txns.push(txn);
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
