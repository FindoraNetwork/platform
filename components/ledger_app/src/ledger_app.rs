extern crate api_service;
extern crate ledger;

use ledger::data_model::errors::PlatformError;
use ledger::data_model::Transaction;
use ledger::store::*;
use rand::{CryptoRng, Rng};
use std::sync::{Arc, RwLock};

pub struct LedgerApp<RNG, LA>
  where RNG: Rng + CryptoRng,
        LA: LedgerUpdate<RNG>
{
  committed_state: Arc<RwLock<LA>>,
  block: Option<LA::Block>,
  temp_sids: Vec<TxnTempSID>,
  prng: RNG,
}

impl<RNG, LA> LedgerApp<RNG, LA>
  where RNG: Rng + CryptoRng,
        LA: LedgerUpdate<RNG>
{
  pub fn new(prng: RNG, ledger_state: LA) -> Result<LedgerApp<RNG, LA>, PlatformError> {
    Ok(LedgerApp { committed_state: Arc::new(RwLock::new(ledger_state)),
                   block: None,
                   temp_sids: vec![],
                   prng })
  }

  pub fn borrowable_ledger_state(&self) -> Arc<RwLock<LA>> {
    self.committed_state.clone()
  }

  pub fn get_committed_state(&self) -> &RwLock<LA> {
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
