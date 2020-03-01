//#![deny(warnings)]
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  FinalizedTransaction, Operation, Transaction, TxnSID, TxnTempSID, TxoSID, XfrAddress,
};
use ledger::store::*;
use log::{error, info};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::marker::PhantomData;
use std::sync::{Arc, RwLock};

pub struct QueryServer<RNG, LU>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + ArchiveAccess
{
  committed_state: Arc<RwLock<LU>>,
  addresses_to_utxos: HashMap<XfrAddress, HashSet<TxoSID>>,
  utxos_to_map_index: HashMap<TxoSID, XfrAddress>,
  prng: RNG,
}

impl<RNG, LU> QueryServer<RNG, LU>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + ArchiveAccess
{
  pub fn new(prng: RNG,
             ledger_state: Arc<RwLock<LU>>)
             -> Result<QueryServer<RNG, LU>, PlatformError> {
    Ok(QueryServer { committed_state: ledger_state,
                     addresses_to_utxos: HashMap::new(),
                     utxo_to_map_index: HashMap::new(),
                     prng })
  }

  pub fn update_spent_utxos(&self, transfer: &TransferAsset) {
    inputs = transfer.body.inputs;
    for input in &inputs {
      match input {
        Relative(_) => panic!("wtf"),
        Absolute(txo_sid) => {
          let address = self.utxos_to_map_index.get(&txo_sid);
          self.addresses_to_utxos
        }
      }
    }
  }

  pub fn poll_new_blocks(&self) -> Result<(), PlatformError> {
    let ledger_url = std::env::var_os("LEDGER_URL").filter(|x| !x.is_empty())
                                                   .unwrap_or_else(|| "localhost:8668".into());
    let latest_block = {
      let ledger = self.committed_state.read().unwrap();
      (*ledger).get_block_count()
    };
    let new_blocks = match reqwest::get(&format!("http://{}/{}/{}",
                                                 ledger_url.to_str().unwrap(),
                                                 "blocks_since",
                                                 &latest_block))
    {
      Err(e) => return Err(PlatformError::SubmissionServerError(Some("whoops".into()))),

      Ok(mut bs) => match bs.json::<Vec<(usize, Vec<FinalizedTransaction>)>>() {
        Err(_) => return Err(PlatformError::DeserializationError),
        Ok(bs) => bs,
      },
    };

    let mut ledger = self.committed_state.write().unwrap();
    for (bid, block) in new_blocks {
      info!("Received block {}", bid);
      let mut block_builder = ledger.start_block().unwrap();
      for txn in block {
        let txn = txn.txn;
        let eff = TxnEffect::compute_effect(ledger.get_prng(), txn.clone()).unwrap();
        ledger.apply_transaction(&mut block_builder, eff).unwrap();
        for op in &txn.operations {
          self.update_utxos(&op);
        }
      }
      ledger.finish_block(block_builder).unwrap();
    }
    Ok(())
  }
}
