//#![deny(warnings)]
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  FinalizedTransaction, Operation, Transaction, TransferAsset, TxnSID, TxnTempSID, TxoRef, TxoSID,
  XfrAddress,
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
        LU: LedgerUpdate<RNG> + ArchiveAccess + LedgerAccess
{
  committed_state: Arc<RwLock<LU>>,
  addresses_to_utxos: HashMap<XfrAddress, HashSet<TxoSID>>,
  utxos_to_map_index: HashMap<TxoSID, XfrAddress>,
  prng: RNG,
}

impl<RNG, LU> QueryServer<RNG, LU>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + ArchiveAccess + LedgerAccess
{
  pub fn new(prng: RNG,
             ledger_state: Arc<RwLock<LU>>)
             -> Result<QueryServer<RNG, LU>, PlatformError> {
    Ok(QueryServer { committed_state: ledger_state,
                     addresses_to_utxos: HashMap::new(),
                     utxos_to_map_index: HashMap::new(),
                     prng })
  }

  pub fn get_address_of_sid(&self, txo_sid: TxoSID) -> Option<XfrAddress> {
    self.utxos_to_map_index.get(&txo_sid).cloned()
  }

  pub fn remove_spent_utxos(&mut self, transfer: &TransferAsset) -> Result<(), PlatformError> {
    for input in &transfer.body.inputs {
      match input {
        TxoRef::Relative(_) => panic!("wtf"),
        TxoRef::Absolute(txo_sid) => {
          let address = self.utxos_to_map_index
                            .get(&txo_sid)
                            .ok_or(PlatformError::SubmissionServerError(Some("whoops".into())))?;
          let mut hash_set =
            self.addresses_to_utxos
                .get_mut(&address)
                .ok_or(PlatformError::SubmissionServerError(Some("whoops".into())))?;
          let removed = hash_set.remove(&txo_sid);
          if (!removed) {
            return Err(PlatformError::SubmissionServerError(Some("whoops".into())));
          }
        }
      }
    }
    Ok(())
  }

  pub fn poll_new_blocks(&mut self) -> Result<(), PlatformError> {
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

    let mut finalized_blocks = vec![];
    {
      let mut ledger = self.committed_state.write().unwrap();
      for (bid, block) in new_blocks {
        info!("Received block {}", bid);
        let mut block_builder = ledger.start_block().unwrap();
        for txn in block {
          let eff = TxnEffect::compute_effect(ledger.get_prng(), txn.txn.clone()).unwrap();
          ledger.apply_transaction(&mut block_builder, eff).unwrap();
        }

        finalized_blocks.push(ledger.finish_block(block_builder).unwrap());
      }
    }

    for block in &finalized_blocks {
      for (_, (txn_sid, txo_sids)) in block.iter() {
        // get the transaction and ownership addresses associated with each transaction
        let (txn, addresses) = {
          let mut ledger = self.committed_state.read().unwrap();
          let addresses: Vec<XfrAddress> =
            txo_sids.iter()
                    .map(|sid| XfrAddress { key: ledger.get_utxo(*sid).unwrap().0 .0.public_key })
                    .collect();
          (ledger.get_transaction(*txn_sid).unwrap().finalized_txn.txn, addresses)
        };

        // Remove spent utxos
        for op in &txn.operations {
          if let Operation::TransferAsset(transfer_asset) = op {
            self.remove_spent_utxos(&transfer_asset);
          };
        }
        // Add new utxos (this handles both transfers and issuances)
        for (txo_sid, address) in txo_sids.iter().zip(addresses.iter()) {
          self.addresses_to_utxos
              .entry(*address)
              .or_insert(HashSet::new())
              .insert(*txo_sid);
          self.utxos_to_map_index.insert(*txo_sid, *address);
        }
      }
    }
    Ok(())
  }
}
