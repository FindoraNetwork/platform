#![deny(warnings)]
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  FinalizedTransaction, Operation, TransferAsset, TxoRef, TxoSID, XfrAddress,
};
use ledger::store::*;
use log::info;
use rand_core::{CryptoRng, RngCore};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::sync::{Arc, RwLock};

const PORT: usize = 8668;

pub struct QueryServer<RNG, LU>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + ArchiveAccess + LedgerAccess
{
  committed_state: Arc<RwLock<LU>>,
  addresses_to_utxos: HashMap<XfrAddress, HashSet<TxoSID>>,
  utxos_to_map_index: HashMap<TxoSID, XfrAddress>,
  prng: PhantomData<RNG>,
}

impl<RNG, LU> QueryServer<RNG, LU>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + ArchiveAccess + LedgerAccess
{
  pub fn new(ledger_state: Arc<RwLock<LU>>) -> QueryServer<RNG, LU> {
    QueryServer { committed_state: ledger_state,
                  addresses_to_utxos: HashMap::new(),
                  utxos_to_map_index: HashMap::new(),
                  prng: PhantomData }
  }

  pub fn get_address_of_sid(&self, txo_sid: TxoSID) -> Option<XfrAddress> {
    self.utxos_to_map_index.get(&txo_sid).cloned()
  }

  pub fn get_owned_utxo_sids(&self, address: &XfrAddress) -> Option<HashSet<TxoSID>> {
    self.addresses_to_utxos.get(&address).cloned()
  }

  pub fn remove_spent_utxos(&mut self, transfer: &TransferAsset) -> Result<(), PlatformError> {
    for input in &transfer.body.inputs {
      match input {
        TxoRef::Relative(_) => {} // Relative utxos were never cached so no need to do anything here
        TxoRef::Absolute(txo_sid) => {
          let address = self.utxos_to_map_index
                            .get(&txo_sid)
                            .ok_or_else(|| PlatformError::QueryServerError(Some("Attempting to remove owned txo of address that isn't cached".into())))?;
          let hash_set = self.addresses_to_utxos
                             .get_mut(&address)
                             .ok_or_else(|| PlatformError::QueryServerError(Some("No txos stored for this address".into())))?;
          let removed = hash_set.remove(&txo_sid);
          if !removed {
            return Err(PlatformError::QueryServerError(Some("Input txo not found".into())));
          }
        }
      }
    }
    Ok(())
  }

  pub fn poll_new_blocks(&mut self) -> Result<(), PlatformError> {
    let ledger_url =
      std::env::var_os("LEDGER_URL").filter(|x| !x.is_empty())
                                    .unwrap_or_else(|| format!("localhost:{}", PORT).into());
    let latest_block = {
      let ledger = self.committed_state.read().unwrap();
      (*ledger).get_block_count()
    };
    let new_blocks = match reqwest::get(&format!("http://{}/{}/{}",
                                                 ledger_url.to_str().unwrap(),
                                                 "blocks_since",
                                                 &latest_block))
    {
      Err(_) => {
        return Err(PlatformError::QueryServerError(Some("Cannot connect to ledger server".into())))
      }

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
          let ledger = self.committed_state.read().unwrap();
          let addresses: Vec<XfrAddress> =
            txo_sids.iter()
                    .map(|sid| XfrAddress { key: ledger.get_utxo(*sid).unwrap().0 .0.public_key })
                    .collect();
          (ledger.get_transaction(*txn_sid).unwrap().finalized_txn.txn, addresses)
        };

        // Remove spent utxos
        for op in &txn.operations {
          if let Operation::TransferAsset(transfer_asset) = op {
            self.remove_spent_utxos(&transfer_asset)?;
          };
        }
        // Add new utxos (this handles both transfers and issuances)
        for (txo_sid, address) in txo_sids.iter().zip(addresses.iter()) {
          self.addresses_to_utxos
              .entry(*address)
              .or_insert_with(HashSet::new)
              .insert(*txo_sid);
          self.utxos_to_map_index.insert(*txo_sid, *address);
        }
      }
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::{AssetTypeCode, TransferType};
  use ledger_standalone::LedgerStandalone;
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use txn_builder::{BuildsTransactions, TransactionBuilder, TransferOperationBuilder};
  use zei::xfr::asset_record::open_blind_asset_record;
  use zei::xfr::asset_record::AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
  use zei::xfr::sig::XfrKeyPair;
  use zei::xfr::structs::AssetRecordTemplate;

  #[test]
  // Ignoring this test since it sometimes fails on master
  pub fn test_query_server() {
    let ledger_state = LedgerState::test_ledger();
    let mut prng = ChaChaRng::from_entropy();
    let mut query_server = QueryServer::new(Arc::new(RwLock::new(ledger_state)));
    let token_code = AssetTypeCode::gen_random();
    let ledger_standalone = LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();
    // Define keys
    let alice = XfrKeyPair::generate(&mut prng);
    let bob = XfrKeyPair::generate(&mut prng);
    // Define asset
    let mut builder = TransactionBuilder::default();
    let define_tx =
      builder.add_operation_create_asset(&alice, Some(token_code), false, false, "fiat".into())
             .unwrap()
             .transaction();

    ledger_standalone.submit_transaction(&define_tx);
    let mut builder = TransactionBuilder::default();

    //Issuance txn
    let amt = 1000;
    let confidentiality_flag = NonConfidentialAmount_NonConfidentialAssetType; // TODO (fernando) get this right
    let issuance_tx =
      builder.add_basic_issue_asset(&alice, &None, &token_code, 0, amt, confidentiality_flag)
             .unwrap()
             .add_basic_issue_asset(&alice, &None, &token_code, 1, amt, confidentiality_flag)
             .unwrap()
             .add_basic_issue_asset(&alice, &None, &token_code, 2, amt, confidentiality_flag)
             .unwrap()
             .transaction();
    ledger_standalone.submit_transaction(&issuance_tx);

    // Query server will now fetch new blocks
    query_server.poll_new_blocks().unwrap();

    // Ensure that query server is aware of issuances
    let alice_sids = query_server.get_owned_utxo_sids(&XfrAddress { key: *alice.get_pk_ref() })
                                 .unwrap();

    assert!(alice_sids.contains(&TxoSID(0)));
    assert!(alice_sids.contains(&TxoSID(1)));
    assert!(alice_sids.contains(&TxoSID(2)));

    // Transfer to Bob
    let transfer_sid = TxoSID(0);
    let bar = ledger_standalone.fetch_blind_asset_record(transfer_sid);
    let memo = None; // TODO (fernando) need to fectch Owner memo associated with bar
    let oar = open_blind_asset_record(&bar, &memo, alice.get_sk_ref()).unwrap();
    let mut xfr_builder = TransferOperationBuilder::new();
    let out_template = AssetRecordTemplate::with_no_asset_tracking(amt,
                                                                   token_code.val,
                                                                   oar.get_record_type(),
                                                                   bob.get_pk());
    let xfr_op = xfr_builder.add_input(TxoRef::Absolute(transfer_sid), oar, amt)
                            .unwrap()
                            .add_output(&out_template)
                            .unwrap()
                            .create(TransferType::Standard)
                            .unwrap()
                            .sign(&alice)
                            .unwrap();
    let mut builder = TransactionBuilder::default();
    let xfr_txn = builder.add_operation(xfr_op.transaction().unwrap())
                         .transaction();
    ledger_standalone.submit_transaction(&xfr_txn);
    // Query server will now fetch new blocks
    query_server.poll_new_blocks().unwrap();

    // Ensure that query server is aware of ownership changes
    let alice_sids = query_server.get_owned_utxo_sids(&XfrAddress { key: *alice.get_pk_ref() })
                                 .unwrap();
    let bob_sids = query_server.get_owned_utxo_sids(&XfrAddress { key: *bob.get_pk_ref() })
                               .unwrap();

    assert!(!alice_sids.contains(&TxoSID(0)));
    assert!(bob_sids.contains(&TxoSID(3)));
  }
}
