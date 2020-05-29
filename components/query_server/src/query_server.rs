#![deny(warnings)]
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  FinalizedTransaction, Operation, TransferAsset, TxoRef, TxoSID, XfrAddress,
};
use ledger::error_location;
use ledger::store::*;
use log::info;
use rand_core::{CryptoRng, RngCore};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::sync::{Arc, RwLock};

macro_rules! fail {
  () => {
    PlatformError::QueryServerError(error_location!())
  };
  ($s:expr) => {
    PlatformError::QueryServerError(format!("[{}] {}", &error_location!(), &$s))
  };
}

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
                            .ok_or_else(|| fail!("Attempting to remove owned txo of address that isn't cached"))?;
          let hash_set = self.addresses_to_utxos
                             .get_mut(&address)
                             .ok_or_else(|| fail!("No txos stored for this address"))?;
          let removed = hash_set.remove(&txo_sid);
          if !removed {
            return Err(fail!("Input txo not found"));
          }
        }
      }
    }
    Ok(())
  }

  // Updates query server cache with new transactions from a block.
  // Each new block must be consistent with the state of the cached ledger up until this point
  pub fn add_new_block(&mut self, block: &[FinalizedTransaction]) -> Result<(), PlatformError> {
    // First, we add block to local ledger state
    let finalized_block = {
      let mut ledger = self.committed_state.write().unwrap();
      let mut block_builder = ledger.start_block().unwrap();
      for txn in block {
        let eff = TxnEffect::compute_effect(txn.txn.clone()).unwrap();
        ledger.apply_transaction(&mut block_builder, eff).unwrap();
      }

      ledger.finish_block(block_builder).unwrap()
    };
    // Next, update ownership status
    for (_, (txn_sid, txo_sids)) in finalized_block.iter() {
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
      for op in &txn.body.operations {
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
    Ok(())
  }

  pub fn poll_new_blocks(&mut self) -> Result<(), PlatformError> {
    let ledger_url =
      std::env::var_os("LEDGER_URL").filter(|x| !x.is_empty())
                                    .unwrap_or_else(|| format!("localhost:{}", PORT).into());
    let protocol = std::env::var_os("LEDGER_PROTOCOL").filter(|x| !x.is_empty())
                                                      .unwrap_or_else(|| "http".into());
    let latest_block = {
      let ledger = self.committed_state.read().unwrap();
      (*ledger).get_block_count()
    };
    let new_blocks = match reqwest::get(&format!("{}://{}/{}/{}",
                                                 protocol.to_str().unwrap(),
                                                 ledger_url.to_str().unwrap(),
                                                 "blocks_since",
                                                 &latest_block))
    {
      Err(_) => {
        return Err(fail!("Cannot connect to ledger server"));
      }

      Ok(mut bs) => match bs.json::<Vec<(usize, Vec<FinalizedTransaction>)>>() {
        Err(e) => {
          return Err(PlatformError::DeserializationError(format!("[{}]: {:?}",
                                                                 &error_location!(),
                                                                 e)));
        }
        Ok(bs) => bs,
      },
    };

    for (bid, block) in new_blocks {
      info!("Received block {}", bid);
      self.add_new_block(&block)?;
    }

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::{AssetRules, AssetTypeCode, BlockSID, TransferType};
  use ledger::store::helpers::apply_transaction;
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use txn_builder::{
    BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder,
  };
  use zei::xfr::asset_record::open_blind_asset_record;
  use zei::xfr::asset_record::AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
  use zei::xfr::sig::XfrKeyPair;
  use zei::xfr::structs::AssetRecordTemplate;

  #[test]
  pub fn test_query_server() {
    let query_server_ledger_state = LedgerState::test_ledger();
    let mut ledger_state = LedgerState::test_ledger();
    let mut prng = ChaChaRng::from_entropy();
    let mut query_server = QueryServer::new(Arc::new(RwLock::new(query_server_ledger_state)));
    let token_code = AssetTypeCode::gen_random();
    // Define keys
    let alice = XfrKeyPair::generate(&mut prng);
    let bob = XfrKeyPair::generate(&mut prng);
    // Define asset
    let mut builder = TransactionBuilder::default();
    let define_tx = builder.add_operation_create_asset(&alice,
                                                       Some(token_code),
                                                       AssetRules::default(),
                                                       "fiat".into(),
                                                       PolicyChoice::Fungible())
                           .unwrap()
                           .transaction();

    let mut builder = TransactionBuilder::default();

    //Issuance txn
    let amt = 1000;
    let confidentiality_flag = NonConfidentialAmount_NonConfidentialAssetType;
    let issuance_tx =
      builder.add_basic_issue_asset(&alice, None, &token_code, 0, amt, confidentiality_flag)
             .unwrap()
             .add_basic_issue_asset(&alice, None, &token_code, 1, amt, confidentiality_flag)
             .unwrap()
             .add_basic_issue_asset(&alice, None, &token_code, 2, amt, confidentiality_flag)
             .unwrap()
             .transaction();

    apply_transaction(&mut ledger_state, define_tx.clone());
    apply_transaction(&mut ledger_state, issuance_tx.clone());

    // Transfer to Bob
    let transfer_sid = TxoSID(0);
    let bar = &(ledger_state.get_utxo(transfer_sid).unwrap().0).0;
    let oar = open_blind_asset_record(&bar, &None, alice.get_sk_ref()).unwrap();
    let mut xfr_builder = TransferOperationBuilder::new();
    let out_template = AssetRecordTemplate::with_no_asset_tracking(amt,
                                                                   token_code.val,
                                                                   oar.get_record_type(),
                                                                   bob.get_pk());
    let xfr_op = xfr_builder.add_input(TxoRef::Absolute(transfer_sid), oar, None, None, amt)
                            .unwrap()
                            .add_output(&out_template, None, None, None)
                            .unwrap()
                            .create(TransferType::Standard)
                            .unwrap()
                            .sign(&alice)
                            .unwrap();
    let mut builder = TransactionBuilder::default();
    let xfr_txn = builder.add_operation(xfr_op.transaction().unwrap())
                         .transaction();

    apply_transaction(&mut ledger_state, xfr_txn.clone());

    let block0 = ledger_state.get_block(BlockSID(0)).unwrap();
    let block1 = ledger_state.get_block(BlockSID(1)).unwrap();
    let block2 = ledger_state.get_block(BlockSID(2)).unwrap();

    // Query server will now fetch new blocks
    query_server.add_new_block(&block0.block.txns).unwrap();
    query_server.add_new_block(&block1.block.txns).unwrap();
    //query_server.poll_new_blocks().unwrap();

    // Ensure that query server is aware of issuances
    let alice_sids = query_server.get_owned_utxo_sids(&XfrAddress { key: *alice.get_pk_ref() })
                                 .unwrap();

    assert!(alice_sids.contains(&TxoSID(0)));
    assert!(alice_sids.contains(&TxoSID(1)));
    assert!(alice_sids.contains(&TxoSID(2)));

    query_server.add_new_block(&block2.block.txns).unwrap();

    // Ensure that query server is aware of ownership changes
    let alice_sids = query_server.get_owned_utxo_sids(&XfrAddress { key: *alice.get_pk_ref() })
                                 .unwrap();
    let bob_sids = query_server.get_owned_utxo_sids(&XfrAddress { key: *bob.get_pk_ref() })
                               .unwrap();

    assert!(!alice_sids.contains(&TxoSID(0)));
    assert!(bob_sids.contains(&TxoSID(3)));
  }
}
