#![deny(warnings)]
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  b64enc, AssetTypeCode, BlockSID, DefineAsset, FinalizedTransaction, IssueAsset, IssuerPublicKey,
  KVBlind, KVHash, KVUpdate, Operation, Transaction, TransferAsset, TxOutput, TxnSID, TxoRef,
  TxoSID, XfrAddress,
};
use ledger::error_location;
use ledger::store::*;
use ledger_api_service::RestfulArchiveAccess;
use log::{error, info};
use sparse_merkle_tree::Key;
use std::collections::{HashMap, HashSet};
use zei::xfr::structs::OwnerMemo;

macro_rules! fail {
  () => {
    PlatformError::QueryServerError(error_location!())
  };
  ($s:expr) => {
    PlatformError::QueryServerError(format!("[{}] {}", &error_location!(), &$s))
  };
}

pub struct QueryServer<T>
  where T: RestfulArchiveAccess
{
  committed_state: LedgerState,
  addresses_to_utxos: HashMap<XfrAddress, HashSet<TxoSID>>,
  related_transactions: HashMap<XfrAddress, HashSet<TxnSID>>, // Set of transactions related to a ledger address
  created_assets: HashMap<IssuerPublicKey, Vec<AssetTypeCode>>,
  issuances: HashMap<IssuerPublicKey, Vec<TxOutput>>,
  owner_memos: HashMap<TxoSID, OwnerMemo>,
  utxos_to_map_index: HashMap<TxoSID, XfrAddress>,
  custom_data_store: HashMap<Key, (Vec<u8>, KVHash)>,
  rest_client: T,
}

impl<T> QueryServer<T> where T: RestfulArchiveAccess
{
  pub fn new(rest_client: T) -> QueryServer<T> {
    QueryServer { committed_state: LedgerState::test_ledger(),
                  addresses_to_utxos: HashMap::new(),
                  related_transactions: HashMap::new(),
                  owner_memos: HashMap::new(),
                  created_assets: HashMap::new(),
                  issuances: HashMap::new(),
                  utxos_to_map_index: HashMap::new(),
                  custom_data_store: HashMap::new(),
                  rest_client }
  }

  // Fetch custom data at a given key.
  pub fn get_custom_data(&self, key: &Key) -> Option<&(Vec<u8>, KVHash)> {
    self.custom_data_store.get(key)
  }

  // Returns the set of records issued by a certain key.
  pub fn get_issued_records(&self, issuer: &IssuerPublicKey) -> Option<&Vec<TxOutput>> {
    self.issuances.get(issuer)
  }

  pub fn get_created_assets(&self, issuer: &IssuerPublicKey) -> Option<&Vec<AssetTypeCode>> {
    self.created_assets.get(issuer)
  }

  // Returns the set of transactions that are in some way related to a given ledger address.
  // An xfr address is related to a transaction if it is one of the following:
  // 1. Owner of a transfer output
  // 2. Transfer signer (owner of input or co-signer)
  // 3. Signer of a an issuance txn
  // 4. Signer of a kv_update txn
  // 5. Signer of a memo_update txn
  pub fn get_related_transactions(&self, address: &XfrAddress) -> Option<&HashSet<TxnSID>> {
    self.related_transactions.get(&address)
  }

  // Returns the set of TxoSIDs that are the indices of records owned by a given address.
  pub fn get_owned_utxo_sids(&self, address: &XfrAddress) -> Option<&HashSet<TxoSID>> {
    self.addresses_to_utxos.get(&address)
  }

  // Returns the owner of a given txo_sid.
  pub fn get_address_of_sid(&self, txo_sid: TxoSID) -> Option<&XfrAddress> {
    self.utxos_to_map_index.get(&txo_sid)
  }

  // Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
  pub fn get_owner_memo(&self, txo_sid: TxoSID) -> Option<&OwnerMemo> {
    self.owner_memos.get(&txo_sid)
  }

  // Attempt to add to data store at a given location
  // Returns an error if the hash of the data doesn't match the hash stored by the
  // ledger's arbitrary data store at the given key
  pub fn add_to_data_store(&mut self,
                           key: &Key,
                           data: &dyn AsRef<[u8]>,
                           blind: Option<&KVBlind>)
                           -> Result<(), PlatformError> {
    let hash = KVHash::new(data, blind);
    let auth_entry = self.committed_state.get_kv_entry(*key);

    let result =
      auth_entry.result
                .ok_or_else(|| fail!("Nothing found in the custom data store at this key"))?;
    let entry_hash = result.deserialize().1.ok_or_else(|| fail!("Nothing found in the custom data store at this key. A hash was once here, but has been removed"))?.1;

    // Ensure that hash matches
    if hash != entry_hash {
      return Err(fail!("The hash of the data supplied does not match the hash stored by the ledger"));
    }

    // Hash matches, store data
    self.custom_data_store
        .insert(*key, (data.as_ref().into(), hash));
    Ok(())
  }

  // Add created asset
  pub fn add_created_asset(&mut self, creation: &DefineAsset) {
    let issuer = creation.pubkey;
    let new_asset_code = creation.body.asset.code;
    self.created_assets
        .entry(issuer)
        .or_insert_with(Vec::new)
        .push(new_asset_code);
  }

  // Cache issuance records
  pub fn cache_issuance(&mut self, issuance: &IssueAsset) {
    let issuer = issuance.pubkey;
    let mut new_records = issuance.body
                                  .records
                                  .iter()
                                  .map(|(rec, _)| rec.clone())
                                  .collect();
    let records = self.issuances.entry(issuer).or_insert_with(Vec::new);
    info!("Issuance record cached for asset issuer key {}",
          b64enc(&issuer.key.as_bytes()));
    records.append(&mut new_records);
  }

  // Remove data that may be outdated based on this kv_update
  fn remove_stale_data(&mut self, kv_update: &KVUpdate) {
    let key = kv_update.body.0;
    let entry = kv_update.body.2.as_ref();
    if let Some((_, curr_hash)) = self.custom_data_store.get(&key) {
      // If hashes don't match, data is stale
      if let Some(entry) = entry {
        if entry.1 != *curr_hash {
          self.custom_data_store.remove(&key);
        }
      } else {
        self.custom_data_store.remove(&key);
      }
    }
  }

  fn remove_spent_utxos(&mut self, transfer: &TransferAsset) -> Result<(), PlatformError> {
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
      let mut block_builder = self.committed_state.start_block().unwrap();
      for txn in block {
        let eff = TxnEffect::compute_effect(txn.txn.clone()).unwrap();
        self.committed_state
            .apply_transaction(&mut block_builder, eff)
            .unwrap();
      }

      self.committed_state.finish_block(block_builder).unwrap()
    };
    // Next, update ownership status
    for (_, (txn_sid, txo_sids)) in finalized_block.iter() {
      let ledger = &mut self.committed_state;
      let curr_txn = ledger.get_transaction(*txn_sid).unwrap().finalized_txn.txn;
      // get the transaction, ownership addresses, and memos associated with each transaction
      let (addresses, owner_memos) = {
        let addresses: Vec<XfrAddress> = txo_sids.iter()
                                                 .map(|sid| {
                                                   XfrAddress { key: ((ledger.get_utxo(*sid)
                                                                             .unwrap()
                                                                             .utxo)
                                                                                   .0)
                                                                                      .0
                                                                                      .public_key }
                                                 })
                                                 .collect();

        let owner_memos = curr_txn.get_owner_memos_ref();

        (addresses, owner_memos)
      };

      // Update related addresses
      let related_addresses = get_related_addresses(&curr_txn);
      for address in &related_addresses {
        self.related_transactions
            .entry(*address)
            .or_insert_with(HashSet::new)
            .insert(*txn_sid);
      }

      // Add created asset and remove spent utxos
      for op in &curr_txn.body.operations {
        match op {
          Operation::DefineAsset(define_asset) => self.add_created_asset(&define_asset),
          Operation::IssueAsset(issue_asset) => self.cache_issuance(&issue_asset),
          Operation::TransferAsset(transfer_asset) => self.remove_spent_utxos(&transfer_asset)?,
          Operation::KVStoreUpdate(kv_update) => self.remove_stale_data(&kv_update),
          _ => {}
        };
      }

      // Add new utxos (this handles both transfers and issuances)
      for (txo_sid, (address, owner_memo)) in txo_sids.iter()
                                                      .zip(addresses.iter().zip(owner_memos.iter()))
      {
        self.addresses_to_utxos
            .entry(*address)
            .or_insert_with(HashSet::new)
            .insert(*txo_sid);
        self.utxos_to_map_index.insert(*txo_sid, *address);
        if let Some(owner_memo) = owner_memo {
          self.owner_memos.insert(*txo_sid, (*owner_memo).clone());
        }
      }
    }
    Ok(())
  }

  pub fn poll_new_blocks(&mut self) -> Result<(), PlatformError> {
    let latest_block = self.committed_state.get_block_count();
    let new_blocks = match self.rest_client.get_blocks_since(BlockSID(latest_block)) {
      Err(_) => {
        error!("Could not connect to ledger at {}",
               self.rest_client.get_source());
        return Err(fail!("Cannot connect to ledger server"));
      }

      Ok(blocks_and_sid) => blocks_and_sid,
    };

    for (bid, block) in new_blocks {
      info!("Received block {}", bid);
      self.add_new_block(&block)?;
    }

    Ok(())
  }
}

// An xfr address is related to a transaction if it is one of the following:
// 1. Owner of a transfer output
// 2. Transfer signer (owner of input or co-signer)
// 3. Signer of a an issuance txn
// 4. Signer of a kv_update txn
// 5. Signer of a memo_update txn
fn get_related_addresses(txn: &Transaction) -> HashSet<XfrAddress> {
  let mut related_addresses = HashSet::new();
  for op in &txn.body.operations {
    match op {
      Operation::TransferAsset(transfer) => {
        for input in transfer.body.transfer.inputs.iter() {
          related_addresses.insert(XfrAddress { key: input.public_key });
        }

        for output in transfer.body.transfer.outputs.iter() {
          related_addresses.insert(XfrAddress { key: output.public_key });
        }
      }
      Operation::IssueAsset(issue_asset) => {
        related_addresses.insert(XfrAddress { key: issue_asset.pubkey.key });
      }
      Operation::DefineAsset(define_asset) => {
        related_addresses.insert(XfrAddress { key: define_asset.pubkey.key });
      }
      Operation::UpdateMemo(update_memo) => {
        related_addresses.insert(XfrAddress { key: update_memo.pubkey });
      }
      Operation::AIRAssign(air_assign) => {
        related_addresses.insert(XfrAddress { key: air_assign.pubkey });
      }
      Operation::KVStoreUpdate(kv_store_update) => {
        if let Some(entry) = &kv_store_update.body.2 {
          related_addresses.insert(XfrAddress { key: entry.0 });
        }
      }
    }
  }
  related_addresses
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::{
    AssetRules, AssetTypeCode, BlockSID, KVHash, KVUpdate, Memo, TransferType,
  };
  use ledger::store::helpers::{apply_transaction, create_definition_transaction};
  use ledger_api_service::MockLedgerClient;
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use std::str;
  use std::sync::{Arc, RwLock};
  use txn_builder::{
    BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder,
  };
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::open_blind_asset_record;
  use zei::xfr::asset_record::AssetRecordType::{
    ConfidentialAmount_NonConfidentialAssetType, NonConfidentialAmount_NonConfidentialAssetType,
  };
  use zei::xfr::sig::XfrKeyPair;
  use zei::xfr::structs::AssetRecordTemplate;

  #[test]
  pub fn test_custom_data_store() {
    // This isn't actually being used in the test, we just make a ledger client so we can compile
    let client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
    let mut ledger_state = LedgerState::test_ledger();
    let mut prng = ChaChaRng::from_entropy();
    let mut query_server = QueryServer::new(MockLedgerClient::new(&client_ledger_state));
    let kp = XfrKeyPair::generate(&mut prng);

    let data = "some_data";
    let blind = KVBlind::gen_random();
    let hash = KVHash::new(&data, Some(&blind));
    let key = Key::gen_random(&mut prng);

    // Add hash to ledger and update query server
    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    builder.add_operation_kv_update(&kp, &key, 0, Some(&hash))
           .unwrap();
    let update_kv_tx = builder.transaction();
    apply_transaction(&mut ledger_state, update_kv_tx.clone());
    let block = ledger_state.get_block(BlockSID(0)).unwrap();
    query_server.add_new_block(&block.block.txns).unwrap();

    // Add data to query server
    let res = query_server.add_to_data_store(&key, &data, Some(&blind));
    assert!(res.is_ok());

    // Make sure data is there
    let fetched_data = query_server.get_custom_data(&key).unwrap();
    assert_eq!(str::from_utf8(&fetched_data.0).unwrap(), data);

    // Add incorrect  data to query server
    let wrong_data = "wrong_data";
    let res = query_server.add_to_data_store(&key, &wrong_data, Some(&blind));
    assert!(res.is_err());

    // Replace commitment
    let hash = KVHash::new(&String::from("new_data"), Some(&blind));
    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    builder.add_operation_kv_update(&kp, &key, 1, Some(&hash))
           .unwrap();
    let update_kv_tx = builder.transaction();
    apply_transaction(&mut ledger_state, update_kv_tx.clone());
    let block = ledger_state.get_block(BlockSID(1)).unwrap();
    query_server.add_new_block(&block.block.txns).unwrap();

    // Ensure stale data is removed
    assert!(query_server.get_custom_data(&key).is_none());
  }

  #[test]
  pub fn test_owner_memo_storage() {
    let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
    let mut ledger_state = LedgerState::test_ledger();
    let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
    let params = PublicParams::new();
    let mut prng = ChaChaRng::from_entropy();
    let mut query_server = QueryServer::new(mock_ledger);
    let token_code = AssetTypeCode::gen_random();
    // Define keys
    let alice = XfrKeyPair::generate(&mut prng);
    let bob = XfrKeyPair::generate(&mut prng);
    // Define asset
    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    let define_tx = builder.add_operation_create_asset(&alice,
                                                       Some(token_code),
                                                       AssetRules::default(),
                                                       "test".into(),
                                                       PolicyChoice::Fungible())
                           .unwrap()
                           .transaction();

    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);

    //Issuance txn
    let amt = 1000;
    let confidentiality_flag = ConfidentialAmount_NonConfidentialAssetType;
    let issuance_tx =
      builder.add_basic_issue_asset(&alice, &token_code, 0, amt, confidentiality_flag, &params)
             .unwrap()
             .add_basic_issue_asset(&alice, &token_code, 1, amt, confidentiality_flag, &params)
             .unwrap()
             .transaction();

    apply_transaction(&mut ledger_state, define_tx.clone());
    apply_transaction(&mut ledger_state, issuance_tx.clone());

    let block0 = ledger_state.get_block(BlockSID(0)).unwrap();
    let block1 = ledger_state.get_block(BlockSID(1)).unwrap();

    // Add new blocks to query server
    query_server.add_new_block(&block0.block.txns).unwrap();
    query_server.add_new_block(&block1.block.txns).unwrap();

    // Transfer first record to Bob
    let transfer_sid = TxoSID(0);
    let bar = &(ledger_state.get_utxo(transfer_sid).unwrap().utxo.0).0;
    let alice_memo = query_server.get_owner_memo(TxoSID(0));
    let oar = open_blind_asset_record(&bar, &alice_memo.cloned(), alice.get_sk_ref()).unwrap();
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
    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    let xfr_txn = builder.add_operation(xfr_op.transaction().unwrap())
                         .transaction();

    apply_transaction(&mut ledger_state, xfr_txn.clone());

    let block2 = ledger_state.get_block(BlockSID(2)).unwrap();

    // Query server will now fetch new blocks
    query_server.add_new_block(&block2.block.txns).unwrap();

    // Ensure that query server returns correct memos
    let bob_memo = query_server.get_owner_memo(TxoSID(2));
    let bar = &(ledger_state.get_utxo(TxoSID(2)).unwrap().utxo.0).0;
    open_blind_asset_record(&bar, &bob_memo.cloned(), alice.get_sk_ref()).unwrap();
  }

  #[test]
  pub fn test_record_storage() {
    let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
    let mut ledger_state = LedgerState::test_ledger();
    // This isn't actually being used in the test, we just make a ledger client so we can compile
    let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
    let mut prng = ChaChaRng::from_entropy();
    let mut query_server = QueryServer::new(mock_ledger);
    let params = PublicParams::new();
    let token_code = AssetTypeCode::gen_random();
    // Define keys
    let alice = XfrKeyPair::generate(&mut prng);
    let bob = XfrKeyPair::generate(&mut prng);
    // Define asset
    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    let define_tx = builder.add_operation_create_asset(&alice,
                                                       Some(token_code),
                                                       AssetRules::default(),
                                                       "fiat".into(),
                                                       PolicyChoice::Fungible())
                           .unwrap()
                           .transaction();

    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);

    //Issuance txn
    let amt = 1000;
    let confidentiality_flag = NonConfidentialAmount_NonConfidentialAssetType;
    let issuance_tx =
      builder.add_basic_issue_asset(&alice, &token_code, 0, amt, confidentiality_flag, &params)
             .unwrap()
             .add_basic_issue_asset(&alice, &token_code, 1, amt, confidentiality_flag, &params)
             .unwrap()
             .add_basic_issue_asset(&alice, &token_code, 2, amt, confidentiality_flag, &params)
             .unwrap()
             .transaction();

    apply_transaction(&mut ledger_state, define_tx.clone());
    apply_transaction(&mut ledger_state, issuance_tx.clone());

    // Transfer to Bob
    let transfer_sid = TxoSID(0);
    let bar = &(ledger_state.get_utxo(transfer_sid).unwrap().utxo.0).0;
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
    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
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
    let issuer_records = query_server.get_issued_records(&IssuerPublicKey { key: alice.get_pk()
                                                                                      .clone() })
                                     .unwrap();
    let alice_related_txns =
      query_server.get_related_transactions(&XfrAddress { key: *alice.get_pk_ref() })
                  .unwrap();

    assert!(issuer_records.len() == 3);
    assert!(!alice_sids.contains(&TxoSID(0)));
    assert!(alice_related_txns.contains(&TxnSID(0)));
    assert!(bob_sids.contains(&TxoSID(3)));
  }
  #[test]
  fn test_related_txns_memo_update() {
    let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
    let mut ledger_state = LedgerState::test_ledger();
    // This isn't actually being used in the test, we just make a ledger client so we can compile
    let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
    let mut query_server = QueryServer::new(mock_ledger);
    let code = AssetTypeCode::from_identical_byte(1);
    let creator = XfrKeyPair::generate(&mut ledger_state.get_prng());
    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    let asset_rules = AssetRules::default().set_updatable(true).clone();
    builder.add_operation_create_asset(&creator,
                                       Some(code),
                                       asset_rules,
                                       "test",
                                       PolicyChoice::Fungible())
           .unwrap();
    let tx = builder.transaction();
    apply_transaction(&mut ledger_state, tx.clone());

    // Change memo
    let seq_id = ledger_state.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    builder.add_operation_update_memo(&creator, code, "new_memo");
    let tx = builder.transaction();
    apply_transaction(&mut ledger_state, tx.clone());

    let block0 = ledger_state.get_block(BlockSID(0)).unwrap();
    let block1 = ledger_state.get_block(BlockSID(1)).unwrap();
    query_server.add_new_block(&block0.block.txns).unwrap();
    query_server.add_new_block(&block1.block.txns).unwrap();
    let related_txns =
      query_server.get_related_transactions(&XfrAddress { key: *creator.get_pk_ref() })
                  .unwrap();
    assert!(related_txns.contains(&TxnSID(1)));
  }
  #[test]
  fn test_related_txns_define_asset() {
    let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
    let mut ledger_state = LedgerState::test_ledger();
    // This isn't actually being used in the test, we just make a ledger client so we can compile
    let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
    let mut query_server = QueryServer::new(mock_ledger);
    let code = AssetTypeCode::from_identical_byte(1);
    let creator = XfrKeyPair::generate(&mut ledger_state.get_prng());
    let seq_id = ledger_state.get_block_commit_count();
    let tx = create_definition_transaction(&code,
                                           &creator,
                                           AssetRules::default(),
                                           Some(Memo("test".to_string())),
                                           seq_id).unwrap();
    apply_transaction(&mut ledger_state, tx);
    let block0 = ledger_state.get_block(BlockSID(0)).unwrap();
    query_server.add_new_block(&block0.block.txns).unwrap();
    let related_txns =
      query_server.get_related_transactions(&XfrAddress { key: *creator.get_pk_ref() })
                  .unwrap();
    assert!(related_txns.contains(&TxnSID(0)));
  }
  #[test]
  fn test_related_txns_kv_store_update() {
    let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
    let mut ledger_state = LedgerState::test_ledger();
    // This isn't actually being used in the test, we just make a ledger client so we can compile
    let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
    let mut prng = ChaChaRng::from_entropy();
    let kp = XfrKeyPair::generate(&mut prng);
    let mut query_server = QueryServer::new(mock_ledger);

    // KV update txn
    let data = [0u8, 16];
    let key = Key::gen_random(&mut prng);
    let hash = KVHash::new(&data, None);
    let update = KVUpdate::new((key, Some(hash)), 0, &kp);

    // Submit
    let seq_id = ledger_state.get_block_commit_count();
    let tx = Transaction::from_operation(Operation::KVStoreUpdate(update.clone()), seq_id);
    apply_transaction(&mut ledger_state, tx);

    // Check related txns
    let block0 = ledger_state.get_block(BlockSID(0)).unwrap();
    query_server.add_new_block(&block0.block.txns).unwrap();
    let related_txns = query_server.get_related_transactions(&XfrAddress { key:
                                                                             *kp.get_pk_ref() })
                                   .unwrap();
    assert!(related_txns.contains(&TxnSID(0)));
  }

  #[test]
  fn test_created_assets() {
    let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
    let mut ledger_state = LedgerState::test_ledger();
    // This isn't actually being used in the test, we just make a ledger client so we can compile
    let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
    let mut query_server = QueryServer::new(mock_ledger);
    let creator = XfrKeyPair::generate(&mut ledger_state.get_prng());

    // Create the first asset
    let code1 = AssetTypeCode::from_identical_byte(1);
    let seq_id = ledger_state.get_block_commit_count();
    let tx1 = create_definition_transaction(&code1,
                                            &creator,
                                            AssetRules::default(),
                                            Some(Memo("test".to_string())),
                                            seq_id).unwrap();
    apply_transaction(&mut ledger_state, tx1);
    let block1 = ledger_state.get_block(BlockSID(0)).unwrap();
    query_server.add_new_block(&block1.block.txns).unwrap();

    // Create the second asset
    let code2 = AssetTypeCode::from_identical_byte(2);
    let seq_id = ledger_state.get_block_commit_count();
    let tx2 = create_definition_transaction(&code2,
                                            &creator,
                                            AssetRules::default(),
                                            Some(Memo("test".to_string())),
                                            seq_id).unwrap();
    apply_transaction(&mut ledger_state, tx2);
    let block2 = ledger_state.get_block(BlockSID(1)).unwrap();
    query_server.add_new_block(&block2.block.txns).unwrap();

    // Verify the created assets
    let created_assets =
      query_server.get_created_assets(&IssuerPublicKey { key: *creator.get_pk_ref() })
                  .unwrap();
    assert_eq!(created_assets, &vec![code1, code2]);
  }
}
