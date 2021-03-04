use ledger::data_model::errors::PlatformError;
use ledger::data_model::*;
use ledger::store::*;
use ledger_api_service::RestfulArchiveAccess;
use log::{error, info};
use ruc::*;
use sparse_merkle_tree::Key;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::sync::Arc;
use utils::MetricsRenderer;
use zei::xfr::structs::OwnerMemo;

macro_rules! fail {
    () => {
        PlatformError::QueryServerError(None)
    };
    ($s:expr) => {
        PlatformError::QueryServerError(Some($s.to_string()))
    };
}

pub struct QueryServer<T, U>
where
    T: RestfulArchiveAccess,
    U: MetricsRenderer,
{
    committed_state: LedgerState,
    addresses_to_utxos: HashMap<XfrAddress, HashSet<TxoSID>>,
    related_transactions: HashMap<XfrAddress, HashSet<TxnSID>>, // Set of transactions related to a ledger address
    related_transfers: HashMap<AssetTypeCode, HashSet<TxnSID>>, // Set of transfer transactions related to an asset code
    created_assets: HashMap<IssuerPublicKey, Vec<DefineAsset>>,
    traced_assets: HashMap<IssuerPublicKey, Vec<AssetTypeCode>>, // List of assets traced by a ledger address
    issuances: HashMap<IssuerPublicKey, Vec<Arc<(TxOutput, Option<OwnerMemo>)>>>, // issuance mapped by public key
    token_code_issuances:
        HashMap<AssetTypeCode, Vec<Arc<(TxOutput, Option<OwnerMemo>)>>>, // issuance mapped by token code
    owner_memos: HashMap<TxoSID, OwnerMemo>,
    utxos_to_map_index: HashMap<TxoSID, XfrAddress>,
    custom_data_store: HashMap<Key, (Vec<u8>, KVHash)>,
    rest_client: T,
    metrics_renderer: U,
}

impl<T, U> QueryServer<T, U>
where
    T: RestfulArchiveAccess,
    U: MetricsRenderer,
{
    pub fn new(rest_client: T, metrics_renderer: U) -> QueryServer<T, U> {
        QueryServer {
            committed_state: LedgerState::test_ledger(),
            addresses_to_utxos: HashMap::new(),
            related_transactions: HashMap::new(),
            related_transfers: HashMap::new(),
            owner_memos: HashMap::new(),
            created_assets: HashMap::new(),
            traced_assets: HashMap::new(),
            issuances: HashMap::new(),
            token_code_issuances: HashMap::new(),
            utxos_to_map_index: HashMap::new(),
            custom_data_store: HashMap::new(),
            rest_client,
            metrics_renderer,
        }
    }

    pub fn render(&self) -> String {
        self.metrics_renderer.rendered()
    }

    // Fetch custom data at a given key.
    pub fn get_custom_data(&self, key: &Key) -> Option<&(Vec<u8>, KVHash)> {
        self.custom_data_store.get(key)
    }

    // Returns the set of records issued by a certain key.
    pub fn get_issued_records(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.issuances
            .get(issuer)
            .map(|recs| recs.iter().map(|rec| rec.deref().clone()).collect())
    }

    // Returns the set of records issued by a certain token code.
    pub fn get_issued_records_by_code(
        &self,
        code: &AssetTypeCode,
    ) -> Option<Vec<(TxOutput, Option<OwnerMemo>)>> {
        self.token_code_issuances
            .get(code)
            .map(|recs| recs.iter().map(|rec| rec.deref().clone()).collect())
    }

    pub fn get_created_assets(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<&Vec<DefineAsset>> {
        self.created_assets.get(issuer)
    }

    // Returns the list of assets traced by a certain key.
    pub fn get_traced_assets(
        &self,
        issuer: &IssuerPublicKey,
    ) -> Option<&Vec<AssetTypeCode>> {
        self.traced_assets.get(issuer)
    }

    // Returns the set of transactions that are in some way related to a given ledger address.
    // An xfr address is related to a transaction if it is one of the following:
    // 1. Owner of a transfer output
    // 2. Transfer signer (owner of input or co-signer)
    // 3. Signer of a an issuance txn
    // 4. Signer of a kv_update txn
    // 5. Signer of a memo_update txn
    pub fn get_related_transactions(
        &self,
        address: &XfrAddress,
    ) -> Option<&HashSet<TxnSID>> {
        self.related_transactions.get(&address)
    }

    // Returns the set of transfer transactions that are associated with a given asset.
    // The asset type must be nonconfidential.
    pub fn get_related_transfers(
        &self,
        code: &AssetTypeCode,
    ) -> Option<&HashSet<TxnSID>> {
        self.related_transfers.get(&code)
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
    pub fn add_to_data_store(
        &mut self,
        key: &Key,
        data: &dyn AsRef<[u8]>,
        blind: Option<&KVBlind>,
    ) -> Result<()> {
        let hash = KVHash::new(data, blind);
        let auth_entry = self.committed_state.get_kv_entry(*key);

        let result = auth_entry.result.c(d!(fail!(
            "Nothing found in the custom data store at this key"
        )))?;
        let entry_hash = result.deserialize().1.c(d!(fail!("Nothing found in the custom data store at this key. A hash was once here, but has been removed")))?.1;

        // Ensure that hash matches
        if hash != entry_hash {
            return Err(eg!(fail!(
                "The hash of the data supplied does not match the hash stored by the ledger"
            )));
        }

        // Hash matches, store data
        self.custom_data_store
            .insert(*key, (data.as_ref().into(), hash));
        Ok(())
    }

    // Add created asset
    pub fn add_created_asset(&mut self, creation: &DefineAsset) {
        let issuer = creation.pubkey;
        self.created_assets
            .entry(issuer)
            .or_insert_with(Vec::new)
            .push(creation.clone());
    }

    // Add traced asset
    pub fn add_traced_asset(&mut self, creation: &DefineAsset) {
        let tracing_policies = &creation.body.asset.asset_rules.tracing_policies;
        if !tracing_policies.is_empty() {
            let issuer = creation.pubkey;
            let new_asset_code = creation.body.asset.code;
            self.traced_assets
                .entry(issuer)
                .or_insert_with(Vec::new)
                .push(new_asset_code);
        }
    }

    // Cache issuance records
    pub fn cache_issuance(&mut self, issuance: &IssueAsset) {
        let new_records: Vec<Arc<(TxOutput, Option<OwnerMemo>)>> = issuance
            .body
            .records
            .iter()
            .map(|rec| Arc::new(rec.clone()))
            .collect();

        macro_rules! save_issuance {
            ($maps: tt, $key: tt) => {
                let records = $maps.entry($key).or_insert_with(Vec::new);
                records.extend_from_slice(&new_records);
            };
        }

        let key_issuances = &mut self.issuances;
        let pubkey = issuance.pubkey;
        save_issuance!(key_issuances, pubkey);

        let token_issuances = &mut self.token_code_issuances;
        let token_code = issuance.body.code;
        save_issuance!(token_issuances, token_code);
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

    fn remove_spent_utxos(&mut self, transfer: &TransferAsset) -> Result<()> {
        for input in &transfer.body.inputs {
            match input {
                TxoRef::Relative(_) => {} // Relative utxos were never cached so no need to do anything here
                TxoRef::Absolute(txo_sid) => {
                    let address = self.utxos_to_map_index.get(&txo_sid).c(d!(fail!(
                        "Attempting to remove owned txo of address that isn't cached"
                    )))?;
                    let hash_set = self
                        .addresses_to_utxos
                        .get_mut(&address)
                        .c(d!(fail!("No txos stored for this address")))?;
                    let removed = hash_set.remove(&txo_sid);
                    if !removed {
                        return Err(eg!(fail!("Input txo not found")));
                    }
                }
            }
        }
        Ok(())
    }

    // Updates query server cache with new transactions from a block.
    // Each new block must be consistent with the state of the cached ledger up until this point
    pub fn add_new_block(&mut self, block: &[FinalizedTransaction]) -> Result<()> {
        // First, we add block to local ledger state
        let finalized_block = {
            let mut block_builder = self.committed_state.start_block().c(d!())?;
            for txn in block {
                let eff = TxnEffect::compute_effect(txn.txn.clone()).c(d!())?;
                self.committed_state
                    .apply_transaction(&mut block_builder, eff)
                    .c(d!())?;
            }

            self.committed_state.finish_block(block_builder).c(d!())?
        };
        // Next, update ownership status
        for (_, (txn_sid, txo_sids)) in finalized_block.iter() {
            let ledger = &mut self.committed_state;
            let curr_txn = ledger.get_transaction(*txn_sid).unwrap().finalized_txn.txn;
            // get the transaction, ownership addresses, and memos associated with each transaction
            let (addresses, owner_memos) = {
                let addresses: Vec<XfrAddress> = txo_sids
                    .iter()
                    .map(|sid| XfrAddress {
                        key: ((ledger.get_utxo(*sid).unwrap().utxo).0).record.public_key,
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

            // Update transferred nonconfidential assets
            let transferred_assets = get_transferred_nonconfidential_assets(&curr_txn);
            for asset in &transferred_assets {
                self.related_transfers
                    .entry(*asset)
                    .or_insert_with(HashSet::new)
                    .insert(*txn_sid);
            }

            // Add created asset and remove spent utxos
            for op in &curr_txn.body.operations {
                match op {
                    Operation::DefineAsset(define_asset) => {
                        self.add_created_asset(&define_asset);
                        self.add_traced_asset(&define_asset);
                    }
                    Operation::IssueAsset(issue_asset) => {
                        self.cache_issuance(&issue_asset)
                    }
                    Operation::TransferAsset(transfer_asset) => {
                        self.remove_spent_utxos(&transfer_asset).c(d!())?
                    }
                    Operation::KVStoreUpdate(kv_update) => {
                        self.remove_stale_data(&kv_update)
                    }
                    _ => {}
                };
            }

            // Add new utxos (this handles both transfers and issuances)
            for (txo_sid, (address, owner_memo)) in txo_sids
                .iter()
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

    pub fn poll_new_blocks(&mut self) -> Result<()> {
        let latest_block = self.committed_state.get_block_count();
        let new_blocks = match self.rest_client.get_blocks_since(BlockSID(latest_block))
        {
            Err(_) => {
                error!(
                    "Could not connect to ledger at {}",
                    self.rest_client.get_source()
                );
                return Err(eg!(fail!("Cannot connect to ledger server")));
            }

            Ok(blocks_and_sid) => blocks_and_sid,
        };

        for (bid, block) in new_blocks {
            info!("Received block {}", bid);
            self.add_new_block(&block).c(d!())?;
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
                    related_addresses.insert(XfrAddress {
                        key: input.public_key,
                    });
                }

                for output in transfer.body.transfer.outputs.iter() {
                    related_addresses.insert(XfrAddress {
                        key: output.public_key,
                    });
                }
            }
            Operation::IssueAsset(issue_asset) => {
                related_addresses.insert(XfrAddress {
                    key: issue_asset.pubkey.key,
                });
            }
            Operation::BindAssets(_bind_assets) => {
                unimplemented!();
            }
            Operation::ReleaseAssets(_release_assets) => {
                unimplemented!();
            }
            Operation::DefineAsset(define_asset) => {
                related_addresses.insert(XfrAddress {
                    key: define_asset.pubkey.key,
                });
            }
            Operation::UpdateMemo(update_memo) => {
                related_addresses.insert(XfrAddress {
                    key: update_memo.pubkey,
                });
            }
            Operation::AIRAssign(air_assign) => {
                related_addresses.insert(XfrAddress {
                    key: air_assign.pubkey,
                });
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

// Returns the set of nonconfidential assets transferred in a transaction.
fn get_transferred_nonconfidential_assets(txn: &Transaction) -> HashSet<AssetTypeCode> {
    let mut transferred_assets = HashSet::new();
    for op in &txn.body.operations {
        if let Operation::TransferAsset(transfer) = op {
            for input in transfer.body.transfer.inputs.iter() {
                if let Some(asset_type) = input.asset_type.get_asset_type() {
                    transferred_assets.insert(AssetTypeCode { val: asset_type });
                }
            }
        }
    }
    transferred_assets
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
    use utils::MockMetricsRenderer;
    use zei::setup::PublicParams;
    use zei::xfr::asset_record::open_blind_asset_record;
    use zei::xfr::asset_record::AssetRecordType::{
        ConfidentialAmount_NonConfidentialAssetType,
        NonConfidentialAmount_NonConfidentialAssetType,
    };
    use zei::xfr::sig::XfrKeyPair;
    use zei::xfr::structs::{AssetRecordTemplate, AssetTracerKeyPair, TracingPolicy};

    #[test]
    pub fn test_custom_data_store() {
        // This isn't actually being used in the test, we just make a ledger client so we can compile
        let client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
        let mut ledger_state = LedgerState::test_ledger();
        let mut prng = ChaChaRng::from_entropy();
        let mut query_server = QueryServer::new(
            MockLedgerClient::new(&client_ledger_state),
            MockMetricsRenderer::new(),
        );
        let kp = XfrKeyPair::generate(&mut prng);

        let data = "some_data";
        let blind = KVBlind::gen_random();
        let hash = KVHash::new(&data, Some(&blind));
        let key = Key::gen_random(&mut prng);

        // Add hash to ledger and update query server
        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);
        builder
            .add_operation_kv_update(&kp, &key, 0, Some(&hash))
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
        builder
            .add_operation_kv_update(&kp, &key, 1, Some(&hash))
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
        let params = PublicParams::default();
        let mut prng = ChaChaRng::from_entropy();
        let mut query_server = QueryServer::new(mock_ledger, MockMetricsRenderer::new());
        let token_code = AssetTypeCode::gen_random();
        // Define keys
        let alice = XfrKeyPair::generate(&mut prng);
        let bob = XfrKeyPair::generate(&mut prng);
        // Define asset
        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);
        let define_tx = builder
            .add_operation_create_asset(
                &alice,
                Some(token_code),
                AssetRules::default(),
                "test".into(),
                PolicyChoice::Fungible(),
            )
            .unwrap()
            .transaction();

        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);

        //Issuance txn
        let amt = 1000;
        let confidentiality_flag = ConfidentialAmount_NonConfidentialAssetType;
        let issuance_tx = builder
            .add_basic_issue_asset(
                &alice,
                &token_code,
                0,
                amt,
                confidentiality_flag,
                &params,
            )
            .unwrap()
            .add_basic_issue_asset(
                &alice,
                &token_code,
                1,
                amt,
                confidentiality_flag,
                &params,
            )
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
        let bar = &(ledger_state.get_utxo(transfer_sid).unwrap().utxo.0).record;
        let alice_memo = query_server.get_owner_memo(TxoSID(0));
        let oar = open_blind_asset_record(&bar, &alice_memo.cloned(), &alice).unwrap();
        let mut xfr_builder = TransferOperationBuilder::new();
        let out_template = AssetRecordTemplate::with_no_asset_tracing(
            amt,
            token_code.val,
            oar.get_record_type(),
            bob.get_pk(),
        );
        let xfr_op = xfr_builder
            .add_input(TxoRef::Absolute(transfer_sid), oar, None, None, amt)
            .unwrap()
            .add_output(&out_template, None, None, None)
            .unwrap()
            .create(TransferType::Standard)
            .unwrap()
            .sign(&alice)
            .unwrap();
        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);
        let xfr_txn = builder
            .add_operation(xfr_op.transaction().unwrap())
            .transaction();

        apply_transaction(&mut ledger_state, xfr_txn.clone());

        let block2 = ledger_state.get_block(BlockSID(2)).unwrap();

        // Query server will now fetch new blocks
        query_server.add_new_block(&block2.block.txns).unwrap();

        // Ensure that query server returns correct memos
        let bob_memo = query_server.get_owner_memo(TxoSID(2));
        let bar = &(ledger_state.get_utxo(TxoSID(2)).unwrap().utxo.0).record;
        open_blind_asset_record(&bar, &bob_memo.cloned(), &alice).unwrap();
    }

    #[test]
    pub fn test_record_storage() {
        let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
        let mut ledger_state = LedgerState::test_ledger();
        // This isn't actually being used in the test, we just make a ledger client so we can compile
        let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
        let mut prng = ChaChaRng::from_entropy();
        let mut query_server = QueryServer::new(mock_ledger, MockMetricsRenderer::new());
        let params = PublicParams::default();
        let token_code = AssetTypeCode::gen_random();
        // Define keys
        let alice = XfrKeyPair::generate(&mut prng);
        let bob = XfrKeyPair::generate(&mut prng);
        // Define asset
        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);
        let define_tx = builder
            .add_operation_create_asset(
                &alice,
                Some(token_code),
                AssetRules::default(),
                "fiat".into(),
                PolicyChoice::Fungible(),
            )
            .unwrap()
            .transaction();

        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);

        //Issuance txn
        let amt = 1000;
        let confidentiality_flag = NonConfidentialAmount_NonConfidentialAssetType;
        let issuance_tx = builder
            .add_basic_issue_asset(
                &alice,
                &token_code,
                0,
                amt,
                confidentiality_flag,
                &params,
            )
            .unwrap()
            .add_basic_issue_asset(
                &alice,
                &token_code,
                1,
                amt,
                confidentiality_flag,
                &params,
            )
            .unwrap()
            .add_basic_issue_asset(
                &alice,
                &token_code,
                2,
                amt,
                confidentiality_flag,
                &params,
            )
            .unwrap()
            .transaction();

        apply_transaction(&mut ledger_state, define_tx.clone());
        apply_transaction(&mut ledger_state, issuance_tx.clone());

        // Transfer to Bob
        let transfer_sid = TxoSID(0);
        let bar = &(ledger_state.get_utxo(transfer_sid).unwrap().utxo.0).record;
        let oar = open_blind_asset_record(&bar, &None, &alice).unwrap();
        let mut xfr_builder = TransferOperationBuilder::new();
        let out_template = AssetRecordTemplate::with_no_asset_tracing(
            amt,
            token_code.val,
            oar.get_record_type(),
            bob.get_pk(),
        );
        let xfr_op = xfr_builder
            .add_input(TxoRef::Absolute(transfer_sid), oar, None, None, amt)
            .unwrap()
            .add_output(&out_template, None, None, None)
            .unwrap()
            .create(TransferType::Standard)
            .unwrap()
            .sign(&alice)
            .unwrap();
        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);
        let xfr_txn = builder
            .add_operation(xfr_op.transaction().unwrap())
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
        let alice_sids = query_server
            .get_owned_utxo_sids(&XfrAddress {
                key: *alice.get_pk_ref(),
            })
            .unwrap();

        assert!(alice_sids.contains(&TxoSID(0)));
        assert!(alice_sids.contains(&TxoSID(1)));
        assert!(alice_sids.contains(&TxoSID(2)));

        query_server.add_new_block(&block2.block.txns).unwrap();

        // Ensure that query server is aware of ownership changes
        let alice_sids = query_server
            .get_owned_utxo_sids(&XfrAddress {
                key: *alice.get_pk_ref(),
            })
            .unwrap();
        let bob_sids = query_server
            .get_owned_utxo_sids(&XfrAddress {
                key: *bob.get_pk_ref(),
            })
            .unwrap();
        let issuer_records = query_server
            .get_issued_records(&IssuerPublicKey {
                key: alice.get_pk().clone(),
            })
            .unwrap();
        let token_records = query_server
            .get_issued_records_by_code(&token_code)
            .unwrap();
        let alice_related_txns = query_server
            .get_related_transactions(&XfrAddress {
                key: *alice.get_pk_ref(),
            })
            .unwrap();

        assert!(issuer_records.len() == 3);
        assert!(token_records.len() == 3);
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
        let mut query_server = QueryServer::new(mock_ledger, MockMetricsRenderer::new());
        let code = AssetTypeCode::gen_random();
        let creator = XfrKeyPair::generate(&mut ledger_state.get_prng());
        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);
        let asset_rules = AssetRules::default().set_updatable(true).clone();
        builder
            .add_operation_create_asset(
                &creator,
                Some(code),
                asset_rules,
                "test",
                PolicyChoice::Fungible(),
            )
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
        let related_txns = query_server
            .get_related_transactions(&XfrAddress {
                key: *creator.get_pk_ref(),
            })
            .unwrap();
        assert!(related_txns.contains(&TxnSID(1)));
    }
    #[test]
    fn test_related_txns_define_asset() {
        let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
        let mut ledger_state = LedgerState::test_ledger();
        // This isn't actually being used in the test, we just make a ledger client so we can compile
        let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
        let mut query_server = QueryServer::new(mock_ledger, MockMetricsRenderer::new());
        let code = AssetTypeCode::gen_random();
        let creator = XfrKeyPair::generate(&mut ledger_state.get_prng());
        let seq_id = ledger_state.get_block_commit_count();
        let tx = create_definition_transaction(
            &code,
            &creator,
            AssetRules::default(),
            Some(Memo("test".to_string())),
            seq_id,
        )
        .unwrap();
        apply_transaction(&mut ledger_state, tx);
        let block0 = ledger_state.get_block(BlockSID(0)).unwrap();
        query_server.add_new_block(&block0.block.txns).unwrap();
        let related_txns = query_server
            .get_related_transactions(&XfrAddress {
                key: *creator.get_pk_ref(),
            })
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
        let mut query_server = QueryServer::new(mock_ledger, MockMetricsRenderer::new());

        // KV update txn
        let data = [0u8, 16];
        let key = Key::gen_random(&mut prng);
        let hash = KVHash::new(&data, None);
        let update = KVUpdate::new((key, Some(hash)), 0, &kp);

        // Submit
        let seq_id = ledger_state.get_block_commit_count();
        let tx = Transaction::from_operation(
            Operation::KVStoreUpdate(update.clone()),
            seq_id,
        );
        apply_transaction(&mut ledger_state, tx);

        // Check related txns
        let block0 = ledger_state.get_block(BlockSID(0)).unwrap();
        query_server.add_new_block(&block0.block.txns).unwrap();
        let related_txns = query_server
            .get_related_transactions(&XfrAddress {
                key: *kp.get_pk_ref(),
            })
            .unwrap();
        assert!(related_txns.contains(&TxnSID(0)));
    }

    #[test]
    pub fn test_related_xfrs() {
        let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
        let mut ledger_state = LedgerState::test_ledger();
        // This isn't actually being used in the test, we just make a ledger client so we can compile
        let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
        let mut prng = ChaChaRng::from_entropy();
        let mut query_server = QueryServer::new(mock_ledger, MockMetricsRenderer::new());
        let params = PublicParams::default();
        let token_code = AssetTypeCode::gen_random();
        // Define keys
        let alice = XfrKeyPair::generate(&mut prng);
        let bob = XfrKeyPair::generate(&mut prng);
        // Define asset
        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);
        let define_tx = builder
            .add_operation_create_asset(
                &alice,
                Some(token_code),
                AssetRules::default(),
                "fiat".into(),
                PolicyChoice::Fungible(),
            )
            .unwrap()
            .transaction();

        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);

        //Issuance txn
        let amt = 1000;
        let confidentiality_flag = NonConfidentialAmount_NonConfidentialAssetType;
        let issuance_tx = builder
            .add_basic_issue_asset(
                &alice,
                &token_code,
                0,
                amt,
                confidentiality_flag,
                &params,
            )
            .unwrap()
            .transaction();

        apply_transaction(&mut ledger_state, define_tx.clone());
        apply_transaction(&mut ledger_state, issuance_tx.clone());

        // Transfer to Bob
        let transfer_sid = TxoSID(0);
        let bar = &(ledger_state.get_utxo(transfer_sid).unwrap().utxo.0).record;
        let oar = open_blind_asset_record(&bar, &None, &alice).unwrap();
        let mut xfr_builder = TransferOperationBuilder::new();
        let out_template = AssetRecordTemplate::with_no_asset_tracing(
            amt,
            token_code.val,
            oar.get_record_type(),
            bob.get_pk(),
        );
        let xfr_op = xfr_builder
            .add_input(TxoRef::Absolute(transfer_sid), oar, None, None, amt)
            .unwrap()
            .add_output(&out_template, None, None, None)
            .unwrap()
            .create(TransferType::Standard)
            .unwrap()
            .sign(&alice)
            .unwrap();
        let seq_id = ledger_state.get_block_commit_count();
        let mut builder = TransactionBuilder::from_seq_id(seq_id);
        let xfr_txn = builder
            .add_operation(xfr_op.transaction().unwrap())
            .transaction();

        apply_transaction(&mut ledger_state, xfr_txn.clone());

        let block0 = ledger_state.get_block(BlockSID(0)).unwrap();
        let block1 = ledger_state.get_block(BlockSID(1)).unwrap();
        let block2 = ledger_state.get_block(BlockSID(2)).unwrap();

        // Query server will now fetch new blocks
        query_server.add_new_block(&block0.block.txns).unwrap();
        query_server.add_new_block(&block1.block.txns).unwrap();
        query_server.add_new_block(&block2.block.txns).unwrap();

        // Verify the related transfer
        let related_xfr = query_server.get_related_transfers(&token_code).unwrap();
        assert_eq!(related_xfr.len(), 1);
        assert!(related_xfr.contains(&TxnSID(2)));
    }

    #[test]
    fn test_created_assets() {
        let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
        let mut ledger_state = LedgerState::test_ledger();
        // This isn't actually being used in the test, we just make a ledger client so we can compile
        let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
        let mut query_server = QueryServer::new(mock_ledger, MockMetricsRenderer::new());
        let creator = XfrKeyPair::generate(&mut ledger_state.get_prng());

        // Create the first asset
        let code1 = AssetTypeCode::gen_random();
        let seq_id = ledger_state.get_block_commit_count();
        let tx1 = create_definition_transaction(
            &code1,
            &creator,
            AssetRules::default(),
            Some(Memo("test".to_string())),
            seq_id,
        )
        .unwrap();
        apply_transaction(&mut ledger_state, tx1);
        let block1 = ledger_state.get_block(BlockSID(0)).unwrap();
        query_server.add_new_block(&block1.block.txns).unwrap();

        // Create the second asset
        let code2 = AssetTypeCode::gen_random();
        let seq_id = ledger_state.get_block_commit_count();
        let tx2 = create_definition_transaction(
            &code2,
            &creator,
            AssetRules::default(),
            Some(Memo("test".to_string())),
            seq_id,
        )
        .unwrap();
        apply_transaction(&mut ledger_state, tx2);
        let block2 = ledger_state.get_block(BlockSID(1)).unwrap();
        query_server.add_new_block(&block2.block.txns).unwrap();

        // Verify the created assets
        let created_assets = query_server
            .get_created_assets(&IssuerPublicKey {
                key: *creator.get_pk_ref(),
            })
            .unwrap();

        // Check if the created assets have two asset
        let asset_found = |code: &AssetTypeCode| -> bool {
            created_assets.iter().any(|ca| ca.body.asset.code == *code)
        };
        assert!(asset_found(&code1));
        assert!(asset_found(&code2));
    }

    #[test]
    fn test_traced_assets() {
        let rest_client_ledger_state = Arc::new(RwLock::new(LedgerState::test_ledger()));
        let mut ledger_state = LedgerState::test_ledger();
        // This isn't actually being used in the test, we just make a ledger client so we can compile
        let mock_ledger = MockLedgerClient::new(&Arc::clone(&rest_client_ledger_state));
        let mut query_server = QueryServer::new(mock_ledger, MockMetricsRenderer::new());
        let creator = XfrKeyPair::generate(&mut ledger_state.get_prng());

        // Set the tracing policy
        let tracer_kp = AssetTracerKeyPair::generate(&mut ledger_state.get_prng());
        let tracing_policy = TracingPolicy {
            enc_keys: tracer_kp.enc_key.clone(),
            asset_tracing: true,
            identity_tracing: None,
        };

        // Create the first traceable asset
        let code1 = AssetTypeCode::gen_random();
        let tx1 = create_definition_transaction(
            &code1,
            &creator,
            AssetRules::default()
                .add_tracing_policy(tracing_policy.clone())
                .clone(),
            Some(Memo("test".to_string())),
            ledger_state.get_block_commit_count(),
        )
        .unwrap();
        apply_transaction(&mut ledger_state, tx1);
        let block1 = ledger_state.get_block(BlockSID(0)).unwrap();
        query_server.add_new_block(&block1.block.txns).unwrap();

        // Create the second traceable asset
        let code2 = AssetTypeCode::gen_random();
        let tx2 = create_definition_transaction(
            &code2,
            &creator,
            AssetRules::default()
                .add_tracing_policy(tracing_policy)
                .clone(),
            Some(Memo("test".to_string())),
            ledger_state.get_block_commit_count(),
        )
        .unwrap();
        apply_transaction(&mut ledger_state, tx2);
        let block2 = ledger_state.get_block(BlockSID(1)).unwrap();
        query_server.add_new_block(&block2.block.txns).unwrap();

        // Create a non-traceable asset
        let code3 = AssetTypeCode::gen_random();
        let tx3 = create_definition_transaction(
            &code3,
            &creator,
            AssetRules::default(),
            Some(Memo("test".to_string())),
            ledger_state.get_block_commit_count(),
        )
        .unwrap();
        apply_transaction(&mut ledger_state, tx3);
        let block3 = ledger_state.get_block(BlockSID(2)).unwrap();
        query_server.add_new_block(&block3.block.txns).unwrap();

        // Verify that the list of traced assets contains only the first and second assets
        let traced_assets = query_server
            .get_traced_assets(&IssuerPublicKey {
                key: *creator.get_pk_ref(),
            })
            .unwrap();
        assert_eq!(traced_assets, &vec![code1, code2]);
    }
}
