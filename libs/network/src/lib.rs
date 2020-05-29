use actix_http::Request;
use actix_service::Service;
use actix_web::dev::ServiceResponse;
use actix_web::{test, web, App};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  AssetType, AssetTypeCode, AuthenticatedKVLookup, AuthenticatedUtxoStatus, StateCommitmentData,
  Transaction, TxoSID, Utxo,
};
use ledger::store::{LedgerAccess, LedgerState};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::{Deserialize, Serialize};
use sparse_merkle_tree::Key;
use std::sync::{Arc, RwLock};
use submission_api::{force_end_block, submit_transaction, txn_status, SubmissionApi};
use submission_server::{SubmissionServer, TxnHandle, TxnStatus};
use utils::{HashOf, ProofOf, Serialized, SignatureOf};
use zei::xfr::sig::XfrPublicKey;

// Trait for rest clients that can submit transactions
trait SubmitsTransactions {
  // Forward transaction to a transaction submission server, returning a handle to the transaction
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError>;
  // Forces the the validator to end the block. Useful for testing when it is desirable to commit
  // txns to the ledger as soon as possible
  fn force_end_block(&mut self) -> Result<(), PlatformError>;
  // Get txn status from a handle
  fn txn_status(&self, handle: &TxnHandle) -> TxnStatus;
}

// this will do all the actix stuff
pub struct RestClient {
  submit_port: usize,
  ledger_port: usize,
  client: reqwest::Client,
}

//impl LedgerAccess for RestClient {}
//impl LedgerUpdate for RestClient {}
//impl ArchiveAccess for RestClient {}

/// Mock rest client that simulates http requests, no ports required
pub struct MockRestClient {
  mock_submission_server: Arc<RwLock<SubmissionServer<ChaChaRng, LedgerState>>>,
  mock_ledger: Arc<RwLock<LedgerState>>,
}

impl MockRestClient {
  pub fn new(block_capacity: usize) -> Self {
    let mut prng = ChaChaRng::from_entropy();
    let ledger_state = LedgerState::test_ledger();
    let state_lock = Arc::new(RwLock::new(ledger_state));
    let mock_ledger = Arc::clone(&state_lock);

    let mock_submission_server =
      Arc::new(RwLock::new(SubmissionServer::new(prng, state_lock, block_capacity).unwrap()));

    MockRestClient { mock_submission_server,
                     mock_ledger }
  }
}

impl SubmitsTransactions for MockRestClient {
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError> {
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_submission_server))
                                   .route("/submit_transaction",
                                          web::post().to(submit_transaction::<rand_chacha::ChaChaRng,
                                                                            LedgerState>)));
    let req = test::TestRequest::post().uri("/submit_transaction")
                                       .set_json(&txn)
                                       .to_request();
    let handle: TxnHandle = test::read_response_json(&mut app, req);
    Ok(handle)
  }

  fn force_end_block(&mut self) -> Result<(), PlatformError> {
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_submission_server))
                                   .route("/force_end_block",
                                          web::post().to(force_end_block::<rand_chacha::ChaChaRng,
                                                                            LedgerState>)));
    let req = test::TestRequest::post().uri("/force_end_block")
                                       .to_request();
    let submit_resp = test::block_on(app.call(req)).unwrap();
    Ok(())
  }

  fn txn_status(&self, handle: &TxnHandle) -> TxnStatus {
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_submission_server))
                                   .route("/txn_status/{handle}",
                                          web::get().to(txn_status::<rand_chacha::ChaChaRng,
                                                                   LedgerState>)));
    let req = test::TestRequest::get().uri(format!("/txn_status/{}", handle.0).as_str())
                                      .to_request();
    let status: TxnStatus = test::read_response_json(&mut app, req);
    status
  }
}

impl LedgerAccess for MockRestClient {
  fn get_utxo(&self, addr: TxoSID) -> Option<&Utxo> {
    unimplemented!();
  }

  fn get_issuance_num(&self, code: &AssetTypeCode) -> Option<u64> {
    unimplemented!();
  }

  fn get_asset_type(&self, code: &AssetTypeCode) -> Option<&AssetType> {
    unimplemented!();
  }

  fn get_state_commitment(&self) -> (HashOf<Option<StateCommitmentData>>, u64) {
    unimplemented!();
  }

  fn get_utxo_status(&mut self, addr: TxoSID) -> AuthenticatedUtxoStatus {
    unimplemented!();
  }

  fn get_kv_entry(&self, addr: Key) -> AuthenticatedKVLookup {
    unimplemented!();
  }

  fn public_key(&self) -> &XfrPublicKey {
    unimplemented!();
  }

  fn sign_message<T: Serialize + serde::de::DeserializeOwned>(&self, msg: &T) -> SignatureOf<T> {
    unimplemented!();
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_mock_client() {
    let tx = Transaction::default();
    let mut mock_rest_client = MockRestClient::new(2);
    let handle = mock_rest_client.submit_transaction(&tx).unwrap();
    mock_rest_client.force_end_block().unwrap();
    let status = mock_rest_client.txn_status(&handle);
    if let TxnStatus::Committed(_comm) = status {
      assert!(true);
    } else {
      assert!(false);
    }
  }
}

//impl LedgerAccess for MockRestClient {}
//impl ArchiveAccess for MockRestClient {}
