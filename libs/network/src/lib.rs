use actix_http::Request;
use actix_service::Service;
use actix_web::dev::ServiceResponse;
use actix_web::test::TestRequest;
use actix_web::{test, web, App};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  AssetType, AssetTypeCode, AuthenticatedKVLookup, AuthenticatedUtxoStatus, StateCommitmentData,
  Transaction, TxoSID, Utxo,
};
use ledger::store::{LedgerAccess, LedgerState};
use ledger_api_service::{
  query_asset, query_asset_issuance_num, query_global_state, query_utxo, LedgerAccessRoutes,
  LedgerArchiveRoutes,
};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::{Deserialize, Serialize};
use sparse_merkle_tree::Key;
use std::sync::{Arc, RwLock};
use submission_api::{
  force_end_block, submit_transaction, txn_status, SubmissionApi, SubmissionRoutes,
};
use submission_server::{SubmissionServer, TxnHandle, TxnStatus};
use utils::{HashOf, NetworkRoute, ProofOf, Serialized, SignatureOf};
use zei::xfr::sig::XfrPublicKey;

// Trait for rest clients that can submit transactions
pub trait RestfulLedgerUpdate {
  // Forward transaction to a transaction submission server, returning a handle to the transaction
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError>;
  // Forces the the validator to end the block. Useful for testing when it is desirable to commit
  // txns to the ledger as soon as possible
  fn force_end_block(&mut self) -> Result<(), PlatformError>;
  // Get txn status from a handle
  fn txn_status(&self, handle: &TxnHandle) -> TxnStatus;
}

pub trait RestfulLedgerAccess {
  fn get_utxo(&self, addr: TxoSID) -> Utxo;

  fn get_issuance_num(&self, code: &AssetTypeCode) -> u64;

  fn get_asset_type(&self, code: &AssetTypeCode) -> AssetType;

  fn get_state_commitment(&self) -> (HashOf<Option<StateCommitmentData>>, u64);

  fn get_kv_entry(&self, addr: Key) -> AuthenticatedKVLookup;

  fn public_key(&self) -> XfrPublicKey;

  fn sign_message<T: Serialize + serde::de::DeserializeOwned>(&self, msg: &T) -> SignatureOf<T>;
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

impl RestfulLedgerUpdate for MockRestClient {
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError> {
    let route = SubmissionRoutes::SubmitTransaction.route();
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_submission_server))
                                   .route(&route,
                                          web::post().to(submit_transaction::<rand_chacha::ChaChaRng,
                                                                            LedgerState>)));
    let req = TestRequest::post().uri(&route).set_json(&txn).to_request();
    let handle: TxnHandle = test::read_response_json(&mut app, req);
    Ok(handle)
  }

  fn force_end_block(&mut self) -> Result<(), PlatformError> {
    let route = SubmissionRoutes::ForceEndBlock.route();
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_submission_server))
                                   .route(&route,
                                          web::post().to(force_end_block::<rand_chacha::ChaChaRng,
                                                                            LedgerState>)));
    let req = TestRequest::post().uri(&route).to_request();
    let submit_resp = test::block_on(app.call(req)).unwrap();
    Ok(())
  }

  fn txn_status(&self, handle: &TxnHandle) -> TxnStatus {
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_submission_server))
                                   .route(&SubmissionRoutes::TxnStatus.with_arg_template("handle"),
                                          web::get().to(txn_status::<rand_chacha::ChaChaRng,
                                                                   LedgerState>)));
    let req = test::TestRequest::get().uri(&SubmissionRoutes::TxnStatus.with_arg(&handle.0))
                                      .to_request();
    test::read_response_json(&mut app, req)
  }
}

impl RestfulLedgerAccess for MockRestClient {
  fn get_utxo(&self, addr: TxoSID) -> Utxo {
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_ledger))
                                   .route(&LedgerAccessRoutes::UtxoSid.with_arg_template("sid"),
                                          web::get().to(query_utxo::<LedgerState>)));
    let req = test::TestRequest::get().uri(&LedgerAccessRoutes::UtxoSid.with_arg(&addr.0))
                                      .to_request();
    test::read_response_json(&mut app, req)
  }

  fn get_issuance_num(&self, code: &AssetTypeCode) -> u64 {
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_ledger))
                                   .route(&LedgerAccessRoutes::AssetIssuanceNum.with_arg_template("code"),
                                          web::get().to(query_asset_issuance_num::<LedgerState>)));
    let req =
      test::TestRequest::get().uri(&LedgerAccessRoutes::AssetIssuanceNum.with_arg(&code.to_base64()))
                              .to_request();
    test::read_response_json(&mut app, req)
  }

  fn get_asset_type(&self, code: &AssetTypeCode) -> AssetType {
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_ledger))
                                   .route(&LedgerAccessRoutes::AssetToken.with_arg_template("code"),
                                          web::get().to(query_asset::<LedgerState>)));
    let req =
      test::TestRequest::get().uri(&LedgerAccessRoutes::AssetToken.with_arg(&code.to_base64()))
                              .to_request();
    test::read_response_json(&mut app, req)
  }

  fn get_state_commitment(&self) -> (HashOf<Option<StateCommitmentData>>, u64) {
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_ledger))
                                   .route(&LedgerAccessRoutes::GlobalState.route(),
                                          web::get().to(query_global_state::<LedgerState>)));
    let req = test::TestRequest::get().uri(&LedgerAccessRoutes::GlobalState.route())
                                      .to_request();
    test::read_response_json(&mut app, req)
  }

  fn get_kv_entry(&self, addr: Key) -> AuthenticatedKVLookup {
    unimplemented!();
  }

  fn public_key(&self) -> XfrPublicKey {
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
