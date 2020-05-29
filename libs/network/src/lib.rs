use actix_http::Request;
use actix_web::dev::ServiceResponse;
use actix_web::test;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::Transaction;
use ledger::store::LedgerState;
use std::sync::{Arc, RwLock};
use submission_api::SubmissionApi;
use submission_server::{TxnHandle, TxnStatus};
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

/// Mock rest client that uses test servers in the backgruond
pub struct MockRestClient {
  //mock_ledger_server: usize, //ADD real types, allow mock server returned from client
  mock_submission_server: dyn actix_service::Service<Request = Request,
                                                     Response = ServiceResponse,
                                                     Error = actix_web::error::Error>,
}

impl MockRestClient {
  pub fn new(block_capacity: usize) -> Self {
    // Mock server uses a fresh ledger state in the background
    let ledger_state = LedgerState::test_ledger();
    let state_lock = Arc::new(RwLock::new(ledger_state));
    let cloned_lock = Arc::clone(&state_lock);

    let mock_submission_server = SubmissionApi::create_mock(state_lock, block_capacity);
    //let mock_ledger_server = create_mock_ledger_server(cloned_lock);

    MockRestClient { mock_submission_server }
  }
}

impl SubmitsTransactions for MockRestClient {
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError> {
    let req = test::TestRequest::post().uri("/submit_transaction")
                                       .set_json(&txn)
                                       .to_request();
    //let submit_resp = test::block_on(self.mock_submission_server.call(req)).unwrap();
    let handle: TxnHandle = test::read_response_json(&mut self.mock_submission_server, req);
    Ok(handle)
  }
  fn force_end_block(&mut self) -> Result<(), PlatformError> {
    unimplemented!();
  }
  fn txn_status(&self, handle: &TxnHandle) -> TxnStatus {
    unimplemented!();
  }
}
//impl LedgerAccess for MockRestClient {}
//impl ArchiveAccess for MockRestClient {}
