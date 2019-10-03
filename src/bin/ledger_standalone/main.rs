use api_service::RestfulApiService;
use ledger::store::LedgerState;
use std::sync::{Arc, RwLock};
use std::thread;

fn main() {
  let mut ledger = LedgerState::test_ledger();
  let query_service = RestfulApiService::create(Arc::new(RwLock::new(ledger))).unwrap();
  query_service.run();
}
