use api_service::RestfulApiService;
use ledger::store::LedgerState;
use std::sync::{Arc, RwLock};

fn main() {
  let ledger = LedgerState::test_ledger();
  let query_service = RestfulApiService::create(Arc::new(RwLock::new(ledger))).unwrap();
  match query_service.run() {
    Ok(_) => {}
    Err(_) => println!("Error running query service"),
  }
}
