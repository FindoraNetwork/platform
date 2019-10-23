#![deny(warnings)]
use api_service::RestfulApiService;
use ledger::store::LedgerState;
use std::sync::{Arc, RwLock};

fn main() {
  let ledger = LedgerState::test_ledger();
  let host = std::option_env!("SERVER_HOST").unwrap_or("localhost");
  let port = std::option_env!("SERVER_PORT").unwrap_or("8668");
  let query_service = RestfulApiService::create(Arc::new(RwLock::new(ledger)), host, port).unwrap();
  match query_service.run() {
    Ok(_) => println!("Successfully ran query service"),
    Err(_) => println!("Error running query service"),
  }
}
