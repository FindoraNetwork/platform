use api_service::RestfulApiService;
use ledger::store::LedgerState;
use rand::SeedableRng;
use std::sync::{Arc, RwLock};
use std::thread;

fn main() {
  let ledger_state = LedgerState::test_ledger();
  let prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

  let host = std::option_env!("SERVER_HOST").unwrap_or("localhost");
  let port = std::option_env!("SERVER_PORT").unwrap_or("8668");

  //TODO (keyao/noah) Figure out how to get the query service and standalone running together
  let query_service =
    RestfulApiService::create(Arc::new(RwLock::new(ledger_state)), host, port).unwrap();
  match query_service.run() {
    Ok(_) => println!("Successfully ran standalone"),
    Err(_) => println!("Error running standalone"),
  }
}
