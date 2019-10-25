use api_service_standalone::RestfulApiServiceStandalone;
use ledger::store::LedgerState;
use ledger_app::LedgerApp;
use rand::SeedableRng;
use std::sync::{Arc, RwLock};

fn main() {
  let ledger_state = LedgerState::test_ledger();
  let prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
  let ledger_app = LedgerApp::new(prng, ledger_state).unwrap();

  let host = std::option_env!("SERVER_HOST").unwrap_or("localhost");
  let port = std::option_env!("SERVER_PORT").unwrap_or("8668");

  let query_service = RestfulApiServiceStandalone::create_standalone(Arc::new(RwLock::new(ledger_app)), host, port).unwrap();

  match query_service.run() {
    Ok(_) => println!("Successfully ran query service"),
    Err(_) => println!("Error running query service"),
  }
}
