use api_service::RestfulApiService;
use api_service_standalone::RestfulApiServiceStandalone;
use ledger::store::LedgerState;
use ledger_app::LedgerApp;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::sync::{Arc, RwLock};
use std::thread;

fn main() {
  let ledger_state = LedgerState::test_ledger();
  let prng = ChaChaRng::from_seed([0u8; 32]);
  let state_lock = Arc::new(RwLock::new(ledger_state));
  let cloned_lock = Arc::clone(&state_lock);
  let ledger_app = LedgerApp::new(prng.clone(), state_lock).unwrap();

  let host = std::option_env!("SERVER_HOST").unwrap_or("localhost");
  let submission_port = std::option_env!("SUBMISSION_PORT").unwrap_or("8669");
  let query_port = std::option_env!("QUERY_PORT").unwrap_or("8668");

  thread::spawn(move || {
    let standalone_service =
      RestfulApiServiceStandalone::create_standalone(Arc::new(RwLock::new(ledger_app)),
                                                     host,
                                                     submission_port).unwrap();
    match standalone_service.run() {
      Ok(_) => println!("Successfully ran standalone"),
      Err(_) => println!("Error running standalone"),
    }
  });
  let query_service = RestfulApiService::create(cloned_lock, host, query_port).unwrap();
  match query_service.run() {
    Ok(_) => println!("Successfully ran standalone"),
    Err(_) => println!("Error running standalone"),
  }
}
