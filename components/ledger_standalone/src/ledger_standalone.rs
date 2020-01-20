#![deny(warnings)]
use api_service::RestfulApiService;
use api_service_standalone::RestfulApiServiceStandalone;
use ledger::store::LedgerState;
use ledger_app::LedgerApp;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::path::Path;
use std::sync::{Arc, RwLock};
use std::thread;

fn main() {
  let base_dir = std::env::var_os("LEDGER_DIR").filter(|x| !x.is_empty());
  let base_dir = base_dir.as_ref().map(Path::new);
  env_logger::init();
  dbg!(&base_dir);
  let ledger_state = match base_dir {
    None => LedgerState::test_ledger(),
    Some(base_dir) => {
      dbg!(&base_dir);
      let block_merkle = base_dir.join("block_merkle");
      let block_merkle = block_merkle.to_str().unwrap();
      let txn_merkle = base_dir.join("txn_merkle");
      let txn_merkle = txn_merkle.to_str().unwrap();
      let txn_log = base_dir.join("txn_log");
      let txn_log = txn_log.to_str().unwrap();
      let utxo_map = base_dir.join("utxo_map");
      let utxo_map = utxo_map.to_str().unwrap();

      // TODO(joe): distinguish between the transaction log not existing
      // and it being corrupted
      LedgerState::load_from_log(&block_merkle, &txn_merkle, &txn_log,
                &utxo_map, None)
              .or_else(|_| LedgerState::new(&block_merkle, &txn_merkle, &txn_log,
                &utxo_map, None)).unwrap()
    }
  };
  let prng = ChaChaRng::from_seed([0u8; 32]);
  let state_lock = Arc::new(RwLock::new(ledger_state));
  let cloned_lock = Arc::clone(&state_lock);
  let ledger_app = LedgerApp::new(prng, state_lock, 1).unwrap();

  let host = std::env::var_os("SERVER_HOST").filter(|x| !x.is_empty())
                                            .unwrap_or_else(|| "localhost".into());
  let host2 = host.clone();
  let submission_port = std::env::var_os("SUBMISSION_PORT").filter(|x| !x.is_empty())
                                                           .unwrap_or_else(|| "8669".into());
  let query_port = std::env::var_os("QUERY_PORT").filter(|x| !x.is_empty())
                                                 .unwrap_or_else(|| "8668".into());

  thread::spawn(move || {
    let standalone_service =
      RestfulApiServiceStandalone::create_standalone(Arc::new(RwLock::new(ledger_app)),
                                                     host.to_str().unwrap(),
                                                     submission_port.to_str().unwrap()).unwrap();
    println!("Starting submission service");
    match standalone_service.run() {
      Ok(_) => println!("Successfully ran standalone"),
      Err(_) => println!("Error running standalone"),
    }
  });

  let query_service = RestfulApiService::create(cloned_lock,
                                                host2.to_str().unwrap(),
                                                query_port.to_str().unwrap()).unwrap();
  println!("Starting query service");
  match query_service.run() {
    Ok(_) => println!("Successfully ran standalone"),
    Err(_) => println!("Error running standalone"),
  }
}
