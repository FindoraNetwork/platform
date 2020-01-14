use api_service::RestfulApiService;
use ledger::store::LedgerState;
use std::sync::{Arc, RwLock};
use std::path::Path;

fn main() {
  let base_dir = std::option_env!("LEDGER_DIR").map(Path::new);
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

          LedgerState::load_from_log(&block_merkle, &txn_merkle, &txn_log,
                &utxo_map, None)
              .or_else(|_| LedgerState::new(&block_merkle, &txn_merkle, &txn_log,
                &utxo_map, None)).unwrap()
      },
  };

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
