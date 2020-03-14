#![deny(warnings)]
use ledger::store::LedgerState;
use log::{error, info};
use query_api::QueryApi;
use query_server::QueryServer;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use std::thread;
use std::time;

fn main() {
  let running = Arc::new(AtomicBool::new(true));
  let query_server = QueryServer::new(Arc::new(RwLock::new(LedgerState::test_ledger())));
  let host = std::env::var_os("SERVER_HOST").filter(|x| !x.is_empty())
                                            .unwrap_or_else(|| "localhost".into());
  let query_port = std::env::var_os("QUERY_PORT").filter(|x| !x.is_empty())
                                                 .unwrap_or_else(|| "8667".into());

  let wrapped_server = Arc::new(RwLock::new(query_server));
  let query_api = QueryApi::create(wrapped_server.clone(),
                                   host.to_str().unwrap(),
                                   query_port.to_str().unwrap()).unwrap();
  println!("Starting query service");
  while running.load(Ordering::SeqCst) {
    let poll_time = time::Duration::from_millis(1000);
    thread::sleep(poll_time);
    let mut server = wrapped_server.write().unwrap();
    match server.poll_new_blocks() {
      Ok(_) => info!("Block successfuly polled"),
      Err(_) => error!("Error fetching blocks"),
    }
  }
  match query_api.run() {
    Ok(_) => info!("Successfully ran query service"),
    Err(_) => error!("Error running query service"),
  }
}
