#![deny(warnings)]
use ledger::store::LedgerState;
use ledger_api_service::RestfulApiService;
use query_api::QueryApi;
use query_server::QueryServer;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use std::thread;
use std::time;
use submission_api::SubmissionApi;
use submission_server::SubmissionServer;

fn main() {
  let running = Arc::new(AtomicBool::new(true));
  let base_dir = std::env::var_os("LEDGER_DIR").filter(|x| !x.is_empty());
  let base_dir = base_dir.as_ref().map(Path::new);
  env_logger::init();
  dbg!(&base_dir);
  let ledger_state = match base_dir {
    None => LedgerState::test_ledger(),
    Some(base_dir) => LedgerState::load_or_init(base_dir).unwrap(),
  };
  let prng = ChaChaRng::from_seed([0u8; 32]);
  let prng_2 = ChaChaRng::from_seed([0u8; 32]);
  let state_lock = Arc::new(RwLock::new(ledger_state));
  let cloned_lock = Arc::clone(&state_lock);
  let submission_server = SubmissionServer::new(prng, state_lock, 1).unwrap();
  let query_server =
    QueryServer::new(prng_2, Arc::new(RwLock::new(LedgerState::test_ledger()))).unwrap();

  let host = std::env::var_os("SERVER_HOST").filter(|x| !x.is_empty())
                                            .unwrap_or_else(|| "localhost".into());
  let host2 = host.clone();
  let host3 = host.clone();
  let submission_port = std::env::var_os("SUBMISSION_PORT").filter(|x| !x.is_empty())
                                                           .unwrap_or_else(|| "8669".into());
  let ledger_port = std::env::var_os("LEDGER_PORT").filter(|x| !x.is_empty())
                                                   .unwrap_or_else(|| "8668".into());
  let query_port = std::env::var_os("QUERY_PORT").filter(|x| !x.is_empty())
                                                 .unwrap_or_else(|| "8667".into());

  thread::spawn(move || {
    let submission_api = SubmissionApi::create(Arc::new(RwLock::new(submission_server)),
                                               host.to_str().unwrap(),
                                               submission_port.to_str().unwrap()).unwrap();
    println!("Starting submission service");
    match submission_api.run() {
      Ok(_) => println!("Successfully ran submission service"),
      Err(_) => println!("Error running submission service"),
    }
  });

  thread::spawn(move || {
    let wrapped_server = Arc::new(RwLock::new(query_server));
    let query_api = QueryApi::create(wrapped_server.clone(),
                                     host3.to_str().unwrap(),
                                     query_port.to_str().unwrap()).unwrap();
    println!("Starting query service");
    while running.load(Ordering::SeqCst) {
      let poll_time = time::Duration::from_millis(1000);
      thread::sleep(poll_time);
      let mut server = wrapped_server.write().unwrap();
      server.poll_new_blocks().unwrap();
    }
    match query_api.run() {
      Ok(_) => println!("Successfully ran query service"),
      Err(_) => println!("Error running query service"),
    }
  });

  let query_service = RestfulApiService::create(cloned_lock,
                                                host2.to_str().unwrap(),
                                                ledger_port.to_str().unwrap()).unwrap();
  println!("Starting query service");
  match query_service.run() {
    Ok(_) => println!("Successfully ran standalone"),
    Err(_) => println!("Error running standalone"),
  }
}
