#![deny(warnings)]
extern crate abci;
extern crate arrayref;
extern crate ledger;
extern crate ledger_app;
extern crate serde;
extern crate serde_derive;
extern crate serde_json;

use abci::*;
use api_service::RestfulApiService;
use ledger::data_model::errors::PlatformError;
use ledger::store::*;
use ledger_app::{convert_tx, LedgerApp};
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use std::thread;

struct ABCILedgerApp {
  la: LedgerApp<ChaChaRng, LedgerState>,
}

impl ABCILedgerApp {
  fn new() -> Result<ABCILedgerApp, PlatformError> {
    let ledger = LedgerState::test_ledger();
    let prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
    Ok(ABCILedgerApp { la: LedgerApp::new(prng, ledger)? })
  }
}

// TODO: implement abci hooks
impl abci::Application for ABCILedgerApp {
  fn check_tx(&mut self, req: &RequestCheckTx) -> ResponseCheckTx {
    // Get the Tx [u8] and convert to u64
    let mut resp = ResponseCheckTx::new();

    if let Some(tx) = convert_tx(req.get_tx()) {
      if let Ok(mut state) = self.la.get_committed_state().write() {
        if TxnEffect::compute_effect(state.get_prng(), tx).is_err() {
          resp.set_code(1);
          resp.set_log(String::from("Check failed"));
        }
      } else {
        resp.set_code(1);
        resp.set_log(String::from("Could not access ledger"));
      }
    } else {
      resp.set_code(1);
      resp.set_log(String::from("Could not unpack transaction"));
    }

    resp
  }

  fn deliver_tx(&mut self, req: &RequestDeliverTx) -> ResponseDeliverTx {
    // Get the Tx [u8]
    let mut resp = ResponseDeliverTx::new();
    if let Some(tx) = convert_tx(req.get_tx()) {
      if self.la.cache_transaction(tx).is_ok() {
        return resp;
      }
    }
    resp.set_code(1);
    resp
  }

  fn begin_block(&mut self, _req: &RequestBeginBlock) -> ResponseBeginBlock {
    self.la.begin_block();
    ResponseBeginBlock::new()
  }

  fn end_block(&mut self, _req: &RequestEndBlock) -> ResponseEndBlock {
    self.la.end_block();
    ResponseEndBlock::new()
  }

  fn commit(&mut self, _req: &RequestCommit) -> ResponseCommit {
    self.la.begin_commit();
    // TODO: anything not handled by the general LedgerApp (publishing notifications?) should go here.
    self.la.end_commit();
    ResponseCommit::new()
  }

  fn query(&mut self, req: &RequestQuery) -> ResponseQuery {
    println!("{:?}", &req);
    let q = &req.data;
    println!("Path = {}, data = {:?}", &req.path, q);
    ResponseQuery::new()
  }
}

fn main() {
  // Tendermint ABCI port
  // let addr = "127.0.0.1:26658".parse().unwrap();

  // abci::run(addr, ABCILedgerApp::default());
  let app = ABCILedgerApp::new().unwrap();
  let ledger_state = app.la.borrowable_ledger_state();
  let host = std::option_env!("SERVER_HOST").unwrap_or("localhost");
  let port = std::option_env!("SERVER_PORT").unwrap_or("8668");
  let _join = thread::spawn(move || {
    let query_service = RestfulApiService::create(ledger_state, host, port)?;
    query_service.run()
  });

  abci::run_local(app);
}
