extern crate abci;
extern crate arrayref;
extern crate core;
extern crate ledger_app;
extern crate serde;
extern crate serde_derive;
extern crate serde_json;
extern crate actix_web;

use abci::*;
use core::data_model::errors::PlatformError;
use core::store::*;
use ledger_app::{convert_tx, LedgerApp};

struct ABCILedgerApp {
  la: LedgerApp,
}

impl ABCILedgerApp {
  fn new() -> Result<ABCILedgerApp, PlatformError> {
    Ok(ABCILedgerApp { la: LedgerApp::new(LedgerState::default())? })
  }
}

// TODO: implement abci hooks
impl abci::Application for ABCILedgerApp {
  fn check_tx(&mut self, req: &RequestCheckTx) -> ResponseCheckTx {
    // Get the Tx [u8] and convert to u64
    let mut resp = ResponseCheckTx::new();

    if let Some(tx) = convert_tx(req.get_tx()) {
      if let Ok(state) = self.la.get_committed_state().read() {
        if !state.check_txn_structure(&tx) {
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
      if let Some(tracker) = self.la.get_mut_pending_state() {
        if let Ok(mut context) = TxnContext::new(tracker) {
          let result = context.validate_transaction(&tx);
          if result {
            self.la.cache_transaction(tx);
            return resp;
          }
        }
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

  abci::run_local(ABCILedgerApp::new().unwrap());
}
