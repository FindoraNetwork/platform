#![deny(warnings)]
use abci::*;
use cryptohash::sha256::Digest as BitDigest;
use cryptohash::sha256::DIGESTBYTES;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::Transaction;
use ledger::store::*;
use ledger_api_service::RestfulApiService;
use log::info;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::sync::{Arc, RwLock};
use std::thread;
use submission_server::{convert_tx, NoTF, SubmissionServer, TxnForward};

pub struct TendermintForward;

impl TxnForward for TendermintForward {
  fn forward_txn(&self, txn: Transaction) -> Result<(), PlatformError> {
    // TODO (Nathan/John): Something useful.
    info!("forward_txn: {}", serde_json::to_string(&txn)?);
    Ok(())
  }
}

struct ABCISubmissionServer {
  la: SubmissionServer<ChaChaRng, LedgerState, NoTF>,
}

impl ABCISubmissionServer {
  fn new() -> Result<ABCISubmissionServer, PlatformError> {
    let ledger = LedgerState::test_ledger();
    let prng = rand_chacha::ChaChaRng::from_entropy();
    Ok(ABCISubmissionServer { la:
                                SubmissionServer::new_no_auto_commit(prng,
                                                                     Arc::new(RwLock::new(ledger)),
                                                                     None)? })
  }
}

// TODO: implement abci hooks
impl abci::Application for ABCISubmissionServer {
  fn check_tx(&mut self, req: &RequestCheckTx) -> ResponseCheckTx {
    // Get the Tx [u8] and convert to u64
    let mut resp = ResponseCheckTx::new();

    if let Some(tx) = convert_tx(req.get_tx()) {
      if self.la.get_committed_state().write().is_ok() {
        if TxnEffect::compute_effect(tx).is_err() {
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
    // TODO: this should propagate errors instead of panicking
    self.la.end_block().unwrap();
    ResponseEndBlock::new()
  }

  fn commit(&mut self, _req: &RequestCommit) -> ResponseCommit {
    // Tendermint does not accept an error return type here.
    let error_commitment = (BitDigest { 0: [0_u8; DIGESTBYTES] }, 0);
    self.la.begin_commit();
    let commitment = if let Ok(state) = self.la.get_committed_state().read() {
      state.get_state_commitment()
    } else {
      error_commitment
    };
    self.la.end_commit();
    let mut r = ResponseCommit::new();
    r.data = commitment.0.as_ref().to_vec();
    r
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

  // abci::run(addr, ABCISubmissionServer::default());
  let app = ABCISubmissionServer::new().unwrap();
  let ledger_state = app.la.borrowable_ledger_state();
  let host = std::option_env!("SERVER_HOST").unwrap_or("localhost");
  let port = std::option_env!("SERVER_PORT").unwrap_or("8668");
  let _join = thread::spawn(move || {
    let query_service = RestfulApiService::create(ledger_state, host, port)?;
    query_service.run()
  });

  abci::run_local(app);
}
