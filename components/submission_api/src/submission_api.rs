#![deny(warnings)]
use actix_cors::Cors;
use actix_service::Service;
use actix_web::test::TestRequest;
use actix_web::{error, middleware, test, web, App, HttpServer};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::Transaction;
use ledger::{error_location, inp_fail, ser_fail};

use ledger::store::{LedgerState, LedgerUpdate};
use log::{error, info};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use rand_core::{CryptoRng, RngCore};
use std::io;
use std::marker::{Send, Sync};
use std::sync::{Arc, RwLock};
use submission_server::{NoTF, SubmissionServer, TxnForward, TxnHandle, TxnStatus};
use utils::{actix_get_request, actix_post_request, NetworkRoute};

// Ping route to check for liveness of API
fn ping() -> actix_web::Result<String> {
  Ok("success".into())
}

pub fn submit_transaction<RNG, LU, TF>(data: web::Data<Arc<RwLock<SubmissionServer<RNG, LU, TF>>>>,
                                       body: web::Json<Transaction>)
                                       -> Result<web::Json<TxnHandle>, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + Sync + Send,
        TF: TxnForward + Sync + Send
{
  let mut submission_server = data.write().unwrap();
  let tx = body.into_inner();

  let handle_res = submission_server.handle_transaction(tx);

  match handle_res {
    Ok(handle) => Ok(web::Json(handle)),
    Err(e) => {
      error!("Transaction invalid");
      Err(error::ErrorBadRequest(format!("{}", e)))
    }
  }
}

// Force the validator node to end the block. Useful for testing when it is desirable to commmit
// txns to the ledger as soon as possible.
//
// When a block is successfully finalized, returns HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>
pub fn force_end_block<RNG, LU, TF>(data: web::Data<Arc<RwLock<SubmissionServer<RNG, LU, TF>>>>)
                                    -> Result<String, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + Sync + Send,
        TF: TxnForward + Sync + Send
{
  let mut submission_server = data.write().unwrap();
  if submission_server.end_block().is_ok() {
    Ok("Block successfully ended. All previously valid pending transactions are now committed".to_string())
  } else {
    Ok("No pending transactions to commit".to_string())
  }
}

// Queries the status of a transaction by its handle. Returns either a not committed message or a
// serialized TxnStatus.
pub fn txn_status<RNG, LU, TF>(data: web::Data<Arc<RwLock<SubmissionServer<RNG, LU, TF>>>>,
                               info: web::Path<String>)
                               -> Result<String, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + Sync + Send,
        TF: TxnForward + Sync + Send
{
  let submission_server = data.write().unwrap();
  let txn_status = submission_server.get_txn_status(&TxnHandle(info.clone()));
  let res;
  if let Some(status) = txn_status {
    res = serde_json::to_string(&status)?;
  } else {
    res = format!("No transaction with handle {} found. Please retry with a new handle.",
                  &info);
  }

  Ok(res)
}

pub struct SubmissionApi {
  web_runtime: actix_rt::SystemRunner,
}

pub enum SubmissionRoutes {
  SubmitTransaction,
  TxnStatus,
  Ping,
  ForceEndBlock,
}

impl NetworkRoute for SubmissionRoutes {
  fn route(&self) -> String {
    let endpoint = match *self {
      SubmissionRoutes::SubmitTransaction => "submit_transaction",
      SubmissionRoutes::TxnStatus => "txn_status",
      SubmissionRoutes::Ping => "ping",
      SubmissionRoutes::ForceEndBlock => "force_end_block",
    };
    "/".to_owned() + endpoint
  }
}

impl SubmissionApi {
  pub fn create<RNG: 'static + RngCore + CryptoRng + Sync + Send,
                  LU: 'static + LedgerUpdate<RNG> + Sync + Send,
                  TF: 'static + TxnForward + Sync + Send>(
    submission_server: Arc<RwLock<SubmissionServer<RNG, LU, TF>>>,
    host: &str,
    port: &str)
    -> io::Result<SubmissionApi> {
    let web_runtime = actix_rt::System::new("findora API");

    HttpServer::new(move || {
      App::new().wrap(middleware::Logger::default())
                .wrap(Cors::new().supports_credentials())
                .data(submission_server.clone())
                .route(&SubmissionRoutes::SubmitTransaction.route(),
                       web::post().to(submit_transaction::<RNG, LU, TF>))
                .route(&SubmissionRoutes::Ping.route(), web::get().to(ping))
                .route(&SubmissionRoutes::TxnStatus.with_arg_template("handle"),
                       web::get().to(txn_status::<RNG, LU, TF>))
                .route(&SubmissionRoutes::ForceEndBlock.route(),
                       web::post().to(force_end_block::<RNG, LU, TF>))
    }).bind(&format!("{}:{}", host, port))?
      .start();

    info!("Submission server started");

    Ok(SubmissionApi { web_runtime })
  }

  // call from a thread; this will block.
  pub fn run(self) -> io::Result<()> {
    self.web_runtime.run()
  }
}

// Trait for rest clients that can submit transactions
pub trait RestfulLedgerUpdate {
  // Forward transaction to a transaction submission server, returning a handle to the transaction
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError>;
  // Forces the the validator to end the block. Useful for testing when it is desirable to commit
  // txns to the ledger as soon as possible
  fn force_end_block(&mut self) -> Result<(), PlatformError>;
  // Get txn status from a handle
  fn txn_status(&self, handle: &TxnHandle) -> Result<TxnStatus, PlatformError>;
}

pub struct MockLUClient {
  mock_submission_server: Arc<RwLock<SubmissionServer<ChaChaRng, LedgerState, NoTF>>>,
}

impl MockLUClient {
  pub fn new(state: &Arc<RwLock<LedgerState>>, block_capacity: usize) -> Self {
    let prng = ChaChaRng::from_entropy();
    let state_lock = Arc::clone(state);
    let mock_submission_server =
      Arc::new(RwLock::new(SubmissionServer::new(prng, state_lock, block_capacity).unwrap()));
    MockLUClient { mock_submission_server }
  }
}

impl RestfulLedgerUpdate for MockLUClient {
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError> {
    let route = SubmissionRoutes::SubmitTransaction.route();
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_submission_server))
                                   .route(&route,
                                          web::post().to(submit_transaction::<rand_chacha::ChaChaRng,
                                                                            LedgerState, NoTF>)));
    let req = TestRequest::post().uri(&route).set_json(&txn).to_request();
    let handle: TxnHandle = test::read_response_json(&mut app, req);
    Ok(handle)
  }

  fn force_end_block(&mut self) -> Result<(), PlatformError> {
    let route = SubmissionRoutes::ForceEndBlock.route();
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_submission_server))
                                   .route(&route,
                                          web::post().to(force_end_block::<rand_chacha::ChaChaRng,
                                                                            LedgerState, NoTF>)));
    let req = TestRequest::post().uri(&route).to_request();
    test::block_on(app.call(req)).unwrap();
    Ok(())
  }

  fn txn_status(&self, handle: &TxnHandle) -> Result<TxnStatus, PlatformError> {
    let mut app =
      test::init_service(App::new().data(Arc::clone(&self.mock_submission_server))
                                   .route(&SubmissionRoutes::TxnStatus.with_arg_template("handle"),
                                          web::get().to(txn_status::<rand_chacha::ChaChaRng,
                                                                   LedgerState, NoTF>)));
    let req = test::TestRequest::get().uri(&SubmissionRoutes::TxnStatus.with_arg(&handle.0))
                                      .to_request();
    Ok(test::read_response_json(&mut app, req))
  }
}

pub struct ActixLUClient {
  port: usize,
  host: String,
  protocol: String,
  client: reqwest::Client,
}

impl ActixLUClient {
  pub fn new(port: usize, host: &str, protocol: &str) -> Self {
    ActixLUClient { port,
                    host: String::from(host),
                    protocol: String::from(protocol),
                    client: reqwest::Client::new() }
  }
}

impl RestfulLedgerUpdate for ActixLUClient {
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError> {
    let query = format!("{}://{}:{}{}",
                        self.protocol,
                        self.host,
                        self.port,
                        SubmissionRoutes::SubmitTransaction.route());
    let text = actix_post_request(&self.client, &query, Some(&txn)).map_err(|_| inp_fail!())?;
    let handle = serde_json::from_str::<TxnHandle>(&text).map_err(|_| ser_fail!())?;
    info!("Transaction submitted successfully");
    Ok(handle)
  }

  fn force_end_block(&mut self) -> Result<(), PlatformError> {
    let query = format!("{}://{}:{}{}",
                        self.protocol,
                        self.host,
                        self.port,
                        SubmissionRoutes::ForceEndBlock.route());
    let opt: Option<u64> = None;
    actix_post_request(&self.client, &query, opt).map_err(|_| inp_fail!())?;
    Ok(())
  }

  fn txn_status(&self, handle: &TxnHandle) -> Result<TxnStatus, PlatformError> {
    let query = format!("{}://{}:{}{}",
                        self.protocol,
                        self.host,
                        self.port,
                        SubmissionRoutes::TxnStatus.with_arg(&handle.0));
    let text = actix_get_request(&self.client, &query).map_err(|_| inp_fail!())?;
    Ok(serde_json::from_str::<TxnStatus>(&text).map_err(|_| ser_fail!())?)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use actix_web::dev::Service;
  use actix_web::{test, web, App};
  use ledger::data_model::{AssetRules, AssetTypeCode, Operation, Transaction};
  use ledger::store::helpers::*;
  use ledger::store::{LedgerAccess, LedgerState};
  use rand_core::SeedableRng;
  use submission_server::NoTF;

  #[test]
  fn test_submit_transaction_standalone() {
    let mut prng = rand_chacha::ChaChaRng::from_entropy();
    let ledger_state = LedgerState::test_ledger();
    let block_commit_count = ledger_state.get_block_commit_count();
    let submission_server =
      Arc::new(RwLock::new(SubmissionServer::<_, _, NoTF>::new(prng.clone(),
                                                 Arc::new(RwLock::new(ledger_state)),
                                                 8).unwrap()));
    let app_copy = Arc::clone(&submission_server);
    let mut tx = Transaction::from_seq_id(block_commit_count);

    let token_code1 = AssetTypeCode { val: [1; 16] };
    let keypair = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1,
                                         keypair.get_pk_ref(),
                                         AssetRules::default(),
                                         None,
                                         None);
    let asset_create = asset_creation_operation(&asset_body, &keypair);
    tx.body
      .operations
      .push(Operation::DefineAsset(asset_create));

    let mut app =
      test::init_service(App::new().data(submission_server)
                                   .route("/submit_transaction",
                                          web::post().to(submit_transaction::<rand_chacha::ChaChaRng,
                                                                            LedgerState, NoTF>))
                                  .route("/force_end_block",
                                          web::post().to(force_end_block::<rand_chacha::ChaChaRng,
                                                                            LedgerState, NoTF>)));

    let req = test::TestRequest::post().uri("/submit_transaction")
                                       .set_json(&tx)
                                       .to_request();

    let submit_resp = test::block_on(app.call(req)).unwrap();

    assert!(submit_resp.status().is_success());
    let req = test::TestRequest::post().uri("/force_end_block")
                                       .to_request();
    let submit_resp = test::block_on(app.call(req)).unwrap();
    assert!(submit_resp.status().is_success());
    assert!(app_copy.read()
                    .unwrap()
                    .borrowable_ledger_state()
                    .read()
                    .unwrap()
                    .get_asset_type(&token_code1)
                    .is_some());
  }
}
