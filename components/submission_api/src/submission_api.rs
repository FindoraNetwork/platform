#![deny(warnings)]
use actix_cors::Cors;
use actix_web::{error, middleware, web, App, HttpServer};
use ledger::data_model::Transaction;
use ledger::store::{ArchiveAccess, LedgerAccess, LedgerUpdate};
use log::{error, info};
use percent_encoding::percent_decode_str;
use rand_core::{CryptoRng, RngCore};
use std::io;
use std::marker::{Send, Sync};
use std::sync::{Arc, RwLock};
use submission_server::{SubmissionServer, TxnHandle};

fn submit_transaction<RNG, LU>(data: web::Data<Arc<RwLock<SubmissionServer<RNG, LU>>>>,
                               body: web::Json<Transaction>)
                               -> Result<web::Json<TxnHandle>, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send
{
  let mut submission_server = data.write().unwrap();
  let tx = body.into_inner();

  let handle_res = submission_server.handle_transaction(tx);

  if let Ok(handle) = handle_res {
    Ok(web::Json(handle))
  } else {
    error!("Transaction invalid");
    Err(error::ErrorBadRequest("Transaction Invalid"))
  }
}

fn submit_transaction_wasm<RNG, LU>(data: web::Data<Arc<RwLock<SubmissionServer<RNG, LU>>>>,
                                    info: web::Path<String>)
                                    -> Result<web::Json<TxnHandle>, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send
{
  let mut submission_server = data.write().unwrap();
  let uri_string = percent_decode_str(&*info).decode_utf8().unwrap();
  let tx = serde_json::from_str(&uri_string).unwrap();
  let handle = submission_server.handle_transaction(tx)
                                .map_err(error::ErrorBadRequest)?;
  Ok(web::Json(handle))
}

// Force the validator node to end the block. Useful for testing when it is desirable to commmit
// txns to the ledger as soon as possible.
//
// When a block is successfully finalized, returns HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>
fn force_end_block<RNG, LU>(data: web::Data<Arc<RwLock<SubmissionServer<RNG, LU>>>>)
                            -> Result<String, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send
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
fn txn_status<RNG, LU>(data: web::Data<Arc<RwLock<SubmissionServer<RNG, LU>>>>,
                       info: web::Path<String>)
                       -> Result<String, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send
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

impl SubmissionApi {
  pub fn create<RNG: 'static + RngCore + CryptoRng + Sync + Send,
                  LU: 'static + LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send>(
    submission_server: Arc<RwLock<SubmissionServer<RNG, LU>>>,
    host: &str,
    port: &str)
    -> io::Result<SubmissionApi> {
    let web_runtime = actix_rt::System::new("findora API");

    HttpServer::new(move || {
      App::new().wrap(middleware::Logger::default())
                .wrap(Cors::new().supports_credentials())
                .data(submission_server.clone())
                .route("/submit_transaction",
                       web::post().to(submit_transaction::<RNG, LU>))
                .route("/submit_transaction_wasm/{tx}",
                       web::post().to(submit_transaction_wasm::<RNG, LU>))
                .route("/txn_status/{handle}", web::get().to(txn_status::<RNG, LU>))
                .route("/force_end_block",
                       web::post().to(force_end_block::<RNG, LU>))
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

#[cfg(test)]
mod tests {
  use super::*;
  use actix_web::dev::Service;
  use actix_web::{test, web, App};
  use ledger::data_model::AssetTypeCode;
  use ledger::data_model::{Operation, Transaction};
  use ledger::store::helpers::*;
  use ledger::store::{LedgerAccess, LedgerState};
  use rand_core::SeedableRng;

  #[test]
  fn test_submit_transaction_standalone() {
    let mut prng = rand_chacha::ChaChaRng::from_entropy();
    let ledger_state = LedgerState::test_ledger();
    let submission_server =
      Arc::new(RwLock::new(SubmissionServer::new(prng.clone(),
                                                 Arc::new(RwLock::new(ledger_state)),
                                                 8).unwrap()));
    let app_copy = Arc::clone(&submission_server);
    let mut tx = Transaction::default();

    let token_code1 = AssetTypeCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::DefineAsset(asset_create));

    let mut app =
      test::init_service(App::new().data(submission_server)
                                   .route("/submit_transaction",
                                          web::post().to(submit_transaction::<rand_chacha::ChaChaRng,
                                                                            LedgerState>))
                                  .route("/force_end_block",
                                          web::post().to(force_end_block::<rand_chacha::ChaChaRng,
                                                                            LedgerState>)));

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
