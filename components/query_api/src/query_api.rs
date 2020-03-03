//#![deny(warnings)]
use actix_cors::Cors;
use actix_web::{error, middleware, web, App, HttpServer};
use ledger::data_model::TxoSID;
use ledger::store::{ArchiveAccess, LedgerAccess, LedgerUpdate};
use log::{error, info};
use query_server::QueryServer;
use rand_core::{CryptoRng, RngCore};
use std::io;
use std::marker::{Send, Sync};
use std::sync::{Arc, RwLock};

// Queries the status of a transaction by its handle. Returns either a not committed message or a
// serialized TxnStatus.
fn get_address<RNG, LU>(data: web::Data<Arc<RwLock<QueryServer<RNG, LU>>>>,
                        info: web::Path<u64>)
                        -> Result<String, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send
{
  let query_server = data.read().unwrap();
  let address_res = query_server.get_address_of_sid(TxoSID(*info));
  let res;
  if let Some(address) = address_res {
    res = serde_json::to_string(&address)?;
  } else {
    res = format!("No utxo {} found. Please retry with a new utxo.", &info);
  }
  Ok(res)
}

pub struct QueryApi {
  web_runtime: actix_rt::SystemRunner,
}

impl QueryApi {
  pub fn create<RNG: 'static + RngCore + CryptoRng + Sync + Send,
                  LU: 'static + LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send>(
    query_server: Arc<RwLock<QueryServer<RNG, LU>>>,
    host: &str,
    port: &str)
    -> io::Result<QueryApi> {
    let web_runtime = actix_rt::System::new("findora API");

    HttpServer::new(move || {
      App::new().wrap(middleware::Logger::default())
                .wrap(Cors::new().supports_credentials())
                .data(query_server.clone())
                .route("/get_address/{txo_sid}",
                       web::get().to(get_address::<RNG, LU>))
    }).bind(&format!("{}:{}", host, port))?
      .start();

    info!("Query server started");

    Ok(QueryApi { web_runtime })
  }

  // call from a thread; this will block.
  pub fn run(self) -> io::Result<()> {
    self.web_runtime.run()
  }
}

//#[cfg(test)]
//mod tests {
//  use super::*;
//  use actix_web::dev::Service;
//  use actix_web::{test, web, App};
//  use ledger::data_model::AssetTypeCode;
//  use ledger::data_model::{Operation, Transaction};
//  use ledger::store::helpers::*;
//  use ledger::store::{LedgerAccess, LedgerState};
//  use rand_core::SeedableRng;
//
//  #[test]
//  fn test_submit_transaction_standalone() {
//    let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
//    let ledger_state = LedgerState::test_ledger();
//    let submission_server =
//      Arc::new(RwLock::new(SubmissionServer::new(prng.clone(),
//                                                 Arc::new(RwLock::new(ledger_state)),
//                                                 8).unwrap()));
//    let app_copy = Arc::clone(&submission_server);
//    let mut tx = Transaction::default();
//
//    let token_code1 = AssetTypeCode { val: [1; 16] };
//    let (public_key, secret_key) = build_keys(&mut prng);
//
//    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
//    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
//    tx.operations.push(Operation::DefineAsset(asset_create));
//
//    let mut app =
//      test::init_service(App::new().data(submission_server)
//                                   .route("/submit_transaction",
//                                          web::post().to(submit_transaction::<rand_chacha::ChaChaRng,
//                                                                            LedgerState>))
//                                  .route("/force_end_block",
//                                          web::post().to(force_end_block::<rand_chacha::ChaChaRng,
//                                                                            LedgerState>)));
//
//    let req = test::TestRequest::post().uri("/submit_transaction")
//                                       .set_json(&tx)
//                                       .to_request();
//
//    let submit_resp = test::block_on(app.call(req)).unwrap();
//
//    assert!(submit_resp.status().is_success());
//    let req = test::TestRequest::post().uri("/force_end_block")
//                                       .to_request();
//    let submit_resp = test::block_on(app.call(req)).unwrap();
//    assert!(submit_resp.status().is_success());
//    assert!(app_copy.read()
//                    .unwrap()
//                    .borrowable_ledger_state()
//                    .read()
//                    .unwrap()
//                    .get_asset_type(&token_code1)
//                    .is_some());
//  }
//}
