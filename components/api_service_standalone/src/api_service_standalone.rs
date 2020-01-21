#![deny(warnings)]
extern crate actix_rt;
extern crate actix_web;
extern crate ledger;
extern crate ledger_app;
extern crate serde_json;

use actix_web::{dev, error, web, App, HttpServer};
use ledger::data_model::Transaction;
use ledger::store::{LedgerAccess, LedgerUpdate};
use ledger_app::{LedgerApp, TxnHandle};
use rand_core::{CryptoRng, RngCore};
use std::io;
use std::marker::{Send, Sized, Sync};
use std::sync::{Arc, RwLock};

fn submit_transaction<RNG, LU>(data: web::Data<Arc<RwLock<LedgerApp<RNG, LU>>>>,
                               body: web::Json<Transaction>)
                               -> Result<web::Json<TxnHandle>, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + Sync + Send
{
  let mut ledger_app = data.write().unwrap();
  let tx = body.into_inner();

  let handle = ledger_app.handle_transaction(tx)
                         .map_err(error::ErrorBadRequest)?;
  Ok(web::Json(handle))
}

// Force the validator node to end the block. Useful for testing when it is desirable to commmit
// txns to the ledger as soon as possible.
//
// When a block is successfully finalized, returns HashMap<TxnTempSID, (TxnSID, Vec<TxoSID>)>
fn force_end_block<RNG, LU>(data: web::Data<Arc<RwLock<LedgerApp<RNG, LU>>>>)
                            -> Result<String, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + Sync + Send
{
  let mut ledger_app = data.write().unwrap();
  if ledger_app.end_block().is_ok() {
    Ok("Block successfully ended. All previously valid pending transactions are now committed".to_string())
  } else {
    Ok("No pending transactions to commit".to_string())
  }
}

fn txn_status<RNG, LU>(data: web::Data<Arc<RwLock<LedgerApp<RNG, LU>>>>,
                       info: web::Path<String>)
                       -> Result<String, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + Sync + Send
{
  let ledger_app = data.write().unwrap();
  let txn_status = ledger_app.get_txn_status(&TxnHandle(info.clone()));
  let res;
  if let Some(status) = txn_status {
    res = serde_json::to_string(&status)?;
  } else {
    res = format!("No transaction with handle {} found. Please retry with a new handle.",
                  &info);
  }
  Ok(res)
}

pub enum ServiceInterfaceStandalone {
  LedgerUpdate,
}

pub trait RouteStandalone {
  fn set_route<RNG: 'static + RngCore + CryptoRng,
                 LU: 'static + LedgerUpdate<RNG> + LedgerAccess + Sync + Send>(
    self,
    service_interface: ServiceInterfaceStandalone)
    -> Self
    where Self: Sized
  {
    match service_interface {
      ServiceInterfaceStandalone::LedgerUpdate => {
        self.set_route_for_ledger_update_standalone::<RNG, LU>()
      }
    }
  }

  fn set_route_for_ledger_update_standalone<RNG: 'static + RngCore + CryptoRng,
                                              LU: 'static
                                                + LedgerUpdate<RNG>
                                                + LedgerAccess
                                                + Sync
                                                + Send>(
    self)
    -> Self;
}

impl<T, B> RouteStandalone for App<T, B>
  where B: actix_web::dev::MessageBody,
        T: actix_service::NewService<Config = (),
                                     Request = dev::ServiceRequest,
                                     Response = dev::ServiceResponse<B>,
                                     Error = error::Error,
                                     InitError = ()>
{
  // Set routes for the LedgerUpdate interface via ledger_app
  fn set_route_for_ledger_update_standalone<RNG: 'static + RngCore + CryptoRng,
                                              LU: 'static
                                                + LedgerUpdate<RNG>
                                                + LedgerAccess
                                                + Sync
                                                + Send>(
    self)
    -> Self {
    self.route("/submit_transaction",
               web::post().to(submit_transaction::<RNG, LU>))
        .route("/txn_status/{handle}", web::get().to(txn_status::<RNG, LU>))
        .route("/force_end_block",
               web::post().to(force_end_block::<RNG, LU>))
  }
}

pub struct RestfulApiServiceStandalone {
  web_runtime: actix_rt::SystemRunner,
}

impl RestfulApiServiceStandalone {
  pub fn create_standalone<RNG: 'static + RngCore + CryptoRng + Sync + Send,
                             LU: 'static + LedgerUpdate<RNG> + LedgerAccess + Sync + Send>(
    ledger_app: Arc<RwLock<LedgerApp<RNG, LU>>>,
    host: &str,
    port: &str)
    -> io::Result<RestfulApiServiceStandalone> {
    let web_runtime = actix_rt::System::new("findora API");

    HttpServer::new(move || {
      App::new().data(ledger_app.clone())
                .set_route::<RNG, LU>(ServiceInterfaceStandalone::LedgerUpdate)
    }).bind(&format!("{}:{}", host, port))?
      .start();

    Ok(RestfulApiServiceStandalone { web_runtime })
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
    let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
    let ledger_state = LedgerState::test_ledger();
    let ledger_app = Arc::new(RwLock::new(LedgerApp::new(prng.clone(),
                                                         Arc::new(RwLock::new(ledger_state)),
                                                         8).unwrap()));
    let app_copy = Arc::clone(&ledger_app);
    let mut tx = Transaction::default();

    let token_code1 = AssetTypeCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::DefineAsset(asset_create));

    let mut app =
      test::init_service(App::new().data(ledger_app)
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
