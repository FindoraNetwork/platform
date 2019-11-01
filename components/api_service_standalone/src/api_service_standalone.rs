extern crate actix_rt;
extern crate actix_web;
extern crate ledger;
extern crate ledger_app;
extern crate percent_encoding;
extern crate rand;
extern crate serde_json;

use actix_web::{dev, error, web, App, HttpServer};
use ledger::store::LedgerUpdate;
use ledger_app::LedgerApp;
use percent_encoding::percent_decode_str;
use rand::{CryptoRng, Rng};
use std::io;
use std::marker::{Send, Sized, Sync};
use std::sync::{Arc, RwLock};

fn submit_transaction_standalone<RNG, LU>(data: web::Data<Arc<RwLock<LedgerApp<RNG, LU>>>>,
                                          info: web::Path<String>)
  where RNG: Rng + CryptoRng,
        LU: LedgerUpdate<RNG> + Sync + Send
{
  let mut ledger_app = data.write().unwrap();
  let uri_string = percent_decode_str(&*info).decode_utf8().unwrap();
  let tx = serde_json::from_str(&uri_string).map_err(|e| actix_web::error::ErrorBadRequest(e))
                                            .unwrap();

  ledger_app.handle_transaction(tx);
}

pub enum ServiceInterfaceStandalone {
  LedgerUpdate,
}

pub trait RouteStandalone {
  fn set_route<RNG: 'static + Rng + CryptoRng, LU: 'static + LedgerUpdate<RNG> + Sync + Send>(
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

  fn set_route_for_ledger_update_standalone<RNG: 'static + Rng + CryptoRng,
                                              LU: 'static + LedgerUpdate<RNG> + Sync + Send>(
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
  fn set_route_for_ledger_update_standalone<RNG: 'static + Rng + CryptoRng,
                                              LU: 'static + LedgerUpdate<RNG> + Sync + Send>(
    self)
    -> Self {
    self.route("/submit_transaction_standalone/{tx}",
               web::post().to(submit_transaction_standalone::<RNG, LU>))
  }
}

pub struct RestfulApiServiceStandalone {
  web_runtime: actix_rt::SystemRunner,
}

impl RestfulApiServiceStandalone {
  pub fn create_standalone<RNG: 'static + Rng + CryptoRng + Sync + Send,
                             LU: 'static + LedgerUpdate<RNG> + Sync + Send>(
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
  use ledger::store::LedgerState;
  use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
  use rand::SeedableRng;

  #[test]
  fn test_submit_transaction_standalone() {
    let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
    let ledger_state = LedgerState::test_ledger();
    let ledger_app = LedgerApp::new(prng.clone(), ledger_state).unwrap();
    let mut tx = Transaction::default();

    let token_code1 = AssetTypeCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::DefineAsset(asset_create));

    let mut app =
      test::init_service(App::new().data(Arc::new(RwLock::new(ledger_app)))
                                   .route("/submit_transaction_standalone/{tx}",
                                          web::post().to(submit_transaction_standalone::<rand_chacha::ChaChaRng,
                                                                            LedgerState>)));

    let serialize = serde_json::to_string(&tx).unwrap();
    // Set of invalid URI characters that may appear in a JSON transaction
    // TODO: (Noah) make sure set is complete
    const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ')
                                         .add(b'"')
                                         .add(b'`')
                                         .add(b'{')
                                         .add(b'/')
                                         .add(b'}');
    let uri_string = utf8_percent_encode(&serialize, FRAGMENT).to_string();

    let req = test::TestRequest::post().uri(&format!("/submit_transaction_standalone/{}",
                                                     uri_string))
                                       .to_request();

    let submit_resp = test::block_on(app.call(req)).unwrap();

    assert!(submit_resp.status().is_success());
  }
}
