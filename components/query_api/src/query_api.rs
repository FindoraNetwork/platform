#![deny(warnings)]
use actix_cors::Cors;
use actix_web::{middleware, web, App, HttpServer};
use ledger::data_model::{b64dec, TxoSID, XfrAddress};
use ledger::store::{ArchiveAccess, LedgerAccess, LedgerUpdate};
use log::info;
use query_server::QueryServer;
use rand_core::{CryptoRng, RngCore};
use std::collections::HashSet;
use std::io;
use std::marker::{Send, Sync};
use std::sync::{Arc, RwLock};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;

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

// Returns an array of the utxo sids currently spendable by a given address
fn get_owned_txos<RNG, LU>(data: web::Data<Arc<RwLock<QueryServer<RNG, LU>>>>,
                           info: web::Path<String>)
                           -> actix_web::Result<web::Json<HashSet<TxoSID>>>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send
{
  // Convert from basee64 representation
  let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(&b64dec(&*info).map_err(|_| actix_web::error::ErrorBadRequest("Could not deserialize public key"))?);
  let query_server = data.read().unwrap();
  let sids = query_server.get_owned_utxo_sids(&XfrAddress { key });
  Ok(web::Json(sids.unwrap_or_default()))
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
                .route("/get_owned_utxos/{address}",
                       web::get().to(get_owned_txos::<RNG, LU>))
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
