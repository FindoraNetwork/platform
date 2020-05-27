#![deny(warnings)]
use actix_cors::Cors;
use actix_web::{error, middleware, web, App, HttpServer};
use ledger::data_model::{b64dec, KVBlind, KVHash, TxoSID, XfrAddress};
use ledger::store::{ArchiveAccess, LedgerAccess, LedgerUpdate};
use log::info;
use query_server::QueryServer;
use rand_core::{CryptoRng, RngCore};
use sparse_merkle_tree::Key;
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

fn key_from_base64(b64_str: &str) -> Result<Key, actix_web::error::Error> {
  Ok(Key::from_slice(&b64dec(b64_str).map_err(|_| {
                        actix_web::error::ErrorBadRequest("Could not deserialize key(1)")
                      })?).ok_or_else(|| {
                            actix_web::error::ErrorBadRequest("Could not deserialize key")
                          })?)
}

type CustomDataResult = (Vec<u8>, KVHash);

// Returns custom data at a given location
fn get_custom_data<RNG, LU>(
  data: web::Data<Arc<RwLock<QueryServer<RNG, LU>>>>,
  info: web::Path<String>)
  -> actix_web::Result<web::Json<Option<CustomDataResult>>, actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send
{
  let query_server = data.read().unwrap();
  let key = key_from_base64(&*info)?;
  Ok(web::Json(query_server.get_custom_data(&key).cloned()))
}

// Submits custom data to be stored by the query server. The request will fail if the hash of the
// data doesn't match the commitment stored by the ledger.
fn store_custom_data<RNG, LU>(data: web::Data<Arc<RwLock<QueryServer<RNG, LU>>>>,
                              body: web::Json<(Key, Vec<u8>, Option<KVBlind>)>)
                              -> actix_web::Result<(), actix_web::error::Error>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send
{
  let (key, custom_data, blind) = body.into_inner();
  let mut query_server = data.write().unwrap();
  query_server.add_to_data_store(&key, &custom_data, blind.as_ref())
              .map_err(|e| error::ErrorBadRequest(format!("{}", e)))?;
  Ok(())
}

// Returns an array of the utxo sids currently spendable by a given address
fn get_owned_txos<RNG, LU>(data: web::Data<Arc<RwLock<QueryServer<RNG, LU>>>>,
                           info: web::Path<String>)
                           -> actix_web::Result<web::Json<HashSet<TxoSID>>>
  where RNG: RngCore + CryptoRng,
        LU: LedgerUpdate<RNG> + LedgerAccess + ArchiveAccess + Sync + Send
{
  // Convert from basee64 representation
  let key: XfrPublicKey =
    XfrPublicKey::zei_from_bytes(&b64dec(&*info).map_err(|_| {
                                    error::ErrorBadRequest("Could not deserialize public key")
                                  })?);
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
                .route("/store_custom_data",
                       web::post().to(store_custom_data::<RNG, LU>))
                .route("/get_custom_data/{key}",
                       web::get().to(get_custom_data::<RNG, LU>))
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
