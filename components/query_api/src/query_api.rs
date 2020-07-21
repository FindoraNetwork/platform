#![deny(warnings)]
use actix_cors::Cors;
use actix_web::{error, middleware, web, App, HttpServer};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  b64dec, b64enc, AssetTypeCode, IssuerPublicKey, KVBlind, KVHash, TxOutput, TxnSID, TxoSID,
  XfrAddress,
};
use ledger::{error_location, inp_fail, ser_fail};
use ledger_api_service::RestfulArchiveAccess;
use log::info;
use query_server::QueryServer;
use sparse_merkle_tree::Key;
use std::collections::HashSet;
use std::io;
use std::marker::{Send, Sync};
use std::sync::{Arc, RwLock};
use utils::{actix_get_request, actix_post_request, NetworkRoute};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::OwnerMemo;

// Queries the status of a transaction by its handle. Returns either a not committed message or a
// serialized TxnStatus.
fn get_address<T>(data: web::Data<Arc<RwLock<QueryServer<T>>>>,
                  info: web::Path<u64>)
                  -> Result<String, actix_web::error::Error>
  where T: RestfulArchiveAccess
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
fn get_custom_data<T>(
  data: web::Data<Arc<RwLock<QueryServer<T>>>>,
  info: web::Path<String>)
  -> actix_web::Result<web::Json<Option<CustomDataResult>>, actix_web::error::Error>
  where T: RestfulArchiveAccess
{
  let query_server = data.read().unwrap();
  let key = key_from_base64(&*info)?;
  Ok(web::Json(query_server.get_custom_data(&key).cloned()))
}

// Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
fn get_owner_memo<T>(data: web::Data<Arc<RwLock<QueryServer<T>>>>,
                     info: web::Path<u64>)
                     -> actix_web::Result<web::Json<Option<OwnerMemo>>, actix_web::error::Error>
  where T: RestfulArchiveAccess
{
  let query_server = data.read().unwrap();
  Ok(web::Json(query_server.get_owner_memo(TxoSID(*info)).cloned()))
}

// Submits custom data to be stored by the query server. The request will fail if the hash of the
// data doesn't match the commitment stored by the ledger.
fn store_custom_data<T>(data: web::Data<Arc<RwLock<QueryServer<T>>>>,
                        body: web::Json<(Key, Vec<u8>, Option<KVBlind>)>)
                        -> actix_web::Result<(), actix_web::error::Error>
  where T: RestfulArchiveAccess + Sync + Send
{
  let (key, custom_data, blind) = body.into_inner();
  let mut query_server = data.write().unwrap();
  query_server.add_to_data_store(&key, &custom_data, blind.as_ref())
              .map_err(|e| error::ErrorBadRequest(format!("{}", e)))?;
  Ok(())
}
// Returns an array of the utxo sids currently spendable by a given address
fn get_owned_utxos<T>(data: web::Data<Arc<RwLock<QueryServer<T>>>>,
                      info: web::Path<String>)
                      -> actix_web::Result<web::Json<HashSet<TxoSID>>>
  where T: RestfulArchiveAccess + Sync + Send
{
  // Convert from basee64 representation
  let key: XfrPublicKey =
    XfrPublicKey::zei_from_bytes(&b64dec(&*info).map_err(|_| {
                                    error::ErrorBadRequest("Could not deserialize public key")
                                  })?).map_err(|_| {
                                        error::ErrorBadRequest("Could not deserialize public key")
                                      })?;
  let query_server = data.read().unwrap();
  let sids = query_server.get_owned_utxo_sids(&XfrAddress { key });
  Ok(web::Json(sids.cloned().unwrap_or_default()))
}

pub enum QueryServerRoutes {
  GetAddress,
  GetOwnerMemo,
  GetOwnedUtxos,
  StoreCustomData,
  GetCustomData,
  GetCreatedAssets,
  GetIssuedRecords,
  GetRelatedTxns,
}

impl NetworkRoute for QueryServerRoutes {
  fn route(&self) -> String {
    let endpoint = match *self {
      QueryServerRoutes::GetAddress => "get_address",
      QueryServerRoutes::GetRelatedTxns => "get_related_txns",
      QueryServerRoutes::GetOwnedUtxos => "get_owned_utxos",
      QueryServerRoutes::GetOwnerMemo => "get_owner_memo",
      QueryServerRoutes::StoreCustomData => "store_custom_data",
      QueryServerRoutes::GetCustomData => "get_custom_data",
      QueryServerRoutes::GetCreatedAssets => "get_created_assets",
      QueryServerRoutes::GetIssuedRecords => "get_issued_records",
    };
    "/".to_owned() + endpoint
  }
}

// Returns the list of assets created by a public key
fn get_created_assets<T>(data: web::Data<Arc<RwLock<QueryServer<T>>>>,
                         info: web::Path<String>)
                         -> actix_web::Result<web::Json<Vec<AssetTypeCode>>>
  where T: RestfulArchiveAccess + Sync + Send
{
  // Convert from base64 representation
  let key: XfrPublicKey =
    XfrPublicKey::zei_from_bytes(&b64dec(&*info).map_err(|_| {
                                    error::ErrorBadRequest("Could not deserialize public key")
                                  })?).map_err(|_| {
                                        error::ErrorBadRequest("Could not deserialize public key")
                                      })?;
  let query_server = data.read().unwrap();
  let assets = query_server.get_created_assets(&IssuerPublicKey { key });
  Ok(web::Json(assets.cloned().unwrap_or_default()))
}

// Returns the list of records issued by a public key
fn get_issued_records<T>(data: web::Data<Arc<RwLock<QueryServer<T>>>>,
                         info: web::Path<String>)
                         -> actix_web::Result<web::Json<Vec<TxOutput>>>
  where T: RestfulArchiveAccess + Sync + Send
{
  // Convert from base64 representation
  let key: XfrPublicKey =
    XfrPublicKey::zei_from_bytes(&b64dec(&*info).map_err(|_| {
                                    error::ErrorBadRequest("Could not deserialize public key")
                                  })?).map_err(|_| {
                                        error::ErrorBadRequest("Could not deserialize public key")
                                      })?;
  let query_server = data.read().unwrap();
  let records = query_server.get_issued_records(&IssuerPublicKey { key });
  Ok(web::Json(records.cloned().unwrap_or_default()))
}

// Returns the list of transations associated with a given ledger address
fn get_related_txns<T>(data: web::Data<Arc<RwLock<QueryServer<T>>>>,
                       info: web::Path<String>)
                       -> actix_web::Result<web::Json<HashSet<TxnSID>>>
  where T: RestfulArchiveAccess + Sync + Send
{
  // Convert from base64 representation
  let key: XfrPublicKey =
    XfrPublicKey::zei_from_bytes(&b64dec(&*info).map_err(|_| {
                                    error::ErrorBadRequest("Could not deserialize public key")
                                  })?).map_err(|_| {
                                        error::ErrorBadRequest("Could not deserialize public key")
                                      })?;
  let query_server = data.read().unwrap();
  let records = query_server.get_related_transactions(&XfrAddress { key });
  Ok(web::Json(records.cloned().unwrap_or_default()))
}

pub struct QueryApi {
  web_runtime: actix_rt::SystemRunner,
}

impl QueryApi {
  pub fn create<T>(query_server: Arc<RwLock<QueryServer<T>>>,
                   host: &str,
                   port: &str)
                   -> io::Result<QueryApi>
    where T: 'static + RestfulArchiveAccess + Sync + Send
  {
    let web_runtime = actix_rt::System::new("findora API");

    HttpServer::new(move || {
      App::new().wrap(middleware::Logger::default())
                .wrap(Cors::new().supports_credentials())
                .data(query_server.clone())
                .route(&QueryServerRoutes::GetAddress.with_arg_template("txo_sid"),
                       web::get().to(get_address::<T>))
                .route(&QueryServerRoutes::GetOwnedUtxos.with_arg_template("address"),
                       web::get().to(get_owned_utxos::<T>))
                .route(&QueryServerRoutes::GetOwnerMemo.with_arg_template("txo_sid"),
                       web::get().to(get_owner_memo::<T>))
                .route(&QueryServerRoutes::GetRelatedTxns.with_arg_template("address"),
                       web::get().to(get_related_txns::<T>))
                .route(&QueryServerRoutes::GetCreatedAssets.with_arg_template("address"),
                       web::get().to(get_created_assets::<T>))
                .route(&QueryServerRoutes::GetIssuedRecords.with_arg_template("address"),
                       web::get().to(get_issued_records::<T>))
                .route(&QueryServerRoutes::StoreCustomData.route(),
                       web::post().to(store_custom_data::<T>))
                .route(&QueryServerRoutes::GetCustomData.with_arg_template("key"),
                       web::get().to(get_custom_data::<T>))
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

// Trait for rest clients that can access the query server
pub trait RestfulQueryServerAccess {
  fn store_custom_data(&mut self,
                       data: &dyn AsRef<[u8]>,
                       key: &Key,
                       blind: Option<KVBlind>)
                       -> Result<(), PlatformError>;

  fn fetch_custom_data(&self, key: &Key) -> Result<Vec<u8>, PlatformError>;
}

// Unimplemented until I can figure out a way to force the mock server to get new data (we can do
// this with a new endpoint)
pub struct MockQueryServerClient();

impl RestfulQueryServerAccess for MockQueryServerClient {
  fn store_custom_data(&mut self,
                       _data: &dyn AsRef<[u8]>,
                       _key: &Key,
                       _blind: Option<KVBlind>)
                       -> Result<(), PlatformError> {
    unimplemented!();
  }

  fn fetch_custom_data(&self, _key: &Key) -> Result<Vec<u8>, PlatformError> {
    unimplemented!();
  }
}

pub struct ActixQueryServerClient {
  port: usize,
  host: String,
  protocol: String,
  client: reqwest::Client,
}

impl ActixQueryServerClient {
  pub fn new(port: usize, host: &str, protocol: &str) -> Self {
    ActixQueryServerClient { port,
                             host: String::from(host),
                             protocol: String::from(protocol),
                             client: reqwest::Client::new() }
  }
}

impl RestfulQueryServerAccess for ActixQueryServerClient {
  fn store_custom_data(&mut self,
                       data: &dyn AsRef<[u8]>,
                       key: &Key,
                       blind: Option<KVBlind>)
                       -> Result<(), PlatformError> {
    let query = format!("{}://{}:{}{}",
                        self.protocol,
                        self.host,
                        self.port,
                        QueryServerRoutes::StoreCustomData.route());
    actix_post_request(&self.client,
                       &query,
                       Some(&(key, data.as_ref().to_vec(), blind))).map_err(|_| inp_fail!())?;
    Ok(())
  }

  fn fetch_custom_data(&self, key: &Key) -> Result<Vec<u8>, PlatformError> {
    let b64key = b64enc(&key);
    let query = format!("{}://{}:{}{}",
                        self.protocol,
                        self.host,
                        self.port,
                        QueryServerRoutes::GetCustomData.with_arg(&b64key));
    let text = actix_get_request(&self.client, &query).map_err(|_| inp_fail!())?;
    Ok(serde_json::from_str::<Vec<u8>>(&text).map_err(|_| ser_fail!())?)
  }
}
