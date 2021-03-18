#![feature(str_split_as_str)]
#![deny(warnings)]

use actix_cors::Cors;
use actix_service::Service;
use actix_web::{error, middleware, web, App, HttpServer};
use futures::FutureExt;
use ledger::data_model::{
    b64dec, AssetTypeCode, DefineAsset, IssuerPublicKey, KVHash, TxOutput, TxnSID,
    TxoSID, XfrAddress,
};
use ledger::{inp_fail, ser_fail};
use ledger_api_service::RestfulArchiveAccess;
use log::info;
use metrics::{Key as MetricsKey, KeyData};
use parking_lot::RwLock;
use query_server::{QueryServer, TxnIDHash};
use ruc::*;
use sparse_merkle_tree::Key;
use std::collections::HashSet;
use std::marker::{Send, Sync};
use std::sync::Arc;
use std::time::Instant;
use utils::{http_get_request, MetricsRenderer, NetworkRoute};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::OwnerMemo;

/// Returns the git commit hash and commit date of this build
#[allow(clippy::unnecessary_wraps)]
async fn version() -> actix_web::Result<String> {
    Ok(format!(
        "Build: {} {}",
        option_env!("VERGEN_SHA_SHORT_EXTERN").unwrap_or(env!("VERGEN_SHA_SHORT")),
        env!("VERGEN_BUILD_DATE")
    ))
}

// Queries the status of a transaction by its handle. Returns either a not committed message or a
// serialized TxnStatus.
async fn get_address<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<u64>,
) -> actix_web::Result<String, actix_web::error::Error>
where
    T: RestfulArchiveAccess,
    U: MetricsRenderer,
{
    let query_server = data.read();
    let address_res = query_server.get_address_of_sid(TxoSID(*info));
    let res;
    if let Some(address) = address_res {
        res = serde_json::to_string(&address)?;
    } else {
        res = format!("No utxo {} found. Please retry with a new utxo.", &info);
    }
    Ok(res)
}

type CustomDataResult = (Vec<u8>, KVHash);

// Returns custom data at a given location
async fn get_custom_data<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Option<CustomDataResult>>, actix_web::error::Error>
where
    T: RestfulArchiveAccess,
    U: MetricsRenderer,
{
    let query_server = data.read();
    let key = Key::from_base64(&*info)
        .c(d!())
        .map_err(|e| actix_web::error::ErrorBadRequest(e.generate_log()))?;
    Ok(web::Json(query_server.get_custom_data(&key).cloned()))
}

// Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
#[allow(clippy::unnecessary_wraps)]
async fn get_owner_memo<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<Option<OwnerMemo>>, actix_web::error::Error>
where
    T: RestfulArchiveAccess,
    U: MetricsRenderer,
{
    let query_server = data.read();
    Ok(web::Json(
        query_server.get_owner_memo(TxoSID(*info)).cloned(),
    ))
}

// Submits custom data to be stored by the query server. The request will fail if the hash of the
// data doesn't match the commitment stored by the ledger.
// fn store_custom_data<T, U>(
//     data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
//     body: web::Json<(String, Vec<u8>, Option<KVBlind>)>,
// ) -> actix_web::Result<(), actix_web::error::Error>
// where
//     T: RestfulArchiveAccess + Sync + Send,
//     U: MetricsRenderer,
// {
//     let (key, custom_data, blind) = body.into_inner();
//     let key = Key::from_base64(&key)
//         .c(d!())
//         .map_err(|e| actix_web::error::ErrorBadRequest(e.generate_log()))?;
//     let mut query_server = data.write();
//     query_server
//         .add_to_data_store(&key, &custom_data, blind.as_ref())
//         .c(d!())
//         .map_err(|e| error::ErrorBadRequest(e.generate_log()))
//         .map(|_| ())
// }
// Returns an array of the utxo sids currently spendable by a given address
async fn get_owned_utxos<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxoSID>>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    // Convert from basee64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log()))?,
    )
    .c(d!())
    .map_err(|e| error::ErrorBadRequest(e.generate_log()))?;
    let query_server = data.read();
    let sids = query_server.get_owned_utxo_sids(&XfrAddress { key });
    Ok(web::Json(sids.cloned().unwrap_or_default()))
}

// Returns rendered metrics
#[allow(clippy::unnecessary_wraps)]
async fn get_metrics<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    _info: web::Path<()>,
) -> actix_web::Result<String>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    let query_server = data.read();
    Ok(query_server.render())
}

pub enum QueryServerRoutes {
    GetAddress,
    GetOwnerMemo,
    GetOwnedUtxos,
    GetCustomData,
    GetCreatedAssets,
    GetTracedAssets,
    GetIssuedRecords,
    GetIssuedRecordsByCode,
    GetRelatedTxns,
    GetRelatedXfrs,
    GetAuthencatedTxnIDHash,
    GetTransactionHash,
    GetCommits,
    Version,
}

impl NetworkRoute for QueryServerRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            QueryServerRoutes::GetAddress => "get_address",
            QueryServerRoutes::GetRelatedTxns => "get_related_txns",
            QueryServerRoutes::GetRelatedXfrs => "get_related_xfrs",
            QueryServerRoutes::GetOwnedUtxos => "get_owned_utxos",
            QueryServerRoutes::GetOwnerMemo => "get_owner_memo",
            // QueryServerRoutes::StoreCustomData => "store_custom_data",
            QueryServerRoutes::GetCustomData => "get_custom_data",
            QueryServerRoutes::GetCreatedAssets => "get_created_assets",
            QueryServerRoutes::GetTracedAssets => "get_traced_assets",
            QueryServerRoutes::GetIssuedRecords => "get_issued_records",
            QueryServerRoutes::GetIssuedRecordsByCode => "get_issued_records_by_code",
            QueryServerRoutes::GetAuthencatedTxnIDHash => "get_authencated_txnid_hash",
            QueryServerRoutes::GetTransactionHash => "get_transaction_hash",
            QueryServerRoutes::GetCommits => "get_commits",
            QueryServerRoutes::Version => "version",
        };
        "/".to_owned() + endpoint
    }
}

// Returns the list of assets created by a public key
async fn get_created_assets<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<DefineAsset>>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log()))?,
    )
    .map_err(|e| error::ErrorBadRequest(e.generate_log()))?;
    let query_server = data.read();
    let assets = query_server.get_created_assets(&IssuerPublicKey { key });
    Ok(web::Json(assets.cloned().unwrap_or_default()))
}

// Returns the list of assets traced by a public key
async fn get_traced_assets<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<AssetTypeCode>>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log()))?,
    )
    .map_err(|e| error::ErrorBadRequest(e.generate_log()))?;
    let query_server = data.read();
    let assets = query_server.get_traced_assets(&IssuerPublicKey { key });
    Ok(web::Json(assets.cloned().unwrap_or_default()))
}

// Returns the list of records issued by a public key
#[allow(clippy::type_complexity)]
async fn get_issued_records<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<(TxOutput, Option<OwnerMemo>)>>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log()))?,
    )
    .map_err(|e| error::ErrorBadRequest(e.generate_log()))?;
    let query_server = data.read();
    let records = query_server.get_issued_records(&IssuerPublicKey { key });
    Ok(web::Json(records.unwrap_or_default()))
}

// Returns the list of records issued by a token code
#[allow(clippy::type_complexity)]
async fn get_issued_records_by_code<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<(TxOutput, Option<OwnerMemo>)>>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    let query_server = data.read();

    match AssetTypeCode::new_from_base64(&*info).c(d!()) {
        Ok(token_code) => {
            if let Some(records) = query_server.get_issued_records_by_code(&token_code) {
                Ok(web::Json(records))
            } else {
                Err(actix_web::error::ErrorNotFound(
                    "Specified asset definition does not currently exist.",
                ))
            }
        }
        Err(e) => Err(actix_web::error::ErrorBadRequest(e.generate_log())),
    }
}

// Returns authenticated txn sid and hash
async fn get_authenticated_txnid_hash<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<TxnIDHash>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    let query_server = data.read();
    match query_server.get_authenticated_txnid(TxoSID(*info)) {
        Some(txnid) => Ok(web::Json(txnid.clone())),
        None => Err(actix_web::error::ErrorNotFound(
            "No authenticated transaction found. Please retry with correct sid.",
        )),
    }
}

// Returns authenticated txn hash
async fn get_transaction_hash<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<usize>,
) -> actix_web::Result<web::Json<String>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    let query_server = data.read();
    match query_server.get_transaction_hash(TxnSID(*info)) {
        Some(hash) => Ok(web::Json(hash.clone())),
        None => Err(actix_web::error::ErrorNotFound(
            "No transaction found. Please retry with correct sid.",
        )),
    }
}

// Returns most recent commit count at query_server side
// Check this number to make sure query_server is in sync
async fn get_commits<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
) -> actix_web::Result<web::Json<u64>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    let query_server = data.read();
    Ok(web::Json(query_server.get_commits()))
}

// Returns the list of transations associated with a given ledger address
async fn get_related_txns<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxnSID>>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log()))?,
    )
    .c(d!())
    .map_err(|e| error::ErrorBadRequest(e.generate_log()))?;
    let query_server = data.read();
    let records = query_server.get_related_transactions(&XfrAddress { key });
    Ok(web::Json(records.cloned().unwrap_or_default()))
}

// Returns the list of transfer transations associated with a given asset
async fn get_related_xfrs<T, U>(
    data: web::Data<Arc<RwLock<QueryServer<T, U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxnSID>>>
where
    T: RestfulArchiveAccess + Sync + Send,
    U: MetricsRenderer,
{
    let query_server = data.read();
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&*info) {
        if let Some(records) = query_server.get_related_transfers(&token_code) {
            Ok(web::Json(records.clone()))
        } else {
            Err(actix_web::error::ErrorNotFound(
                "Specified asset definition does not currently exist.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid asset definition encoding.",
        ))
    }
}

pub struct QueryApi {
    web_runtime: actix_rt::SystemRunner,
}

impl QueryApi {
    pub fn create<T, U>(
        query_server: Arc<RwLock<QueryServer<T, U>>>,
        host: &str,
        port: &str,
    ) -> Result<QueryApi>
    where
        T: 'static + RestfulArchiveAccess + Sync + Send,
        U: 'static + MetricsRenderer + Sync + Send,
    {
        let web_runtime = actix_rt::System::new("findora API");

        HttpServer::new(move || {
            App::new()
                .wrap(middleware::Logger::default())
                .wrap(Cors::permissive().supports_credentials())
                .data(query_server.clone())
                .wrap_fn(|req, srv| {
                    let start = Instant::now();

                    // Get route name without url params
                    let split: Vec<_> = req.path().split('/').collect();
                    let metric_name = format!("{}_{}", "query", &split[1]);

                    srv.call(req).map(move |res| {
                        let key_data = KeyData::from_name(metric_name);
                        let key = MetricsKey::from(key_data);

                        let duration = start.elapsed();
                        metrics::recorder()
                            .record_histogram(key, duration.as_millis() as f64);

                        res
                    })
                })
                .route(
                    &QueryServerRoutes::GetAddress.with_arg_template("txo_sid"),
                    web::get().to(get_address::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetOwnedUtxos.with_arg_template("address"),
                    web::get().to(get_owned_utxos::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetOwnerMemo.with_arg_template("txo_sid"),
                    web::get().to(get_owner_memo::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetRelatedTxns.with_arg_template("address"),
                    web::get().to(get_related_txns::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetRelatedXfrs.with_arg_template("asset_token"),
                    web::get().to(get_related_xfrs::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetCreatedAssets.with_arg_template("address"),
                    web::get().to(get_created_assets::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetTracedAssets.with_arg_template("address"),
                    web::get().to(get_traced_assets::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetIssuedRecords.with_arg_template("address"),
                    web::get().to(get_issued_records::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetIssuedRecordsByCode
                        .with_arg_template("asset_token"),
                    web::get().to(get_issued_records_by_code::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetAuthencatedTxnIDHash
                        .with_arg_template("txo_sid"),
                    web::get().to(get_authenticated_txnid_hash::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetTransactionHash.with_arg_template("txn_sid"),
                    web::get().to(get_transaction_hash::<T, U>),
                )
                .route(
                    &QueryServerRoutes::GetCommits.route(),
                    web::get().to(get_commits::<T, U>),
                )
                // .route(
                //     &QueryServerRoutes::StoreCustomData.route(),
                //     web::post().to(store_custom_data::<T, U>),
                // )
                .route(
                    &QueryServerRoutes::GetCustomData.with_arg_template("key"),
                    web::get().to(get_custom_data::<T, U>),
                )
                .route(&QueryServerRoutes::Version.route(), web::get().to(version))
                .route(
                    &String::from("/metrics"),
                    web::get().to(get_metrics::<T, U>),
                )
        })
        .bind(&format!("{}:{}", host, port))
        .c(d!())?
        .run();

        info!("Query server started");

        Ok(QueryApi { web_runtime })
    }

    // call from a thread; this will block.
    pub fn run(self) -> Result<()> {
        self.web_runtime.run().c(d!())
    }
}

// Trait for rest clients that can access the query server
pub trait RestfulQueryServerAccess {
    // fn store_custom_data(
    //     &mut self,
    //     data: &dyn AsRef<[u8]>,
    //     key: &Key,
    //     blind: Option<KVBlind>,
    // ) -> Result<()>;

    fn fetch_custom_data(&self, key: &Key) -> Result<Vec<u8>>;

    fn get_owner_memo(&self, txo_sid: u64) -> Result<Option<OwnerMemo>>;
}

// Unimplemented until I can figure out a way to force the mock server to get new data (we can do
// this with a new endpoint)
pub struct MockQueryServerClient();

impl RestfulQueryServerAccess for MockQueryServerClient {
    // fn store_custom_data(
    //     &mut self,
    //     _data: &dyn AsRef<[u8]>,
    //     _key: &Key,
    //     _blind: Option<KVBlind>,
    // ) -> Result<()> {
    //     unimplemented!();
    // }

    fn fetch_custom_data(&self, _key: &Key) -> Result<Vec<u8>> {
        unimplemented!();
    }

    fn get_owner_memo(&self, _txo_sid: u64) -> Result<Option<OwnerMemo>> {
        unimplemented!();
    }
}

pub struct ActixQueryServerClient {
    port: usize,
    host: String,
    protocol: String,
}

impl ActixQueryServerClient {
    pub fn new(port: usize, host: &str, protocol: &str) -> Self {
        ActixQueryServerClient {
            port,
            host: String::from(host),
            protocol: String::from(protocol),
        }
    }
}

impl RestfulQueryServerAccess for ActixQueryServerClient {
    // fn store_custom_data(
    //     &mut self,
    //     data: &dyn AsRef<[u8]>,
    //     key: &Key,
    //     blind: Option<KVBlind>,
    // ) -> Result<()> {
    //     let query = format!(
    //         "{}://{}:{}{}",
    //         self.protocol,
    //         self.host,
    //         self.port,
    //         QueryServerRoutes::StoreCustomData.route()
    //     );
    //     http_post_request(&query, Some(&(key, data.as_ref().to_vec(), blind)))
    //         .c(d!(inp_fail!()))
    //         .map(|_| ())
    // }

    fn fetch_custom_data(&self, key: &Key) -> Result<Vec<u8>> {
        let b64key = key.to_base64();
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            QueryServerRoutes::GetCustomData.with_arg(&b64key)
        );
        let text = http_get_request(&query).c(d!(inp_fail!()))?;

        serde_json::from_str::<Vec<u8>>(&text).c(d!(ser_fail!()))
    }

    fn get_owner_memo(&self, txo_sid: u64) -> Result<Option<OwnerMemo>> {
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            QueryServerRoutes::GetOwnerMemo.with_arg(&txo_sid)
        );
        let text = http_get_request(&query).c(d!(inp_fail!()))?;
        serde_json::from_str::<Option<OwnerMemo>>(&text).c(d!(ser_fail!()))
    }
}
