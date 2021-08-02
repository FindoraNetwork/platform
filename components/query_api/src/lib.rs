#![deny(warnings)]

use actix_cors::Cors;
use actix_service::Service;
use actix_web::{error, middleware, web, App, HttpServer};
use futures::FutureExt;
use ledger::data_model::{
    b64dec, AssetTypeCode, DefineAsset, IssuerPublicKey, Transaction, TxOutput, TxnSID,
    TxoSID, XfrAddress,
};
use ledger::staking::ops::mint_fra::MintEntry;
use ledger::{inp_fail, ser_fail};
use log::info;
use metrics::{Key as MetricsKey, KeyData};
use parking_lot::RwLock;
use query_server::{QueryServer, TxnIDHash};
use ruc::*;
use serde::{Deserialize, Serialize};
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
        option_env!("VERGEN_SHA_EXTERN").unwrap_or(env!("VERGEN_SHA")),
        env!("VERGEN_BUILD_DATE")
    ))
}

// Queries the status of a transaction by its handle. Returns either a not committed message or a
// serialized TxnStatus.
async fn get_address<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<u64>,
) -> actix_web::Result<String, actix_web::error::Error>
where
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

// Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
#[allow(clippy::unnecessary_wraps)]
async fn get_owner_memo<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<Option<OwnerMemo>>, actix_web::error::Error>
where
    U: MetricsRenderer,
{
    let query_server = data.read();
    Ok(web::Json(
        query_server.get_owner_memo(TxoSID(*info)).cloned(),
    ))
}

#[allow(clippy::unnecessary_wraps)]
async fn get_owner_memo_batch<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<Option<OwnerMemo>>>, actix_web::error::Error>
where
    U: MetricsRenderer,
{
    let ids = info
        .as_ref()
        .split(',')
        .map(|i| i.parse::<u64>().map_err(actix_web::error::ErrorBadRequest))
        .collect::<actix_web::Result<Vec<_>, actix_web::error::Error>>()?;
    let hdr = data.read();
    let resp = ids
        .into_iter()
        .map(|i| hdr.get_owner_memo(TxoSID(i)).cloned())
        .collect();
    Ok(web::Json(resp))
}

// Returns an array of the utxo sids currently spendable by a given address
async fn get_owned_utxos<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxoSID>>>
where
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
async fn get_metrics<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    _info: web::Path<()>,
) -> actix_web::Result<String>
where
    U: MetricsRenderer,
{
    let query_server = data.read();
    Ok(query_server.render())
}

pub enum QueryServerRoutes {
    GetAddress,
    GetOwnerMemo,
    GetOwnerMemoBatch,
    GetOwnedUtxos,
    GetCreatedAssets,
    GetTracedAssets,
    GetIssuedRecords,
    GetIssuedRecordsByCode,
    GetRelatedTxns,
    GetRelatedXfrs,
    GetAuthencatedTxnIDHash,
    GetTransactionHash,
    GetTransactionSid,
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
            QueryServerRoutes::GetOwnerMemoBatch => "get_owner_memo_batch",
            QueryServerRoutes::GetCreatedAssets => "get_created_assets",
            QueryServerRoutes::GetTracedAssets => "get_traced_assets",
            QueryServerRoutes::GetIssuedRecords => "get_issued_records",
            QueryServerRoutes::GetIssuedRecordsByCode => "get_issued_records_by_code",
            QueryServerRoutes::GetAuthencatedTxnIDHash => "get_authencated_txnid_hash",
            QueryServerRoutes::GetTransactionHash => "get_transaction_hash",
            QueryServerRoutes::GetTransactionSid => "get_transaction_sid",
            QueryServerRoutes::GetCommits => "get_commits",
            QueryServerRoutes::Version => "version",
        };
        "/".to_owned() + endpoint
    }
}

// Returns the list of assets created by a public key
async fn get_created_assets<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<DefineAsset>>>
where
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
async fn get_traced_assets<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<AssetTypeCode>>>
where
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
async fn get_issued_records<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<(TxOutput, Option<OwnerMemo>)>>>
where
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
async fn get_issued_records_by_code<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<(TxOutput, Option<OwnerMemo>)>>>
where
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
async fn get_authenticated_txnid_hash<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<TxnIDHash>>
where
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

// Returns txn hash by sid
async fn get_transaction_hash<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<usize>,
) -> actix_web::Result<web::Json<String>>
where
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

// Returns txn sid by hash
async fn get_transaction_sid<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<usize>>
where
    U: MetricsRenderer,
{
    let query_server = data.read();
    match query_server.get_transaction_sid((*info).clone()) {
        Some(sid) => Ok(web::Json(sid.0)),
        None => Err(actix_web::error::ErrorNotFound(
            "No transaction found. Please retry with correct hash.",
        )),
    }
}

// Returns most recent commit count at query_server side
// Check this number to make sure query_server is in sync
async fn get_commits<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
) -> actix_web::Result<web::Json<u64>>
where
    U: MetricsRenderer,
{
    let query_server = data.read();
    Ok(web::Json(query_server.get_commits()))
}

#[derive(Debug, Deserialize)]
pub struct WalletQueryParams {
    address: String,
    page: usize,
    per_page: usize,
    order: OrderOption,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
enum OrderOption {
    Desc,
    Asc,
}

#[derive(Debug, Deserialize, Serialize)]
struct CoinbaseTxnBody {
    height: u64,
    data: MintEntry,
}

#[derive(Debug, Deserialize, Serialize)]
struct CoinbaseOperInfo {
    total_count: u64,
    txs: Vec<CoinbaseTxnBody>,
}

async fn get_coinbase_oper_list<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    web::Query(info): web::Query<WalletQueryParams>,
) -> actix_web::Result<web::Json<CoinbaseOperInfo>>
where
    U: MetricsRenderer,
{
    // Convert from base64 representation
    let key: XfrPublicKey = wallet::public_key_from_base64(&info.address)
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.generate_log()))?;

    let query_server = data.read();

    if info.page == 0 {
        return Ok(web::Json(CoinbaseOperInfo {
            total_count: 0u64,
            txs: vec![],
        }));
    }

    let start = (info.page - 1)
        .checked_mul(info.per_page)
        .c(d!())
        .map_err(error::ErrorBadRequest)?;
    let end = start
        .checked_add(info.per_page)
        .c(d!())
        .map_err(error::ErrorBadRequest)?;

    let resp = query_server
        .get_coinbase_entries(
            &XfrAddress { key },
            start,
            end,
            info.order == OrderOption::Desc,
        )
        .c(d!())
        .map_err(error::ErrorBadRequest)?;

    Ok(web::Json(CoinbaseOperInfo {
        total_count: resp.0,
        txs: resp
            .1
            .into_iter()
            .map(|r| CoinbaseTxnBody {
                height: r.0,
                data: r.1,
            })
            .collect(),
    }))
}

// Returns the list of claim transations of a given ledger address
async fn get_claim_txns<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    web::Query(info): web::Query<WalletQueryParams>,
) -> actix_web::Result<web::Json<Vec<Option<Transaction>>>>
where
    U: MetricsRenderer,
{
    // Convert from base64 representation
    let key: XfrPublicKey = wallet::public_key_from_base64(&info.address)
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.generate_log()))?;

    let query_server = data.read();

    if info.page == 0 {
        return Ok(web::Json(vec![]));
    }

    let start = (info.page - 1)
        .checked_mul(info.per_page)
        .c(d!())
        .map_err(error::ErrorBadRequest)?;
    let end = start
        .checked_add(info.per_page)
        .c(d!())
        .map_err(error::ErrorBadRequest)?;

    let records = query_server
        .get_claim_transactions(
            &XfrAddress { key },
            start,
            end,
            info.order == OrderOption::Desc,
        )
        .c(d!())
        .map_err(error::ErrorBadRequest)?;

    Ok(web::Json(records))
}

// Returns the list of transations associated with a given ledger address
async fn get_related_txns<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxnSID>>>
where
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
async fn get_related_xfrs<U>(
    data: web::Data<Arc<RwLock<QueryServer<U>>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxnSID>>>
where
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
    pub fn create<U>(
        query_server: Arc<RwLock<QueryServer<U>>>,
        host: &str,
        port: u16,
    ) -> Result<QueryApi>
    where
        U: 'static + MetricsRenderer + Sync + Send,
    {
        let web_runtime = actix_rt::System::new("findora API");

        HttpServer::new(move || {
            App::new()
                .wrap(middleware::Logger::default())
                .wrap(Cors::permissive().supports_credentials())
                .data(Arc::clone(&query_server))
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
                    web::get().to(get_address::<U>),
                )
                .route(
                    &QueryServerRoutes::GetOwnedUtxos.with_arg_template("address"),
                    web::get().to(get_owned_utxos::<U>),
                )
                .route(
                    &QueryServerRoutes::GetOwnerMemo.with_arg_template("txo_sid"),
                    web::get().to(get_owner_memo::<U>),
                )
                .route(
                    &QueryServerRoutes::GetOwnerMemoBatch
                        .with_arg_template("txo_sid_list"),
                    web::get().to(get_owner_memo_batch::<U>),
                )
                .route(
                    &QueryServerRoutes::GetRelatedTxns.with_arg_template("address"),
                    web::get().to(get_related_txns::<U>),
                )
                .service(
                    web::resource("claim_history")
                        .route(web::get().to(get_claim_txns::<U>)),
                )
                .service(
                    web::resource("coinbase_history")
                        .route(web::get().to(get_coinbase_oper_list::<U>)),
                )
                .route(
                    &QueryServerRoutes::GetRelatedXfrs.with_arg_template("asset_token"),
                    web::get().to(get_related_xfrs::<U>),
                )
                .route(
                    &QueryServerRoutes::GetCreatedAssets.with_arg_template("address"),
                    web::get().to(get_created_assets::<U>),
                )
                .route(
                    &QueryServerRoutes::GetTracedAssets.with_arg_template("address"),
                    web::get().to(get_traced_assets::<U>),
                )
                .route(
                    &QueryServerRoutes::GetIssuedRecords.with_arg_template("address"),
                    web::get().to(get_issued_records::<U>),
                )
                .route(
                    &QueryServerRoutes::GetIssuedRecordsByCode
                        .with_arg_template("asset_token"),
                    web::get().to(get_issued_records_by_code::<U>),
                )
                .route(
                    &QueryServerRoutes::GetAuthencatedTxnIDHash
                        .with_arg_template("txo_sid"),
                    web::get().to(get_authenticated_txnid_hash::<U>),
                )
                .route(
                    &QueryServerRoutes::GetTransactionHash.with_arg_template("txn_sid"),
                    web::get().to(get_transaction_hash::<U>),
                )
                .route(
                    &QueryServerRoutes::GetTransactionSid.with_arg_template("txn_hash"),
                    web::get().to(get_transaction_sid::<U>),
                )
                .route(
                    &QueryServerRoutes::GetCommits.route(),
                    web::get().to(get_commits::<U>),
                )
                .route(&QueryServerRoutes::Version.route(), web::get().to(version))
                .route(&String::from("/metrics"), web::get().to(get_metrics::<U>))
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
    fn get_owner_memo(&self, txo_sid: u64) -> Result<Option<OwnerMemo>>;
}

// Unimplemented until I can figure out a way to force the mock server to get new data (we can do
// this with a new endpoint)
pub struct MockQueryServerClient();

impl RestfulQueryServerAccess for MockQueryServerClient {
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

pub mod service {
    use super::QueryApi;
    use ledger::store::LedgerState;
    use metrics_exporter_prometheus::PrometheusHandle;
    use parking_lot::RwLock;
    use query_server::{QueryServer, BLOCK_CREATED};
    use ruc::*;
    use std::{sync::Arc, thread};
    use utils::MetricsRenderer;

    pub struct PromHandle(metrics_exporter_prometheus::PrometheusHandle);

    impl PromHandle {
        pub fn new(h: PrometheusHandle) -> PromHandle {
            PromHandle(h)
        }
    }

    impl MetricsRenderer for PromHandle {
        fn rendered(&self) -> String {
            self.0.render()
        }
    }

    pub fn start_query_server(
        ledger: Arc<RwLock<LedgerState>>,
        host: &str,
        port: u16,
    ) -> Result<Arc<RwLock<QueryServer<PromHandle>>>> {
        // Extract prometheus handle
        let builder = metrics_exporter_prometheus::PrometheusBuilder::new();
        let recorder = builder.build();
        let handle = PromHandle::new(recorder.handle());

        // Register recorder
        let _ = metrics::set_boxed_recorder(Box::new(recorder));

        let qs = Arc::new(RwLock::new(QueryServer::new(ledger, handle)));
        QueryApi::create(Arc::clone(&qs), host, port)
            .c(d!())
            .map(|_| {
                let qs1 = Arc::clone(&qs);
                thread::spawn(move || loop {
                    let mut created = BLOCK_CREATED.0.lock();
                    if !*created {
                        BLOCK_CREATED.1.wait(&mut created);
                    }
                    qs1.write().update();
                    *created = false;
                });
                qs
            })
    }
}
