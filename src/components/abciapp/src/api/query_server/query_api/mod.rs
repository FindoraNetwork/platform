//!
//! need to transform the data in ledgerState to store
//!

pub mod query_server;
pub mod service;

#[cfg(test)]
#[allow(missing_docs)]
mod test;

use actix_cors::Cors;
use actix_web::{error, middleware, web, App, HttpServer};
use finutils::api::NetworkRoute;
use globutils::http_get_request;
use ledger::{
    data_model::{
        b64dec, AssetTypeCode, DefineAsset, IssuerPublicKey, Transaction, TxOutput,
        TxnSID, TxoSID, XfrAddress,
    },
    staking::ops::mint_fra::MintEntry,
};
use log::info;
use parking_lot::RwLock;
use query_server::{QueryServer, TxnIDHash};
use ruc::*;
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, sync::Arc};
use zei::{
    serialization::ZeiFromToBytes,
    xfr::{sig::XfrPublicKey, structs::OwnerMemo},
};

/// Returns the git commit hash and commit date of this build
#[allow(clippy::unnecessary_wraps)]
async fn version() -> actix_web::Result<String> {
    Ok(format!(
        "Build: {} {}",
        option_env!("VERGEN_SHA_EXTERN").unwrap_or(env!("VERGEN_SHA")),
        env!("VERGEN_BUILD_DATE")
    ))
}

/// Queries the status of a transaction by its handle. Returns either a not committed message or a
/// serialized TxnStatus.
async fn get_address(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<String, actix_web::error::Error> {
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

/// Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
#[allow(clippy::unnecessary_wraps)]
async fn get_owner_memo(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<Option<OwnerMemo>>, actix_web::error::Error> {
    let query_server = data.read();
    Ok(web::Json(query_server.get_owner_memo(TxoSID(*info))))
}

/// Separate a string of `TxoSID` by ',' and query the corresponding memo
#[allow(clippy::unnecessary_wraps)]
async fn get_owner_memo_batch(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<Option<OwnerMemo>>>, actix_web::error::Error> {
    let ids = info
        .as_ref()
        .split(',')
        .map(|i| i.parse::<u64>().map_err(actix_web::error::ErrorBadRequest))
        .collect::<actix_web::Result<Vec<_>, actix_web::error::Error>>()?;
    let hdr = data.read();
    let resp = ids
        .into_iter()
        .map(|i| hdr.get_owner_memo(TxoSID(i)))
        .collect();
    Ok(web::Json(resp))
}

/// Returns an array of the utxo sids currently spendable by a given address
async fn get_owned_utxos(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxoSID>>> {
    // Convert from basee64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?,
    )
    .c(d!())
    .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?;
    let query_server = data.read();
    let sids = query_server.get_owned_utxo_sids(&XfrAddress { key });
    Ok(web::Json(sids.unwrap_or_default()))
}

/// Define interface type
#[allow(missing_docs)]
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

/// Returns the list of assets created by a public key
async fn get_created_assets(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<DefineAsset>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?,
    )
    .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?;
    let query_server = data.read();
    let assets = query_server.get_created_assets(&IssuerPublicKey { key });
    Ok(web::Json(assets.unwrap_or_default()))
}

/// Returns the list of assets traced by a public key
async fn get_traced_assets(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<AssetTypeCode>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?,
    )
    .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?;
    let query_server = data.read();
    let assets = query_server.get_traced_assets(&IssuerPublicKey { key });
    Ok(web::Json(assets.unwrap_or_default()))
}

/// Returns the list of records issued by a public key
#[allow(clippy::type_complexity)]
async fn get_issued_records(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<(TxOutput, Option<OwnerMemo>)>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?,
    )
    .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?;
    let query_server = data.read();
    let records = query_server.get_issued_records(&IssuerPublicKey { key });
    Ok(web::Json(records.unwrap_or_default()))
}

/// Returns the list of records issued by a token code
#[allow(clippy::type_complexity)]
async fn get_issued_records_by_code(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<(TxOutput, Option<OwnerMemo>)>>> {
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
        Err(e) => Err(actix_web::error::ErrorBadRequest(e.generate_log(None))),
    }
}

/// Returns authenticated txn sid and hash
async fn get_authenticated_txnid_hash(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<TxnIDHash>> {
    let query_server = data.read();
    match query_server.get_authenticated_txnid(TxoSID(*info)) {
        Some(txnid) => Ok(web::Json(txnid)),
        None => Err(actix_web::error::ErrorNotFound(
            "No authenticated transaction found. Please retry with correct sid.",
        )),
    }
}

/// Returns txn hash by sid
async fn get_transaction_hash(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<usize>,
) -> actix_web::Result<web::Json<String>> {
    let query_server = data.read();
    match query_server.get_transaction_hash(TxnSID(*info)) {
        Some(hash) => Ok(web::Json(hash)),
        None => Err(actix_web::error::ErrorNotFound(
            "No transaction found. Please retry with correct sid.",
        )),
    }
}

/// Returns txn sid by hash
async fn get_transaction_sid(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<usize>> {
    let query_server = data.read();
    match query_server.get_transaction_sid((*info).clone()) {
        Some(sid) => Ok(web::Json(sid.0)),
        None => Err(actix_web::error::ErrorNotFound(
            "No transaction found. Please retry with correct hash.",
        )),
    }
}

/// Returns most recent commit count at query_server side
/// Check this number to make sure query_server is in sync
async fn get_commits(
    data: web::Data<Arc<RwLock<QueryServer>>>,
) -> actix_web::Result<web::Json<u64>> {
    let query_server = data.read();
    Ok(web::Json(query_server.get_commits()))
}

#[allow(missing_docs)]
#[derive(Debug, Deserialize)]
pub struct WalletQueryParams {
    address: String,
    page: usize,
    per_page: usize,
    order: OrderOption,
}

#[allow(missing_docs)]
#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
enum OrderOption {
    Desc,
    Asc,
}

#[allow(missing_docs)]
#[derive(Debug, Deserialize, Serialize)]
struct CoinbaseTxnBody {
    height: u64,
    data: MintEntry,
}

#[allow(missing_docs)]
#[derive(Debug, Deserialize, Serialize)]
struct CoinbaseOperInfo {
    total_count: u64,
    txs: Vec<CoinbaseTxnBody>,
}

/// paging Query delegators according to `WalletQueryParams`
async fn get_coinbase_oper_list(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    web::Query(info): web::Query<WalletQueryParams>,
) -> actix_web::Result<web::Json<CoinbaseOperInfo>> {
    // Convert from base64 representation
    let key: XfrPublicKey = globutils::wallet::public_key_from_base64(&info.address)
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?;

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

/// Returns the list of claim transations of a given ledger address
async fn get_claim_txns(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    web::Query(info): web::Query<WalletQueryParams>,
) -> actix_web::Result<web::Json<Vec<Option<Transaction>>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = globutils::wallet::public_key_from_base64(&info.address)
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?;

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

/// Returns the list of transations associated with a given ledger address
async fn get_related_txns(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxnSID>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::zei_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?,
    )
    .c(d!())
    .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))?;
    let query_server = data.read();
    let records = query_server.get_related_transactions(&XfrAddress { key });
    Ok(web::Json(records.unwrap_or_default()))
}

/// Returns the list of transfer transations associated with a given asset
async fn get_related_xfrs(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxnSID>>> {
    let query_server = data.read();
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&*info) {
        if let Some(records) = query_server.get_related_transfers(&token_code) {
            Ok(web::Json(records))
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

/// Structures exposed to the outside world
pub struct QueryApi;

impl QueryApi {
    /// create query api
    pub fn create(
        query_server: Arc<RwLock<QueryServer>>,
        host: &str,
        port: u16,
    ) -> Result<QueryApi> {
        let _ = actix_rt::System::new("findora API");

        HttpServer::new(move || {
            App::new()
                .wrap(middleware::Logger::default())
                .wrap(Cors::permissive().supports_credentials())
                .data(Arc::clone(&query_server))
                .route(
                    &QueryServerRoutes::GetAddress.with_arg_template("txo_sid"),
                    web::get().to(get_address),
                )
                .route(
                    &QueryServerRoutes::GetOwnedUtxos.with_arg_template("address"),
                    web::get().to(get_owned_utxos),
                )
                .route(
                    &QueryServerRoutes::GetOwnerMemo.with_arg_template("txo_sid"),
                    web::get().to(get_owner_memo),
                )
                .route(
                    &QueryServerRoutes::GetOwnerMemoBatch
                        .with_arg_template("txo_sid_list"),
                    web::get().to(get_owner_memo_batch),
                )
                .route(
                    &QueryServerRoutes::GetRelatedTxns.with_arg_template("address"),
                    web::get().to(get_related_txns),
                )
                .service(
                    web::resource("claim_history").route(web::get().to(get_claim_txns)),
                )
                .service(
                    web::resource("coinbase_history")
                        .route(web::get().to(get_coinbase_oper_list)),
                )
                .route(
                    &QueryServerRoutes::GetRelatedXfrs.with_arg_template("asset_token"),
                    web::get().to(get_related_xfrs),
                )
                .route(
                    &QueryServerRoutes::GetCreatedAssets.with_arg_template("address"),
                    web::get().to(get_created_assets),
                )
                .route(
                    &QueryServerRoutes::GetTracedAssets.with_arg_template("address"),
                    web::get().to(get_traced_assets),
                )
                .route(
                    &QueryServerRoutes::GetIssuedRecords.with_arg_template("address"),
                    web::get().to(get_issued_records),
                )
                .route(
                    &QueryServerRoutes::GetIssuedRecordsByCode
                        .with_arg_template("asset_token"),
                    web::get().to(get_issued_records_by_code),
                )
                .route(
                    &QueryServerRoutes::GetAuthencatedTxnIDHash
                        .with_arg_template("txo_sid"),
                    web::get().to(get_authenticated_txnid_hash),
                )
                .route(
                    &QueryServerRoutes::GetTransactionHash.with_arg_template("txn_sid"),
                    web::get().to(get_transaction_hash),
                )
                .route(
                    &QueryServerRoutes::GetTransactionSid.with_arg_template("txn_hash"),
                    web::get().to(get_transaction_sid),
                )
                .route(
                    &QueryServerRoutes::GetCommits.route(),
                    web::get().to(get_commits),
                )
                .route(&QueryServerRoutes::Version.route(), web::get().to(version))
        })
        .bind(&format!("{}:{}", host, port))
        .c(d!())?
        .run();

        info!("Query server started");

        Ok(QueryApi)
    }
}

#[allow(missing_docs)]
pub trait RestfulQueryServerAccess {
    fn get_owner_memo(&self, txo_sid: u64) -> Result<Option<OwnerMemo>>;
}

/// Unimplemented until I can figure out a way to force the mock server to get new data (we can do
/// this with a new endpoint)
pub struct MockQueryServerClient();

#[allow(missing_docs)]
impl RestfulQueryServerAccess for MockQueryServerClient {
    fn get_owner_memo(&self, _txo_sid: u64) -> Result<Option<OwnerMemo>> {
        unimplemented!();
    }
}

#[allow(missing_docs)]
pub struct ActixQueryServerClient {
    port: usize,
    host: String,
    protocol: String,
}

#[allow(missing_docs)]
impl ActixQueryServerClient {
    pub fn new(port: usize, host: &str, protocol: &str) -> Self {
        ActixQueryServerClient {
            port,
            host: String::from(host),
            protocol: String::from(protocol),
        }
    }
}

#[allow(missing_docs)]
impl RestfulQueryServerAccess for ActixQueryServerClient {
    /// query memo based on `RestfulQueryServerAccess` implementation
    fn get_owner_memo(&self, txo_sid: u64) -> Result<Option<OwnerMemo>> {
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            QueryServerRoutes::GetOwnerMemo.with_arg(&txo_sid)
        );
        let text = http_get_request(&query).c(d!())?;
        serde_json::from_str::<Option<OwnerMemo>>(&text).c(d!())
    }
}
