//!
//! need to transform the data in ledgerState to store
//!

// pub it for doc
pub mod ledger_api;

pub mod server;
pub mod service;

use {
    actix_cors::Cors,
    actix_web::{error, middleware, web, App, HttpServer},
    config::abci::{global_cfg::CFG, CheckPointConfig},
    finutils::api::NetworkRoute,
    globutils::wallet,
    ledger::{
        data_model::{
            b64dec, ATxoSID, AssetTypeCode, DefineAsset, IssuerPublicKey, Transaction,
            TxOutput, TxnIDHash, TxnSID, TxoSID, XfrAddress, BLACK_HOLE_PUBKEY,
        },
        staking::{
            ops::mint_fra::MintEntry, FF_PK_EXTRA_120_0000, FRA, FRA_TOTAL_AMOUNT,
        },
    },
    ledger_api::*,
    noah::{
        anon_xfr::structs::{AxfrOwnerMemo, Commitment, MTLeafInfo},
        xfr::{sig::XfrPublicKey, structs::OwnerMemo},
    },
    noah_algebra::serialization::NoahFromToBytes,
    parking_lot::RwLock,
    ruc::*,
    serde::{Deserialize, Serialize},
    server::QueryServer,
    std::{
        collections::{BTreeMap, HashMap, HashSet},
        sync::Arc,
    },
    tracing::info,
};

/// Returns the git commit hash and commit date of this build
#[allow(clippy::unnecessary_wraps)]
pub async fn version() -> actix_web::Result<String> {
    Ok(format!(
        "Build: {} {}",
        option_env!("VERGEN_SHA_EXTERN").unwrap_or(env!("VERGEN_SHA")),
        env!("VERGEN_BUILD_DATE")
    ))
}

/// Queries the status of a transaction by its handle. Returns either a not committed message or a
/// serialized TxnStatus.
pub async fn get_address(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<String, actix_web::error::Error> {
    let server = data.read();
    let address_res = server.get_address_of_sid(TxoSID(*info));
    let res = if let Some(address) = address_res {
        serde_json::to_string(&address)?
    } else {
        format!("No utxo {} found. Please retry with a new utxo.", &info)
    };
    Ok(res)
}

/// Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
#[allow(clippy::unnecessary_wraps)]
pub async fn get_owner_memo(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<Option<OwnerMemo>>, actix_web::error::Error> {
    let server = data.read();
    Ok(web::Json(server.get_owner_memo(TxoSID(*info))))
}

/// Separate a string of `TxoSID` by ',' and query the corresponding memo
#[allow(clippy::unnecessary_wraps)]
pub async fn get_owner_memo_batch(
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

/// Returns the owner memo required to decrypt the asset record stored at given index, if it exists.
#[allow(clippy::unnecessary_wraps)]
async fn get_abar_memo(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<Option<AxfrOwnerMemo>>, actix_web::error::Error> {
    let server = data.read();
    Ok(web::Json(server.get_abar_memo(ATxoSID(*info))))
}

/// Returns the owner memos required to decrypt the asset record stored at between start and end,
/// include start and end, limit 100.
async fn get_abar_memos(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    query: web::Query<HashMap<String, u64>>,
) -> actix_web::Result<web::Json<Vec<(u64, AxfrOwnerMemo)>>, actix_web::error::Error> {
    match (query.get("start"), query.get("end")) {
        (Some(start), Some(end)) => {
            if end < start || end - start > 100 {
                // return limit 100 error.
                return Err(actix_web::error::ErrorBadRequest("Limit 100"));
            }
            let server = data.read();
            Ok(web::Json(server.get_abar_memos(*start, *end)))
        }
        _ => Err(actix_web::error::ErrorBadRequest("Missing start and end")),
    }
}

/// Return the abar commitment by sid.
async fn get_abar_commitment(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<Option<Commitment>>, actix_web::error::Error> {
    let server = data.read();
    Ok(web::Json(server.get_abar_commitment(ATxoSID(*info))))
}

/// Returns an array of the utxo sids currently spendable by a given address
pub async fn get_owned_utxos(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    owner: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxoSID>>> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;

    let pk = wallet::public_key_from_base64(owner.as_str())
        .map_err(actix_web::error::ErrorServiceUnavailable)?;

    let utxos = ledger
        .get_owned_utxos(&pk)
        .map_err(actix_web::error::ErrorServiceUnavailable)?
        .keys()
        .copied()
        .collect();

    Ok(web::Json(utxos))
}

/// Returns the ATxo Sid currently spendable by a given commitment
async fn get_owned_abar(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    com: web::Path<String>,
) -> actix_web::Result<web::Json<Option<ATxoSID>>> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    //let read = qs.state.as_ref().unwrap().read();
    globutils::wallet::commitment_from_base58(com.as_str())
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.generate_log(None)))
        .map(|com| web::Json(ledger.get_owned_abar(&com)))
}
/// Returns the merkle proof for anonymous transactions
async fn get_abar_proof(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<Option<MTLeafInfo>>, actix_web::error::Error> {
    let server = data.read();
    Ok(web::Json(server.get_abar_proof(ATxoSID(*info))))
}

/// Checks if a nullifier hash is present in nullifier set
async fn check_nullifier_hash(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Option<bool>>, actix_web::error::Error> {
    let server = data.read();
    Ok(web::Json(server.check_nullifier_hash((*info).clone())))
}

async fn get_max_atxo_sid(
    data: web::Data<Arc<RwLock<QueryServer>>>,
) -> actix_web::Result<web::Json<Option<usize>>, actix_web::error::Error> {
    let server = data.read();
    Ok(web::Json(server.max_atxo_sid()))
}

async fn get_max_atxo_sid_at_height(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<Option<usize>>, actix_web::error::Error> {
    let server = data.read();
    Ok(web::Json(server.max_atxo_sid_at_height(*info)))
}

/// Define interface type
#[allow(missing_docs)]
pub enum QueryServerRoutes {
    GetAddress,
    GetOwnerMemo,
    GetOwnerMemoBatch,
    GetOwnedUtxos,
    GetOwnedAbars,
    GetAbarCommitment,
    GetAbarMemo,
    GetAbarMemos,
    GetAbarProof,
    CheckNullifierHash,
    GetMaxATxoSid,
    GetMaxATxoSidAtHeight,
    GetCreatedAssets,
    GetIssuedRecords,
    GetIssuedRecordsByCode,
    GetRelatedTxns,
    GetRelatedXfrs,
    GetAuthencatedTxnIDHash,
    GetTransactionHash,
    GetTransactionSid,
    GetCommits,
}

impl NetworkRoute for QueryServerRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            QueryServerRoutes::GetAddress => "get_address",
            QueryServerRoutes::GetRelatedTxns => "get_related_txns",
            QueryServerRoutes::GetRelatedXfrs => "get_related_xfrs",
            QueryServerRoutes::GetOwnedUtxos => "get_owned_utxos",
            QueryServerRoutes::GetOwnedAbars => "get_owned_abar",
            QueryServerRoutes::GetOwnerMemo => "get_owner_memo",
            QueryServerRoutes::GetOwnerMemoBatch => "get_owner_memo_batch",
            QueryServerRoutes::GetAbarCommitment => "get_abar_commitment",
            QueryServerRoutes::GetAbarMemo => "get_abar_memo",
            QueryServerRoutes::GetAbarMemos => "get_abar_memos",
            QueryServerRoutes::GetAbarProof => "get_abar_proof",
            QueryServerRoutes::CheckNullifierHash => "check_nullifier_hash",
            QueryServerRoutes::GetMaxATxoSid => "get_max_atxo_sid",
            QueryServerRoutes::GetMaxATxoSidAtHeight => "get_max_atxo_sid_at_height",
            QueryServerRoutes::GetCreatedAssets => "get_created_assets",
            QueryServerRoutes::GetIssuedRecords => "get_issued_records",
            QueryServerRoutes::GetIssuedRecordsByCode => "get_issued_records_by_code",
            QueryServerRoutes::GetAuthencatedTxnIDHash => "get_authencated_txnid_hash",
            QueryServerRoutes::GetTransactionHash => "get_transaction_hash",
            QueryServerRoutes::GetTransactionSid => "get_transaction_sid",
            QueryServerRoutes::GetCommits => "get_commits",
        };
        "/".to_owned() + endpoint
    }
}

/// Returns the list of assets created by a public key
pub async fn get_created_assets(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<DefineAsset>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::noah_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.to_string()))?,
    )
    .map_err(|e| error::ErrorBadRequest(e.to_string()))?;
    let server = data.read();
    let mut assets_tuple = server
        .get_created_assets(&IssuerPublicKey { key })
        .unwrap_or_default();

    let mut das = vec![];
    for (code, da) in assets_tuple.iter_mut() {
        da.body.asset.code = *code;
        das.push(da.clone());
    }

    Ok(web::Json(das))
}

/// Returns the list of records issued by a public key
#[allow(clippy::type_complexity)]
pub async fn get_issued_records(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<(TxOutput, Option<OwnerMemo>)>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::noah_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.to_string()))?,
    )
    .map_err(|e| error::ErrorBadRequest(e.to_string()))?;
    let server = data.read();
    let records = server.get_issued_records(&IssuerPublicKey { key });
    Ok(web::Json(records.unwrap_or_default()))
}

/// Returns the list of records issued by a token code
#[allow(clippy::type_complexity)]
pub async fn get_issued_records_by_code(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<(TxOutput, Option<OwnerMemo>)>>> {
    let server = data.read();

    match AssetTypeCode::new_from_base64(&info).c(d!()) {
        Ok(token_code) => {
            if let Some(records) = server.get_issued_records_by_code(&token_code) {
                Ok(web::Json(records))
            } else {
                Err(actix_web::error::ErrorNotFound(
                    "Specified asset definition does not currently exist.",
                ))
            }
        }
        Err(e) => Err(actix_web::error::ErrorBadRequest(e.to_string())),
    }
}

/// Returns authenticated txn sid and hash
pub async fn get_authenticated_txnid_hash(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<u64>,
) -> actix_web::Result<web::Json<TxnIDHash>> {
    let server = data.read();
    match server.get_authenticated_txnid(TxoSID(*info)) {
        Some(txnid) => Ok(web::Json(txnid)),
        None => Err(actix_web::error::ErrorNotFound(
            "No authenticated transaction found. Please retry with correct sid.",
        )),
    }
}

/// Returns txn hash by sid
pub async fn get_transaction_hash(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<usize>,
) -> actix_web::Result<web::Json<String>> {
    let server = data.read();
    match server.get_transaction_hash(TxnSID(*info)) {
        Some(hash) => Ok(web::Json(hash)),
        None => Err(actix_web::error::ErrorNotFound(
            "No transaction found. Please retry with correct sid.",
        )),
    }
}

/// Returns txn sid by hash
pub async fn get_transaction_sid(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<usize>> {
    let server = data.read();
    match server.get_transaction_sid((*info).clone()) {
        Some(sid) => Ok(web::Json(sid.0)),
        None => Err(actix_web::error::ErrorNotFound(
            "No transaction found. Please retry with correct hash.",
        )),
    }
}

/// Returns most recent commit count at server side
/// Check this number to make sure server is in sync
pub async fn get_commits(
    data: web::Data<Arc<RwLock<QueryServer>>>,
) -> actix_web::Result<web::Json<u64>> {
    let server = data.read();
    Ok(web::Json(server.get_commits()))
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
pub struct CoinbaseOperInfo {
    total_count: u64,
    txs: Vec<CoinbaseTxnBody>,
}

/// paging Query delegators according to `WalletQueryParams`
pub async fn get_coinbase_oper_list(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    web::Query(info): web::Query<WalletQueryParams>,
) -> actix_web::Result<web::Json<CoinbaseOperInfo>> {
    // Convert from base64 representation
    let key: XfrPublicKey = wallet::public_key_from_base64(&info.address)
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.to_string()))?;

    let server = data.read();

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

    let resp = server
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
pub async fn get_claim_txns(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    web::Query(info): web::Query<WalletQueryParams>,
) -> actix_web::Result<web::Json<Vec<Option<Transaction>>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = wallet::public_key_from_base64(&info.address)
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.to_string()))?;

    let server = data.read();

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

    let records = server
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
pub async fn get_related_txns(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxnSID>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = XfrPublicKey::noah_from_bytes(
        &b64dec(&*info)
            .c(d!())
            .map_err(|e| error::ErrorBadRequest(e.to_string()))?,
    )
    .c(d!())
    .map_err(|e| error::ErrorBadRequest(e.to_string()))?;
    let server = data.read();
    let records = server.get_related_transactions(&XfrAddress { key });
    Ok(web::Json(records.unwrap_or_default()))
}

/// Returns the list of transfer transations associated with a given asset
pub async fn get_related_xfrs(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<HashSet<TxnSID>>> {
    let server = data.read();
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&info) {
        if let Some(records) = server.get_related_transfers(&token_code) {
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

#[allow(missing_docs)]
#[allow(clippy::unnecessary_wraps)]

pub async fn get_circulating_supply(
    data: web::Data<Arc<RwLock<QueryServer>>>,
) -> actix_web::Result<web::Json<BTreeMap<&'static str, f64>>, actix_web::error::Error> {
    let l = data.read();
    let fra = FRA as f64;

    let cs = l.ledger_cloned.staking_get_global_unlocked_amount() as f64 / fra;
    let gd = l.ledger_cloned.get_staking().get_global_delegation_amount() as f64 / fra;
    let rr = l.ledger_cloned.staking_get_block_rewards_rate();
    let rr = rr[0] as f64 / rr[1] as f64;

    let res = map! { B
        "global_return_rate" => rr,
        "global_circulating_supply" => cs,
        "global_delegation_amount" => gd
    };

    Ok(web::Json(res))
}

/// return
/// global_circulating_supply
/// global_adjusted_circulating_supply
/// global_total_supply
pub async fn get_total_supply(
    data: web::Data<Arc<RwLock<QueryServer>>>,
) -> actix_web::Result<web::Json<BTreeMap<&'static str, f64>>, actix_web::error::Error> {
    let l = data.read();
    let burn_pubkey = *BLACK_HOLE_PUBKEY;
    let extra_pubkey = *FF_PK_EXTRA_120_0000;

    let burn_balance = l
        .ledger_cloned
        .get_nonconfidential_balance(&burn_pubkey)
        .unwrap_or(0);
    let extra_balance = l
        .ledger_cloned
        .get_nonconfidential_balance(&extra_pubkey)
        .unwrap_or(0);

    let fra = FRA as f64;

    let big_9 = l.ledger_cloned.staking_get_global_unlocked_amount();
    let big_8 = big_9 + extra_balance;

    let cs = big_8 as f64 / fra;
    let acs = big_9 as f64 / fra;
    let ts = (FRA_TOTAL_AMOUNT - burn_balance) as f64 / fra;

    let res = map! { B
        "global_circulating_supply" => cs,
        "global_adjusted_circulating_supply" => acs,
        "global_total_supply" => ts
    };

    Ok(web::Json(res))
}

#[allow(missing_docs)]
pub async fn get_checkpoint(
) -> actix_web::Result<web::Json<CheckPointConfig>, actix_web::error::Error> {
    let mut checkpoint = CFG.checkpoint.clone();

    if let Ok(sc) = module_evm::system_contracts::SystemContracts::new() {
        checkpoint.prism_bridge_address = format!("{:?}", sc.bridge_address);
        checkpoint.evm_staking_address = format!("{:?}", sc.staking_address);
    }

    Ok(web::Json(checkpoint))
}
/// Structures exposed to the outside world
pub struct QueryApi;

impl QueryApi {
    pub(crate) fn create(
        server: Arc<RwLock<QueryServer>>,
        addrs: &[(&str, u16)],
    ) -> Result<QueryApi> {
        let _ = actix_rt::System::new("findora API");

        let mut hdr = HttpServer::new(move || {
            App::new()
                .wrap(middleware::Logger::default())
                .wrap(Cors::permissive().supports_credentials())
                .data(Arc::clone(&server))
                .route("/ping", web::get().to(ping))
                .route("/version", web::get().to(version))
                .service(
                    web::resource("get_total_supply")
                        .route(web::get().to(get_total_supply)),
                )
                .service(
                    web::resource("circulating_supply")
                        .route(web::get().to(get_circulating_supply)),
                )
                .route(
                    &QueryServerRoutes::GetAddress.with_arg_template("txo_sid"),
                    web::get().to(get_address),
                )
                .route(
                    &QueryServerRoutes::GetOwnedUtxos.with_arg_template("address"),
                    web::get().to(get_owned_utxos),
                )
                .route(
                    &QueryServerRoutes::GetOwnedAbars.with_arg_template("commitment"),
                    web::get().to(get_owned_abar),
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
                    &QueryServerRoutes::GetAbarCommitment.with_arg_template("atxo_sid"),
                    web::get().to(get_abar_commitment),
                )
                .route(
                    &QueryServerRoutes::GetAbarMemo.with_arg_template("atxo_sid"),
                    web::get().to(get_abar_memo),
                )
                .route(
                    &QueryServerRoutes::GetAbarMemos.route(),
                    web::get().to(get_abar_memos),
                )
                .route(
                    &QueryServerRoutes::GetAbarProof.with_arg_template("atxo_sid"),
                    web::get().to(get_abar_proof),
                )
                .route(
                    &QueryServerRoutes::CheckNullifierHash
                        .with_arg_template("null_hash"),
                    web::get().to(check_nullifier_hash),
                )
                .route(
                    &QueryServerRoutes::GetMaxATxoSid.route(),
                    web::get().to(get_max_atxo_sid),
                )
                .route(
                    &QueryServerRoutes::GetMaxATxoSidAtHeight
                        .with_arg_template("height"),
                    web::get().to(get_max_atxo_sid_at_height),
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
                .route(
                    &ApiRoutes::UtxoSid.with_arg_template("sid"),
                    web::get().to(query_utxo),
                )
                .route(
                    &ApiRoutes::UtxoSidLight.with_arg_template("sid"),
                    web::get().to(query_utxo_light),
                )
                .route(
                    &ApiRoutes::UtxoSidList.with_arg_template("sid_list"),
                    web::get().to(query_utxos),
                )
                .route(
                    &ApiRoutes::AssetIssuanceNum.with_arg_template("code"),
                    web::get().to(query_asset_issuance_num),
                )
                .route(
                    &ApiRoutes::AssetToken.with_arg_template("code"),
                    web::get().to(query_asset),
                )
                .route(
                    &ApiRoutes::GlobalState.route(),
                    web::get().to(query_global_state),
                )
                .route(
                    &ApiRoutes::TxnSid.with_arg_template("sid"),
                    web::get().to(query_txn),
                )
                .route(
                    &ApiRoutes::TxnSidLight.with_arg_template("sid"),
                    web::get().to(query_txn_light),
                )
                .route(
                    &ApiRoutes::GlobalStateVersion.with_arg_template("version"),
                    web::get().to(query_global_state_version),
                )
                .route(
                    &ApiRoutes::OwnedUtxos.with_arg_template("owner"),
                    web::get().to(query_owned_utxos),
                )
                .route(
                    &ApiRoutes::OwnedAbars.with_arg_template("owner"),
                    web::get().to(query_owned_abar),
                )
                .route(
                    &ApiRoutes::ValidatorList.route(),
                    web::get().to(query_validators),
                )
                .route(
                    &ApiRoutes::DelegationInfo.with_arg_template("XfrPublicKey"),
                    web::get().to(query_delegation_info),
                )
                .route(
                    &ApiRoutes::DelegatorList.with_arg_template("NodeAddress"),
                    web::get().to(query_delegator_list),
                )
                .service(
                    web::resource("/delegator_list")
                        .route(web::get().to(get_delegators_with_params)),
                )
                .service(
                    web::resource("/delegation_rewards")
                        .route(web::get().to(get_delegation_reward)),
                )
                .service(
                    web::resource("/validator_delegation")
                        .route(web::get().to(get_validator_delegation_history)),
                )
                .route(
                    &ApiRoutes::ValidatorDetail.with_arg_template("NodeAddress"),
                    web::get().to(query_validator_detail),
                )
                .service(
                    web::resource("/display_checkpoint")
                        .route(web::get().to(get_checkpoint)),
                )
        });

        for (host, port) in addrs.iter() {
            hdr = hdr.bind(&format!("{host}:{port}")).c(d!())?
        }

        hdr.run();

        info!("Query server started");

        Ok(QueryApi)
    }
}
