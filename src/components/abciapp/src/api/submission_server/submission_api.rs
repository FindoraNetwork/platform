//!
//! # interface of operating tx
//!

use {
    super::{SubmissionServer, TxnForward, TxnHandle},
    actix_cors::Cors,
    actix_web::{error, middleware, web, App, HttpServer},
    finutils::api::NetworkRoute,
    ledger::data_model::Transaction,
    parking_lot::RwLock,
    rand_core::{CryptoRng, RngCore},
    ruc::*,
    std::result::Result as StdResult,
    std::sync::Arc,
    tracing::info,
};

/// Ping route to check for liveness of API
#[allow(clippy::unnecessary_wraps)]
async fn ping() -> actix_web::Result<String> {
    Ok("success".into())
}

/// Returns the git commit hash and commit date of this build
#[allow(clippy::unnecessary_wraps)]
async fn version() -> actix_web::Result<String> {
    Ok(format!(
        "Build: {} {}",
        option_env!("VERGEN_SHA_EXTERN").unwrap_or(env!("VERGEN_SHA")),
        env!("VERGEN_BUILD_DATE")
    ))
}

/// Sending transactions to tendermint
pub async fn submit_transaction<RNG, TF>(
    data: web::Data<Arc<RwLock<SubmissionServer<RNG, TF>>>>,
    body: web::Json<Transaction>,
) -> StdResult<web::Json<TxnHandle>, actix_web::error::Error>
where
    RNG: RngCore + CryptoRng,
    TF: TxnForward + Sync + Send,
{
    let tx = body.into_inner();

    let mut submission_server = data.write();
    submission_server
        .handle_transaction(tx)
        .map(web::Json)
        .map_err(|e| {
            e.print(None);
            error::ErrorBadRequest(e.to_string())
        })
}

/// Queries the status of a transaction by its handle. Returns either a not committed message or a
/// serialized TxnStatus.
pub async fn txn_status<RNG, TF>(
    data: web::Data<Arc<RwLock<SubmissionServer<RNG, TF>>>>,
    info: web::Path<String>,
) -> StdResult<String, actix_web::error::Error>
where
    RNG: RngCore + CryptoRng,
    TF: TxnForward + Sync + Send,
{
    let submission_server = data.write();
    let txn_status = submission_server.get_txn_status(&TxnHandle(info.clone()));
    let res = if let Some(status) = txn_status {
        serde_json::to_string(&status)?
    } else {
        format!(
            "No transaction with handle {} found. Please retry with a new handle.",
            &info
        )
    };

    Ok(res)
}

/// Structures exposed to the outside world
pub struct SubmissionApi;

/// Define interface
#[allow(missing_docs)]
pub enum SubmissionRoutes {
    SubmitTransaction,
    TxnStatus,
    Ping,
    Version,
}

impl NetworkRoute for SubmissionRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            SubmissionRoutes::SubmitTransaction => "submit_transaction",
            SubmissionRoutes::TxnStatus => "txn_status",
            SubmissionRoutes::Ping => "ping",
            SubmissionRoutes::Version => "version",
        };
        "/".to_owned() + endpoint
    }
}

impl SubmissionApi {
    /// Create submission api
    pub fn create<
        RNG: 'static + RngCore + CryptoRng + Sync + Send,
        TF: 'static + TxnForward + Sync + Send,
    >(
        submission_server: Arc<RwLock<SubmissionServer<RNG, TF>>>,
        host: &str,
        port: u16,
    ) -> Result<SubmissionApi> {
        let _ = actix_rt::System::new("findora API");

        HttpServer::new(move || {
            App::new()
                .wrap(middleware::Logger::default())
                .wrap(Cors::permissive().supports_credentials())
                .data(web::JsonConfig::default().limit(2048 * 1024))
                .data(submission_server.clone())
                .route(
                    &SubmissionRoutes::SubmitTransaction.route(),
                    web::post().to(submit_transaction::<RNG, TF>),
                )
                .route(&SubmissionRoutes::Ping.route(), web::get().to(ping))
                .route(&SubmissionRoutes::Version.route(), web::get().to(version))
                .route(
                    &SubmissionRoutes::TxnStatus.with_arg_template("handle"),
                    web::get().to(txn_status::<RNG, TF>),
                )
        })
        .bind(&format!("{host}:{port}"))
        .c(d!())?
        .run();

        info!("Submission server started");

        Ok(SubmissionApi)
    }
}
