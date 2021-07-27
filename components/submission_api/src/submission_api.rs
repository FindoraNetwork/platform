#![deny(warnings)]

use actix_cors::Cors;
use actix_web::{error, middleware, web, App, HttpServer};
use ledger::data_model::Transaction;
use ledger::store::{LedgerAccess, LedgerUpdate};
use log::info;
use parking_lot::RwLock;
use rand_core::{CryptoRng, RngCore};
use ruc::*;
use std::marker::{Send, Sync};
use std::result::Result as StdResult;
use std::sync::Arc;
use submission_server::{SubmissionServer, TxnForward, TxnHandle};
use utils::NetworkRoute;

// Ping route to check for liveness of API
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

pub async fn submit_transaction<RNG, LU, TF>(
    data: web::Data<Arc<RwLock<SubmissionServer<RNG, LU, TF>>>>,
    body: web::Json<Transaction>,
) -> StdResult<web::Json<TxnHandle>, actix_web::error::Error>
where
    RNG: RngCore + CryptoRng,
    LU: LedgerUpdate<RNG> + LedgerAccess + Sync + Send,
    TF: TxnForward + Sync + Send,
{
    let tx = body.into_inner();

    if tx.in_blk_list() {
        return Err(error::ErrorBadRequest(""));
    }

    let mut submission_server = data.write();
    submission_server
        .handle_transaction(tx)
        .map(web::Json)
        .map_err(|e| {
            e.print();
            error::ErrorBadRequest(e.generate_log())
        })
}

// Queries the status of a transaction by its handle. Returns either a not committed message or a
// serialized TxnStatus.
pub async fn txn_status<RNG, LU, TF>(
    data: web::Data<Arc<RwLock<SubmissionServer<RNG, LU, TF>>>>,
    info: web::Path<String>,
) -> StdResult<String, actix_web::error::Error>
where
    RNG: RngCore + CryptoRng,
    LU: LedgerUpdate<RNG> + LedgerAccess + Sync + Send,
    TF: TxnForward + Sync + Send,
{
    let submission_server = data.write();
    let txn_status = submission_server.get_txn_status(&TxnHandle(info.clone()));
    let res;
    if let Some(status) = txn_status {
        res = serde_json::to_string(&status)?;
    } else {
        res = format!(
            "No transaction with handle {} found. Please retry with a new handle.",
            &info
        );
    }

    Ok(res)
}

pub struct SubmissionApi {
    web_runtime: actix_rt::SystemRunner,
}

pub enum SubmissionRoutes {
    SubmitTransaction,
    TxnStatus,
    Ping,
    ForceEndBlock,
    Version,
}

impl NetworkRoute for SubmissionRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            SubmissionRoutes::SubmitTransaction => "submit_transaction",
            SubmissionRoutes::TxnStatus => "txn_status",
            SubmissionRoutes::Ping => "ping",
            SubmissionRoutes::ForceEndBlock => "force_end_block",
            SubmissionRoutes::Version => "version",
        };
        "/".to_owned() + endpoint
    }
}

impl SubmissionApi {
    pub fn create<
        RNG: 'static + RngCore + CryptoRng + Sync + Send,
        LU: 'static + LedgerUpdate<RNG> + LedgerAccess + Sync + Send,
        TF: 'static + TxnForward + Sync + Send,
    >(
        submission_server: Arc<RwLock<SubmissionServer<RNG, LU, TF>>>,
        host: &str,
        port: u16,
    ) -> Result<SubmissionApi> {
        let web_runtime = actix_rt::System::new("findora API");

        HttpServer::new(move || {
            App::new()
                .wrap(middleware::Logger::default())
                .wrap(Cors::permissive().supports_credentials())
                .data(web::JsonConfig::default().limit(1024 * 512))
                .data(submission_server.clone())
                .route(
                    &SubmissionRoutes::SubmitTransaction.route(),
                    web::post().to(submit_transaction::<RNG, LU, TF>),
                )
                .route(&SubmissionRoutes::Ping.route(), web::get().to(ping))
                .route(&SubmissionRoutes::Version.route(), web::get().to(version))
                .route(
                    &SubmissionRoutes::TxnStatus.with_arg_template("handle"),
                    web::get().to(txn_status::<RNG, LU, TF>),
                )
            // .route(
            //     &SubmissionRoutes::ForceEndBlock.route(),
            //     web::post().to(force_end_block::<RNG, LU, TF>),
            // )
        })
        .bind(&format!("{}:{}", host, port))
        .c(d!())?
        .run();

        info!("Submission server started");

        Ok(SubmissionApi { web_runtime })
    }

    // call from a thread; this will block.
    pub fn run(self) -> Result<()> {
        self.web_runtime.run().c(d!())
    }
}
