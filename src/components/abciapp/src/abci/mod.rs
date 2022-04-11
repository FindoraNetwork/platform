//!
//! Business Logics of Findora Network
//!

#![deny(warnings)]
#![allow(clippy::needless_borrow)]

mod server;
pub mod staking;

pub use server::PROFILER_ENABLED;

use {
    crate::api::{
        query_server::query_api, submission_server::submission_api::SubmissionApi,
    },
    config::abci::{global_cfg::CFG, ABCIConfig},
    futures::executor::ThreadPool,
    lazy_static::lazy_static,
    ruc::*,
    std::{
        env, fs,
        net::SocketAddr,
        sync::{atomic::AtomicBool, Arc},
        thread,
    },
};

lazy_static! {
    /// if `true`,
    /// we can exit safely without the risk of breaking data
    pub static ref IN_SAFE_ITV: AtomicBool = AtomicBool::new(true);
    /// A shared pool of the ABCI area
    pub static ref POOL: ThreadPool = pnk!(ThreadPool::new());
    /// if is exiting, we should not do anything.
    pub static ref IS_EXITING: AtomicBool = AtomicBool::new(false);
}

/// Starting findorad
pub fn run() -> Result<()> {
    let basedir = {
        fs::create_dir_all(&CFG.ledger_dir).c(d!())?;
        Some(CFG.ledger_dir.as_str())
    };

    let config = ruc::info!(ABCIConfig::from_file())
        .or_else(|_| ABCIConfig::from_env().c(d!()))?;

    env::set_var("BNC_DATA_DIR", format!("{}/__bnc__", &config.ledger_dir));

    if CFG.enable_query_service {
        env::set_var("FINDORAD_KEEP_HIST", "1");
    }

    let app = server::ABCISubmissionServer::new(
        basedir,
        format!("{}:{}", config.tendermint_host, config.tendermint_port),
    )?;

    let submission_service_hdr = Arc::clone(&app.la);

    if CFG.enable_query_service {
        let query_service_hdr = submission_service_hdr.read().borrowable_ledger_state();
        pnk!(query_api::service::start_query_server(
            Arc::clone(&query_service_hdr),
            &[
                (&config.abci_host, config.query_port),
                (&config.abci_host, config.ledger_port)
            ],
        ))
        .write()
        .update();

        let submission_host = config.abci_host.clone();
        let submission_port = config.submission_port;
        thread::spawn(move || {
            pnk!(SubmissionApi::create(
                submission_service_hdr,
                &submission_host,
                submission_port,
            ));
        });
    }

    let mut web3_rpc: Box<dyn std::any::Any + Send> = Box::new(());
    if CFG.enable_eth_api_service {
        let base_app = app.account_base_app.clone();
        let evm_http = format!("{}:{}", config.abci_host, config.evm_http_port);
        let evm_ws = format!("{}:{}", config.abci_host, config.evm_ws_port);
        let tendermint_rpc = format!(
            "http://{}:{}",
            config.tendermint_host, config.tendermint_port
        );
        web3_rpc =
            fc_rpc::start_web3_service(evm_http, evm_ws, tendermint_rpc, base_app);
    }

    let addr_str = format!("{}:{}", config.abci_host, config.abci_port);
    let addr = addr_str.parse::<SocketAddr>().c(d!())?;

    abci::run(addr, app);

    drop(web3_rpc);

    Ok(())
}
