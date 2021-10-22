//!
//! Business Logics of Findora Network
//!

#![deny(warnings)]
#![allow(clippy::needless_borrow)]

mod config;
mod server;
pub mod staking;

use {
    crate::api::{
        query_server::query_api, submission_server::submission_api::SubmissionApi,
    },
    config::{global_cfg::CFG, ABCIConfig},
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

    let app = server::ABCISubmissionServer::new(
        basedir,
        format!("{}:{}", config.tendermint_host, config.tendermint_port),
    )?;

    let submission_service_hdr = Arc::clone(&app.la);

    if CFG.enable_query_service {
        env::set_var("FINDORAD_KEEP_HIST", "1");

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

    let addr_str = format!("{}:{}", config.abci_host, config.abci_port);
    let addr = addr_str.parse::<SocketAddr>().c(d!())?;

    abci::run(addr, app);

    Ok(())
}
