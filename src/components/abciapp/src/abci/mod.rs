//!
//! Business Logics of Findora Network
//!

#![deny(warnings)]
#![allow(clippy::needless_borrow)]

mod config;
mod server;
pub mod staking;

use crate::api::{
    query_server::{ledger_api::RestfulApiService, query_api},
    submission_server::submission_api::SubmissionApi,
};
use lazy_static::lazy_static;
use ruc::*;
use std::{
    fs,
    net::SocketAddr,
    path::Path,
    sync::{atomic::AtomicBool, Arc},
    thread,
};

use config::{global_cfg::CFG, ABCIConfig};

lazy_static! {
    /// if `true`,
    /// we can exit safely without the risk of breaking data
    pub static ref IN_SAFE_ITV: AtomicBool = AtomicBool::new(true);
}

/// Starting findorad
pub fn run() -> Result<()> {
    let base_dir = {
        fs::create_dir_all(&CFG.ledger_dir).c(d!())?;
        Some(Path::new(&CFG.ledger_dir))
    };

    let config = ruc::info!(ABCIConfig::from_file())
        .or_else(|_| ABCIConfig::from_env().c(d!()))?;

    let app = server::ABCISubmissionServer::new(
        base_dir,
        format!("{}:{}", config.tendermint_host, config.tendermint_port),
    )?;

    let submission_service_hdr = Arc::clone(&app.la);

    if CFG.enable_ledger_service {
        let ledger_api_service_hdr =
            submission_service_hdr.read().borrowable_ledger_state();
        let ledger_host = config.abci_host.clone();
        let ledger_port = config.ledger_port;
        thread::spawn(move || {
            pnk!(RestfulApiService::create(
                ledger_api_service_hdr,
                &ledger_host,
                ledger_port
            ));
        });
    }

    if CFG.enable_query_service {
        let query_service_hdr = submission_service_hdr.read().borrowable_ledger_state();
        pnk!(query_api::service::start_query_server(
            query_service_hdr,
            &config.abci_host,
            config.query_port,
            Some(Path::new(&config.ledger_dir)),
        ))
        .write()
        .update();
    }

    let submission_host = config.abci_host.clone();
    let submission_port = config.submission_port;
    thread::spawn(move || {
        pnk!(SubmissionApi::create(
            submission_service_hdr,
            &submission_host,
            submission_port,
        ));
    });

    let addr_str = format!("{}:{}", config.abci_host, config.abci_port);
    let addr = addr_str.parse::<SocketAddr>().c(d!())?;

    abci::run(addr, app);
    Ok(())
}
