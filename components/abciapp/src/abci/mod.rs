#![deny(warnings)]
#![allow(clippy::needless_borrow)]

use ledger_api_service::RestfulApiService;
use ruc::*;
use std::env;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use std::thread;
use submission_api::SubmissionApi;
use tendermint_sys::Node;

mod config;
mod init;
mod server;
pub mod staking;

use config::{global_cfg::CFG, ABCIConfig};

pub fn node_command() -> Result<()> {
    let base_dir = if let Some(d) = CFG.ledger_dir.as_ref() {
        fs::create_dir_all(d).c(d!())?;
        Some(Path::new(d))
    } else {
        None
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

    // handle SIGINT signal
    ctrlc::set_handler(move || {
        std::process::exit(0);
    })
    .c(d!())?;

    let config_path = if let Some(path) = CFG.tendermint_config {
        String::from(path)
    } else {
        env::var("HOME").unwrap() + "/.tendermint/config/config.toml"
    };

    let node = Node::new(&config_path, app).unwrap();
    node.start().unwrap();
    std::thread::park();
    Ok(())
}

fn init_command() -> Result<()> {
    let home_path = if let Some(home_path) = CFG.tendermint_home {
        String::from(home_path)
    } else {
        env::var("HOME").unwrap() + "/.tendermint"
    };
    tendermint_sys::init_home(&home_path).unwrap();
    init::init_genesis(
        CFG.init_mode.unwrap_or(init::InitMode::Dev),
        &(home_path.clone() + "/config/genesis.json"),
    )?;
    init::generate_tendermint_config(
        CFG.init_mode.unwrap_or(init::InitMode::Dev),
        &(home_path + "/config/config.toml"),
    )?;
    Ok(())
}

pub fn run() -> Result<()> {
    match CFG.command {
        "init" => init_command(),
        "node" => node_command(),
        _ => Err(eg!("Must use command is node or init")),
    }
}
