//!
//! # abci init and startup methods
//!

mod config;
mod init;
mod server;
pub mod staking;

use crate::api::{
    query_server::{ledger_api::RestfulApiService, query_api},
    submission_server::submission_api::SubmissionApi,
};
use ruc::*;
use std::{env, fs, path::Path, sync::mpsc::channel, sync::Arc, thread};
use tendermint_sys::Node;

use config::{global_cfg::CFG, ABCIConfig};

/// run node
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
        let ledger_host = config.tendermint_host.clone();
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
            &config.tendermint_host,
            config.query_port,
            base_dir,
        ))
        .write()
        .update();
    }

    let submission_host = config.tendermint_host.clone();
    let submission_port = config.submission_port;

    thread::spawn(move || {
        pnk!(SubmissionApi::create(
            submission_service_hdr,
            &submission_host,
            submission_port,
        ));
    });

    let config_path = if let Some(path) = CFG.tendermint_config {
        String::from(path)
    } else {
        env::var("HOME").unwrap() + "/.tendermint/config/config.toml"
    };

    let node = Node::new(&config_path, app).unwrap();
    node.start().unwrap();

    std::thread::park();

    node.stop().unwrap();
    Ok(())
}

/// init abci
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

/// run abci
pub fn run() -> Result<()> {
    match CFG.command {
        "init" => init_command(),
        "node" => {
            let thread = thread::Builder::new()
                .spawn(|| node_command().unwrap())
                .unwrap();

            let (tx, rx) = channel();

            ctrlc::set_handler(move || {
                tx.send(()).expect("Could not send signal.");
            })
            .c(d!())?;

            rx.recv().expect("Could not receive signal.");

            thread.thread().unpark();

            thread.join().unwrap();

            Ok(())
        }
        _ => Err(eg!("Must use command is node or init")),
    }
}
