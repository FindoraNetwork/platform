#![deny(warnings)]

use log::debug;
use network::{HttpStandaloneConfig, LedgerStandalone};
use ruc::*;
use txn_cli::txn_app::{get_cli_app, process_inputs};
use txn_cli::txn_lib::init_logging;

fn main() {
    init_logging();
    let app = get_cli_app();
    let inputs = app.get_matches();
    let local = inputs.is_present("local");
    let config = {
        if local {
            debug!("Using standalone ledger at localhost");
            HttpStandaloneConfig::local()
        } else {
            debug!("Using testnet ledger");
            HttpStandaloneConfig::testnet()
        }
    };

    let mut rest_client = LedgerStandalone::new_http(&config);

    pnk!(process_inputs(inputs, &mut rest_client));
}
