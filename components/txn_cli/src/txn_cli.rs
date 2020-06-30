use ledger::data_model::NoReplayToken;
use log::debug;
use network::{HttpStandaloneConfig, LedgerStandalone};
use txn_cli::txn_app::{get_cli_app, process_inputs};
use txn_cli::txn_lib::{init_logging, match_error_and_exit};

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
  if let Err(error) = process_inputs(inputs, NoReplayToken::default(), &mut rest_client) {
    match_error_and_exit(error);
  }
}
