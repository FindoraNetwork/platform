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
      println!("LOCAL");
      HttpStandaloneConfig::local()
    } else {
      println!("TESTNET");
      HttpStandaloneConfig::testnet()
    }
  };

  let seq_id = 0;
  let mut rest_client = LedgerStandalone::new_http(&config);
  if let Err(error) = process_inputs(inputs, seq_id, &mut rest_client) {
    match_error_and_exit(error);
  }
}
