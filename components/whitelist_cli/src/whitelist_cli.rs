#![deny(warnings)]
use clap::{App, Arg, SubCommand};
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::AssetTypeCode;
use ledger::{des_fail, error_location};
use ledger_api_service::RestfulLedgerAccess;
use network::{HttpStandaloneConfig, LedgerStandalone};
use std::fs;
use txn_cli::txn_lib::query_utxo_and_get_type_commitment;
use whitelist::*;

/// Path to the data file.
const WHITELIST_FILE: &str = "whitelist.json";

/// Stores the whitelist data to `WHITELIST_FILE`, when the program starts or the whitelist is updated.
fn store_whitelist_to_file(whitelist: &Whitelist) -> Result<(), PlatformError> {
  match serde_json::to_string(whitelist) {
    Ok(whitelist_str) => {
      if let Err(error) = fs::write(WHITELIST_FILE, &whitelist_str) {
        return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                  WHITELIST_FILE, error)));
      };
    }
    Err(e) => {
      return Err(des_fail!(e));
    }
  }
  Ok(())
}

/// Loads the whitelist.
/// * If the whitelist file exists, loads the whitelist from it.
/// * Otherwise, stores an empty whitelist to the file and returns the empty list.
fn load_whitelist() -> Result<Whitelist, PlatformError> {
  let whitelist = match fs::read_to_string(WHITELIST_FILE) {
    Ok(list) => list,
    Err(_) => {
      let init_list = Whitelist::default();
      store_whitelist_to_file(&init_list)?;
      return Ok(init_list);
    }
  };

  serde_json::from_str::<Whitelist>(&whitelist).map_err(|e| des_fail!(e))
}

/// Parses a string to u64.
fn parse_to_u64(val_str: &str) -> Result<u64, PlatformError> {
  val_str.trim()
         .parse::<u64>()
         .map_err(|_| PlatformError::InputsError(error_location!()))
}

/// Processes input commands and arguments.
fn process_inputs<T>(inputs: clap::ArgMatches, rest_client: &T) -> Result<(), PlatformError>
  where T: RestfulLedgerAccess
{
  let mut whitelist = load_whitelist()?;
  match inputs.subcommand() {
    ("add_member", Some(add_matches)) => {
      let code = if let Some(code_arg) = add_matches.value_of("code") {
        AssetTypeCode::new_from_base64(code_arg)?
      } else {
        println!("Missing asset type code. Use --code.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      whitelist.add_member(code);
      store_whitelist_to_file(&whitelist)
    }
    ("prove_and_verify_membership", Some(prove_and_verify_matches)) => {
      let index = if let Some(index_arg) = prove_and_verify_matches.value_of("index") {
        parse_to_u64(index_arg)?
      } else {
        println!("Missing index of the asset in the whitelist. Use --index.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      let utxo = if let Some(utxo_arg) = prove_and_verify_matches.value_of("utxo") {
        parse_to_u64(utxo_arg)?
      } else {
        println!("Missing UTXO of the transaction which transferred the asset. Use --utxo.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      let blind = if let Some(blind_arg) = prove_and_verify_matches.value_of("blind") {
        serde_json::from_str::<Scalar>(&blind_arg).map_err(|e| des_fail!(e))?
      } else {
        println!("Missing serialized blinding factor for the asset type code commitment. Use --blind.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      let commitment = query_utxo_and_get_type_commitment(utxo, rest_client)?;
      let proof = whitelist.prove_membership(index, commitment, blind)?;
      whitelist.verify_membership(commitment, proof)
    }
    _ => {
      println!("Subcommand missing or not recognized. Try --help");
      Err(PlatformError::InputsError(error_location!()))
    }
  }
}

fn get_cli_app<'a, 'b>() -> App<'a, 'b> {
  App::new("Solvency Proof")
    .version(concat!("0.0.1 Build: ",env!("VERGEN_SHA_SHORT")," ", env!("VERGEN_COMMIT_DATE")))
    .about("Copyright 2020 © Findora. All rights reserved.")
    .arg(Arg::with_name("local")
      .long("local")
      .help("If local flag is specified, data will be queried from a local ledger."))
    .subcommand(SubCommand::with_name("add_member")
      .arg(Arg::with_name("code")
        .short("c")
        .long("code")
        .required(true)
        .takes_value(true)
        .help("Asset type code to add to the whitelist.")))
    .subcommand(SubCommand::with_name("prove_and_verify_membership")
      .arg(Arg::with_name("index")
        .short("i")
        .long("index")
        .required(true)
        .takes_value(true)
        .help("Index of the asset in the whitelist."))
      .arg(Arg::with_name("utxo")
        .short("u")
        .long("utxo")
        .required(true)
        .takes_value(true)
        .help("UTXO of the transaction which transffered the asset."))
      .arg(Arg::with_name("blind")
        .short("b")
        .long("blind")
        .required(true)
        .takes_value(true)
        .help("Serialized blinding factor for the asset type code commitment.")))
}

fn main() -> Result<(), PlatformError> {
  let inputs = get_cli_app().get_matches();

  let local = inputs.value_of("local").is_some();
  let config = {
    if local {
      HttpStandaloneConfig::local()
    } else {
      HttpStandaloneConfig::testnet()
    }
  };

  let rest_client = LedgerStandalone::new_http(&config);

  process_inputs(inputs, &rest_client)
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::AssetRules;
  use ledger::ser_fail;
  use network::{LedgerStandalone, MockLedgerStandalone};
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use txn_cli::txn_lib::define_issue_transfer_and_get_utxo_and_blinds;
  use zei::xfr::asset_record::AssetRecordType;
  use zei::xfr::sig::XfrKeyPair;

  fn submit_command(cmd_vec: Vec<&str>,
                    rest_client: &MockLedgerStandalone)
                    -> Result<(), PlatformError> {
    let app = get_cli_app();
    let inputs = app.get_matches_from_safe(cmd_vec).unwrap();
    process_inputs(inputs, rest_client)
  }

  // Command to add an asset type code to the whitelist
  fn add_member_cmd(code: &str, rest_client: &MockLedgerStandalone) -> Result<(), PlatformError> {
    let args = vec!["Solvency Proof", "add_member", "--code", code];
    submit_command(args, rest_client)
  }

  // Command to add an asset or a liability
  fn prove_and_verify_membership(index: &str,
                                 utxo: &str,
                                 blind: &str,
                                 rest_client: &MockLedgerStandalone)
                                 -> Result<(), PlatformError> {
    let args = vec!["Solvency Proof",
                    "prove_and_verify_membership",
                    "--index",
                    index,
                    "--utxo",
                    utxo,
                    "--blind",
                    blind];
    submit_command(args, rest_client)
  }

  // Ignoring this test because it is broken
  // Redmine #68
  // #[ignore]
  // #[test]
  #[cfg(test)]
  #[allow(unused)]
  fn test_cmd() {
    // Start the standalone ledger
    let mut ledger_standalone = LedgerStandalone::new_mock(1);

    // Generate asset codes and key pairs
    let codes = vec![AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random()];
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let recipient_key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());

    // Define, issue and transfer assets
    let mut utxos = Vec::new();
    let mut blinds = Vec::new();
    for code in codes.iter() {
      let (utxo, _, code_blind) = define_issue_transfer_and_get_utxo_and_blinds(&issuer_key_pair,
                                                    &recipient_key_pair,
                                                    10,
                                                    *code,
                                                    AssetRules::default(),
                                                    AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
                                                    &mut ledger_standalone,
                                                    &mut ChaChaRng::from_entropy()).unwrap();
      let utxo_str = format!("{}", utxo);
      let blind_str = serde_json::to_string(&code_blind).map_err(|e| ser_fail!(e))
                                                        .unwrap();
      utxos.push(utxo_str);
      blinds.push(blind_str);
    }

    // Adds the assets to the whitelist
    add_member_cmd(&codes[0].to_base64(), &mut ledger_standalone).expect("Failed to set conversion rate.");
    add_member_cmd(&codes[1].to_base64(), &mut ledger_standalone).expect("Failed to set conversion rate.");
    add_member_cmd(&codes[2].to_base64(), &mut ledger_standalone).expect("Failed to set conversion rate.");

    // Prove and verify the whitelist membership with the incorrect index
    // catch unwind necessary because there is an unhandled panic in Zei
    let output = std::panic::catch_unwind(|| {
      prove_and_verify_membership("0", &utxos[1], &blinds[1], &ledger_standalone)
    });
    assert!(output.is_err());

    // Prove and verify the whitelist membership with the incorrect UTXO SID
    let output = std::panic::catch_unwind(|| {
      prove_and_verify_membership("1", &utxos[0], &blinds[1], &ledger_standalone)
    });
    assert!(output.is_err());

    // Prove and verify the whitelist membership with the incorrect blinding factor for the asset type code
    let output = std::panic::catch_unwind(|| {
      prove_and_verify_membership("1", &utxos[1], &blinds[0], &ledger_standalone)
    });
    assert!(output.is_err());

    // Prove and verify the whitelist membership with the correct information
    prove_and_verify_membership("0", &utxos[0], &blinds[0], &ledger_standalone)
                          .expect("Failed to prove and verify the whitelist membership.");

    prove_and_verify_membership("1", &utxos[1], &blinds[1], &ledger_standalone)
                           .expect("Failed to prove and verify the whitelist membership.");

    prove_and_verify_membership("2", &utxos[2], &blinds[2], &ledger_standalone)
                            .expect("Failed to prove and verify the whitelist membership.");

    fs::remove_file(WHITELIST_FILE).unwrap();
  }
}
