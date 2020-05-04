#![deny(warnings)]
use clap::{App, Arg, SubCommand};
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::AssetTypeCode;
use ledger::error_location;
use std::fs;
use whitelist::*;
// use zei::errors::ZeiError;
// use zei::serialization::ZeiFromToBytes;
// use zei::xfr::sig::XfrKeyPair;

/// Path to the data file.
const WHITELIST_FILE: &str = "whitelist.json";

/// Stores the whitelist data to `WHITELIST_FILE`, when the program starts or the whitelist is updated.
fn store_whitelist_to_file(whitelist: &Whitelist) -> Result<(), PlatformError> {
  if let Ok(whitelist_str) = serde_json::to_string(whitelist) {
    if let Err(error) = fs::write(WHITELIST_FILE, &whitelist_str) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                WHITELIST_FILE, error)));
    };
  } else {
    return Err(PlatformError::SerializationError);
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

  serde_json::from_str::<Whitelist>(&whitelist).or(Err(PlatformError::DeserializationError))
}

/// Parses a string to u64.
fn parse_to_u64(val_str: &str) -> Result<u64, PlatformError> {
  val_str.trim()
         .parse::<u64>()
         .or_else(|_| Err(PlatformError::InputsError(error_location!())))
}

/// Processes input commands and arguments.
fn process_inputs(inputs: clap::ArgMatches) -> Result<(), PlatformError> {
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
        serde_json::from_str::<Scalar>(&blind_arg).or(Err(PlatformError::DeserializationError))?
      } else {
        println!("Missing serialized blinding factor for the asset type code commitment. Use --blind.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      whitelist.prove_and_verify_membership(index, utxo, blind)
    }
    _ => {
      println!("Subcommand missing or not recognized. Try --help");
      Err(PlatformError::InputsError(error_location!()))
    }
  }
}

fn main() -> Result<(), PlatformError> {
  let inputs = App::new("Solvency Proof").version("0.1.0").about("Copyright 2020 © Findora. All rights reserved.")
    .subcommand(SubCommand::with_name("add_member")
      .arg(Arg::with_name("code")
        .short("c")
        .long("code")
        .required(true)
        .takes_value(true)
        .help("Asset type code to add to the whitelist.")))
    .subcommand(SubCommand::with_name("prove_and_verify_whitelist")
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
    .get_matches();

  process_inputs(inputs)
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger_standalone::LedgerStandalone;
  // use rand_chacha::ChaChaRng;
  // use rand_core::SeedableRng;
  // use std::io::{self, Write};
  use std::process::{Command, Output};
  use zei::xfr::sig::XfrKeyPair;

  #[cfg(debug_assertions)]
  const COMMAND: &str = "../../target/debug/whitelist_cli";
  #[cfg(not(debug_assertions))]
  const COMMAND: &str = "../../target/release/whitelist_cli";

  // Command to add an asset type code to the whitelist
  fn add_member_cmd(code: &str, rate: &str) -> io::Result<Output> {
    Command::new(COMMAND).arg("add_member")
                         .args(&["--code", code])
                         .output()
  }

  // Command to add an asset or a liability
  fn add_asset_or_liability_cmd(add_type: &str,
                                key_file: &str,
                                code: &str,
                                utxo: &str)
                                -> io::Result<Output> {
    Command::new(COMMAND).arg("add_asset_or_liability")
                         .args(&["--key_file", key_file])
                         .args(&["--type", add_type])
                         .args(&["--code", code])
                         .args(&["--utxo", utxo])
                         .output()
  }

  //   #[test]
  //   fn test_cmd() {
  //     // Start the standalone ledger
  //     let ledger_standalone = &LedgerStandalone::new();
  //     ledger_standalone.poll_until_ready().unwrap();

  //     // Generate asset codes and key pairs
  //     let codes = vec![AssetTypeCode::gen_random(),
  //                      AssetTypeCode::gen_random(),
  //                      AssetTypeCode::gen_random(),
  //                      AssetTypeCode::gen_random(),
  //                      AssetTypeCode::gen_random()];
  //     let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
  //     let receipient_key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());

  //     // Define, issue and transfer assets
  //     test_define_and_submit_multiple(issuer_key_pair, codes, ledger_standalone).unwrap();
  //     let code_0 = &codes.0.to_base64();
  //     let code_1 = &codes.1.to_base64();
  //     let code_2 = &codes.2.to_base64();
  //     let (utxo_0, utxo_1, utxo_2, utxo_3, utxo_4, utxo_5, utxo_6) =
  //       issue_transfer_and_get_utxos(issuer_key_pair,
  //                                    &receipient_key_pair,
  //                                    codes,
  //                                    ledger_standalone);

  //     // Set asset conversion rates
  //     let output = set_rate_cmd(code_0, "1").expect("Failed to set conversion rate.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     let output = set_rate_cmd(code_1, "100").expect("Failed to set conversion rate.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     let output = set_rate_cmd(code_2, "1").expect("Failed to set conversion rate.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     // Add assets and liabilities such that total asset amount > total liabiliity amount
  //     let output =
  //       add_asset_or_liability_cmd("public_asset", key_file, code_0, &utxo_0).expect("Failed to add public asset.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     let output =
  //       add_asset_or_liability_cmd("hidden_asset", key_file, code_1, &utxo_1).expect("Failed to add hidden asset.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     let output =
  //     add_asset_or_liability_cmd("hidden_asset", key_file, code_2, &utxo_2).expect("Failed to add hidden asset.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     let output = add_asset_or_liability_cmd("public_liability", key_file, code_0, &utxo_3)
  //                                       .expect("Failed to add public liability.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     let output = add_asset_or_liability_cmd("hidden_liability", key_file, code_1, &utxo_4)
  //                                       .expect("Failed to add hidden liability.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     // Prove and verify solvency
  //     let output = Command::new(COMMAND).arg("prove_and_verify_solvency")
  //                                       .output()
  //                                       .expect("Failed to prove and verify solvency.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     // Add additional liabilities to make total asset amount < total liabiliity amount
  //     let output = add_asset_or_liability_cmd("hidden_liability", key_file, code_1, &utxo_5).expect("Failed to add hidden liability.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     // Prove and verify solvency
  //     // Should fail since total asset amount < total liabiliity amount
  //     let output = Command::new(COMMAND).arg("prove_and_verify_solvency")
  //                                       .output()
  //                                       .expect("Failed to prove and verify solvency.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(!output.status.success());

  //     // Add additional assets to make total asset amount > total liabiliity amount
  //     let output =
  //       add_asset_or_liability_cmd("public_asset", key_file, code_0, &utxo_6).expect("Failed to add public asset.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     // Prove and verify solvency
  //     let output = Command::new(COMMAND).arg("prove_and_verify_solvency")
  //                                       .output()
  //                                       .expect("Failed to prove and verify solvency.");
  //     io::stdout().write_all(&output.stdout).unwrap();
  //     io::stdout().write_all(&output.stderr).unwrap();
  //     assert!(output.status.success());

  //     fs::remove_file("solvency_data.json").unwrap();
  //     fs::remove_file(key_file).unwrap();
  //   }
}
