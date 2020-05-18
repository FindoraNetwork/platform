#![deny(warnings)]
use clap::{App, Arg, SubCommand};
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::AssetTypeCode;
use ledger::{des_fail, error_location};
use serde::{Deserialize, Serialize};
use solvency::*;
use std::fs;
use zei::errors::ZeiError;

/// Path to the data file.
const DATA_FILE: &str = "solvency_data.json";
// TODO (Keyao): support protocol and host switch
const PROTOCOL: &str = "http";
const HOST: &str = "localhost";

// TODO (Keyao): Serialize codes
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
/// Information of assets, liabilities and conversion rates.
struct AssetLiabilityAndRateData {
  /// Asset and liability account
  asset_and_liability_account: AssetAndLiabilityAccount,

  /// Contains the asset conversion rate
  solvency_audit: SolvencyAudit,
}

/// Stores a string to the specified file.
fn store_string_to_file(string_data: &str, file: &str) -> Result<(), PlatformError> {
  match fs::write(file, string_data) {
    Ok(_) => Ok(()),
    Err(e) => Err(PlatformError::IoError(format!("Failed to create file {}: {}.", file, e))),
  }
}

/// Stores the program data to `DATA_FILE`, when the program starts or the data is updated.
fn store_data_to_file(data_dir: &str,
                      data: AssetLiabilityAndRateData)
                      -> Result<(), PlatformError> {
  let data_file_path = format!("{}/{}", data_dir, DATA_FILE);
  if let Ok(data_string) = serde_json::to_string(&data) {
    store_string_to_file(&data_string, &data_file_path)?;
  } else {
  }
  Ok(())
}

/// Loads data.
/// * If the data file exists, loads data from it.
/// * Otherwise, stores the initial data to file and returns the data.
fn load_data(data_dir: &str) -> Result<AssetLiabilityAndRateData, PlatformError> {
  let data_file_path = format!("{}/{}", data_dir, DATA_FILE);
  let data = match fs::read_to_string(data_file_path) {
    Ok(data) => data,
    Err(_) => {
      let init_data = AssetLiabilityAndRateData::default();
      store_data_to_file(data_dir, init_data.clone())?;
      return Ok(init_data);
    }
  };

  serde_json::from_str::<AssetLiabilityAndRateData>(&data).or_else(|e| Err(des_fail!(e)))
}

/// Parses a string to u64.
fn parse_to_u64(val_str: &str) -> Result<u64, PlatformError> {
  val_str.trim()
         .parse::<u64>()
         .or_else(|_| Err(PlatformError::InputsError(error_location!())))
}

/// Processes input commands and arguments.
fn process_inputs(inputs: clap::ArgMatches) -> Result<(), PlatformError> {
  let dir = if let Some(d) = inputs.value_of("dir") {
    d
  } else {
    println!("Missing directory to store data. Use --dir.");
    return Err(PlatformError::InputsError(error_location!()));
  };
  let mut data = load_data(dir)?;
  match inputs.subcommand() {
    ("set_rate", Some(set_matches)) => {
      let code = if let Some(code_arg) = set_matches.value_of("code") {
        AssetTypeCode::new_from_base64(code_arg)?
      } else {
        println!("Missing asset code. Use --code.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      let rate = if let Some(rate_arg) = set_matches.value_of("rate") {
        parse_to_u64(rate_arg)?
      } else {
        println!("Missing conversion rate. Use --rate.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      data.solvency_audit.set_rate(code, rate);
      store_data_to_file(dir, data)
    }
    ("add_asset_or_liability", Some(add_matches)) => {
      let amount_type = if let Some(type_arg) = add_matches.value_of("type") {
        match type_arg {
          "asset" => AmountType::Asset,
          _ => AmountType::Liability,
        }
      } else {
        println!("Missing asset or liability type. Use --type.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      let amount = if let Some(amount_arg) = add_matches.value_of("amount") {
        parse_to_u64(amount_arg)?
      } else {
        println!("Missing amount of the asset or liability. Use --amount.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      let code = if let Some(code_arg) = add_matches.value_of("code") {
        AssetTypeCode::new_from_base64(code_arg)?
      } else {
        println!("Missing asset code. Use --code.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      let blinds = if let Some(blinds_arg) = add_matches.value_of("blinds") {
        Some(serde_json::from_str::<((Scalar, Scalar), Scalar)>(&blinds_arg).or_else(|e| {
                                                                              Err(des_fail!(e))
                                                                            })?)
      } else {
        None
      };
      let utxo = if let Some(utxo_arg) = add_matches.value_of("utxo") {
        parse_to_u64(utxo_arg)?
      } else {
        println!("Missing UTXO of the asset or liability. Use --utxo.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      data.asset_and_liability_account.update(amount_type,
                                               amount,
                                               code,
                                               blinds,
                                               utxo,
                                               PROTOCOL,
                                               HOST)?;
      store_data_to_file(dir, data)
    }
    ("prove_and_verify_solvency", _) => {
      data.solvency_audit
          .prove_solvency_and_store(&mut data.asset_and_liability_account)?;
      match data.solvency_audit
                .verify_solvency(&data.asset_and_liability_account)
      {
        Ok(_) => {
          println!("Solvency proof and verification succeeded.");
        }
        _ => {
          println!("Solvency proof and verification failed.");
          return Err(PlatformError::ZeiError(error_location!(),
                                             ZeiError::SolvencyVerificationError));
        }
      }
      store_data_to_file(dir, data)
    }
    _ => {
      println!("Subcommand missing or not recognized. Try --help");
      Err(PlatformError::InputsError(error_location!()))
    }
  }
}

fn main() -> Result<(), PlatformError> {
  let inputs = App::new("Solvency Proof").version("0.1.0").about("Copyright 2020 © Findora. All rights reserved.")
    .arg(Arg::with_name("dir")
      .short("d")
      .long("dir")
      .value_name("PATH")
      .required(true)
      .takes_value(true)
      .help("Directory to store data"))
    .subcommand(SubCommand::with_name("set_rate")
      .arg(Arg::with_name("code")
        .short("c")
        .long("code")
        .required(true)
        .takes_value(true)
        .help("Asset code."))
      .arg(Arg::with_name("rate")
        .short("r")
        .long("rate")
        .required(true)
        .takes_value(true)
        .help("Conversion rate of this asset.")))
    .subcommand(SubCommand::with_name("add_asset_or_liability")
      .arg(Arg::with_name("type")
        .short("t")
        .long("type")
        .required(true)
        .takes_value(true)
        .possible_values(&["asset", "liability"])
        .help("Specify whether to add asset or liability."))
      .arg(Arg::with_name("amount")
        .short("a")
        .long("amount")
        .required(true)
        .takes_value(true)
        .help("Amount of the asset or liability."))
      .arg(Arg::with_name("code")
        .short("c")
        .long("code")
        .required(true)
        .takes_value(true)
        .help("Code of the asset or liability."))
      .arg(Arg::with_name("blinds")
        .short("b")
        .long("blinds")
        .required(false)
        .takes_value(true)
        .help("Serialized ((low asset amount blind, high assset amount blind), asset code blind) for confidential amount."))
      .arg(Arg::with_name("utxo")
        .short("u")
        .long("utxo")
        .required(true)
        .takes_value(true)
        .help("UTXO of the asset or liability.")))
    .subcommand(SubCommand::with_name("prove_and_verify_solvency"))
    .get_matches();

  process_inputs(inputs)
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::AssetRules;
  use ledger_standalone::LedgerStandalone;
  use rand_chacha::ChaChaRng;
  use rand_core::{CryptoRng, RngCore, SeedableRng};
  use std::io::{self, Write};
  use std::process::{Command, Output};
  use tempfile::tempdir;
  use txn_cli::txn_lib::{define_and_submit, issue_transfer_and_get_utxo_and_blinds};
  use zei::xfr::asset_record::AssetRecordType;
  use zei::xfr::sig::XfrKeyPair;

  #[cfg(debug_assertions)]
  const COMMAND: &str = "../../target/debug/solvency_cli";
  #[cfg(not(debug_assertions))]
  const COMMAND: &str = "../../target/release/solvency_cli";

  // Command to set asset conversion rates
  fn set_rate_cmd(dir: &str, code: &str, rate: &str) -> io::Result<Output> {
    Command::new(COMMAND).args(&["--dir", dir])
                         .arg("set_rate")
                         .args(&["--code", code])
                         .args(&["--rate", rate])
                         .output()
  }

  // Issue and transfer assets, and get the serialized UTXOs and blinds
  fn issue_transfer_multiple<R: CryptoRng + RngCore>(issuer_key_pair: &XfrKeyPair,
                                                     recipient_key_pair: &XfrKeyPair,
                                                     codes: Vec<AssetTypeCode>,
                                                     prng: &mut R,
                                                     ledger_standalone: &LedgerStandalone)
                                                     -> (Vec<String>, Vec<String>) {
    let mut utxos = Vec::new();
    let mut blinds = Vec::new();
    let (utxo_0, _, _) = issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                     recipient_key_pair,
                                                     10,
                                                     codes[0],
                                                     AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                     0,
                                                     prng,
                                                     ledger_standalone).unwrap();
    utxos.push(format!("{}", utxo_0));
    let (utxo_1, amount_blinds_1, code_blind_1) = issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                     recipient_key_pair,
                                                     200,
                                                     codes[1],
                                                     AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                     1,
                                                     prng,
                                                     ledger_standalone).unwrap();
    utxos.push(format!("{}", utxo_1));
    blinds.push(serde_json::to_string(&(amount_blinds_1, code_blind_1)).unwrap());
    let (utxo_2, amount_blinds_2, code_blind_2) = issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                     recipient_key_pair,
                                                     3,
                                                     codes[2],
                                                     AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                     2,
                                                     prng,
                                                     ledger_standalone).unwrap();
    utxos.push(format!("{}", utxo_2));
    blinds.push(serde_json::to_string(&(amount_blinds_2, code_blind_2)).unwrap());
    let (utxo_3, _, _) = issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                     recipient_key_pair,
                                                     40,
                                                     codes[0],
                                                     AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                     3,
                                                     prng,
                                                     ledger_standalone).unwrap();
    utxos.push(format!("{}", utxo_3));
    let (utxo_4, amount_blinds_4, code_blind_4) =issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                     recipient_key_pair,
                                                     50,
                                                     codes[1],
                                                     AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                     4,
                                                     prng,
                                                     ledger_standalone).unwrap();
    utxos.push(format!("{}", utxo_4));
    blinds.push(serde_json::to_string(&(amount_blinds_4, code_blind_4)).unwrap());
    let (utxo_5, amount_blinds_5, code_blind_5) =issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                     recipient_key_pair,
                                                     150,
                                                     codes[1],
                                                     AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                     5,
                                                     prng,
                                                     ledger_standalone).unwrap();
    utxos.push(format!("{}", utxo_5));
    blinds.push(serde_json::to_string(&(amount_blinds_5, code_blind_5)).unwrap());
    let (utxo_6, _, _) =issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                     recipient_key_pair,
                                                     30,
                                                     codes[0],
                                                     AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                     6,
                                                     prng,
                                                     ledger_standalone).unwrap();
    utxos.push(format!("{}", utxo_6));
    (utxos, blinds)
  }

  // Command to add a confidential asset or liability
  fn add_confidential_asset_or_liability_cmd(dir: &str,
                                             amount_type: &str,
                                             amount: &str,
                                             code: &str,
                                             blinds: &str,
                                             utxo: &str)
                                             -> io::Result<Output> {
    Command::new(COMMAND).args(&["--dir", dir])
                         .arg("add_asset_or_liability")
                         .args(&["--type", amount_type])
                         .args(&["--amount", amount])
                         .args(&["--code", code])
                         .args(&["--blinds", blinds])
                         .args(&["--utxo", utxo])
                         .output()
  }

  // Command to add a nonconfidential asset or liability
  fn add_nonconfidential_asset_or_liability_cmd(dir: &str,
                                                amount_type: &str,
                                                amount: &str,
                                                code: &str,
                                                utxo: &str)
                                                -> io::Result<Output> {
    Command::new(COMMAND).args(&["--dir", dir])
                         .arg("add_asset_or_liability")
                         .args(&["--type", amount_type])
                         .args(&["--amount", amount])
                         .args(&["--code", code])
                         .args(&["--utxo", utxo])
                         .output()
  }

  #[test]
  #[ignore]
  fn test_cmd() {
    let tmp_dir = tempdir().unwrap();
    let dir = tmp_dir.path().to_str().unwrap();

    // Start the standalone ledger
    let ledger_standalone = &LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Generate asset codes and key pairs
    let codes = vec![AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random()];
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let recipient_key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());

    // Define, issue and transfer assets
    for code in codes.iter() {
      define_and_submit(&issuer_key_pair,
                        *code,
                        AssetRules::default(),
                        ledger_standalone).unwrap();
    }
    let code_0 = &codes[0].to_base64();
    let code_1 = &codes[1].to_base64();
    let code_2 = &codes[2].to_base64();
    let (utxos, blinds) = issue_transfer_multiple(issuer_key_pair,
                                                  &recipient_key_pair,
                                                  codes,
                                                  &mut ChaChaRng::from_entropy(),
                                                  ledger_standalone);

    // Set asset conversion rates
    let output = set_rate_cmd(dir, code_0, "1").expect("Failed to set conversion rate.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output = set_rate_cmd(dir, code_1, "100").expect("Failed to set conversion rate.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output = set_rate_cmd(dir, code_2, "1").expect("Failed to set conversion rate.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    // Add assets and liabilities such that total asset amount > total liabiliity amount
    let output =
    add_nonconfidential_asset_or_liability_cmd(dir, "asset", "10", code_0, &utxos[0]).expect("Failed to add public asset.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output =
    add_confidential_asset_or_liability_cmd(dir, "asset", "200", code_1, &blinds[0], &utxos[1]).expect("Failed to add hidden asset.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output =
    add_confidential_asset_or_liability_cmd(dir, "asset","3", code_2, &blinds[1], &utxos[2]).expect("Failed to add hidden asset.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output = add_nonconfidential_asset_or_liability_cmd(dir, "liability","40", code_0, &utxos[3])
                                      .expect("Failed to add public liability.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output =
      add_confidential_asset_or_liability_cmd(dir,
                                              "liability",
                                              "50",
                                              code_1,
                                              &blinds[2],
                                              &utxos[4]).expect("Failed to add hidden liability.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    // Prove and verify solvency
    let output = Command::new(COMMAND).args(&["--dir", dir])
                                      .arg("prove_and_verify_solvency")
                                      .output()
                                      .expect("Failed to prove and verify solvency.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    // Add additional liabilities to make total asset amount < total liabiliity amount
    let output =
      add_confidential_asset_or_liability_cmd(dir,
                                              "liability",
                                              "150",
                                              code_1,
                                              &blinds[3],
                                              &utxos[5]).expect("Failed to add hidden liability.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    // Prove and verify solvency
    // Should fail since total asset amount < total liabiliity amount
    let output = Command::new(COMMAND).args(&["--dir", dir])
                                      .arg("prove_and_verify_solvency")
                                      .output()
                                      .expect("Failed to prove and verify solvency.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(!output.status.success());

    // Add additional assets to make total asset amount > total liabiliity amount
    let output =
    add_nonconfidential_asset_or_liability_cmd(dir, "asset", "30", code_0, &utxos[6]).expect("Failed to add public asset.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    // Prove and verify solvency
    let output = Command::new(COMMAND).args(&["--dir", dir])
                                      .arg("prove_and_verify_solvency")
                                      .output()
                                      .expect("Failed to prove and verify solvency.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    tmp_dir.close().unwrap();
  }
}
