// #![deny(warnings)]
// use clap::{App, Arg, SubCommand};
// use ledger::data_model::errors::PlatformError;
// use ledger::data_model::AssetTypeCode;
// use ledger::error_location;
// use serde::{Deserialize, Serialize};
// use solvency::*;
// use std::fs;
// use zei::errors::ZeiError;
// use zei::serialization::ZeiFromToBytes;
// use zei::xfr::sig::XfrKeyPair;

// /// Path to the data file.
// const DATA_FILE: &str = "solvency_data.json";

// // TODO (Keyao): Serialize codes
// #[derive(Clone, Debug, Default, Deserialize, Serialize)]
// /// Information of assets, liabilities and conversion rates.
// struct AssetLiabilityAndRateData {
//   /// Asset and liability account
//   asset_and_liability_account: AssetAndLiabilityAccount,

//   /// Contains the asset conversion rate
//   solvency_audit: SolvencyAudit,
// }

// /// Stores the program data to `DATA_FILE`, when the program starts or the data is updated.
// fn store_data_to_file(data: AssetLiabilityAndRateData) -> Result<(), PlatformError> {
//   if let Ok(as_json) = serde_json::to_string(&data) {
//     if let Err(error) = fs::write(DATA_FILE, &as_json) {
//       return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
//                                                 DATA_FILE, error)));
//     };
//   } else {
//   }
//   Ok(())
// }

// /// Loads data.
// /// * If the data file exists, loads data from it.
// /// * Otherwise, stores the initial data to file and returns the data.
// fn load_data() -> Result<AssetLiabilityAndRateData, PlatformError> {
//   let data = match fs::read_to_string(DATA_FILE) {
//     Ok(data) => data,
//     Err(_) => {
//       let init_data = AssetLiabilityAndRateData::default();
//       store_data_to_file(init_data.clone())?;
//       return Ok(init_data);
//     }
//   };

//   serde_json::from_str::<AssetLiabilityAndRateData>(&data).or(Err(PlatformError::DeserializationError))
// }

// /// Parses a string to u64.
// fn parse_to_u64(val_str: &str) -> Result<u64, PlatformError> {
//   val_str.trim()
//          .parse::<u64>()
//          .or_else(|_| Err(PlatformError::InputsError(error_location!())))
// }

// /// Processes input commands and arguments.
// fn process_inputs(inputs: clap::ArgMatches) -> Result<(), PlatformError> {
//   let mut data = load_data()?;
//   match inputs.subcommand() {
//     ("store_key_pair", Some(store_matches)) => {
//       let key_pair = if let Some(key_pair_arg) = store_matches.value_of("key_pair") {
//         key_pair_arg
//       } else {
//         println!("Missing encoded key pair string. Use --key_pair.");
//         return Err(PlatformError::InputsError(error_location!()));
//       };
//       if let Some(file_arg) = store_matches.value_of("file") {
//         if let Err(error) = fs::write(file_arg, &key_pair) {
//           return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
//                                                     file_arg, error)));
//         };
//       } else {
//         println!("Missing file path. Use --file.");
//         return Err(PlatformError::InputsError(error_location!()));
//       }
//       Ok(())
//     }
//     ("set_rate", Some(set_matches)) => {
//       let code = if let Some(code_arg) = set_matches.value_of("code") {
//         AssetTypeCode::new_from_base64(code_arg)?
//       } else {
//         println!("Missing asset code. Use --code.");
//         return Err(PlatformError::InputsError(error_location!()));
//       };
//       let rate = if let Some(rate_arg) = set_matches.value_of("rate") {
//         parse_to_u64(rate_arg)?
//       } else {
//         println!("Missing conversion rate. Use --rate.");
//         return Err(PlatformError::InputsError(error_location!()));
//       };
//       data.solvency_audit.set_rate(code, rate);
//       store_data_to_file(data)
//     }
//     ("add_asset_or_liability", Some(add_matches)) => {
//       let key_pair = if let Some(key_file_arg) = add_matches.value_of("key_file") {
//         let key_pair_str = match fs::read_to_string(key_file_arg) {
//           Ok(k) => k,
//           Err(_) => {
//             return Err(PlatformError::IoError(format!("Failed to read file: {}", key_file_arg)));
//           }
//         };
//         XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|_| {
//           Err(PlatformError::DeserializationError)
//         })?)
//       } else {
//         println!("Missing path to the key pair file. Use --key_file.");
//         return Err(PlatformError::InputsError(error_location!()));
//       };
//       let code = if let Some(code_arg) = add_matches.value_of("code") {
//         AssetTypeCode::new_from_base64(code_arg)?
//       } else {
//         println!("Missing asset code. Use --code.");
//         return Err(PlatformError::InputsError(error_location!()));
//       };
//       let utxo = if let Some(utxo_arg) = add_matches.value_of("utxo") {
//         parse_to_u64(utxo_arg)?
//       } else {
//         println!("Missing UTXO of the asset or liability. Use --utxo.");
//         return Err(PlatformError::InputsError(error_location!()));
//       };
//       if let Some(type_arg) = add_matches.value_of("type") {
//         match type_arg {
//           "public_asset" => data.asset_and_liability_account
//                                 .add_public_asset(key_pair.get_sk_ref(), code, utxo)?,
//           "hidden_asset" => data.asset_and_liability_account
//                                 .add_hidden_asset(key_pair.get_sk_ref(), code, utxo)?,
//           "public_liability" => data.asset_and_liability_account
//                                     .add_public_liability(key_pair.get_sk_ref(), code, utxo)?,
//           _ => data.asset_and_liability_account
//                    .add_hidden_liability(key_pair.get_sk_ref(), code, utxo)?,
//         }
//       } else {
//         println!("Missing asset or liability type. Use --type.");
//         return Err(PlatformError::InputsError(error_location!()));
//       }
//       store_data_to_file(data)
//     }
//     ("prove_and_verify_solvency", _) => {
//       data.solvency_audit
//           .prove_solvency_and_store(&mut data.asset_and_liability_account)?;
//       match data.solvency_audit
//                 .verify_solvency(&data.asset_and_liability_account)
//       {
//         Ok(_) => {
//           println!("Solvency proof and verification succeeded.");
//         }
//         _ => {
//           println!("Solvency proof and verification failed.");
//           return Err(PlatformError::ZeiError(error_location!(),
//                                              ZeiError::SolvencyVerificationError));
//         }
//       }
//       store_data_to_file(data)
//     }
//     _ => {
//       println!("Subcommand missing or not recognized. Try --help");
//       Err(PlatformError::InputsError(error_location!()))
//     }
//   }
// }

fn main() {}
// fn main() -> Result<(), PlatformError> {
//   let inputs = App::new("Solvency Proof").version("0.1.0").about("Copyright 2020 © Findora. All rights reserved.")
//     .subcommand(SubCommand::with_name("store_key_pair")
//       .arg(Arg::with_name("key_pair")
//         .short("k")
//         .long("key_pair")
//         .required(true)
//         .takes_value(true)
//         .help("Encoded key pair string."))
//       .arg(Arg::with_name("file")
//         .short("f")
//         .long("file")
//         .required(true)
//         .takes_value(true)
//         .help("File to store the generated key pair.")))
//     .subcommand(SubCommand::with_name("set_rate")
//       .arg(Arg::with_name("code")
//         .short("c")
//         .long("code")
//         .required(true)
//         .takes_value(true)
//         .help("Asset code."))
//       .arg(Arg::with_name("rate")
//         .short("r")
//         .long("rate")
//         .required(true)
//         .takes_value(true)
//         .help("Conversion rate of this asset.")))
//     .subcommand(SubCommand::with_name("add_asset_or_liability")
//       .arg(Arg::with_name("key_file")
//         .short("k")
//         .long("key_file")
//         .required(true)
//         .takes_value(true)
//         .help("File storing the key pair."))
//       .arg(Arg::with_name("type")
//         .short("t")
//         .long("type")
//         .required(true)
//         .takes_value(true)
//         .possible_values(&["public_asset", "hidden_asset", "public_liability", "hidden_liability"])
//         .help("Specify whether to add asset or liability, and whether the record is public or hidden."))
//       .arg(Arg::with_name("code")
//         .short("c")
//         .long("code")
//         .required(true)
//         .takes_value(true)
//         .help("Code of the asset or liability."))
//       .arg(Arg::with_name("utxo")
//         .short("u")
//         .long("utxo")
//         .required(true)
//         .takes_value(true)
//         .help("UTXO of the asset or liability.")))
//     .subcommand(SubCommand::with_name("prove_and_verify_solvency"))
//     .get_matches();

//   process_inputs(inputs)
// }

// #[cfg(test)]
// mod tests {
//   use super::*;
//   use ledger_standalone::LedgerStandalone;
//   use rand_chacha::ChaChaRng;
//   use rand_core::SeedableRng;
//   use std::io::{self, Write};
//   use std::process::{Command, Output};
//   use zei::xfr::sig::XfrKeyPair;

//   #[cfg(debug_assertions)]
//   const COMMAND: &str = "../../target/debug/solvency_cli";
//   #[cfg(not(debug_assertions))]
//   const COMMAND: &str = "../../target/release/solvency_cli";

//   // Command to set asset conversion rates
//   fn set_rate_cmd(code: &str, rate: &str) -> io::Result<Output> {
//     Command::new(COMMAND).arg("set_rate")
//                          .args(&["--code", code])
//                          .args(&["--rate", rate])
//                          .output()
//   }

//   // Issue and transfer assets, and get the UTXOs
//   fn issue_transfer_and_get_utxos(issuer_key_pair: &XfrKeyPair,
//                                   recipient_key_pair: &XfrKeyPair,
//                                   codes: (AssetTypeCode, AssetTypeCode, AssetTypeCode),
//                                   ledger_standalone: &LedgerStandalone)
//                                   -> (String, String, String, String, String, String, String) {
//     (format!("{}",
//              test_issue_transfer_submit_and_get_utxo(issuer_key_pair,
//                                                      recipient_key_pair,
//                                                      codes.0,
//                                                      10,
//                                                      0,
//                                                      ledger_standalone).unwrap()).to_string(),
//      format!("{}",
//              test_issue_transfer_submit_and_get_utxo(issuer_key_pair,
//                                                      recipient_key_pair,
//                                                      codes.1,
//                                                      200,
//                                                      1,
//                                                      ledger_standalone).unwrap()).to_string(),
//      format!("{}",
//              test_issue_transfer_submit_and_get_utxo(issuer_key_pair,
//                                                      recipient_key_pair,
//                                                      codes.2,
//                                                      3,
//                                                      2,
//                                                      ledger_standalone).unwrap()).to_string(),
//      format!("{}",
//              test_issue_transfer_submit_and_get_utxo(issuer_key_pair,
//                                                      recipient_key_pair,
//                                                      codes.0,
//                                                      40,
//                                                      3,
//                                                      ledger_standalone).unwrap()).to_string(),
//      format!("{}",
//              test_issue_transfer_submit_and_get_utxo(issuer_key_pair,
//                                                      recipient_key_pair,
//                                                      codes.1,
//                                                      50,
//                                                      4,
//                                                      ledger_standalone).unwrap()).to_string(),
//      format!("{}",
//              test_issue_transfer_submit_and_get_utxo(issuer_key_pair,
//                                                      recipient_key_pair,
//                                                      codes.1,
//                                                      150,
//                                                      5,
//                                                      ledger_standalone).unwrap()).to_string(),
//      format!("{}",
//              test_issue_transfer_submit_and_get_utxo(issuer_key_pair,
//                                                      recipient_key_pair,
//                                                      codes.0,
//                                                      30,
//                                                      6,
//                                                      ledger_standalone).unwrap()).to_string())
//   }

//   // Command to add an asset or a liability
//   fn add_asset_or_liability_cmd(add_type: &str,
//                                 key_file: &str,
//                                 code: &str,
//                                 utxo: &str)
//                                 -> io::Result<Output> {
//     Command::new(COMMAND).arg("add_asset_or_liability")
//                          .args(&["--key_file", key_file])
//                          .args(&["--type", add_type])
//                          .args(&["--code", code])
//                          .args(&["--utxo", utxo])
//                          .output()
//   }

//   #[test]
//   #[ignore]
//   fn test_cmd() {
//     // Start the standalone ledger
//     let ledger_standalone = &LedgerStandalone::new();
//     ledger_standalone.poll_until_ready().unwrap();

//     // Generate asset codes and key pairs
//     let codes =
//       (AssetTypeCode::gen_random(), AssetTypeCode::gen_random(), AssetTypeCode::gen_random());
//     let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
//     let recipient_key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());

//     // Store the recipient's key pair
//     let recipient_key_pair_str = hex::encode(recipient_key_pair.zei_to_bytes());
//     let key_file = "solvency_key_pair";
//     let output = Command::new(COMMAND).arg("store_key_pair")
//                                       .args(&["--key_pair", &recipient_key_pair_str])
//                                       .args(&["--file", key_file])
//                                       .output()
//                                       .expect("Failed to store the key pair.");
//     io::stdout().write_all(&output.stdout).unwrap();
//     io::stdout().write_all(&output.stderr).unwrap();
//     assert!(output.status.success());

//     // Define, issue and transfer assets
//     test_define_and_submit_multiple(issuer_key_pair, codes, ledger_standalone).unwrap();
//     let code_0 = &codes.0.to_base64();
//     let code_1 = &codes.1.to_base64();
//     let code_2 = &codes.2.to_base64();
//     let (utxo_0, utxo_1, utxo_2, utxo_3, utxo_4, utxo_5, utxo_6) =
//       issue_transfer_and_get_utxos(issuer_key_pair,
//                                    &recipient_key_pair,
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
// }
