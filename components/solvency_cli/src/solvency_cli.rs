// #![deny(warnings)]
// use clap::{App, Arg, SubCommand};
// use ledger::data_model::errors::PlatformError;
// use ledger::error_location;
// use serde::{Deserialize, Serialize};
// use solvency::*;
// use std::fs;
// use zei::errors::ZeiError;

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
//     ("set_asset_and_rate", Some(set_matches)) => {
//       let rate = if let Some(rate_arg) = set_matches.value_of("rate") {
//         parse_to_u64(rate_arg)?
//       } else {
//         println!("Missing conversion rate. Use --rate.");
//         return Err(PlatformError::InputsError(error_location!()));
//       };
//       data.solvency_audit.set_asset_and_rate(rate);
//       store_data_to_file(data)
//     }
//     ("add_asset_or_liability", Some(add_matches)) => {
//       let code = if let Some(id_arg) = add_matches.value_of("id") {
//         data.solvency_audit.conversion_rates[parse_to_u64(id_arg)? as usize].0
//       } else {
//         println!("Missing asset or liability id. Use --id.");
//         return Err(PlatformError::InputsError(error_location!()));
//       };
//       let amount = if let Some(amount_arg) = add_matches.value_of("amount") {
//         parse_to_u64(amount_arg)?
//       } else {
//         println!("Missing asset or liability amount. Use --amount.");
//         return Err(PlatformError::InputsError(error_location!()));
//       };
//       if let Some(type_arg) = add_matches.value_of("type") {
//         match type_arg {
//           "public_asset" => data.asset_and_liability_account
//                                 .add_public_asset(amount, code),
//           "hidden_asset" => data.asset_and_liability_account
//                                 .add_hidden_asset(amount, code),
//           "public_liability" => data.asset_and_liability_account
//                                     .add_public_liability(amount, code),
//           _ => data.asset_and_liability_account
//                    .add_hidden_liability(amount, code),
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
//     .subcommand(SubCommand::with_name("set_asset_and_rate")
//       .arg(Arg::with_name("rate")
//         .short("r")
//         .long("rate")
//         .required(true)
//         .takes_value(true)
//         .help("Conversion rate of this asset.")))
//     .subcommand(SubCommand::with_name("add_asset_or_liability")
//       .arg(Arg::with_name("type")
//         .short("t")
//         .long("type")
//         .required(true)
//         .takes_value(true)
//         .possible_values(&["public_asset", "hidden_asset", "public_liability", "hidden_liability"])
//         .help("Specify whether to add asset or liability, and whether the record is public or hidden."))
//       .arg(Arg::with_name("id")
//         .short("i")
//         .long("id")
//         .required(true)
//         .takes_value(true)
//         .help("Index in the conversion rate table."))
//       .arg(Arg::with_name("amount")
//         .short("a")
//         .long("amount")
//         .required(true)
//         .takes_value(true)
//         .help("Amount of this asset or liability.")))
//     .subcommand(SubCommand::with_name("prove_and_verify_solvency"))
//     .get_matches();

//   process_inputs(inputs)
// }

// #[cfg(test)]
// mod tests {
//   use super::*;
//   use std::io::{self, Write};
//   use std::process::{Command, Output};

//   #[cfg(debug_assertions)]
//   const COMMAND: &str = "../../target/debug/solvency_cli";
//   #[cfg(not(debug_assertions))]
//   const COMMAND: &str = "../../target/release/solvency_cli";

//   // Command to set asset conversion rates
//   fn set_asset_and_rate_cmd(rate: &str) -> io::Result<Output> {
//     Command::new(COMMAND).arg("set_asset_and_rate")
//                          .args(&["--rate", rate])
//                          .output()
//   }

//   // Command to add an asset or a liability
//   fn add_asset_or_liability_cmd(add_type: &str, id: &str, amount: &str) -> io::Result<Output> {
//     Command::new(COMMAND).arg("add_asset_or_liability")
//                          .args(&["--type", add_type])
//                          .args(&["--id", id])
//                          .args(&["--amount", amount])
//                          .output()
//   }

//   #[test]
//   fn test_cmd() {
//     // Set asset conversion rates
//     let output = set_asset_and_rate_cmd("1").expect("Failed to set conversion rate.");
//     io::stdout().write_all(&output.stdout).unwrap();
//     io::stdout().write_all(&output.stderr).unwrap();
//     assert!(output.status.success());

//     let output = set_asset_and_rate_cmd("100").expect("Failed to set conversion rate.");
//     io::stdout().write_all(&output.stdout).unwrap();
//     io::stdout().write_all(&output.stderr).unwrap();
//     assert!(output.status.success());

//     // Add assets and liabilities such that total asset amount > total liabiliity amount
//     let output =
//       add_asset_or_liability_cmd("hidden_asset", "0", "10").expect("Failed to add public asset.");
//     io::stdout().write_all(&output.stdout).unwrap();
//     io::stdout().write_all(&output.stderr).unwrap();
//     assert!(output.status.success());

//     let output =
//       add_asset_or_liability_cmd("hidden_asset", "1", "200").expect("Failed to add hidden asset.");
//     io::stdout().write_all(&output.stdout).unwrap();
//     io::stdout().write_all(&output.stderr).unwrap();
//     assert!(output.status.success());

//     let output = add_asset_or_liability_cmd("hidden_liability","0","30")
//                                       .expect("Failed to add public liability.");
//     io::stdout().write_all(&output.stdout).unwrap();
//     io::stdout().write_all(&output.stderr).unwrap();
//     assert!(output.status.success());

//     let output = add_asset_or_liability_cmd("hidden_liability","1","40")
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
//     let output = add_asset_or_liability_cmd("hidden_liability","1","160")
//     .expect("Failed to add hidden liability.");
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
//       add_asset_or_liability_cmd("public_asset", "0", "30").expect("Failed to add public asset.");
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
//   }
// }
