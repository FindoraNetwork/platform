#![deny(warnings)]
use codes::exit_code::*;
use ledger::data_model::AssetTypeCode;
use std::fs;
use std::io::{self, Write};
use std::process::{Command, Output};
use std::str::from_utf8;

// TODOs:
// Derive path and command name from cwd
// Figure out how to colorize stdout and stderr

const COMMAND: &str = "../../target/debug/txn_builder_cli";

//
// Helper functions: create and store without path
//
#[cfg(test)]
fn create_no_path() -> io::Result<Output> {
  Command::new(COMMAND).arg("create").output()
}

#[cfg(test)]
fn keygen_no_path() -> io::Result<Output> {
  Command::new(COMMAND).arg("keygen").output()
}

#[cfg(test)]
fn pubkeygen_no_path() -> io::Result<Output> {
  Command::new(COMMAND).arg("pubkeygen").output()
}

#[cfg(test)]
fn store_sids_no_path(amount: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["store", "sids"])
                       .args(&["--indices", amount])
                       .output()
}

#[cfg(test)]
fn store_blind_asset_record_no_path(amount: &str,
                                    asset_type: &str,
                                    pub_key_path: &str)
                                    -> io::Result<Output> {
  Command::new(COMMAND).args(&["store", "blind_asset_record"])
                       .args(&["--amount", amount])
                       .args(&["--asset_type", asset_type])
                       .args(&["--pub_key_path", pub_key_path])
                       .output()
}

//
// Helper functions: create and store with path
//
#[cfg(test)]
fn create_with_path(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["create", "--name", path])
                       .output()
}

#[cfg(test)]
fn create_overwrite_path(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["create", "--name", path])
                       .arg("--force")
                       .output()
}

#[cfg(test)]
fn keygen_with_path(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["keygen", "--name", path])
                       .output()
}

#[cfg(test)]
fn pubkeygen_with_path(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["pubkeygen", "--name", path])
                       .output()
}

#[cfg(test)]
fn store_sids_with_path(path: &str, amount: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["store", "sids"])
                       .args(&["--path", path])
                       .args(&["--indices", amount])
                       .output()
}

#[cfg(test)]
fn store_blind_asset_record_with_path(path: &str,
                                      amount: &str,
                                      asset_type: &str,
                                      pub_key_path: &str)
                                      -> io::Result<Output> {
  Command::new(COMMAND).args(&["store", "blind_asset_record"])
                       .args(&["--path", path])
                       .args(&["--amount", amount])
                       .args(&["--asset_type", asset_type])
                       .args(&["--pub_key_path", pub_key_path])
                       .output()
}

//
// Helper functions: define, issue and transfer
//
#[cfg(test)]
fn define_asset(txn_builder_path: &str,
                key_pair_path: &str,
                token_code: &str,
                memo: &str)
                -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["--keys", key_pair_path])
                       .args(&["add", "define_asset"])
                       .args(&["--token_code", token_code])
                       .args(&["--memo", memo])
                       .output()
}

#[cfg(test)]
fn issue_asset(txn_builder_path: &str,
               key_pair_path: &str,
               token_code: &str,
               sequence_number: &str,
               amount: &str)
               -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["--keys", key_pair_path])
                       .args(&["add", "issue_asset"])
                       .args(&["--token_code", token_code])
                       .args(&["--sequence_number", sequence_number])
                       .args(&["--amount", amount])
                       .output()
}

#[cfg(test)]
fn transfer_asset(txn_builder_path: &str,
                  key_pair_path: &str,
                  sids_path: &str,
                  blind_asset_record_paths: &str,
                  input_amounts: &str,
                  output_amounts: &str,
                  address_paths: &str)
                  -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["--keys", key_pair_path])
                       .args(&["add", "transfer_asset"])
                       .args(&["--sids_path", sids_path])
                       .args(&["--blind_asset_record_paths", blind_asset_record_paths])
                       .args(&["--input_amounts", input_amounts])
                       .args(&["--output_amounts", output_amounts])
                       .args(&["--address_paths", address_paths])
                       .output()
}

#[cfg(test)]
fn _submit(txn_builder_path: &str, host: &str, port: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .arg("submit")
                       .arg("--http")
                       .args(&["--host", host])
                       .args(&["--port", port])
                       .output()
}

//
// Subcommand or argument missing
// Note: Not all cases are tested
//
#[test]
fn test_call_no_args() {
  let output = Command::new(COMMAND).output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(USAGE));
  assert!(from_utf8(&output.stdout).unwrap().contains(&"Subcommand missing or not recognized. Try --help".to_owned()));
}

#[test]
fn test_store_no_args() {
  let output = Command::new(COMMAND).arg("store")
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(USAGE));
  assert!(from_utf8(&output.stdout).unwrap().contains(&"Subcommand missing or not recognized. Try store --help".to_owned()));
}

/* FIXME
#[test]
fn test_add_no_args() {
  let output = Command::new(COMMAND).arg("add")
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(USAGE));
  assert!(from_utf8(&output.stdout).unwrap().contains(&"Subcommand missing or not recognized. Try add --help".to_owned()));
}
 */

//
// "help" arg
// Note: Not all cases with "help" arg are tested
//
#[test]
fn test_call_with_help() {
  let output = Command::new(COMMAND).arg("help")
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_create_with_help() {
  let output = Command::new(COMMAND).args(&["create", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_keygen_with_help() {
  let output = Command::new(COMMAND).args(&["keygen", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
}

#[test]
fn test_pubkeygen_with_help() {
  let output = Command::new(COMMAND).args(&["pubkeygen", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_add_with_help() {
  let output = Command::new(COMMAND).args(&["add", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_define_asset_with_help() {
  let output = Command::new(COMMAND).args(&["add", "define_asset", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_issue_asset_with_help() {
  let output = Command::new(COMMAND).args(&["add", "issue_asset", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_transfer_asset_with_help() {
  let output = Command::new(COMMAND).args(&["add", "transfer_asset", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_submit_with_help() {
  let output = Command::new(COMMAND).args(&["submit", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

//
// File creation (txn builder, key pair, and public key)
//
#[test]
fn test_invalid_valid_overwrite_and_rename_path() {
  // Invalid path
  let output = create_with_path(".").expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(USAGE));
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Is directory".to_owned()));

  // Valid path
  let path = "valid_path";
  let output = create_with_path(path).expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Overwrite existing file
  let output = create_overwrite_path(path).expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Rename existing file
  let output = create_with_path(path).expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  fs::remove_file("valid_path").unwrap();
  fs::remove_file("valid_path.0").unwrap();
}

#[test]
fn test_create_no_path() {
  let output = create_no_path().expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
}

#[test]
fn test_keygen_no_path() {
  let output = keygen_no_path().expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
}

#[test]
fn test_pubkeygen_no_path() {
  let output = pubkeygen_no_path().expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
}

#[test]
fn test_create_with_name() {
  let output = create_with_path("txn_builder").expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file("txn_builder").unwrap();
}

#[test]
fn test_keygen_with_name() {
  let output = keygen_with_path("key_pair").expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file("key_pair").unwrap();
}

#[test]
fn test_pubkeygen_with_name() {
  let output = pubkeygen_with_path("pub").expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file("pub").unwrap();
}

//
// Store (sids and blind asset record)
//
#[test]
fn test_store_sids_no_path() {
  let output = store_sids_no_path("1,2,4").expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
}

#[test]
fn test_store_blind_asset_record_no_path() {
  let pubkeygen_path = "pub_no_bar_path";
  pubkeygen_with_path(pubkeygen_path).expect("Failed to generate public key");

  let output = store_blind_asset_record_no_path("10", "0000000000000000", pubkeygen_path).expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file(pubkeygen_path).unwrap();
}

#[test]
fn test_store_sids_with_path() {
  let output = store_sids_with_path("sids", "1,2,4").expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file("sids").unwrap();
}

#[test]
fn test_store_blind_asset_record_with_path() {
  let pubkeygen_path = "pub_with_bar_path";
  pubkeygen_with_path(pubkeygen_path).expect("Failed to generate public key");

  let output = store_blind_asset_record_with_path("bar", "10", "0000000000000000", pubkeygen_path).expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file(pubkeygen_path).unwrap();
  fs::remove_file("bar").unwrap();
}

//
// Define, issue and transfer
//
#[test]
fn test_define_asset_with_args() {
  // Create txn builder and key pair
  let txn_builder_file = "tb_define";
  let key_pair_file = "kp_define";
  create_with_path(txn_builder_file).expect("Failed to create transaction builder");
  keygen_with_path(key_pair_file).expect("Failed to generate key pair");

  // Define asset
  let output = define_asset(txn_builder_file,
                            key_pair_file,
                            &AssetTypeCode::gen_random().to_base64(),
                            "define an asset").expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  fs::remove_file(txn_builder_file).unwrap();
  fs::remove_file(key_pair_file).unwrap();
}

#[test]
fn test_issue_asset_with_args() {
  // Create txn builder and key pair
  let txn_builder_file = "tb_issue";
  let key_pair_file = "kp_issue";
  create_with_path(txn_builder_file).expect("Failed to create transaction builder");
  keygen_with_path(key_pair_file).expect("Failed to generate key pair");

  // Issue asset
  let output = issue_asset(txn_builder_file,
                           key_pair_file,
                           &AssetTypeCode::gen_random().to_base64(),
                           "1",
                           "10").expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  fs::remove_file(txn_builder_file).unwrap();
  fs::remove_file(key_pair_file).unwrap();
}

#[test]
fn test_transfer_asset_with_args() {
  // Create txn builder, key pair, and public keys
  let txn_builder_file = "tb_transfer";
  let key_pair_file = "kp_transfer";
  create_with_path(txn_builder_file).expect("Failed to create transaction builder");
  keygen_with_path(key_pair_file).expect("Failed to generate key pair");

  let files = vec!["pub1", "pub2", "pub3", "addr1", "addr2", "addr3", "s", "bar1", "bar2", "bar3"];
  for file in &files[0..6] {
    pubkeygen_with_path(file).expect("Failed to generate public key");
  }

  // Store sids and blind asset records
  store_sids_with_path(files[6], "1,2,4").expect("Failed to store sids");
  store_blind_asset_record_with_path(files[7],
                           "10",
                           "0000000000000000",
                           files[0]).expect("Failed to store blind asset record");
  store_blind_asset_record_with_path(files[8],
                           "100",
                           "0000000000000000",
                           files[1]).expect("Failed to store blind asset record");
  store_blind_asset_record_with_path(files[9],
                           "1000",
                           "0000000000000000",
                           files[2]).expect("Failed to store blind asset record");

  // Transfer asset
  let output = transfer_asset(txn_builder_file,
                              key_pair_file,
                              files[6],
                              "bar1,bar2,bar3",
                              "1,2,3",
                              "1,1,4",
                              "addr1,addr2,addr3").expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  fs::remove_file(txn_builder_file).unwrap();
  fs::remove_file(key_pair_file).unwrap();
  for file in files {
    fs::remove_file(file).unwrap();
  }
}

// TODO (Keyao): Tests below don't pass. Fix them.

// //
// // Submit
// //
// #[test]
// fn test_define_and_submit() {
//   // Create txn builder and key pair
//   let txn_builder_file = "tb_define_submit";
//   let key_pair_file = "kp_define_submit";
//   create_with_path(txn_builder_file).expect("Failed to create transaction builder");
//   keygen_with_path(key_pair_file).expect("Failed to generate key pair");

//   // Define asset
//   define_asset(txn_builder_file,
//                key_pair_file,
//                &AssetTypeCode::gen_random().to_base64(),
//                "Define an asset").expect("Failed to define asset");

//   // Submit transaction
//   let output =
//     submit(txn_builder_file, "testnet.findora.org", "8669").expect("Failed to execute process");

//   io::stdout().write_all(&output.stdout).unwrap();
//   io::stdout().write_all(&output.stderr).unwrap();

//   assert!(output.status.success());

//   fs::remove_file(txn_builder_file).unwrap();
//   fs::remove_file(key_pair_file).unwrap();
// }

// #[test]
// fn test_submit_with_args() {
//   // Create txn builder and key pair
//   let txn_builder_file = "tb_submit";
//   let key_pair_file = "kp_submit";
//   create_with_path(txn_builder_file).expect("Failed to create transaction builder");
//   keygen_with_path(key_pair_file).expect("Failed to generate key pair");

//   let files = vec!["pub", "addr", "sid", "bar"];
//   for file in &files[0..2] {
//     pubkeygen_with_path(file).expect("Failed to generate public key");
//   }

//   // Store sids and blind asset records
//   let token_code = AssetTypeCode::gen_random().to_base64();
//   store_sids_with_path(files[2], "1").expect("Failed to store sids");
//   store_blind_asset_record_with_path(files[3],
//                              "10",
//                              &token_code,
//                              files[0]).expect("Failed to store blind asset record");

//   // Define asset
//   define_asset(txn_builder_file,
//                key_pair_file,
//                &token_code,
//                "Define an asset").expect("Failed to define asset");

//   let host = "testnet.findora.org";
//   let port = "8669";

//   let output = submit(txn_builder_file, host, port).expect("Failed to execute process");

//   io::stdout().write_all(&output.stdout).unwrap();
//   io::stdout().write_all(&output.stderr).unwrap();

//   assert!(output.status.success());

//   // Issue asset
//   issue_asset(txn_builder_file,
//               key_pair_file,
//               &token_code,
//               "0",
//               "100").expect("Failed to issue fiat asset");

//   let output = submit(txn_builder_file, host, port).expect("Failed to execute process");

//   io::stdout().write_all(&output.stdout).unwrap();
//   io::stdout().write_all(&output.stderr).unwrap();

//   assert!(output.status.success());

//   // Transfer asset

//   transfer_asset(txn_builder_file,
//                  key_pair_file,
//                  files[2],
//                  files[3],
//                  "10",
//                  "10",
//                  files[1]).expect("Failed to transfer asset");

//   let output = submit(txn_builder_file, host, port).expect("Failed to execute process");

//   io::stdout().write_all(&output.stdout).unwrap();
//   io::stdout().write_all(&output.stderr).unwrap();

//   assert!(output.status.success());

//   fs::remove_file(txn_builder_file).unwrap();
//   fs::remove_file(key_pair_file).unwrap();
// }
