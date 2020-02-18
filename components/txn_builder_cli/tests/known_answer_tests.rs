#![deny(warnings)]
use ledger::data_model::AssetTypeCode;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Output};
use std::str::from_utf8;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrKeyPair;

extern crate exitcode;

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

#[cfg(test)]
fn get_findora_dir() -> String {
  let findora_dir = {
    let home_dir = dirs::home_dir().unwrap_or_else(|| Path::new(".").to_path_buf());
    format!("{}/.findora", home_dir.to_str().unwrap_or("./.findora"))
  };

  findora_dir
}

#[cfg(test)]
fn remove_txn_dir() {
  fs::remove_dir_all(format!("{}/txn", get_findora_dir())).unwrap();
}

#[cfg(test)]
fn remove_keypair_dir() {
  fs::remove_dir_all(format!("{}/keypair", get_findora_dir())).unwrap();
}

#[cfg(test)]
fn remove_pubkey_dir() {
  fs::remove_dir_all(format!("{}/pubkey", get_findora_dir())).unwrap();
}

#[cfg(test)]
fn remove_values_dir() {
  fs::remove_dir_all(format!("{}/values", get_findora_dir())).unwrap();
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
                       .args(&["--key_pair", key_pair_path])
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
                       .args(&["--key_pair", key_pair_path])
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
                       .args(&["--key_pair", key_pair_path])
                       .args(&["add", "transfer_asset"])
                       .args(&["--sids_path", sids_path])
                       .args(&["--blind_asset_record_paths", blind_asset_record_paths])
                       .args(&["--input_amounts", input_amounts])
                       .args(&["--output_amounts", output_amounts])
                       .args(&["--address_paths", address_paths])
                       .output()
}

#[cfg(test)]
fn submit(txn_builder_path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .arg("submit")
                       //  .arg("--http")
                       .output()
}

// Helper functions: load funds
#[cfg(test)]
fn _store_key_pair_and_pub_key_to_files(kay_pair_path: &str,
                                        pub_key_path: &str)
                                        -> std::io::Result<()> {
  let mut prng = ChaChaRng::from_seed([0u8; 32]);
  let key_pair = XfrKeyPair::generate(&mut prng);
  fs::write(&kay_pair_path, key_pair.zei_to_bytes())?;
  fs::write(&pub_key_path, key_pair.get_pk_ref().as_bytes())?;
  Ok(())
}

#[cfg(test)]
fn _load_funds(txn_builder_path: &str,
               issuer_key_pair_path: &str,
               sid_pre: &str,
               sid_new: &str,
               blind_asset_record_pre_path: &str,
               blind_asset_record_new_path: &str,
               recipient_key_pair_path: &str,
               amount: &str,
               token_code: &str,
               sequence_number: &str)
               -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["--key_pair", issuer_key_pair_path])
                       .arg("load_funds")
                       .args(&["--sid_pre", sid_pre])
                       .args(&["--sid_new", sid_new])
                       .args(&["--blind_asset_record_pre_path", blind_asset_record_pre_path])
                       .args(&["--blind_asset_record_new_path", blind_asset_record_new_path])
                       .args(&["--recipient_key_pair_path", recipient_key_pair_path])
                       .args(&["--amount", amount])
                       .args(&["--token_code", token_code])
                       .args(&["--sequence_number", sequence_number])
                       //  .arg("--http")
                       .output()
}

//
// No path
//
#[test]
fn test_no_path() {
  // Create transaction builder
  let output = create_no_path().expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Generate key pair
  let output = keygen_no_path().expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Generate public key
  let output = pubkeygen_no_path().expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Store sids
  let output = store_sids_no_path("1,2,4").expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Store blind asset record
  let pubkeygen_path = "pub_no_bar_path";
  pubkeygen_with_path(pubkeygen_path).expect("Failed to generate public key");

  let output = store_blind_asset_record_no_path("10", "0000000000000000", pubkeygen_path).expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file(pubkeygen_path).unwrap();
  assert!(output.status.success());

  remove_txn_dir();
  remove_keypair_dir();
  remove_pubkey_dir();
  remove_values_dir();
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

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
  assert!(from_utf8(&output.stdout).unwrap().contains(&"Subcommand missing or not recognized. Try --help".to_owned()));
}

#[test]
fn test_store_no_args() {
  let output = Command::new(COMMAND).arg("store")
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
  assert!(from_utf8(&output.stdout).unwrap().contains(&"Subcommand missing or not recognized. Try store --help".to_owned()));
}

#[test]
fn test_add_no_args() {
  keygen_no_path().expect("Failed to generate key pair");

  let output = Command::new(COMMAND).arg("add")
                                    .output()
                                    .expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  remove_keypair_dir();

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
  assert!(from_utf8(&output.stdout).unwrap().contains(&"Subcommand missing or not recognized. Try add --help".to_owned()));
}

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

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
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
fn test_create_with_name() {
  // Create transaction builder
  let output = create_with_path("txn_builder").expect("Failed to create transaction builder");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file("txn_builder").unwrap();
  assert!(output.status.success());

  // Generate key pair
  let output = keygen_with_path("key_pair").expect("Failed to generate key pair");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file("key_pair").unwrap();
  assert!(output.status.success());

  // Generate public key
  let output = pubkeygen_with_path("pub").expect("Failed to generate public key");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file("pub").unwrap();
  assert!(output.status.success());
}

//
// Store (sids and blind asset record)
//
#[test]
fn test_store_with_path() {
  // Store sids
  let output = store_sids_with_path("sids", "1,2,4").expect("Failed to store sids");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file("sids").unwrap();
  assert!(output.status.success());

  // Store blind asset record
  let pubkeygen_path = "pub_with_bar_path";
  pubkeygen_with_path(pubkeygen_path).expect("Failed to generate public key");

  let output = store_blind_asset_record_with_path("bar", "10", "0000000000000000", pubkeygen_path).expect("Failed to store blind asset record");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file(pubkeygen_path).unwrap();
  fs::remove_file("bar").unwrap();

  assert!(output.status.success());
}

//
// Define, issue and transfer
//
#[test]
fn test_define_issue_and_transfer_with_args() {
  // Create transaction builder and key pair
  let txn_builder_file = "tb";
  let key_pair_file = "kp";
  create_with_path(txn_builder_file).expect("Failed to create transaction builder");
  keygen_with_path(key_pair_file).expect("Failed to generate key pair");

  // Define asset
  let token_code = AssetTypeCode::gen_random().to_base64();
  let output = define_asset(txn_builder_file,
                            key_pair_file,
                            &token_code,
                            "define an asset").expect("Failed to define asset");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Issue asset
  let output = issue_asset(txn_builder_file,
                           key_pair_file,
                           &token_code,
                           "1",
                           "10").expect("Failed to issue asset");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create files and generate public keys
  let files = vec!["pub1", "pub2", "pub3", "addr1", "addr2", "addr3", "s", "bar1", "bar2", "bar3"];
  for file in &files[0..6] {
    pubkeygen_with_path(file).expect("Failed to generate public key");
  }

  // Store sids and blind asset records
  store_sids_with_path(files[6], "1,2,4").expect("Failed to store sids");
  store_blind_asset_record_with_path(files[7],
                               "10",
                               &token_code,
                               files[0]).expect("Failed to store blind asset record");
  store_blind_asset_record_with_path(files[8],
                               "100",
                               &token_code,
                               files[1]).expect("Failed to store blind asset record");
  store_blind_asset_record_with_path(files[9],
                               "1000",
                               &token_code,
                               files[2]).expect("Failed to store blind asset record");

  // Transfer asset
  let output = transfer_asset(txn_builder_file,
                              key_pair_file,
                              files[6],
                              "bar1,bar2,bar3",
                              "1,2,3",
                              "1,1,4",
                              "addr1,addr2,addr3").expect("Failed to transfer asset");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file(txn_builder_file).unwrap();
  fs::remove_file(key_pair_file).unwrap();
  for file in files {
    fs::remove_file(file).unwrap();
  }

  assert!(output.status.success());
}

//
// Submit
//
#[test]
fn test_define_and_submit_with_args() {
  // Create txn builder and key pair
  let txn_builder_file = "tb_define_submit";
  let key_pair_file = "kp_define_submit";
  create_with_path(txn_builder_file).expect("Failed to create transaction builder");
  keygen_with_path(key_pair_file).expect("Failed to generate key pair");

  // Define asset
  define_asset(txn_builder_file,
               key_pair_file,
               &AssetTypeCode::gen_random().to_base64(),
               "Define an asset").expect("Failed to define asset");

  // Submit transaction
  let output = submit(txn_builder_file).expect("Failed to submit transaction");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file(txn_builder_file).unwrap();
  fs::remove_file(key_pair_file).unwrap();

  assert!(output.status.success());
}

// TODO (Keyao): I'm working on adding the "issue_and_transfer" command, which will affect the test below.
//               I'll update this test afterwards.

// //
// // Load funds
// //
// #[test]
// fn test_load_funds_with_args() {
//   // Create txn builder, key pairs, and public key
//   let txn_builder_file = "tb_load_funds";
//   let issuer_key_pair_file = "ikp";
//   let issuer_pub_key_file = "rpk";
//   let recipient_key_pair_file = "rkp";
//   let recipient_pub_key_file = "rpk";
//   create_with_path(txn_builder_file).expect("Failed to create transaction builder");
//   store_key_pair_and_pub_key_to_files(issuer_key_pair_file, issuer_pub_key_file).expect("Failed to store kay pair and public key for the issuer");
//   store_key_pair_and_pub_key_to_files(recipient_key_pair_file, recipient_pub_key_file).expect("Failed to store kay pair and public key for the recipient");

//   // Store sids and blind asset records
//   let blind_asset_record_pre_path = "bar_load_pre";
//   let blind_asset_record_new_path = "bar_load_new";
//   let token_code = AssetTypeCode::gen_random().to_base64();
//   store_blind_asset_record_with_path(blind_asset_record_pre_path,
//                                "10",
//                                &token_code,
//                                recipient_pub_key_file).expect("Failed to store blind asset record");
//   store_blind_asset_record_with_path(blind_asset_record_new_path,
//                                 "100",
//                                 &token_code,
//                                 issuer_pub_key_file).expect("Failed to store blind asset record");

//   let output = load_funds(txn_builder_file,
//                           issuer_key_pair_file,
//                           "1",
//                           "2",
//                           blind_asset_record_pre_path,
//                           blind_asset_record_new_path,
//                           recipient_key_pair_file,
//                           "100",
//                           &token_code,
//                           "1").expect("Failed to load funds");

//   io::stdout().write_all(&output.stdout).unwrap();
//   io::stdout().write_all(&output.stderr).unwrap();

//   fs::remove_file(blind_asset_record_pre_path).unwrap();
//   fs::remove_file(blind_asset_record_new_path).unwrap();
//   fs::remove_file(txn_builder_file).unwrap();
//   fs::remove_file(issuer_key_pair_file).unwrap();
//   fs::remove_file(issuer_pub_key_file).unwrap();
//   fs::remove_file(recipient_key_pair_file).unwrap();
//   fs::remove_file(recipient_pub_key_file).unwrap();

//   assert!(output.status.success());
// }
