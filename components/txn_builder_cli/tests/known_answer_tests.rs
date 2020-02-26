#![deny(warnings)]
use ledger::data_model::AssetTypeCode;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Output};
use std::str::from_utf8;

extern crate exitcode;

// TODOs:
// Derive path and command name from cwd
// Figure out how to colorize stdout and stderr

const COMMAND: &str = "../../target/debug/txn_builder_cli";
const DATA_FILE: &str = "data.json";

//
// Helper functions: create and store without path
//
#[cfg(test)]
fn create_user(user_type: &str, name: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["create", "user"])
                       .args(&["--type", user_type])
                       .args(&["--name", name])
                       .output()
}

#[cfg(test)]
fn create_loan(lender: &str, borrower: &str, amount: &str, duration: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["create", "loan"])
                       .args(&["--lender", lender])
                       .args(&["--borrower", borrower])
                       .args(&["--amount", amount])
                       .args(&["--duration", duration])
                       .output()
}

#[cfg(test)]
fn create_txn_builder_no_path() -> io::Result<Output> {
  Command::new(COMMAND).args(&["create", "txn_builder"])
                       .output()
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
fn create_txn_builder_with_path(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["create", "txn_builder"])
                       .args(&["--name", path])
                       .output()
}

#[cfg(test)]
fn create_txn_builder_overwrite_path(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["create", "txn_builder"])
                       .args(&["--name", path])
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
                issuer_id: &str,
                token_code: &str,
                memo: &str)
                -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["add", "define_asset"])
                       .args(&["--issuer", issuer_id])
                       .args(&["--token_code", token_code])
                       .args(&["--memo", memo])
                       .output()
}

#[cfg(test)]
fn define_fiat_asset(txn_builder_path: &str, issuer_id: &str, memo: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["add", "define_asset", "--fiat"])
                       .args(&["--issuer", issuer_id])
                       .args(&["--memo", memo])
                       .output()
}

#[cfg(test)]
fn issue_asset(txn_builder_path: &str,
               id: &str,
               token_code: &str,
               amount: &str)
               -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["add", "issue_asset"])
                       .args(&["--issuer", id])
                       .args(&["--token_code", token_code])
                       .args(&["--amount", amount])
                       .output()
}

#[cfg(test)]
fn transfer_asset(txn_builder_path: &str,
                  id: &str,
                  sids_path: &str,
                  blind_asset_record_paths: &str,
                  input_amounts: &str,
                  output_amounts: &str,
                  address_paths: &str)
                  -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["add", "transfer_asset"])
                       .args(&["--issuer", id])
                       .args(&["--sids_path", sids_path])
                       .args(&["--blind_asset_record_paths", blind_asset_record_paths])
                       .args(&["--input_amounts", input_amounts])
                       .args(&["--output_amounts", output_amounts])
                       .args(&["--address_paths", address_paths])
                       .output()
}

#[cfg(test)]
fn issue_and_transfer_asset(txn_builder_path: &str,
                            issuer_id: &str,
                            recipient_id: &str,
                            amount: &str,
                            token_code: &str)
                            -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["add", "issue_and_transfer_asset"])
                       .args(&["--issuer", issuer_id])
                       .args(&["--recipient", recipient_id])
                       .args(&["--amount", amount])
                       .args(&["--token_code", token_code])
                       .output()
}

// Helper functions: submit transaction
// Note: http://localhost is used instead of https://testnet.findora.org

#[cfg(test)]
fn submit(txn_builder_path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .arg("submit")
                       .args(&["--http", "--localhost"])
                       .output()
}

// Helper function: load funds
#[cfg(test)]
fn load_funds(txn_builder_path: &str,
              issuer_id: &str,
              recipient_id: &str,
              amount: &str)
              -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .arg("load_funds")
                       .args(&["--issuer", issuer_id])
                       .args(&["--recipient", recipient_id])
                       .args(&["--amount", amount])
                       .args(&["--http", "--localhost"])
                       .output()
}

// Helper functions: initiate and pay loan
#[cfg(test)]
fn activate_loan(txn_builder_path: &str, loan_id: &str, issuer_id: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .arg("activate_loan")
                       .args(&["--loan", loan_id])
                       .args(&["--issuer", issuer_id])
                       .args(&["--http", "--localhost"])
                       .output()
}

#[cfg(test)]
fn pay_loan(txn_builder_path: &str, loan_id: &str, amount: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .arg("pay_loan")
                       .args(&["--loan", loan_id])
                       .args(&["--amount", amount])
                       .args(&["--http", "--localhost"])
                       .output()
}

//
// No path
//
#[test]
fn test_create_users() {
  // Create an issuer
  let output = create_user("issuer", "Issuer I").expect("Failed to create an issuer");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create an lender
  let output = create_user("lender", "Lender L").expect("Failed to create a lender");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create a borrower
  let output = create_user("borrower", "Borrower B").expect("Failed to create a borrower");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  let _ = fs::remove_file(DATA_FILE);
}

#[test]
fn test_no_path() {
  // Create transaction builder
  let output = create_txn_builder_no_path().expect("Failed to execute process");

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
  let output = create_txn_builder_with_path(".").expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Is directory".to_owned()));

  // Valid path
  let path = "valid_path";
  let output = create_txn_builder_with_path(path).expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Overwrite existing file
  let output = create_txn_builder_overwrite_path(path).expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Rename existing file
  let output = create_txn_builder_with_path(path).expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  fs::remove_file("valid_path").unwrap();
  fs::remove_file("valid_path.0").unwrap();
}

#[test]
fn test_create_with_name() {
  // Create transaction builder
  let output =
    create_txn_builder_with_path("txn_builder").expect("Failed to create transaction builder");

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
  create_txn_builder_with_path(txn_builder_file).expect("Failed to create transaction builder");

  // Define asset
  let token_code = AssetTypeCode::gen_random().to_base64();
  let output = define_asset(txn_builder_file,
                            "0",
                            &token_code,
                            "define an asset").expect("Failed to define asset");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Issue asset
  let output =
    issue_asset(txn_builder_file, "0", &token_code, "10").expect("Failed to issue asset");

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
                              "0",
                              files[6],
                              "bar1,bar2,bar3",
                              "1,2,3",
                              "1,1,4",
                              "addr1,addr2,addr3").expect("Failed to transfer asset");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  let _ = fs::remove_file(DATA_FILE);
  fs::remove_file(txn_builder_file).unwrap();
  for file in files {
    fs::remove_file(file).unwrap();
  }

  assert!(output.status.success());
}

//
// Compose transaction and submit
//
#[test]
#[ignore]
fn test_define_and_submit_with_args() {
  // Create txn builder and key pair
  let txn_builder_file = "tb_define_submit";
  create_txn_builder_with_path(txn_builder_file).expect("Failed to create transaction builder");

  // Define asset
  define_asset(txn_builder_file,
               "0",
               &AssetTypeCode::gen_random().to_base64(),
               "Define an asset").expect("Failed to define asset");

  // Submit transaction
  let output = submit(txn_builder_file).expect("Failed to submit transaction");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  let _ = fs::remove_file(DATA_FILE);
  fs::remove_file(txn_builder_file).unwrap();

  assert!(output.status.success());
}

#[test]
#[ignore]
fn test_issue_transfer_and_submit_with_args() {
  let _ = fs::remove_file(DATA_FILE);

  // Create txn builder and key pairs
  let txn_builder_file = "tb_issue_transfer_args";
  create_txn_builder_with_path(txn_builder_file).expect("Failed to create transaction builder");

  // Define token code
  let token_code = AssetTypeCode::gen_random().to_base64();

  // Define asset
  define_asset(txn_builder_file,
               "0",
               &token_code,
               "Define an asset").expect("Failed to define asset");
  submit(txn_builder_file).expect("Failed to submit transaction");

  // Issue and transfer
  issue_and_transfer_asset(txn_builder_file,
                           "0",
                           "0",
                           "1000",
                           &token_code).expect("Failed to issue and transfer asset");

  // Submit transaction
  let output = submit(txn_builder_file).expect("Failed to submit transaction");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  let _ = fs::remove_file(DATA_FILE);
  fs::remove_file(txn_builder_file).unwrap();

  assert!(output.status.success());
}

#[test]
#[ignore]
fn test_load_funds_with_args() {
  // Create txn builder, key pairs, and public key
  let txn_builder_file = "tb_load_funds_args";
  create_txn_builder_with_path(txn_builder_file).expect("Failed to create transaction builder");

  // Define fiat asset
  define_fiat_asset(txn_builder_file, "0", "Define fiat asset.").expect("Failed to define fiat asset");
  submit(txn_builder_file).expect("Failed to submit transaction");

  // Load funds
  let output = load_funds(txn_builder_file, "0", "0", "500").expect("Failed to load funds");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file(txn_builder_file).unwrap();

  assert!(output.status.success());
}

#[test]
#[ignore]
fn test_create_activate_and_pay_loan_with_args() {
  let _ = fs::remove_file(DATA_FILE);

  // Create loan
  let output = create_loan("0", "0", "1500", "8").expect("Failed to create an issuer");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create txn builder, key pairs, and public key
  let txn_builder_file = "tb_activate_loan_args";
  create_txn_builder_with_path(txn_builder_file).expect("Failed to create transaction builder");

  // Initiate loan
  let output = activate_loan(txn_builder_file, "0", "0").expect("Failed to load funds");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Pay loan
  let output = pay_loan(txn_builder_file, "0", "300").expect("Failed to pay loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  let _ = fs::remove_file(DATA_FILE);
  fs::remove_file(txn_builder_file).unwrap();

  assert!(output.status.success());
}
