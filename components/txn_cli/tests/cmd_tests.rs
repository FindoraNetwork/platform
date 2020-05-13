#![deny(warnings)]
use ledger::data_model::AssetTypeCode;
use ledger_standalone::LedgerStandalone;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Output};
use std::str::from_utf8;
use tempfile::tempdir;

extern crate exitcode;

// TODOs:
// Derive path and command name from cwd
// Figure out how to colorize stdout and stderr

#[cfg(debug_assertions)]
const COMMAND: &str = "../../target/debug/txn_cli";

#[cfg(not(debug_assertions))]
const COMMAND: &str = "../../target/release/txn_cli";

//
// Helper functions: sign up an account
//
#[cfg(test)]
fn sign_up_asset_issuer(dir: &str, name: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--dir", dir])
                       .args(&["asset_issuer", "sign_up"])
                       .args(&["--name", name])
                       .output()
}

#[cfg(test)]
fn sign_up_credential_issuer(dir: &str, name: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--dir", dir])
                       .args(&["credential_issuer", "sign_up"])
                       .args(&["--name", name])
                       .output()
}

#[cfg(test)]
fn sign_up_lender(dir: &str, name: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--dir", dir])
                       .args(&["lender", "sign_up"])
                       .args(&["--name", name])
                       .output()
}

#[cfg(test)]
fn sign_up_borrower(dir: &str, name: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--dir", dir])
                       .args(&["borrower", "sign_up"])
                       .args(&["--name", name])
                       .output()
}

//
// Helper functions: create and store without path
//
#[cfg(test)]
fn create_txn_builder_no_path() -> io::Result<Output> {
  Command::new(COMMAND).arg("create_txn_builder").output()
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

//
// Helper functions: create and store with path
//
#[cfg(test)]
fn create_txn_builder_with_path(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).arg("create_txn_builder")
                       .args(&["--name", path])
                       .output()
}

#[cfg(test)]
fn create_txn_builder_overwrite_path(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).arg("create_txn_builder")
                       .args(&["--name", path])
                       .arg("--force")
                       .output()
}

#[cfg(test)]
fn store_sids_with_path(file: &str, indices: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["asset_issuer", "store_sids"])
                       .args(&["--file", file])
                       .args(&["--indices", indices])
                       .output()
}

#[cfg(test)]
fn store_memos_with_confidential_amount(dir: &str,
                                        id: &str,
                                        amount: &str,
                                        token_code: &str,
                                        file: &str)
                                        -> io::Result<Output> {
  Command::new(COMMAND).args(&["--dir", dir])
                       .args(&["asset_issuer", "--id", id])
                       .arg("store_memos")
                       .args(&["--amount", amount])
                       .arg("--confidential_amount")
                       .args(&["--token_code", token_code])
                       .args(&["--file", file])
                       .output()
}

#[cfg(test)]
fn trace_and_verify_asset(dir: &str,
                          id: &str,
                          memo_file: &str,
                          expected_amount: &str)
                          -> io::Result<Output> {
  Command::new(COMMAND).args(&["--dir", dir])
                       .args(&["asset_issuer", "--id", id])
                       .arg("trace_and_verify_asset")
                       .args(&["--memo_file", memo_file])
                       .args(&["--expected_amount", expected_amount])
                       .output()
}

#[cfg(test)]
fn define_asset(dir: &str,
                txn_builder_path: &str,
                issuer_id: &str,
                token_code: &str,
                memo: &str)
                -> io::Result<Output> {
  Command::new(COMMAND).args(&["--dir", dir])
                       .args(&["--txn", txn_builder_path])
                       .args(&["asset_issuer", "--id", issuer_id])
                       .arg("define_asset")
                       .args(&["--token_code", token_code])
                       .args(&["--memo", memo])
                       .output()
}

#[cfg(test)]
fn issue_and_transfer_asset_confidential(txn_builder_path: &str,
                                         issuer_id: &str,
                                         recipient_id: &str,
                                         amount: &str,
                                         token_code: &str)
                                         -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["asset_issuer", "--id", issuer_id])
                       .arg("issue_and_transfer_asset")
                       .args(&["--recipient", recipient_id])
                       .args(&["--amount", amount])
                       .args(&["--token_code", token_code])
                       .args(&["--confidential_amount", "--confidential_asset"])
                       .output()
}

// Helper functions: submit transaction
// Note:
// Since http://localhost is used instead of https://testnet.findora.org,
// make sure the standalone ledger is running before calling a function that will submit a transaction:
// ```
// let ledger_standalone = LedgerStandalone::new();
// ledger_standalone.poll_until_ready().unwrap();
// ```

#[cfg(test)]
fn submit(txn_builder_path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .arg("submit")
                       .args(&["--http", "--localhost"])
                       .output()
}

//
// No path
//
#[test]
fn test_create_users() {
  let tmp_dir = tempdir().unwrap();
  let dir = tmp_dir.path().to_str().unwrap();

  // Create an asset issuer
  let output = sign_up_asset_issuer(dir, "Issuer AI").expect("Failed to create an asset issuer");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create a credential issuer
  let output =
    sign_up_credential_issuer(dir, "Issuer CI").expect("Failed to create a credential issuer");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create a lender
  let output = sign_up_lender(dir, "Lender L").expect("Failed to create a lender");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create a borrower
  let output = sign_up_borrower(dir, "Borrower B").expect("Failed to create a borrower");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  tmp_dir.close().unwrap();
}

#[test]
fn test_create_txn_builder_no_path() {
  // Create transaction builder
  let output = create_txn_builder_no_path().expect("Failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  remove_txn_dir();
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
fn test_create_txn_builder_with_help() {
  let output = Command::new(COMMAND).args(&["create_txn_builder", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_define_asset_with_help() {
  let output = Command::new(COMMAND).args(&["asset_issuer", "define_asset", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_issue_asset_with_help() {
  let output = Command::new(COMMAND).args(&["asset_issuer", "issue_asset", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

#[test]
fn test_transfer_asset_with_help() {
  let output = Command::new(COMMAND).args(&["asset_issuer", "transfer_asset", "--help"])
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
fn test_create_txn_builder_with_name() {
  // Create transaction builder
  let output =
    create_txn_builder_with_path("txn_builder").expect("Failed to create transaction builder");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file("txn_builder").unwrap();
  assert!(output.status.success());
}

//
// Store sids
//
#[test]
fn test_store_sids_with_path() {
  // Store sids
  let output = store_sids_with_path("sids", "1,2,4").expect("Failed to store sids");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file("sids").unwrap();
  assert!(output.status.success());
}

#[test]
fn test_issue_transfer_trace_and_submit_with_args() {
  let tmp_dir = tempdir().unwrap();
  let dir = tmp_dir.path().to_str().unwrap();
  let txn_builder_buf = tmp_dir.path().join("tb_issue_transfer_args");
  let txn_builder_file = txn_builder_buf.to_str().unwrap();

  let ledger_standalone = LedgerStandalone::new();

  // Create txn builder and key pairs
  create_txn_builder_with_path(txn_builder_file).expect("Failed to create transaction builder");

  // Define token code
  let token_code = AssetTypeCode::gen_random().to_base64();

  // Define asset
  define_asset(dir,
               txn_builder_file,
               "0",
               &token_code,
               "Define an asset").expect("Failed to define asset");
  ledger_standalone.poll_until_ready().unwrap();
  submit(txn_builder_file).expect("Failed to submit transaction");

  // Issue and transfer
  let amount = "1000";
  issue_and_transfer_asset_confidential(txn_builder_file,
                           "0",
                           "0",
                           amount,
                           &token_code).expect("Failed to issue and transfer asset");

  // Store tracer and owner memos
  let memo_file = "memos_issue_transfer_and_submit";
  let output =
  store_memos_with_confidential_amount(dir, "0", amount, &token_code, memo_file).expect("Failed to store memos");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Trace the asset and verify the amount
  let output =
    trace_and_verify_asset(dir, "0", memo_file, amount).expect("Failed to trace the asset");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file(memo_file).unwrap();
  assert!(output.status.success());

  // Submit transaction
  ledger_standalone.poll_until_ready().unwrap();
  let output = submit(txn_builder_file).expect("Failed to submit transaction");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  tmp_dir.close().unwrap();
}
