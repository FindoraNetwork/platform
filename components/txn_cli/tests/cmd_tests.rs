#![deny(warnings)]
use std::fs;
use std::io::{self, Write};
use std::process::{Command, Output};
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

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
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

  assert!(output.status.success());
  fs::remove_file("txn_builder").unwrap();
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

  assert!(output.status.success());
}
