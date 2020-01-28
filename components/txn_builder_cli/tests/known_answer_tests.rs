use std::fs;
use std::io::{self, Write};
use std::process::{Command, Output};
use std::str::from_utf8;

// TODOs:
// Derive path and command name from cwd
// Figure out how to colorize stdout and stderr

const COMMAND: &str = "../../target/debug/txn_builder_cli";

#[cfg(test)]
fn create(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["create", "--name", path])
                       .output()
}

#[cfg(test)]
fn keygen(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["keygen", "--name", path])
                       .output()
}

#[cfg(test)]
fn pubkeygen(path: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["pubkeygen", "--name", path])
                       .output()
}

#[cfg(test)]
fn store_sids(path: &str, amount: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["store", "sids"])
                       .args(&["--path", path])
                       .args(&["--indices", amount])
                       .output()
}

#[cfg(test)]
fn store_blind_asset_record(path: &str,
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

// No subcommand
#[test]
fn test_call_no_args() {
  let output = Command::new(COMMAND).output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(from_utf8(&output.stderr[..]).unwrap()
                                       .contains("Subcommand missing or not recognized"));
}

#[test]
fn test_call_with_help() {
  let output = Command::new(COMMAND).arg("help")
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

// Create
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
fn test_create_with_name() {
  let output = create("txn_builder").expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file("txn_builder").unwrap();
}

// Generate key pair
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
fn test_keygen_with_name() {
  let output = keygen("key_pair").expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file("key_pair").unwrap();
}

// Generate public key
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
fn test_pubkeygen_with_name() {
  let output = pubkeygen("pub").expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file("pub").unwrap();
}

// Store
#[test]
fn test_store_sids() {
  let output = store_sids("sids", "1,2,4").expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file("sids").unwrap();
}

#[test]
fn test_store_blind_asset_record() {
  pubkeygen("store_pub").expect("Failed to generate public key");

  let output = store_blind_asset_record("bar", "10", "0000000000000000", "store_pub").expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  fs::remove_file("store_pub").unwrap();
  fs::remove_file("bar").unwrap();
}

// Add
#[test]
fn test_add_with_help() {
  let output = Command::new(COMMAND).args(&["add", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

// Define asset
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
fn test_define_asset_with_args() {
  // Create txn builder and key pair
  create("tb_define").expect("Failed to create transaction builder");
  keygen("kp_define").expect("Failed to generate key pair");

  // Define asset
  let output = Command::new(COMMAND).args(&["--txn", "tb_define"])
                                    .args(&["--keys", "kp_define"])
                                    .args(&["add", "define_asset"])
                                    .args(&["--token_code", "0000000000000000"])
                                    .args(&["--allow_updates", "--traceable"])
                                    .args(&["--memo", "define an asset", "--confidential"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  fs::remove_file("tb_define").unwrap();
  fs::remove_file("kp_define").unwrap();
}

// Issue asset
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
fn test_issue_asset_with_args() {
  // Create txn builder and key pair
  create("tb_issue").expect("Failed to create transaction builder");
  keygen("kp_issue").expect("Failed to generate key pair");

  // Issue asset
  let output = Command::new(COMMAND).args(&["--txn", "tb_issue"])
                                    .args(&["--keys", "kp_issue"])
                                    .args(&["add", "issue_asset"])
                                    .args(&["--token_code", "0000000000000000"])
                                    .args(&["--sequence_number", "1"])
                                    .args(&["--amount", "100"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  fs::remove_file("tb_issue").unwrap();
  fs::remove_file("kp_issue").unwrap();
}

// Transfer asset
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
fn test_transfer_asset_with_args() {
  // Create txn builder, key pair, and public keys
  create("tb_transfer").expect("Failed to create transaction builder");
  keygen("kp_transfer").expect("Failed to generate key pair");

  let files = vec!["pub1", "pub2", "pub3", "addr1", "addr2", "addr3", "s", "bar1", "bar2", "bar3"];
  for file in &files[0..6] {
    pubkeygen(file).expect("Failed to generate public key");
  }

  // Store sids and blind asset records
  store_sids(files[6], "1,2,4").expect("Failed to store sids");
  store_blind_asset_record(files[7],
                           "10",
                           "0000000000000000",
                           files[0]).expect("Failed to store blind asset record");
  store_blind_asset_record(files[8],
                           "100",
                           "0000000000000000",
                           files[1]).expect("Failed to store blind asset record");
  store_blind_asset_record(files[9],
                           "1000",
                           "0000000000000000",
                           files[2]).expect("Failed to store blind asset record");

  // Transfer asset
  let output = Command::new(COMMAND).args(&["--txn", "tb_transfer"])
                                    .args(&["--keys", "kp_transfer"])
                                    .args(&["add", "transfer_asset"])
                                    .args(&["--sids_path", files[6]])
                                    .args(&["--blind_asset_record_paths", "bar1,bar2,bar3"])
                                    .args(&["--input_amounts", "1,2,3"])
                                    .args(&["--output_amounts", "1,1,4"])
                                    .args(&["--address_paths", "addr1,addr2,addr3"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  fs::remove_file("tb_transfer").unwrap();
  fs::remove_file("kp_transfer").unwrap();
  for file in files {
    fs::remove_file(file).unwrap();
  }
}

// Submit
#[test]
fn test_submit_with_help() {
  let output = Command::new(COMMAND).args(&["submit", "--help"])
                                    .output()
                                    .expect("failed to execute process");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}
