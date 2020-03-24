#![deny(warnings)]
use ledger::data_model::AssetTypeCode;
use ledger_standalone::LedgerStandalone;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Output};
use std::str::from_utf8;

extern crate exitcode;

// TODOs:
// Derive path and command name from cwd
// Figure out how to colorize stdout and stderr

// TODO (Keyao): Fix tests with #[ignore].
// Those tests pass individually, but occasionally fail when run with other tests.
// They take more time to complete, thus might cause data conflicts.

const COMMAND: &str = "../../target/debug/txn_builder_cli";
const DATA_FILE: &str = "data.json";

//
// Helper functions: view records
//
#[cfg(test)]
fn view_loan_all(user_type: &str, user_id: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&[user_type, "--id", user_id])
                       .arg("view_loan")
                       .output()
}

#[cfg(test)]
fn view_loan_with_loan_id(user_type: &str, user_id: &str, loan_id: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&[user_type, "--id", user_id])
                       .arg("view_loan")
                       .args(&["--loan", loan_id])
                       .output()
}

#[cfg(test)]
fn view_loan_with_filter(user_type: &str, user_id: &str, filter: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&[user_type, "--id", user_id])
                       .arg("view_loan")
                       .args(&["--filter", filter])
                       .output()
}

#[cfg(test)]
fn view_credential_all(borrower_id: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["borrower", "--id", borrower_id])
                       .arg("view_credential")
                       .output()
}

#[cfg(test)]
fn view_credential_with_credential_id(borrower_id: &str,
                                      credential_id: &str)
                                      -> io::Result<Output> {
  Command::new(COMMAND).args(&["borrower", "--id", borrower_id])
                       .arg("view_credential")
                       .args(&["--credential", credential_id])
                       .output()
}

//
// Helper functions: sign up an account
//
#[cfg(test)]
fn sign_up_asset_issuer(name: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["asset_issuer", "sign_up"])
                       .args(&["--name", name])
                       .output()
}

#[cfg(test)]
fn sign_up_credential_issuer(name: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["credential_issuer", "sign_up"])
                       .args(&["--name", name])
                       .output()
}

#[cfg(test)]
fn sign_up_lender(name: &str, min_credit_score: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["lender", "sign_up"])
                       .args(&["--name", name])
                       .args(&["--min_credit_score", min_credit_score])
                       .output()
}

#[cfg(test)]
fn sign_up_borrower(name: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["borrower", "sign_up"])
                       .args(&["--name", name])
                       .output()
}

//
// Helper functions: create and store without path
//
#[cfg(test)]
fn create_or_overwrite_credential(id: &str, attribute: &str, value: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["borrower", "--id", id])
                       .arg("create_or_overwrite_credential")
                       .args(&["--credential_issuer", "0"])
                       .args(&["--attribute", attribute])
                       .args(&["--value", value])
                       .output()
}

#[cfg(test)]
fn request_loan(lender: &str,
                borrower: &str,
                amount: &str,
                interest_per_mille: &str,
                duration: &str)
                -> io::Result<Output> {
  Command::new(COMMAND).args(&["borrower", "--id", borrower])
                       .arg("request_loan")
                       .args(&["--lender", lender])
                       .args(&["--amount", amount])
                       .args(&["--interest_per_mille", interest_per_mille])
                       .args(&["--duration", duration])
                       .output()
}

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
fn store_blind_asset_record_and_memos_nonconfidential(id: &str,
                                                      amount: &str,
                                                      token_code: &str,
                                                      file: &str)
                                                      -> io::Result<Output> {
  Command::new(COMMAND).args(&["asset_issuer", "--id", id])
                       .arg("store_blind_asset_record_and_memos")
                       .args(&["--amount", amount])
                       .args(&["--token_code", token_code])
                       .args(&["--file", file])
                       .output()
}

//
// Helper functions: define, issue and transfer
//
#[cfg(test)]
fn air_assign(txn_builder_path: &str,
              issuer_id: &str,
              address: &str,
              data: &str)
              -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["asset_issuer", "--id", issuer_id])
                       .arg("air_assign")
                       .args(&["--address", address])
                       .args(&["--data", data])
                       .output()
}

#[cfg(test)]
fn define_asset(txn_builder_path: &str,
                issuer_id: &str,
                token_code: &str,
                memo: &str)
                -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["asset_issuer", "--id", issuer_id])
                       .arg("define_asset")
                       .args(&["--token_code", token_code])
                       .args(&["--memo", memo])
                       .arg("--traceable")
                       .output()
}

#[cfg(test)]
fn issue_asset(txn_builder_path: &str,
               id: &str,
               token_code: &str,
               amount: &str)
               -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["asset_issuer", "--id", id])
                       .arg("issue_asset")
                       .args(&["--token_code", token_code])
                       .args(&["--amount", amount])
                       .output()
}

#[cfg(test)]
fn transfer_asset(txn_builder_path: &str,
                  issuer_id: &str,
                  recipient_ids: &str,
                  sids_file: &str,
                  bar_and_memo_files: &str,
                  input_amounts: &str,
                  output_amounts: &str)
                  -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["asset_issuer", "--id", issuer_id])
                       .arg("transfer_asset")
                       .args(&["--recipients", recipient_ids])
                       .args(&["--sids_file", sids_file])
                       .args(&["--blind_asset_record_and_memo_files", bar_and_memo_files])
                       .args(&["--input_amounts", input_amounts])
                       .args(&["--output_amounts", output_amounts])
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
                       .args(&["asset_issuer", "--id", issuer_id])
                       .arg("issue_and_transfer_asset")
                       .args(&["--recipient", recipient_id])
                       .args(&["--amount", amount])
                       .args(&["--token_code", token_code])
                       .arg("--confidential_amount")
                       .arg("--confidential_asset")
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

#[cfg(test)]
fn submit_and_store_sids(txn_builder_path: &str, sids_file: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .arg("submit")
                       .args(&["--sids_file", sids_file])
                       .args(&["--http", "--localhost"])
                       .output()
}

// Helper function: load funds
#[cfg(test)]
fn load_funds(txn_builder_path: &str,
              issuer_id: &str,
              borrower_id: &str,
              amount: &str)
              -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["borrower", "--id", borrower_id])
                       .arg("load_funds")
                       .args(&["--issuer", issuer_id])
                       .args(&["--amount", amount])
                       .args(&["--http", "--localhost"])
                       .output()
}

// Helper functions: initiate and pay loan
#[cfg(test)]
fn fulfill_loan(txn_builder_path: &str,
                lender_id: &str,
                loan_id: &str,
                issuer_id: &str)
                -> io::Result<Output> {
  Command::new(COMMAND).args(&["--txn", txn_builder_path])
                       .args(&["lender", "--id", lender_id])
                       .arg("fulfill_loan")
                       .args(&["--loan", loan_id])
                       .args(&["--issuer", issuer_id])
                       .args(&["--http", "--localhost"])
                       .output()
}

#[cfg(test)]
fn pay_loan(borrower_id: &str, loan_id: &str, amount: &str) -> io::Result<Output> {
  Command::new(COMMAND).args(&["borrower", "--id", borrower_id])
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
  // Create an asset issuer
  let output = sign_up_asset_issuer("Issuer AI").expect("Failed to create an asset issuer");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create a credential issuer
  let output =
    sign_up_credential_issuer("Issuer CI").expect("Failed to create a credential issuer");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create a lender
  let output = sign_up_lender("Lender L", "550").expect("Failed to create a lender");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Create a borrower
  let output = sign_up_borrower("Borrower B").expect("Failed to create a borrower");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  let _ = fs::remove_file(DATA_FILE);
}

#[test]
#[ignore]
fn test_create_or_update_credentials() {
  // Update the min_credit_score credential
  let output = create_or_overwrite_credential("0", "min_credit_score", "600").expect("Failed to create a min_credit_score credential");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Updating the credential record.".to_owned()));

  // Create a min_income credential
  let output =
  create_or_overwrite_credential("0", "min_income", "1000").expect("Failed to create a min_income credential");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Adding the credential record.".to_owned()));

  // Create a citizenshiip credential
  let output =
  create_or_overwrite_credential("0", "citizenship", "1").expect("Failed to create a citizenship credential");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Adding the credential record.".to_owned()));

  let _ = fs::remove_file(DATA_FILE);
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
// Lender or borrower views loans or credentials
//
#[test]
#[ignore]
fn test_view() {
  let ledger_standalone = LedgerStandalone::new();

  // Add a credential
  create_or_overwrite_credential("0", "min_income", "1500").expect("Failed to create a credential");

  // Create loans
  request_loan("0", "0", "100", "100", "3").expect("Failed to request the loan");
  request_loan("0", "0", "200", "150", "6").expect("Failed to request the loan");
  request_loan("1", "0", "300", "200", "9").expect("Failed to request the loan");
  request_loan("1", "0", "500", "300", "15").expect("Failed to request the loan");

  // Fulfill some of the loans
  let txn_builder_path = "txn_builder_view_loans";
  create_txn_builder_with_path(txn_builder_path).expect("Failed to create transaction builder");
  ledger_standalone.poll_until_ready().unwrap();
  fulfill_loan(txn_builder_path, "0", "0", "0").expect("Failed to fulfill the loan");
  ledger_standalone.poll_until_ready().unwrap();
  fulfill_loan(txn_builder_path, "0", "1", "0").expect("Failed to fulfill the loan");
  ledger_standalone.poll_until_ready().unwrap();
  fulfill_loan(txn_builder_path, "1", "2", "0").expect("Failed to fulfill the loan");

  // View loans
  // 1. View all loans of a lender
  let output = view_loan_all("lender", "1").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying 2 loan(s):".to_owned()));

  // 2. View all loans of a borrower
  let output = view_loan_all("borrower", "0").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying 4 loan(s):".to_owned()));

  // 3.   View a loan by its id
  // 3.1  The loan is owned by the user
  let output = view_loan_with_loan_id("lender", "0", "0").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying loan".to_owned()));

  // 3.2  The loan isn't owned by the user
  let output = view_loan_with_loan_id("lender", "0", "2").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"doesn't own loan".to_owned()));

  // 4. View loans with a filter
  // 4.1 Requested but not fulfilled loan
  let output =
    view_loan_with_filter("borrower", "0", "requested").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying 1 loan(s):".to_owned()));

  // 4.2. View fulfilled loan
  let output =
    view_loan_with_filter("borrower", "0", "fulfilled").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying 2 loan(s):".to_owned()));

  // 4.3. View declined loan
  let output = view_loan_with_filter("borrower", "0", "declined").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying 1 loan(s):".to_owned()));

  // 4.4. View active loan
  let output = view_loan_with_filter("borrower", "0", "active").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying 2 loan(s):".to_owned()));

  // 4.5. View complete loan
  let output = view_loan_with_filter("borrower", "0", "complete").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying 0 loan(s):".to_owned()));

  // View credentials
  // 1. View all credentials of a borrower
  let output = view_credential_all("0").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying 2 credential(s):".to_owned()));

  // 2. View a credential by credential id
  let output = view_credential_with_credential_id("0", "0").expect("Failed to view the loan");
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Displaying credential".to_owned()));

  let _ = fs::remove_file(DATA_FILE);
  fs::remove_file(txn_builder_path).unwrap();
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

//
// Compose transaction and submit
//
#[test]
#[ignore]
fn test_define_issue_transfer_and_submit_with_args() {
  let ledger_standalone = LedgerStandalone::new();

  // Create users and txn builder
  sign_up_borrower("Borrower 1").expect("Failed to create a borrower");
  sign_up_borrower("Borrower 2").expect("Failed to create a borrower");
  let txn_builder_file = "tb_define_issue_transfer_and_submit";
  create_txn_builder_with_path(txn_builder_file).expect("Failed to create transaction builder");

  // Define asset
  let token_code = AssetTypeCode::gen_random().to_base64();
  let output = define_asset(txn_builder_file,
               "0",
               &token_code,
               "Define an asset").expect("Failed to define asset");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Submit transaction
  ledger_standalone.poll_until_ready().unwrap();
  let output = submit(txn_builder_file).expect("Failed to submit transaction");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Issue asset
  let amount_issue = "100";
  let output =
    issue_asset(txn_builder_file, "0", &token_code, amount_issue).expect("Failed to issue asset");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Submit transaction
  let sids_file = "sids_define_issue_transfer_and_submit";
  ledger_standalone.poll_until_ready().unwrap();
  let output =
    submit_and_store_sids(txn_builder_file, sids_file).expect("Failed to submit transaction");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Store blind asset record and associated memos
  let bar_and_memos_file = "bar_and_memos_define_issue_transfer_and_submit";
  let output = store_blind_asset_record_and_memos_nonconfidential("0", amount_issue, &token_code, bar_and_memos_file).expect("Failed to store blind asset record and memos");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Transfer asset
  let output = transfer_asset(txn_builder_file,
                              "0",
                              "1,2",
                              sids_file,
                              bar_and_memos_file,
                              "50",
                              "30,20").expect("Failed to transfer asset");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Submit transaction
  ledger_standalone.poll_until_ready().unwrap();
  let output = submit(txn_builder_file).expect("Failed to submit transaction");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  let _ = fs::remove_file(DATA_FILE);
  fs::remove_file(txn_builder_file).unwrap();
  fs::remove_file(bar_and_memos_file).unwrap();
  fs::remove_file(sids_file).unwrap();

  assert!(output.status.success());
}

//
// Compose transaction and submit
//
#[test]
fn test_air_assign() {
  // Create txn builder and key pair
  let txn_builder_file = "tb_air_assign";
  create_txn_builder_with_path(txn_builder_file).expect("Failed to create transaction builder");

  // Air assigning
  air_assign(txn_builder_file, "0", "666", "Hell").expect("Failed to assign to AIR");

  // Submit transaction
  let ledger_standalone = LedgerStandalone::new();
  ledger_standalone.poll_until_ready().unwrap();
  let output = submit(txn_builder_file).expect("Failed to submit transaction");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  let _ = fs::remove_file(DATA_FILE);
  fs::remove_file(txn_builder_file).unwrap();

  assert!(output.status.success());
}

#[test]
fn test_issue_transfer_and_submit_with_args() {
  let ledger_standalone = LedgerStandalone::new();

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
  ledger_standalone.poll_until_ready().unwrap();
  submit(txn_builder_file).expect("Failed to submit transaction");

  // Issue and transfer
  issue_and_transfer_asset(txn_builder_file,
                           "0",
                           "0",
                           "1000",
                           &token_code).expect("Failed to issue and transfer asset");

  // Submit transaction
  ledger_standalone.poll_until_ready().unwrap();
  let output = submit(txn_builder_file).expect("Failed to submit transaction");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  let _ = fs::remove_file(DATA_FILE);
  fs::remove_file(txn_builder_file).unwrap();

  assert!(output.status.success());
}

#[test]
#[ignore]
// Test funds loading, loan request, fulfilling and repayment
fn test_request_fulfill_and_pay_loan_with_args() {
  let ledger_standalone = LedgerStandalone::new();

  // Load funds
  let txn_builder_file = "tb_load_funds_args";
  ledger_standalone.poll_until_ready().unwrap();
  let output = load_funds(txn_builder_file, "0", "0", "5000").expect("Failed to load funds");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  fs::remove_file(txn_builder_file).unwrap();
  assert!(output.status.success());

  // Request the first loan
  let output = request_loan("0", "0", "1500", "100", "8").expect("Failed to request a loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Request the second loan
  let output = request_loan("1", "0", "1000", "80", "10").expect("Failed to request a loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // Fulfill the first loan
  // 1. First time:
  //    Add the credential proof, then successfully initiate the loan
  let txn_builder_file = "tb_fulfill_loan_args";
  ledger_standalone.poll_until_ready().unwrap();
  let output = fulfill_loan(txn_builder_file, "0", "0", "0").expect("Failed to initiate the loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"Proving before attesting.".to_owned()));

  // 2. Second time:
  //    Fail because the loan has been fulfilled
  ledger_standalone.poll_until_ready().unwrap();
  let output = fulfill_loan(txn_builder_file, "0", "0", "0").expect("Failed to initiate the loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"has already been fulfilled.".to_owned()));

  // Fulfill the second loan
  // 1. First time:
  //    Get the credential proof, then fail to initiate the loan because the requirement isn't met
  ledger_standalone.poll_until_ready().unwrap();
  let output = fulfill_loan(txn_builder_file, "1", "1", "0").expect("Failed to initiate the loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
  let stdout = from_utf8(&output.stdout).unwrap();
  assert!(stdout.contains(&"Attesting with the existing proof.".to_owned())
          && stdout.contains(&"Value should be at least:".to_owned()));

  // 2. Second time:
  //    Fail because the loan has been declined
  ledger_standalone.poll_until_ready().unwrap();
  let output = fulfill_loan(txn_builder_file, "1", "1", "0").expect("Failed to initiate the loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"has already been declined.".to_owned()));

  // Pay loan
  // 1. First time:
  //    Burn part of the loan balance
  ledger_standalone.poll_until_ready().unwrap();
  let output = pay_loan("0", "0", "300").expect("Failed to pay loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // 2. Second time
  //    Pay off the loan
  ledger_standalone.poll_until_ready().unwrap();
  let output = pay_loan("0", "0", "2000").expect("Failed to pay loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success());

  // 3. Third time:
  //    Fail because the loan has been paid off
  ledger_standalone.poll_until_ready().unwrap();
  let output = pay_loan("0", "0", "3000").expect("Failed to pay loan");

  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert_eq!(output.status.code(), Some(exitcode::USAGE));
  assert!(from_utf8(&output.stdout).unwrap()
                                   .contains(&"has been paid off.".to_owned()));

  let _ = fs::remove_file(DATA_FILE);
  fs::remove_file(txn_builder_file).unwrap();
}
