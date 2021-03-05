#![allow(unused)]
use ledger::data_model::AssetTypeCode;
#[cfg(test)]
use network::MockLedgerStandalone;
use ruc::*;
use tempfile::tempdir;
use txn_cli::txn_app::{get_cli_app, process_inputs};

extern crate exitcode;

#[cfg(test)]
fn submit_command(
    cmd_vec: Vec<&str>,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let app = get_cli_app();
    let inputs = app.get_matches_from_safe(cmd_vec).unwrap();
    process_inputs(inputs, rest_client).c(d!())
}

//
// Helper functions: view records
//
#[cfg(test)]
fn view_loan_all(
    dir: &str,
    user_type: &str,
    user_id: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        user_type,
        "--id",
        user_id,
        "view_loan",
    ];
    submit_command(args, rest_client).c(d!())
}

#[cfg(test)]
fn view_loan_with_loan_id(
    dir: &str,
    user_type: &str,
    user_id: &str,
    loan_id: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        user_type,
        "--id",
        user_id,
        "view_loan",
        "--loan",
        loan_id,
    ];
    submit_command(args, rest_client).c(d!())
}

#[cfg(test)]
fn view_loan_with_filter(
    dir: &str,
    user_type: &str,
    user_id: &str,
    filter: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        user_type,
        "--id",
        user_id,
        "view_loan",
        "--filter",
        filter,
    ];
    submit_command(args, rest_client).c(d!())
}

#[cfg(test)]
fn view_credential_all(
    borrower_id: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "borrower",
        "--id",
        borrower_id,
        "view_credential",
    ];
    submit_command(args, rest_client).c(d!())
}

#[cfg(test)]
fn view_credential_attribute(
    borrower_id: &str,
    attribute: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "borrower",
        "--id",
        borrower_id,
        "view_credential",
        "--attribute",
        attribute,
    ];
    submit_command(args, rest_client).c(d!())
}

//
// Helper functions: sign up an account
//
#[cfg(test)]
fn sign_up_borrower(
    dir: &str,
    name: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        "borrower",
        "sign_up",
        "--name",
        name,
    ];
    submit_command(args, rest_client).c(d!())
}

//
// Helper functions: create and store without path
//
#[cfg(test)]
fn create_or_overwrite_credential(
    dir: &str,
    id: &str,
    attribute: &str,
    value: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        "borrower",
        "--id",
        id,
        "create_or_overwrite_credential",
        "--credential_issuer",
        "0",
        "--attribute",
        attribute,
        "--value",
        value,
    ];
    submit_command(args, rest_client).c(d!())
}

fn mk_tmp_file(name: &str) -> String {
    let tmp_dir = tempdir().unwrap();
    let txn_builder_buf = tmp_dir.path().join(name);
    txn_builder_buf.to_str().unwrap().to_string()
}

#[cfg(test)]
fn request_loan(
    dir: &str,
    lender: &str,
    borrower: &str,
    amount: &str,
    interest_per_mille: &str,
    duration: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let txn_builder_file = mk_tmp_file("tb_tmp_request_loan");

    let args = vec![
        "Transaction Builder",
        "--txn",
        &txn_builder_file,
        "--dir",
        dir,
        "borrower",
        "--id",
        borrower,
        "request_loan",
        "--lender",
        lender,
        "--amount",
        amount,
        "--interest_per_mille",
        interest_per_mille,
        "--duration",
        duration,
    ];
    submit_command(args, rest_client).c(d!())
}

//
// Helper functions: create and store with path
//
#[cfg(test)]
fn create_txn_builder_with_path(
    path: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let arg_vec = vec!["Transaction Builder", "create_txn_builder", "--name", path];
    submit_command(arg_vec, rest_client).c(d!())
}
/* Unused
#[cfg(test)]
fn store_memos_with_confidential_amount(dir: &str,
                                        txn_builder_path: &str,
                                        id: &str,
                                        amount: &str,
                                        token_code: &str,
                                        file: &str,
                                        rest_client: &mut MockLedgerStandalone)
                                        -> Result<()> {
  let args = vec!["Transaction Builder",
                  "--txn",
                  txn_builder_path,
                  "--dir",
                  dir,
                  "asset_issuer",
                  "--id",
                  id,
                  "store_memos",
                  "--amount",
                  amount,
                  "--confidential_amount",
                  "--token_code",
                  token_code,
                  "--file",
                  file];
  submit_command(args, rest_client).c(d!())
}

#[cfg(test)]
fn trace_and_verify_asset(dir: &str,
                          txn_builder_path: &str,
                          id: &str,
                          memo_file: &str,
                          expected_amount: &str,
                          rest_client: &mut MockLedgerStandalone)
                          -> Result<()> {
  let args = vec!["Transaction Builder",
                  "--txn",
                  txn_builder_path,
                  "--dir",
                  dir,
                  "asset_issuer",
                  "--id",
                  id,
                  "trace_and_verify_asset",
                  "--memo_file",
                  memo_file,
                  "--expected_amount",
                  expected_amount];
  submit_command(args, rest_client).c(d!())
}
*/
#[cfg(test)]
fn trace_credential(
    dir: &str,
    id: &str,
    memo_file: &str,
    attribute: &str,
    expected_value: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let txn_builder_file = mk_tmp_file("tb_trace_credential");
    let args = vec![
        "Transaction Builder",
        "--txn",
        &txn_builder_file,
        "--dir",
        dir,
        "asset_issuer",
        "--id",
        id,
        "trace_credential",
        "--memo_file",
        memo_file,
        "--attribute",
        attribute,
        "--expected_value",
        expected_value,
    ];
    submit_command(args, rest_client).c(d!())
}

//
// Helper functions: define, issue and transfer
//

// This test is ignored because it does not work (no proof is being passed in)
#[cfg(test)]
fn air_assign(
    dir: &str,
    txn_builder_path: &str,
    issuer_id: &str,
    address: &str,
    data: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        "--txn",
        txn_builder_path,
        "asset_issuer",
        "--id",
        issuer_id,
        "air_assign",
        "--address",
        address,
        "--data",
        data,
    ];
    submit_command(args, rest_client).c(d!())
}

#[cfg(test)]
fn define_asset(
    dir: &str,
    txn_builder_path: &str,
    issuer_id: &str,
    token_code: &str,
    memo: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let token_arg = format!("--token_code={}", token_code);
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        "--txn",
        txn_builder_path,
        "asset_issuer",
        "--id",
        issuer_id,
        "define_asset",
        &token_arg,
        "--memo",
        memo,
    ];
    submit_command(args, rest_client).c(d!())
}

#[cfg(test)]
fn issue_asset_with_confidential_amount(
    dir: &str,
    txn_builder_path: &str,
    id: &str,
    token_code: &str,
    amount: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let token_arg = format!("--token_code={}", token_code);
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        "--txn",
        txn_builder_path,
        "asset_issuer",
        "--id",
        id,
        "issue_asset",
        &token_arg,
        "--amount",
        amount,
        "--confidential_amount",
    ];
    submit_command(args, rest_client).c(d!())
}

#[cfg(test)]
#[allow(clippy::too_many_arguments)]
fn transfer_asset(
    dir: &str,
    txn_builder_path: &str,
    issuer_id: &str,
    recipient_ids: &str,
    sids_file: &str,
    issuance_txn_files: &str,
    input_amounts: &str,
    output_amounts: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        "--txn",
        txn_builder_path,
        "asset_issuer",
        "--id",
        issuer_id,
        "transfer_asset",
        "--recipients",
        recipient_ids,
        "--sids_file",
        sids_file,
        "--issuance_txn_files",
        issuance_txn_files,
        "--input_amounts",
        input_amounts,
        "--output_amounts",
        output_amounts,
    ];
    submit_command(args, rest_client).c(d!())
}
/*
#[cfg(test)]
fn issue_and_transfer_asset_confidential(txn_builder_path: &str,
                                         issuer_id: &str,
                                         recipient_id: &str,
                                         amount: &str,
                                         token_code: &str,
                                         rest_client: &mut MockLedgerStandalone)
                                         -> Result<()> {
  let args = vec!["Transaction Builder",
                  "--txn",
                  txn_builder_path,
                  "asset_issuer",
                  "--id",
                  issuer_id,
                  "issue_and_transfer_asset",
                  "--recipient",
                  recipient_id,
                  "--amount",
                  amount,
                  "--token_code",
                  token_code,
                  "--confidential_amount",
                  "--confidential_asset"];
  submit_command(args, rest_client).c(d!())
}
*/
#[cfg(test)]
fn submit(txn_builder_path: &str, rest_client: &mut MockLedgerStandalone) -> Result<()> {
    let arg_vec = vec!["Transaction Builder", "--txn", txn_builder_path, "submit"];
    submit_command(arg_vec, rest_client).c(d!())
}

#[cfg(test)]
fn submit_and_store_sids(
    txn_builder_path: &str,
    sids_file: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "--txn",
        txn_builder_path,
        "submit",
        "--sids_file",
        sids_file,
    ];
    submit_command(args, rest_client).c(d!())
}

// Helper function: load funds
#[cfg(test)]
fn load_funds(
    dir: &str,
    issuer_id: &str,
    borrower_id: &str,
    amount: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let txn_builder_file = mk_tmp_file("tb_tmp_load_funds");
    let args = vec![
        "Transaction Builder",
        "--txn",
        &txn_builder_file,
        "--dir",
        dir,
        "borrower",
        "--id",
        borrower_id,
        "load_funds",
        "--issuer",
        issuer_id,
        "--amount",
        amount,
    ];
    submit_command(args, rest_client).c(d!())
}

// Helper functions: initiate and pay loan
#[cfg(test)]
fn fulfill_loan(
    dir: &str,
    lender_id: &str,
    loan_id: &str,
    issuer_id: &str,
    memo_file: Option<&str>,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let arg_vec = if let Some(file) = memo_file {
        vec![
            "Transaction Builder",
            "--dir",
            dir,
            "lender",
            "--id",
            lender_id,
            "fulfill_loan",
            "--loan",
            loan_id,
            "--issuer",
            issuer_id,
            "--memo_file",
            file,
        ]
    } else {
        vec![
            "Transaction Builder",
            "--dir",
            dir,
            "lender",
            "--id",
            lender_id,
            "fulfill_loan",
            "--loan",
            loan_id,
            "--issuer",
            issuer_id,
        ]
    };
    submit_command(arg_vec, rest_client).c(d!())
}

#[cfg(test)]
fn pay_loan(
    dir: &str,
    borrower_id: &str,
    loan_id: &str,
    amount: &str,
    rest_client: &mut MockLedgerStandalone,
) -> Result<()> {
    let args = vec![
        "Transaction Builder",
        "--dir",
        dir,
        "borrower",
        "--id",
        borrower_id,
        "pay_loan",
        "--loan",
        loan_id,
        "--amount",
        amount,
    ];
    submit_command(args, rest_client).c(d!())
}

// This test passes individually, but we ignore it since it occasionally fails with SubmissionServerError when run with other tests
// which also use the standalone ledger
// GitHub issue: #324
// Redmind issue: #38
#[test]
fn test_create_or_overwrite_credentials() {
    let tmp_dir = tempdir().unwrap();
    let dir = tmp_dir.path().to_str().unwrap();
    let mut ledger_standalone = MockLedgerStandalone::new_mock(1);

    // Create a borrower
    sign_up_borrower(dir, "Borrower B", &mut ledger_standalone)
        .expect("Failed to create a borrower");

    // Create the credential with minimum credit score record
    create_or_overwrite_credential(
        dir,
        "1",
        "min_credit_score",
        "600",
        &mut ledger_standalone,
    )
    .expect("Failed to create a min_credit_score credential");

    // Overwrite the minimum credit score record
    create_or_overwrite_credential(
        dir,
        "1",
        "min_credit_score",
        "680",
        &mut ledger_standalone,
    )
    .expect("Failed to overwrite the min_credit_score credential");

    // Add the minimum income record to the credential

    create_or_overwrite_credential(
        dir,
        "1",
        "min_income",
        "1000",
        &mut ledger_standalone,
    )
    .expect("Failed to create a min_income credential");

    tmp_dir.close().unwrap();
}

// TODO: update `init_data.json`, and uncomment this case
//
// //
// // Lender or borrower views loans or credentials
// //
// #[test]
// fn test_view() {
//     let tmp_dir = tempdir().unwrap();
//     let dir = tmp_dir.path().to_str().unwrap();
//
//     let mut ledger_standalone = MockLedgerStandalone::new_mock(1);
//
//     // Add a credential
//     create_or_overwrite_credential(
//         dir,
//         "0",
//         "min_credit_score",
//         "550",
//         &mut ledger_standalone,
//     )
//     .expect("Failed to create a credential");
//
//     // Create loans
//     request_loan(dir, "0", "0", "100", "100", "3", &mut ledger_standalone)
//         .expect("Failed to request the loan");
//     request_loan(dir, "0", "0", "200", "150", "6", &mut ledger_standalone)
//         .expect("Failed to request the loan");
//     request_loan(dir, "1", "0", "300", "200", "9", &mut ledger_standalone)
//         .expect("Failed to request the loan");
//     request_loan(dir, "1", "0", "500", "300", "15", &mut ledger_standalone)
//         .expect("Failed to request the loan");
//
//     // Fulfill some of the loans
//     fulfill_loan(dir, "0", "0", "0", None, &mut ledger_standalone)
//         .expect("Failed to fulfill the loan");
//     fulfill_loan(dir, "0", "1", "0", None, &mut ledger_standalone)
//         .expect("Failed to fulfill the loan");
//     // Should fail with InputsError
//     assert!(
//         fulfill_loan(dir, "1", "2", "0", None, &mut ledger_standalone)
//             .unwrap_err()
//             .eq_any(eg!(PlatformError::InputsError(None)).as_ref()),
//     );
//
//     // View loans
//     // 1. View all loans of a lender
//     view_loan_all(dir, "lender", "1", &mut ledger_standalone)
//         .expect("Failed to view the loan");
//     // 2. View all loans of a borrower
//     view_loan_all(dir, "borrower", "0", &mut ledger_standalone)
//         .expect("Failed to view the loan");
//
//     // 3. View a loan by its id
//     // 3.1  The loan is owned by the user
//     view_loan_with_loan_id(dir, "lender", "0", "0", &mut ledger_standalone)
//         .expect("Failed to view the loan");
//
//     // 3.2  The loan isn't owned by the user
//     // Should fail with InputsError
//     assert!(
//         view_loan_with_loan_id(dir, "lender", "0", "2", &mut ledger_standalone)
//             .unwrap_err()
//             .eq_any(eg!(PlatformError::InputsError(None)).as_ref()),
//     );
//
//     // 4. View loans with a filter
//     // 4.1 Requested but not fulfilled loan
//     view_loan_with_filter(dir, "borrower", "0", "requested", &mut ledger_standalone)
//         .expect("Failed to view the loan");
//
//     // 4.2. View fulfilled loan
//     view_loan_with_filter(dir, "borrower", "0", "fulfilled", &mut ledger_standalone)
//         .expect("Failed to view the loan");
//
//     // 4.3. View declined loan
//     view_loan_with_filter(dir, "borrower", "0", "declined", &mut ledger_standalone)
//         .expect("Failed to view the loan");
//
//     // 4.4. View active loan
//     view_loan_with_filter(dir, "borrower", "0", "active", &mut ledger_standalone)
//         .expect("Failed to view the loan");
//
//     // 4.5. View complete loan
//     view_loan_with_filter(dir, "borrower", "0", "complete", &mut ledger_standalone)
//         .expect("Failed to view the loan");
//
//     // View credentials
//     // 1. View all credentials of a borrower
//     view_credential_all("0", &mut ledger_standalone).expect("Failed to view the loan");
//
//     // 2. View a credential attribute
//     view_credential_attribute("0", "min_income", &mut ledger_standalone)
//         .expect("Failed to view the attribute");
//
//     tmp_dir.close().unwrap();
// }

//
// Compose transaction and submit
//
#[test]
fn test_define_asset_simple_policies() {
    let tmp_dir = tempdir().unwrap();
    let dir = tmp_dir.path().to_str().unwrap();
    let mut ledger_standalone = MockLedgerStandalone::new_mock(1);
    let txn_builder_buf = tmp_dir.path().join("tb_define_policies");
    let txn_builder_file = txn_builder_buf.to_str().unwrap();
    sign_up_borrower(dir, "Borrower B", &mut ledger_standalone)
        .expect("Failed to create a borrower");

    // Create txn builder and key pairs
    create_txn_builder_with_path(txn_builder_file, &mut ledger_standalone)
        .unwrap_or_else(|_| {
            panic!(
                "Failed to create transaction builder at file {}",
                txn_builder_file
            )
        });

    // Define token code
    let token_code = AssetTypeCode::gen_random().to_base64();

    // Define asset
    let app = get_cli_app();
    let token_arg = format!("--token_code={}", token_code);
    let inputs_vec = vec![
        "Transaction Builder",
        "--dir",
        dir,
        "--txn",
        txn_builder_file,
        "asset_issuer",
        "--id",
        "0",
        "define_asset",
        &token_arg,
        "--memo",
        "Define an asset",
        "--cosigners",
        "1",
        "--traceable",
        "--non_transferable",
    ];
    let inputs = app.get_matches_from_safe(inputs_vec).unwrap();
    process_inputs(inputs, &mut ledger_standalone).expect("Failed to define asset");

    submit(txn_builder_file, &mut ledger_standalone)
        .expect("Failed to submit transaction");

    tmp_dir.close().unwrap();
}

// TODO: update `init_data.json`, and uncomment this case
//
// #[test]
// fn test_define_issue_transfer_and_submit_with_args() {
//     // Create users and files
//     let tmp_dir = tempdir().unwrap();
//     let dir = tmp_dir.path().to_str().unwrap();
//     let mut ledger_standalone = MockLedgerStandalone::new_mock(1);
//     sign_up_borrower(dir, "Borrower 1", &mut ledger_standalone)
//         .expect("Failed to create a borrower");
//     sign_up_borrower(dir, "Borrower 2", &mut ledger_standalone)
//         .expect("Failed to create a borrower");
//     let creation_txn_builder_buf = tmp_dir.path().join("tb_define_and_submit");
//     let issuance_txn_builder_buf = tmp_dir.path().join("tb_issue_submit");
//     let transfer_txn_builder_buf = tmp_dir.path().join("tb_transfer_submit");
//     let sids_buf = tmp_dir.path().join("sids_define_issue_transfer_and_submit");
//     let creation_txn_builder_file = creation_txn_builder_buf.to_str().unwrap();
//     let issuance_txn_builder_file = issuance_txn_builder_buf.to_str().unwrap();
//     let transfer_txn_builder_file = transfer_txn_builder_buf.to_str().unwrap();
//     let sids_file = sids_buf.to_str().unwrap();
//
//     // Define asset
//     let token_code = AssetTypeCode::gen_random().to_base64();
//     define_asset(
//         dir,
//         creation_txn_builder_file,
//         "0",
//         &token_code,
//         "Define an asset",
//         &mut ledger_standalone,
//     )
//     .expect("Failed to define asset");
//
//     // Submit transaction
//     submit(creation_txn_builder_file, &mut ledger_standalone)
//         .expect("Failed to submit transaction");
//
//     // Issue asset
//     let amount_issue = "50";
//
//     issue_asset_with_confidential_amount(
//         dir,
//         issuance_txn_builder_file,
//         "0",
//         &token_code,
//         amount_issue,
//         &mut ledger_standalone,
//     )
//     .expect("Failed to issue asset");
//
//     // Submit transaction
//
//     submit_and_store_sids(issuance_txn_builder_file, sids_file, &mut ledger_standalone)
//         .expect("Failed to submit transaction");
//
//     // Transfer asset
//     transfer_asset(
//         dir,
//         transfer_txn_builder_file,
//         "0",
//         "1,2",
//         sids_file,
//         issuance_txn_builder_file,
//         "50",
//         "30, 20",
//         &mut ledger_standalone,
//     )
//     .expect("Failed to transfer asset");
//
//     // Submit transaction
//     submit(transfer_txn_builder_file, &mut ledger_standalone)
//         .expect("Failed to submit transaction");
//
//     tmp_dir.close().unwrap();
// }

// TODO: update `init_data.json`, and uncomment this case
//
// #[test]
// fn test_define_issue_transfer_and_submit_with_args_hyphen() {
//     // Create users and files
//     let tmp_dir = tempdir().unwrap();
//     let dir = tmp_dir.path().to_str().unwrap();
//     let mut ledger_standalone = MockLedgerStandalone::new_mock(1);
//     sign_up_borrower(dir, "Borrower 1", &mut ledger_standalone)
//         .expect("Failed to create a borrower");
//     sign_up_borrower(dir, "Borrower 2", &mut ledger_standalone)
//         .expect("Failed to create a borrower");
//     let creation_txn_builder_buf = tmp_dir.path().join("tb_define_and_submit");
//     let issuance_txn_builder_buf = tmp_dir.path().join("tb_issue_submit");
//     let transfer_txn_builder_buf = tmp_dir.path().join("tb_transfer_submit");
//     let sids_buf = tmp_dir.path().join("sids_define_issue_transfer_and_submit");
//     let creation_txn_builder_file = creation_txn_builder_buf.to_str().unwrap();
//     let issuance_txn_builder_file = issuance_txn_builder_buf.to_str().unwrap();
//     let transfer_txn_builder_file = transfer_txn_builder_buf.to_str().unwrap();
//     let sids_file = sids_buf.to_str().unwrap();
//
//     // Define asset
//     let token_code = AssetTypeCode::new_from_base64(
//         &once('-')
//             .chain(AssetTypeCode::gen_random().to_base64().chars().skip(1))
//             .collect::<String>(),
//     )
//     .unwrap()
//     .to_base64();
//     define_asset(
//         dir,
//         creation_txn_builder_file,
//         "0",
//         &token_code,
//         "Define an asset",
//         &mut ledger_standalone,
//     )
//     .expect("Failed to define asset");
//
//     // Submit transaction
//     submit(creation_txn_builder_file, &mut ledger_standalone)
//         .expect("Failed to submit transaction");
//
//     // Issue asset
//     let amount_issue = "50";
//
//     issue_asset_with_confidential_amount(
//         dir,
//         issuance_txn_builder_file,
//         "0",
//         &token_code,
//         amount_issue,
//         &mut ledger_standalone,
//     )
//     .expect("Failed to issue asset");
//
//     // Submit transaction
//
//     submit_and_store_sids(issuance_txn_builder_file, sids_file, &mut ledger_standalone)
//         .expect("Failed to submit transaction");
//
//     // Transfer asset
//     transfer_asset(
//         dir,
//         transfer_txn_builder_file,
//         "0",
//         "1,2",
//         sids_file,
//         issuance_txn_builder_file,
//         "50",
//         "30, 20",
//         &mut ledger_standalone,
//     )
//     .expect("Failed to transfer asset");
//
//     // Submit transaction
//     submit(transfer_txn_builder_file, &mut ledger_standalone)
//         .expect("Failed to submit transaction");
//
//     tmp_dir.close().unwrap();
// }

/* This test causes a CI failure. It is a riddle, wrapped in a mystery, inside an enigma; but perhaps there is a key.
#[test]
#[ignore]
fn test_issue_transfer_trace_and_submit_with_args() {
  let tmp_dir = tempdir().unwrap();
  let dir = tmp_dir.path().to_str().unwrap();
  let txn_builder_buf = tmp_dir.path().join("tb_issue_transfer_args");
  let txn_builder_file = txn_builder_buf.to_str().unwrap();
  let memo_buf = tmp_dir.path().join("memos_issue_transfer_and_submit");
  let mut ledger_standalone = MockLedgerStandalone::new_mock(1);
  let memo_file = memo_buf.to_str().unwrap();

  // Create txn builder and key pairs
  create_txn_builder_with_path(txn_builder_file,
                               &mut ledger_standalone).expect("Failed to create transaction builder");

  // Define token code
  let token_code = AssetTypeCode::gen_random().to_base64();

  // Define asset
  define_asset(dir,
               txn_builder_file,
               "0",
               &token_code,
               "Define an asset",
               &mut ledger_standalone).expect("Failed to define asset");
  submit(txn_builder_file, &mut ledger_standalone).expect("Failed to submit transaction");

  // Issue and transfer
  let amount = "1000";
  issue_and_transfer_asset_confidential(txn_builder_file,
                           "0",
                           "0",
                           amount,
                           &token_code, &mut ledger_standalone).expect("Failed to issue and transfer asset");

  // Store tracer and owner memos

  store_memos_with_confidential_amount(dir,
                                       txn_builder_file,
                                       "0",
                                       amount,
                                       &token_code,
                                       memo_file,
                                       &mut ledger_standalone).expect("Failed to store memos");

  trace_and_verify_asset(dir,
                         txn_builder_file,
                         "0",
                         memo_file,
                         amount,
                         &mut ledger_standalone).expect("Failed to trace the asset");

  // Submit transaction

  submit(txn_builder_file, &mut ledger_standalone).expect("Failed to submit transaction");

  tmp_dir.close().unwrap();
}
*/
// Redmine #70
// #[test]
// #[ignore]
#[cfg(test)]
fn test_air_assign() {
    // Create txn builder and key pair
    let tmp_dir = tempdir().unwrap();
    let dir = tmp_dir.path().to_str().unwrap();
    let txn_builder_buf = tmp_dir.path().join("tb_air_assign");
    let mut ledger_standalone = MockLedgerStandalone::new_mock(1);
    let txn_builder_file = txn_builder_buf.to_str().unwrap();
    create_txn_builder_with_path(txn_builder_file, &mut ledger_standalone)
        .expect("Failed to create transaction builder");

    // Air assigning
    air_assign(
        dir,
        txn_builder_file,
        "0",
        "666",
        "Hell",
        &mut ledger_standalone,
    )
    .expect("Failed to assign to AIR");

    // Submit transaction

    submit(txn_builder_file, &mut ledger_standalone)
        .expect("Failed to submit transaction");

    tmp_dir.close().unwrap();
}

// TODO: update `init_data.json`, and uncomment this case
//
// // Test funds loading, loan request, fulfilling and repayment
// #[test]
// #[ignore]
// fn test_request_fulfill_and_pay_loan_with_args() {
//     let tmp_dir = tempdir().unwrap();
//     let dir = tmp_dir.path().to_str().unwrap();
//     let memo_buf = tmp_dir.path().join("memo_fulfill_loan_args");
//     let mut ledger_standalone = MockLedgerStandalone::new_mock(1);
//     let memo_file = memo_buf.to_str().unwrap();
//
//     // Load funds
//
//     load_funds(dir, "0", "0", "5000", &mut ledger_standalone)
//         .expect("Failed to load funds");
//
//     // Request the first loan
//     request_loan(dir, "0", "0", "1500", "100", "8", &mut ledger_standalone)
//         .expect("Failed to request a loan");
//
//     // Request the second loan
//     request_loan(dir, "1", "0", "1000", "80", "10", &mut ledger_standalone)
//         .expect("Failed to request a loan");
//
//     // Fulfill the first loan
//     // 1. First time:
//     //    Add the credential proof, then successfully initiate the loan
//     //    Trace the credential associated with the first loan
//     fulfill_loan(dir, "0", "0", "0", Some(memo_file), &mut ledger_standalone)
//         .expect("Failed to initiate the loan");
//     let output_trace_fail = trace_credential(
//         dir,
//         "0",
//         memo_file,
//         "min_income",
//         "1000",
//         &mut ledger_standalone,
//     );
//     assert!(output_trace_fail.is_err());
//
//     trace_credential(
//         dir,
//         "0",
//         memo_file,
//         "min_credit_score",
//         "650",
//         &mut ledger_standalone,
//     )
//     .expect("Failed to trace the credential");
//
//     // 2. Second time:
//     //    Fail because the loan has been fulfilled
//     let output = fulfill_loan(dir, "0", "0", "0", None, &mut ledger_standalone);
//     assert!(output.is_err());
//
//     // Fulfill the second loan
//     // 1. First time:
//     //    Should fail with InputsError because the requirement isn't met
//     assert!(fulfill_loan(dir, "1", "1", "0", None, &mut ledger_standalone).is_err());
//
//     // 2. Second time:
//     //    Should fail because the loan has been declined
//     assert!(fulfill_loan(dir, "1", "1", "0", None, &mut ledger_standalone).is_err());
//
//     // Pay loan
//     // 1. First time:
//     //    Burn part of the loan balance
//     pay_loan(dir, "0", "0", "300", &mut ledger_standalone).expect("Failed to pay loan");
//
//     // 2. Second time
//     //    Pay off the loan
//     pay_loan(dir, "0", "0", "2000", &mut ledger_standalone).expect("Failed to pay loan");
//
//     // 3. Third time:
//     //    Should fail because the loan has been paid off
//     assert!(pay_loan(dir, "0", "0", "3000", &mut ledger_standalone).is_err());
//     tmp_dir.close().unwrap();
// }
