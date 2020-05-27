#![deny(warnings)]
use crate::data_lib::*;
use credentials::{
  CredCommitment, CredCommitmentKey, CredIssuerPublicKey, CredPoK, CredUserPublicKey,
  CredUserSecretKey,
};
use curve25519_dalek::ristretto::CompressedRistretto;
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AssetRules, AssetTypeCode, TransferType, TxOutput, TxoRef, TxoSID};
use ledger::{des_fail, error_location};
use ledger_standalone::LedgerStandalone;
use rand_core::{CryptoRng, RngCore};
use std::process::exit;
use submission_server::{TxnHandle, TxnStatus};
use txn_builder::{BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder};
use utils::{LEDGER_PORT, SUBMIT_PORT};
use zei::api::anon_creds::Credential as ZeiCredential;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::XfrKeyPair;
use zei::xfr::structs::{
  AssetRecordTemplate, AssetTracingPolicy, BlindAssetRecord, OpenAssetRecord, OwnerMemo, XfrAmount,
  XfrAssetType,
};

extern crate exitcode;

pub fn air_assign(data_dir: &str,
                  issuer_id: u64,
                  address: &str,
                  data: &str,
                  issuer_pk: &str,
                  pok: &str,
                  txn_file: &str)
                  -> Result<(), PlatformError> {
  let issuer_data = load_data(data_dir)?;
  let xfr_key_pair = issuer_data.get_asset_issuer_key_pair(issuer_id)?;
  let mut txn_builder = TransactionBuilder::default();
  let address = serde_json::from_str::<CredUserPublicKey>(address)?;
  let data = serde_json::from_str::<CredCommitment>(data)?;
  let issuer_pk = serde_json::from_str::<CredIssuerPublicKey>(issuer_pk)?;
  let pok = serde_json::from_str::<CredPoK>(pok)?;
  txn_builder.add_operation_air_assign(&xfr_key_pair, address, data, issuer_pk, pok)?;
  store_txn_to_file(&txn_file, &txn_builder)?;
  Ok(())
}

/// Defines an asset.
///
/// Note: the transaction isn't submitted until `submit` or `submit_and_get_sids` is called.
///
/// # Arguments
/// * `fiat_asset`: whether the asset is a fiat asset.
/// * `issuer_key_pair`: asset issuer's key pair.
/// * `token_code`: asset token code.
/// * `memo`: memo for defining the asset.
/// * `asset_rules`: simple asset rules (e.g. traceable, transferable)
/// * `txn_file`: path to store the transaction file.
pub fn define_asset(data_dir: &str,
                    fiat_asset: bool,
                    issuer_key_pair: &XfrKeyPair,
                    token_code: AssetTypeCode,
                    memo: &str,
                    asset_rules: AssetRules,
                    txn_file: Option<&str>)
                    -> Result<TransactionBuilder, PlatformError> {
  let mut txn_builder = TransactionBuilder::default();
  txn_builder.add_operation_create_asset(issuer_key_pair,
                                         Some(token_code),
                                         asset_rules,
                                         &memo,
                                         PolicyChoice::Fungible())?;
  if let Some(file) = txn_file {
    store_txn_to_file(&file, &txn_builder)?;
  }

  // Update data
  let mut data = load_data(data_dir)?;

  if fiat_asset {
    data.fiat_code = Some(token_code.to_base64());
    store_data_to_file(data, data_dir)?;
  };

  Ok(txn_builder)
}

/// Defines an asset and submits the transaction with the standalone ledger.
pub fn define_and_submit(issuer_key_pair: &XfrKeyPair,
                         code: AssetTypeCode,
                         rules: AssetRules,
                         ledger_standalone: &LedgerStandalone)
                         -> Result<(), PlatformError> {
  // Define the asset
  let mut txn_builder = TransactionBuilder::default();
  let txn = txn_builder.add_operation_create_asset(issuer_key_pair,
                                                   Some(code),
                                                   rules,
                                                   "",
                                                   PolicyChoice::Fungible())?
                       .transaction();

  // Submit the transaction
  ledger_standalone.submit_transaction(&txn);

  Ok(())
}

#[allow(clippy::too_many_arguments)]
/// Issues and transfers asset.
/// # Arguments
/// * `issuer_key_pair`: asset issuer's key pair.
/// * `recipient_key_pair`: rercipient's key pair.
/// * `amount`: amount to issue and transfer.
/// * `token_code`: asset token code.
/// * `record_type`: booleans representing whether the amount and asset transfer are confidential.
///   Asset issuance is always nonconfidential.
/// * `memo_file`: path to store the tracer and owner memos, optional.
/// * `txn_file`: path to the transaction file.
/// * `tracing_policy`: asset tracing policy, if any.
#[allow(clippy::too_many_arguments)]
pub fn issue_and_transfer_asset(data_dir: &str,
                                issuer_key_pair: &XfrKeyPair,
                                recipient_key_pair: &XfrKeyPair,
                                amount: u64,
                                token_code: AssetTypeCode,
                                record_type: AssetRecordType,
                                credential_record: Option<(&CredUserSecretKey,
                                        &ZeiCredential,
                                        &CredCommitmentKey)>,
                                txn_file: Option<&str>,
                                tracing_policy: Option<AssetTracingPolicy>,
                                identity_commitment: Option<CredCommitment>)
                                -> Result<TransactionBuilder, PlatformError> {
  // Asset issuance is always nonconfidential
  let (blind_asset_record, _, owner_memo) =
      get_blind_asset_record_and_memos(issuer_key_pair.get_pk(),
                                       amount,
                                       token_code,
                                       AssetRecordType::from_booleans(record_type.is_confidential_amount(), false),
                                       tracing_policy.clone())?;

  // Transfer Operation
  let output_template = if let Some(policy) = tracing_policy.clone() {
    AssetRecordTemplate::with_asset_tracking(amount,
                                             token_code.val,
                                             record_type,
                                             recipient_key_pair.get_pk(),
                                             policy)
  } else {
    AssetRecordTemplate::with_no_asset_tracking(amount,
                                                token_code.val,
                                                record_type,
                                                recipient_key_pair.get_pk())
  };

  let xfr_op = TransferOperationBuilder::new().add_input(TxoRef::Relative(0),
                                                          open_blind_asset_record(&blind_asset_record,
                                                                                  &owner_memo,
                                                                                  issuer_key_pair.get_sk_ref()).map_err(|e| PlatformError::ZeiError(error_location!(),e))?,
                                                          None,
                                                          None,
                                                          amount)?
                                                 .add_output(&output_template,
                                                             tracing_policy.clone(),
                                                             identity_commitment,
                                                             credential_record)?
                                                 .balance()?
                                                 .create(TransferType::Standard)?
                                                 .sign(issuer_key_pair)?
                                                 .transaction()?;

  // Issue and Transfer transaction
  let mut txn_builder = TransactionBuilder::default();
  txn_builder.add_operation_issue_asset(issuer_key_pair,
                                        &token_code,
                                        get_and_update_sequence_number(data_dir)?,
                                        &[(TxOutput(blind_asset_record), owner_memo)],
                                        tracing_policy)?
             .add_operation(xfr_op)
             .transaction();

  if let Some(file) = txn_file {
    store_txn_to_file(file, &txn_builder)?;
  }

  Ok(txn_builder)
}

/// Issues and transfers an asset, submits the transactio with the standalone ledger, and get the UTXO SID, amount blinds and type blind.
#[allow(clippy::too_many_arguments)]
pub fn issue_transfer_and_get_utxo_and_blinds<R: CryptoRng + RngCore>(
  issuer_key_pair: &XfrKeyPair,
  recipient_key_pair: &XfrKeyPair,
  amount: u64,
  code: AssetTypeCode,
  record_type: AssetRecordType,
  sequence_number: u64,
  mut prng: &mut R,
  ledger_standalone: &LedgerStandalone)
  -> Result<(u64, (Scalar, Scalar), Scalar), PlatformError> {
  // Issue and transfer the asset
  let pc_gens = PublicParams::new().pc_gens;
  let input_template = AssetRecordTemplate::with_no_asset_tracking(amount, code.val, AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType, issuer_key_pair.get_pk());
  let input_blind_asset_record =
    build_blind_asset_record(&mut prng, &pc_gens, &input_template, None).0;
  let output_template = AssetRecordTemplate::with_no_asset_tracking(amount,
                                                                    code.val,
                                                                    record_type,
                                                                    recipient_key_pair.get_pk());
  let blinds = &mut ((Scalar::default(), Scalar::default()), Scalar::default());
  let xfr_op = TransferOperationBuilder::new().add_input(TxoRef::Relative(0),
                                                           open_blind_asset_record(&input_blind_asset_record,
                                                                                   &None,
                                                                                   issuer_key_pair.get_sk_ref()).map_err(|e| {
                                                                                     PlatformError::ZeiError(error_location!(), e)
                                                                                   })?,
                                                           None,
                                                           None,
                                                           amount)?
                                                .add_output_and_store_blinds(&output_template, None, prng, blinds)?.balance()?
                                                .create(TransferType::Standard)?
                                                .sign(issuer_key_pair)?
                                                .transaction()?;

  let mut txn_builder = TransactionBuilder::default();
  let txn = txn_builder.add_operation_issue_asset(issuer_key_pair,
                                                  &code,
                                                  sequence_number,
                                                  &[(TxOutput(input_blind_asset_record), None)],
                                                  None)?
                       .add_operation(xfr_op)
                       .transaction();

  // Submit the transaction, and get the UTXO and asset type blind
  Ok((ledger_standalone.submit_transaction_and_fetch_utxos(&txn)[0].0, blinds.0, blinds.1))
}

/// Defines, issues and transfers an asset, and submits the transactions with the standalone ledger.
/// Returns the UTXO SID, the blinding factors for the asset amount, and the blinding factor for the asset type code.
#[allow(clippy::too_many_arguments)]
pub fn define_issue_transfer_and_get_utxo_and_blinds<R: CryptoRng + RngCore>(
  issuer_key_pair: &XfrKeyPair,
  recipient_key_pair: &XfrKeyPair,
  amount: u64,
  code: AssetTypeCode,
  rules: AssetRules,
  record_type: AssetRecordType,
  ledger_standalone: &LedgerStandalone,
  prng: &mut R)
  -> Result<(u64, (Scalar, Scalar), Scalar), PlatformError> {
  // Define the asset
  define_and_submit(issuer_key_pair, code, rules, ledger_standalone)?;

  // Issue and transfer the asset, and get the UTXO SID and asset type blind
  issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                         recipient_key_pair,
                                         amount,
                                         code,
                                         record_type,
                                         1,
                                         prng,
                                         ledger_standalone)
}

/// Queries a value.
///
/// # Arguments
/// * `protocol`: either `https` or `http`.
/// * `host`: either `testnet.findora.org` or `localhost`.
/// * `port`: either `LEDGER_PORT` or `SUBMIT_PORT`.
/// * `route`: route to query.
/// * `value`: value to look up.
///
/// # Examples
/// * To query the BlindAssetRecord with utxo_sid 100 from https://testnet.findora.org:
/// use txn_cli::txn_lib::query;
/// query("https", "testnet.findora.org", LEDGER_PORT, "utxo_sid", "100").unwrap();
pub fn query(protocol: &str,
             host: &str,
             port: &str,
             route: &str,
             value: &str)
             -> Result<String, PlatformError> {
  let mut res = if let Ok(response) =
    reqwest::get(&format!("{}://{}:{}/{}/{}", protocol, host, port, route, value))
  {
    response
  } else {
    return Err(PlatformError::SubmissionServerError(format!("[{}] {}",
                                                            &error_location!(),
                                                            &"Failed to query.")));
  };

  // Log body
  println!("Querying status: {}", res.status());
  let text = res.text().or_else(|_| {
                          Err(PlatformError::SubmissionServerError(format!("[{}] {}",
                                                                           &error_location!(),
                                                                           &"Failed to query.")))
                        })?;
  println!("Querying result: {}", text);

  Ok(text)
}

/// Queries the UTXO SID and gets the asset type commitment.
/// Asset should be confidential, otherwise the commitmemt will be null.
pub fn query_utxo_and_get_type_commitment(utxo: u64,
                                          protocol: &str,
                                          host: &str)
                                          -> Result<CompressedRistretto, PlatformError> {
  let res = query(protocol,
                  host,
                  LEDGER_PORT,
                  "utxo_sid",
                  &format!("{}", utxo))?;
  let blind_asset_record =
    serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| Err(des_fail!()))?;
  match blind_asset_record.asset_type {
    XfrAssetType::Confidential(commitment) => Ok(commitment),
    _ => {
      println!("Found nonconfidential asset.");
      Err(PlatformError::InputsError(error_location!()))
    }
  }
}

/// Queries the UTXO SID to get the amount, either confidential or nonconfidential.
pub fn query_utxo_and_get_amount(utxo: u64,
                                 protocol: &str,
                                 host: &str)
                                 -> Result<XfrAmount, PlatformError> {
  let res = query(protocol,
                  host,
                  LEDGER_PORT,
                  "utxo_sid",
                  &format!("{}", utxo))?;
  let blind_asset_record =
    serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| Err(des_fail!()))?;
  Ok(blind_asset_record.amount)
}

/// Submits a transaction.
///
/// Either this function or `submit_and_get_sids` should be called after a transaction is composed by any of the following:
/// * `air_assign`
/// * `define_asset`
/// * `issue_asset`
/// * `transfer_asset`
/// * `issue_and_transfer_asset`
///
/// # Arguments
/// * `protocol`: either `https` or `http`.
/// * `host`: either `testnet.findora.org` or `localhost`.
/// * `txn_builder`: transation builder.
pub fn submit(protocol: &str,
              host: &str,
              txn_builder: TransactionBuilder)
              -> Result<(), PlatformError> {
  // Submit transaction
  let client = reqwest::Client::new();
  let txn = txn_builder.transaction();
  dbg!(protocol);
  dbg!(host);
  let mut res = client.post(&format!("{}://{}:{}/{}",
                                     protocol, host, SUBMIT_PORT, "submit_transaction"))
                      .json(&txn)
                      .send()
                      .or_else(|_| {
                        Err(PlatformError::SubmissionServerError(format!("[{}] {}",
                                                                         &error_location!(),
                                                                         &"Failed to submit.")))
                      })?;
  // Log body
  let txt = res.text().expect("no response");
  let handle = serde_json::from_str::<TxnHandle>(&txt).unwrap_or_else(|e| {
                                                        panic!("<Invalid JSON> ({}): \"{}\"",
                                                               &e, &txt)
                                                      });
  println!("Submission response: {}", handle);
  println!("Submission status: {}", res.status());

  Ok(())
}

/// Submits a transaction and gets the UTXO (unspent transaction output) SIDs.
///
/// Either this function or `submit` should be called after a transaction is composed by any of the following:
/// * `air_assign`
/// * `define_asset`
/// * `issue_asset`
/// * `transfer_asset`
/// * `issue_and_transfer_asset`
///
/// # Arguments
/// * `protocol`: either `https` or `http`.
/// * `host`: either `testnet.findora.org` or `localhost`.
/// * `txn_builder`: transation builder.
pub fn submit_and_get_sids(protocol: &str,
                           host: &str,
                           txn_builder: TransactionBuilder)
                           -> Result<Vec<TxoSID>, PlatformError> {
  // Submit transaction
  let client = reqwest::Client::new();
  let txn = txn_builder.transaction();
  let mut res = client.post(&format!("{}://{}:{}/{}",
                                     protocol, host, SUBMIT_PORT, "submit_transaction"))
                      .json(&txn)
                      .send()
                      .or_else(|_| {
                        Err(PlatformError::SubmissionServerError(format!("[{}] {}",
                                                                         &error_location!(),
                                                                         &"Failed to submit.")))
                      })?;

  // Log body
  let txt = res.text().expect("no response");
  let handle = serde_json::from_str::<TxnHandle>(&txt).unwrap_or_else(|e| {
                                                        panic!("<Invalid JSON> ({}): \"{}\"",
                                                               &e, &txt)
                                                      });
  println!("Submission response: {}", handle);
  println!("Submission status: {}", res.status());

  // Return sid
  let res = query(protocol, host, SUBMIT_PORT, "txn_status", &handle.0)?;
  match serde_json::from_str::<TxnStatus>(&res).or_else(|_| Err(des_fail!()))? {
    TxnStatus::Committed((_sid, txos)) => Ok(txos),
    _ => Err(des_fail!()),
  }
}

/// Querys the blind asset record by querying the UTXO (unspent transaction output) SID.
/// # Arguments
/// * `txn_file`: path to the transaction file.
/// * `key_pair`: key pair of the asset record.
/// * `owner_memo`: Memo associated with utxo.
pub fn query_open_asset_record(protocol: &str,
                               host: &str,
                               sid: TxoSID,
                               key_pair: &XfrKeyPair,
                               owner_memo: &Option<OwnerMemo>)
                               -> Result<OpenAssetRecord, PlatformError> {
  let res = query(protocol,
                  host,
                  LEDGER_PORT,
                  "utxo_sid",
                  &format!("{}", sid.0))?;
  let blind_asset_record =
    serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| Err(des_fail!()))?;
  open_blind_asset_record(&blind_asset_record, owner_memo, key_pair.get_sk_ref()).or_else(|error| {
                      Err(PlatformError::ZeiError(error_location!(), error))
                    })
}

/// Uses environment variable RUST_LOG to select log level and filters output by module or regex.
///
/// By default, log everything "trace" level or greater to stdout.
///
/// # Examples
/// RUST_LOG="info,txn_cli=trace" ../../target/debug/txn_cli create_txn_builder
pub fn init_logging() {
  flexi_logger::Logger::with_env_or_str("trace").start()
                                                .unwrap();
}

/// Matches the PlatformError with an exitcode and exits.
/// * SerializationError: exits with code `DATAERR`.
/// * DeserializationError: exits with code `DATAERR`.
/// * IoError:
///   * If the input file doesn't exist: exits with code `NOINPUT`.
///     * Note: make sure the error message contains "File doesn't exist:" when constructing the PlatformError.
///   * If the input file isn't readable: exits with code `NOINPUT`.
///     * Note: make sure the error message contains "Failed to read" when constructing the PlatformError.
///   * If the output file or directory can't be created: exits with code `CANTCREAT`.
///     * Note: make sure the error message contains "Failed to create" when constructing the PlatformError.
///   * Otherwise: exits with code `IOERR`.
/// * SubmissionServerError: exits with code `UNAVAILABLE`.
/// * Otherwise: exits with code `USAGE`.
pub fn match_error_and_exit(error: PlatformError) {
  match error {
    PlatformError::SerializationError(_) => exit(exitcode::DATAERR),
    PlatformError::DeserializationError(_) => exit(exitcode::DATAERR),
    PlatformError::IoError(io_error) => {
      if io_error.contains("File doesn't exist:") || io_error.contains("Failed to read") {
        exit(exitcode::NOINPUT)
      }
      if io_error.contains("Failed to create") {
        exit(exitcode::CANTCREAT)
      }
      exit(exitcode::IOERR)
    }
    _ => exit(exitcode::USAGE),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use tempfile::tempdir;

  #[test]
  fn test_define_asset() {
    let tmp_dir = tempdir().unwrap();
    let data_dir = tmp_dir.path().to_str().unwrap();

    // Create key pair
    let mut prng: ChaChaRng = ChaChaRng::from_entropy();
    let issuer_key_pair = XfrKeyPair::generate(&mut prng);

    // Define asset
    let res = define_asset(data_dir,
                           false,
                           &issuer_key_pair,
                           AssetTypeCode::gen_random(),
                           "Define asset",
                           AssetRules::default(),
                           None);

    assert!(res.is_ok());

    tmp_dir.close().unwrap();
  }

  #[test]
  fn test_issue_and_transfer_asset() {
    let tmp_dir = tempdir().unwrap();
    let data_dir = tmp_dir.path().to_str().unwrap();

    // Create key pairs
    let mut prng: ChaChaRng = ChaChaRng::from_entropy();
    let issuer_key_pair = XfrKeyPair::generate(&mut prng);
    let recipient_key_pair = XfrKeyPair::generate(&mut prng);

    // Issue and transfer asset
    let code = AssetTypeCode::gen_random();
    let amount = 1000;
    let res =
      issue_and_transfer_asset(data_dir,
                               &issuer_key_pair,
                               &recipient_key_pair,
                               amount,
                               code,
                               AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                               None,
                               None,
                               None,
                               None);

    assert!(res.is_ok());

    tmp_dir.close().unwrap();
  }
}
