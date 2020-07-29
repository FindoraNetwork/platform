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
use ledger_api_service::RestfulLedgerAccess;
use log::info;
use rand_core::{CryptoRng, RngCore};
use std::process::exit;
use submission_api::RestfulLedgerUpdate;
use submission_server::TxnStatus;
use txn_builder::{BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder};
use zei::api::anon_creds::Credential as ZeiCredential;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::XfrKeyPair;
use zei::xfr::structs::{
  AssetRecordTemplate, AssetTracingPolicies, AssetTracingPolicy, OpenAssetRecord, OwnerMemo,
  XfrAmount, XfrAssetType,
};

extern crate exitcode;

#[allow(clippy::too_many_arguments)]
pub fn air_assign(data_dir: &str,
                  seq_id: u64,
                  issuer_id: u64,
                  address: &str,
                  data: &str,
                  issuer_pk: &str,
                  pok: &str,
                  txn_file: &str)
                  -> Result<(), PlatformError> {
  let issuer_data = load_data(data_dir)?;
  let xfr_key_pair = issuer_data.get_asset_issuer_key_pair(issuer_id)?;
  let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
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
#[allow(clippy::too_many_arguments)]
pub fn define_asset(data_dir: &str,
                    seq_id: u64,
                    fiat_asset: bool,
                    issuer_key_pair: &XfrKeyPair,
                    token_code: AssetTypeCode,
                    memo: &str,
                    asset_rules: AssetRules,
                    txn_file: Option<&str>)
                    -> Result<TransactionBuilder, PlatformError> {
  let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
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

/// Defines an asset and submits the transaction with an http client
pub fn define_and_submit<T>(issuer_key_pair: &XfrKeyPair,
                            code: AssetTypeCode,
                            rules: AssetRules,
                            rest_client: &mut T)
                            -> Result<(), PlatformError>
  where T: RestfulLedgerUpdate + RestfulLedgerAccess
{
  let (_, seq_id, _) = rest_client.get_state_commitment().unwrap();
  // Define the asset
  let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
  let txn = txn_builder.add_operation_create_asset(issuer_key_pair,
                                                   Some(code),
                                                   rules,
                                                   "",
                                                   PolicyChoice::Fungible())?
                       .transaction();

  // Submit the transaction
  rest_client.submit_transaction(&txn)?;

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
                                seq_id: u64,
                                issuer_key_pair: &XfrKeyPair,
                                recipient_key_pair: &XfrKeyPair,
                                amount: u64,
                                token_code: AssetTypeCode,
                                record_type: AssetRecordType,
                                credential_record: Option<(&CredUserSecretKey,
                                        &ZeiCredential,
                                        &CredCommitmentKey)>,
                                txn_file: Option<&str>,
                                memo_file: Option<&str>,
                                tracing_policy: Option<AssetTracingPolicy>,
                                identity_commitment: Option<CredCommitment>)
                                -> Result<TransactionBuilder, PlatformError> {
  // Asset issuance is always nonconfidential
  let (blind_asset_record, tracer_memos, owner_memo) =
      get_blind_asset_record_and_memos(issuer_key_pair.get_pk(),
                                       amount,
                                       token_code,
                                       AssetRecordType::from_booleans(record_type.is_confidential_amount(), false),
                                       tracing_policy.clone())?;

  // Transfer Operation
  let mut policies = AssetTracingPolicies::new();
  if let Some(x) = tracing_policy.as_ref() {
    policies.add(x.clone());
  }
  let output_template = AssetRecordTemplate::with_asset_tracking(amount,
                                                                 token_code.val,
                                                                 record_type,
                                                                 recipient_key_pair.get_pk(),
                                                                 policies);
  let xfr_op =
    TransferOperationBuilder::new().add_input(TxoRef::Relative(0),
                                              open_blind_asset_record(&blind_asset_record,
                                                                &owner_memo,
                                                                issuer_key_pair.get_sk_ref())
                                              .map_err(|e| PlatformError::ZeiError(error_location!(),e))?,
                                              None,
                                              None,
                                              amount)?
                                   .add_output(&output_template,
                                               tracing_policy.map(AssetTracingPolicies::from_policy),
                                               identity_commitment,
                                               credential_record)?
                                   .balance()?
                                   .create(TransferType::Standard)?
                                   .sign(issuer_key_pair)?
                                   .transaction()?;

  // Issue and Transfer transaction
  let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
  txn_builder.add_operation_issue_asset(issuer_key_pair,
                                        &token_code,
                                        get_and_update_sequence_number(data_dir)?,
                                        &[(TxOutput(blind_asset_record), owner_memo.clone())])?
             .add_operation(xfr_op)
             .transaction();

  if let Some(file) = txn_file {
    store_txn_to_file(file, &txn_builder)?;
  }

  if let Some(file) = memo_file {
    store_tracer_and_owner_memos_to_file(file, (tracer_memos, owner_memo))?;
  }

  Ok(txn_builder)
}

/// Issues and transfers an asset, submits the transactio with the standalone ledger, and get the UTXO SID, amount blinds and type blind.
#[allow(clippy::too_many_arguments)]
pub fn issue_transfer_and_get_utxo_and_blinds<R: CryptoRng + RngCore, T>(
  issuer_key_pair: &XfrKeyPair,
  recipient_key_pair: &XfrKeyPair,
  amount: u64,
  code: AssetTypeCode,
  record_type: AssetRecordType,
  sequence_number: u64,
  mut prng: &mut R,
  rest_client: &mut T)
  -> Result<(u64, (Scalar, Scalar), Scalar), PlatformError>
  where T: RestfulLedgerAccess + RestfulLedgerUpdate
{
  // Issue and transfer the asset
  let pc_gens = PublicParams::new().pc_gens;
  let input_template = AssetRecordTemplate::with_no_asset_tracking(amount, code.val, AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType, issuer_key_pair.get_pk());
  let input_blind_asset_record =
    build_blind_asset_record(&mut prng, &pc_gens, &input_template, vec![]).0;
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

  let (_, seq_id, _) = rest_client.get_state_commitment().unwrap();

  let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
  let txn = txn_builder.add_operation_issue_asset(issuer_key_pair,
                                                  &code,
                                                  sequence_number,
                                                  &[(TxOutput(input_blind_asset_record), None)])?
                       .add_operation(xfr_op)
                       .transaction();

  // Submit the transaction, and get the UTXO and asset type blind
  let handle = rest_client.submit_transaction(&txn)?;
  let status = rest_client.txn_status(&handle)?;
  let txos = match status {
    TxnStatus::Committed((_sid, txos)) => txos,
    _ => panic!("Failed to fetch UTXO SIDs"),
  };
  Ok((txos[0].0, blinds.0, blinds.1))
}

/// Defines, issues and transfers an asset, and submits the transactions with the standalone ledger.
/// Returns the UTXO SID, the blinding factors for the asset amount, and the blinding factor for the asset type code.
#[allow(clippy::too_many_arguments)]
pub fn define_issue_transfer_and_get_utxo_and_blinds<T, R: CryptoRng + RngCore>(
  issuer_key_pair: &XfrKeyPair,
  recipient_key_pair: &XfrKeyPair,
  amount: u64,
  code: AssetTypeCode,
  rules: AssetRules,
  record_type: AssetRecordType,
  rest_client: &mut T,
  prng: &mut R)
  -> Result<(u64, (Scalar, Scalar), Scalar), PlatformError>
  where T: RestfulLedgerAccess + RestfulLedgerUpdate
{
  // Define the asset
  define_and_submit(issuer_key_pair, code, rules, rest_client)?;

  // Issue and transfer the asset, and get the UTXO SID and asset type blind
  issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                         recipient_key_pair,
                                         amount,
                                         code,
                                         record_type,
                                         1,
                                         prng,
                                         rest_client)
}

/// Queries the UTXO SID and gets the asset type commitment.
/// Asset should be confidential, otherwise the commitmemt will be null.
pub fn query_utxo_and_get_type_commitment<T>(utxo: u64,
                                             rest_client: &T)
                                             -> Result<CompressedRistretto, PlatformError>
  where T: RestfulLedgerAccess
{
  let blind_asset_record = (rest_client.get_utxo(TxoSID(utxo))?.utxo.0).0;
  match blind_asset_record.asset_type {
    XfrAssetType::Confidential(commitment) => Ok(commitment),
    _ => {
      println!("Found nonconfidential asset.");
      Err(PlatformError::InputsError(error_location!()))
    }
  }
}

/// Queries the UTXO SID to get the amount, either confidential or nonconfidential.
pub fn query_utxo_and_get_amount<T>(utxo: u64, rest_client: &T) -> Result<XfrAmount, PlatformError>
  where T: RestfulLedgerAccess
{
  let blind_asset_record = (rest_client.get_utxo(TxoSID(utxo))?.utxo.0).0;
  Ok(blind_asset_record.amount)
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
/// * `txn_builder`: transaction builder.
/// * `rest_client`: HTTP client.
pub fn submit_and_get_sids<T>(rest_client: &mut T,
                              txn_builder: TransactionBuilder)
                              -> Result<Vec<TxoSID>, PlatformError>
  where T: RestfulLedgerUpdate
{
  let txn = txn_builder.transaction();
  let handle = rest_client.submit_transaction(&txn)?;
  println!("Txn handle: {}", handle);
  // Return sid
  let status = rest_client.txn_status(&handle)?;
  match status {
    TxnStatus::Committed((_sid, txos)) => Ok(txos),
    _ => Err(des_fail!()),
  }
}

/// Querys the blind asset record by querying the UTXO (unspent transaction output) SID.
/// # Arguments
/// * `txn_file`: path to the transaction file.
/// * `key_pair`: key pair of the asset record.
/// * `owner_memo`: Memo associated with utxo.
pub fn query_open_asset_record<T>(rest_client: &T,
                                  sid: TxoSID,
                                  key_pair: &XfrKeyPair,
                                  owner_memo: &Option<OwnerMemo>)
                                  -> Result<OpenAssetRecord, PlatformError>
  where T: RestfulLedgerAccess
{
  let blind_asset_record = (rest_client.get_utxo(sid)?.utxo.0).0;
  open_blind_asset_record(&blind_asset_record, owner_memo, key_pair.get_sk_ref()).map_err(|error| {
                                                PlatformError::ZeiError(error_location!(), error)
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
    PlatformError::SerializationError(e) => {
      info!("SerializationError: {}", e);
      exit(exitcode::DATAERR);
    }
    PlatformError::DeserializationError(e) => {
      info!("Deserializationerror: {}", e);
      exit(exitcode::DATAERR);
    }
    PlatformError::IoError(io_error) => {
      info!("IoError: {}", io_error);
      if io_error.contains("File doesn't exist:") || io_error.contains("Failed to read") {
        exit(exitcode::NOINPUT)
      }
      if io_error.contains("Failed to create") {
        exit(exitcode::CANTCREAT)
      }
      exit(exitcode::IOERR)
    }
    e => {
      info!("Error: {}", e);
      exit(exitcode::USAGE);
    }
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
    let seq_id = 0;
    let res = define_asset(data_dir,
                           seq_id,
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
    let seq_id = 0;
    let res =
      issue_and_transfer_asset(data_dir,
                               seq_id,
                               &issuer_key_pair,
                               &recipient_key_pair,
                               amount,
                               code,
                               AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                               None,
                               None,
                               None,
                               None,
                               None);

    assert!(res.is_ok());

    tmp_dir.close().unwrap();
  }
}
