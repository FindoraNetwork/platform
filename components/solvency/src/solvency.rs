#![deny(warnings)]
use bulletproofs::r1cs::R1CSProof;
use curve25519_dalek::ristretto::CompressedRistretto;
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AssetRules, AssetTypeCode, TransferType, TxOutput, TxoRef};
use ledger::error_location;
use ledger_standalone::LedgerStandalone;
use linear_map::LinearMap;
use rand_chacha::ChaChaRng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use serde::{Deserialize, Serialize};
use txn_builder::BuildsTransactions;
use txn_builder::{PolicyChoice, TransactionBuilder, TransferOperationBuilder};
use txn_cli::txn_lib::query;
use zei::crypto::solvency::{prove_solvency, verify_solvency};
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::XfrKeyPair;
use zei::xfr::structs::{asset_type_to_scalar, AssetRecordTemplate, BlindAssetRecord};

pub(crate) const QUERY_PORT: &str = "8668";

pub(crate) type AssetAmountAndCode = (Scalar, Scalar);
pub(crate) type AssetCodeAndRate = (Scalar, Scalar);
pub(crate) type AssetCommitment = (CompressedRistretto, CompressedRistretto);
pub(crate) type LiabilityCommitment = (CompressedRistretto, CompressedRistretto);

pub enum AmountType {
  Asset,
  Liability,
}

/// Asset and liability information, and associated solvency proof if exists
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AssetAndLiabilityAccount {
  /// Public asset amounts and associated codes
  pub public_assets: Vec<AssetAmountAndCode>,

  /// Hidden asset amounts and associated codes
  pub hidden_assets: Vec<AssetAmountAndCode>,

  /// Commitments to hidden asset amounts
  pub hidden_assets_commitments: Vec<AssetCommitment>,

  /// Public liability amounts and associated codes
  pub public_liabilities: Vec<AssetAmountAndCode>,

  /// Hidden liability amounts and associated codes
  pub hidden_liabilities: Vec<AssetAmountAndCode>,

  /// Commitments to hidden liability amounts
  pub hidden_liabilities_commitments: Vec<LiabilityCommitment>,

  /// Serialized solvency proof, null iff any of the following:
  /// * Solvency hasn't been proved
  /// * Assets or liabilities have been updated
  pub proof: Option<Vec<u8>>,
}

impl AssetAndLiabilityAccount {
  /// Queries a UTXO SID to get the amount or amount blinds and updates the account.
  /// * If the amount is public, verifies it and updates the list of public assets or liabilities.
  /// * Otherwise, updates the list of hidden assets or liabilities, and the list of commitments.
  pub fn query_utxo_and_update_account(&mut self,
                                       amount_type: AmountType,
                                       amount: u64,
                                       code: AssetTypeCode,
                                       utxo: u64,
                                       protocol: &str,
                                       host: &str)
                                       -> Result<(), PlatformError> {
    let res = query(protocol, host, QUERY_PORT, "utxo_sid", &format!("{}", utxo))?;
    let blind_asset_record =
      serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| {
                                                      Err(PlatformError::DeserializationError)
                                                    })?;
    if let Some(fetched_amount) = blind_asset_record.amount.get_amount() {
      if fetched_amount != amount {
        println!("Incorrect amount.");
        return Err(PlatformError::InputsError(error_location!()));
      }
      match amount_type {
        AmountType::Asset => {
          self.public_assets
              .push((Scalar::from(amount), asset_type_to_scalar(&code.val)));
        }
        _ => {
          self.public_liabilities
              .push((Scalar::from(amount), asset_type_to_scalar(&code.val)));
        }
      }
      Ok(())
    } else if let Some(commitments) = blind_asset_record.amount.get_commitments() {
      match amount_type {
        AmountType::Asset => {
          self.hidden_assets
              .push((Scalar::from(amount), asset_type_to_scalar(&code.val)));
          self.hidden_assets_commitments.push(commitments);
        }
        _ => {
          self.hidden_liabilities
              .push((Scalar::from(amount), asset_type_to_scalar(&code.val)));
          self.hidden_liabilities_commitments.push(commitments);
        }
      }
      Ok(())
    } else {
      println!("Amount commitments not found for confidential amount.");
      Err(PlatformError::InputsError(error_location!()))
    }
  }
}

/// Used to audit the solvency.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolvencyAudit {
  /// Table mapping each asset code to its conversion rate.
  pub conversion_rates: Vec<AssetCodeAndRate>,
}

impl SolvencyAudit {
  /// Sets conversion rate for the asset.
  pub fn set_rate(&mut self, code: AssetTypeCode, rate: u64) {
    self.conversion_rates
        .push((asset_type_to_scalar(&code.val), Scalar::from(rate)));
  }

  /// Proves the solvency and stores the commitments and proof.
  /// Must be used before `verify_solvency`.
  pub fn prove_solvency_and_store(&self,
                                  account: &mut AssetAndLiabilityAccount,
                                  asset_blinds: Vec<(Scalar, Scalar)>,
                                  liability_blinds: Vec<(Scalar, Scalar)>)
                                  -> Result<(), PlatformError> {
    // Prove the solvency
    let mut rates = LinearMap::new();
    for (code, rate) in self.conversion_rates.clone() {
      rates.insert(code, rate);
    }
    println!("Account {:?}", account.clone());
    println!("Asset blinds {:?}", asset_blinds);
    println!("Liability blinds {:?}", liability_blinds);

    let proof =
      prove_solvency(&account.hidden_assets,
                     &asset_blinds,
                     &account.public_assets,
                     &account.hidden_liabilities,
                     &liability_blinds,
                     &account.public_liabilities,
                     &rates).or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))?;

    // Update data
    account.proof = Some(proof.to_bytes());
    Ok(())
  }

  /// Verifies the solvency proof.
  /// Must not be used before `prove_solvency_and_store`.
  pub fn verify_solvency(&self, account: &AssetAndLiabilityAccount) -> Result<(), PlatformError> {
    let proof = if let Some(p) = &account.proof {
      R1CSProof::from_bytes(p).or(Err(PlatformError::DeserializationError))?
    } else {
      println!("Prove the solvency first.");
      return Err(PlatformError::InputsError(error_location!()));
    };
    let mut rates = LinearMap::new();
    for (code, rate) in self.conversion_rates.clone() {
      rates.insert(code, rate);
    }
    verify_solvency(&account.hidden_assets_commitments,
                    &account.public_assets,
                    &account.hidden_liabilities_commitments,
                    &account.public_liabilities,
                    &rates,
                    &proof).or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))
  }
}

/// For unit testing: defines an asset and submits the transaction.
fn test_define_and_submit_one(issuer_key_pair: &XfrKeyPair,
                              code: AssetTypeCode,
                              ledger_standalone: &LedgerStandalone)
                              -> Result<(), PlatformError> {
  // Define the asset
  let mut txn_builder = TransactionBuilder::default();
  let txn = txn_builder.add_operation_create_asset(issuer_key_pair,
                                                   Some(code),
                                                   AssetRules::default(),
                                                   "",
                                                   PolicyChoice::Fungible())?
                       .transaction();

  // Submit the transaction
  ledger_standalone.submit_transaction(&txn);

  Ok(())
}

/// For unit testing: defines three assets and submits the transactions.
pub fn test_define_and_submit_multiple(issuer_key_pair: &XfrKeyPair,
                                       codes: (AssetTypeCode, AssetTypeCode, AssetTypeCode),
                                       ledger_standalone: &LedgerStandalone)
                                       -> Result<(), PlatformError> {
  test_define_and_submit_one(issuer_key_pair, codes.0, ledger_standalone)?;
  test_define_and_submit_one(issuer_key_pair, codes.1, ledger_standalone)?;
  test_define_and_submit_one(issuer_key_pair, codes.2, ledger_standalone)
}

/// For unit testing: issues and transfers an asset, submits the transaction, and gets the UTXO SID and asset amount blind.
pub fn test_issue_transfer_and_get_utxo_and_blinds<R: CryptoRng + RngCore>(
  issuer_key_pair: &XfrKeyPair,
  recipient_key_pair: &XfrKeyPair,
  amount: u64,
  code: AssetTypeCode,
  record_type: AssetRecordType,
  sequence_number: u64,
  prng: &mut R,
  ledger_standalone: &LedgerStandalone)
  -> Result<(u64, (Scalar, Scalar)), PlatformError> {
  // Issue and transfer the asset
  let input_template =
    AssetRecordTemplate::with_no_asset_tracking(amount,
                                                code.val,
                                                AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                issuer_key_pair.get_pk());
  let (blind_asset_record, _, _) =
    build_blind_asset_record(prng, &PublicParams::new().pc_gens, &input_template, None);

  let output_template = AssetRecordTemplate::with_no_asset_tracking(amount,
                                                                    code.val,
                                                                    record_type,
                                                                    recipient_key_pair.get_pk());
  let mut txn_builder = TransferOperationBuilder::new();
  txn_builder.add_input(TxoRef::Relative(0),
      open_blind_asset_record(&blind_asset_record,
                        &None,
                        recipient_key_pair.get_sk_ref())
      .map_err(|e| PlatformError::ZeiError(error_location!(),e))?,
      amount)?;
  let (amount_blinds, _) = txn_builder.add_output_and_get_blinds(&output_template, None, prng)?;
  let xfr_op = txn_builder.balance()?
                          .create(TransferType::Standard)?
                          .sign(issuer_key_pair)?
                          .transaction()?;

  let mut txn_builder = TransactionBuilder::default();
  let txn = txn_builder.add_operation_issue_asset(issuer_key_pair,
                                                  &code,
                                                  sequence_number,
                                                  &[(TxOutput(blind_asset_record), None)],
                                                  None)?
                       .add_operation(xfr_op)
                       .transaction();

  // Submit the transaction
  Ok((ledger_standalone.submit_transaction_and_fetch_utxos(&txn)[0].0, amount_blinds))
}

#[cfg(test)]
mod tests {
  use super::*;
  use zei::errors::ZeiError;

  const PROTOCOL: &str = "http";
  const HOST: &str = "localhost";

  // Randomly generate three asset codes
  fn generate_codes() -> (AssetTypeCode, AssetTypeCode, AssetTypeCode) {
    (AssetTypeCode::gen_random(), AssetTypeCode::gen_random(), AssetTypeCode::gen_random())
  }

  // Add three public assets
  fn add_public_assets<R: CryptoRng + RngCore>(issuer_key_pair: &XfrKeyPair,
                                               receipient_key_pair: &XfrKeyPair,
                                               account: &mut AssetAndLiabilityAccount,
                                               codes: (AssetTypeCode,
                                                AssetTypeCode,
                                                AssetTypeCode),
                                               prng: &mut R,
                                               ledger_standalone: &LedgerStandalone)
                                               -> Result<(), PlatformError> {
    let (utxo_0, _) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  100,
                                                  codes.0,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  0,prng,
                                                  ledger_standalone)?;
    let (utxo_1, _) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  200,
                                                  codes.1,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  0,prng,
                                                  ledger_standalone)?;
    let (utxo_2, _) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  300,
                                                  codes.2,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  0,prng,
                                                  ledger_standalone)?;

    account.query_utxo_and_update_account(AmountType::Asset, 100, codes.0, utxo_0, PROTOCOL, HOST)?;
    account.query_utxo_and_update_account(AmountType::Asset, 200, codes.1, utxo_1, PROTOCOL, HOST)?;
    account.query_utxo_and_update_account(AmountType::Asset, 300, codes.2, utxo_2, PROTOCOL, HOST)?;

    Ok(())
  }

  // Add three hidden assets
  fn add_hidden_assets<R: CryptoRng + RngCore>(issuer_key_pair: &XfrKeyPair,
                                               receipient_key_pair: &XfrKeyPair,
                                               account: &mut AssetAndLiabilityAccount,
                                               blinds: &mut Vec<(Scalar, Scalar)>,
                                               codes: (AssetTypeCode,
                                                AssetTypeCode,
                                                AssetTypeCode),
                                               prng: &mut R,
                                               ledger_standalone: &LedgerStandalone)
                                               -> Result<(), PlatformError> {
    let (utxo_0, blinds_0) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  10,
                                                  codes.0,
                                                  AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                  1,prng,
                                                  ledger_standalone)?;
    let (utxo_1, blinds_1) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  20,
                                                  codes.1,
                                                  AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                  1,prng,
                                                  ledger_standalone)?;
    let (utxo_2, blinds_2) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  30,
                                                  codes.2,
                                                  AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                  1,prng,
                                                  ledger_standalone)?;

    account.query_utxo_and_update_account(AmountType::Asset, 10, codes.0, utxo_0, PROTOCOL, HOST)?;
    account.query_utxo_and_update_account(AmountType::Asset, 20, codes.1, utxo_1, PROTOCOL, HOST)?;
    account.query_utxo_and_update_account(AmountType::Asset, 30, codes.2, utxo_2, PROTOCOL, HOST)?;

    blinds.push(blinds_0);
    blinds.push(blinds_1);
    blinds.push(blinds_2);

    Ok(())
  }

  // Add three public liabilities
  fn add_public_liabilities<R: CryptoRng + RngCore>(issuer_key_pair: &XfrKeyPair,
                                                    receipient_key_pair: &XfrKeyPair,
                                                    account: &mut AssetAndLiabilityAccount,
                                                    codes: (AssetTypeCode,
                                                     AssetTypeCode,
                                                     AssetTypeCode),
                                                    prng: &mut R,
                                                    ledger_standalone: &LedgerStandalone)
                                                    -> Result<(), PlatformError> {
    let (utxo_0, _) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  100,
                                                  codes.0,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  2,prng,
                                                  ledger_standalone)?;
    let (utxo_1, _) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  200,
                                                  codes.1,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  2,prng,
                                                  ledger_standalone)?;
    let (utxo_2, _) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  200,
                                                  codes.2,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  2,prng,
                                                  ledger_standalone)?;

    account.query_utxo_and_update_account(AmountType::Liability,
                                          100,
                                          codes.0,
                                          utxo_0,
                                          PROTOCOL,
                                          HOST)?;
    account.query_utxo_and_update_account(AmountType::Liability,
                                          200,
                                          codes.1,
                                          utxo_1,
                                          PROTOCOL,
                                          HOST)?;
    account.query_utxo_and_update_account(AmountType::Liability,
                                          200,
                                          codes.2,
                                          utxo_2,
                                          PROTOCOL,
                                          HOST)?;

    Ok(())
  }

  // Add three hidden liabilities, with total value smaller than hidden assets'
  fn add_hidden_liabilities_smaller<R: CryptoRng + RngCore>(issuer_key_pair: &XfrKeyPair,
                                                            receipient_key_pair: &XfrKeyPair,
                                                            account: &mut AssetAndLiabilityAccount,
                                                            blinds: &mut Vec<(Scalar, Scalar)>,
                                                            codes: (AssetTypeCode,
                                                             AssetTypeCode,
                                                             AssetTypeCode),
                                                            prng: &mut R,
                                                            ledger_standalone: &LedgerStandalone)
                                                            -> Result<(), PlatformError> {
    let (utxo_0, blinds_0) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  10,
                                                  codes.0,
                                                  AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                  3,prng,
                                                  ledger_standalone)?;
    let (utxo_1, blinds_1) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  20,
                                                  codes.1,
                                                  AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                  3,prng,
                                                  ledger_standalone)?;
    let (utxo_2, blinds_2) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  20,
                                                  codes.2,
                                                  AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                  3,prng,
                                                  ledger_standalone)?;

    account.query_utxo_and_update_account(AmountType::Liability,
                                          10,
                                          codes.0,
                                          utxo_0,
                                          PROTOCOL,
                                          HOST)?;
    account.query_utxo_and_update_account(AmountType::Liability,
                                          20,
                                          codes.1,
                                          utxo_1,
                                          PROTOCOL,
                                          HOST)?;
    account.query_utxo_and_update_account(AmountType::Liability,
                                          20,
                                          codes.2,
                                          utxo_2,
                                          PROTOCOL,
                                          HOST)?;

    blinds.push(blinds_0);
    blinds.push(blinds_1);
    blinds.push(blinds_2);

    Ok(())
  }

  // Add three hidden liabilities, with total value larger than hidden assets'
  fn add_hidden_liabilities_larger<R: CryptoRng + RngCore>(issuer_key_pair: &XfrKeyPair,
                                                           receipient_key_pair: &XfrKeyPair,
                                                           account: &mut AssetAndLiabilityAccount,
                                                           blinds: &mut Vec<(Scalar, Scalar)>,
                                                           codes: (AssetTypeCode,
                                                            AssetTypeCode,
                                                            AssetTypeCode),
                                                           prng: &mut R,
                                                           ledger_standalone: &LedgerStandalone)
                                                           -> Result<(), PlatformError> {
    let (utxo_0, blinds_0) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  10,
                                                  codes.0,
                                                  AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                  4,prng,
                                                  ledger_standalone)?;
    let (utxo_1, blinds_1) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  20,
                                                  codes.1,
                                                  AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                  4,prng,
                                                  ledger_standalone)?;
    let (utxo_2, blinds_2) =
      test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                  receipient_key_pair,
                                                  40,
                                                  codes.2,
                                                  AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                  4,prng,
                                                  ledger_standalone)?;

    account.query_utxo_and_update_account(AmountType::Liability,
                                          10,
                                          codes.0,
                                          utxo_0,
                                          PROTOCOL,
                                          HOST)?;
    account.query_utxo_and_update_account(AmountType::Liability,
                                          20,
                                          codes.1,
                                          utxo_1,
                                          PROTOCOL,
                                          HOST)?;
    account.query_utxo_and_update_account(AmountType::Liability,
                                          40,
                                          codes.2,
                                          utxo_2,
                                          PROTOCOL,
                                          HOST)?;

    blinds.push(blinds_0);
    blinds.push(blinds_1);
    blinds.push(blinds_2);

    Ok(())
  }

  // Add asset conversion rates for all related assets
  fn add_conversion_rate_complete(audit: &mut SolvencyAudit,
                                  codes: (AssetTypeCode, AssetTypeCode, AssetTypeCode)) {
    audit.set_rate(codes.0, 1);
    audit.set_rate(codes.1, 2);
    audit.set_rate(codes.2, 3);
  }

  // Add asset conversion rates with one missing asset
  fn add_conversion_rate_incomplete(audit: &mut SolvencyAudit,
                                    codes: (AssetTypeCode, AssetTypeCode)) {
    audit.set_rate(codes.0, 1);
    audit.set_rate(codes.1, 2);
  }

  #[test]
  fn test_prove_solvency_fail() {
    // Start the standalone ledger
    let ledger_standalone = &LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Generate and define assets
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let codes = generate_codes();
    test_define_and_submit_multiple(issuer_key_pair, codes, ledger_standalone).unwrap();

    // Set asset conversion rates, but miss one asset
    add_conversion_rate_incomplete(&mut audit, (codes.0, codes.1));

    // Create an asset and liability account
    let mut account = AssetAndLiabilityAccount::default();
    let receipient_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let asset_blinds = &mut Vec::new();
    let liability_blinds = &mut Vec::new();
    let prng = &mut ChaChaRng::from_entropy();
    add_hidden_assets(issuer_key_pair,
                      receipient_key_pair,
                      &mut account,
                      asset_blinds,
                      codes,
                      prng,
                      ledger_standalone).unwrap();
    add_hidden_liabilities_smaller(issuer_key_pair,
                                   receipient_key_pair,
                                   &mut account,
                                   liability_blinds,
                                   codes,
                                   prng,
                                   ledger_standalone).unwrap();

    // Prove the solvency
    // Should fail with ZeiError::SolvencyProveError
    match audit.prove_solvency_and_store(&mut account,
                                         asset_blinds.to_vec(),
                                         liability_blinds.to_vec())
    {
      Err(PlatformError::ZeiError(_, ZeiError::SolvencyProveError)) => {}
      unexpected_result => {
        panic!(format!("Expected ZeiError::SolvencyProveError, found {:?}.",
                       unexpected_result));
      }
    }
  }

  #[test]
  #[ignore]
  fn test_verify_solvency_fail() {
    // Start the standalone ledger
    let ledger_standalone = &LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Generate and define assets
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let codes = generate_codes();
    test_define_and_submit_multiple(issuer_key_pair, codes, ledger_standalone).unwrap();

    // Set asset conversion rates
    add_conversion_rate_complete(&mut audit, codes);

    // Create an asset and liability account
    let mut account = AssetAndLiabilityAccount::default();
    let receipient_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let asset_blinds = &mut Vec::new();
    let liability_blinds = &mut Vec::new();
    let prng = &mut ChaChaRng::from_entropy();
    add_hidden_assets(issuer_key_pair,
                      receipient_key_pair,
                      &mut account,
                      asset_blinds,
                      codes,
                      prng,
                      ledger_standalone).unwrap();
    add_hidden_liabilities_smaller(issuer_key_pair,
                                   receipient_key_pair,
                                   &mut account,
                                   liability_blinds,
                                   codes,
                                   prng,
                                   ledger_standalone).unwrap();

    // Verify the solvency without a proof
    // Should fail with InputsError
    match audit.verify_solvency(&account) {
      Err(PlatformError::InputsError(_)) => {}
      unexpected_result => {
        panic!(format!("Expected InputsError, found {:?}.", unexpected_result));
      }
    }
  }

  #[test]
  #[ignore]
  fn test_prove_and_verify_solvency_fail() {
    // Start the standalone ledger
    let ledger_standalone = &LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Generate and define assets
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let codes = generate_codes();
    test_define_and_submit_multiple(issuer_key_pair, codes, ledger_standalone).unwrap();

    // Set asset conversion rates
    add_conversion_rate_complete(&mut audit, codes);

    // Create an asset and liability account
    let mut account = AssetAndLiabilityAccount::default();

    // Adds hidden assets
    let receipient_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let asset_blinds = &mut Vec::new();
    let liability_blinds = &mut Vec::new();
    let prng = &mut ChaChaRng::from_entropy();
    add_hidden_assets(issuer_key_pair,
                      receipient_key_pair,
                      &mut account,
                      asset_blinds,
                      codes,
                      prng,
                      ledger_standalone).unwrap();

    // Adds hidden liabilities, with total value larger than hidden assets'
    add_hidden_liabilities_larger(issuer_key_pair,
                                  receipient_key_pair,
                                  &mut account,
                                  liability_blinds,
                                  codes,
                                  prng,
                                  ledger_standalone).unwrap();

    // Prove the solvency
    audit.prove_solvency_and_store(&mut account,
                                   asset_blinds.to_vec(),
                                   liability_blinds.to_vec())
         .unwrap();
    assert!(account.proof.is_some());

    // Verify the solvency proof
    // Should fail with ZeiError::SolvencyVerificationError
    match audit.verify_solvency(&account) {
      Err(PlatformError::ZeiError(_, ZeiError::SolvencyVerificationError)) => {}
      unexpected_result => {
        panic!(format!("Expected ZeiError::SolvencyVerificationError, found {:?}.",
                       unexpected_result));
      }
    }
  }

  #[test]
  //   #[ignore]
  fn test_prove_and_verify_solvency_simple_pass() {
    // Start the standalone ledger
    let ledger_standalone = &LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Generate and define assets
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let code = AssetTypeCode::gen_random();
    test_define_and_submit_one(issuer_key_pair, code, ledger_standalone).unwrap();

    // Set asset conversion rates
    audit.set_rate(code, 1);

    // Create an asset and liability account
    let mut account = AssetAndLiabilityAccount::default();
    let receipient_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let asset_blinds = &mut Vec::new();
    let prng = &mut ChaChaRng::from_entropy();
    let (utxo, blinds) =
    test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                receipient_key_pair,
                                                10,
                                                code,
                                                AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                1,
                                                prng,
                                                ledger_standalone).unwrap();

    account.query_utxo_and_update_account(AmountType::Asset, 10, code, utxo, PROTOCOL, HOST)
           .unwrap();
    asset_blinds.push(blinds);

    // Prove the solvency
    audit.prove_solvency_and_store(&mut account,
                                   asset_blinds.to_vec(),
                                   Vec::<(Scalar, Scalar)>::new())
         .unwrap();
    assert!(account.proof.is_some());

    // Verify the solvency proof
    audit.verify_solvency(&account).unwrap();
  }

  #[test]
  //   #[ignore]
  fn test_prove_and_verify_solvency_pass() {
    // Start the standalone ledger
    let ledger_standalone = &LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Generate and define assets
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let codes = generate_codes();
    test_define_and_submit_multiple(issuer_key_pair, codes, ledger_standalone).unwrap();

    // Set asset conversion rates
    add_conversion_rate_complete(&mut audit, codes);

    // Create an asset and liability account
    let mut account = AssetAndLiabilityAccount::default();
    let receipient_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let asset_blinds = &mut Vec::new();
    // let liability_blinds = &mut Vec::new();
    let liability_blinds: Vec<(Scalar, Scalar)> = Vec::new();
    let prng = &mut ChaChaRng::from_entropy();
    // add_public_assets(issuer_key_pair,
    //                   receipient_key_pair,
    //                   &mut account,
    //                   codes,
    //                   prng,
    //                   ledger_standalone).unwrap();
    add_hidden_assets(issuer_key_pair,
                      receipient_key_pair,
                      &mut account,
                      asset_blinds,
                      codes,
                      prng,
                      ledger_standalone).unwrap();
    // add_public_liabilities(issuer_key_pair,
    //                        receipient_key_pair,
    //                        &mut account,
    //                        codes,
    //                        prng,
    //                        ledger_standalone).unwrap();
    // add_hidden_liabilities_smaller(issuer_key_pair,
    //                                receipient_key_pair,
    //                                &mut account,
    //                                liability_blinds,
    //                                codes,prng,
    //                                ledger_standalone).unwrap();

    // Prove the solvency
    audit.prove_solvency_and_store(&mut account,
                                   asset_blinds.to_vec(),
                                   liability_blinds.to_vec())
         .unwrap();
    assert!(account.proof.is_some());

    // Verify the solvency proof
    audit.verify_solvency(&account).unwrap();
  }

  #[test]
  //   #[ignore]
  fn test_update_asset_and_verify_solvency_mixed() {
    // Start the standalone ledger
    let ledger_standalone = &LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Generate and define assets
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let codes = generate_codes();
    test_define_and_submit_multiple(issuer_key_pair, codes, ledger_standalone).unwrap();

    // Set asset conversion rates
    add_conversion_rate_complete(&mut audit, codes);

    // Create an asset and liability account
    let mut account = AssetAndLiabilityAccount::default();
    let receipient_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let asset_blinds = &mut Vec::new();
    let liability_blinds = &mut Vec::new();
    let prng = &mut ChaChaRng::from_entropy();
    add_public_assets(issuer_key_pair,
                      receipient_key_pair,
                      &mut account,
                      codes,
                      prng,
                      ledger_standalone).unwrap();
    add_hidden_assets(issuer_key_pair,
                      receipient_key_pair,
                      &mut account,
                      asset_blinds,
                      codes,
                      prng,
                      ledger_standalone).unwrap();
    add_public_liabilities(issuer_key_pair,
                           receipient_key_pair,
                           &mut account,
                           codes,
                           prng,
                           ledger_standalone).unwrap();
    add_hidden_liabilities_smaller(issuer_key_pair,
                                   receipient_key_pair,
                                   &mut account,
                                   liability_blinds,
                                   codes,
                                   prng,
                                   ledger_standalone).unwrap();

    // Prove and verify the solvency
    audit.prove_solvency_and_store(&mut account,
                                   asset_blinds.to_vec(),
                                   liability_blinds.to_vec())
         .unwrap();
    audit.verify_solvency(&account).unwrap();

    // Update the public assets
    let (utxo, _) = test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                                     receipient_key_pair,
                                                                     40,
                                                                     codes.0,
                                                                     AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                                     5,prng,
                                                                     ledger_standalone).unwrap();
    account.query_utxo_and_update_account(AmountType::Liability, 40, codes.0, utxo, PROTOCOL, HOST)
           .unwrap();

    // Verify the solvency without proving it again
    // Should fail with InputsError
    match audit.verify_solvency(&account) {
      Err(PlatformError::InputsError(_)) => {}
      unexpected_result => {
        panic!(format!("Expected InputsError, found {:?}.", unexpected_result));
      }
    }

    // Prove the solvency again and verify the proof
    audit.prove_solvency_and_store(&mut account,
                                   asset_blinds.to_vec(),
                                   liability_blinds.to_vec())
         .unwrap();
    audit.verify_solvency(&account).unwrap();
  }

  #[test]
  //   #[ignore]
  fn test_update_liability_and_verify_solvency_fail() {
    // Start the standalone ledger
    let ledger_standalone = &LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Generate and define assets
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let codes = generate_codes();
    test_define_and_submit_multiple(issuer_key_pair, codes, ledger_standalone).unwrap();

    // Set asset conversion rates
    add_conversion_rate_complete(&mut audit, codes);

    // Create an asset and liability account
    let mut account = AssetAndLiabilityAccount::default();
    let receipient_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let asset_blinds = &mut Vec::new();
    let liability_blinds = &mut Vec::new();
    let prng = &mut ChaChaRng::from_entropy();
    add_public_assets(issuer_key_pair,
                      receipient_key_pair,
                      &mut account,
                      codes,
                      prng,
                      ledger_standalone).unwrap();
    add_hidden_assets(issuer_key_pair,
                      receipient_key_pair,
                      &mut account,
                      asset_blinds,
                      codes,
                      prng,
                      ledger_standalone).unwrap();
    add_public_liabilities(issuer_key_pair,
                           receipient_key_pair,
                           &mut account,
                           codes,
                           prng,
                           ledger_standalone).unwrap();
    add_hidden_liabilities_smaller(issuer_key_pair,
                                   receipient_key_pair,
                                   &mut account,
                                   liability_blinds,
                                   codes,
                                   prng,
                                   ledger_standalone).unwrap();

    // Prove and verify the solvency
    audit.prove_solvency_and_store(&mut account,
                                   asset_blinds.to_vec(),
                                   liability_blinds.to_vec())
         .unwrap();
    audit.verify_solvency(&account).unwrap();

    // Update the hidden liabilities
    let (utxo, blinds) = test_issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                                                     receipient_key_pair,
                                                                     4000,
                                                                     codes.0,
                                                                     AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                                                                     5,prng,
                                                                     ledger_standalone).unwrap();
    account.query_utxo_and_update_account(AmountType::Liability,
                                          4000,
                                          codes.0,
                                          utxo,
                                          PROTOCOL,
                                          HOST)
           .unwrap();
    liability_blinds.push(blinds);

    // Prove the solvency again
    audit.prove_solvency_and_store(&mut account,
                                   asset_blinds.to_vec(),
                                   liability_blinds.to_vec())
         .unwrap();

    // Verify the solvency proof
    // Should fail with SolvencyVerificationError
    match audit.verify_solvency(&account) {
      Err(PlatformError::ZeiError(_, ZeiError::SolvencyVerificationError)) => {}
      unexpected_result => {
        panic!(format!("Expected ZeiError::SolvencyVerificationError, found {:?}.",
                       unexpected_result));
      }
    }
  }
}
