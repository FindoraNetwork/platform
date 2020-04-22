#![deny(warnings)]
use bulletproofs::r1cs::R1CSProof;
use bulletproofs::PedersenGens;
use curve25519_dalek::ristretto::CompressedRistretto;
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::AssetTypeCode;
use ledger::error_location;
use linear_map::LinearMap;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::{Deserialize, Serialize};
use zei::crypto::solvency::{prove_solvency, verify_solvency};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::asset_record::open_blind_asset_record;
use zei::xfr::sig::XfrKeyPair;
use zei::xfr::structs::{asset_type_to_scalar, BlindAssetRecord};

const PROTOCOL: &str = "http";
const HOST: &str = "testnet.findora.org";
const QUERY_PORT: &str = "8669";

pub type AssetAmountAndCode = (Scalar, Scalar);
pub type AssetCodeAndRate = (Scalar, Scalar);
pub type AssetCommitment = (CompressedRistretto, CompressedRistretto);
pub type LiabilityCommitment = (CompressedRistretto, CompressedRistretto);

/// Asset and liability information, and associated solvency proof if exists
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AssetAndLiabilityAccount {
  /// Serialized key pair
  key_pair: String,

  /// Amount and code of the public assets
  pub public_assets: Vec<AssetAmountAndCode>,

  /// Amount and code of the hidden assets
  pub hidden_assets: Vec<AssetAmountAndCode>,

  /// Commitments to hidden assets, null iff any of the following:
  /// * Solvency hasn't been proved
  /// * Assets or liabilities have been updated
  pub hidden_assets_commitments: Option<Vec<AssetCommitment>>,

  /// Amount and code of the public liabilities
  pub public_liabilities: Vec<AssetAmountAndCode>,

  /// Amount and code of the hidden liabilities
  pub hidden_liabilities: Vec<AssetAmountAndCode>,

  /// Commitments to hidden liabilities, null iff any of the following:
  /// * Solvency hasn't been proved
  /// * Assets or liabilities have been updated
  pub hidden_liabilities_commitments: Option<Vec<LiabilityCommitment>>,

  /// Serialized solvency proof, null iff any of the following:
  /// * Solvency hasn't been proved
  /// * Assets or liabilities have been updated
  pub proof: Option<Vec<u8>>,
}

impl AssetAndLiabilityAccount {
  pub fn new(key_pair: XfrKeyPair) -> Result<Self, PlatformError> {
    let mut account = AssetAndLiabilityAccount::default();
    account.key_pair =
      serde_json::to_string(&key_pair).or_else(|_| Err(PlatformError::SerializationError))?;
    Ok(account)
  }

  /// Sets the commitments to hidden assets and liabilities, and the solvency proof to null.
  /// Used when the asset or liabilities are updated.
  fn remove_commitments_and_proof(&mut self) {
    self.hidden_assets_commitments = None;
    self.hidden_liabilities_commitments = None;
    self.proof = None;
  }

  fn get_key_pair(&mut self) -> Result<XfrKeyPair, PlatformError> {
    Ok(XfrKeyPair::zei_from_bytes(&hex::decode(&self.key_pair).or_else(|_| {
      Err(PlatformError::DeserializationError)
    })?))
  }

  /// Queries a UTXO SID to get the asset or liability amount.
  ///
  /// # Arguments
  /// * `utxo`: UTXO SID.
  fn query_utxo_and_get_amount(&mut self, utxo: u64) -> Result<Scalar, PlatformError> {
    let response = if let Ok(mut res) =
      reqwest::get(&format!("{}://{}:{}/utxo_sid/{}", PROTOCOL, HOST, QUERY_PORT, utxo))
    {
      res.text().or_else(|_| {
                   Err(PlatformError::SubmissionServerError(Some("Failed to query.".to_owned())))
                 })?
    } else {
      return Err(PlatformError::SubmissionServerError(Some("Failed to query.".to_owned())));
    };

    let blind_asset_record =
      serde_json::from_str::<BlindAssetRecord>(&response).or_else(|_| {
                                                           Err(PlatformError::DeserializationError)
                                                         })?;

    let key_pair = self.get_key_pair()?;

    // TODO (Keyao): Set owner memo
    let open_asset_record =
open_blind_asset_record(&blind_asset_record, &None, key_pair.get_sk_ref()).map_err(|e| {
                                       PlatformError::ZeiError(error_location!(), e)
                                     })?;

    Ok(Scalar::from(*open_asset_record.get_amount()))
  }

  /// Adds the commitments to hidden assets and liabilities, and the solvency proof.
  /// Used when the the solvency is proved.
  pub fn add_commitments_and_proof(&mut self,
                                   hidden_assets_commitments: Vec<AssetCommitment>,
                                   hidden_liabilities_commitments: Vec<LiabilityCommitment>,
                                   proof: R1CSProof) {
    self.hidden_assets_commitments = Some(hidden_assets_commitments);
    self.hidden_liabilities_commitments = Some(hidden_liabilities_commitments);
    self.proof = Some(proof.to_bytes());
  }

  /// Adds a public asset and remove the solvency proof.
  pub fn add_public_asset(&mut self, code: AssetTypeCode, utxo: u64) -> Result<(), PlatformError> {
    let amount = self.query_utxo_and_get_amount(utxo)?;
    self.public_assets
        .push((amount, asset_type_to_scalar(&code.val)));
    self.remove_commitments_and_proof();
    Ok(())
  }

  /// Adds a hidden asset and remove the solvency proof.
  pub fn add_hidden_asset(&mut self, code: AssetTypeCode, utxo: u64) -> Result<(), PlatformError> {
    let amount = self.query_utxo_and_get_amount(utxo)?;
    self.hidden_assets
        .push((amount, asset_type_to_scalar(&code.val)));
    self.remove_commitments_and_proof();
    Ok(())
  }

  /// Adds a public liability and remove the solvency proof.
  pub fn add_public_liability(&mut self,
                              code: AssetTypeCode,
                              utxo: u64)
                              -> Result<(), PlatformError> {
    let amount = self.query_utxo_and_get_amount(utxo)?;
    self.public_liabilities
        .push((amount, asset_type_to_scalar(&code.val)));
    self.remove_commitments_and_proof();
    Ok(())
  }

  /// Adds a hidden liability and remove the solvency proof.
  pub fn add_hidden_liability(&mut self,
                              code: AssetTypeCode,
                              utxo: u64)
                              -> Result<(), PlatformError> {
    let amount = self.query_utxo_and_get_amount(utxo)?;
    self.hidden_liabilities
        .push((amount, asset_type_to_scalar(&code.val)));
    self.remove_commitments_and_proof();
    Ok(())
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

  /// Geneartes a new asset and sets its conversion rate.
  /// Returns the generated asset code.
  pub fn set_asset_and_rate(&mut self, rate: u64) -> Scalar {
    let code = asset_type_to_scalar(&AssetTypeCode::gen_random().val);
    self.conversion_rates.push((code, Scalar::from(rate)));
    code
  }

  /// Proves the solvency and stores the commitments and proof.
  /// Must be used before `verify_solvency`.
  pub fn prove_solvency_and_store(&self,
                                  account: &mut AssetAndLiabilityAccount)
                                  -> Result<(), PlatformError> {
    // Prove the solvency
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let hidden_assets_size = account.hidden_assets.len();
    let hidden_liabilities_size = account.hidden_liabilities.len();
    let assets_hiddens =
      vec![(Scalar::random(&mut prng), Scalar::random(&mut prng)); hidden_assets_size];
    let liabilities_hiddens =
      vec![(Scalar::random(&mut prng), Scalar::random(&mut prng)); hidden_liabilities_size];
    let mut rates = LinearMap::new();
    for (code, rate) in self.conversion_rates.clone() {
      rates.insert(code, rate);
    }
    let proof =
      prove_solvency(&account.hidden_assets,
                     &assets_hiddens,
                     &account.public_assets,
                     &account.hidden_liabilities,
                     &liabilities_hiddens,
                     &account.public_liabilities,
                     &rates).or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))?;

    // Commit the hidden assets and liabilities
    let pc_gens = PedersenGens::default();
    let hidden_assets_commitments: Vec<AssetCommitment> =
      account.hidden_assets
             .iter()
             .zip(assets_hiddens.iter())
             .map(|((a, t), (ba, bt))| {
               (pc_gens.commit(*a, *ba).compress(), pc_gens.commit(*t, *bt).compress())
             })
             .collect();
    let hidden_liabilities_commitments: Vec<LiabilityCommitment> =
      account.hidden_liabilities
             .iter()
             .zip(liabilities_hiddens.iter())
             .map(|((a, t), (ba, bt))| {
               (pc_gens.commit(*a, *ba).compress(), pc_gens.commit(*t, *bt).compress())
             })
             .collect();

    // Update data
    account.add_commitments_and_proof(hidden_assets_commitments,
                                      hidden_liabilities_commitments,
                                      proof);
    Ok(())
  }

  /// Verifies the solvency proof.
  /// Must not be used before `prove_solvency_and_store`.
  pub fn verify_solvency(&self, account: &AssetAndLiabilityAccount) -> Result<(), PlatformError> {
    let hidden_assets_commitments = if let Some(commitments) = &account.hidden_assets_commitments {
      commitments
    } else {
      println!("Missing commitments to the hidden assets. Prove the solvency first.");
      return Err(PlatformError::InputsError(error_location!()));
    };
    let hidden_liabilities_commitments =
      if let Some(commitments) = &account.hidden_liabilities_commitments {
        commitments
      } else {
        println!("Missing commitments to the hidden liabilities. Prove the solvency first.");
        return Err(PlatformError::InputsError(error_location!()));
      };
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
    verify_solvency(hidden_assets_commitments,
                    &account.public_assets,
                    hidden_liabilities_commitments,
                    &account.public_liabilities,
                    &rates,
                    &proof).or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::{AssetRules, TransferType, TxOutput, TxoRef};
  use ledger_standalone::LedgerStandalone;
  use submission_server::{TxnHandle, TxnStatus};
  use txn_builder::BuildsTransactions;
  use txn_builder::{PolicyChoice, TransactionBuilder, TransferOperationBuilder};
  use zei::errors::ZeiError;
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::{build_blind_asset_record, AssetRecordType};
  use zei::xfr::structs::AssetRecordTemplate;

  const SUBMIT_PORT: &str = "8668";

  // Randomly generate three asset codes
  fn generate_codes() -> (AssetTypeCode, AssetTypeCode, AssetTypeCode) {
    (AssetTypeCode::gen_random(), AssetTypeCode::gen_random(), AssetTypeCode::gen_random())
  }

  fn submit(txn_builder: TransactionBuilder) -> Result<(), PlatformError> {
    // Submit transaction
    let client = reqwest::Client::new();
    let txn = txn_builder.transaction();
    let mut res =
      client.post(&format!("{}://{}:{}/{}",
                           PROTOCOL, HOST, SUBMIT_PORT, "submit_transaction"))
            .json(&txn)
            .send()
            .or_else(|_| {
              Err(PlatformError::SubmissionServerError(Some("Failed to submit.".to_owned())))
            })?;

    // Log body
    println!("Submission response: {}",
             res.json::<TxnHandle>().expect("<Invalid JSON>"));
    println!("Submission status: {}", res.status());
    Ok(())
  }

  fn submit_and_get_sid(txn_builder: TransactionBuilder) -> Result<u64, PlatformError> {
    // Submit transaction
    let client = reqwest::Client::new();
    let txn = txn_builder.transaction();
    let mut res =
      client.post(&format!("{}://{}:{}/{}",
                           PROTOCOL, HOST, SUBMIT_PORT, "submit_transaction"))
            .json(&txn)
            .send()
            .or_else(|_| {
              Err(PlatformError::SubmissionServerError(Some("Failed to submit.".to_owned())))
            })?;
    let handle = res.json::<TxnHandle>().expect("<Invalid JSON>");

    // Return sid
    let mut res = if let Ok(response) =
      reqwest::get(&format!("{}://{}:{}/txn_status/{}",
                            PROTOCOL, HOST, SUBMIT_PORT, &handle.0))
    {
      response
    } else {
      return Err(PlatformError::SubmissionServerError(Some("Failed to query.".to_owned())));
    };
    let res =
      res.text().or_else(|_| {
                   Err(PlatformError::SubmissionServerError(Some("Failed to query.".to_owned())))
                 })?;
    match serde_json::from_str::<TxnStatus>(&res).or_else(|_| {
                                                   Err(PlatformError::DeserializationError)
                                                 })? {
      TxnStatus::Committed((_sid, txos)) => Ok(txos[0].0),
      _ => Err(PlatformError::DeserializationError),
    }
  }

  fn define_and_submit_asset(issuer_key_pair: &XfrKeyPair,
                             code: AssetTypeCode)
                             -> Result<(), PlatformError> {
    let mut txn_builder = TransactionBuilder::default();
    txn_builder.add_operation_create_asset(issuer_key_pair,
                                           Some(code),
                                           AssetRules::default(),
                                           "",
                                           PolicyChoice::Fungible())?;
    submit(txn_builder)
  }

  fn issue_and_transfer_and_submit_asset(issuer_key_pair: &XfrKeyPair,
                                         recipient_key_pair: &XfrKeyPair,
                                         code: AssetTypeCode,
                                         amount: u64)
                                         -> Result<u64, PlatformError> {
    let input_template = AssetRecordTemplate::with_no_asset_tracking(amount, code.val, AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType, issuer_key_pair.get_pk());
    let mut prng = ChaChaRng::from_entropy();
    let params = PublicParams::new();
    let blind_asset_record =
      build_blind_asset_record(&mut prng, &params.pc_gens, &input_template, None).0;

    let output_template = AssetRecordTemplate::with_no_asset_tracking(amount,
                                                                      code.val,
                                                                      AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                                      recipient_key_pair.get_pk());
    let xfr_op =
    TransferOperationBuilder::new().add_input(TxoRef::Relative(0),
                                              open_blind_asset_record(&blind_asset_record,
                                                                &None,
                                                                issuer_key_pair.get_sk_ref())
                                              .map_err(|e| PlatformError::ZeiError(error_location!(),e))?,
                                              amount)?
                                   .add_output(&output_template, None)?
                                   .balance()?
                                   .create(TransferType::Standard)?
                                   .sign(issuer_key_pair)?
                                   .transaction()?;

    let mut txn_builder = TransactionBuilder::default();
    txn_builder.add_operation_issue_asset(issuer_key_pair,
                                          &code,
                                          1,
                                          &[(TxOutput(blind_asset_record), None)],
                                          None)?
               .add_operation(xfr_op)
               .transaction();

    submit_and_get_sid(txn_builder)
  }

  // Add three public assets
  fn _add_public_assets(issuer_key_pair: &XfrKeyPair,
                        account: &mut AssetAndLiabilityAccount,
                        codes: (AssetTypeCode, AssetTypeCode, AssetTypeCode))
                        -> Result<(), PlatformError> {
    let receipient_key_pair = &account.get_key_pair()?;
    let utxo_0 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.0, 100)?;
    let utxo_1 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.1, 200)?;
    let utxo_2 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.2, 300)?;

    account.add_public_asset(codes.0, utxo_0)?;
    account.add_public_asset(codes.1, utxo_1)?;
    account.add_public_asset(codes.2, utxo_2)?;

    Ok(())
  }

  // Add three hidden assets
  fn add_hidden_assets(issuer_key_pair: &XfrKeyPair,
                       account: &mut AssetAndLiabilityAccount,
                       codes: (AssetTypeCode, AssetTypeCode, AssetTypeCode))
                       -> Result<(), PlatformError> {
    let receipient_key_pair = &account.get_key_pair()?;
    let utxo_0 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.0, 10)?;
    let utxo_1 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.1, 20)?;
    let utxo_2 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.2, 30)?;

    account.add_hidden_asset(codes.0, utxo_0)?;
    account.add_hidden_asset(codes.1, utxo_1)?;
    account.add_hidden_asset(codes.2, utxo_2)?;

    Ok(())
  }

  // Add three public liabilities
  fn _add_public_liabilities(issuer_key_pair: &XfrKeyPair,
                             account: &mut AssetAndLiabilityAccount,
                             codes: (AssetTypeCode, AssetTypeCode, AssetTypeCode))
                             -> Result<(), PlatformError> {
    let receipient_key_pair = &account.get_key_pair()?;
    let utxo_0 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.0, 100)?;
    let utxo_1 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.1, 200)?;
    let utxo_2 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.2, 200)?;

    account.add_public_liability(codes.0, utxo_0)?;
    account.add_public_liability(codes.1, utxo_1)?;
    account.add_public_liability(codes.2, utxo_2)?;

    Ok(())
  }

  // Add three hidden liabilities, with total value smaller than hidden assets'
  fn add_hidden_liabilities_smaller(issuer_key_pair: &XfrKeyPair,
                                    account: &mut AssetAndLiabilityAccount,
                                    codes: (AssetTypeCode, AssetTypeCode, AssetTypeCode))
                                    -> Result<(), PlatformError> {
    let receipient_key_pair = &account.get_key_pair()?;
    let utxo_0 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.0, 10)?;
    let utxo_1 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.1, 20)?;
    let utxo_2 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.2, 20)?;

    account.add_hidden_liability(codes.0, utxo_0)?;
    account.add_hidden_liability(codes.1, utxo_1)?;
    account.add_hidden_liability(codes.2, utxo_2)?;

    Ok(())
  }

  // Add three hidden liabilities, with total value larger than hidden assets'
  fn _add_hidden_liabilities_larger(issuer_key_pair: &XfrKeyPair,
                                    account: &mut AssetAndLiabilityAccount,
                                    codes: (AssetTypeCode, AssetTypeCode, AssetTypeCode))
                                    -> Result<(), PlatformError> {
    let receipient_key_pair = &account.get_key_pair()?;
    let utxo_0 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.0, 10)?;
    let utxo_1 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.1, 20)?;
    let utxo_2 =
      issue_and_transfer_and_submit_asset(issuer_key_pair, receipient_key_pair, codes.2, 40)?;

    account.add_hidden_liability(codes.0, utxo_0)?;
    account.add_hidden_liability(codes.1, utxo_1)?;
    account.add_hidden_liability(codes.2, utxo_2)?;

    Ok(())
  }

  // Add asset conversion rates for all related assets
  fn _add_conversion_rate_complete(audit: &mut SolvencyAudit,
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
    let ledger_standalone = LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Generate and define assets
    let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let codes = generate_codes();
    define_and_submit_asset(issuer_key_pair, codes.0).unwrap();
    define_and_submit_asset(issuer_key_pair, codes.1).unwrap();
    define_and_submit_asset(issuer_key_pair, codes.2).unwrap();

    // Set asset conversion rates, but miss one asset
    add_conversion_rate_incomplete(&mut audit, (codes.0, codes.1));

    // Create an asset and liability account
    let mut account =
      AssetAndLiabilityAccount::new(XfrKeyPair::generate(&mut ChaChaRng::from_entropy())).unwrap();

    // Adds hidden assets and liabilities
    add_hidden_assets(issuer_key_pair, &mut account, codes).unwrap();
    add_hidden_liabilities_smaller(issuer_key_pair, &mut account, codes).unwrap();

    // Prove the solvency
    // Should fail with ZeiError::SolvencyProveError
    match audit.prove_solvency_and_store(&mut account) {
      Err(PlatformError::ZeiError(_, ZeiError::SolvencyProveError)) => {}
      unexpected_result => {
        panic!(format!("Expected ZeiError::SolvencyVerificationError, found {:?}.",
                       unexpected_result));
      }
    }
  }

  // #[test]
  // fn test_verify_solvency_fail() {
  //   // Start a solvency audit process and set the asset conversion rates
  //   let mut audit = SolvencyAudit::default();
  //   let codes = add_conversion_rate_complete(&mut audit);

  //   // Create a asset and liability account
  //   let mut account = &mut AssetAndLiabilityAccount::default();

  //   // Adds hidden assets
  //   add_hidden_assets(&mut account, codes);

  //   // Adds hidden liabilities, with total value larger than hidden assets'
  //   add_hidden_liabilities_larger(&mut account, codes);

  //   // Verify the solvency without a proof
  //   // Should fail with InputsError
  //   match audit.verify_solvency(&account) {
  //     Err(PlatformError::InputsError(_)) => {}
  //     unexpected_result => {
  //       panic!(format!("Expected InputsError, found {:?}.", unexpected_result));
  //     }
  //   }
  // }

  // #[test]
  // fn test_prove_and_verify_solvency_fail() {
  //   // Start a solvency audit process and set the asset conversion rates
  //   let mut audit = SolvencyAudit::default();
  //   let codes = add_conversion_rate_complete(&mut audit);

  //   // Create a asset and liability account
  //   let mut account = &mut AssetAndLiabilityAccount::default();

  //   // Adds hidden assets
  //   add_hidden_assets(&mut account, codes);

  //   // Adds hidden liabilities, with total value larger than hidden assets'
  //   add_hidden_liabilities_larger(&mut account, codes);

  //   // Prove the solvency
  //   audit.prove_solvency_and_store(&mut account).unwrap();
  //   assert!(account.hidden_assets_commitments.is_some());
  //   assert!(account.hidden_liabilities_commitments.is_some());
  //   assert!(account.proof.is_some());

  //   // Verify the solvency proof
  //   // Should fail with ZeiError::SolvencyVerificationError
  //   match audit.verify_solvency(&account) {
  //     Err(PlatformError::ZeiError(_, ZeiError::SolvencyVerificationError)) => {}
  //     unexpected_result => {
  //       panic!(format!("Expected ZeiError::SolvencyVerificationError, found {:?}.",
  //                      unexpected_result));
  //     }
  //   }
  // }

  // #[test]
  // fn test_prove_and_verify_solvency_pass() {
  //   // Start a solvency audit process and set the asset conversion rates
  //   let mut audit = SolvencyAudit::default();
  //   let codes = add_conversion_rate_complete(&mut audit);

  //   // Create an account and add assets and liabilities
  //   let mut account = &mut AssetAndLiabilityAccount::default();
  //   add_public_assets(&mut account, codes);
  //   add_hidden_assets(&mut account, codes);
  //   add_public_liabilities(&mut account, codes);
  //   add_hidden_liabilities_smaller(&mut account, codes);

  //   // Prove the solvency and verify the commitments and proof are stored
  //   audit.prove_solvency_and_store(&mut account).unwrap();
  //   assert!(account.hidden_assets_commitments.is_some());
  //   assert!(account.hidden_liabilities_commitments.is_some());
  //   assert!(account.proof.is_some());

  //   // Verify the solvency proof
  //   audit.verify_solvency(&account).unwrap();
  // }

  // #[test]
  // fn test_update_asset_and_verify_solvency_mixed() {
  //   // Start a solvency audit process and set the asset conversion rates
  //   let mut audit = SolvencyAudit::default();
  //   let codes = add_conversion_rate_complete(&mut audit);

  //   // Create an account and add assets and liabilities
  //   let mut account = &mut AssetAndLiabilityAccount::default();
  //   add_public_assets(&mut account, codes);
  //   add_hidden_assets(&mut account, codes);
  //   add_public_liabilities(&mut account, codes);
  //   add_hidden_liabilities_smaller(&mut account, codes);

  //   // Prove and verify the solvency
  //   audit.prove_solvency_and_store(&mut account).unwrap();
  //   audit.verify_solvency(&account).unwrap();

  //   // Update the public assets and verify the commitments and proof are removed
  //   account.add_public_asset(40, codes.0);
  //   assert!(account.hidden_assets_commitments.is_none());
  //   assert!(account.hidden_liabilities_commitments.is_none());
  //   assert!(account.proof.is_none());

  //   // Verify the solvency without proving it again
  //   // Should fail with InputsError
  //   match audit.verify_solvency(&account) {
  //     Err(PlatformError::InputsError(_)) => {}
  //     unexpected_result => {
  //       panic!(format!("Expected InputsError, found {:?}.", unexpected_result));
  //     }
  //   }

  //   // Prove the solvency again and verify the proof
  //   audit.prove_solvency_and_store(&mut account).unwrap();
  //   audit.verify_solvency(&account).unwrap();
  // }

  // #[test]
  // fn test_update_liability_and_verify_solvency_fail() {
  //   // Start a solvency audit process and set the asset conversion rates
  //   let mut audit = SolvencyAudit::default();
  //   let codes = add_conversion_rate_complete(&mut audit);

  //   // Create an account and add assets and liabilities
  //   let mut account = &mut AssetAndLiabilityAccount::default();
  //   add_public_assets(&mut account, codes);
  //   add_hidden_assets(&mut account, codes);
  //   add_public_liabilities(&mut account, codes);
  //   add_hidden_liabilities_smaller(&mut account, codes);

  //   // Prove and verify the solvency
  //   audit.prove_solvency_and_store(&mut account).unwrap();
  //   audit.verify_solvency(&account).unwrap();

  //   // Update the hidden liability
  //   // to make the liabilities' total value greater than assets'
  //   account.add_hidden_liability(4000, codes.0);

  //   // Prove the solvency again
  //   audit.prove_solvency_and_store(&mut account).unwrap();

  //   // Verify the solvency proof
  //   // Should fail with SolvencyVerificationError
  //   match audit.verify_solvency(&account) {
  //     Err(PlatformError::ZeiError(_, ZeiError::SolvencyVerificationError)) => {}
  //     unexpected_result => {
  //       panic!(format!("Expected ZeiError::SolvencyVerificationError, found {:?}.",
  //                      unexpected_result));
  //     }
  //   }
  // }
}
