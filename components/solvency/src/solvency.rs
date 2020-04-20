#![deny(warnings)]
use bulletproofs::r1cs::R1CSProof;
use bulletproofs::PedersenGens;
use clap::{App, Arg, SubCommand};
use curve25519_dalek::ristretto::CompressedRistretto;
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::AssetTypeCode;
use ledger::error_location;
use linear_map::LinearMap;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::{Deserialize, Serialize};
use std::fs;
use zei::crypto::solvency::{prove_solvency, verify_solvency};
use zei::xfr::structs::asset_type_to_scalar;

/// Path to the data file.
const DATA_FILE: &str = "solvency_data.json";

pub type AssetAmountAndCode = (Scalar, Scalar);
pub type AssetCodeAndRate = (Scalar, Scalar);
pub type AssetCommitment = (CompressedRistretto, CompressedRistretto);
pub type LiabilityCommitment = (CompressedRistretto, CompressedRistretto);

/// Asset and liability information, and associated solvency proof if exists
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AssetAndLiabilityAccount {
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

  /// Solvency proof, null iff any of the following:
  /// * Solvency hasn't been proved
  /// * Assets or liabilities have been updated
  pub proof: Option<R1CSProof>,
}

impl AssetAndLiabilityAccount {
  /// Sets the commitments to hidden assets and liabilities, and the solvency proof to null.
  /// Used when the asset or liabilities are updated.
  fn remove_commitments_and_proof(&mut self) {
    self.hidden_assets_commitments = None;
    self.hidden_liabilities_commitments = None;
    self.proof = None;
  }

  /// Adds the commitments to hidden assets and liabilities, and the solvency proof.
  /// Used when the the solvency is proved.
  pub fn add_commitments_and_proof(&mut self,
                                   hidden_assets_commitments: Vec<AssetCommitment>,
                                   hidden_liabilities_commitments: Vec<LiabilityCommitment>,
                                   proof: R1CSProof) {
    self.hidden_assets_commitments = Some(hidden_assets_commitments);
    self.hidden_liabilities_commitments = Some(hidden_liabilities_commitments);
    self.proof = Some(proof);
  }

  /// Adds a public asset and remove the solvency proof.
  pub fn add_public_asset(&mut self, amount: u64, code: Scalar) {
    self.public_assets.push((Scalar::from(amount), code));
    self.remove_commitments_and_proof();
  }

  /// Adds a hidden asset and remove the solvency proof.
  pub fn add_hidden_asset(&mut self, amount: u64, code: Scalar) {
    self.hidden_assets.push((Scalar::from(amount), code));
    self.remove_commitments_and_proof();
  }

  /// Adds a public liability and remove the solvency proof.
  pub fn add_public_liability(&mut self, amount: u64, code: Scalar) {
    self.public_liabilities.push((Scalar::from(amount), code));
    self.remove_commitments_and_proof();
  }

  /// Adds a hidden liability and remove the solvency proof.
  pub fn add_hidden_liability(&mut self, amount: u64, code: Scalar) {
    self.hidden_liabilities.push((Scalar::from(amount), code));
    self.remove_commitments_and_proof();
  }
}

/// Used to audit the solvency.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolvencyAudit {
  /// Table mapping each asset code to its conversion rate.
  conversion_rates: Vec<AssetCodeAndRate>,
}

impl SolvencyAudit {
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
      p
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
                    proof).or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))
  }
}

// TODO (Keyao): Serialize codes
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
/// Information of assets, liabilities and conversion rates.
struct AssetLiabilityAndRateData {
  /// Asset and liability account
  asset_and_liability_account: AssetAndLiabilityAccount,

  /// Contains the asset conversion rate
  solvency_audit: SolvencyAudit,
}

/// Stores the program data to `DATA_FILE`, when the program starts or the data is updated.
fn store_data_to_file(data: AssetLiabilityAndRateData) -> Result<(), PlatformError> {
  if let Ok(as_json) = serde_json::to_string(&data) {
    if let Err(error) = fs::write(DATA_FILE, &as_json) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                DATA_FILE, error)));
    };
  } else {
  }
  Ok(())
}

/// Loads data.
/// * If the data file exists, loads data from it.
/// * Otherwise, stores the initial data to file and returns the data.
fn load_data() -> Result<AssetLiabilityAndRateData, PlatformError> {
  let data = match fs::read_to_string(DATA_FILE) {
    Ok(data) => data,
    Err(_) => {
      let init_data = AssetLiabilityAndRateData::default();
      store_data_to_file(init_data.clone())?;
      return Ok(init_data);
    }
  };

  serde_json::from_str::<AssetLiabilityAndRateData>(&data).or(Err(PlatformError::DeserializationError))
}

/// Parses a string to u64.
fn parse_to_u64(val_str: &str) -> Result<u64, PlatformError> {
  val_str.trim()
         .parse::<u64>()
         .or_else(|_| Err(PlatformError::InputsError(error_location!())))
}

/// Processes input commands and arguments.
fn process_inputs(inputs: clap::ArgMatches) -> Result<(), PlatformError> {
  let mut data = load_data()?;
  match inputs.subcommand() {
    ("set_asset_and_rate", Some(set_matches)) => {
      let rate = if let Some(rate_arg) = set_matches.value_of("rate") {
        parse_to_u64(rate_arg)?
      } else {
        println!("Missing conversion rate. Use --rate.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      data.solvency_audit.set_asset_and_rate(rate);
      store_data_to_file(data)
    }
    ("add_asset_or_liability", Some(add_matches)) => {
      let code = if let Some(id_arg) = add_matches.value_of("id") {
        data.solvency_audit.conversion_rates[parse_to_u64(id_arg)? as usize].0
      } else {
        println!("Missing asset or liability id. Use --id.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      let amount = if let Some(amount_arg) = add_matches.value_of("amount") {
        parse_to_u64(amount_arg)?
      } else {
        println!("Missing asset or liability amount. Use --amount.");
        return Err(PlatformError::InputsError(error_location!()));
      };
      if let Some(type_arg) = add_matches.value_of("type") {
        match type_arg {
          "public_asset" => data.asset_and_liability_account
                                .add_public_asset(amount, code),
          "hidden_asset" => data.asset_and_liability_account
                                .add_hidden_asset(amount, code),
          "public_liability" => data.asset_and_liability_account
                                    .add_public_liability(amount, code),
          _ => data.asset_and_liability_account
                   .add_hidden_liability(amount, code),
        }
      } else {
        println!("Missing asset or liability type. Use --type.");
        return Err(PlatformError::InputsError(error_location!()));
      }
      store_data_to_file(data)
    }
    ("prove_and_verify_solvency", _) => {
      data.solvency_audit
          .prove_solvency_and_store(&mut data.asset_and_liability_account)?;
      data.solvency_audit
          .verify_solvency(&mut data.asset_and_liability_account)?;
      store_data_to_file(data)
    }
    _ => {
      println!("Subcommand missing or not recognized. Try --help");
      Err(PlatformError::InputsError(error_location!()))
    }
  }
}

fn main() -> Result<(), PlatformError> {
  let inputs = App::new("Solvency Proof").version("0.1.0").about("Copyright 2020 © Findora. All rights reserved.")
    .subcommand(SubCommand::with_name("set_asset_and_rate")
      .arg(Arg::with_name("rate")
        .short("r")
        .long("rate")
        .required(true)
        .takes_value(true)
        .help("Conversion rate of this asset.")))
    .subcommand(SubCommand::with_name("add_asset_or_liability")
      .arg(Arg::with_name("type")
        .short("t")
        .long("type")
        .required(true)
        .takes_value(true)
        .possible_values(&["public_asset", "hidden_asset", "public_liability", "hidden_liability"])
        .help("Specify whether to add asset or liability, and whether the record is public or hidden."))
      .arg(Arg::with_name("id")
        .short("i")
        .long("id")
        .required(true)
        .takes_value(true)
        .help("Index in the conversion rate table."))
      .arg(Arg::with_name("amount")
        .short("a")
        .long("amount")
        .required(true)
        .takes_value(true)
        .help("Amount of this asset or liability.")))
    .subcommand(SubCommand::with_name("prove_and_verify_solvency"))
    .get_matches();

  process_inputs(inputs)
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::io::{self, Write};
  use std::process::{Command, Output};
  use zei::errors::ZeiError;

  const COMMAND: &str = "../../target/debug/solvency";

  // Add three public assets
  fn add_public_assets(account: &mut AssetAndLiabilityAccount, codes: (Scalar, Scalar, Scalar)) {
    account.add_public_asset(100, codes.0);
    account.add_public_asset(200, codes.1);
    account.add_public_asset(300, codes.2);
  }

  // Add three hidden assets
  fn add_hidden_assets(account: &mut AssetAndLiabilityAccount, codes: (Scalar, Scalar, Scalar)) {
    account.add_hidden_asset(10, codes.0);
    account.add_hidden_asset(20, codes.1);
    account.add_hidden_asset(30, codes.2);
  }

  // Add three public liabilities
  fn add_public_liabilities(account: &mut AssetAndLiabilityAccount,
                            codes: (Scalar, Scalar, Scalar)) {
    account.add_public_asset(100, codes.0);
    account.add_public_asset(200, codes.1);
    account.add_public_asset(200, codes.2);
  }

  // Add three hidden liabilities, with total value smaller than hidden assets'
  fn add_hidden_liabilities_smaller(account: &mut AssetAndLiabilityAccount,
                                    codes: (Scalar, Scalar, Scalar)) {
    account.add_hidden_liability(10, codes.0);
    account.add_hidden_liability(20, codes.1);
    account.add_hidden_liability(20, codes.2);
  }

  // Add three hidden liabilities, with total value larger than hidden assets'
  fn add_hidden_liabilities_larger(account: &mut AssetAndLiabilityAccount,
                                   codes: (Scalar, Scalar, Scalar)) {
    account.add_hidden_liability(10, codes.0);
    account.add_hidden_liability(20, codes.1);
    account.add_hidden_liability(40, codes.2);
  }

  // Add asset conversion rates for all related assets
  fn add_conversion_rate_complete(audit: &mut SolvencyAudit) -> (Scalar, Scalar, Scalar) {
    (audit.set_asset_and_rate(1), audit.set_asset_and_rate(2), audit.set_asset_and_rate(3))
  }

  // Add asset conversion rates with one missing asset
  fn add_conversion_rate_incomplete(audit: &mut SolvencyAudit) -> (Scalar, Scalar) {
    (audit.set_asset_and_rate(1), audit.set_asset_and_rate(2))
  }

  // Command to set asset conversion rates
  fn set_asset_and_rate_cmd(rate: &str) -> io::Result<Output> {
    Command::new(COMMAND).arg("set_asset_and_rate")
                         .args(&["--rate", rate])
                         .output()
  }

  // Command to add an asset or a liability
  fn add_asset_or_liability_cmd(add_type: &str, id: &str, amount: &str) -> io::Result<Output> {
    Command::new(COMMAND).arg("add_asset_or_liability")
                         .args(&["--type", add_type])
                         .args(&["--id", id])
                         .args(&["--amount", amount])
                         .output()
  }

  #[test]
  fn test_prove_solvency_fail() {
    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Set asset conversion rates, but miss one asset
    let (codes_0, codes_1) = add_conversion_rate_incomplete(&mut audit);
    let code_2 = asset_type_to_scalar(&AssetTypeCode::gen_random().val);

    // Create an asset and liability account
    let mut account = &mut AssetAndLiabilityAccount::default();

    // Adds hidden assets and liabilities
    add_hidden_assets(&mut account, (codes_0, codes_1, code_2));
    add_hidden_liabilities_smaller(&mut account, (codes_0, codes_1, code_2));

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

  #[test]
  fn test_verify_solvency_fail() {
    // Start a solvency audit process and set the asset conversion rates
    let mut audit = SolvencyAudit::default();
    let codes = add_conversion_rate_complete(&mut audit);

    // Create a asset and liability account
    let mut account = &mut AssetAndLiabilityAccount::default();

    // Adds hidden assets
    add_hidden_assets(&mut account, codes);

    // Adds hidden liabilities, with total value larger than hidden assets'
    add_hidden_liabilities_larger(&mut account, codes);

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
  fn test_prove_and_verify_solvency_fail() {
    // Start a solvency audit process and set the asset conversion rates
    let mut audit = SolvencyAudit::default();
    let codes = add_conversion_rate_complete(&mut audit);

    // Create a asset and liability account
    let mut account = &mut AssetAndLiabilityAccount::default();

    // Adds hidden assets
    add_hidden_assets(&mut account, codes);

    // Adds hidden liabilities, with total value larger than hidden assets'
    add_hidden_liabilities_larger(&mut account, codes);

    // Prove the solvency
    audit.prove_solvency_and_store(&mut account).unwrap();
    assert!(account.hidden_assets_commitments.is_some());
    assert!(account.hidden_liabilities_commitments.is_some());
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
  fn test_prove_and_verify_solvency_pass() {
    // Start a solvency audit process and set the asset conversion rates
    let mut audit = SolvencyAudit::default();
    let codes = add_conversion_rate_complete(&mut audit);

    // Create an account and add assets and liabilities
    let mut account = &mut AssetAndLiabilityAccount::default();
    add_public_assets(&mut account, codes);
    add_hidden_assets(&mut account, codes);
    add_public_liabilities(&mut account, codes);
    add_hidden_liabilities_smaller(&mut account, codes);

    // Prove the solvency and verify the commitments and proof are stored
    audit.prove_solvency_and_store(&mut account).unwrap();
    assert!(account.hidden_assets_commitments.is_some());
    assert!(account.hidden_liabilities_commitments.is_some());
    assert!(account.proof.is_some());

    // Verify the solvency proof
    audit.verify_solvency(&account).unwrap();
  }

  #[test]
  fn test_update_asset_and_verify_solvency_mixed() {
    // Start a solvency audit process and set the asset conversion rates
    let mut audit = SolvencyAudit::default();
    let codes = add_conversion_rate_complete(&mut audit);

    // Create an account and add assets and liabilities
    let mut account = &mut AssetAndLiabilityAccount::default();
    add_public_assets(&mut account, codes);
    add_hidden_assets(&mut account, codes);
    add_public_liabilities(&mut account, codes);
    add_hidden_liabilities_smaller(&mut account, codes);

    // Prove and verify the solvency
    audit.prove_solvency_and_store(&mut account).unwrap();
    audit.verify_solvency(&account).unwrap();

    // Update the public assets and verify the commitments and proof are removed
    account.add_public_asset(40, codes.0);
    assert!(account.hidden_assets_commitments.is_none());
    assert!(account.hidden_liabilities_commitments.is_none());
    assert!(account.proof.is_none());

    // Verify the solvency without proving it again
    // Should fail with InputsError
    match audit.verify_solvency(&account) {
      Err(PlatformError::InputsError(_)) => {}
      unexpected_result => {
        panic!(format!("Expected InputsError, found {:?}.", unexpected_result));
      }
    }

    // Prove the solvency again and verify the proof
    audit.prove_solvency_and_store(&mut account).unwrap();
    audit.verify_solvency(&account).unwrap();
  }

  #[test]
  fn test_update_liability_and_verify_solvency_fail() {
    // Start a solvency audit process and set the asset conversion rates
    let mut audit = SolvencyAudit::default();
    let codes = add_conversion_rate_complete(&mut audit);

    // Create an account and add assets and liabilities
    let mut account = &mut AssetAndLiabilityAccount::default();
    add_public_assets(&mut account, codes);
    add_hidden_assets(&mut account, codes);
    add_public_liabilities(&mut account, codes);
    add_hidden_liabilities_smaller(&mut account, codes);

    // Prove and verify the solvency
    audit.prove_solvency_and_store(&mut account).unwrap();
    audit.verify_solvency(&account).unwrap();

    // Update the hidden liability
    // to make the liabilities' total value greater than assets'
    account.add_hidden_liability(4000, codes.0);

    // Prove the solvency again
    audit.prove_solvency_and_store(&mut account).unwrap();

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

  #[test]
  fn test_cmd() {
    // Set asset conversion rates
    let output = set_asset_and_rate_cmd("1").expect("Failed to set conversion rate.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output = set_asset_and_rate_cmd("100").expect("Failed to set conversion rate.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    // Add assets and liabilities
    let output =
      add_asset_or_liability_cmd("hidden_asset", "0", "10").expect("Failed to add public asset.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output =
      add_asset_or_liability_cmd("hidden_asset", "1", "200").expect("Failed to add hidden asset.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output = add_asset_or_liability_cmd("hidden_liability","0","30")
                                      .expect("Failed to add public liability.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    let output = add_asset_or_liability_cmd("hidden_liability","1","40")
                                      .expect("Failed to add hidden liability.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    // Prove solvency
    let output = Command::new(COMMAND).arg("prove_and_verify_solvency")
                                      .output()
                                      .expect("Failed to prove and verify solvency.");
    io::stdout().write_all(&output.stdout).unwrap();
    io::stdout().write_all(&output.stderr).unwrap();
    assert!(output.status.success());

    fs::remove_file("solvency_data.json").unwrap();
  }
}
