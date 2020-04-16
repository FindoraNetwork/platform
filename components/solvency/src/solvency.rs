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
use zei::crypto::solvency::{prove_solvency, verify_solvency};
use zei::xfr::structs::asset_type_to_scalar;

pub type AssetAmountAndCode = (Scalar, Scalar);
pub type AssetCommitment = (CompressedRistretto, CompressedRistretto);
pub type LiabilityCommitment = (CompressedRistretto, CompressedRistretto);
pub type AssetConversionTable = LinearMap<Scalar, Scalar>;

/// Converts a set of asset amount and code to a scalar set.
/// This is used for proving and verifying solvency.
pub fn amount_and_code_to_scalars(amount: u64, code: AssetTypeCode) -> AssetAmountAndCode {
  (asset_type_to_scalar(&code.val), Scalar::from(amount))
}

/// Asset and liability information, and associated solvency proof if exists
#[derive(Default)]
pub struct AssetAndLiabilityAccount {
  /// Amount and code of the public assets
  pub public_assets: Vec<AssetAmountAndCode>,

  /// Amount and code of the hidden assets
  pub hidden_assets: Vec<AssetAmountAndCode>,

  /// Commitments to hidden assets, null iff any of the following:
  /// * Solvency hasn't been proved
  /// * Assets or liabilities have been updated
  // TODO (Keyao): no need to remove the proof if the asset is updated?
  pub hidden_assets_commitments: Option<Vec<AssetCommitment>>,

  /// Amount and code of the public liabilities
  pub public_liabilities: Vec<AssetAmountAndCode>,

  /// Amount and code of the hidden liabilities
  pub hidden_liabilities: Vec<AssetAmountAndCode>,

  /// Commitments to hidden liabilities, null iff any of the following:
  /// * Solvency hasn't been proved
  /// * Assets or liabilities have been updated
  // TODO (Keyao): no need to remove the proof if the asset is updated?
  pub hidden_liabilities_commitments: Option<Vec<LiabilityCommitment>>,

  /// Solvency proof, null iff any of the following:
  /// * Solvency hasn't been proved
  /// * Assets or liabilities have been updated
  // TODO (Keyao): no need to remove the proof if the asset is updated?
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
  // TODO (Keyao): no need to remove the proof if the asset is updated?
  pub fn add_public_asset(&mut self, amount: u64, code: AssetTypeCode) {
    self.public_assets
        .push(amount_and_code_to_scalars(amount, code));
    self.remove_commitments_and_proof();
  }

  /// Adds a hidden asset and remove the solvency proof.
  // TODO (Keyao): no need to remove the proof if the asset is updated?
  pub fn add_hidden_asset(&mut self, amount: u64, code: AssetTypeCode) {
    self.hidden_assets
        .push(amount_and_code_to_scalars(amount, code));
    self.remove_commitments_and_proof();
  }

  /// Adds a public liability and remove the solvency proof.
  pub fn add_public_liability(&mut self, amount: u64, code: AssetTypeCode) {
    self.public_liabilities
        .push(amount_and_code_to_scalars(amount, code));
    self.remove_commitments_and_proof();
  }

  /// Adds a hidden liability and remove the solvency proof.
  pub fn add_hidden_liability(&mut self, amount: u64, code: AssetTypeCode) {
    self.hidden_liabilities
        .push(amount_and_code_to_scalars(amount, code));
    self.remove_commitments_and_proof();
  }
}

/// Used to audit the solvency.
#[derive(Default)]
pub struct SolvencyAudit {
  /// Table mapping each asset code to its conversion rate.
  conversion_rates: AssetConversionTable,
}

impl SolvencyAudit {
  /// Adds or overwrites the conversion table.
  /// * If the asset code doesn't exist in the map, adds it with the conversion rate.
  /// * Otherwise, overwrite the original conversion rate.
  pub fn add_or_overwrite(&mut self, code: AssetTypeCode, rate: u64) {
    self.conversion_rates
        .insert(asset_type_to_scalar(&code.val), Scalar::from(rate));
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
    let assets_blinds =
      vec![(Scalar::random(&mut prng), Scalar::random(&mut prng)); hidden_assets_size];
    let liabilities_blinds =
      vec![(Scalar::random(&mut prng), Scalar::random(&mut prng)); hidden_liabilities_size];
    let proof =
      prove_solvency(&account.hidden_assets,
                     &assets_blinds,
                     &account.public_assets,
                     &account.hidden_liabilities,
                     &liabilities_blinds,
                     &account.public_liabilities,
                     &self.conversion_rates).or_else(|error| Err(PlatformError::ZeiError(error)))?;

    // Commit the hidden assets and liabilities
    let pc_gens = PedersenGens::default();
    let hidden_assets_commitments: Vec<AssetCommitment> =
      account.hidden_assets
             .iter()
             .zip(assets_blinds.iter())
             .map(|((a, t), (ba, bt))| {
               (pc_gens.commit(*a, *ba).compress(), pc_gens.commit(*t, *bt).compress())
             })
             .collect();
    let hidden_liabilities_commitments: Vec<LiabilityCommitment> =
      account.hidden_liabilities
             .iter()
             .zip(liabilities_blinds.iter())
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
    verify_solvency(hidden_assets_commitments,
                    &account.public_assets,
                    hidden_liabilities_commitments,
                    &account.public_liabilities,
                    &self.conversion_rates,
                    proof).or_else(|error| Err(PlatformError::ZeiError(error)))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_prove_and_verify_solvency() {
    // Generate asset codes
    let asset_1 = AssetTypeCode::gen_random();
    let asset_2 = AssetTypeCode::gen_random();
    let asset_3 = AssetTypeCode::gen_random();

    // Create a asset and liability account
    let mut account = &mut AssetAndLiabilityAccount::default();

    // Adds hidden assets
    account.add_hidden_asset(10, asset_1);
    account.add_hidden_asset(20, asset_2);
    account.add_hidden_asset(30, asset_3);

    // Adds hidden liabilities
    account.add_hidden_liability(10, asset_1);
    account.add_hidden_liability(20, asset_2);

    // Start a solvency audit process
    let mut audit = SolvencyAudit::default();

    // Set asset conversion rates
    audit.add_or_overwrite(asset_1, 1);
    audit.add_or_overwrite(asset_2, 2);
    audit.add_or_overwrite(asset_3, 3);

    // Prove the solvency
    audit.prove_solvency_and_store(&mut account).unwrap();
    assert!(account.hidden_assets_commitments.is_some());
    assert!(account.hidden_liabilities_commitments.is_some());
    assert!(account.proof.is_some());

    // Verify the solvency proof
    audit.verify_solvency(&account).unwrap();
  }
}
