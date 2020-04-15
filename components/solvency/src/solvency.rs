#![deny(warnings)]
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::AssetTypeCode;
use linear_map::LinearMap;
use zei::algebra::ristretto::ZeiScalar;
use zei::crypto::solvency::{prove_solvency, verify_solvency};
use zei::xfr::structs::asset_type_to_scalar;

pub type AssetCodeAndAmountScalar = (Scalar, Scalar);
pub type AssetCodeAndRateScalar = (Scalar, Scalar);

/// Converts a set of asset code and amount to a scalar set.
/// This is used for proving and verifying solvency.
pub fn code_and_amount_to_scalars(code: AssetTypeCode, amount: u64) -> AssetCodeAndAmountScalar {
  (asset_type_to_scalar(&code), ZeiScalar::from_u64(amount))
}

// /// Converts a set of asset code and exchange rate to a scalar set.
// /// This is used for proving and verifying solvency.
// pub fn code_and_rate_to_scalars(code: AssetTypeCode, rate: u64) -> AssetCodeAndRateScalar {
//     (asset_type_to_scalar(&code),ZeiScalar::from_u64(rate) )
// }

/// Table mapping an asset code to its exchange rate.
#[derive(Default)]
pub struct AssetCodeAndRateTable {
  map: LinearMap<Scalar, Scalar>,
}

impl AssetCodeAndRateTable {
  pub fn new(&self) {
    self.map = LinearMap::new();
  }

  pub fn add(&self, code: AssetTypeCode, rate: u64) {
    self.map
        .insert(asset_type_to_scalar(&code), ZeiScalar::from_u64(rate));
  }
}

fn main() {}
