use crate::data_model::errors::PlatformError;
use crate::data_model::AssetTypeCode;
use fixed::types::I20F12;
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::XfrNote;

// this should be defined in ledger utils perhaps
pub type Fraction = I20F12;

// Debt swap parameters that must be validated against current ledger state
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DebtSwapEffect {
  pub fiat_code: AssetTypeCode,
  pub borrower_key: XfrPublicKey,
  pub lender_key: XfrPublicKey,
  pub fee_percentage: Fraction,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct DebtMemo {
  pub interest_rate: Fraction,
  pub fiat_code: AssetTypeCode,
  pub borrower_key: XfrPublicKey,
}

// A debt swap is valid under the following conditions:
// 1) Outputs contain fiat payment, burn of debt tokens, and refund of debt tokens with remaining
//    balance
// 2) The fee is paid in the correct currency
// 3) The fee is the correct amount
// This function returns all of the information needed to validate these conditions in DebtSwapEffect
// The function also computes internal consistency checks that do not rely on external information
pub fn compute_debt_swap_effect(transfer: &XfrNote)
                                -> Result<(AssetTypeCode, DebtSwapEffect), PlatformError> {
  let fiat_output = &transfer.body.outputs[0];
  let burned_debt_output = &transfer.body.outputs[1];
  let returned_debt_output = &transfer.body.outputs[2];
  let null_address = *(XfrKeyPair::generate(&mut ChaChaRng::from_seed([0u8; 32])).get_pk_ref());

  // Ensure that payment and debt tokens are going to the same place
  if fiat_output.public_key != returned_debt_output.public_key {
    return Err(PlatformError::InputsError);
  }

  // Debt tokens must be burned
  if burned_debt_output.public_key != null_address {
    return Err(PlatformError::InputsError);
  }

  // Ensure that debt output types are consistent
  match (burned_debt_output.asset_type, returned_debt_output.asset_type) {
    (Some(type_a), Some(type_b)) => {
      if type_a != type_b {
        return Err(PlatformError::InputsError);
      }
    }
    (_, _) => return Err(PlatformError::InputsError),
  }

  // Compute fee percentage and return effect
  match (burned_debt_output.amount, returned_debt_output.amount, fiat_output.amount) {
    (Some(amount_a), Some(amount_b), Some(amount_c)) => {
      Ok((AssetTypeCode { val: burned_debt_output.asset_type.unwrap() },
          DebtSwapEffect { fiat_code: AssetTypeCode { val: fiat_output.asset_type.unwrap() },
                           lender_key: returned_debt_output.public_key,
                           borrower_key: fiat_output.public_key,
                           fee_percentage: I20F12::from_num(amount_c - amount_a)
                                           / I20F12::from_num(amount_a + amount_b) }))
    }
    (_, _, _) => Err(PlatformError::InputsError),
  }
}
