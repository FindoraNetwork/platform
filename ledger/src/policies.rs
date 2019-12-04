use crate::data_model::errors::PlatformError;
use crate::data_model::{AssetType, AssetTypeCode};
use fixed::types::I20F12;
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::XfrNote;

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Fraction(pub I20F12);

impl Fraction {
  pub fn new(num: u64, denom: u64) -> Fraction {
    Fraction(I20F12::from_num(num) / I20F12::from_num(denom))
  }
}

pub fn is_debt_token(asset_type: &AssetType) -> bool {
  serde_json::from_str::<DebtMemo>(&asset_type.properties.memo.0).is_ok()
}

// Debt swap parameters that must be validated against current ledger state
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DebtSwapEffect {
  pub fiat_code: AssetTypeCode,
  pub lender_key: XfrPublicKey,
  pub fee_percentage: Fraction,
}

// TODO: Noah require signature here
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct DebtMemo {
  pub interest_rate: Fraction,
  pub fiat_code: AssetTypeCode,
  pub borrower_key: XfrPublicKey,
  pub loan_amount: u64,
}

#[derive(Clone, Copy)]
enum DebtIndices {
  Fiat = 0,
  BurnedDebt = 1,
  ReturnedDebt = 2,
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
  let fiat_output = &transfer.body.outputs[DebtIndices::Fiat as usize];
  let burned_debt_output = &transfer.body.outputs[DebtIndices::BurnedDebt as usize];
  let returned_debt_output = &transfer.body.outputs[DebtIndices::ReturnedDebt as usize];
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
                           fee_percentage: Fraction::new(amount_c - amount_a,
                                                         amount_a + amount_b) }))
    }
    (_, _, _) => Err(PlatformError::InputsError),
  }
}
