use crate::data_model::errors::PlatformError;
use crate::data_model::AssetTypeCode;
use fixed::types::I20F12;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::XfrNote;

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Fraction(pub I20F12);

impl Fraction {
  pub fn new(num: u64, denom: u64) -> Fraction {
    Fraction(I20F12::from_num(num) / I20F12::from_num(denom))
  }
}

// Debt swap parameters that must be validated against current ledger state
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DebtSwapEffect {
  pub fiat_code: AssetTypeCode,
  pub fee_percentage: Fraction,
}

// TODO: Noah require signature here
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct DebtMemo {
  pub interest_rate: Fraction,
  pub fiat_code: AssetTypeCode,
  pub loan_amount: u64,
}

#[derive(Clone, Copy)]
enum DebtOutputIndices {
  Fiat = 0,
  BurnedDebt = 1,
  ReturnedDebt = 2,
}

#[derive(Clone, Copy)]
enum DebtInputIndices {
  Debt = 0,
}

// A debt swap is valid under the following conditions:
// 1) Outputs contain fiat payment, burn of debt tokens, and refund of debt tokens with remaining
//    balance
// 2) The fee is paid in the correct currency
// 3) The fee is the correct amount
// 4) There are no extraneous inputs.
// This function returns all of the information needed to validate these conditions in DebtSwapEffect
// The function also computes internal consistency checks that do not rely on external information
pub fn compute_debt_swap_effect(transfer: &XfrNote)
                                -> Result<(AssetTypeCode, DebtSwapEffect), PlatformError> {
  let fiat_output = &transfer.body
                             .outputs
                             .get(DebtOutputIndices::Fiat as usize)
                             .ok_or(PlatformError::InputsError)?;

  let burned_debt_output = &transfer.body
                                    .outputs
                                    .get(DebtOutputIndices::BurnedDebt as usize)
                                    .ok_or(PlatformError::InputsError)?;
  let returned_debt_output = &transfer.body
                                      .outputs
                                      .get(DebtOutputIndices::ReturnedDebt as usize);
  let debt_input = &transfer.body
                            .inputs
                            .get(DebtInputIndices::Debt as usize)
                            .ok_or(PlatformError::InputsError)?;

  if transfer.body.inputs.len() > 2 {
    return Err(PlatformError::InputsError);
  }

  // TODO: (noah) figure out how to safely increment lender public key for null pk
  let null_public_key = XfrPublicKey::zei_from_bytes(&[0; 32]);

  // Debt tokens must be burned and payment must go to owner of the debt
  if burned_debt_output.public_key != null_public_key
     || debt_input.public_key != fiat_output.public_key
  {
    dbg!("failed here");
    return Err(PlatformError::InputsError);
  }

  if let Some(returned_debt_output) = returned_debt_output {
    // Ensure that payment and debt tokens are going to the same place
    if fiat_output.public_key != returned_debt_output.public_key {
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
  }

  // Compute fee percentage and return effect
  match (burned_debt_output.amount, debt_input.amount, fiat_output.amount) {
    (Some(amount_a), Some(amount_b), Some(amount_c)) => {
      Ok((AssetTypeCode { val: burned_debt_output.asset_type.unwrap() },
          DebtSwapEffect { fiat_code: AssetTypeCode { val: fiat_output.asset_type.unwrap() },
                           fee_percentage: Fraction::new(amount_c - amount_a, amount_b) }))
    }
    (_, _, _) => Err(PlatformError::InputsError),
  }
}
