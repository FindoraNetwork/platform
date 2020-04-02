use crate::data_model::errors::PlatformError;
use crate::data_model::AssetTypeCode;
use crate::error_location;
use fixed::types::I20F12;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::{XfrAmount, XfrAssetType, XfrBody};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub struct Fraction(pub I20F12);

impl Fraction {
  pub fn new(num: u64, denom: u64) -> Fraction {
    Fraction(I20F12::from_num(num) / I20F12::from_num(denom))
  }
  pub fn checked_new(num: u64, denom: u64) -> Option<Fraction> {
    Some(Fraction(I20F12::checked_from_num(num)?.checked_div(I20F12::checked_from_num(denom)?)?))
  }
}

// Debt swap parameters that must be validated against current ledger state
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DebtSwapEffect {
  pub fiat_code: AssetTypeCode,
  pub initial_balance: u64,
  pub fiat_paid: u64,
  pub debt_burned: u64,
}

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

// Returns expected fee for debt swap transaction
pub fn calculate_fee(principal: u64, interest_rate: Fraction) -> u64 {
  (I20F12::from_num(principal) * interest_rate.0).round()
                                                 .to_num()
}

// A debt swap is valid under the following conditions:
// 1) Outputs contain fiat payment, burn of debt tokens, and refund of debt tokens with remaining
//    balance
// 2) The fee is paid in the correct currency
// 3) The fee is the correct amount
// 4) There are no extraneous inputs.
// This function returns all of the information needed to validate these conditions in DebtSwapEffect
// The function also computes internal consistency checks that do not rely on external information
pub fn compute_debt_swap_effect(transfer: &XfrBody)
                                -> Result<(AssetTypeCode, DebtSwapEffect), PlatformError> {
  let fiat_output = &transfer.outputs
                             .get(DebtOutputIndices::Fiat as usize)
                             .ok_or_else(|| PlatformError::InputsError(error_location!()))?;

  let burned_debt_output = &transfer.outputs
                                    .get(DebtOutputIndices::BurnedDebt as usize)
                                    .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
  let returned_debt_output = &transfer.outputs
                                      .get(DebtOutputIndices::ReturnedDebt as usize);
  let debt_input = &transfer.inputs
                            .get(DebtInputIndices::Debt as usize)
                            .ok_or_else(|| PlatformError::InputsError(error_location!()))?;

  if transfer.inputs.len() > 2 {
    return Err(PlatformError::InputsError(error_location!()));
  }

  // TODO: (noah) figure out how to safely increment lender public key for null pk
  let null_public_key = XfrPublicKey::zei_from_bytes(&[0; 32]);

  // Debt tokens must be burned and payment must go to owner of the debt
  if burned_debt_output.public_key != null_public_key
     || debt_input.public_key != fiat_output.public_key
  {
    return Err(PlatformError::InputsError(error_location!()));
  }

  if let Some(returned_debt_output) = returned_debt_output {
    // Ensure that payment and debt tokens are going to the same place
    if fiat_output.public_key != returned_debt_output.public_key {
      return Err(PlatformError::InputsError(error_location!()));
    }

    // Ensure that debt output types are consistent
    match (&burned_debt_output.asset_type, &returned_debt_output.asset_type) {
      (XfrAssetType::NonConfidential(type_a), XfrAssetType::NonConfidential(type_b)) => {
        if type_a != type_b {
          return Err(PlatformError::InputsError(error_location!()));
        }
      }
      (_, _) => return Err(PlatformError::InputsError(error_location!())),
    }
  }

  match (&debt_input.amount,
         &fiat_output.amount,
         &fiat_output.asset_type,
         &burned_debt_output.amount,
         &burned_debt_output.asset_type)
  {
    (XfrAmount::NonConfidential(initial_balance),
     XfrAmount::NonConfidential(fiat_paid),
     XfrAssetType::NonConfidential(fiat_type),
     XfrAmount::NonConfidential(debt_burned),
     XfrAssetType::NonConfidential(debt_burned_type)) => {
      // Return effect
      Ok((AssetTypeCode { val: *debt_burned_type },
          DebtSwapEffect { fiat_code: AssetTypeCode { val: *fiat_type },
                           initial_balance: *initial_balance,
                           fiat_paid: *fiat_paid,
                           debt_burned: *debt_burned }))
    }
    (_, _, _, _, _) => Err(PlatformError::InputsError(error_location!())),
  }
}
