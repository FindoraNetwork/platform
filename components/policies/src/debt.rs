use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AssetTypeCode, TransferAsset, TransferAssetBody};

// An asset with a debt swap policy must store the following parameters in its memo
// These parameters will be referenced by the validator verifying the policy
struct DebtSwapParams {
  pub fee_percentage: f64, // Percentage of the outstanding debt balance  that must be paid as a fee
  pub fiat_type: AssetTypeCode, // Fiat currency token code
}

// Stores debt swap operation information that must be externally validated
// 1) the debt_type issuer must be lender_address and the
// 2) fiat_type must be the type defined in the debt_type memo
struct DebtSwapEffect {
  pub lender_address: XfrPublicKey,
  pub fiat_type: AssetTypeCode,
}

// A debt swap transfer represents a loan repayment, an atomic transfer that contains the following components
// 1) Principal payment: N fiat tokens sent from lender to borrower
// 2) Debt token burn: N debt tokens sent from lender to null address
// 3) Fee payment: M*fee_percentage fiat tokens sent from lender to the borrower
// 4) Remainder: Any leftover fiat is sent back to the borrower and M - N debt tokens are sent back to
//    the lender
//
// A debt swap is valid iff for every N out of M debt tokens burned:
//  1) The total fiat amount sent to the lender is N + M*fee_percentage
//  2) Inputs and outputs are balanced
//  3) Outputs must be ordered (Debt burned, Debt returned, fiat to lender, remainder)
pub fn compute_debt_swap_effect(transfer_body: &TransferAssetBody)
                                -> Result<(AssetTypeCode, DebtSwapEffect), PlatformError> {
  let inputs = transfer_body.transfer.body.inputs;
  let outputs = transfer_body.transfer.body.outputs;

  let burned_debt = outputs[0];
  let returned_debt = outputs[1];
  let fiat_paid = outputs[2];
  let fiat_remainder = outputs[3];
  let lender_key = returned_debt.public_key;
  let debt_type;

  // Ensure that everything is going to the right place
  if (burned_debt.public_key != NULL_ADDRESS || returned_debt.public_key != lender_key) {
    return Err(PlatformError::InputsError);
  }

  // Ensures that asset types are consistent (2)
  match (burned_debt.asset_type, returned_debt.asset_type) {
    (Some(type_a), Some(type_b)) => {
      if (type_a != type_b) {
        return Err(PlatformError::InputsError);
      }
      debt_type = type_a;
    }
    // types must be unencryped
    (_, _) => return Err(PlatformError: InputsError),
  }

  // Ensure that correct fee is paid
  match (burned_debt.amount, returned_debt.amount, fiat_paid.amount) {
    (Some(burned_amount), Some(returned_amount), Some(fiat_amount)) => {
      // (1) fiat amount correct
      if ((burned_amount + returned_amount) * fee_percentage + returned_amount != fiat_amount) {
        return Err(PlatformError::InputsError);
      }
    }
    // amounts must be unencrypted
    (_, _, _, _) => return Err(PlatformError: InputsError),
  }

  return DebtSwapEffect { lender_address: lender_key,
                          debt_type: debt_type,
                          fiat_type: fiat_paid.asset_type.unwrap() };
}
