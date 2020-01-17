/// Tests submission of transactions constructed by the txn_builder.  
/// All P2P lending-related operations and transactions are tested.

#![deny(warnings)]
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  AssetTypeCode, Transaction, TransferType, TxOutput, TxnSID, TxoRef, TxoSID,
};
use ledger::policies::{calculate_fee, DebtMemo, Fraction};
use ledger::store::LedgerState;
use ledger::store::*;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use txn_builder::{BuildsTransactions, TransactionBuilder, TransferOperationBuilder};
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::AssetRecord;

pub fn apply_transaction(ledger: &mut LedgerState, tx: Transaction) -> (TxnSID, Vec<TxoSID>) {
  let effect =
    TxnEffect::compute_effect(&mut ledger.get_prng(), tx).expect("compute effect failed");

  let mut block = ledger.start_block().expect("starting block failed");
  let temp_sid = ledger.apply_transaction(&mut block, effect)
                       .expect("apply transaction failed");
  ledger.finish_block(block)
        .unwrap()
        .remove(&temp_sid)
        .expect("finishing block failed")
}

#[test]
fn test_create_asset() -> Result<(), PlatformError> {
  let mut prng = ChaChaRng::from_seed([0u8; 32]);
  let mut ledger = LedgerState::test_ledger();
  let code = AssetTypeCode { val: [1; 16] };
  let keys = XfrKeyPair::generate(&mut prng);
  let mut builder = TransactionBuilder::default();

  // Define
  let tx = builder.add_operation_create_asset(&keys, Some(code), false, false, "test".into())?
                  .transaction();
  apply_transaction(&mut ledger, tx.clone());
  assert!(ledger.get_asset_type(&code).is_some());

  // Issue
  let mut builder = TransactionBuilder::default();
  let tx = builder.add_basic_issue_asset(&keys, &None, &code, 0, 1000)?
                  .add_basic_issue_asset(&keys, &None, &code, 1, 500)?
                  .transaction();
  let (_, txos) = apply_transaction(&mut ledger, tx.clone());

  // Basic transfer
  let bar1 = ((ledger.get_utxo(txos[0]).unwrap().0).0).clone();
  let bar2 = ((ledger.get_utxo(txos[1]).unwrap().0).0).clone();
  let oar1 = open_asset_record(&bar1, keys.get_sk_ref()).unwrap();
  let oar2 = open_asset_record(&bar2, keys.get_sk_ref()).unwrap();

  let op = TransferOperationBuilder::new().add_input(TxoRef::Absolute(txos[0]), oar1, 1000)?
                                          .add_input(TxoRef::Absolute(txos[1]), oar2, 500)?
                                          .add_output(1500, keys.get_pk_ref(), code)?
                                          .create(TransferType::Standard)?
                                          .sign(&keys)?
                                          .transaction()?;

  let mut builder = TransactionBuilder::default();
  let tx = builder.add_operation(op).transaction();
  apply_transaction(&mut ledger, tx.clone());

  Ok(())
}

fn test_loan_repayment(loan_amount: u64,
                       loan_repayment_amount: u64,
                       interest_num: u64,
                       interest_denom: u64)
                       -> Result<(), PlatformError> {
  let mut prng = ChaChaRng::from_seed([0u8; 32]);
  let mut ledger = LedgerState::test_ledger();
  let params = PublicParams::new();

  // Asset Info
  let fiat_code = AssetTypeCode { val: [0; 16] };
  let debt_code = AssetTypeCode { val: [1; 16] };
  let interest_rate = Fraction::new(interest_num, interest_denom); // Interest rate interest_num/interest_denom
  let debt_memo = DebtMemo { interest_rate,
                             fiat_code,
                             loan_amount: loan_amount as u64 };
  // Debt Info
  let fee = calculate_fee(loan_amount, interest_rate);

  // Keys
  let fiat_issuer_keys = XfrKeyPair::generate(&mut prng);
  let lender_keys = XfrKeyPair::generate(&mut prng);
  let borrower_keys = XfrKeyPair::generate(&mut prng);
  let burn_address = XfrPublicKey::zei_from_bytes(&[0; 32]);

  // Define assets
  let mut builder = TransactionBuilder::default();
  let tx = builder.add_operation_create_asset(&fiat_issuer_keys,
                                              Some(fiat_code),
                                              false,
                                              false,
                                              "fiat".into())?
                  .add_operation_create_asset(&borrower_keys,
                                              Some(debt_code),
                                              false,
                                              false,
                                              &serde_json::to_string(&debt_memo).unwrap())?
                  .transaction();

  apply_transaction(&mut ledger, tx.clone());

  assert!(ledger.get_asset_type(&fiat_code).is_some());
  assert!(ledger.get_asset_type(&debt_code).is_some());

  let debt_ar = AssetRecord::new(loan_amount, debt_code.val, *borrower_keys.get_pk_ref()).unwrap();
  let fiat_ar =
    AssetRecord::new(loan_amount, fiat_code.val, *fiat_issuer_keys.get_pk_ref()).unwrap();
  let debt_ba = build_blind_asset_record(ledger.get_prng(),
                                         &params.pc_gens,
                                         &debt_ar,
                                         false,
                                         false,
                                         &None);
  let fiat_ba = build_blind_asset_record(ledger.get_prng(),
                                         &params.pc_gens,
                                         &fiat_ar,
                                         false,
                                         false,
                                         &None);
  let debt_oar = open_asset_record(&debt_ba, borrower_keys.get_sk_ref()).unwrap();
  let fiat_oar = open_asset_record(&fiat_ba, lender_keys.get_sk_ref()).unwrap();

  //  Mega transaction to do everything
  let mut builder = TransactionBuilder::default();
  let tx =
    builder.add_operation_issue_asset(&fiat_issuer_keys,
                                      &fiat_code,
                                      0,
                                      &[TxOutput(fiat_ba.clone())])?
           .add_operation_issue_asset(&borrower_keys,
                                      &debt_code,
                                      0,
                                      &[TxOutput(debt_ba.clone())])?;
  let mut xfr_builder = TransferOperationBuilder::new();
  let fiat_to_lender_op = xfr_builder.add_input(TxoRef::Relative(1), fiat_oar, loan_amount)?
                                     .add_output(loan_amount, lender_keys.get_pk_ref(), fiat_code)?
                                     .create(TransferType::Standard)?
                                     .sign(&fiat_issuer_keys)?;

  let fiat_to_borrower_input_ba = fiat_to_lender_op.get_output_record(0).unwrap();
  let fiat_to_borrower_input_oar =
    open_asset_record(&fiat_to_borrower_input_ba, lender_keys.get_sk_ref()).unwrap();

  let mut xfr_builder = TransferOperationBuilder::new();
  let debt_initiation_op =
    xfr_builder.add_input(TxoRef::Relative(0), fiat_to_borrower_input_oar, loan_amount)?
               .add_input(TxoRef::Relative(1), debt_oar, loan_amount)?
               .add_output(loan_amount, borrower_keys.get_pk_ref(), fiat_code)?
               .add_output(loan_amount, lender_keys.get_pk_ref(), debt_code)?
               .create(TransferType::Standard)?
               .sign(&lender_keys)?
               .sign(&borrower_keys)?;

  let debt_burned_input_ba = debt_initiation_op.get_output_record(1).unwrap();
  let debt_burned_input_oar =
    open_asset_record(&debt_burned_input_ba, lender_keys.get_sk_ref()).unwrap();
  let fiat_payment_input_ba = debt_initiation_op.get_output_record(0).unwrap();
  let fiat_payment_input_oar =
    open_asset_record(&fiat_payment_input_ba, borrower_keys.get_sk_ref()).unwrap();

  let mut xfr_builder = TransferOperationBuilder::new();
  let repayment_op = xfr_builder.add_input(TxoRef::Relative(0),
                                           debt_burned_input_oar,
                                           loan_repayment_amount)?
                                .add_input(TxoRef::Relative(1),
                                           fiat_payment_input_oar,
                                           fee + loan_repayment_amount)?
                                .add_output(fee + loan_repayment_amount,
                                            lender_keys.get_pk_ref(),
                                            fiat_code)?
                                .add_output(loan_repayment_amount, &burn_address, debt_code)?
                                .balance()?
                                .create(TransferType::DebtSwap)?
                                .sign(&borrower_keys)?;

  let tx = tx.add_operation(fiat_to_lender_op.transaction()?)
             .add_operation(debt_initiation_op.transaction()?)
             .add_operation(repayment_op.transaction()?)
             .transaction();

  apply_transaction(&mut ledger, tx.clone());

  Ok(())
}

#[test]
fn test_loan_repayments() -> Result<(), PlatformError> {
  test_loan_repayment(1000, 100, 1, 10)?;
  test_loan_repayment(500, 3, 1, 25)?;
  Ok(())
}
