#![deny(warnings)]
/// Tests submission of transactions constructed by the txn_builder.  
/// All P2P lending-related operations and transactions are tested.
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  AssetRules, AssetTypeCode, Transaction, TransferType, TxOutput, TxnSID, TxoRef, TxoSID,
};
use ledger::policies::{calculate_fee, DebtMemo, Fraction};
use ledger::store::LedgerState;
use ledger::store::*;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use txn_builder::{BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder};
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record, AssetRecordType};
use zei::xfr::asset_tracer::gen_asset_tracer_keypair;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{AssetRecordTemplate, AssetTracingPolicy};

pub fn apply_transaction(ledger: &mut LedgerState, tx: Transaction) -> (TxnSID, Vec<TxoSID>) {
  let effect = TxnEffect::compute_effect(tx).expect("compute effect failed");

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
  let mut prng = ChaChaRng::from_entropy();
  let mut ledger = LedgerState::test_ledger();
  let code = AssetTypeCode { val: [1; 16] };
  let keys = XfrKeyPair::generate(&mut prng);
  let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());

  // Tracer kp
  let tracer_kp = gen_asset_tracer_keypair(&mut prng);
  let policy = AssetTracingPolicy { enc_keys: tracer_kp.enc_key.clone(),
                                    asset_tracking: true,
                                    identity_tracking: None };

  // Define
  let tx = builder.add_operation_create_asset(&keys,
                                              Some(code),
                                              AssetRules::default().set_traceable(true).clone(),
                                              "test".into(),
                                              PolicyChoice::Fungible())?
                  .transaction();
  apply_transaction(&mut ledger, tx.clone());
  assert!(ledger.get_asset_type(&code).is_some());

  // Issue
  let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
  let tx =
    builder.add_basic_issue_asset(&keys,
                                  Some(policy.clone()),
                                  &code,
                                  0,
                                  1000,
                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType)?
           .add_basic_issue_asset(&keys,
                                  Some(policy.clone()),
                                  &code,
                                  1,
                                  500,
                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType)?
           .transaction();
  let (_, txos) = apply_transaction(&mut ledger, tx.clone());

  // Basic transfer
  let bar1 = ((ledger.get_utxo(txos[0]).unwrap().0).0).clone();
  let bar2 = ((ledger.get_utxo(txos[1]).unwrap().0).0).clone();
  let oar1 = open_blind_asset_record(&bar1, &None, keys.get_sk_ref()).unwrap();
  let oar2 = open_blind_asset_record(&bar2, &None, keys.get_sk_ref()).unwrap();

  let op = TransferOperationBuilder::new().add_input(TxoRef::Absolute(txos[0]), oar1, None, None, 1000)?
                                          .add_input(TxoRef::Absolute(txos[1]), oar2, Some(policy.clone()), None, 500)?
                                          .add_output(&AssetRecordTemplate::with_asset_tracking(1500, code.val, NonConfidentialAmount_NonConfidentialAssetType, keys.get_pk(), policy.clone()), None, None, None)?
                                          .create(TransferType::Standard)?
                                          .sign(&keys)?
                                          .transaction()?;

  let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
  let tx = builder.add_operation(op).transaction();
  apply_transaction(&mut ledger, tx.clone());

  Ok(())
}

fn test_loan_repayment(loan_amount: u64,
                       loan_repayment_amount: u64,
                       interest_num: u64,
                       interest_denom: u64)
                       -> Result<(), PlatformError> {
  let mut prng = ChaChaRng::from_entropy();
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
  let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
  let tx = builder.add_operation_create_asset(&fiat_issuer_keys,
                                              Some(fiat_code),
                                              AssetRules::default(),
                                              "fiat".into(),
                                              PolicyChoice::Fungible())?
                  .add_operation_create_asset(&borrower_keys,
                                              Some(debt_code),
                                              AssetRules::default(),
                                              &serde_json::to_string(&debt_memo).unwrap(),
                                              PolicyChoice::Fungible())?
                  .transaction();

  apply_transaction(&mut ledger, tx.clone());

  assert!(ledger.get_asset_type(&fiat_code).is_some());
  assert!(ledger.get_asset_type(&debt_code).is_some());

  let debt_ar =
    AssetRecordTemplate::with_no_asset_tracking(loan_amount,
                                                debt_code.val,
                                                NonConfidentialAmount_NonConfidentialAssetType,
                                                borrower_keys.get_pk());
  let fiat_ar =
    AssetRecordTemplate::with_no_asset_tracking(loan_amount,
                                                fiat_code.val,
                                                NonConfidentialAmount_NonConfidentialAssetType,
                                                fiat_issuer_keys.get_pk());
  let (debt_ba, _, debt_owner_memo) =
    build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &debt_ar, None);
  let (fiat_ba, _, fiat_owner_memo) =
    build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &fiat_ar, None);
  let debt_oar =
    open_blind_asset_record(&debt_ba, &debt_owner_memo, borrower_keys.get_sk_ref()).unwrap();
  let fiat_oar =
    open_blind_asset_record(&fiat_ba, &fiat_owner_memo, lender_keys.get_sk_ref()).unwrap();

  //  Mega transaction to do everything
  let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
  let tx = builder.add_operation_issue_asset(&fiat_issuer_keys,
                                             &fiat_code,
                                             0,
                                             &[(TxOutput(fiat_ba.clone()), fiat_owner_memo)],
                                             None)?
                  .add_operation_issue_asset(&borrower_keys,
                                             &debt_code,
                                             0,
                                             &[(TxOutput(debt_ba.clone()), debt_owner_memo)],
                                             None)?;
  let mut xfr_builder = TransferOperationBuilder::new();
  let output_template =
    AssetRecordTemplate::with_no_asset_tracking(loan_amount,
                                                fiat_code.val,
                                                NonConfidentialAmount_NonConfidentialAssetType,
                                                lender_keys.get_pk());
  let fiat_to_lender_op =
    xfr_builder.add_input(TxoRef::Relative(1), fiat_oar, None, None, loan_amount)?
               .add_output(&output_template, None, None, None)?
               .create(TransferType::Standard)?
               .sign(&fiat_issuer_keys)?;

  let fiat_to_borrower_input_ba = fiat_to_lender_op.get_output_record(0).unwrap();
  let fiat_to_borrower_input_oar =
    open_blind_asset_record(&fiat_to_borrower_input_ba, &None, lender_keys.get_sk_ref()).unwrap();

  let mut xfr_builder = TransferOperationBuilder::new();
  let borrower_output_template =
    AssetRecordTemplate::with_no_asset_tracking(loan_amount,
                                                fiat_code.val,
                                                NonConfidentialAmount_NonConfidentialAssetType,
                                                borrower_keys.get_pk());
  let lender_output_template =
    AssetRecordTemplate::with_no_asset_tracking(loan_amount,
                                                debt_code.val,
                                                NonConfidentialAmount_NonConfidentialAssetType,
                                                lender_keys.get_pk());
  let debt_initiation_op =
    xfr_builder.add_input(TxoRef::Relative(0),
                          fiat_to_borrower_input_oar,
                          None,
                          None,
                          loan_amount)?
               .add_input(TxoRef::Relative(1), debt_oar, None, None, loan_amount)?
               .add_output(&borrower_output_template, None, None, None)?
               .add_output(&lender_output_template, None, None, None)?
               .create(TransferType::Standard)?
               .sign(&lender_keys)?
               .sign(&borrower_keys)?;

  let debt_burned_input_ba = debt_initiation_op.get_output_record(1).unwrap();
  let debt_burned_input_oar =
    open_blind_asset_record(&debt_burned_input_ba, &None, lender_keys.get_sk_ref()).unwrap();
  let fiat_payment_input_ba = debt_initiation_op.get_output_record(0).unwrap();
  let fiat_payment_input_oar =
    open_blind_asset_record(&fiat_payment_input_ba, &None, borrower_keys.get_sk_ref()).unwrap();

  let mut xfr_builder = TransferOperationBuilder::new();
  let loan_repayment_template =
    AssetRecordTemplate::with_no_asset_tracking(fee + loan_repayment_amount,
                                                fiat_code.val,
                                                NonConfidentialAmount_NonConfidentialAssetType,
                                                lender_keys.get_pk());
  let burn_repayment_template =
    AssetRecordTemplate::with_no_asset_tracking(loan_repayment_amount,
                                                debt_code.val,
                                                NonConfidentialAmount_NonConfidentialAssetType,
                                                burn_address);
  let repayment_op = xfr_builder.add_input(TxoRef::Relative(0),
                                           debt_burned_input_oar,
                                           None,
                                           None,
                                           loan_repayment_amount)?
                                .add_input(TxoRef::Relative(1),
                                           fiat_payment_input_oar,
                                           None,
                                           None,
                                           fee + loan_repayment_amount)?
                                .add_output(&loan_repayment_template, None, None, None)?
                                .add_output(&burn_repayment_template, None, None, None)?
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
