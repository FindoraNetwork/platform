/// Tests submission of transactions constructed by the txn_builder.
/// All P2P lending-related operations and transactions are tested.
use credentials::{
    credential_commit, credential_issuer_key_gen, credential_sign,
    credential_user_key_gen, Credential,
};

use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
    AssetRules, AssetTypeCode, Memo, NoReplayToken, Operation, Transaction,
    TransferType, TxOutput, TxnEffect, TxnSID, TxoRef, TxoSID,
};
use ledger::error_location;
use ledger::policies::{calculate_fee, DebtMemo, Fraction};
use ledger::store::helpers::create_definition_transaction;
use ledger::store::LedgerState;
use ledger::store::*;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use txn_builder::{
    BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder,
};
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
use zei::xfr::asset_record::{
    build_blind_asset_record, open_blind_asset_record, AssetRecordType,
};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::AssetRecordTemplate;

pub fn apply_transaction(
    ledger: &mut LedgerState,
    tx: Transaction,
) -> (TxnSID, Vec<TxoSID>) {
    let effect = TxnEffect::compute_effect(tx).expect("compute effect failed");

    let mut block = ledger.start_block().expect("starting block failed");
    let temp_sid = ledger
        .apply_transaction(&mut block, effect)
        .expect("apply transaction failed");
    ledger
        .finish_block(block)
        .unwrap()
        .remove(&temp_sid)
        .expect("finishing block failed")
}

#[test]
fn test_create_asset() -> Result<(), PlatformError> {
    let mut prng = ChaChaRng::from_entropy();
    let mut ledger = LedgerState::test_ledger();
    let code = AssetTypeCode::gen_random();
    let keys = XfrKeyPair::generate(&mut prng);
    let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
    let params = PublicParams::new();

    // Define
    let tx = builder
        .add_operation_create_asset(
            &keys,
            Some(code),
            AssetRules::default(),
            "test".into(),
            PolicyChoice::Fungible(),
        )?
        .transaction();
    apply_transaction(&mut ledger, tx.clone());

    // Issue
    let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
    let tx = builder
        .add_basic_issue_asset(
            &keys,
            &code,
            0,
            1000,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            &params,
        )?
        .add_basic_issue_asset(
            &keys,
            &code,
            1,
            500,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            &params,
        )?
        .transaction();
    let (_, txos) = apply_transaction(&mut ledger, tx.clone());

    // Basic transfer
    let state_comm1 = ledger.get_state_commitment().0;
    let bar1_proof = ledger.get_utxo(txos[0]).unwrap();
    let bar2_proof = ledger.get_utxo(txos[1]).unwrap();
    let bar1 = (bar1_proof.utxo.0).record.clone();
    let bar2 = (bar2_proof.utxo.0).record.clone();
    let oar1 = open_blind_asset_record(&bar1, &None, keys.get_sk_ref()).unwrap();
    let oar2 = open_blind_asset_record(&bar2, &None, keys.get_sk_ref()).unwrap();
    assert!(bar1_proof.is_valid(state_comm1.clone()));
    assert!(bar2_proof.is_valid(state_comm1.clone()));

    let mut builder = TransferOperationBuilder::new();
    builder
        .add_input(TxoRef::Absolute(txos[0]), oar1, None, None, 1000)?
        .add_input(TxoRef::Absolute(txos[1]), oar2, None, None, 500)?
        .add_output(
            &AssetRecordTemplate::with_no_asset_tracking(
                1500,
                code.val,
                NonConfidentialAmount_NonConfidentialAssetType,
                keys.get_pk(),
            ),
            None,
            None,
            None,
        )?
        .create(TransferType::Standard)?;

    let input_sig = builder.create_input_signature(&keys).unwrap();
    builder.attach_signature(input_sig).unwrap();
    let op = builder.transaction()?;

    let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
    let tx = builder.add_operation(op).transaction();
    apply_transaction(&mut ledger, tx.clone());

    Ok(())
}

fn test_loan_repayment(
    loan_amount: u64,
    loan_repayment_amount: u64,
    interest_num: u64,
    interest_denom: u64,
) -> Result<(), PlatformError> {
    let mut prng = ChaChaRng::from_entropy();
    let mut ledger = LedgerState::test_ledger();
    let params = PublicParams::new();

    // Asset Info
    let fiat_code = AssetTypeCode::gen_random();
    let debt_code = AssetTypeCode::gen_random();
    let interest_rate = Fraction::new(interest_num, interest_denom); // Interest rate interest_num/interest_denom
    let debt_memo = DebtMemo {
        interest_rate,
        fiat_code,
        loan_amount: loan_amount as u64,
    };
    // Debt Info
    let fee = calculate_fee(loan_amount, interest_rate);

    // Keys
    let fiat_issuer_keys = XfrKeyPair::generate(&mut prng);
    let lender_keys = XfrKeyPair::generate(&mut prng);
    let borrower_keys = XfrKeyPair::generate(&mut prng);
    let burn_address = XfrPublicKey::zei_from_bytes(&[0; 32])
        .map_err(|e| PlatformError::ZeiError(error_location!(), e))?;

    // Define assets
    let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
    let tx = builder
        .add_operation_create_asset(
            &fiat_issuer_keys,
            Some(fiat_code),
            AssetRules::default(),
            "fiat".into(),
            PolicyChoice::Fungible(),
        )?
        .add_operation_create_asset(
            &borrower_keys,
            Some(debt_code),
            AssetRules::default(),
            &serde_json::to_string(&debt_memo).unwrap(),
            PolicyChoice::Fungible(),
        )?
        .transaction();

    apply_transaction(&mut ledger, tx.clone());

    assert!(ledger.get_asset_type(&fiat_code).is_some());
    assert!(ledger.get_asset_type(&debt_code).is_some());

    let debt_ar = AssetRecordTemplate::with_no_asset_tracking(
        loan_amount,
        debt_code.val,
        NonConfidentialAmount_NonConfidentialAssetType,
        borrower_keys.get_pk(),
    );
    let fiat_ar = AssetRecordTemplate::with_no_asset_tracking(
        loan_amount,
        fiat_code.val,
        NonConfidentialAmount_NonConfidentialAssetType,
        fiat_issuer_keys.get_pk(),
    );
    let (debt_ba, _, debt_owner_memo) =
        build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &debt_ar, vec![]);
    let (fiat_ba, _, fiat_owner_memo) =
        build_blind_asset_record(ledger.get_prng(), &params.pc_gens, &fiat_ar, vec![]);
    let debt_oar =
        open_blind_asset_record(&debt_ba, &debt_owner_memo, borrower_keys.get_sk_ref())
            .unwrap();
    let fiat_oar =
        open_blind_asset_record(&fiat_ba, &fiat_owner_memo, lender_keys.get_sk_ref())
            .unwrap();

    //  Mega transaction to do everything
    let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
    let tx = builder
        .add_operation_issue_asset(
            &fiat_issuer_keys,
            &fiat_code,
            0,
            &[(
                TxOutput {
                    record: fiat_ba.clone(),
                    lien: None,
                },
                fiat_owner_memo,
            )],
        )?
        .add_operation_issue_asset(
            &borrower_keys,
            &debt_code,
            0,
            &[(
                TxOutput {
                    record: debt_ba.clone(),
                    lien: None,
                },
                debt_owner_memo,
            )],
        )?;
    let mut xfr_builder = TransferOperationBuilder::new();
    let output_template = AssetRecordTemplate::with_no_asset_tracking(
        loan_amount,
        fiat_code.val,
        NonConfidentialAmount_NonConfidentialAssetType,
        lender_keys.get_pk(),
    );
    let fiat_to_lender_op = xfr_builder
        .add_input(TxoRef::Relative(1), fiat_oar, None, None, loan_amount)?
        .add_output(&output_template, None, None, None)?
        .create(TransferType::Standard)?
        .sign(&fiat_issuer_keys)?;

    let fiat_to_borrower_input_ba = fiat_to_lender_op.get_output_record(0).unwrap();
    let fiat_to_borrower_input_oar = open_blind_asset_record(
        &fiat_to_borrower_input_ba,
        &None,
        lender_keys.get_sk_ref(),
    )
    .unwrap();

    let mut xfr_builder = TransferOperationBuilder::new();
    let borrower_output_template = AssetRecordTemplate::with_no_asset_tracking(
        loan_amount,
        fiat_code.val,
        NonConfidentialAmount_NonConfidentialAssetType,
        borrower_keys.get_pk(),
    );
    let lender_output_template = AssetRecordTemplate::with_no_asset_tracking(
        loan_amount,
        debt_code.val,
        NonConfidentialAmount_NonConfidentialAssetType,
        lender_keys.get_pk(),
    );
    let debt_initiation_op = xfr_builder
        .add_input(
            TxoRef::Relative(0),
            fiat_to_borrower_input_oar,
            None,
            None,
            loan_amount,
        )?
        .add_input(TxoRef::Relative(1), debt_oar, None, None, loan_amount)?
        .add_output(&borrower_output_template, None, None, None)?
        .add_output(&lender_output_template, None, None, None)?
        .create(TransferType::Standard)?
        .sign(&lender_keys)?
        .sign(&borrower_keys)?;

    let debt_burned_input_ba = debt_initiation_op.get_output_record(1).unwrap();
    let debt_burned_input_oar =
        open_blind_asset_record(&debt_burned_input_ba, &None, lender_keys.get_sk_ref())
            .unwrap();
    let fiat_payment_input_ba = debt_initiation_op.get_output_record(0).unwrap();
    let fiat_payment_input_oar = open_blind_asset_record(
        &fiat_payment_input_ba,
        &None,
        borrower_keys.get_sk_ref(),
    )
    .unwrap();

    let mut xfr_builder = TransferOperationBuilder::new();
    let loan_repayment_template = AssetRecordTemplate::with_no_asset_tracking(
        fee + loan_repayment_amount,
        fiat_code.val,
        NonConfidentialAmount_NonConfidentialAssetType,
        lender_keys.get_pk(),
    );
    let burn_repayment_template = AssetRecordTemplate::with_no_asset_tracking(
        loan_repayment_amount,
        debt_code.val,
        NonConfidentialAmount_NonConfidentialAssetType,
        burn_address,
    );
    let repayment_op = xfr_builder
        .add_input(
            TxoRef::Relative(0),
            debt_burned_input_oar,
            None,
            None,
            loan_repayment_amount,
        )?
        .add_input(
            TxoRef::Relative(1),
            fiat_payment_input_oar,
            None,
            None,
            fee + loan_repayment_amount,
        )?
        .add_output(&loan_repayment_template, None, None, None)?
        .add_output(&burn_repayment_template, None, None, None)?
        .balance()?
        .create(TransferType::DebtSwap)?
        .sign(&borrower_keys)?;

    let tx = tx
        .add_operation(fiat_to_lender_op.transaction()?)
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

#[test]
fn test_update_memo() -> Result<(), PlatformError> {
    // Generate the ledger and the things we need to define an asset
    let mut prng = ChaChaRng::from_entropy();
    let mut ledger = LedgerState::test_ledger();
    let code = AssetTypeCode::gen_random();
    let keys = XfrKeyPair::generate(&mut prng);
    let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());

    // Define the asset and verify
    let mut asset_rules = AssetRules::default();
    // The asset must be up updatable in order to change the memo later
    asset_rules.updatable = true;
    // Create an asset with the memo defined as "test"
    let tx = builder
        .add_operation_create_asset(
            &keys,
            Some(code),
            asset_rules,
            "test".into(),
            PolicyChoice::Fungible(),
        )?
        .transaction();
    apply_transaction(&mut ledger, tx.clone());
    assert!(ledger.get_asset_type(&code).is_some());

    // Define a transaction to update the memo
    let mut builder = TransactionBuilder::from_seq_id(ledger.get_block_commit_count());
    let tx = builder
        .add_operation_update_memo(&keys, code, "changed")
        .transaction();
    apply_transaction(&mut ledger, tx.clone());

    // Attempt to get the changed memo, and verify it has been changed correctly
    let asset = ledger
        .get_asset_type(&code)
        .expect("The asset disappeared after updating the memo.");

    let new_memo = asset.properties.memo.0.as_str();
    assert_eq!(new_memo, "changed");

    Ok(())
}

#[test]
pub fn test_update_memo_orig() {
    let mut ledger = LedgerState::test_ledger();

    let creator = XfrKeyPair::generate(&mut ledger.get_prng());
    let adversary = XfrKeyPair::generate(&mut ledger.get_prng());

    // Define fiat token
    let code = AssetTypeCode::gen_random();
    let seq_id = ledger.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    let asset_rules = AssetRules::default().set_updatable(true).clone();
    builder
        .add_operation_create_asset(
            &creator,
            Some(code),
            asset_rules,
            "test",
            PolicyChoice::Fungible(),
        )
        .unwrap();
    let tx = builder.transaction();
    apply_transaction(&mut ledger, tx.clone());

    let mut block = ledger.start_block().unwrap();
    let seq_id = ledger.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    builder.add_operation_update_memo(&creator, code, "test");
    // Ensure that invalid signature fails
    let mut tx_bad = builder.transaction().clone();
    for i in 0..tx_bad.body.operations.len() {
        let op = &mut tx_bad.body.operations[i];
        match op {
            Operation::UpdateMemo(ref mut memo_update) => {
                memo_update.pubkey = adversary.get_pk();
            }
            _ => (),
        }
    }
    assert!(TxnEffect::compute_effect(tx_bad.clone()).is_err());

    // Ensure that valid signature succeeds
    assert!(TxnEffect::compute_effect(tx.clone()).is_ok());

    // Only the asset creator can change the memo
    let seq_id = ledger.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    builder.add_operation_update_memo(&adversary, code, "new memo");
    let tx = builder.transaction().clone();
    let effect = TxnEffect::compute_effect(tx).unwrap();
    assert!(ledger.apply_transaction(&mut block, effect).is_err());
    ledger.finish_block(block).unwrap();

    // Cant change memo more than once in the same block
    let mut block = ledger.start_block().expect("starting block failed");
    let seq_id = ledger.get_block_commit_count();
    let mut builder = TransactionBuilder::from_seq_id(seq_id);
    builder.add_operation_update_memo(&creator, code, "test");
    let tx = builder.transaction();
    let effect0 = TxnEffect::compute_effect(tx.clone()).expect("compute effect failed");
    let temp_sid = ledger
        .apply_transaction(&mut block, effect0)
        .expect("apply transaction failed");
    let effect1 = TxnEffect::compute_effect(tx.clone()).expect("compute effect failed");
    assert!(ledger.apply_transaction(&mut block, effect1).is_err());
    ledger
        .finish_block(block)
        .unwrap()
        .remove(&temp_sid)
        .expect("finishing block failed");
}

#[test]
pub fn test_update_memo_darp() {
    let mut ledger = LedgerState::test_ledger();
    let creator = XfrKeyPair::generate(&mut ledger.get_prng());

    // Define fiat token
    let code = AssetTypeCode::gen_random();
    let seq_id = ledger.get_block_commit_count();
    let tx = create_definition_transaction(
        &code,
        &creator,
        AssetRules::default().set_updatable(true).clone(),
        Some(Memo("test".to_string())),
        seq_id,
    )
    .unwrap();
    apply_transaction(&mut ledger, tx);

    let mut block = ledger.start_block().expect("starting first block failed");
    let _new_memo = Memo("new_memo".to_string());
    let seq_id = ledger.get_block_commit_count();
    let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
    txn_builder.add_operation_update_memo(&creator, code, "new_memo");
    let txn = txn_builder.transaction();
    let effect0 =
        TxnEffect::compute_effect(txn.clone()).expect("compute effect0 failed");
    let temp_sid0 = ledger
        .apply_transaction(&mut block, effect0.clone())
        .expect("apply transaction0 failed");

    // Test 1: replay the exact same txn, it should fail
    let effect1 =
        TxnEffect::compute_effect(txn.clone()).expect("compute effect1 failed");
    assert!(
        ledger
            .apply_transaction(&mut block, effect1.clone())
            .is_err()
    );
    ledger
        .finish_block(block)
        .unwrap()
        .remove(&temp_sid0)
        .expect("finishing block failed");

    let mut block = ledger
        .start_block()
        .expect("starting replay exact txn block failed");
    let txn = txn_builder.transaction();
    let effect = TxnEffect::compute_effect(txn.clone()).expect("compute effect failed");

    assert!(
        ledger
            .apply_transaction(&mut block, effect.clone())
            .is_err()
    );

    ledger.finish_block(block).unwrap();

    //assert!(ledger.apply_transaction(&mut block, effect.clone())
    //              .is_err());

    // Test 2: wrong NRPT
    // create a new txn
    let seq_id = ledger.get_block_commit_count();
    let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
    // insert the copied operation (which will not match the NRPT of the txn) into the new txn
    txn_builder.add_operation_update_memo(&creator, code, "new_memo");
    let mut tx_bad = txn_builder.transaction().clone();
    for i in 0..tx_bad.body.operations.len() {
        let op = &mut tx_bad.body.operations[i];
        match op {
            Operation::UpdateMemo(ref mut memo_update) => {
                let mut prng = ChaChaRng::from_entropy();
                memo_update.body.no_replay_token = NoReplayToken::new(&mut prng, seq_id);
            }
            _ => (),
        }
    }
    assert!(TxnEffect::compute_effect(tx_bad.clone()).is_err());
}

#[test]
/// Tests that a valid AIR credential can be appended to the AIR with the air_assign operation.
pub fn test_air_assign_operation() {
    let mut ledger = LedgerState::test_ledger();
    let dl = String::from("dl");
    let cred_issuer_key =
        credential_issuer_key_gen(&mut ledger.get_prng(), &[(dl.clone(), 8)]);
    let cred_user_key =
        credential_user_key_gen(&mut ledger.get_prng(), &cred_issuer_key.0);
    let user_kp = XfrKeyPair::generate(&mut ledger.get_prng());

    // Construct credential
    let dl_attr = b"A1903479";
    let attr_map = vec![(dl.clone(), dl_attr.to_vec())];
    let attributes = [(dl.clone(), &dl_attr[..])];
    let signature = credential_sign(
        &mut ledger.get_prng(),
        &cred_issuer_key.1,
        &cred_user_key.0,
        &attributes,
    )
    .unwrap();
    let credential = Credential {
        signature: signature.clone(),
        attributes: attr_map,
        issuer_pub_key: cred_issuer_key.0.clone(),
    };
    let (commitment, pok, _key) = credential_commit(
        &mut ledger.get_prng(),
        &cred_user_key.1,
        &credential,
        user_kp.get_pk_ref().as_bytes(),
    )
    .unwrap();

    let mut block = ledger.start_block().expect("starting first block failed");
    let seq_id = ledger.get_block_commit_count();
    let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
    txn_builder
        .add_operation_air_assign(
            &user_kp,
            cred_user_key.0.clone(),
            commitment,
            cred_issuer_key.0,
            pok,
        )
        .unwrap();
    let tx = txn_builder.transaction();
    let effect0 = TxnEffect::compute_effect(tx.clone()).expect("compute effect0 failed");
    let temp_sid0 = ledger
        .apply_transaction(&mut block, effect0.clone())
        .expect("apply transaction0 failed");

    // Test 1: replay the exact same txn, it should fail
    let effect1 = TxnEffect::compute_effect(tx.clone()).expect("compute effect1 failed");
    assert!(
        ledger
            .apply_transaction(&mut block, effect1.clone())
            .is_err()
    );
    ledger
        .finish_block(block)
        .unwrap()
        .remove(&temp_sid0)
        .expect("finishing block failed");

    /*
      // apply_transaction(&mut ledger, tx.clone());

      // Immediately replaying should fail
      let air_assign_op2 = air_assign_op.clone();
      let tx2 = Transaction::from_operation(Operation::AIRAssign(air_assign_op2), seq_id);
      let effect = TxnEffect::compute_effect(tx2).unwrap();
      assert!(ledger.status.check_txn_effects(effect).is_err());

      // If we reset the key it should fail
      let mut adversarial_op = air_assign_op.clone();
      adversarial_op.pubkey = XfrKeyPair::generate(&mut ledger.get_prng()).get_pk();
      let tx = Transaction::from_operation(Operation::AIRAssign(adversarial_op), seq_id);
      let effect = TxnEffect::compute_effect(tx);
      assert!(effect.is_err());
      let authenticated_air_res =
        ledger.get_air_data(&serde_json::to_string(&cred_user_key.0).unwrap());
      assert!(authenticated_air_res.is_valid(ledger.get_state_commitment().0));
    */
}
