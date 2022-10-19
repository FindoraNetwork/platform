#![cfg(test)]
#![allow(missing_docs)]

use {
    super::{helpers::*, *},
    crate::data_model::{
        get_abar_commitment, AssetRules, AssetTypeCode, IssueAsset, IssueAssetBody, IssuerKeyPair,
        Memo, Operation, Transaction, TransferAsset, TransferAssetBody, TransferType, TxOutput,
        TxnEffect, TxoRef, TxoSID, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY, TX_FEE_MIN,
    },
    crate::utils::{self, *},
    rand_core::SeedableRng,
    noah::{
        anon_xfr::{keys::AXfrKeyPair, structs::OpenAnonAssetRecordBuilder},
        xfr::{
            asset_record::{
                build_blind_asset_record, open_blind_asset_record, AssetRecordType,
            },
            sig::XfrKeyPair,
            structs::{AssetRecord, AssetRecordTemplate},
        },
    },
    noah_algebra::prelude::{One, Zero},
    noah_crypto::basic::pedersen_comm::PedersenCommitmentRistretto,
};

#[cfg(test)]
fn abort_block(block: BlockEffect) -> HashMap<TxnTempSID, Transaction> {
    let mut block = block;
    let txns = block.txns.drain(..);
    let ret: HashMap<TxnTempSID, Transaction> =
        block.temp_sids.drain(..).zip(txns).collect();

    block.txos.clear();
    block.output_abars.clear();
    block.new_nullifiers.clear();
    block.input_txos.clear();
    block.new_asset_codes.clear();
    block.new_issuance_nums.clear();
    block.issuance_keys.clear();

    ret
}

#[test]
fn test_compute_and_save_block_hash() {
    let mut ledger_state = LedgerState::tmp_ledger();
    let mut data = StateCommitmentData {
        bitmap: ledger_state.utxo_map.write().compute_checksum(),
        block_merkle: ledger_state.block_merkle.read().get_root_hash(),
        txns_in_block_hash: HashOf::new(&vec![]),
        previous_state_commitment: HashOf::new(&None),
        transaction_merkle_commitment: ledger_state.txn_merkle.read().get_root_hash(),
        air_commitment: BitDigest::from_slice(&[0; 32][..]).unwrap(),
        txo_count: 0,
        pulse_count: 0,
        staking: None,
    };

    let count_original = ledger_state.status.block_commit_count;

    let b = ledger_state.start_block().unwrap();
    ledger_state.finish_block(b).unwrap();
    data.block_merkle = ledger_state.block_merkle.read().get_root_hash();

    let first_hash = data.compute_commitment();

    assert_eq!(
        ledger_state
            .status
            .state_commitment_data
            .clone()
            .unwrap()
            .compute_commitment(),
        first_hash
    );
    assert_eq!(
        ledger_state
            .get_state_commitment_at_block_height(1)
            .unwrap(),
        first_hash
    );
    assert_eq!(ledger_state.status.block_commit_count, count_original + 1);
}

#[test]
fn test_asset_creation_valid() {
    let mut prng = ChaChaRng::from_entropy();
    let mut state = LedgerState::tmp_ledger();

    let keypair = build_keys(&mut prng);
    let code = AssetTypeCode::gen_random();

    let (asset_body, mut token_code) = asset_creation_body(
        &code,
        keypair.get_pk_ref(),
        AssetRules::default(),
        None,
        None,
    );

    if CFG.checkpoint.utxo_asset_prefix_height > state.get_tendermint_height() {
        token_code = code;
    }

    let asset_create = asset_creation_operation(&asset_body, &keypair);
    let seq_id = state.get_block_commit_count();
    let tx = Transaction::from_operation(Operation::DefineAsset(asset_create), seq_id);
    let effect = TxnEffect::compute_effect(tx).unwrap();
    {
        let mut block = state.start_block().unwrap();
        state.apply_transaction(&mut block, effect).unwrap();
        state.finish_block(block).unwrap();
    }

    assert!(state.get_asset_type(&token_code).is_some());

    //     assert_eq!(
    // *asset_body.asset,
    // state.get_asset_type(&token_code).unwrap().properties
    //     );

    assert_eq!(0, state.get_asset_type(&token_code).unwrap().units);
}

// Change the signature to have the wrong public key
#[test]
fn test_asset_creation_invalid_public_key() {
    // Create a valid asset creation operation.
    let mut prng = ChaChaRng::from_entropy();
    let keypair = build_keys(&mut prng);
    let (asset_body, _) = asset_creation_body(
        &AssetTypeCode::gen_random(),
        keypair.get_pk_ref(),
        AssetRules::default(),
        None,
        None,
    );
    let mut asset_create = asset_creation_operation(&asset_body, &keypair);

    // Now re-sign the operation with the wrong key.
    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let keypair = build_keys(&mut prng);

    asset_create.pubkey.key = *keypair.get_pk_ref();
    let tx = Transaction::from_operation(Operation::DefineAsset(asset_create), 0);

    assert!(TxnEffect::compute_effect(tx).is_err());
}

#[test]
fn test_asset_transfer() {
    let mut ledger = LedgerState::tmp_ledger();

    let code = AssetTypeCode::gen_random();
    let mut prng = ChaChaRng::from_entropy();
    let key_pair = XfrKeyPair::generate(&mut prng);
    let key_pair_adversary = XfrKeyPair::generate(&mut ledger.get_prng());

    let (tx, mut new_code) = create_definition_transaction(
        &code,
        &key_pair,
        AssetRules::default(),
        None,
        ledger.get_block_commit_count(),
    )
    .unwrap();

    if CFG.checkpoint.utxo_asset_prefix_height > ledger.get_tendermint_height() {
        new_code = code;
    }

    let effect = TxnEffect::compute_effect(tx).unwrap();
    {
        let mut block = ledger.start_block().unwrap();
        ledger.apply_transaction(&mut block, effect).unwrap();
        ledger.finish_block(block).unwrap();
    }

    // Issuance with two outputs
    let art = AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
    let template = AssetRecordTemplate::with_no_asset_tracing(
        100,
        new_code.val,
        art,
        key_pair.get_pk(),
    );
    let pc_gens = PedersenCommitmentRistretto::default();
    let (ba, _, _) =
        build_blind_asset_record(&mut ledger.get_prng(), &pc_gens, &template, vec![]);
    let second_ba = ba.clone();

    let asset_issuance_body = IssueAssetBody::new(
        &new_code,
        0,
        &[
            (
                TxOutput {
                    id: None,
                    record: ba,
                    lien: None,
                },
                None,
            ),
            (
                TxOutput {
                    id: None,
                    record: second_ba,
                    lien: None,
                },
                None,
            ),
        ],
    )
    .unwrap();
    let asset_issuance_operation =
        IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &key_pair })
            .unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    let tx = Transaction::from_operation(issue_op, ledger.get_block_commit_count());

    // Commit issuance to block
    let effect = TxnEffect::compute_effect(tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (_txn_sid, txos) = ledger
        .finish_block(block)
        .unwrap()
        .remove(&temp_sid)
        .unwrap();
    ledger.api_cache.as_mut().unwrap().state_commitment_version =
        ledger.status.state_commitment_versions.last();
    let state_commitment = ledger.get_state_commitment().0;

    for txo_id in &txos {
        assert!(ledger.status.utxos.contains_key(&txo_id));
        let utxo_status = ledger.get_utxo_status(*txo_id).unwrap();
        assert!(utxo_status.is_valid(state_commitment.clone()));
        assert!(utxo_status.status == UtxoStatus::Unspent);
    }

    // Store txo_sids for subsequent transfers
    let txo_sid = txos[0];
    let second_txo_id = txos[1];

    // Construct transfer operation
    let input_bar_proof = ledger.get_utxo(txo_sid).unwrap();
    let input_bar = (input_bar_proof.clone().utxo.0).record;
    let input_oar = open_blind_asset_record(&input_bar, &None, &key_pair).unwrap();
    assert!(input_bar_proof.is_valid(state_commitment));

    let output_template = AssetRecordTemplate::with_no_asset_tracing(
        100,
        new_code.val,
        art,
        key_pair_adversary.get_pk(),
    );
    let output_ar = AssetRecord::from_template_no_identity_tracing(
        &mut ledger.get_prng(),
        &output_template,
    )
    .unwrap();
    let input_ar = AssetRecord::from_open_asset_record_no_asset_tracing(input_oar);

    let mut transfer = TransferAsset::new(
        TransferAssetBody::new(
            &mut ledger.get_prng(),
            vec![TxoRef::Absolute(txo_sid)],
            &[input_ar],
            &[output_ar],
            None,
            vec![],
            TransferType::Standard,
        )
        .unwrap(),
    )
    .unwrap();

    let mut second_transfer = transfer.clone();
    transfer.sign(&key_pair);
    let tx = Transaction::from_operation(
        Operation::TransferAsset(transfer),
        ledger.get_block_commit_count(),
    );

    // Commit first transfer
    let effect = TxnEffect::compute_effect(tx).unwrap();
    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (_txn_sid, _txos) = ledger
        .finish_block(block)
        .unwrap()
        .remove(&temp_sid)
        .unwrap();
    ledger.api_cache.as_mut().unwrap().state_commitment_version =
        ledger.status.state_commitment_versions.last();
    // Ensure that previous txo is now spent
    let state_commitment = ledger.get_state_commitment().0;
    let utxo_status = ledger.get_utxo_status(TxoSID(0)).unwrap();
    assert!(utxo_status.is_valid(state_commitment.clone()));
    assert!(!input_bar_proof.is_valid(state_commitment));
    assert!(utxo_status.status == UtxoStatus::Spent);

    // Adversary will attempt to spend the same blind asset record at another index
    second_transfer.body.inputs = vec![TxoRef::Absolute(second_txo_id)];

    // Submit spend of same asset at second sid without signature
    second_transfer.body_signatures = Vec::new();
    let seq_id = ledger.get_block_commit_count();
    let tx =
        Transaction::from_operation(Operation::TransferAsset(second_transfer), seq_id);

    let effect = TxnEffect::compute_effect(tx);
    assert!(effect.is_err());
}

// Sign with the wrong key.
#[test]
fn test_asset_creation_invalid_signature() {
    // Create a valid operation.
    let mut prng = ChaChaRng::from_entropy();
    let keypair1 = build_keys(&mut prng);

    let (asset_body, _) = asset_creation_body(
        &AssetTypeCode::gen_random(),
        keypair1.get_pk_ref(),
        AssetRules::default(),
        None,
        None,
    );
    let mut asset_create = asset_creation_operation(&asset_body, &keypair1);

    // Re-sign the operation with the wrong key.
    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let keypair2 = build_keys(&mut prng);

    asset_create.pubkey.key = *keypair2.get_pk_ref();
    let tx = Transaction::from_operation(Operation::DefineAsset(asset_create), 0); // OK because no ledger interaction

    assert!(TxnEffect::compute_effect(tx).is_err());
}

#[test]
fn asset_issued() {
    let mut ledger = LedgerState::tmp_ledger();

    assert!(ledger.get_state_commitment() == (HashOf::new(&None), 0));
    let keypair = build_keys(&mut ledger.get_prng());
    let seq_id = ledger.get_block_commit_count();
    let code = AssetTypeCode::gen_random();
    let (tx, mut new_token_code) = create_definition_transaction(
        &code,
        &keypair,
        AssetRules::default(),
        None,
        seq_id,
    )
    .unwrap();

    if CFG.checkpoint.utxo_asset_prefix_height > ledger.get_tendermint_height() {
        new_token_code = code;
    }

    let effect = TxnEffect::compute_effect(tx).unwrap();
    {
        let mut block = ledger.start_block().unwrap();
        ledger.apply_transaction(&mut block, effect).unwrap();
        ledger.finish_block(block).unwrap();
    }

    let art = AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
    let ar = AssetRecordTemplate::with_no_asset_tracing(
        100,
        new_token_code.val,
        art,
        *keypair.get_pk_ref(),
    );

    let pc_gens = PedersenCommitmentRistretto::default();
    let (ba, _, _) =
        build_blind_asset_record(&mut ledger.get_prng(), &pc_gens, &ar, vec![]);
    let asset_issuance_body = IssueAssetBody::new(
        &new_token_code,
        0,
        &[(
            TxOutput {
                id: None,
                record: ba,
                lien: None,
            },
            None,
        )],
    )
    .unwrap();
    let asset_issuance_operation =
        IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &keypair })
            .unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    let seq_id = ledger.get_block_commit_count();
    let tx = Transaction::from_operation(issue_op, seq_id);
    let second_tx = tx.clone();

    let effect = TxnEffect::compute_effect(tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();

    let (txn_sid, txos) = ledger
        .finish_block(block)
        .unwrap()
        .remove(&temp_sid)
        .unwrap();

    // shouldn't be able to replay issuance
    let effect = TxnEffect::compute_effect(second_tx).unwrap();
    let mut block = ledger.start_block().unwrap();
    let result = ledger.apply_transaction(&mut block, effect);
    assert!(result.is_err());
    abort_block(block);

    let transaction = ledger.get_transaction(txn_sid).unwrap();
    let txn_id = transaction.finalized_txn.tx_id;
    ledger.api_cache.as_mut().unwrap().state_commitment_version =
        ledger.status.state_commitment_versions.last();
    let state_commitment_and_version = ledger.get_state_commitment();

    println!("utxos = {:?}", ledger.status.utxos);
    for txo_id in txos {
        assert!(ledger.status.utxos.contains_key(&txo_id));
        let utxo_status = ledger.get_utxo_status(txo_id).unwrap();
        assert!(utxo_status.is_valid(state_commitment_and_version.0.clone()));
        assert!(utxo_status.status == UtxoStatus::Unspent);
    }

    match ledger.get_block(BlockSID(0)) {
        Some(authenticated_block) => {
            assert!(authenticated_block.is_valid(state_commitment_and_version.0.clone()));
        }
        None => panic!("get_proof failed for block id 0"),
    }

    match ledger.get_transaction(txn_id) {
        Ok(authenticated_txn) => {
            assert!(
                authenticated_txn.txn_inclusion_proof.0.proof.tx_id
                    == authenticated_txn.finalized_txn.merkle_id
            );
            assert!(authenticated_txn.is_valid(state_commitment_and_version.0.clone()));
            assert!(transaction.finalized_txn == authenticated_txn.finalized_txn);
        }
        Err(_) => {
            panic!(
                    "get_proof failed for tx_id {}, merkle_id {}, block state {}, transaction state {}",
                    transaction.finalized_txn.tx_id.0,
                    transaction.finalized_txn.merkle_id,
                    ledger.block_merkle.read().state(),
                    ledger.txn_merkle.read().state()
                );
        }
    }

    // We don't actually have anything to commmit yet,
    // but this will save the empty checksum, which is
    // enough for a bit of a test.
    assert!(
        state_commitment_and_version
            == (
                ledger
                    .status
                    .state_commitment_data
                    .clone()
                    .unwrap()
                    .compute_commitment(),
                2
            )
    );
}

#[test]
pub fn test_transferable() {
    let mut ledger = LedgerState::tmp_ledger();
    let issuer = XfrKeyPair::generate(&mut ledger.get_prng());
    let alice = XfrKeyPair::generate(&mut ledger.get_prng());
    let bob = XfrKeyPair::generate(&mut ledger.get_prng());

    // Define fiat token
    let code = AssetTypeCode::gen_random();
    let seq_id = ledger.get_block_commit_count();
    let (tx, mut new_code) = create_definition_transaction(
        &code,
        &issuer,
        AssetRules::default().set_transferable(false).clone(),
        Some(Memo("test".to_string())),
        seq_id,
    )
    .unwrap();

    if CFG.checkpoint.utxo_asset_prefix_height > ledger.get_tendermint_height() {
        new_code = code;
    }

    apply_transaction(&mut ledger, tx);
    let (tx, _) = create_issue_and_transfer_txn(
        &mut ledger,
        &new_code,
        100,
        &issuer,
        alice.get_pk_ref(),
        0,
    );
    let (_, sids) = apply_transaction(&mut ledger, tx);
    let sid = sids[0];

    let bar = ledger.get_utxo_light(sid).unwrap().utxo.0.record;

    let transfer_template = AssetRecordTemplate::with_no_asset_tracing(
        100,
        new_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        bob.get_pk(),
    );
    let record = AssetRecord::from_template_no_identity_tracing(
        &mut ledger.get_prng(),
        &transfer_template,
    )
    .unwrap();

    // Cant transfer non-transferable asset
    let mut transfer = TransferAsset::new(
        TransferAssetBody::new(
            &mut ledger.get_prng(),
            vec![TxoRef::Absolute(sid)],
            &[AssetRecord::from_open_asset_record_no_asset_tracing(
                open_blind_asset_record(&bar, &None, &alice).unwrap(),
            )],
            &[record],
            None,
            vec![],
            TransferType::Standard,
        )
        .unwrap(),
    )
    .unwrap();
    transfer.sign(&alice);
    let seq_id = ledger.get_block_commit_count();
    let tx = Transaction::from_operation(Operation::TransferAsset(transfer), seq_id);
    let effect = TxnEffect::compute_effect(tx).unwrap();

    let mut block = ledger.start_block().unwrap();
    let res = ledger.apply_transaction(&mut block, effect);
    assert!(res.is_err());
    // Cant transfer by making asset confidential
    let transfer_template = AssetRecordTemplate::with_no_asset_tracing(
        100,
        new_code.val,
        AssetRecordType::ConfidentialAmount_ConfidentialAssetType,
        bob.get_pk(),
    );
    let record = AssetRecord::from_template_no_identity_tracing(
        &mut ledger.get_prng(),
        &transfer_template,
    )
    .unwrap();

    // Cant transfer non-transferable asset
    let mut transfer = TransferAsset::new(
        TransferAssetBody::new(
            &mut ledger.get_prng(),
            vec![TxoRef::Absolute(sid)],
            &[AssetRecord::from_open_asset_record_no_asset_tracing(
                open_blind_asset_record(&bar, &None, &alice).unwrap(),
            )],
            &[record],
            None,
            vec![],
            TransferType::Standard,
        )
        .unwrap(),
    )
    .unwrap();
    transfer.sign(&alice);
    let seq_id = ledger.get_block_commit_count();
    let tx = Transaction::from_operation(Operation::TransferAsset(transfer), seq_id);
    let effect = TxnEffect::compute_effect(tx).unwrap();

    let res = ledger.apply_transaction(&mut block, effect);
    assert!(res.is_err());
    // Cant transfer non-transferable asset through some intermediate operation
    // In this case, alice attempts to spend her non-transferable asset in the same transaction it
    // was issued.
    let second_transfer_template = AssetRecordTemplate::with_no_asset_tracing(
        100,
        new_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        bob.get_pk(),
    );
    let second_record = AssetRecord::from_template_no_identity_tracing(
        &mut ledger.get_prng(),
        &second_transfer_template,
    )
    .unwrap();
    let (mut tx, ar) = create_issue_and_transfer_txn(
        &mut ledger,
        &new_code,
        100,
        &issuer,
        alice.get_pk_ref(),
        1,
    );
    let mut transfer = TransferAsset::new(
        TransferAssetBody::new(
            &mut ledger.get_prng(),
            vec![TxoRef::Relative(0)],
            &[AssetRecord::from_open_asset_record_no_asset_tracing(
                ar.open_asset_record,
            )],
            &[second_record],
            None,
            vec![],
            TransferType::Standard,
        )
        .unwrap(),
    )
    .unwrap();
    transfer.sign(&alice);
    tx.body.operations.push(Operation::TransferAsset(transfer));
    let effect = TxnEffect::compute_effect(tx).unwrap();
    let res = ledger.apply_transaction(&mut block, effect);
    assert!(res.is_err());
}

#[test]
pub fn test_max_units() {
    let mut ledger = LedgerState::tmp_ledger();

    let issuer = XfrKeyPair::generate(&mut ledger.get_prng());

    // Define fiat token
    let code = AssetTypeCode::gen_random();
    let seq_id = ledger.get_block_commit_count();
    let (tx, mut new_code) = create_definition_transaction(
        &code,
        &issuer,
        AssetRules::default().set_max_units(Some(100)).clone(),
        Some(Memo("test".to_string())),
        seq_id,
    )
    .unwrap();

    if CFG.checkpoint.utxo_asset_prefix_height > ledger.get_tendermint_height() {
        new_code = code;
    }

    apply_transaction(&mut ledger, tx);
    let tx = create_issuance_txn(
        &mut ledger,
        &new_code,
        50,
        0,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        &issuer,
    );
    apply_transaction(&mut ledger, tx);
    {
        // Ensure that a single overlfowing transaction fails
        let tx = create_issuance_txn(
            &mut ledger,
            &new_code,
            51,
            1,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            &issuer,
        );
        let effect = TxnEffect::compute_effect(tx).unwrap();

        let mut block = ledger.start_block().unwrap();
        let res = ledger.apply_transaction(&mut block, effect);
        assert!(res.is_err());

        // Ensure that cap can be reached
        let tx = create_issuance_txn(
            &mut ledger,
            &new_code,
            50,
            1,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            &issuer,
        );
        let effect = TxnEffect::compute_effect(tx).unwrap();
        ledger.apply_transaction(&mut block, effect).unwrap();
        ledger.finish_block(block).unwrap();

        // Cant try to exceed asset cap by issuing confidentially
        let tx = create_issuance_txn(
            &mut ledger,
            &new_code,
            1,
            2,
            AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
            &issuer,
        );
        let effect = TxnEffect::compute_effect(tx).unwrap();
        let mut block = ledger.start_block().unwrap();
        let res = ledger.apply_transaction(&mut block, effect);
        assert!(res.is_err());
    }
}

fn gen_fee_operation(
    l: &mut LedgerState,
    txo_sid: TxoSID,
    fra_owner_kp: &XfrKeyPair,
) -> Operation {
    let fra_code = &AssetTypeCode {
        val: ASSET_TYPE_FRA,
    };

    let input_bar_proof = l.get_utxo_light(txo_sid).unwrap();
    let input_bar = (input_bar_proof.utxo.0).record;
    let input_oar = open_blind_asset_record(&input_bar, &None, &fra_owner_kp).unwrap();

    let output_template = AssetRecordTemplate::with_no_asset_tracing(
        input_oar.amount - TX_FEE_MIN,
        fra_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        fra_owner_kp.get_pk(),
    );
    let output_ar = AssetRecord::from_template_no_identity_tracing(
        &mut l.get_prng(),
        &output_template,
    )
    .unwrap();

    let output_template = AssetRecordTemplate::with_no_asset_tracing(
        TX_FEE_MIN,
        fra_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        *BLACK_HOLE_PUBKEY,
    );
    let output_ar_fee = AssetRecord::from_template_no_identity_tracing(
        &mut l.get_prng(),
        &output_template,
    )
    .unwrap();

    let input_ar = AssetRecord::from_open_asset_record_no_asset_tracing(input_oar);

    let mut transfer = TransferAsset::new(
        TransferAssetBody::new(
            &mut l.get_prng(),
            vec![TxoRef::Absolute(txo_sid)],
            &[input_ar],
            &[output_ar, output_ar_fee],
            None,
            vec![],
            TransferType::Standard,
        )
        .unwrap(),
    )
    .unwrap();

    transfer.sign(&fra_owner_kp);

    Operation::TransferAsset(transfer)
}

#[test]
fn test_check_fee_with_ledger() {
    let mut ledger = LedgerState::tmp_ledger();
    let fra_owner_kp = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());

    let tx = utils::fra_gen_initial_tx(&fra_owner_kp);
    assert!(tx.check_fee());

    let effect = TxnEffect::compute_effect(tx.clone()).unwrap();
    let mut block = ledger.start_block().unwrap();
    let tmp_sid = ledger.apply_transaction(&mut block, effect).unwrap();
    let txo_sid = ledger
        .finish_block(block)
        .unwrap()
        .remove(&tmp_sid)
        .unwrap()
        .1[0];

    let tx2 = Transaction::from_operation(
        gen_fee_operation(&mut ledger, txo_sid, &fra_owner_kp),
        1,
    );
    assert!(tx2.check_fee());

    let effect = TxnEffect::compute_effect(tx2).unwrap();
    let mut block = ledger.start_block().unwrap();
    ledger.apply_transaction(&mut block, effect).unwrap();
    ledger.finish_block(block).unwrap();

    // Ensure that FRA can only be defined only once.
    let effect = TxnEffect::compute_effect(tx).unwrap();
    let mut block = ledger.start_block().unwrap();
    assert!(ledger.apply_transaction(&mut block, effect).is_err());
}

#[test]
fn test_update_anon_stores() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);

    let mut state = LedgerState::tmp_ledger();

    let nullifiers = vec![
        Nullifier::zero() as Nullifier,
        Nullifier::one() as Nullifier,
    ];

    let pub_key = AXfrKeyPair::generate(&mut prng).get_public_key();
    let oabar = OpenAnonAssetRecordBuilder::new()
        .amount(123)
        .asset_type(noah::xfr::structs::AssetType([39u8; 32]))
        .pub_key(&pub_key)
        .finalize(&mut prng)
        .unwrap()
        .build()
        .unwrap();
    let oabar2 = OpenAnonAssetRecordBuilder::new()
        .amount(123)
        .asset_type(noah::xfr::structs::AssetType([39u8; 32]))
        .pub_key(&pub_key)
        .finalize(&mut prng)
        .unwrap()
        .build()
        .unwrap();
    let output_abars = vec![
        vec![AnonAssetRecord::from_oabar(&oabar)],
        vec![AnonAssetRecord::from_oabar(&oabar2)],
    ];
    let new_com = get_abar_commitment(oabar);
    let new_com2 = get_abar_commitment(oabar2);
    let tx_block = vec![
        FinalizedTransaction {
            txn: Default::default(),
            tx_id: Default::default(),
            txo_ids: vec![],
            atxo_ids: vec![],
            merkle_id: 0,
        },
        FinalizedTransaction {
            txn: Default::default(),
            tx_id: Default::default(),
            txo_ids: vec![],
            atxo_ids: vec![],
            merkle_id: 0,
        },
    ];

    let str0 = bs58::encode(&BLSScalar::zero().noah_to_bytes()).into_string();
    let d0: Key = Key::from_base58(&str0).unwrap();
    assert!(state.nullifier_set.read().get(&d0).unwrap().is_none());

    let str1 = bs58::encode(&BLSScalar::one().noah_to_bytes()).into_string();
    let d1: Key = Key::from_base58(&str1).unwrap();
    assert!(state.nullifier_set.read().get(&d1).unwrap().is_none());

    let res = state.update_anon_stores(nullifiers, output_abars, 0, tx_block);
    assert!(res.is_ok());

    let res2 = state.commit_anon_changes();
    assert!(res2.is_ok());
    assert_eq!(res2.unwrap(), 1);

    assert!(state.nullifier_set.read().get(&d0).unwrap().is_some());
    assert!(state.nullifier_set.read().get(&d1).unwrap().is_some());

    assert_eq!(state.status.next_atxo.0, 2);
    assert_eq!(
        state.status.ax_txo_to_txn_location.get(&ATxoSID(0)),
        Some((TxnSID(0), OutputPosition(0)))
    );
    assert_eq!(
        state.status.ax_txo_to_txn_location.get(&ATxoSID(1)),
        Some((TxnSID(1), OutputPosition(0)))
    );

    assert_eq!(state.status.owned_ax_utxos.get(&new_com), Some(ATxoSID(0)));
    assert_eq!(state.status.owned_ax_utxos.get(&new_com2), Some(ATxoSID(1)));
}
