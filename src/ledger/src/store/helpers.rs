//!
//! # Helper Utils
//!

use crate::data_model::AssetTypePrefix;
use fbnc::NumKey;
use {
    super::{
        IssuerKeyPair, IssuerPublicKey, LedgerState, TracingPolicies, TracingPolicy,
        TransferType, XfrNotePolicies,
    },
    crate::data_model::{
        Asset, AssetRules, AssetTypeCode, ConfidentialMemo, DefineAsset,
        DefineAssetBody, IssueAsset, IssueAssetBody, Memo, Operation, Transaction,
        TransferAsset, TransferAssetBody, TxOutput, TxnEffect, TxnSID, TxoRef, TxoSID,
    },
    globutils::SignatureOf,
    rand_core::{CryptoRng, RngCore},
    ruc::*,
    std::fmt::Debug,
    zei::xfr::{
        asset_record::AssetRecordType,
        asset_record::{build_blind_asset_record, open_blind_asset_record},
        sig::{XfrKeyPair, XfrPublicKey},
        structs::{AssetRecord, AssetRecordTemplate},
    },
    zei_crypto::basic::ristretto_pedersen_comm::RistrettoPedersenCommitment,
};

/// Create a transaction to define a custom asset
pub fn create_definition_transaction(
    code: &AssetTypeCode,
    keypair: &XfrKeyPair,
    asset_rules: AssetRules,
    memo: Option<Memo>,
    seq_id: u64,
) -> Result<Transaction> {
    let issuer_key = IssuerPublicKey {
        key: *keypair.get_pk_ref(),
    };

    let mut asset_code = AssetTypePrefix::UserDefined.bytes();
    asset_code.append(&mut code.to_bytes());

    let asset_body = DefineAssetBody::new(
        asset_code.as_slice(),
        &issuer_key,
        asset_rules,
        memo,
        None,
    )
    .c(d!())?;
    let asset_create =
        DefineAsset::new(asset_body, &IssuerKeyPair { keypair: &keypair }).c(d!())?;
    Ok(Transaction::from_operation(
        Operation::DefineAsset(asset_create),
        seq_id,
    ))
}

#[inline(always)]
#[allow(missing_docs)]
pub fn build_keys<R: CryptoRng + RngCore>(prng: &mut R) -> XfrKeyPair {
    XfrKeyPair::generate(prng)
}

#[allow(missing_docs)]
pub fn asset_creation_body(
    token_code: &AssetTypeCode,
    issuer_key: &XfrPublicKey,
    asset_rules: AssetRules,
    memo: Option<Memo>,
    confidential_memo: Option<ConfidentialMemo>,
) -> DefineAssetBody {
    let mut token = Asset {
        code: *token_code,
        issuer: IssuerPublicKey { key: *issuer_key },
        asset_rules,
        ..Default::default()
    };

    if let Some(memo) = memo {
        token.memo = memo;
    } else {
        token.memo = Memo(String::from(""));
    }

    if let Some(confidential_memo) = confidential_memo {
        token.confidential_memo = confidential_memo;
    } else {
        token.confidential_memo = ConfidentialMemo {};
    }

    DefineAssetBody {
        asset: Box::new(token),
    }
}

#[allow(missing_docs)]
pub fn asset_creation_operation(
    asset_body: &DefineAssetBody,
    iss_key: &XfrKeyPair,
) -> DefineAsset {
    let signature = SignatureOf::new(iss_key, asset_body);
    DefineAsset {
        body: asset_body.clone(),
        pubkey: IssuerPublicKey {
            key: *iss_key.get_pk_ref(),
        },
        signature,
    }
}

#[allow(missing_docs)]
pub fn apply_transaction(
    ledger: &mut LedgerState,
    tx: Transaction,
) -> (TxnSID, Vec<TxoSID>) {
    match TxnEffect::compute_effect(tx) {
        Ok(effect) => {
            let mut block = ledger.start_block().unwrap();
            let temp_sid = ledger.apply_transaction(&mut block, effect).unwrap();
            ledger
                .finish_block(block)
                .unwrap()
                .remove(&temp_sid)
                .unwrap()
        }
        Err(e) => {
            fn unwrap_failed(msg: &str, error: impl Debug) -> ! {
                panic!("{}: {:?}", msg, error)
            }
            unwrap_failed("apply_transaction: error in compute_effect", e)
        }
    }
}

#[allow(clippy::too_many_arguments)]
#[allow(missing_docs)]
pub fn create_issue_and_transfer_txn(
    ledger: &mut LedgerState,
    code: &AssetTypeCode,
    amount: u64,
    issuer_keys: &XfrKeyPair,
    recipient_pk: &XfrPublicKey,
    seq_num: u64,
) -> (Transaction, AssetRecord) {
    // issue operation
    let ar_template = AssetRecordTemplate::with_no_asset_tracing(
        amount,
        code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        issuer_keys.get_pk(),
    );

    let pc_gens = RistrettoPedersenCommitment::default();
    let (ba, _tracer_memo, owner_memo) =
        build_blind_asset_record(&mut ledger.get_prng(), &pc_gens, &ar_template, vec![]);

    let asset_issuance_body = IssueAssetBody::new(
        &code,
        seq_num,
        &[(
            TxOutput {
                id: None,
                record: ba.clone(),
                lien: None,
            },
            None,
        )],
    )
    .unwrap();
    let asset_issuance_operation = IssueAsset::new(
        asset_issuance_body,
        &IssuerKeyPair {
            keypair: &issuer_keys,
        },
    )
    .unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    // transfer operation
    let ar_template = AssetRecordTemplate::with_no_asset_tracing(
        amount,
        code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        *recipient_pk,
    );
    let ar = AssetRecord::from_template_no_identity_tracing(
        &mut ledger.get_prng(),
        &ar_template,
    )
    .unwrap();
    let mut transfer = pnk!(TransferAsset::new(pnk!(TransferAssetBody::new(
        &mut ledger.get_prng(),
        vec![TxoRef::Relative(0)],
        &[AssetRecord::from_open_asset_record_no_asset_tracing(
            open_blind_asset_record(&ba, &owner_memo, &issuer_keys).unwrap()
        )],
        &[ar.clone()],
        None,
        vec![],
        TransferType::Standard,
    )),));

    transfer.sign(&issuer_keys);
    let seq_id = ledger.get_block_commit_count();
    let mut tx = Transaction::from_operation(issue_op, seq_id);
    tx.add_operation(Operation::TransferAsset(transfer));
    (tx, ar)
}

#[allow(clippy::too_many_arguments)]
#[allow(missing_docs)]
pub fn create_issue_and_transfer_txn_with_asset_tracing(
    ledger: &mut LedgerState,
    code: &AssetTypeCode,
    amount: u64,
    issuer_keys: &XfrKeyPair,
    recipient_pk: &XfrPublicKey,
    seq_num: u64,
    tracing_policy: TracingPolicy,
) -> (Transaction, AssetRecord) {
    let tracing_policies = TracingPolicies::from_policy(tracing_policy);
    let xfr_note_policies = XfrNotePolicies::new(
        vec![tracing_policies.clone()],
        vec![None],
        vec![tracing_policies.clone()],
        vec![None],
    );
    // issue operation
    let ar_template = AssetRecordTemplate::with_asset_tracing(
        amount,
        code.val,
        AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
        issuer_keys.get_pk(),
        tracing_policies.clone(),
    );
    let pc_gens = RistrettoPedersenCommitment::default();
    let (ba, _tracer_memo, owner_memo) = build_blind_asset_record(
        &mut ledger.get_prng(),
        &pc_gens,
        &ar_template,
        vec![vec![]],
    );

    let asset_issuance_body = IssueAssetBody::new(
        &code,
        seq_num,
        &[(
            TxOutput {
                id: None,
                record: ba.clone(),
                lien: None,
            },
            None,
        )],
    )
    .unwrap();
    let asset_issuance_operation = IssueAsset::new(
        asset_issuance_body,
        &IssuerKeyPair {
            keypair: &issuer_keys,
        },
    )
    .unwrap();

    let issue_op = Operation::IssueAsset(asset_issuance_operation);

    // transfer operation
    let ar_template = AssetRecordTemplate::with_asset_tracing(
        amount,
        code.val,
        AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
        *recipient_pk,
        tracing_policies.clone(),
    );
    let ar = AssetRecord::from_template_no_identity_tracing(
        &mut ledger.get_prng(),
        &ar_template,
    )
    .unwrap();
    let tar = AssetRecord::from_open_asset_record_with_asset_tracing_but_no_identity(
        &mut ledger.get_prng(),
        open_blind_asset_record(&ba, &owner_memo, &issuer_keys).unwrap(),
        tracing_policies,
    )
    .unwrap();
    let mut transfer = TransferAsset::new(
        TransferAssetBody::new(
            &mut ledger.get_prng(),
            vec![TxoRef::Relative(0)],
            &[tar],
            &[ar.clone()],
            Some(xfr_note_policies),
            vec![],
            TransferType::Standard,
        )
        .unwrap(),
    )
    .unwrap();

    transfer.sign(&issuer_keys);
    // IssueAsset does not, so we use a default
    let seq_id = ledger.get_block_commit_count();
    let mut tx = Transaction::from_operation(issue_op, seq_id);
    tx.add_operation(Operation::TransferAsset(transfer));
    (tx, ar)
}

#[allow(missing_docs)]
pub fn create_issuance_txn(
    ledger: &mut LedgerState,
    code: &AssetTypeCode,
    amount: u64,
    seq_num: u64,
    record_type: AssetRecordType,
    issuer_keys: &XfrKeyPair,
) -> Transaction {
    // issue operation
    let ar_template = AssetRecordTemplate::with_no_asset_tracing(
        amount,
        code.val,
        record_type,
        issuer_keys.get_pk(),
    );
    let pc_gens = RistrettoPedersenCommitment::default();
    let (ba, _tracer_memo, _owner_memo) =
        build_blind_asset_record(&mut ledger.get_prng(), &pc_gens, &ar_template, vec![]);

    let asset_issuance_body = IssueAssetBody::new(
        &code,
        seq_num,
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
    let asset_issuance_operation = IssueAsset::new(
        asset_issuance_body,
        &IssuerKeyPair {
            keypair: &issuer_keys,
        },
    )
    .unwrap();
    let seq_id = ledger.get_block_commit_count();
    Transaction::from_operation(Operation::IssueAsset(asset_issuance_operation), seq_id)
}
