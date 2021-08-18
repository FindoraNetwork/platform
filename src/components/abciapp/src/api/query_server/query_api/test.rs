//!
//! # Test query server
//!

#![allow(warnings)]

use crate::api::query_server::query_api::QueryServer;
use finutils::txn_builder::{BuildsTransactions, TransactionBuilder};
use lazy_static::lazy_static;
use ledger::{
    data_model::{
        AccountAddress, AssetRules, AssetTypeCode, IssuerPublicKey, Operation,
        Transaction, TxOutput, TxnEffect, TxnSID, TxoRef, TxoSID, Utxo, XfrAddress,
        ASSET_TYPE_FRA,
    },
    staking::ops::mint_fra::{MintEntry, MintFraOps, MintKind},
    store::LedgerState,
};
use metrics_exporter_prometheus::PrometheusHandle;
use parking_lot::RwLock;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ruc::*;
use std::{
    borrow::BorrowMut,
    collections::{BTreeMap, HashSet},
    sync::Arc,
};
use zei::{
    api::anon_creds::ACCommitment,
    setup::PublicParams,
    xfr::{
        asset_record::{
            build_blind_asset_record, open_blind_asset_record, AssetRecordType,
        },
        sig::XfrKeyPair,
        structs::{
            AssetRecordTemplate, AssetTracerKeyPair, OwnerMemo, TracingPolicies,
            TracingPolicy,
        },
    },
};

lazy_static! {
    static ref LEDGER: Arc<RwLock<LedgerState>> =
        Arc::new(RwLock::new(LedgerState::test_ledger()));
    static ref QS: Arc<RwLock<QueryServer>> = Arc::new(RwLock::new(create_server()));
}

fn create_server() -> QueryServer {
    let builder = metrics_exporter_prometheus::PrometheusBuilder::new();
    let recorder = builder.build();

    let mut qs = QueryServer::new(LEDGER.clone());
    qs
}

fn apply_transaction(tx: Transaction) -> Option<(TxnSID, Vec<TxoSID>)> {
    let effect = pnk!(TxnEffect::compute_effect(tx));
    let mut ledger = LEDGER.write();
    let mut block = pnk!(ledger.start_block());
    let temp_sid = pnk!(ledger.apply_transaction(&mut block, effect, false));
    pnk!(ledger.finish_block(block)).remove(&temp_sid)
}

/// process
/// *. define
/// *. issue
/// *. mint
/// test query sever function
/// 1. get_address_of_sid
/// 2. get_owned_utxo_sids
/// 3. get_coinbase_entries
/// 4. get_traced_assets
/// 5. get_issued_records
/// 6. get_owner_memo
/// 7. get_created_assets
/// 8. get_issued_records_by_code
/// 9. get_transaction_hash
/// 10. get_transaction_sid
/// 11. get_commits
/// 12. get_related_transfers (not complete)
/// 13. get_related_transactions
fn test_scene_1() -> Result<()> {
    let mut prng = ChaChaRng::from_entropy();
    let mut code = AssetTypeCode::gen_random();
    let x_kp = XfrKeyPair::generate(&mut prng);
    let params = PublicParams::default();

    // define
    let mut builder =
        TransactionBuilder::from_seq_id(LEDGER.read().get_block_commit_count());

    {
        // if get traced assets,must create AssetRules from TracingPolicy
        let asset_tracer_key_pair = AssetTracerKeyPair::generate(&mut prng);
        let tracing_policy = TracingPolicy {
            enc_keys: asset_tracer_key_pair.enc_key,
            asset_tracing: false,
            identity_tracing: None,
        };

        let mut ar = AssetRules::default();
        ar.add_tracing_policy(tracing_policy);

        builder
            .add_operation_create_asset(&x_kp, Some(code), ar, "test")
            .c(d!());
    }

    let tx = builder.transaction();
    let (define_txns, _) = pnk!(apply_transaction(tx.clone()));

    // issue
    let mut builder =
        TransactionBuilder::from_seq_id(LEDGER.read().get_block_commit_count());

    {
        let mut prng = ChaChaRng::from_entropy();
        let ar = AssetRecordTemplate::with_no_asset_tracing(
            1000,
            code.val,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            x_kp.clone().get_pk(),
        );
        let (ba, _, owner_memo) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &ar, vec![]);
        let (om, (_, _)) =
            pnk!(OwnerMemo::from_amount(&mut prng, 1000, &x_kp.pub_key).c(d!()));

        builder
            .add_operation_issue_asset(
                &x_kp,
                &code,
                LEDGER.read().get_block_commit_count(),
                &[(
                    TxOutput {
                        id: None,
                        record: ba,
                        lien: None,
                    },
                    Some(om),
                )],
            )
            .c(d!());
    }

    let issue_tx = builder.transaction();
    let (issue_txns, issue_txos) = pnk!(apply_transaction(issue_tx.clone()));

    // mint_ops
    let mint_ops = Operation::MintFra(MintFraOps::new(
        0u64,
        vec![
            MintEntry::new(MintKind::Claim, x_kp.pub_key, None, 100, ASSET_TYPE_FRA),
            MintEntry::new(MintKind::UnStake, x_kp.pub_key, None, 900, ASSET_TYPE_FRA),
        ],
    ));
    let mint_tx =
        Transaction::from_operation(mint_ops, LEDGER.read().get_block_commit_count());
    let (mint_txns, mint_txos) = pnk!(apply_transaction(mint_tx.clone()));

    // A necessary step, update data from tendermint on query server
    QS.write().update();

    if let Some(set) = QS
        .read()
        .get_related_transactions(&XfrAddress { key: x_kp.pub_key })
    {
        assert!(
            set.contains(&define_txns)
                && set.contains(&issue_txns)
                && set.contains(&mint_txns)
        );
    }

    let seq_id = QS.read().get_commits();
    assert_eq!(3, seq_id); // define,issue,mint_fra

    if let Some(issue_tx_hash) = QS.read().get_transaction_hash(issue_txns) {
        assert_eq!(issue_tx_hash, &issue_tx.hash_tm().hex().to_uppercase());

        if let Some(issue_txn_sid) = QS.read().get_transaction_sid(issue_tx_hash.clone())
        {
            assert_eq!(issue_txn_sid, &issue_txns);
        }
    }

    if let Some(mint_tx_hash) = QS.read().get_transaction_hash(mint_txns) {
        assert_eq!(mint_tx_hash, &mint_tx.hash_tm().hex().to_uppercase());

        if let Some(mint_txn_sid) = QS.read().get_transaction_sid(mint_tx_hash.clone()) {
            assert_eq!(mint_txn_sid, &mint_txns);
        }
    }

    for (i, ts) in issue_txos.iter().chain(mint_txos.iter()).enumerate() {
        match i {
            0 => {
                if let Some(om) = QS.read().get_owner_memo(*ts) {
                    assert_eq!(om.decrypt_amount(&x_kp).ok(), Some(1000));
                }
            }
            1 | 2 => {
                assert_eq!(None, QS.read().get_owner_memo(*ts));
            }
            _ => {}
        }
    }

    if let Some(v) = QS.read().get_issued_records_by_code(&code) {
        assert_eq!(v[0].0.record.amount.get_amount(), Some(1000));
        assert_eq!(v[0].0.record.public_key, x_kp.pub_key);
    }

    if let Some(define_assets) = QS.read().get_created_assets(&IssuerPublicKey {
        key: x_kp.pub_key.clone(),
    }) {
        assert_eq!(define_assets.clone()[0].pubkey.key, x_kp.pub_key);
    }

    let records = pnk!(QS.read().get_issued_records(&IssuerPublicKey {
        key: x_kp.pub_key.clone()
    }));
    assert_eq!(records[0].0.record.public_key, x_kp.pub_key.clone());

    let atc_vec = pnk!(QS
        .read()
        .get_traced_assets(&IssuerPublicKey {
            key: x_kp.pub_key.clone()
        })
        .cloned());
    assert_eq!(atc_vec[0], code);

    let (_, result) = QS
        .read()
        .get_coinbase_entries(&XfrAddress { key: x_kp.pub_key }, 0, 5, true)
        .unwrap();

    // judgement api resp
    let judgement_mint_result =
        move |result: Vec<(u64, MintEntry)>, mut amounts: Vec<u64>| {
            amounts.reverse();
            for (amount, (_block_height, mint_entry)) in
                amounts.iter().zip(result.iter())
            {
                assert_eq!(*amount, mint_entry.amount);
                assert_eq!(Some(*amount), mint_entry.utxo.record.amount.get_amount());
            }
        };
    judgement_mint_result(result, vec![100, 900]);

    for (_, ts) in issue_txos.iter().chain(mint_txos.iter()).enumerate() {
        let op = QS.read().get_address_of_sid(*ts).cloned();
        assert_eq!(Some(XfrAddress { key: x_kp.pub_key }), op);
    }

    let op = QS
        .read()
        .get_owned_utxo_sids(&XfrAddress {
            key: x_kp.pub_key.clone(),
        })
        .cloned();

    let map = pnk!(LEDGER.read().get_owned_utxos(&x_kp.get_pk()));
    let judgement_get_utxo_sids_result =
        move |set: HashSet<TxoSID>, map: BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)>| {
            for txo_sid in set.iter() {
                assert!(map.get(txo_sid).is_some())
            }
        };
    judgement_get_utxo_sids_result(op.unwrap(), map);

    Ok(())
}

#[test]
fn test() {
    pnk!(test_scene_1());
}
