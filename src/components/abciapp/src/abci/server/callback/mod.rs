//!
//! # impl function of tendermint abci
//!

mod utils;

use {
    crate::{
        abci::{
            config::global_cfg::CFG, server::ABCISubmissionServer, staking, IN_SAFE_ITV,
            POOL,
        },
        api::{query_server::BLOCK_CREATED, submission_server::convert_tx},
    },
    abci::{
        CheckTxType, RequestBeginBlock, RequestCheckTx, RequestCommit, RequestDeliverTx,
        RequestEndBlock, RequestInfo, ResponseBeginBlock, ResponseCheckTx,
        ResponseCommit, ResponseDeliverTx, ResponseEndBlock, ResponseInfo,
    },
    lazy_static::lazy_static,
    ledger::{
        staking::KEEP_HIST,
        store::{
            api_cache,
            fbnc::{new_mapx, Mapx},
        },
    },
    parking_lot::{Mutex, RwLock},
    protobuf::RepeatedField,
    ruc::*,
    std::{
        fs,
        ops::Deref,
        sync::{
            atomic::{AtomicI64, Ordering},
            Arc,
        },
    },
};

pub(crate) static TENDERMINT_BLOCK_HEIGHT: AtomicI64 = AtomicI64::new(0);

lazy_static! {
    // save the request parameters from the begin_block for use in the end_block
    static ref REQ_BEGIN_BLOCK: Arc<Mutex<RequestBeginBlock>> =
        Arc::new(Mutex::new(RequestBeginBlock::new()));
    // avoid on-chain-existing transactions to be stored again
    static ref TX_HISTORY: Arc<RwLock<Mapx<Vec<u8>, bool>>> =
        Arc::new(RwLock::new(new_mapx!("tx_history")));
}

pub fn info(s: &mut ABCISubmissionServer, _req: &RequestInfo) -> ResponseInfo {
    let mut resp = ResponseInfo::new();

    let mut la = s.la.write();
    let state = la.get_committed_state().write();

    let h = state.get_tendermint_height() as i64;
    TENDERMINT_BLOCK_HEIGHT.swap(h, Ordering::Relaxed);
    resp.set_last_block_height(h);
    if 0 < h {
        let la_hash = state.get_state_commitment().0.as_ref().to_vec();
        resp.set_last_block_app_hash(la_hash);
    }

    drop(state);

    println!("\n\n");
    println!("==========================================");
    println!("======== Last committed height: {} ========", h);
    println!("==========================================");
    println!("\n\n");

    if la.all_commited() {
        la.begin_block();
    }

    resp
}

/// any new tx will trigger this callback before it can enter the mem-pool of tendermint
pub fn check_tx(_s: &mut ABCISubmissionServer, req: &RequestCheckTx) -> ResponseCheckTx {
    let mut resp = ResponseCheckTx::new();

    if matches!(req.field_type, CheckTxType::New) {
        if let Ok(tx) = convert_tx(req.get_tx()) {
            if !tx.valid_in_abci() {
                resp.log = "Should not appear in ABCI".to_owned();
                resp.code = 1;
            } else if TX_HISTORY.read().contains_key(&tx.hash_tm_rawbytes()) {
                resp.log = "Historical transaction".to_owned();
                resp.code = 1;
            }
        } else {
            resp.log = "Invalid format".to_owned();
            resp.code = 1;
        }
    }

    resp
}

pub fn begin_block(
    s: &mut ABCISubmissionServer,
    req: &RequestBeginBlock,
) -> ResponseBeginBlock {
    #[cfg(target_os = "linux")]
    {
        // snapshot the last block
        ledger::store::fbnc::flush_data();
        let last_height = TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed);
        info_omit!(CFG.btmcfg.snapshot(last_height as u64));
    }

    // notify here to make abci-commit safer
    //
    // NOTE:
    // We must ensure that the ledger lock is not occupied,
    // otherwise the peer will not perform any substantial operations,
    // because there is a `try_lock` mechanism used to avoid deadlock
    {
        let mut created = BLOCK_CREATED.0.lock();
        *created = true;
        BLOCK_CREATED.1.notify_one();
    }

    IN_SAFE_ITV.swap(true, Ordering::Relaxed);

    let header = pnk!(req.header.as_ref());
    TENDERMINT_BLOCK_HEIGHT.swap(header.height, Ordering::Relaxed);

    *REQ_BEGIN_BLOCK.lock() = req.clone();

    let mut la = s.la.write();

    // set height first
    la.get_committed_state()
        .write()
        .get_staking_mut()
        .set_custom_block_height(header.height as u64);

    // then create new block or update simulator
    if la.all_commited() {
        la.begin_block();
    } else {
        pnk!(la.update_staking_simulator());
    }

    ResponseBeginBlock::new()
}

pub fn deliver_tx(
    s: &mut ABCISubmissionServer,
    req: &RequestDeliverTx,
) -> ResponseDeliverTx {
    let mut resp = ResponseDeliverTx::new();

    if let Ok(tx) = convert_tx(req.get_tx()) {
        let txhash = tx.hash_tm_rawbytes();
        POOL.spawn_ok(async move {
            TX_HISTORY.write().set_value(txhash, Default::default());
        });

        if tx.valid_in_abci() {
            if *KEEP_HIST {
                // set attr(tags) if any, only needed on a fullnode
                let attr = utils::gen_tendermint_attr(&tx);
                if !attr.is_empty() {
                    resp.set_events(attr);
                }
            }

            if let Err(e) = s.la.write().cache_transaction(tx) {
                resp.code = 1;
                resp.log = e.to_string();
            }
        } else {
            resp.code = 1;
            resp.log = "Should not appear in ABCI".to_owned();
        }
    } else {
        resp.code = 1;
        resp.log = "Invalid data format".to_owned();
    }

    resp
}

/// putting block in the ledgerState
pub fn end_block(
    s: &mut ABCISubmissionServer,
    _req: &RequestEndBlock,
) -> ResponseEndBlock {
    let mut resp = ResponseEndBlock::new();

    let begin_block_req = REQ_BEGIN_BLOCK.lock();
    let header = pnk!(begin_block_req.header.as_ref());

    IN_SAFE_ITV.swap(false, Ordering::Relaxed);
    let mut la = s.la.write();

    // mint coinbase, cache system transactions to ledger
    {
        let laa = la.get_committed_state().read();
        if let Some(tx) = staking::system_mint_pay(&*laa) {
            drop(laa);
            // this unwrap should be safe
            la.cache_transaction(tx).unwrap();
        }
    }

    if !la.all_commited() && la.block_txn_count() != 0 {
        pnk!(la.end_block());
    }

    if let Ok(Some(vs)) = ruc::info!(staking::get_validators(
        la.get_committed_state().read().get_staking().deref(),
        begin_block_req.last_commit_info.as_ref()
    )) {
        resp.set_validator_updates(RepeatedField::from_vec(vs));
    }

    staking::system_ops(
        &mut *la.get_committed_state().write(),
        &header,
        begin_block_req.last_commit_info.as_ref(),
        &begin_block_req.byzantine_validators.as_slice(),
    );

    resp
}

pub fn commit(s: &mut ABCISubmissionServer, _req: &RequestCommit) -> ResponseCommit {
    let la = s.la.write();
    let mut state = la.get_committed_state().write();

    // will change `struct LedgerStatus`
    let td_height = TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed);
    state.set_tendermint_height(td_height as u64);

    // cache last block for QueryServer
    pnk!(api_cache::update_api_cache(&mut state));

    // snapshot them finally
    let path = format!("{}/{}", &CFG.ledger_dir, &state.get_status().snapshot_file);
    pnk!(serde_json::to_vec(&state.get_status())
        .c(d!())
        .and_then(|s| fs::write(&path, s).c(d!(path))));

    let mut r = ResponseCommit::new();
    r.set_data(state.get_state_commitment().0.as_ref().to_vec());

    r
}
