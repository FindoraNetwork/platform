//!
//! # impl function of tendermint abci
//!

mod utils;

use crate::{
    abci::{
        config::global_cfg::CFG, server::ABCISubmissionServer, staking, IN_SAFE_ITV,
    },
    api::{
        query_server::BLOCK_CREATED,
        submission_server::{convert_tx, try_tx_catalog, TxCatalog},
    },
};
use abci::{
    RequestBeginBlock, RequestCheckTx, RequestCommit, RequestDeliverTx, RequestEndBlock,
    RequestInfo, ResponseBeginBlock, ResponseCheckTx, ResponseCommit, ResponseDeliverTx,
    ResponseEndBlock, ResponseInfo,
};
use lazy_static::lazy_static;
use ledger::staking::{is_coinbase_tx, KEEP_HIST};
use parking_lot::Mutex;
use protobuf::RepeatedField;
use ruc::*;
use std::{
    fs,
    ops::Deref,
    sync::{
        atomic::{AtomicBool, AtomicI64, Ordering},
        Arc,
    },
};

static HAS_ACTUAL_TXS: AtomicBool = AtomicBool::new(false);
pub(crate) static TENDERMINT_BLOCK_HEIGHT: AtomicI64 = AtomicI64::new(0);

lazy_static! {
    /// save the request parameters from the begin_block for use in the end_block
    static ref REQ_BEGIN_BLOCK: Arc<Mutex<RequestBeginBlock>> =
        Arc::new(Mutex::new(RequestBeginBlock::new()));
}

pub fn info(s: &mut ABCISubmissionServer, _req: &RequestInfo) -> ResponseInfo {
    let mut resp = ResponseInfo::new();

    let mut la = s.la.write();

    let state = la.get_committed_state().write();
    let commitment = state.get_state_commitment();

    let h = state.get_tendermint_height() as i64;
    TENDERMINT_BLOCK_HEIGHT.swap(h, Ordering::Relaxed);

    if 1 < h {
        resp.set_last_block_app_hash(commitment.0.as_ref().to_vec());
    }

    resp.set_last_block_height(h);

    println!("\n\n");
    println!("==========================================");
    println!("======== Starting from height: {} ========", h);
    println!("==========================================");
    println!("\n\n");

    drop(state);
    if la.all_commited() {
        la.begin_block();
    }

    resp
}

/// any new tx will trigger this callback before it can enter the mem-pool of tendermint
#[inline(always)]
pub fn check_tx(
    _s: &mut ABCISubmissionServer,
    _req: &RequestCheckTx,
) -> ResponseCheckTx {
    ResponseCheckTx::new()
}

pub fn begin_block(
    s: &mut ABCISubmissionServer,
    req: &RequestBeginBlock,
) -> ResponseBeginBlock {
    // cache the last block in query server
    // trigger this op in `BeginBlock` to make abci-commit safer
    if HAS_ACTUAL_TXS.swap(false, Ordering::Relaxed) {
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
    if let Some(tx) = convert_tx(req.get_tx()) {
        if !is_coinbase_tx(&tx)
            && tx.is_basic_valid(TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed))
        {
            if *KEEP_HIST {
                // set attr(tags) if any, only needed on a fullnode
                let attr = utils::gen_tendermint_attr(&tx);
                if !attr.is_empty() {
                    resp.set_events(attr);
                }
            }

            if s.la.write().cache_transaction(tx).is_ok() {
                return resp;
            }

            resp
        }
        TxCatalog::EvmTx => {
            return s.account_base_app.write().deliver_tx(req);
        }
        TxCatalog::Unknown => {
            resp.code = 1;
            resp.log = String::from("Failed to deliver transaction!");
            resp
        }
    }

    resp.set_code(1);
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
        HAS_ACTUAL_TXS.swap(true, Ordering::Relaxed);
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
    state.set_tendermint_commit(TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed) as u64);

    // snapshot them finally
    let path = format!("{}/{}", &CFG.ledger_dir, &state.get_status().snapshot_file);
    pnk!(serde_json::to_vec(&state.get_status())
        .c(d!())
        .and_then(|s| fs::write(&path, s).c(d!(path))));

    let mut r = ResponseCommit::new();
    r.set_data(state.get_state_commitment().0.as_ref().to_vec());
    r
}
