//!
//! # impl function of tendermint abci
//!

mod utils;

use crate::{
    abci::{server::ABCISubmissionServer, staking, IN_SAFE_ITV},
    api::{query_server::BLOCK_CREATED, submission_server::convert_tx},
};
use abci::*;
use lazy_static::lazy_static;
use ledger::{data_model::TxnEffect, staking::is_coinbase_tx};
use parking_lot::Mutex;
use protobuf::RepeatedField;
use ruc::*;
use std::{
    fs,
    ops::Deref,
    sync::{
        atomic::{AtomicI64, Ordering},
        Arc,
    },
};

/// current block height
pub static TENDERMINT_BLOCK_HEIGHT: AtomicI64 = AtomicI64::new(0);

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
pub fn check_tx(_s: &mut ABCISubmissionServer, req: &RequestCheckTx) -> ResponseCheckTx {
    // Get the Tx [u8] and convert to u64
    let mut resp = ResponseCheckTx::new();

    if let Some(tx) = convert_tx(req.get_tx()) {
        if is_coinbase_tx(&tx)
            || !tx.is_basic_valid(TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed))
            || ruc::info!(TxnEffect::compute_effect(tx)).is_err()
        {
            resp.set_code(1);
        }
    } else {
        resp.set_code(1);
    }

    resp
}

pub fn begin_block(
    s: &mut ABCISubmissionServer,
    req: &RequestBeginBlock,
) -> ResponseBeginBlock {
    IN_SAFE_ITV.swap(true, Ordering::SeqCst);

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
            // set attr(tags) if any
            let attr = utils::gen_tendermint_attr(&tx);
            if !attr.is_empty() {
                resp.set_events(attr);
            }

            if s.la.write().cache_transaction(tx).is_ok() {
                return resp;
            }
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

    IN_SAFE_ITV.swap(false, Ordering::SeqCst);
    let mut la = s.la.write();

    // mint coinbase, cache system transactions to ledger
    {
        let laa = la.get_committed_state().write();
        if let Some(tx) = staking::system_mint_pay(&*laa) {
            drop(laa);
            // this unwrap should be safe
            la.cache_transaction(tx).unwrap();
        }
    }

    if !la.all_commited() && la.block_txn_count() != 0 {
        pnk!(la.end_block());

        {
            let mut created = BLOCK_CREATED.0.lock();
            *created = true;
            BLOCK_CREATED.1.notify_one();
        }
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

    let laa = la.get_committed_state().write();
    pnk!(serde_json::to_vec(&laa.get_status())
        .c(d!())
        .and_then(|s| fs::write(&laa.get_status().snapshot_path, s)
            .c(d!(laa.get_status().snapshot_path.clone()))));

    resp
}

pub fn commit(s: &mut ABCISubmissionServer, _req: &RequestCommit) -> ResponseCommit {
    let la = s.la.write();
    let mut state = la.get_committed_state().write();

    state.set_tendermint_commit(TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed) as u64);
    state.flush_data();

    let mut r = ResponseCommit::new();
    r.set_data(state.get_state_commitment().0.as_ref().to_vec());
    r
}
