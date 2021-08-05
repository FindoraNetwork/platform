use crate::abci::{server::ABCISubmissionServer, staking};
use lazy_static::lazy_static;
use ledger::{
    data_model::TxnEffect,
    staking::is_coinbase_tx,
    store::{LedgerAccess, LedgerUpdate},
};
use parking_lot::Mutex;
use query_server::BLOCK_CREATED;
use ruc::*;
use std::sync::{
    atomic::{AtomicI64, Ordering},
    Arc,
};
use submission_server::convert_tx;
use tm_protos::abci::*;

mod pulse_cache;
mod utils;

/// current block height
pub static TENDERMINT_BLOCK_HEIGHT: AtomicI64 = AtomicI64::new(0);

lazy_static! {
    static ref REQ_BEGIN_BLOCK: Arc<Mutex<RequestBeginBlock>> =
        Arc::new(Mutex::new(RequestBeginBlock::default()));
}

pub fn info(s: &mut ABCISubmissionServer, _req: RequestInfo) -> ResponseInfo {
    let mut resp = ResponseInfo::default();

    let mut la = s.la.write();

    let mut state = la.get_committed_state().write();
    let commitment = state.get_state_commitment();
    if commitment.1 > 0 {
        let tendermint_height = commitment.1 + state.get_pulse_count();
        resp.last_block_height = tendermint_height as i64;
        resp.last_block_app_hash = commitment.0.as_ref().to_vec();
    }

    if let Ok(h) = ruc::info!(pulse_cache::read_height()) {
        resp.last_block_height = h;
    }

    if let Ok(s) = ruc::info!(pulse_cache::read_staking()) {
        *state.get_staking_mut() = s;
    }

    if let Ok(cnt) = ruc::info!(pulse_cache::read_block_pulse()) {
        drop(state);
        if la.all_commited() {
            la.begin_block();
        }
        la.restore_block_pulse(cnt);
    }

    resp
}

pub fn check_tx(_s: &mut ABCISubmissionServer, req: RequestCheckTx) -> ResponseCheckTx {
    // Get the Tx [u8] and convert to u64
    let mut resp = ResponseCheckTx::default();

    if let Some(tx) = convert_tx(&req.tx) {
        if is_coinbase_tx(&tx)
            || !tx.is_basic_valid(TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed))
            || ruc::info!(TxnEffect::compute_effect(tx)).is_err()
        {
            resp.code = 1;
            resp.log = String::from("Check failed");
        }
    } else {
        resp.code = 1;
        resp.log = String::from("Could not unpack transaction");
    }

    resp
}

pub fn deliver_tx(
    s: &mut ABCISubmissionServer,
    req: RequestDeliverTx,
) -> ResponseDeliverTx {
    let mut resp = ResponseDeliverTx::default();
    if let Some(tx) = convert_tx(&req.tx) {
        if !is_coinbase_tx(&tx)
            && tx.is_basic_valid(TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed))
        {
            // set attr(tags) if any
            let attr = utils::gen_tendermint_attr(&tx);
            if !attr.is_empty() {
                resp.events = attr;
            }

            if s.la.write().cache_transaction(tx).is_ok() {
                return resp;
            }
        }
    }
    resp.code = 1;
    resp.log = String::from("Failed to deliver transaction!");
    resp
}

pub fn begin_block(
    s: &mut ABCISubmissionServer,
    req: RequestBeginBlock,
) -> ResponseBeginBlock {
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
    drop(la);

    ResponseBeginBlock::default()
}

pub fn end_block(
    s: &mut ABCISubmissionServer,
    _req: RequestEndBlock,
) -> ResponseEndBlock {
    let mut resp = ResponseEndBlock::default();

    let begin_block_req = REQ_BEGIN_BLOCK.lock();
    let header = pnk!(begin_block_req.header.as_ref());

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

    if la.block_txn_count() == 0 {
        la.pulse_block();
    } else if !la.all_commited() {
        pnk!(la.end_block());

        {
            let mut created = BLOCK_CREATED.0.lock();
            *created = true;
            BLOCK_CREATED.1.notify_one();
        }
    }

    if let Ok(Some(vs)) = ruc::info!(staking::get_validators(
        la.get_committed_state().read().get_staking(),
        begin_block_req.last_commit_info.as_ref()
    )) {
        resp.validator_updates = vs;
    }

    staking::system_ops(
        &mut *la.get_committed_state().write(),
        &header,
        begin_block_req.last_commit_info.as_ref(),
        &begin_block_req.byzantine_validators.as_slice(),
    );

    resp
}

pub fn commit(s: &mut ABCISubmissionServer) -> ResponseCommit {
    let mut r = ResponseCommit::default();
    let la = s.la.read();

    // la.begin_commit();

    let state = la.get_committed_state().read();
    let commitment = state.get_state_commitment();

    // la.end_commit();

    r.data = commitment.0.as_ref().to_vec();

    pnk!(pulse_cache::write_height(
        TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed)
    ));

    pnk!(pulse_cache::write_staking(state.get_staking()));

    pnk!(pulse_cache::write_block_pulse(la.block_pulse_count()));

    r
}
