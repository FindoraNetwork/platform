//!
//! # define abci and impl tendermint abci
//!

use crate::{
    abci::server::callback::TENDERMINT_BLOCK_HEIGHT,
    api::submission_server::SubmissionServer,
};
use abci::{
    RequestBeginBlock, RequestCheckTx, RequestCommit, RequestDeliverTx, RequestEndBlock,
    RequestInfo, ResponseBeginBlock, ResponseCheckTx, ResponseCommit, ResponseDeliverTx,
    ResponseEndBlock, ResponseInfo,
};
use ledger::store::LedgerState;
use parking_lot::RwLock;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ruc::*;
use std::sync::{atomic::Ordering, Arc};
use tx_sender::TendermintForward;

pub use tx_sender::forward_txn_with_mode;

pub mod callback;
pub mod tx_sender;

/// findora impl of tendermint abci
pub struct ABCISubmissionServer {
    pub la: Arc<RwLock<SubmissionServer<ChaChaRng, TendermintForward>>>,
}

impl ABCISubmissionServer {
    /// create ABCISubmissionServer
    pub fn new(
        basedir: Option<&str>,
        tendermint_reply: String,
    ) -> Result<ABCISubmissionServer> {
        let ledger_state = match basedir {
            None => LedgerState::tmp_ledger(),
            Some(basedir) => pnk!(LedgerState::load_or_init(basedir)),
        };
        let tendermint_height = ledger_state.get_staking().cur_height();
        TENDERMINT_BLOCK_HEIGHT.swap(tendermint_height as i64, Ordering::Relaxed);

        let prng = rand_chacha::ChaChaRng::from_entropy();
        Ok(ABCISubmissionServer {
            la: Arc::new(RwLock::new(
                SubmissionServer::new_no_auto_commit(
                    prng,
                    Arc::new(RwLock::new(ledger_state)),
                    TendermintForward { tendermint_reply },
                )
                .c(d!())?,
            )),
        })
    }
}

impl abci::Application for ABCISubmissionServer {
    #[inline(always)]
    fn info(&mut self, req: &RequestInfo) -> ResponseInfo {
        callback::info(self, req)
    }

    #[inline(always)]
    fn check_tx(&mut self, req: &RequestCheckTx) -> ResponseCheckTx {
        callback::check_tx(self, req)
    }

    #[inline(always)]
    fn deliver_tx(&mut self, req: &RequestDeliverTx) -> ResponseDeliverTx {
        callback::deliver_tx(self, req)
    }

    #[inline(always)]
    fn begin_block(&mut self, req: &RequestBeginBlock) -> ResponseBeginBlock {
        callback::begin_block(self, req)
    }

    #[inline(always)]
    fn end_block(&mut self, req: &RequestEndBlock) -> ResponseEndBlock {
        callback::end_block(self, req)
    }

    #[inline(always)]
    fn commit(&mut self, req: &RequestCommit) -> ResponseCommit {
        callback::commit(self, req)
    }
}
