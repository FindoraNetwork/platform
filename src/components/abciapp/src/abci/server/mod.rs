use crate::{
    abci::server::callback::TENDERMINT_BLOCK_HEIGHT,
    api::submission_server::SubmissionServer,
};
use ledger::store::LedgerState;
use parking_lot::RwLock;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ruc::*;
use std::{
    path::Path,
    sync::{atomic::Ordering, Arc},
};
use tendermint_sys::SyncApplication;
use tm_protos::abci::*;
use tx_sender::TendermintForward;

pub use tx_sender::forward_txn_with_mode;

pub mod callback;
pub mod tx_sender;

pub struct ABCISubmissionServer {
    pub la: Arc<RwLock<SubmissionServer<ChaChaRng, TendermintForward>>>,
}

impl ABCISubmissionServer {
    pub fn new(
        base_dir: Option<&Path>,
        tendermint_reply: String,
    ) -> Result<ABCISubmissionServer> {
        let ledger_state = match base_dir {
            None => LedgerState::test_ledger(),
            Some(base_dir) => pnk!(LedgerState::load_or_init(base_dir)),
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

impl SyncApplication for ABCISubmissionServer {
    #[inline(always)]
    fn info(&mut self, req: RequestInfo) -> ResponseInfo {
        callback::info(self, req)
    }

    #[inline(always)]
    fn check_tx(&mut self, req: RequestCheckTx) -> ResponseCheckTx {
        callback::check_tx(self, req)
    }

    #[inline(always)]
    fn deliver_tx(&mut self, req: RequestDeliverTx) -> ResponseDeliverTx {
        callback::deliver_tx(self, req)
    }

    #[inline(always)]
    fn begin_block(&mut self, req: RequestBeginBlock) -> ResponseBeginBlock {
        callback::begin_block(self, req)
    }

    #[inline(always)]
    fn end_block(&mut self, req: RequestEndBlock) -> ResponseEndBlock {
        callback::end_block(self, req)
    }

    #[inline(always)]
    fn commit(&mut self) -> ResponseCommit {
        callback::commit(self)
    }
}
