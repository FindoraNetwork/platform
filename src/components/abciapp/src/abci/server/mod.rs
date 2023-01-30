//!
//! # define abci and impl tendermint abci
//!

use {
    crate::{
        abci::server::callback::TENDERMINT_BLOCK_HEIGHT,
        api::submission_server::SubmissionServer,
    },
    baseapp::BaseApp as AccountBaseAPP,
    config::abci::global_cfg::CFG,
    ledger::store::LedgerState,
    parking_lot::RwLock,
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    std::{
        path::Path,
        sync::{atomic::Ordering, Arc},
    },
    tendermint_proto::abci::{
        RequestBeginBlock, RequestCheckTx, RequestDeliverTx, RequestEndBlock,
        RequestInfo, RequestInitChain, RequestQuery, ResponseBeginBlock,
        ResponseCheckTx, ResponseCommit, ResponseDeliverTx, ResponseEndBlock,
        ResponseInfo, ResponseInitChain, ResponseQuery,
    },
    tx_sender::TendermintForward,
};

pub use tx_sender::forward_txn_with_mode;

pub mod callback;
pub mod tx_sender;

/// findora impl of tendermint abci
#[derive(Clone)]
pub struct ABCISubmissionServer {
    pub la: Arc<RwLock<SubmissionServer<ChaChaRng, TendermintForward>>>,
    pub account_base_app: Arc<RwLock<AccountBaseAPP>>,
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

        let account_base_app = match basedir {
            None => {
                pnk!(AccountBaseAPP::new(
                    tempfile::tempdir().unwrap().path(),
                    CFG.disable_eth_empty_blocks,
                    CFG.arc_history,
                    CFG.arc_fresh
                ))
            }
            Some(basedir) => {
                pnk!(AccountBaseAPP::new(
                    Path::new(basedir),
                    CFG.disable_eth_empty_blocks,
                    CFG.arc_history,
                    CFG.arc_fresh
                ))
            }
        };

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
            account_base_app: Arc::new(RwLock::new(account_base_app)),
        })
    }
}

impl abci::Application for ABCISubmissionServer {
    #[inline(always)]
    fn info(&self, req: RequestInfo) -> ResponseInfo {
        callback::info(self, req)
    }

    #[inline(always)]
    fn query(&self, req: RequestQuery) -> ResponseQuery {
        callback::query(self, req)
    }

    #[inline(always)]
    fn check_tx(&self, req: RequestCheckTx) -> ResponseCheckTx {
        callback::check_tx(self, req)
    }

    #[inline(always)]
    fn init_chain(&self, req: RequestInitChain) -> ResponseInitChain {
        callback::init_chain(self, req)
    }

    #[inline(always)]
    fn begin_block(&self, req: RequestBeginBlock) -> ResponseBeginBlock {
        callback::begin_block(self, req)
    }

    #[inline(always)]
    fn deliver_tx(&self, req: RequestDeliverTx) -> ResponseDeliverTx {
        callback::deliver_tx(self, req)
    }

    #[inline(always)]
    fn end_block(&self, req: RequestEndBlock) -> ResponseEndBlock {
        callback::end_block(self, req)
    }

    #[inline(always)]
    fn commit(&self) -> ResponseCommit {
        callback::commit(self)
    }
}
