//!
//! # define abci and impl tendermint abci
//!

use {
    crate::{
        abci::server::callback::TENDERMINT_BLOCK_HEIGHT,
        api::submission_server::SubmissionServer,
    },
    abci::{
        RequestBeginBlock, RequestCheckTx, RequestCommit, RequestDeliverTx,
        RequestEndBlock, RequestInfo, RequestInitChain, RequestQuery,
        ResponseBeginBlock, ResponseCheckTx, ResponseCommit, ResponseDeliverTx,
        ResponseEndBlock, ResponseInfo, ResponseInitChain, ResponseQuery,
    },
    baseapp::BaseApp as AccountBaseAPP,
    config::abci::global_cfg::CFG,
    ledger::{staking::evm::EVM_STAKING, store::LedgerState},
    parking_lot::RwLock,
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    std::{
        path::Path,
        sync::{atomic::Ordering, Arc},
    },
    tx_sender::TendermintForward,
};

pub mod callback;
pub mod tx_sender;

/// findora impl of tendermint abci
#[derive(Clone)]
pub struct ABCISubmissionServer {
    pub la: Arc<RwLock<SubmissionServer<ChaChaRng, TendermintForward>>>,
    pub account_base_app: Arc<RwLock<AccountBaseAPP>>,
    pub eth_api_base_app: Option<Arc<RwLock<AccountBaseAPP>>>,
}

impl ABCISubmissionServer {
    /// create ABCISubmissionServer
    pub fn new(
        basedir: Option<&str>,
        tendermint_reply: String,
        enable_eth_api_service: bool,
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
        let account_base_app = Arc::new(RwLock::new(account_base_app));
        let eth_api_base_app = if enable_eth_api_service {
            let eth_api_base_app = Arc::new(RwLock::new(match basedir {
                None => {
                    pnk!(AccountBaseAPP::new_with_secondary(
                        tempfile::tempdir().unwrap().path(),
                        CFG.disable_eth_empty_blocks,
                        CFG.arc_history,
                        CFG.arc_fresh
                    ))
                }
                Some(basedir) => {
                    pnk!(AccountBaseAPP::new_with_secondary(
                        Path::new(basedir),
                        CFG.disable_eth_empty_blocks,
                        CFG.arc_history,
                        CFG.arc_fresh
                    ))
                }
            }));
            Some(eth_api_base_app)
        } else {
            None
        };

        if EVM_STAKING.set(account_base_app.clone()).is_err() {
            return Err(eg!("Invalid usage."));
        }
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
            account_base_app,
            eth_api_base_app,
        })
    }
}

impl abci::Application for ABCISubmissionServer {
    #[inline(always)]
    fn info(&mut self, req: &RequestInfo) -> ResponseInfo {
        callback::info(self, req)
    }

    #[inline(always)]
    fn query(&mut self, req: &RequestQuery) -> ResponseQuery {
        callback::query(self, req)
    }

    #[inline(always)]
    fn check_tx(&mut self, req: &RequestCheckTx) -> ResponseCheckTx {
        callback::check_tx(self, req)
    }

    #[inline(always)]
    fn init_chain(&mut self, req: &RequestInitChain) -> ResponseInitChain {
        callback::init_chain(self, req)
    }

    #[inline(always)]
    fn begin_block(&mut self, req: &RequestBeginBlock) -> ResponseBeginBlock {
        callback::begin_block(self, req)
    }

    #[inline(always)]
    fn deliver_tx(&mut self, req: &RequestDeliverTx) -> ResponseDeliverTx {
        callback::deliver_tx(self, req)
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
