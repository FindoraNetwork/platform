use fin_db::{FinDB, RocksDB};
use primitive_types::{H160, H256};
use std::{collections::HashMap, sync::Arc};
use storage::state::{ChainState, State};
use tendermint_proto::types::Header;

pub use parking_lot::RwLock;

#[derive(Clone, PartialEq, Eq, Debug, Hash, Copy)]
pub enum RunTxMode {
    None = 0,
    /// Check a transaction
    Check = 1,
    /// Recheck a (pending) transaction after a commit
    ReCheck = 2,
    /// Simulate a transaction
    Simulate = 3,
    /// Deliver a transaction
    Deliver = 4,
}

pub type SignerCache = HashMap<H256, Option<H160>>;

#[derive(Clone, Default)]
pub struct EthereumCache {
    pub current: Arc<RwLock<SignerCache>>,
    pub history_1: Arc<RwLock<SignerCache>>,
    pub history_n: Arc<RwLock<SignerCache>>,
}

#[derive(Clone)]
pub struct Context {
    pub state: Arc<RwLock<State<FinDB>>>,
    pub db: Arc<RwLock<State<RocksDB>>>,
    pub run_mode: RunTxMode,
    pub header: Header,
    pub header_hash: Vec<u8>,
    pub eth_cache: EthereumCache,
}

impl Context {
    pub fn new(
        state_merkle: Arc<RwLock<ChainState<FinDB>>>,
        state_db: Arc<RwLock<ChainState<RocksDB>>>,
    ) -> Self {
        Context {
            state: Arc::new(RwLock::new(State::new(state_merkle, true))),
            db: Arc::new(RwLock::new(State::new(state_db, false))),
            run_mode: RunTxMode::None,
            header: Default::default(),
            header_hash: vec![],
            eth_cache: Default::default(),
        }
    }

    pub fn state_at(&self, height: u64) -> Option<Self> {
        let state = self.state.read().state_at(height);
        let db = State::new(self.db.read().chain_state(), false);
        match state {
            Ok(state) => Some(Context {
                state: Arc::new(RwLock::new(state)),
                db: Arc::new(RwLock::new(db)),
                run_mode: RunTxMode::None,
                header: Default::default(),
                header_hash: Default::default(),
                eth_cache: Default::default(),
            }),
            _ => None,
        }
    }

    pub fn copy_with_state(&self) -> Self {
        Context {
            state: Arc::new(RwLock::new(self.state.read().copy())),
            db: Arc::new(RwLock::new(self.db.read().copy())),
            run_mode: RunTxMode::None,
            header: self.header.clone(),
            header_hash: self.header_hash(),
            eth_cache: Default::default(),
        }
    }

    pub fn copy_with_new_state(&self) -> Self {
        Context {
            state: Arc::new(RwLock::new(State::new(
                self.state.read().chain_state(),
                true,
            ))),
            db: Arc::new(RwLock::new(State::new(self.db.read().chain_state(), false))),
            run_mode: RunTxMode::None,
            header: self.header.clone(),
            header_hash: self.header_hash(),
            eth_cache: Default::default(),
        }
    }
}

impl Context {
    pub fn run_mode(&self) -> RunTxMode {
        self.run_mode
    }

    pub fn block_header(&self) -> &Header {
        &self.header
    }

    pub fn header_hash(&self) -> Vec<u8> {
        self.header_hash.clone()
    }
}
