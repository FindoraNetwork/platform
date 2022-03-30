use abci::Header;
use storage::{
    db::{FinDB, RocksDB},
    state::{ChainState, State},
};

pub use parking_lot::{Mutex, RwLock};
pub use std::sync::Arc;

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

#[derive(Clone)]
pub struct Context {
    pub state: Arc<RwLock<State<FinDB>>>,
    pub db: Arc<RwLock<State<RocksDB>>>,
    pub run_mode: RunTxMode,
    pub header: Header,
    pub header_hash: Vec<u8>,
    pub tx_hash: Arc<Mutex<Option<String>>>,
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
            tx_hash: Arc::new(Mutex::new(None)),
        }
    }

    pub fn copy_with_state(&self) -> Self {
        Context {
            state: Arc::new(RwLock::new(self.state.read().copy())),
            db: Arc::new(RwLock::new(self.db.read().copy())),
            run_mode: RunTxMode::None,
            header: self.header.clone(),
            header_hash: self.header_hash(),
            tx_hash: self.tx_hash.clone(),
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
            tx_hash: self.tx_hash.clone(),
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
