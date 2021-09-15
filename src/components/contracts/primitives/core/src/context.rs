use abci::Header;
use storage::{
    db::FinDB,
    state::{ChainState, State},
};

pub use parking_lot::{Mutex, RwLock};
pub use std::sync::Arc;

pub type Store = State<FinDB>;

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
    pub store: Arc<RwLock<Store>>,
    pub run_mode: RunTxMode,
    pub header: Header,
    pub header_hash: Vec<u8>,
    pub tx: Vec<u8>,
}

impl Context {
    pub fn new(cs: Arc<RwLock<ChainState<FinDB>>>) -> Self {
        Context {
            store: Arc::new(RwLock::new(Store::new(cs))),
            run_mode: RunTxMode::None,
            header: Default::default(),
            header_hash: vec![],
            tx: vec![],
        }
    }

    pub fn copy_with_new_store(
        ctx: &Context,
        cs: Arc<RwLock<ChainState<FinDB>>>,
    ) -> Self {
        Context {
            store: Arc::new(RwLock::new(Store::new(cs))),
            ..ctx.clone()
        }
    }
}

impl Context {
    pub fn commit_store(&self) -> Arc<RwLock<Store>> {
        self.store.clone()
    }

    pub fn run_mode(&self) -> RunTxMode {
        self.run_mode
    }

    pub fn block_header(&self) -> &Header {
        &self.header
    }

    pub fn header_hash(&self) -> Vec<u8> {
        self.header_hash.clone()
    }

    pub fn tx(&self) -> Vec<u8> {
        self.tx.clone()
    }
}
