extern crate abci;

mod data_model;
mod store;
use crate::store::{LedgerState};

struct LedgerApp {
    state: LedgerState,
}

impl LedgerApp {
    pub fn new() -> LedgerApp {
        LedgerApp {state: LedgerState::new()}
    }
}

// TODO: implement abci hooks
impl abci::Application for LedgerApp {}

fn main() {
    // Tendermint ABCI port
    let addr = "127.0.0.1:26658".parse().unwrap();

    abci::run(addr, LedgerApp::new());
}
