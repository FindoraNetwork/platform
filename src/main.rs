extern crate abci;
extern crate serde;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate arrayref;
extern crate serde_json;

mod data_model;
mod store;
use crate::store::*;
use crate::data_model::{Transaction};
use abci::*;

struct LedgerApp {
    state: LedgerState,
}

impl LedgerApp {
    pub fn new() -> LedgerApp {
        LedgerApp {state: LedgerState::new()}
    }
}

// Convert incoming tx data to the proper BigEndian size. txs.len() > 8 will return 0
fn convert_tx(tx: &[u8]) -> Transaction {
    let transaction: Transaction = serde_json::from_slice(tx).unwrap();

    transaction
}

// TODO: implement abci hooks
impl abci::Application for LedgerApp {

	fn check_tx(&mut self, req: &RequestCheckTx) -> ResponseCheckTx {
        // Get the Tx [u8] and convert to u64
        let tx = convert_tx(req.get_tx());
        let mut resp = ResponseCheckTx::new();

        if !self.state.validate_transaction(&tx)
        {
        	resp.set_code(1);
            resp.set_log(String::from("Validation failed"));
            return resp;
        }

        resp
    }

    fn deliver_tx(&mut self, req: &RequestDeliverTx) -> ResponseDeliverTx {
        // Get the Tx [u8]
        let tx = convert_tx(req.get_tx());
        // Update state
        self.state.apply_transaction(tx);
        // Return default code 0 == bueno
        ResponseDeliverTx::new()
    }

}

fn main() {
    // Tendermint ABCI port
    let addr = "127.0.0.1:26658".parse().unwrap();

    abci::run(addr, LedgerApp::new());
}