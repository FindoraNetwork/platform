extern crate abci;
extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate arrayref;
extern crate core;
extern crate ledger_app;
extern crate serde_json;

use abci::*;
use core::data_model::Transaction;
use core::store::*;
use ledger_app::{convert_tx, LedgerApp};

struct ABCILedgerApp(LedgerApp);

// TODO: implement abci hooks
impl abci::Application for ABCILedgerApp {
    fn check_tx(&mut self, req: &RequestCheckTx) -> ResponseCheckTx {
        // Get the Tx [u8] and convert to u64
        let tx = convert_tx(req.get_tx());
        let mut resp = ResponseCheckTx::new();

        if !self.state.validate_transaction(&tx) {
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
        self.state.append_transaction(tx);
        // Return default code 0 == bueno
        ResponseDeliverTx::new()
    }
}

fn main() {
    // Tendermint ABCI port
    let addr = "127.0.0.1:26658".parse().unwrap();

    abci::run(addr, LedgerApp::default());
}
