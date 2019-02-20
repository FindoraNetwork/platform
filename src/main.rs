extern crate abci;

// TODO: add state
struct LedgerApp;

// TODO: implement abci hooks
impl abci::Application for LedgerApp {}

fn main() {
    // Tendermint ABCI port
    let addr = "127.0.0.1:26658".parse().unwrap();

    abci::run(addr, LedgerApp);
}
