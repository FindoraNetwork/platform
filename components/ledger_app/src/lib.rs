extern crate core;

use core::store::*;
use core::data_model::{Transaction};

#[derive(Default)]
pub struct LedgerApp {
    pub state: LedgerState,
}

// Convert incoming tx data to the proper Transaction format
pub fn convert_tx(tx: &[u8]) -> Transaction {
    let transaction: Transaction = serde_json::from_slice(tx).unwrap();

    transaction
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
