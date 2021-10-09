//!
//! EVM tx functions
//!

use ruc::*;

/// EVM_TX_TAG = "evm:"
pub const EVM_TX_TAG: [u8; 4] = [0x65, 0x76, 0x6d, 0x3a];

/// Evm Tx wrapper
pub struct EvmRawTxWrapper {}
impl EvmRawTxWrapper {
    /// wrap
    pub fn wrap(raw_tx: &[u8]) -> Vec<u8> {
        let mut txn_with_tag: Vec<u8> = vec![];
        txn_with_tag.extend_from_slice(&EVM_TX_TAG);
        txn_with_tag.extend_from_slice(raw_tx);

        txn_with_tag
    }

    // unwrap
    // Evm tx bytes starts with EVM_TX_TAG
    pub fn unwrap(tx: &[u8]) -> Result<&[u8]> {
        let len = EVM_TX_TAG.len();
        if tx.len() <= len || !tx[..len].eq(&EVM_TX_TAG) {
            return Err(eg!("Invalide evm transaction"));
        }

        Ok(&tx[len..])
    }
}
