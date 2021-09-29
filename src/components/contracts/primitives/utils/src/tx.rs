//!
//! EVM tx functions
//!

use ruc::*;

/// EVM_TX_TAG = keccak256("EVM_V1")[..4]
pub const EVM_TX_TAG: [u8; 4] = [0xd7, 0x70, 0x31, 0x83];

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

        Ok(&tx[4..])
    }
}
