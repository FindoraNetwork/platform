#![allow(clippy::unused_unit)]

use core::fmt::Display;
use ethereum::{
    LegacyTransactionMessage, TransactionSignature, TransactionV0 as LegacyTransaction,
    TransactionV2 as Transaction,
};
use ethereum_types::{H160, H256};
use serde::{Deserialize, Serialize};

use fp_types::{
    actions::{evm, template, xhub},
    crypto::{secp256k1_ecdsa_recover, Address, Signature},
    transaction,
};
use fp_utils::tx::EvmRawTxWrapper;
use ruc::{d, err::RucResult};
use sha3::{Digest, Keccak256};
use wasm_bindgen::prelude::*;

use baseapp::BaseApp;
use fp_traits::evm::FeeCalculator;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum EthAction {
    Transact(Transaction),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Ethereum(EthAction),
    Evm(evm::Action),
    XHub(xhub::Action),
    Template(template::Action),
}

pub type UncheckedTransaction<Extra> =
    transaction::UncheckedTransaction<Address, Action, Signature, Extra>;

#[inline(always)]
pub(crate) fn error_to_jsvalue<T: Display>(e: T) -> JsValue {
    JsValue::from_str(&e.to_string())
}

#[inline(always)]
pub fn recover_signer(transaction: &LegacyTransaction) -> Option<H160> {
    let mut sig = [0u8; 65];
    let mut msg = [0u8; 32];
    sig[0..32].copy_from_slice(&transaction.signature.r()[..]);
    sig[32..64].copy_from_slice(&transaction.signature.s()[..]);
    sig[64] = transaction.signature.standard_v();
    msg.copy_from_slice(&LegacyTransactionMessage::from(transaction.clone()).hash()[..]);

    let pubkey = secp256k1_ecdsa_recover(&sig, &msg).ok()?;
    Some(H160::from(H256::from_slice(
        Keccak256::digest(pubkey).as_slice(),
    )))
}

#[wasm_bindgen]
pub fn recover_tx_signer(raw_tx: String) -> Result<String, JsValue> {
    let tx_bytes = base64::decode_config(raw_tx, base64::URL_SAFE)
        .c(d!())
        .map_err(error_to_jsvalue)?;
    let raw_tx = EvmRawTxWrapper::unwrap(&tx_bytes)
        .c(d!())
        .map_err(error_to_jsvalue)?;

    let unchecked_tx: UncheckedTransaction<()> = serde_json::from_slice(raw_tx)
        .c(d!())
        .map_err(error_to_jsvalue)?;
    if let Action::Ethereum(EthAction::Transact(tx)) = unchecked_tx.function {
        let tx = match new_tx2legcay_tx(tx) {
            Some(tx) => tx,
            None => return Err(error_to_jsvalue("invalid raw tx")),
        };
        let signer = recover_signer(&tx).c(d!()).map_err(error_to_jsvalue)?;
        Ok(format!("{signer:?}",))
    } else {
        Err(error_to_jsvalue("invalid raw tx"))
    }
}

#[wasm_bindgen]
pub fn evm_tx_hash(raw_tx: String) -> Result<String, JsValue> {
    let tx_bytes = base64::decode_config(raw_tx, base64::URL_SAFE)
        .c(d!())
        .map_err(error_to_jsvalue)?;
    let raw_tx = EvmRawTxWrapper::unwrap(&tx_bytes)
        .c(d!())
        .map_err(error_to_jsvalue)?;

    let unchecked_tx: UncheckedTransaction<()> = serde_json::from_slice(raw_tx)
        .c(d!())
        .map_err(error_to_jsvalue)?;
    if let Action::Ethereum(EthAction::Transact(tx)) = unchecked_tx.function {
        let tx = match new_tx2legcay_tx(tx) {
            Some(tx) => tx,
            None => return Err(error_to_jsvalue("invalid raw tx")),
        };
        let hash = H256::from_slice(Keccak256::digest(&rlp::encode(&tx)).as_slice());
        Ok(format!("{hash:?}",))
    } else {
        Err(error_to_jsvalue("invalid raw tx"))
    }
}

fn new_tx2legcay_tx(tx: Transaction) -> Option<LegacyTransaction> {
    let transaction: LegacyTransaction = match tx {
        ethereum::TransactionV2::Legacy(tx) => tx,
        ethereum::TransactionV2::EIP1559(tx) => {
            let min_gas_price_eip1559 =
                <BaseApp as module_ethereum::Config>::FeeCalculator::min_gas_price(0);
            let chain_id: u64 = <BaseApp as module_ethereum::Config>::ChainId::get();
            let v: u64 = if tx.odd_y_parity {
                chain_id * 2 + 36
            } else {
                chain_id * 2 + 35
            };
            let signature = match TransactionSignature::new(v, tx.r, tx.s) {
                Some(sig) => sig,
                None => return None,
            };

            ethereum::TransactionV0 {
                nonce: tx.nonce,
                gas_price: min_gas_price_eip1559,
                gas_limit: tx.gas_limit,
                action: tx.action,
                value: tx.value,
                input: tx.input,
                signature,
            }
        }
        _ => return None,
    };

    Some(transaction)
}

#[cfg(test)]
#[allow(missing_docs)]
mod test {
    use super::*;

    #[test]
    fn recover_signer_works() {
        let raw_tx = String::from("eyJzaWduYXR1cmUiOm51bGwsImZ1bmN0aW9uIjp7IkV0aGVyZXVtIjp7IlRyYW5zYWN0Ijp7IkxlZ2FjeSI6eyJub25jZSI6IjB4MCIsImdhc19wcmljZSI6IjB4MjU0MGJlNDAwIiwiZ2FzX2xpbWl0IjoiMHgxMDAwMDAiLCJhY3Rpb24iOnsiQ2FsbCI6IjB4MzMyNWE3ODQyNWYxN2E3ZTQ4N2ViNTY2NmIyYmZkOTNhYmIwNmM3MCJ9LCJ2YWx1ZSI6IjB4YSIsImlucHV0IjpbXSwic2lnbmF0dXJlIjp7InYiOjQzNDAsInIiOiIweDZiMjBjNzIzNTEzOTk4ZThmYTQ4NWM1MmI4ZjlhZTRmZDdiMWUwYmQwZGZiNzk4NTIzMThiMGMxMDBlOTFmNWUiLCJzIjoiMHg0ZDRjOGMxZjJlMTdjMDJjNGE4OTZlMjYyMTI3YjhiZDZlYmZkNWY1YTc1NWEzZTkyMjBjZmM2OGI4YzY5ZDVkIn19fX19fQ==");
        let tx_bytes = base64::decode_config(raw_tx, base64::URL_SAFE).unwrap();
        let unchecked_tx: UncheckedTransaction<()> =
            serde_json::from_slice(tx_bytes.as_slice()).unwrap();
        if let Action::Ethereum(EthAction::Transact(tx)) = unchecked_tx.function {
            let tx: LegacyTransaction = new_tx2legcay_tx(tx).unwrap();
            let signer = recover_signer(&tx).unwrap();
            assert_eq!(
                format!("{signer:?}"),
                "0x5050a4f4b3f9338c3472dcc01a87c76a144b3c9c"
            );
        } else {
            panic!()
        }
    }

    #[test]
    fn evm_tx_hash_works() {
        let raw_tx = String::from("eyJzaWduYXR1cmUiOm51bGwsImZ1bmN0aW9uIjp7IkV0aGVyZXVtIjp7IlRyYW5zYWN0Ijp7IkxlZ2FjeSI6eyJub25jZSI6IjB4MCIsImdhc19wcmljZSI6IjB4MjU0MGJlNDAwIiwiZ2FzX2xpbWl0IjoiMHgxMDAwMDAiLCJhY3Rpb24iOnsiQ2FsbCI6IjB4MzMyNWE3ODQyNWYxN2E3ZTQ4N2ViNTY2NmIyYmZkOTNhYmIwNmM3MCJ9LCJ2YWx1ZSI6IjB4YSIsImlucHV0IjpbXSwic2lnbmF0dXJlIjp7InYiOjQzNDAsInIiOiIweDZiMjBjNzIzNTEzOTk4ZThmYTQ4NWM1MmI4ZjlhZTRmZDdiMWUwYmQwZGZiNzk4NTIzMThiMGMxMDBlOTFmNWUiLCJzIjoiMHg0ZDRjOGMxZjJlMTdjMDJjNGE4OTZlMjYyMTI3YjhiZDZlYmZkNWY1YTc1NWEzZTkyMjBjZmM2OGI4YzY5ZDVkIn19fX19fQ==");
        let tx_bytes = base64::decode_config(raw_tx, base64::URL_SAFE).unwrap();
        let unchecked_tx: UncheckedTransaction<()> =
            serde_json::from_slice(tx_bytes.as_slice()).unwrap();
        if let Action::Ethereum(EthAction::Transact(tx)) = unchecked_tx.function {
            let tx: LegacyTransaction = new_tx2legcay_tx(tx).unwrap();
            let hash = H256::from_slice(Keccak256::digest(&rlp::encode(&tx)).as_slice());
            assert_eq!(
                format!("{hash:?}"),
                "0x83901d025accca27ee53fdf1ee354f4437418731e0995ee031beb99499405d26"
            );
        } else {
            panic!()
        }
    }
}
