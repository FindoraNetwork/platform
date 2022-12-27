#![allow(clippy::unused_unit)]

use core::fmt::Display;
use ethereum::TransactionV2 as Transaction;
use ethereum_types::{H160, H256};
use fp_types::{
    actions::{ethereum::Action as EthAction, Action},
    assemble::UncheckedTransaction,
};
use fp_utils::tx::EvmRawTxWrapper;
use ruc::{d, err::RucResult};
use sha3::{Digest, Keccak256};
use wasm_bindgen::prelude::*;

#[inline(always)]
pub(crate) fn error_to_jsvalue<T: Display>(e: T) -> JsValue {
    JsValue::from_str(&e.to_string())
}

#[inline(always)]
pub fn recover_signer(transaction: &Transaction) -> Option<H160> {
    let mut sig = [0u8; 65];
    let mut msg = [0u8; 32];
    match transaction {
        Transaction::Legacy(t) => {
            sig[0..32].copy_from_slice(&t.signature.r()[..]);
            sig[32..64].copy_from_slice(&t.signature.s()[..]);
            sig[64] = t.signature.standard_v();
            msg.copy_from_slice(
                &ethereum::LegacyTransactionMessage::from(t.clone()).hash()[..],
            );
        }
        Transaction::EIP1559(t) => {
            sig[0..32].copy_from_slice(&t.r[..]);
            sig[32..64].copy_from_slice(&t.s[..]);
            sig[64] = t.odd_y_parity as u8;
            msg.copy_from_slice(
                &ethereum::EIP1559TransactionMessage::from(t.clone()).hash()[..],
            );
        }
        _ => {
            return None;
        }
    }
    let pubkey = fp_types::crypto::secp256k1_ecdsa_recover(&sig, &msg).ok()?;
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
        let signer = recover_signer(&tx).c(d!()).map_err(error_to_jsvalue)?;
        Ok(format!("{:?}", signer))
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
        let hash = H256::from_slice(Keccak256::digest(&rlp::encode(&tx)).as_slice());
        Ok(format!("{:?}", hash))
    } else {
        Err(error_to_jsvalue("invalid raw tx"))
    }
}

#[cfg(test)]
#[allow(missing_docs)]
mod test {
    use super::*;
    use fp_types::actions::{ethereum::Action as EthAction, Action};

    #[test]
    fn recover_signer_works() {
        let raw_tx = String::from("ZXZtOnsiRXRoZXJldW0iOnsiVHJhbnNhY3QiOnsiRUlQMTU1OSI6eyJjaGFpbl9pZCI6MjE1Miwibm9uY2UiOiIweDEiLCJtYXhfcHJpb3JpdHlfZmVlX3Blcl9nYXMiOiIweDI3MTAiLCJtYXhfZmVlX3Blcl9nYXMiOiIweDI3MTAiLCJnYXNfbGltaXQiOiIweDI3MTAiLCJhY3Rpb24iOnsiQ2FsbCI6IjB4MmFkMzI4NDZjNmRkMmZmZDNlZGFkYmU1MWNkNWFlMDRhYTVlNTc1ZSJ9LCJ2YWx1ZSI6IjB4MjcxMCIsImlucHV0IjpbXSwiYWNjZXNzX2xpc3QiOltdLCJvZGRfeV9wYXJpdHkiOmZhbHNlLCJyIjoiMHhmOGFlZjdmODA1M2Q4OWZlZTM5NTBjNGQ3MDIwODBiZjNhODA3MmJlZDVkODRhM2FmMTlhMzYwMDgxYjYzNmEyIiwicyI6IjB4Mjk2Mjk5YThmMjQzMGI4NmZkM2ViOTc2ZWFiYzczMGFjMWNmYmJiZTM2ZWI2OWVhZTMzOGNmZjMzYzRhOThjMSJ9fX19");
        let tx_bytes = base64::decode_config(raw_tx, base64::URL_SAFE).unwrap();
        let evm_tx = EvmRawTxWrapper::unwrap(&tx_bytes).unwrap();
        let action: Action = serde_json::from_slice(evm_tx).unwrap();
        if let Action::Ethereum(EthAction::Transact(tx)) = action {
            let signer = recover_signer(&tx).unwrap();
            assert_eq!(
                format!("{:?}", signer),
                "0x7bc371bb41545d62117591e5051be3d2c6296f3e"
            );
        } else {
            panic!()
        }
    }

    #[test]
    // LegacyTransaction
    fn evm_tx_hash_works() {
        let raw_tx = String::from("eyJzaWduYXR1cmUiOm51bGwsImZ1bmN0aW9uIjp7IkV0aGVyZXVtIjp7IlRyYW5zYWN0Ijp7IkxlZ2FjeSI6eyJub25jZSI6IjB4MCIsImdhc19wcmljZSI6IjB4MjU0MGJlNDAwIiwiZ2FzX2xpbWl0IjoiMHgxMDAwMDAiLCJhY3Rpb24iOnsiQ2FsbCI6IjB4MzMyNWE3ODQyNWYxN2E3ZTQ4N2ViNTY2NmIyYmZkOTNhYmIwNmM3MCJ9LCJ2YWx1ZSI6IjB4YSIsImlucHV0IjpbXSwic2lnbmF0dXJlIjp7InYiOjQzNDAsInIiOiIweDZiMjBjNzIzNTEzOTk4ZThmYTQ4NWM1MmI4ZjlhZTRmZDdiMWUwYmQwZGZiNzk4NTIzMThiMGMxMDBlOTFmNWUiLCJzIjoiMHg0ZDRjOGMxZjJlMTdjMDJjNGE4OTZlMjYyMTI3YjhiZDZlYmZkNWY1YTc1NWEzZTkyMjBjZmM2OGI4YzY5ZDVkIn19fX19fQ==");
        let tx_bytes = base64::decode_config(raw_tx, base64::URL_SAFE).unwrap();
        let unchecked_tx: UncheckedTransaction<()> =
            serde_json::from_slice(tx_bytes.as_slice()).unwrap();
        if let Action::Ethereum(EthAction::Transact(tx)) = unchecked_tx.function {
            let hash = H256::from_slice(Keccak256::digest(&rlp::encode(&tx)).as_slice());
            assert_eq!(
                format!("{:?}", hash),
                "0x83901d025accca27ee53fdf1ee354f4437418731e0995ee031beb99499405d26"
            );
        } else {
            panic!()
        }
    }
}
