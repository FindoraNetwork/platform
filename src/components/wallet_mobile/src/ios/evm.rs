use std::os::raw::c_char;
use zei::noah_api::keys::PublicKey;

use crate::rust::{
    self, account::EVMTransactionBuilder, c_char_to_string, string_to_c_char,
};

use super::parse_u64;

use fp_types::U256;

#[no_mangle]
/// Construct a serialzed EVM Transaction that transfer account balance to UTXO.
/// @param {XfrPublicKey} recipient - UTXO Asset receiver.
/// @param {u64} amount - Transfer amount.
/// @param {string} sk - Ethereum wallet private key.
/// @param {U256} nonce - Transaction nonce for sender.
pub extern "C" fn findora_ffi_transfer_to_utxo_from_account(
    recipient: &XfrPublicKey,
    amount: *const c_char,
    sk: *const c_char,
    nonce: *const c_char,
) -> *const c_char {
    let nonce: U256 = {
        let nonce_str = c_char_to_string(nonce);
        match serde_json::from_str(&nonce_str) {
            Ok(n) => n,
            Err(e) => {
                println!("{:?}", e);
                return core::ptr::null_mut();
            }
        }
    };

    let sk = c_char_to_string(sk);

    match EVMTransactionBuilder::new_transfer_to_utxo_from_account(
        *recipient,
        parse_u64(amount),
        sk,
        nonce,
    ) {
        Ok(tx) => string_to_c_char(tx),
        Err(e) => {
            println!("{:?}", e);
            core::ptr::null_mut()
        }
    }
}

#[no_mangle]
/// Serialize ethereum address used to abci query nonce.
pub extern "C" fn get_serialized_address(address: *const c_char) -> *const c_char {
    let addr = c_char_to_string(address);
    if let Ok(data) = rust::account::get_serialized_address(&addr) {
        string_to_c_char(data)
    } else {
        core::ptr::null()
    }
}
