use crate::rust::account::{get_serialized_address, EVMTransactionBuilder};
use jni::objects::{JClass, JString};
use jni::sys::{jlong, jstring};
use jni::JNIEnv;
use zei::XfrPublicKey;

use super::{jStringToString, parseU64};

#[no_mangle]
/// # Safety
/// Construct a serialzied EVM Transaction that transfer account balance to UTXO.
/// @param {XfrPublicKey} recipient - UTXO Asset receiver.
/// @param {u64} amount - Transfer amount.
/// @param {string} sk - Ethereum wallet private key.
/// @param {U256} nonce - Transaction nonce for sender.
pub unsafe extern "system" fn Java_com_findora_JniApi_transferToUtxoFromAccount(
    env: JNIEnv,
    _: JClass,
    recipient: jlong,
    amount: JString,
    sk: JString,
    nonce: JString,
) -> jstring {
    let nonce = serde_json::from_str(&jStringToString(env, nonce)).unwrap();

    let amount = parseU64(env, amount);

    let sk = jStringToString(env, sk);

    let recipient = *(recipient as *mut XfrPublicKey);

    let ser_tx = EVMTransactionBuilder::new_transfer_to_utxo_from_account(
        recipient, amount, sk, nonce,
    )
    .unwrap();

    **env
        .new_string(ser_tx)
        .expect("Couldn't create java String!")
}

#[no_mangle]
/// Serialize ethereum address used to abci query nonce.
pub extern "system" fn Java_com_findora_JniApi_getSerializedAddress(
    env: JNIEnv,
    _: JClass,
    address: JString,
) -> jstring {
    let addr = jStringToString(env, address);
    let data = get_serialized_address(&addr).unwrap();
    **env.new_string(data).expect("Couldn't create java String!")
}
