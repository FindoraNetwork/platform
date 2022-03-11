use crate::rust::account::EVMTransactionBuilder;
use jni::objects::{JClass, JString};
use jni::sys::{jboolean, jint, jlong, jstring, JNI_TRUE};
use jni::JNIEnv;
use zei::xfr::sig::XfrKeyPair;

#[no_mangle]
/// Construct a EVM Transaction that transfer account balance to UTXO.
/// @param {unsigned long long} amount - Amount to transfer.
/// @param {XfrKeyPair} fra_kp - Fra key pair.
/// @param {String} address - EVM address.
/// @param {String} eth_phrase - The account mnemonic.
/// @param {String} nonce - Json encoded U256(256 bits unsigned integer).
pub unsafe extern "system" fn Java_com_findora_JniApi_transfer_from_account_evmTransactionBuilder(
    env: JNIEnv,
    _: JClass,
    amount: jlong,
    address: JString,
    fra_kp: jlong,
    eth_phrase: JString,
    nonce: JString,
) -> jlong {
    let address = {
        let a: String = env
            .get_string(address)
            .expect("Couldn't create java string!")
            .into();
        if a.len() == 0 {
            None
        } else {
            Some(a)
        }
    };

    let eth_phrase = {
        let a: String = env
            .get_string(eth_phrase)
            .expect("Couldn't create java string!")
            .into();
        if a.len() == 0 {
            None
        } else {
            Some(a)
        }
    };

    let nonce = {
        let a: String = env
            .get_string(nonce)
            .expect("Couldn't create java string!")
            .into();
        serde_json::from_str(&a).unwrap()
    };

    let fra_kp = &*(fra_kp as *mut XfrKeyPair);

    let tx = EVMTransactionBuilder::new_transfer_from_account(
        amount as u64,
        address,
        fra_kp,
        eth_phrase,
        nonce,
    )
    .unwrap();
    Box::into_raw(Box::new(tx)) as jlong
}
