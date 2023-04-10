mod constructor;
mod evm;
mod transfer;
mod tx_builder;

use crate::rust::types;
use crate::rust::*;
use jni::objects::{JClass, JString};
use jni::sys::{jboolean, jbyteArray, jint, jlong, jstring};
use jni::JNIEnv;
use ledger::data_model::AssetTypeCode;
use zei::xfr::structs::ASSET_TYPE_LENGTH;

#[no_mangle]
/// Returns the git commit hash and commit date of the commit this library was built against.
pub extern "system" fn Java_com_findora_JniApi_buildId(
    env: JNIEnv,
    // this is the class that owns our
    // static method. Not going to be
    // used, but still needs to have
    // an argument slot
    _: JClass,
) -> jstring {
    let output = env
        .new_string(build_id())
        .expect("Couldn't create java string!");
    // extract the raw pointer to return.
    **output
}

#[no_mangle]
/// Generates asset type as a Base64 string from a JSON-serialized JavaScript value.
pub extern "system" fn Java_com_findora_JniApi_assetTypeFromValue(
    env: JNIEnv,
    _: JClass,
    input: jbyteArray,
) -> jstring {
    let input = env.convert_byte_array(input).unwrap();
    let mut buf = [0u8; ASSET_TYPE_LENGTH];
    buf.copy_from_slice(input.as_ref());

    let asset_type = rs_asset_type_from_value(buf);
    let output = env
        .new_string(asset_type)
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// Given a serialized state commitment and transaction, returns true if the transaction correctly
/// hashes up to the state commitment and false otherwise.
/// @param {string} state_commitment - String representing the state commitment.
/// @param {string} authenticated_txn - String representing the transaction.
/// @see {@link module:Network~Network#getTxn|Network.getTxn} for instructions on fetching a transaction from the ledger.
/// @see {@link module:Network~Network#getStateCommitment|Network.getStateCommitment}
/// for instructions on fetching a ledger state commitment.
/// @throws Will throw an error if the state commitment or the transaction fails to deserialize.
pub extern "system" fn Java_com_findora_JniApi_verifyAuthenticatedTxn(
    env: JNIEnv,
    _: JClass,
    state_commitment: JString,
    authenticated_txn: JString,
) -> jboolean {
    let state_commitment: String = env
        .get_string(state_commitment)
        .expect("Couldn't get java string!")
        .into();

    let authenticated_txn: String = env
        .get_string(authenticated_txn)
        .expect("Couldn't get java string!")
        .into();

    rs_verify_authenticated_txn(state_commitment, authenticated_txn).unwrap_or(false)
        as jboolean
}

#[no_mangle]
/// Generate mnemonic with custom length and language.
/// - @param `wordslen`: acceptable value are one of [ 12, 15, 18, 21, 24 ]
/// - @param `lang`: acceptable value are one of [ "en", "zh", "zh_traditional", "fr", "it", "ko", "sp", "jp" ]
pub extern "system" fn Java_com_findora_JniApi_generateMnemonicCustom(
    env: JNIEnv,
    _: JClass,
    words_len: jint,
    lang: JString,
) -> jstring {
    let lang: String = env
        .get_string(lang)
        .expect("Couldn't get java string!")
        .into();
    let mnemonic = rs_generate_mnemonic_custom(words_len as u8, lang.as_str()).unwrap();
    let output = env
        .new_string(mnemonic)
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
pub extern "system" fn Java_com_findora_JniApi_decryptionPbkdf2Aes256gcm(
    env: JNIEnv,
    _: JClass,
    enc_key_pair: jbyteArray,
    password: JString,
) -> jstring {
    let enc_key_pair = env.convert_byte_array(enc_key_pair).unwrap();
    let password: String = env
        .get_string(password)
        .expect("Couldn't get java string!")
        .into();
    let plaintext = decryption_pbkdf2_aes256gcm(enc_key_pair, password);
    let output = env
        .new_string(plaintext)
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
pub extern "system" fn Java_com_findora_JniApi_encryptionPbkdf2Aes256gcm(
    env: JNIEnv,
    _: JClass,
    key_pair: JString,
    password: JString,
) -> jbyteArray {
    let key_pair: String = env
        .get_string(key_pair)
        .expect("Couldn't get java string!")
        .into();
    let password: String = env
        .get_string(password)
        .expect("Couldn't get java string!")
        .into();

    let res = encryption_pbkdf2_aes256gcm(key_pair, password);
    env.byte_array_from_slice(res.as_slice()).unwrap()
}

#[no_mangle]
/// Constructs a transfer key pair from a hex-encoded string.
/// The encode a key pair, use `keypair_to_str` function.
pub extern "system" fn Java_com_findora_JniApi_keypairFromStr(
    env: JNIEnv,
    _: JClass,
    text: JString,
) -> jlong {
    let text: String = env
        .get_string(text)
        .expect("Couldn't get java string!")
        .into();
    let val = types::XfrKeyPair::from(keypair_from_str(text));
    Box::into_raw(Box::new(val)) as jlong
}

#[no_mangle]
/// # Safety
///
/// Returns bech32 encoded representation of an XfrPublicKey.
pub unsafe extern "system" fn Java_com_findora_JniApi_publicKeyToBech32(
    env: JNIEnv,
    _: JClass,
    xfr_public_key_ptr: jlong,
) -> jstring {
    let key = &*(xfr_public_key_ptr as *mut types::XfrPublicKey);
    let res = public_key_to_bech32(key);
    let output = env.new_string(res).expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
///
/// Extracts the public key as a string from a transfer key pair.
pub unsafe extern "system" fn Java_com_findora_JniApi_getPubKeyStr(
    env: JNIEnv,
    _: JClass,
    xfr_keypair_ptr: jlong,
) -> jstring {
    let key = &*(xfr_keypair_ptr as *mut types::XfrKeyPair);
    let pubkey = get_pub_key_str(key);
    let output = env
        .new_string(pubkey)
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
///
/// Extracts the private key as a string from a transfer key pair.
pub unsafe extern "system" fn Java_com_findora_JniApi_getPrivKeyStr(
    env: JNIEnv,
    _: JClass,
    xfr_keypair_ptr: jlong,
) -> jstring {
    let key = &*(xfr_keypair_ptr as *mut types::XfrKeyPair);
    let prikey = get_priv_key_str(key);
    let output = env
        .new_string(prikey)
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
///
/// Restore the XfrKeyPair from a mnemonic with a default bip44-path,
/// that is "m/44'/917'/0'/0/0" ("m/44'/coin'/account'/change/address").
pub extern "system" fn Java_com_findora_JniApi_restoreKeypairFromMnemonicDefault(
    env: JNIEnv,
    _: JClass,
    phrase: JString,
) -> jlong {
    let phrase: String = env
        .get_string(phrase)
        .expect("Couldn't get java string!")
        .into();
    if let Ok(keypair) = rs_restore_keypair_from_mnemonic_default(phrase.as_str()) {
        Box::into_raw(Box::new(types::XfrKeyPair::from(keypair))) as jlong
    } else {
        ::std::ptr::null_mut::<()>() as jlong
    }
}

#[no_mangle]
/// # Safety
///
/// Expresses a transfer key pair as a hex-encoded string.
/// To decode the string, use `keypair_from_str` function.
pub unsafe extern "system" fn Java_com_findora_JniApi_keypairToStr(
    env: JNIEnv,
    _: JClass,
    xfr_keypair_ptr: jlong,
) -> jstring {
    let key = &*(xfr_keypair_ptr as *mut types::XfrKeyPair);
    let res = keypair_to_str(key);
    let output = env.new_string(res).expect("Couldn't create java string!");
    **output
}

#[no_mangle]
pub extern "system" fn Java_com_findora_JniApi_createKeypairFromSecret(
    env: JNIEnv,
    _: JClass,
    sk_str: JString,
) -> jlong {
    let sk: String = env
        .get_string(sk_str)
        .expect("Couldn't get java string!")
        .into();
    if let Some(keypair) = create_keypair_from_secret(sk) {
        Box::into_raw(Box::new(types::XfrKeyPair::from(keypair))) as jlong
    } else {
        ::std::ptr::null_mut::<()>() as jlong
    }
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_getPkFromKeypair(
    _env: JNIEnv,
    _: JClass,
    xfr_keypair_ptr: jlong,
) -> jlong {
    let kp = &*(xfr_keypair_ptr as *mut types::XfrKeyPair);
    let pk = get_pk_from_keypair(kp);
    Box::into_raw(Box::new(types::XfrPublicKey::from(pk))) as jlong
}

#[no_mangle]
/// Creates a new transfer key pair.
pub extern "system" fn Java_com_findora_JniApi_newKeypair(
    _env: JNIEnv,
    _: JClass,
) -> jlong {
    let keypair = new_keypair();
    Box::into_raw(Box::new(types::XfrKeyPair::from(keypair))) as jlong
}

#[no_mangle]
pub extern "system" fn Java_com_findora_JniApi_bech32ToBase64(
    env: JNIEnv,
    _: JClass,
    pk: JString,
) -> jstring {
    let pk: String = env
        .get_string(pk)
        .expect("Couldn't get java string!")
        .into();

    let bs = rs_bech32_to_base64(pk.as_str()).unwrap();
    let output = env.new_string(bs).expect("Couldn't create java string!");
    **output
}

#[no_mangle]
pub extern "system" fn Java_com_findora_JniApi_base64ToBech32(
    env: JNIEnv,
    _: JClass,
    pk: JString,
) -> jstring {
    let pk: String = env
        .get_string(pk)
        .expect("Couldn't get java string!")
        .into();

    let bs = rs_base64_to_bech32(pk.as_str()).unwrap();
    let output = env.new_string(bs).expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
///
/// Returns a object containing decrypted owner record information,
/// where `amount` is the decrypted asset amount, and `asset_type` is the decrypted asset type code.
///
/// @param {ClientAssetRecord} record - Owner record.
/// @param {OwnerMemo} owner_memo - Owner memo of the associated record.
/// @param {XfrKeyPair} keypair - Keypair of asset owner.
/// @see {@link module:Findora-Wasm~ClientAssetRecord#from_json_record|ClientAssetRecord.from_json_record} for information about how to construct an asset record object
/// from a JSON result returned from the ledger server.
pub unsafe extern "system" fn Java_com_findora_JniApi_openClientAssetRecord(
    _env: JNIEnv,
    _: JClass,
    record_ptr: jlong,
    owner_memo_ptr: jlong,
    keypair_ptr: jlong,
) -> jlong {
    let record = &*(record_ptr as *mut ClientAssetRecord);
    let owner_memo = if 0 == owner_memo_ptr {
        None
    } else {
        let memo = &*(owner_memo_ptr as *mut OwnerMemo);
        Some(memo.clone())
    };
    let keypair = &*(keypair_ptr as *mut types::XfrKeyPair);
    let oar = rs_open_client_asset_record(record, owner_memo, keypair).unwrap();
    Box::into_raw(Box::new(types::OpenAssetRecord::from(oar))) as jlong
}

#[no_mangle]
/// pub enum AssetRecordType {
///     NonConfidentialAmount_ConfidentialAssetType = 0,
///     ConfidentialAmount_NonConfidentialAssetType = 1,
///     ConfidentialAmount_ConfidentialAssetType = 2,
///     NonConfidentialAmount_NonConfidentialAssetType = 3,
/// }
pub extern "system" fn Java_com_findora_JniApi_openClientAssetRecordGetRecordType(
    _env: JNIEnv,
    _: JClass,
    record_ptr: jlong,
) -> jint {
    let record = unsafe { &*(record_ptr as *mut types::OpenAssetRecord) };
    record.get_record_type() as jint
}

#[no_mangle]
pub extern "system" fn Java_com_findora_JniApi_openClientAssetRecordGetAssetType(
    env: JNIEnv,
    _: JClass,
    record_ptr: jlong,
) -> jstring {
    let record = unsafe { &*(record_ptr as *mut types::OpenAssetRecord) };
    let asset_type = AssetTypeCode {
        val: *record.get_asset_type(),
    }
    .to_base64();
    let output = env
        .new_string(asset_type)
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
pub extern "system" fn Java_com_findora_JniApi_openClientAssetRecordGetAmount(
    env: JNIEnv,
    _: JClass,
    record_ptr: jlong,
) -> jstring {
    let record = unsafe { &*(record_ptr as *mut types::OpenAssetRecord) };
    let output = env
        .new_string(record.get_amount().to_string())
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
pub extern "system" fn Java_com_findora_JniApi_openClientAssetRecordGetPubKey(
    _env: JNIEnv,
    _: JClass,
    record_ptr: jlong,
) -> jlong {
    let record = unsafe { &*(record_ptr as *mut types::OpenAssetRecord) };
    Box::into_raw(Box::new(types::XfrPublicKey::from(*record.get_pub_key()))) as jlong
}

pub(super) fn jStringToString(env: JNIEnv, s: JString) -> String {
    env.get_string(s)
        .expect("Couldn't create rust String!")
        .into()
}

pub(super) fn parseU64(env: JNIEnv, amount: JString) -> u64 {
    jStringToString(env, amount)
        .parse()
        .expect("Parse u64 error.")
}
