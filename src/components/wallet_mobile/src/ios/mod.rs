pub mod asset_rules;
pub mod evm;
pub mod fee;
pub mod free;
pub mod tx_builder;
pub mod tx_op_builder;

use crate::rust::types;
use crate::rust::*;
use ledger::data_model::{AssetType as PlatformAssetType, AssetTypeCode};
use noah::xfr::structs::ASSET_TYPE_LENGTH;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;

#[no_mangle]
/// Returns the git commit hash and commit date of the commit this library was built against.
pub extern "C" fn findora_ffi_build_id() -> *mut c_char {
    string_to_c_char(build_id())
}

#[no_mangle]
pub extern "C" fn findora_ffi_random_asset_type() -> *mut c_char {
    string_to_c_char(random_asset_type())
}

#[no_mangle]
/// Generates asset type as a Base64 string from a JSON-serialized JavaScript value.
pub extern "C" fn findora_ffi_asset_type_from_value(code: ByteBuffer) -> *mut c_char {
    use std::convert::TryInto;
    let len = code
        .len
        .try_into()
        .expect("ByteBuffer length negative or overflowed");
    let s = unsafe { std::slice::from_raw_parts(code.data, len) };
    let mut dst = [0u8; ASSET_TYPE_LENGTH];
    dst.copy_from_slice(s);
    string_to_c_char(rs_asset_type_from_value(dst))
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
pub extern "C" fn findora_ffi_verify_authenticated_txn(
    state_commitment: *const c_char,
    authenticated_txn: *const c_char,
) -> bool {
    let state_commitment = c_char_to_string(state_commitment);
    let authenticated_txn = c_char_to_string(authenticated_txn);
    rs_verify_authenticated_txn(state_commitment, authenticated_txn).unwrap_or(false)
}

#[no_mangle]
pub extern "C" fn findora_ffi_get_null_pk() -> *mut types::XfrPublicKey {
    let pk = get_null_pk().into();
    Box::into_raw(Box::new(pk))
}

#[no_mangle]
/// Generate mnemonic with custom length and language.
/// - @param `wordslen`: acceptable value are one of [ 12, 15, 18, 21, 24 ]
/// - @param `lang`: acceptable value are one of [ "en", "zh", "zh_traditional", "fr", "it", "ko", "sp", "jp" ]
pub extern "C" fn findora_ffi_generate_mnemonic_custom(
    words_len: u8,
    lang: *const c_char,
) -> *mut c_char {
    assert!(!lang.is_null());

    if let Ok(info) =
        rs_generate_mnemonic_custom(words_len, c_char_to_string(lang).as_str())
    {
        string_to_c_char(info)
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_decryption_pbkdf2_aes256gcm(
    enc_key_pair: *mut c_char,
    password: *const c_char,
) -> *mut c_char {
    assert!(!enc_key_pair.is_null());
    assert!(!password.is_null());

    let c_str = CString::from_raw(enc_key_pair);
    let plaintext =
        decryption_pbkdf2_aes256gcm(c_str.into_bytes(), c_char_to_string(password));
    string_to_c_char(plaintext)
}

#[repr(C)]
pub struct ByteBuffer {
    len: i64,
    data: *mut u8,
}

impl From<Vec<u8>> for ByteBuffer {
    #[inline]
    fn from(bytes: Vec<u8>) -> Self {
        Self::from_vec(bytes)
    }
}

impl ByteBuffer {
    #[inline]
    pub fn from_vec(bytes: Vec<u8>) -> Self {
        use std::convert::TryFrom;
        let mut buf = bytes.into_boxed_slice();
        let data = buf.as_mut_ptr();
        let len =
            i64::try_from(buf.len()).expect("buffer length cannot fit into a i64.");
        std::mem::forget(buf);
        Self { data, len }
    }
}

#[no_mangle]
extern "C" fn findora_ffi_free_buffer(buf: ByteBuffer) {
    use std::convert::TryInto;
    let len = buf
        .len
        .try_into()
        .expect("ByteBuffer length negative or overflowed");
    let s = unsafe { std::slice::from_raw_parts_mut(buf.data, len) };
    let s = s.as_mut_ptr();
    unsafe {
        Box::from_raw(s);
    }
}

#[no_mangle]
pub extern "C" fn findora_ffi_encryption_pbkdf2_aes256gcm(
    key_pair: *const c_char,
    password: *const c_char,
) -> ByteBuffer {
    assert!(!key_pair.is_null());
    assert!(!password.is_null());

    let res = encryption_pbkdf2_aes256gcm(
        c_char_to_string(key_pair),
        c_char_to_string(password),
    );

    ByteBuffer::from_vec(res)
}

#[no_mangle]
/// Constructs a transfer key pair from a hex-encoded string.
/// The encode a key pair, use `keypair_to_str` function.
pub extern "C" fn findora_ffi_keypair_from_str(
    key_pair_str: *const c_char,
) -> *mut types::XfrKeyPair {
    assert!(!key_pair_str.is_null());
    let val = types::XfrKeyPair::from(keypair_from_str(c_char_to_string(key_pair_str)));

    let boxed_data = Box::new(val);
    Box::into_raw(boxed_data)
}

#[no_mangle]
/// # Safety
///
/// Returns bech32 encoded representation of an XfrPublicKey.
pub unsafe extern "C" fn findora_ffi_public_key_to_bech32(
    key: *const types::XfrPublicKey,
) -> *mut c_char {
    assert!(!key.is_null());

    string_to_c_char(public_key_to_bech32(&*key))
}

#[no_mangle]
/// # Safety
///
/// Extracts the public key as a string from a transfer key pair.
pub unsafe extern "C" fn findora_ffi_get_pub_key_str(
    key: *const types::XfrKeyPair,
) -> *mut c_char {
    assert!(!key.is_null());

    string_to_c_char(get_pub_key_str(&*key))
}

#[no_mangle]
/// # Safety
///
/// Extracts the private key as a string from a transfer key pair.
pub unsafe extern "C" fn findora_ffi_get_priv_key_str(
    key: *const types::XfrKeyPair,
) -> *mut c_char {
    assert!(!key.is_null());

    string_to_c_char(get_priv_key_str(&*key))
}

#[no_mangle]
/// # Safety
///
/// Restore the XfrKeyPair from a mnemonic with a default bip44-path,
/// that is "m/44'/917'/0'/0/0" ("m/44'/coin'/account'/change/address").
pub unsafe extern "C" fn findora_ffi_restore_keypair_from_mnemonic_default(
    phrase: *const c_char,
) -> *mut types::XfrKeyPair {
    // assert!(!phrase.is_null());

    if let Ok(info) =
        rs_restore_keypair_from_mnemonic_default(c_char_to_string(phrase).as_str())
    {
        let boxed_data = Box::new(types::XfrKeyPair::from(info));
        Box::into_raw(boxed_data)
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// Expresses a transfer key pair as a hex-encoded string.
/// To decode the string, use `keypair_from_str` function.
pub unsafe extern "C" fn findora_ffi_keypair_to_str(
    key_pair: *const types::XfrKeyPair,
) -> *mut c_char {
    // assert!(!key_pair.is_null());

    string_to_c_char(keypair_to_str(&*key_pair))
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_create_keypair_from_secret(
    sk_str: *const c_char,
) -> *mut types::XfrKeyPair {
    assert!(!sk_str.is_null());

    if let Some(info) = create_keypair_from_secret(c_char_to_string(sk_str)) {
        let boxed_data = Box::new(types::XfrKeyPair::from(info));
        Box::into_raw(boxed_data)
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_get_pk_from_keypair(
    key_pair: *const types::XfrKeyPair,
) -> *mut types::XfrPublicKey {
    assert!(!key_pair.is_null());

    let boxed_data =
        Box::new(types::XfrPublicKey::from(get_pk_from_keypair(&*key_pair)));
    Box::into_raw(boxed_data)
}

#[no_mangle]
/// # Safety
///
/// Creates a new transfer key pair.
pub unsafe extern "C" fn findora_ffi_new_keypair() -> *mut types::XfrKeyPair {
    let boxed_data = Box::new(types::XfrKeyPair::from(new_keypair()));
    Box::into_raw(boxed_data)
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_bech32_to_base64(pk: *const c_char) -> *mut c_char {
    if let Ok(info) = rs_bech32_to_base64(c_char_to_string(pk).as_str()) {
        string_to_c_char(info)
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_base64_to_bech32(pk: *const c_char) -> *mut c_char {
    if let Ok(info) = rs_base64_to_bech32(c_char_to_string(pk).as_str()) {
        string_to_c_char(info)
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
/// Builds an asset type from a JSON-encoded JavaScript value.
pub unsafe extern "C" fn findora_ffi_asset_type_from_json(
    asset_type_json: *const c_char,
) -> *mut AssetType {
    let asset_type_json = CStr::from_ptr(asset_type_json);
    let asset_type: PlatformAssetType =
        serde_json::from_str(asset_type_json.to_str().unwrap_or("")).unwrap();

    if let Ok(info) = AssetType::from_json(asset_type) {
        Box::into_raw(Box::new(info))
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// Fetch the tracing policies associated with this asset type.
pub unsafe extern "C" fn findora_ffi_asset_type_get_tracing_policies(
    asset_type: *const AssetType,
) -> *mut TracingPolicies {
    assert!(!asset_type.is_null());
    Box::into_raw(Box::new((*asset_type).get_tracing_policies()))
}

#[no_mangle]
/// # Safety
///
/// Converts a base64 encoded public key string to a public key.
pub unsafe extern "C" fn findora_ffi_public_key_from_base64(
    pk: *const c_char,
) -> *mut types::XfrPublicKey {
    let pk = CStr::from_ptr(pk);
    if let Ok(info) = rs_public_key_from_base64(pk.to_str().unwrap_or("")) {
        Box::into_raw(Box::new(types::XfrPublicKey::from(info)))
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
/// Creates a relative txo reference as a JSON string. Relative txo references are offset
/// backwards from the operation they appear in -- 0 is the most recent, (n-1) is the first output
/// of the transaction.
///
/// Use relative txo indexing when referring to outputs of intermediate operations (e.g. a
/// transaction containing both an issuance and a transfer).
///
/// # Arguments
/// @param {BigInt} idx -  Relative TXO (transaction output) SID.
pub extern "C" fn findora_ffi_txo_ref_relative(idx: u64) -> *mut TxoRef {
    Box::into_raw(Box::new(TxoRef::relative(idx)))
}

#[no_mangle]
/// Creates an absolute transaction reference as a JSON string.
///
/// Use absolute txo indexing when referring to an output that has been assigned a utxo index (i.e.
/// when the utxo has been committed to the ledger in an earlier transaction).
///
/// # Arguments
/// @param {BigInt} idx -  Txo (transaction output) SID.
pub extern "C" fn findora_ffi_txo_ref_absolute(idx: u64) -> *mut TxoRef {
    Box::into_raw(Box::new(TxoRef::absolute(idx)))
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
pub unsafe extern "C" fn findora_ffi_open_client_asset_record(
    record: *const ClientAssetRecord,
    owner_memo: *const OwnerMemo,
    keypair: *const types::XfrKeyPair,
) -> *mut types::OpenAssetRecord {
    let memo = if owner_memo.is_null() {
        None
    } else {
        Some((*owner_memo).clone())
    };
    if let Ok(info) = rs_open_client_asset_record(&*record, memo, &**keypair) {
        Box::into_raw(Box::new(info.into()))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// pub enum AssetRecordType {
///     NonConfidentialAmount_ConfidentialAssetType = 0,
///     ConfidentialAmount_NonConfidentialAssetType = 1,
///     ConfidentialAmount_ConfidentialAssetType = 2,
///     NonConfidentialAmount_NonConfidentialAssetType = 3,
/// }
pub unsafe extern "C" fn findora_ffi_open_client_asset_record_get_record_type(
    record: *const types::OpenAssetRecord,
) -> i32 {
    (&*record).get_record_type() as i32
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_open_client_asset_record_get_asset_type(
    record: *const types::OpenAssetRecord,
) -> *mut c_char {
    let asset_type = AssetTypeCode {
        val: *(&*record).get_asset_type(),
    }
    .to_base64();
    string_to_c_char(asset_type)
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_open_client_asset_record_get_amount(
    record: *const types::OpenAssetRecord,
) -> u64 {
    *(&*record).get_amount()
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_open_client_asset_record_get_pub_key(
    record: *const types::OpenAssetRecord,
) -> *mut types::XfrPublicKey {
    Box::into_raw(Box::new(types::XfrPublicKey::from(
        *(&*record).get_pub_key(),
    )))
}

#[no_mangle]
/// # Safety
///
/// Builds a client record from a JSON-encoded JavaScript value.
///
/// @param {JsValue} val - JSON-encoded autehtnicated asset record fetched from ledger server with the `utxo_sid/{sid}` route,
/// where `sid` can be fetched from the query server with the `get_owned_utxos/{address}` route.
/// Note: The first field of an asset record is `utxo`. See the example below.
///
/// @example
/// "utxo":{
///   "amount":{
///     "NonConfidential":5
///   },
///  "asset_type":{
///     "NonConfidential":[113,168,158,149,55,64,18,189,88,156,133,204,156,46,106,46,232,62,69,233,157,112,240,132,164,120,4,110,14,247,109,127]
///   },
///   "public_key":"Glf8dKF6jAPYHzR_PYYYfzaWqpYcMvnrIcazxsilmlA="
/// }
///
/// @see {@link module:Findora-Network~Network#getUtxo|Network.getUtxo} for information about how to
/// fetch an asset record from the ledger server.
pub unsafe extern "C" fn findora_ffi_client_asset_record_from_json(
    val: *const c_char,
) -> *mut ClientAssetRecord {
    if let Ok(info) = ClientAssetRecord::from_json(c_char_to_string(val).as_str()) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// Builds an owner memo from a JSON-serialized JavaScript value.
/// @param {JsValue} val - JSON owner memo fetched from query server with the `get_owner_memo/{sid}` route,
/// where `sid` can be fetched from the query server with the `get_owned_utxos/{address}` route. See the example below.
///
/// @example
/// {
///   "blind_share":[91,251,44,28,7,221,67,155,175,213,25,183,70,90,119,232,212,238,226,142,159,200,54,19,60,115,38,221,248,202,74,248],
///   "lock":{"ciphertext":[119,54,117,136,125,133,112,193],"encoded_rand":"8KDql2JphPB5WLd7-aYE1bxTQAcweFSmrqymLvPDntM="}
/// }
pub unsafe extern "C" fn findora_ffi_owner_memo_from_json(
    val: *const c_char,
) -> *mut OwnerMemo {
    if let Ok(info) = serde_json::from_str(c_char_to_string(val).as_str()) {
        Box::into_raw(Box::new(OwnerMemo::from_json(info).unwrap()))
    } else {
        std::ptr::null_mut()
    }
}

/// Generates a new credential issuer key.
/// @param {JsValue} attributes - Array of attribute types of the form `[{name: "credit_score",
/// size: 3}]`. The size refers to byte-size of the credential. In this case, the "credit_score"
/// attribute is represented as a 3 byte string "760". `attributes` is the list of attribute types
/// that the issuer can sign off on.
#[no_mangle]
pub extern "C" fn findora_ffi_credential_issuer_key_gen(
    attributes: *const c_char,
) -> *mut CredentialIssuerKeyPair {
    let attributes: Vec<AttributeDefinition> =
        serde_json::from_str(c_char_to_string(attributes).as_str()).unwrap();
    Box::into_raw(Box::new(rs_wasm_credential_issuer_key_gen(attributes)))
}

/// Returns the credential issuer's public key.
#[no_mangle]
pub extern "C" fn findora_ffi_credential_issuer_key_pair_get_pk(
    pair: &CredentialIssuerKeyPair,
) -> *mut types::CredIssuerPublicKey {
    Box::into_raw(Box::new(types::CredIssuerPublicKey::from(pair.get_pk())))
}

/// Returns the credential issuer's secret key.
#[no_mangle]
pub extern "C" fn findora_ffi_credential_issuer_key_pair_get_sk(
    pair: &CredentialIssuerKeyPair,
) -> *mut types::CredIssuerSecretKey {
    Box::into_raw(Box::new(types::CredIssuerSecretKey::from(pair.get_sk())))
}

/// Generates a new credential user key.
/// @param {CredIssuerPublicKey} issuer_pub_key - The credential issuer that can sign off on this
/// user's attributes.
#[no_mangle]
pub extern "C" fn findora_ffi_credential_user_key_gen(
    issuer_pub_key: &types::CredIssuerPublicKey,
) -> *mut CredentialUserKeyPair {
    let mut prng = ChaChaRng::from_entropy();
    let (pk, sk) = credentials::credential_user_key_gen(&mut prng, issuer_pub_key);
    Box::into_raw(Box::new(CredentialUserKeyPair { pk, sk }))
}

/// Returns the credential issuer's public key.
#[no_mangle]
pub extern "C" fn findora_ffi_cred_issuer_public_key_get_pk(
    pair: &CredentialUserKeyPair,
) -> *mut types::CredUserPublicKey {
    Box::into_raw(Box::new(types::CredUserPublicKey::from(pair.get_pk())))
}

/// Returns the credential issuer's secret key.
#[no_mangle]
pub extern "C" fn findora_ffi_cred_issuer_public_key_get_sk(
    pair: &CredentialUserKeyPair,
) -> *mut types::CredUserSecretKey {
    Box::into_raw(Box::new(types::CredUserSecretKey::from(pair.get_sk())))
}

pub(super) fn parse_u64(n: *const c_char) -> u64 {
    c_char_to_string(n).parse().expect("Invalid u64.")
}
