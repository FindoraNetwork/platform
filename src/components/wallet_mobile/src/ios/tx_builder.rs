use super::parse_u64;
use crate::rust::{
    c_char_to_string, string_to_c_char, AssetRules, ClientAssetRecord, FeeInputs,
    OwnerMemo, TransactionBuilder,
};

use ledger::data_model::AssetTypeCode;
use std::os::raw::c_char;
use zei::noah_api::keys::KeyPair;

#[no_mangle]
/// @param kp: owner's XfrKeyPair
pub extern "C" fn findora_ffi_transaction_builder_add_fee_relative_auto(
    builder: &TransactionBuilder,
    kp: &XfrKeyPair,
) -> *mut TransactionBuilder {
    if let Ok(info) = builder.clone().add_fee_relative_auto(kp.clone()) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

/// Use this func to get the necessary infomations for generating `Relative Inputs`
///
/// - TxoRef::Relative("Element index of the result")
/// - ClientAssetRecord::from_json("Element of the result")
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_get_relative_outputs(
    builder: &TransactionBuilder,
) -> safer_ffi::vec::Vec<ClientAssetRecord> {
    builder.clone().get_relative_outputs().into()
}

/// As the last operation of any transaction,
/// add a static fee to the transaction.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_fee(
    builder: &TransactionBuilder,
    inputs: &FeeInputs,
) -> *mut TransactionBuilder {
    if let Ok(info) = builder.clone().add_fee(inputs.clone()) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

/// A simple fee checker for mainnet v1.0.
///
/// SEE [check_fee](ledger::data_model::Transaction::check_fee)
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_check_fee(
    builder: &TransactionBuilder,
) -> bool {
    builder.clone().check_fee()
}

/// Create a new transaction builder.
/// @param {BigInt} seq_id - Unique sequence ID to prevent replay attacks.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_new(
    seq_id: u64,
) -> *mut TransactionBuilder {
    Box::into_raw(Box::new(TransactionBuilder::new(seq_id)))
}

/// Wraps around TransactionBuilder to add an asset definition operation to a transaction builder instance.
/// @example <caption> Error handling </caption>
/// try {
///     await wasm.add_operation_create_asset(wasm.new_keypair(), "test_memo", wasm.random_asset_type(), wasm.AssetRules.default());
/// } catch (err) {
///     console.log(err)
/// }
///
/// @param {XfrKeyPair} key_pair -  Issuer XfrKeyPair.
/// @param {string} memo - Text field for asset definition.
/// @param {string} token_code - Optional Base64 string representing the token code of the asset to be issued.
/// If empty, a token code will be chosen at random.
/// @param {AssetRules} asset_rules - Asset rules object specifying which simple policies apply
/// to the asset.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_operation_create_asset(
    builder: &TransactionBuilder,
    key_pair: &XfrKeyPair,
    memo: *const c_char,
    token_code: *const c_char,
    asset_rules: &AssetRules,
) -> *mut TransactionBuilder {
    if let Ok(info) = builder.clone().add_operation_create_asset(
        key_pair,
        c_char_to_string(memo),
        c_char_to_string(token_code),
        asset_rules.clone(),
    ) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

/// Wraps around TransactionBuilder to add an asset issuance to a transaction builder instance.
///
/// Use this function for simple one-shot issuances.
///
/// @param {XfrKeyPair} key_pair  - Issuer XfrKeyPair.
/// and types of traced assets.
/// @param {string} code - base64 string representing the token code of the asset to be issued.
/// @param {BigInt} seq_num - Issuance sequence number. Every subsequent issuance of a given asset type must have a higher sequence number than before.
/// @param {BigInt} amount - Amount to be issued.
/// @param {boolean} conf_amount - `true` means the asset amount is confidential, and `false` means it's nonconfidential.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_basic_issue_asset(
    builder: &TransactionBuilder,
    key_pair: &XfrKeyPair,
    code: *const c_char,
    seq_num: u64,
    amount: *const c_char,
    conf_amount: bool,
) -> *mut TransactionBuilder {
    let amount = parse_u64(amount);
    if let Ok(info) = builder.clone().add_basic_issue_asset(
        key_pair,
        c_char_to_string(code),
        seq_num,
        amount,
        conf_amount,
    ) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

/// Adds an operation to the transaction builder that adds a hash to the ledger's custom data
/// store.
/// @param {XfrKeyPair} auth_key_pair - Asset creator key pair.
/// @param {String} code - base64 string representing token code of the asset whose memo will be updated.
/// transaction validates.
/// @param {String} new_memo - The new asset memo.
/// @see {@link module:Findora-Wasm~AssetRules#set_updatable|AssetRules.set_updatable} for more information about how
/// to define an updatable asset.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_operation_update_memo(
    builder: &TransactionBuilder,
    auth_key_pair: &XfrKeyPair,
    code: *const c_char,
    new_memo: *const c_char,
) -> *mut TransactionBuilder {
    if let Ok(info) = builder.clone().add_operation_update_memo(
        auth_key_pair,
        c_char_to_string(code),
        c_char_to_string(new_memo),
    ) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_operation_delegate(
    builder: &TransactionBuilder,
    keypair: &XfrKeyPair,
    amount: *const c_char,
    validator: *const c_char,
) -> *mut TransactionBuilder {
    let amount = parse_u64(amount);
    if let Ok(info) = builder.clone().add_operation_delegate(
        keypair,
        amount,
        c_char_to_string(validator),
    ) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_operation_undelegate(
    builder: &TransactionBuilder,
    keypair: &XfrKeyPair,
) -> *mut TransactionBuilder {
    if let Ok(info) = builder.clone().add_operation_undelegate(keypair) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_operation_undelegate_partially(
    builder: &TransactionBuilder,
    keypair: &XfrKeyPair,
    am: *const c_char,
    target_validator: *const c_char,
) -> *mut TransactionBuilder {
    let am = parse_u64(am);
    if let Ok(info) = builder.clone().add_operation_undelegate_partially(
        keypair,
        am,
        c_char_to_string(target_validator),
    ) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_operation_claim(
    builder: &TransactionBuilder,
    keypair: &XfrKeyPair,
) -> *mut TransactionBuilder {
    if let Ok(info) = builder.clone().add_operation_claim(keypair) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_operation_claim_custom(
    builder: &TransactionBuilder,
    keypair: &XfrKeyPair,
    am: *const c_char,
) -> *mut TransactionBuilder {
    let am = parse_u64(am);
    if let Ok(info) = builder.clone().add_operation_claim_custom(keypair, am) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

/// Adds a serialized transfer asset operation to a transaction builder instance.
/// @param {string} op - a JSON-serialized transfer operation.
/// @see {@link module:Findora-Wasm~TransferOperationBuilder} for details on constructing a transfer operation.
/// @throws Will throw an error if `op` fails to deserialize.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_transfer_operation(
    builder: &TransactionBuilder,
    op: *const c_char,
) -> *mut TransactionBuilder {
    if let Ok(info) = builder.clone().add_transfer_operation(c_char_to_string(op)) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

/// Adds a serialized transfer account operation to a transaction builder instance.
/// @param {string} address - a String which is hex-encoded EVM address or base64 encoded xfr public key or bech32 encoded xfr public key.
/// @param {unsigned long long} amount - Amount to be transfered.
/// @param {XfrKeyPair} kp - Fra ownner key pair.
/// @return null if `address` or 'kp' is incorrect.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_add_operation_convert_account(
    builder: &TransactionBuilder,
    address: *const c_char,
    amount: *const c_char,
    kp: &XfrKeyPair,
    asset: *const c_char,
    lowlevel_data: *const c_char,
) -> *mut TransactionBuilder {
    let amount = parse_u64(amount);
    let addr_stirng = c_char_to_string(address);
    let addr = if addr_stirng.is_empty() {
        None
    } else {
        Some(addr_stirng)
    };
    let asset_str = c_char_to_string(asset);
    let asset = if asset_str.is_empty() {
        None
    } else {
        Some(AssetTypeCode::new_from_base64(&asset_str).unwrap())
    };

    let lowlevel_data_str = c_char_to_string(lowlevel_data);
    let lowlevel_data = if lowlevel_data_str.is_empty() {
        None
    } else {
        Some(hex::decode(lowlevel_data_str).unwrap())
    };

    if let Ok(builder) = builder.clone().add_transfer_to_account_operation(
        amount,
        addr,
        kp,
        asset,
        lowlevel_data,
    ) {
        Box::into_raw(Box::new(builder))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_sign(
    builder: &TransactionBuilder,
    kp: &XfrKeyPair,
) -> *mut TransactionBuilder {
    if let Ok(info) = builder.clone().sign(kp) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

/// Extracts the serialized form of a transaction.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_transaction(
    builder: &TransactionBuilder,
) -> *mut c_char {
    string_to_c_char(builder.transaction())
}

/// Calculates transaction handle.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_transaction_handle(
    builder: &TransactionBuilder,
) -> *mut c_char {
    string_to_c_char(builder.transaction_handle())
}

/// Fetches a client record from a transaction.
/// @param {number} idx - Record to fetch. Records are added to the transaction builder sequentially.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_get_owner_record(
    builder: &TransactionBuilder,
    idx: usize,
) -> *mut ClientAssetRecord {
    Box::into_raw(Box::new(builder.get_owner_record(idx)))
}

/// Fetches an owner memo from a transaction
/// @param {number} idx - Owner memo to fetch. Owner memos are added to the transaction builder sequentially.
#[no_mangle]
pub extern "C" fn findora_ffi_transaction_builder_get_owner_memo(
    builder: &TransactionBuilder,
    idx: usize,
) -> *mut OwnerMemo {
    if let Some(info) = builder.get_owner_memo(idx) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}
