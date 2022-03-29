use crate::rust::types;
use crate::rust::*;
use std::os::raw::c_char;

use super::parse_u64;

#[no_mangle]
/// Fee smaller than this value will be denied.
pub extern "C" fn findora_ffi_fra_get_minimal_fee() -> u64 {
    fra_get_minimal_fee()
}

#[no_mangle]
/// The destination for fee to be transfered to.
pub extern "C" fn findora_ffi_fra_get_dest_pubkey() -> *mut types::XfrPublicKey {
    Box::into_raw(Box::new(types::XfrPublicKey::from(fra_get_dest_pubkey())))
}

#[no_mangle]
pub extern "C" fn findora_ffi_fee_inputs_new() -> *mut FeeInputs {
    Box::into_raw(Box::new(FeeInputs::new()))
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_fee_inputs_append(
    ptr: *mut FeeInputs,
    am: *const c_char,
    tr: *const TxoRef,
    ar: *const ClientAssetRecord,
    om: *const OwnerMemo,
    kp: *const types::XfrKeyPair,
) {
    let am = parse_u64(am);
    assert!(!ptr.is_null());
    let input = &mut *ptr;

    let om_op = if om.is_null() {
        None
    } else {
        Some((*om).clone())
    };

    input.append(am, *tr, (*ar).clone(), om_op, (**kp).clone());
}

#[no_mangle]
/// The system address used to reveive delegation principals.
pub extern "C" fn findora_ffi_get_delegation_target_address() -> *mut c_char {
    string_to_c_char(get_delegation_target_address())
}

#[no_mangle]
pub extern "C" fn findora_ffi_get_coinbase_address() -> *mut c_char {
    string_to_c_char(get_coinbase_address())
}

#[no_mangle]
pub extern "C" fn findora_ffi_get_coinbase_principal_address() -> *mut c_char {
    string_to_c_char(get_coinbase_principal_address())
}

#[no_mangle]
pub extern "C" fn findora_ffi_get_delegation_min_amount() -> u64 {
    get_delegation_min_amount()
}

#[no_mangle]
pub extern "C" fn findora_ffi_get_delegation_max_amount() -> u64 {
    get_delegation_max_amount()
}
