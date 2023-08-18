use std::os::raw::c_char;
use zei::{XfrKeyPair, XfrPublicKey};

use super::parse_u64;
use crate::rust::TransferOperationBuilder;
use crate::rust::*;

#[no_mangle]
/// Create a new transfer operation builder.
pub extern "C" fn findora_ffi_transfer_operation_builder_new(
) -> *mut TransferOperationBuilder {
    Box::into_raw(Box::new(TransferOperationBuilder::new()))
}

#[no_mangle]
/// # Safety
///
/// Debug function that does not need to go into the docs.
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_debug(
    builder: *const TransferOperationBuilder,
) -> *mut c_char {
    string_to_c_char((&*builder).debug())
}

#[no_mangle]
/// # Safety
///
/// Wraps around TransferOperationBuilder to add an input to a transfer operation builder.
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_add_input_with_tracing(
    builder: *const TransferOperationBuilder,
    txo_ref: *const TxoRef,
    asset_record: *const ClientAssetRecord,
    owner_memo: *const OwnerMemo,
    tracing_policies: *const TracingPolicies,
    key: *const XfrKeyPair,
    amount: *const c_char,
) -> *mut TransferOperationBuilder {
    let amount = parse_u64(amount);
    let memo = if owner_memo.is_null() {
        None
    } else {
        Some((*owner_memo).clone())
    };
    if let Ok(info) = (*builder).clone().add_input_with_tracing(
        *txo_ref,
        (*asset_record).clone(),
        memo,
        &*tracing_policies,
        &*key,
        amount,
    ) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// Wraps around TransferOperationBuilder to add an input to a transfer operation builder.
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_add_input_no_tracing(
    builder: *const TransferOperationBuilder,
    txo_ref: *const TxoRef,
    asset_record: *const ClientAssetRecord,
    owner_memo: *const OwnerMemo,
    key: *const XfrKeyPair,
    amount: *const c_char,
) -> *mut TransferOperationBuilder {
    let amount = parse_u64(amount);
    let memo = if owner_memo.is_null() {
        None
    } else {
        Some((*owner_memo).clone())
    };
    if let Ok(info) = (*builder).clone().add_input_no_tracing(
        *txo_ref,
        &*asset_record,
        memo,
        &*key,
        amount,
    ) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// Wraps around TransferOperationBuilder to add an output to a transfer operation builder.
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_add_output_with_tracing(
    builder: *const TransferOperationBuilder,
    amount: *const c_char,
    recipient: *const XfrPublicKey,
    tracing_policies: *const TracingPolicies,
    code: *const c_char,
    conf_amount: bool,
    conf_type: bool,
) -> *mut TransferOperationBuilder {
    let amount = parse_u64(amount);
    if let Ok(info) = (*builder).clone().add_output_with_tracing(
        amount,
        &*recipient,
        &*tracing_policies,
        c_char_to_string(code),
        conf_amount,
        conf_type,
    ) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// Wraps around TransferOperationBuilder to add an output to a transfer operation builder.
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_add_output_no_tracing(
    builder: *const TransferOperationBuilder,
    amount: *const c_char,
    recipient: &XfrPublicKey,
    code: *const c_char,
    conf_amount: bool,
    conf_type: bool,
) -> *mut TransferOperationBuilder {
    let amount = parse_u64(amount);
    if let Ok(info) = (*builder).clone().add_output_no_tracing(
        amount,
        &*recipient,
        c_char_to_string(code),
        conf_amount,
        conf_type,
    ) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// Wraps around TransferOperationBuilder to ensure the transfer inputs and outputs are balanced.
/// This function will add change outputs for all unspent portions of input records.
/// @throws Will throw an error if the transaction cannot be balanced.
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_balance(
    builder: *const TransferOperationBuilder,
) -> *mut TransferOperationBuilder {
    if let Ok(info) = (*builder).clone().balance(None) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// Wraps around TransferOperationBuilder to finalize the transaction.
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_create(
    builder: *const TransferOperationBuilder,
) -> *mut TransferOperationBuilder {
    if let Ok(info) = (*builder).clone().create() {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
/// Wraps around TransferOperationBuilder to add a signature to the operation.
///
/// All input owners must sign.
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_sign(
    builder: *const TransferOperationBuilder,
    kp: *const XfrKeyPair,
) -> *mut TransferOperationBuilder {
    if let Ok(info) = (*builder).clone().sign(&*kp) {
        Box::into_raw(Box::new(info))
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_builder(
    builder: *const TransferOperationBuilder,
) -> *mut c_char {
    string_to_c_char((&*builder).builder())
}

#[no_mangle]
/// # Safety
///
/// Wraps around TransferOperationBuilder to extract an operation expression as JSON.
pub unsafe extern "C" fn findora_ffi_transfer_operation_builder_transaction(
    builder: *const TransferOperationBuilder,
) -> *mut c_char {
    string_to_c_char((&*builder).transaction().unwrap())
}
