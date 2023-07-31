use crate::rust::types;
use crate::rust::*;

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_xfr_public_key_free(ptr: *mut types::XfrPublicKey) {
    if ptr.is_null() {
        return;
    }
    let _ = Box::from_raw(ptr);
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "C" fn findora_ffi_fee_inputs_free(ptr: *mut FeeInputs) {
    if ptr.is_null() {
        return;
    }
    let _ = Box::from_raw(ptr);
}
