#![allow(clippy::cast_ptr_alignment)]

#[allow(non_upper_case_globals)]
#[allow(non_camel_case_types)]
#[allow(non_snake_case)]
#[allow(dead_code)]
mod cpp {
    include!(concat!(env!("OUT_DIR"), "/transaction_hooks.rs"));
}

// use libc;
use crate::ext_transaction::cpp::{
    AppHandle, BlindTransaction, BlindTransactionResult, ConstBytes, RegisterCallbacks, GetTransactionData, GetTransactionResultData, SetTransactionResultStatus, AllocateTransactionResultBuffer, InitApp, DestroyApp, DeliverTransaction, CommitTransaction, CheckTransaction, TransactionResultStatus, TransactionResultStatus_txSUCCESS,
};
use core::store::{LedgerUpdate, LedgerValidate};
use ledger_app::{convert_tx, LedgerApp};

use std::slice;

unsafe fn register_callbacks_handle() -> &'static mut RegisterCallbacks {
    static mut REGISTER_CALLBACKS: RegisterCallbacks = None;
    &mut REGISTER_CALLBACKS
}

unsafe fn get_transaction_data_handle() -> &'static mut GetTransactionData {
    static mut GET_TRANSACTION_DATA: GetTransactionData = None;
    &mut GET_TRANSACTION_DATA
}

unsafe fn get_transaction_result_data_handle() -> &'static mut GetTransactionResultData {
    static mut GET_TRANSACTION_RESULT_DATA: GetTransactionResultData = None;
    &mut GET_TRANSACTION_RESULT_DATA
}

unsafe fn set_transaction_result_status_handle() -> &'static mut SetTransactionResultStatus {
    static mut SET_TRANSACTION_RESULT_STATUS: SetTransactionResultStatus = None;
    &mut SET_TRANSACTION_RESULT_STATUS
}

unsafe fn allocate_transaction_result_buffer_handle() -> &'static mut AllocateTransactionResultBuffer {
    static mut ALLOCATE_TRANSACTION_RESULT_BUFFER: AllocateTransactionResultBuffer = None;
    &mut ALLOCATE_TRANSACTION_RESULT_BUFFER
}

fn set_register_callbacks(ext_register_callbacks: RegisterCallbacks) {
    unsafe { *register_callbacks_handle() = ext_register_callbacks; }
}
fn set_get_transaction_data(ext_get_transaction_data: GetTransactionData) {
    unsafe { *get_transaction_data_handle() = ext_get_transaction_data; }
}
fn set_get_transaction_result_data(ext_get_transaction_result_data: GetTransactionResultData) {
    unsafe { *get_transaction_result_data_handle() = ext_get_transaction_result_data; }
}
fn set_set_transaction_result_status(ext_set_transaction_result_status: SetTransactionResultStatus) {
    unsafe { *set_transaction_result_status_handle() = ext_set_transaction_result_status; }
}
fn set_allocate_transaction_result_buffer(ext_allocate_transaction_result_buffer: AllocateTransactionResultBuffer) {
    unsafe { *allocate_transaction_result_buffer_handle() = ext_allocate_transaction_result_buffer; }
}

fn register_callbacks(init_app_fn: InitApp,
destroy_app_fn: DestroyApp,deliver_transaction_fn: DeliverTransaction,
                                                                        commit_transaction_fn: CommitTransaction,
                                                                        check_transaction_fn: CheckTransaction) {
    unsafe { register_callbacks_handle().as_ref().unwrap()(init_app_fn, destroy_app_fn, deliver_transaction_fn, commit_transaction_fn, check_transaction_fn) }

}

fn get_transaction_data(txn: *const BlindTransaction) -> ConstBytes {
    unsafe { get_transaction_data_handle().as_ref().unwrap()(txn) }
}

fn get_transaction_result_data(res: *const BlindTransactionResult) -> ConstBytes {
    unsafe { get_transaction_result_data_handle().as_ref().unwrap()(res) }
}

fn set_transaction_result_status(res: *mut BlindTransactionResult, status: TransactionResultStatus) {
    unsafe { set_transaction_result_status_handle().as_ref().unwrap()(res, status) }
}

fn allocate_transaction_result_buffer(res: *mut BlindTransactionResult, length: usize)
                                               -> *mut u8 {
                                                   unsafe { allocate_transaction_result_buffer_handle().as_ref().unwrap()(res, length) }
                                               }

#[no_mangle]
#[link_name = "\u{1}_LoadPlugin"]
pub extern "C" fn load_plugin( ext_register_callbacks: RegisterCallbacks, ext_get_transaction_data: GetTransactionData, ext_get_transaction_result_data:  GetTransactionResultData, ext_set_transaction_result_status:  SetTransactionResultStatus, ext_allocate_transaction_result_buffer:  AllocateTransactionResultBuffer) {
    set_register_callbacks(ext_register_callbacks);
    set_get_transaction_data(ext_get_transaction_data);
    set_get_transaction_result_data(ext_get_transaction_result_data);
    set_set_transaction_result_status(ext_set_transaction_result_status);
    set_allocate_transaction_result_buffer(ext_allocate_transaction_result_buffer);

    register_callbacks(Some(init_app), Some(destroy_app), Some(deliver_transaction), Some(commit_transaction), Some(check_transaction));
}


unsafe fn static_app_handle() -> &'static mut Option<LedgerApp> {
    static mut APP: Option<LedgerApp> = None;
    &mut APP
}

extern "C" fn init_app() -> *mut AppHandle {
    unsafe {
        let opt_app = static_app_handle();
        if opt_app.is_none() {
            *opt_app = Some(LedgerApp::default());
        }
        opt_app.as_mut().unwrap() as *mut LedgerApp as *mut AppHandle
        // std::mem::transmute::<*mut LedgerApp, *mut AppHandle>(opt_app.as_mut().unwrap() as *mut LedgerApp)
    }
}

extern "C" fn destroy_app(_app: *mut AppHandle) -> bool {
    true
}

extern "C" fn deliver_transaction(app: *mut AppHandle,
                                     txn: *const BlindTransaction,
                                     result: *mut BlindTransactionResult)
                                     -> bool {
    unsafe {
        let app = app as *mut LedgerApp; // std::mem::transmute::<*mut AppHandle, *mut LedgerApp>(app);
        let data = get_transaction_data(txn);
        let slice = slice::from_raw_parts(data.bytes, data.length);
        let tx = convert_tx(slice);

        //if !
        (*app).state.apply_transaction(tx);
        // {
        //     set_transaction_result_status(result, TransactionResultStatus_txFAILED);
        //     return false;
        // }
        set_transaction_result_status(result, TransactionResultStatus_txSUCCESS);
    }
    true
}

extern "C" fn commit_transaction(_app: *mut AppHandle,
                                    _txn: *const BlindTransaction,
                                    _result: *const BlindTransactionResult)
                                    -> bool {
    // ...
    true
}

extern "C" fn check_transaction(app: *mut AppHandle, txn: *const BlindTransaction) -> bool {
    unsafe {
        let app = app as *mut LedgerApp; // std::mem::transmute::<*mut AppHandle, *mut LedgerApp>(app);
        let data = get_transaction_data(txn);
        let slice = slice::from_raw_parts(data.bytes, data.length);
        let tx = convert_tx(slice);
        if (*app).state.validate_transaction(&tx) {
            return true;
        }
    }
    false
}
