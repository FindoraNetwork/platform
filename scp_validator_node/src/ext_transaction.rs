#![allow(clippy::cast_ptr_alignment)]

#[allow(non_upper_case_globals)]
#[allow(non_camel_case_types)]
#[allow(non_snake_case)]
#[allow(dead_code)]
mod cpp {
include!(concat!(env!("OUT_DIR"), "/transaction_hooks.rs"));
}

// use libc;

use core::store::{LedgerUpdate, LedgerValidate};
use ledger_app::{ LedgerApp, convert_tx };
use crate::ext_transaction::cpp::{AppHandle, BlindTransaction, BlindTransactionResult, TransactionResultStatus_txSUCCESS};

use std::slice;

unsafe fn static_app_handle() -> &'static mut Option<LedgerApp> {
    static mut APP: Option<LedgerApp> = None;
    &mut APP
}

#[no_mangle]
#[link_name = "\u{1}_InitApp"]
pub extern "C" fn InitApp() -> *mut AppHandle {
    unsafe {
        let opt_app = static_app_handle();
        if opt_app.is_none() {
            *opt_app = Some(LedgerApp::default());
        }
        opt_app.as_mut().unwrap() as *mut LedgerApp as *mut AppHandle
        // std::mem::transmute::<*mut LedgerApp, *mut AppHandle>(opt_app.as_mut().unwrap() as *mut LedgerApp)
    }
}

#[no_mangle]
#[link_name = "\u{1}_DestroyApp"]
pub extern "C" fn DestroyApp(_app: *mut AppHandle) -> bool {
    true
}

#[no_mangle]
#[link_name = "\u{1}_DeliverTransaction"]
pub extern "C" fn DeliverTransaction(app: *mut AppHandle, txn: *const BlindTransaction, result: *mut BlindTransactionResult) -> bool {
    unsafe {
    let app = app as *mut LedgerApp; // std::mem::transmute::<*mut AppHandle, *mut LedgerApp>(app);
    let data = cpp::get_transaction_data(txn);
    let slice = slice::from_raw_parts(data.bytes, data.length);
    let tx = convert_tx(slice);

    //if !
    (*app).state.apply_transaction(tx);
    // {
    //     set_transaction_result_status(result, TransactionResultStatus_txFAILED);
    //     return false;
    // }
    cpp::set_transaction_result_status(result, TransactionResultStatus_txSUCCESS);
    }
    true
}
#[no_mangle]
#[link_name = "\u{1}_CommitTransaction"]
pub extern "C" fn CommitTransaction(_app: *mut AppHandle, _txn: *const BlindTransaction, _result: *const BlindTransactionResult) -> bool {
    // ...  
    true
}
#[no_mangle]
#[link_name = "\u{1}_CheckTransaction"]
pub extern "C" fn CheckTransaction(app: *mut AppHandle, txn: *const BlindTransaction) -> bool {
    unsafe {
    let app = app as *mut LedgerApp; // std::mem::transmute::<*mut AppHandle, *mut LedgerApp>(app);
    let data = cpp::get_transaction_data(txn);
    let slice = slice::from_raw_parts(data.bytes, data.length);
    let tx = convert_tx(slice);
    if (*app).state.validate_transaction(&tx) {
        return true;
    }
    }
    false
}

