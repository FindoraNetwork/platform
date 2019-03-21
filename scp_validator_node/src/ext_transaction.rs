#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/transaction_hooks.rs"));

// use libc;

use core::store::{LedgerAccess, LedgerState, LedgerUpdate, LedgerValidate};
use core::data_model::{Transaction};
use std::slice;

use serde_json;

struct LedgerApp {
    state: LedgerState,
}

impl LedgerApp {
    pub fn new() -> LedgerApp {
        LedgerApp {state: LedgerState::new()}
    }
}

// Convert incoming tx data to the proper Transaction format
fn convert_tx(tx: &[u8]) -> Transaction {
    let transaction: Transaction = serde_json::from_slice(tx).unwrap();

    transaction
}

unsafe fn static_app_handle() -> &'static mut Option<LedgerApp> {
    static mut app: Option<LedgerApp> = None;
    &mut app
}

#[no_mangle]
#[link_name = "\u{1}_InitApp"]
pub extern "C" fn InitApp() -> *mut AppHandle {
    unsafe {
        let opt_app = static_app_handle();
        if opt_app.is_none() {
            *opt_app = Some(LedgerApp::new());
        }
        std::mem::transmute::<*mut LedgerApp, *mut AppHandle>(opt_app.as_mut().unwrap() as *mut LedgerApp)
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
    let app = std::mem::transmute::<*mut AppHandle, *mut LedgerApp>(app);
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
#[no_mangle]
#[link_name = "\u{1}_CommitTransaction"]
pub extern "C" fn CommitTransaction(app: *mut AppHandle, txn: *const BlindTransaction, result: *const BlindTransactionResult) -> bool {
    // ...  
    true
}
#[no_mangle]
#[link_name = "\u{1}_CheckTransaction"]
pub extern "C" fn CheckTransaction(app: *mut AppHandle, txn: *const BlindTransaction) -> bool {
    unsafe {
    let app = unsafe { std::mem::transmute::<*mut AppHandle, *mut LedgerApp>(app) };
    let data = get_transaction_data(txn);
    let slice = unsafe { slice::from_raw_parts(data.bytes, data.length) };
    let tx = convert_tx(slice);

    if (*app).state.validate_transaction(&tx) {
        return true;
    }
    }
    false
}

