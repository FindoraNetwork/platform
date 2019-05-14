#![allow(clippy::cast_ptr_alignment)]
#![allow(dead_code)]

#[allow(non_upper_case_globals)]
#[allow(non_camel_case_types)]
#[allow(non_snake_case)]
mod cpp {
  include!(concat!(env!("OUT_DIR"), "/scp_interface.rs"));
}

// use libc;
use crate::ext_transaction::cpp::{
  AllocateTransactionResultBuffer, AppHandle, BlindTransaction, BlindTransactionResult,
  CheckTransaction, CommitTransaction, ConstBytes, DeliverTransaction, DestroyApp,
  GetTransactionData, GetTransactionResultData, InitApp, ModifyTransactionData, MutBytes,
  PostTxnBlock, PreTxnBlock, RegisterCallbacks, SetTransactionResultStatus,
  TransactionResultStatus, TransactionResultStatus_txFAILED, TransactionResultStatus_txSUCCESS,
};
use core::data_model::errors::PlatformError;
use core::store::{LedgerState, LedgerValidate, TxnContext};
use ledger_app::{convert_tx, LedgerApp};

use std::slice;

struct SCPLedgerApp {
  la: LedgerApp,
}
impl SCPLedgerApp {
  fn new() -> Result<SCPLedgerApp, PlatformError> {
    Ok(SCPLedgerApp { la: LedgerApp::new(LedgerState::default())? })
  }
}
unsafe fn register_callbacks_handle() -> &'static mut RegisterCallbacks {
  static mut REGISTER_CALLBACKS: RegisterCallbacks = None;
  &mut REGISTER_CALLBACKS
}

unsafe fn get_transaction_data_handle() -> &'static mut GetTransactionData {
  static mut GET_TRANSACTION_DATA: GetTransactionData = None;
  &mut GET_TRANSACTION_DATA
}

unsafe fn modify_transaction_data_handle() -> &'static mut ModifyTransactionData {
  static mut MODIFY_TRANSACTION_DATA: ModifyTransactionData = None;
  &mut MODIFY_TRANSACTION_DATA
}

unsafe fn get_transaction_result_data_handle() -> &'static mut GetTransactionResultData {
  static mut GET_TRANSACTION_RESULT_DATA: GetTransactionResultData = None;
  &mut GET_TRANSACTION_RESULT_DATA
}

unsafe fn set_transaction_result_status_handle() -> &'static mut SetTransactionResultStatus {
  static mut SET_TRANSACTION_RESULT_STATUS: SetTransactionResultStatus = None;
  &mut SET_TRANSACTION_RESULT_STATUS
}

unsafe fn allocate_transaction_result_buffer_handle(
  )
    -> &'static mut AllocateTransactionResultBuffer
{
  static mut ALLOCATE_TRANSACTION_RESULT_BUFFER: AllocateTransactionResultBuffer = None;
  &mut ALLOCATE_TRANSACTION_RESULT_BUFFER
}

fn set_register_callbacks(ext_register_callbacks: RegisterCallbacks) {
  unsafe {
    *register_callbacks_handle() = ext_register_callbacks;
  }
}
fn set_get_transaction_data(ext_get_transaction_data: GetTransactionData) {
  unsafe {
    *get_transaction_data_handle() = ext_get_transaction_data;
  }
}
fn set_modify_transaction_data(ext_modify_transaction_data: ModifyTransactionData) {
  unsafe {
    *modify_transaction_data_handle() = ext_modify_transaction_data;
  }
}
fn set_get_transaction_result_data(ext_get_transaction_result_data: GetTransactionResultData) {
  unsafe {
    *get_transaction_result_data_handle() = ext_get_transaction_result_data;
  }
}
fn set_set_transaction_result_status(ext_set_transaction_result_status: SetTransactionResultStatus)
{
  unsafe {
    *set_transaction_result_status_handle() = ext_set_transaction_result_status;
  }
}
fn set_allocate_transaction_result_buffer(ext_allocate_transaction_result_buffer: AllocateTransactionResultBuffer)
{
  unsafe {
    *allocate_transaction_result_buffer_handle() = ext_allocate_transaction_result_buffer;
  }
}

fn register_callbacks(init_app_fn: InitApp,
                      destroy_app_fn: DestroyApp,
                      pre_txn_block_fn: PreTxnBlock,
                      post_txn_block_fn: PostTxnBlock,
                      deliver_transaction_fn: DeliverTransaction,
                      commit_transaction_fn: CommitTransaction,
                      check_transaction_fn: CheckTransaction) {
  unsafe {
    register_callbacks_handle().as_ref().unwrap()(init_app_fn,
                                                  destroy_app_fn,
                                                  pre_txn_block_fn,
                                                  post_txn_block_fn,
                                                  deliver_transaction_fn,
                                                  commit_transaction_fn,
                                                  check_transaction_fn)
  }
}

fn get_transaction_data(txn: *const BlindTransaction) -> ConstBytes {
  unsafe { get_transaction_data_handle().as_ref().unwrap()(txn) }
}

fn modify_transaction_data(txn: *mut BlindTransaction) -> MutBytes {
  unsafe { modify_transaction_data_handle().as_ref().unwrap()(txn) }
}

fn get_transaction_result_data(res: *const BlindTransactionResult) -> ConstBytes {
  unsafe { get_transaction_result_data_handle().as_ref().unwrap()(res) }
}

fn set_transaction_result_status(res: *mut BlindTransactionResult,
                                 status: TransactionResultStatus) {
  unsafe { set_transaction_result_status_handle().as_ref().unwrap()(res, status) }
}

fn allocate_transaction_result_buffer(res: *mut BlindTransactionResult, length: usize) -> *mut u8 {
  unsafe {
    allocate_transaction_result_buffer_handle().as_ref()
                                               .unwrap()(res, length)
  }
}

#[no_mangle]
#[link_name = "\u{1}_LoadPlugin"]
pub extern "C" fn load_plugin(ext_register_callbacks: RegisterCallbacks,
                              ext_get_transaction_data: GetTransactionData,
                              ext_modify_transaction_data: ModifyTransactionData,
                              ext_get_transaction_result_data: GetTransactionResultData,
                              ext_set_transaction_result_status: SetTransactionResultStatus,
                              ext_allocate_transaction_result_buffer:  AllocateTransactionResultBuffer)
{
  set_register_callbacks(ext_register_callbacks);
  set_get_transaction_data(ext_get_transaction_data);
  set_modify_transaction_data(ext_modify_transaction_data);
  set_get_transaction_result_data(ext_get_transaction_result_data);
  set_set_transaction_result_status(ext_set_transaction_result_status);
  set_allocate_transaction_result_buffer(ext_allocate_transaction_result_buffer);

  register_callbacks(Some(init_app),
                     Some(destroy_app),
                     Some(pre_txn_block),
                     Some(post_txn_block),
                     Some(deliver_transaction),
                     Some(commit_transaction),
                     Some(check_transaction));
}

unsafe fn static_app_handle() -> &'static mut Option<SCPLedgerApp> {
  static mut APP: Option<SCPLedgerApp> = None;
  &mut APP
}

extern "C" fn init_app() -> *mut AppHandle {
  unsafe {
    let opt_app = static_app_handle();
    if opt_app.is_none() {
      *opt_app = SCPLedgerApp::new().ok();
    }
    opt_app.as_mut().unwrap() as *mut SCPLedgerApp as *mut AppHandle
    // std::mem::transmute::<*mut LedgerApp, *mut AppHandle>(opt_app.as_mut().unwrap() as *mut LedgerApp)
  }
}

extern "C" fn destroy_app(_app: *mut AppHandle) -> bool {
  true
}

extern "C" fn pre_txn_block(_app: *mut AppHandle) {}

extern "C" fn post_txn_block(_app: *mut AppHandle) {}

extern "C" fn deliver_transaction(app: *mut AppHandle,
                                  txn: *mut BlindTransaction,
                                  result: *mut BlindTransactionResult)
                                  -> bool {
  unsafe {
    let app = app as *mut SCPLedgerApp; // std::mem::transmute::<*mut AppHandle, *mut LedgerApp>(app);
    let data = modify_transaction_data(txn);
    let slice = slice::from_raw_parts(data.bytes, data.length);
    if let Some(tx) = convert_tx(slice) {
      if let Some(tracker) = (*app).la.get_mut_pending_state() {
        if let Ok(mut context) = TxnContext::new(tracker) {
          let result = context.validate_transaction(&tx);
          if result {
            (*app).la.cache_transaction(tx);
            return true;
          }
        }
      }
    }
  }
  false
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
    let app = app as *mut SCPLedgerApp; // std::mem::transmute::<*mut AppHandle, *mut LedgerApp>(app);
    let data = get_transaction_data(txn);
    let slice = slice::from_raw_parts(data.bytes, data.length);
    if let Some(tx) = convert_tx(slice) {
      return true;
    }
  }
  false
}
