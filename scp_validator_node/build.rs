extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    let bindings = bindgen::Builder::default().header("src/TransactionHooks.h")
    .whitelist_type("BlindTransaction")
    .whitelist_type("BlindTransactionResult")
    .whitelist_type("BlindOperation")
    .whitelist_type("BlindOperationResult")
    .whitelist_type("BlindSignature")
    .whitelist_function("begin_operations")
    .whitelist_function("end_operations")
    .whitelist_function("next_operation")
    .whitelist_function("begin_signatures")
    .whitelist_function("end_signatures")
    .whitelist_function("next_signature")
    .whitelist_function("begin_op_results")
    .whitelist_function("end_op_results")
    .whitelist_function("next_op_result")
    .whitelist_function("begin_op_results_const")
    .whitelist_function("end_op_results_const")
    .whitelist_function("next_op_result_const")
    .whitelist_function("allocate_operation_result_buffer")
    .whitelist_function("set_transaction_result_status")
    .generate().expect("Failed to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings.write_to_file(out_path.join("transaction_hooks.rs")).expect("Failed to output bindings");
}
