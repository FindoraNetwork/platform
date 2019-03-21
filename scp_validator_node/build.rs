extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    let bindings =
        bindgen::Builder::default().header("src/TransactionHooks.h")
                                   .whitelist_type("AppHandle")
                                   .whitelist_type("BlindTransaction")
                                   .whitelist_type("BlindTransactionResult")
                                   .whitelist_function("get_transaction_data")
                                   .whitelist_function("get_transaction_result_data")
                                   .whitelist_function("set_transaction_result_status")
                                   .whitelist_function("allocate_transaction_result_buffer")
                                   .generate()
                                   .expect("Failed to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings.write_to_file(out_path.join("transaction_hooks.rs"))
            .expect("Failed to output bindings");
}
