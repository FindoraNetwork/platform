extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    let bindings = bindgen::Builder::default().header("src/TransactionHooks.h")
                                              .whitelist_type("RegisterCallbacks")
                                              .whitelist_type("GetTransactionData")
                                              .whitelist_type("GetTransactionResultData")
                                              .whitelist_type("SetTransactionResultStatus")
                                              .whitelist_type("AllocateTransactionResultBuffer")
                                              .generate()
                                              .expect("Failed to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings.write_to_file(out_path.join("transaction_hooks.rs"))
            .expect("Failed to output bindings");
}
