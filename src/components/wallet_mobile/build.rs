use std::{env, path::PathBuf};
use vergen::{generate_cargo_keys, ConstantsFlags};

fn main() {
    let mut flags = ConstantsFlags::all();
    // Tell vergen to use the semver from cargo and not `git describe`
    flags.set(ConstantsFlags::SEMVER, false);
    flags.set(ConstantsFlags::SEMVER_FROM_CARGO_PKG, true);

    // Generate the 'cargo:' key output
    generate_cargo_keys(flags).expect("Unable to generate the cargo keys!");

    // Export c header file for ffi interface
    let crate_dir = env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR env var is not defined");
    let config = cbindgen::Config::from_file("cbindgen.toml")
        .expect("Unable to find cbindgen.toml configuration file");
    cbindgen::generate_with_config(&crate_dir, config)
        .unwrap()
        .write_to_file(target_dir(crate_dir).join("wallet_mobile_ffi.h"));
}

fn target_dir(crate_dir: String) -> PathBuf {
    if let Ok(target) = env::var("CARGO_TARGET_DIR") {
        PathBuf::from(target)
    } else {
        PathBuf::from(crate_dir)
    }
}
