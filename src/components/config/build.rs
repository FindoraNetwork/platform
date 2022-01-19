use vergen::{generate_cargo_keys, ConstantsFlags};

fn main() {
    let mut flags = ConstantsFlags::all();
    // Tell vergen to use the semver from cargo and not `git describe`
    flags.set(ConstantsFlags::SEMVER, false);
    flags.set(ConstantsFlags::SEMVER_FROM_CARGO_PKG, true);

    // Generate the 'cargo:' key output
    generate_cargo_keys(flags).expect("Unable to generate the cargo keys!");
}
