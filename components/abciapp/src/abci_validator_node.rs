#![deny(warnings)]
#![allow(clippy::field_reassign_with_default)]

use ruc::*;

pub mod abci;

fn main() {
    env_logger::init();
    log::info!(concat!(
        "Build: ",
        env!("VERGEN_SHA"),
        " ",
        env!("VERGEN_BUILD_DATE")
    ));

    pnk!(abci::run());
}
