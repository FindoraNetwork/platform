//!
//! # binary process
//!

use abciapp::abci;
use ruc::*;

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
