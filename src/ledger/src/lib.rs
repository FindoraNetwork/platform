//!
//! The findora ledger core implementation
//!

// #![deny(warnings)]
#![deny(missing_docs)]
#![allow(clippy::needless_borrow)]

#[macro_use]
pub mod data_model;
pub mod converter;
pub mod staking;
pub mod utils;

#[cfg(all(not(target_arch = "wasm32"), feature = "fin_storage"))]
pub mod store;

pub use fbnc;

use ruc::*;
const LSSED_VAR: &str = "LEDGER_STATE_SNAPSHOT_ENTRIES_DIR";
lazy_static::lazy_static! {
    static ref SNAPSHOT_ENTRIES_DIR: String = pnk!(std::env::var(LSSED_VAR));
}
