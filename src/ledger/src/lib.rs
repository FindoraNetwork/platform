//!
//! The findora ledger core implementation
//!

#![deny(warnings)]
#![deny(missing_docs)]
#![allow(clippy::needless_borrow)]

#[macro_use]
pub mod data_model;

pub mod staking;

#[cfg(not(target_arch = "wasm32"))]
pub mod store;
