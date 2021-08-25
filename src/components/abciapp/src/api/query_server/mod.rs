//!
//! # query interface of ledgerState
//!

pub mod ledger_api;
pub mod query_api;

/// used to notify `query server` to do updating
pub use query_api::query_server::BLOCK_CREATED;
