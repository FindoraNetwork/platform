//!
//! # Query Interface for WebEnd
//!

pub mod query_api;

/// used to notify `query server` to do updating
pub use query_api::server::BLOCK_CREATED;
