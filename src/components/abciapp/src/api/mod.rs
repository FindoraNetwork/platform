//!
//! # Services provided by api
//!

/// Provide query service for ledgerState
pub mod query_server;

/// Provide services for operating transactions
pub mod submission_server;

/// Configuration services to adjust running cluster behavior online
mod configure {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize, Debug)]
    pub struct Configuration {
        // abicd, evm, profile
        pub component: String,
        //
        pub module: String,
        //
        pub submodule: String,
        //
        pub parameters: Vec<u8>,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct ProfilerParam {
        pub enable: bool,
    }
}
