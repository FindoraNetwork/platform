//!
//! # Staking About Operations
//!
//! All the logic in this module relies on other operations in the same transaction
//! to prevent replay attacks, and it does not implement this mechanism by itself.
//!
//! In the current implementation, the first operation must be a `TransferAsset`.
//!

pub mod claim;
pub mod delegation;
pub mod fra_distribution;
pub mod governance;
pub mod mint_fra;
pub mod replace_staker;
pub mod undelegation;
pub mod update_staker;
pub mod update_validator;
