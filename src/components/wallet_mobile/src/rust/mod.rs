#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

pub mod account;
mod crypto;
mod data_model;
#[cfg(test)]
mod tests;
pub mod transaction;
pub mod types;
mod util;

pub use crypto::*;
pub use data_model::*;
pub use transaction::*;
pub use util::*;

/// Constant defining the git commit hash and commit date of the commit this library was built
/// against.
const BUILD_ID: &str = concat!(env!("VERGEN_SHA_SHORT"), " ", env!("VERGEN_BUILD_DATE"));

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Returns the git commit hash and commit date of the commit this library was built against.
pub fn build_id() -> String {
    BUILD_ID.to_string()
}
