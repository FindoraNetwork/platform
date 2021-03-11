#![deny(warnings)]

extern crate arrayref;
extern crate base64;
extern crate serde;
extern crate serde_derive;

extern crate itertools;
extern crate zei;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

#[macro_use]
pub mod data_model;
pub mod policies;
pub mod policy_script;

#[cfg(not(target_arch = "wasm32"))]
pub mod store;
