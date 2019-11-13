#![deny(warnings)]
extern crate arrayref;
extern crate base64;
extern crate serde;
extern crate serde_derive;

#[macro_use]
extern crate itertools;
extern crate zei;

#[macro_use]
extern crate findora;

pub mod data_model;
pub mod utils;

#[cfg(not(target_arch = "wasm32"))]
pub mod permissions;

#[cfg(not(target_arch = "wasm32"))]
pub mod store;
