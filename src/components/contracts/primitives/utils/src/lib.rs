#![deny(warnings)]
#![allow(missing_docs)]

pub mod ecdsa;
pub mod hashing;

use primitive_types::H160;

pub fn timestamp_converter(timestamp: protobuf::well_known_types::Timestamp) -> u64 {
    let unix_time =
        core::time::Duration::new(timestamp.seconds as u64, timestamp.nanos as u32);
    unix_time.as_secs() as u64
}

pub fn proposer_converter(address: Vec<u8>) -> Option<H160> {
    if address.len() < 20 {
        None
    } else {
        Some(H160::from_slice(&address[0..20]))
    }
}
