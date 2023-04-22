use crate::*;
use baseapp::BaseApp;
use ethereum_types::H160;
use fp_mocks::*;

use evm_precompile_utils::{error, EvmDataWriter};
use module_evm::precompile::Precompile;
use sha3::{Digest, Keccak256};

pub const ZKCARD_PRECOMPILE_ADDRESS: u64 = 0x3000;

#[test]
fn selector_less_than_four_bytes() {
    let invalid_selector = vec![1u8, 2u8, 3u8];
    assert_eq!(
        ZkCard::execute(
            &invalid_selector,
            None,
            &evm::Context {
                address: H160::from_low_u64_be(ZKCARD_PRECOMPILE_ADDRESS),
                caller: ALICE_ECDSA.address,
                apparent_value: From::from(0),
            },
            &BASE_APP.lock().unwrap().deliver_state,
        ),
        Err(PrecompileFailure::Error {
            exit_status: error("tried to parse selector out of bounds")
        })
    );
}

#[test]
fn no_selector_exists_but_length_is_right() {
    let invalid_selector = vec![1u8, 2u8, 3u8, 4u8];

    assert_eq!(
        ZkCard::execute(
            &invalid_selector,
            None,
            &evm::Context {
                address: H160::from_low_u64_be(ZKCARD_PRECOMPILE_ADDRESS),
                caller: ALICE_ECDSA.address,
                apparent_value: From::from(0),
            },
            &BASE_APP.lock().unwrap().deliver_state,
        ),
        Err(PrecompileFailure::Error {
            exit_status: error("unknown selector")
        })
    );
}


#[test]
fn selectors() {
    assert_eq!(Call::VerifyKeyOwnership as u32, 0x3931f649);
    assert_eq!(Call::VerifyReveal as u32, 0x9ca80d77);
    assert_eq!(Call::ComputeAggregateKey as u32, 0x5b2bfec7);
    assert_eq!(Call::VerifyShuffle as u32, 0x2a379865);
    assert_eq!(Call::Reveal as u32, 0x6a33d652);
    assert_eq!(Call::Mask as u32, 0x5a8890bc);
}

#[test]
fn zkcard_works() {
    verify_key_ownership_works();
    compute_aggregate_key_works();
    verify_shuffle_works();
    verify_reveal_works();
    reveal_works();
    mask_works();
}

fn verify_key_ownership_works() {
}

fn compute_aggregate_key_works() { 
}

fn verify_shuffle_works() {
}

fn verify_reveal_works() {
}

fn reveal_works() {
}

fn mask_works() {
}
