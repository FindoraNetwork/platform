use crate::*;
use baseapp::BaseApp;
use fp_mocks::*;

use evm_precompile_utils::{error, EvmDataWriter, LogsBuilder};
use module_evm::precompile::Precompile;
use sha3::{Digest, Keccak256};

pub const FRC20_PRECOMPILE_ADDRESS: u64 = 9;

#[test]
fn selector_less_than_four_bytes() {
    let invalid_selector = vec![1u8, 2u8, 3u8];

    assert_eq!(
        FRC20::<BaseApp>::execute(
            &invalid_selector,
            None,
            &evm::Context {
                address: H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS),
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
        FRC20::<BaseApp>::execute(
            &invalid_selector,
            None,
            &evm::Context {
                address: H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS),
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
    assert_eq!(Call::Name as u32, 0x06fdde03);
    assert_eq!(Call::Symbol as u32, 0x95d89b41);
    assert_eq!(Call::Decimals as u32, 0x313ce567);
    assert_eq!(Call::TotalSupply as u32, 0x18160ddd);
    assert_eq!(Call::BalanceOf as u32, 0x70a08231);
    assert_eq!(Call::Transfer as u32, 0xa9059cbb);
    assert_eq!(Call::Allowance as u32, 0xdd62ed3e);
    assert_eq!(Call::Approve as u32, 0x095ea7b3);
    assert_eq!(Call::TransferFrom as u32, 0x23b872dd);

    assert_eq!(
        TRANSFER_EVENT_SELECTOR,
        &Keccak256::digest(b"Transfer(address,address,uint256)")[..]
    );
    assert_eq!(
        APPROVAL_EVENT_SELECTOR,
        &Keccak256::digest(b"Approval(address,address,uint256)")[..]
    );
}

#[test]
fn frc20_works() {
    test_mint_balance(&ALICE_ECDSA.account_id, 1000u64.into(), 1);
    test_mint_balance(&BOB_ECDSA.account_id, 1000u64.into(), 2);

    total_supply_works();
    balance_of_works();
    transfer_works();
    approve_works();
    allowance_works();
    transfer_from_works();
}

fn total_supply_works() {
    assert_eq!(
        FRC20::<BaseApp>::execute(
            &EvmDataWriter::new()
                .write_selector(Call::TotalSupply)
                .build(),
            None,
            &evm::Context {
                address: H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS),
                caller: ALICE_ECDSA.address,
                apparent_value: From::from(0),
            },
            &BASE_APP.lock().unwrap().deliver_state,
        ),
        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: EvmDataWriter::new().write(U256::from(2000u64)).build(),
            cost: GAS_TOTAL_SUPPLY,
            logs: Default::default(),
        })
    );
}

fn balance_of_works() {
    balance_of(BOB_ECDSA.address, U256::from(1000));
}

fn balance_of(who: H160, expected_value: U256) {
    assert_eq!(
        FRC20::<BaseApp>::execute(
            &EvmDataWriter::new()
                .write_selector(Call::BalanceOf)
                .write(Address(who))
                .build(),
            None,
            &evm::Context {
                address: H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS),
                caller: ALICE_ECDSA.address,
                apparent_value: From::from(0),
            },
            &BASE_APP.lock().unwrap().deliver_state,
        ),
        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: EvmDataWriter::new().write(expected_value).build(),
            cost: GAS_BALANCE_OF,
            logs: Default::default(),
        })
    );
}

fn transfer_works() {
    assert_eq!(
        FRC20::<BaseApp>::execute(
            &EvmDataWriter::new()
                .write_selector(Call::Transfer)
                .write(Address(BOB_ECDSA.address))
                .write(U256::from(400))
                .build(),
            None,
            &evm::Context {
                address: H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS),
                caller: ALICE_ECDSA.address,
                apparent_value: From::from(0),
            },
            &BASE_APP.lock().unwrap().deliver_state,
        ),
        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: EvmDataWriter::new().write(true).build(),
            cost: GAS_TRANSFER + 1756,
            logs: LogsBuilder::new(H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS))
                .log3(
                    TRANSFER_EVENT_SELECTOR,
                    ALICE_ECDSA.address,
                    BOB_ECDSA.address,
                    EvmDataWriter::new().write(U256::from(400)).build(),
                )
                .build(),
        })
    );

    balance_of(ALICE_ECDSA.address, U256::from(600));

    balance_of(BOB_ECDSA.address, U256::from(1400));
}

fn approve_works() {
    assert_eq!(
        FRC20::<BaseApp>::execute(
            &EvmDataWriter::new()
                .write_selector(Call::Approve)
                .write(Address(BOB_ECDSA.address))
                .write(U256::from(500))
                .build(),
            None,
            &evm::Context {
                address: H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS),
                caller: ALICE_ECDSA.address,
                apparent_value: From::from(0),
            },
            &BASE_APP.lock().unwrap().deliver_state,
        ),
        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: EvmDataWriter::new().write(true).build(),
            cost: GAS_APPROVE + 1756,
            logs: LogsBuilder::new(H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS))
                .log3(
                    APPROVAL_EVENT_SELECTOR,
                    ALICE_ECDSA.address,
                    BOB_ECDSA.address,
                    EvmDataWriter::new().write(U256::from(500)).build(),
                )
                .build(),
        })
    );
}

fn allowance_works() {
    allowance(ALICE_ECDSA.address, BOB_ECDSA.address, U256::from(500));
}

fn allowance(owner: H160, spender: H160, expected_value: U256) {
    assert_eq!(
        FRC20::<BaseApp>::execute(
            &EvmDataWriter::new()
                .write_selector(Call::Allowance)
                .write(Address(owner))
                .write(Address(spender))
                .build(),
            None,
            &evm::Context {
                address: H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS),
                caller: ALICE_ECDSA.address,
                apparent_value: From::from(0),
            },
            &BASE_APP.lock().unwrap().deliver_state,
        ),
        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: EvmDataWriter::new().write(expected_value).build(),
            cost: GAS_ALLOWANCE,
            logs: Default::default(),
        })
    );
}

fn transfer_from_works() {
    assert_eq!(
        FRC20::<BaseApp>::execute(
            &EvmDataWriter::new()
                .write_selector(Call::TransferFrom)
                .write(Address(ALICE_ECDSA.address))
                .write(Address(BOB_ECDSA.address))
                .write(U256::from(400))
                .build(),
            None,
            &evm::Context {
                address: H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS),
                caller: BOB_ECDSA.address,
                apparent_value: From::from(0),
            },
            &BASE_APP.lock().unwrap().deliver_state,
        ),
        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            output: EvmDataWriter::new().write(true).build(),
            cost: GAS_TRANSFER_FROM + 1756 * 2,
            logs: LogsBuilder::new(H160::from_low_u64_be(FRC20_PRECOMPILE_ADDRESS))
                .log3(
                    TRANSFER_EVENT_SELECTOR,
                    ALICE_ECDSA.address,
                    BOB_ECDSA.address,
                    EvmDataWriter::new().write(U256::from(400)).build(),
                )
                .log3(
                    APPROVAL_EVENT_SELECTOR,
                    ALICE_ECDSA.address,
                    BOB_ECDSA.address,
                    EvmDataWriter::new().write(U256::from(100)).build(),
                )
                .build(),
        })
    );

    balance_of(ALICE_ECDSA.address, U256::from(200));

    balance_of(BOB_ECDSA.address, U256::from(1800));

    allowance(ALICE_ECDSA.address, BOB_ECDSA.address, U256::from(100));
}
