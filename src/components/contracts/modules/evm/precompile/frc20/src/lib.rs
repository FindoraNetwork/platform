#[cfg(test)]
mod tests;

use config::abci::global_cfg::CFG;
use core::marker::PhantomData;
use ethereum_types::{H160, U256};
use evm::backend::Log;
use evm::{
    executor::stack::{PrecompileFailure, PrecompileHandle, PrecompileOutput},
    Context, ExitSucceed,
};
use evm_precompile_utils::{
    error, Address, EvmDataReader, EvmDataWriter, EvmResult, Gasometer, LogsBuilder,
};
use fp_traits::{account::AccountAsset, evm::AddressMapping};
use module_evm::{
    precompile::{FinState, Precompile, PrecompileId, PrecompileResult},
    Config,
};
use slices::u8_slice;
use tracing::debug;

/// FRC20 transfer event selector, Keccak256("Transfer(address,address,uint256)")
///
/// event Transfer(address indexed from, address indexed to, uint256 value);
pub const TRANSFER_EVENT_SELECTOR: &[u8; 32] =
    u8_slice!("0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef");

/// FRC20 approval event selector, Keccak256("Approval(address,address,uint256)")
///
/// event Approval(address indexed owner, address indexed spender, uint256 value);
pub const APPROVAL_EVENT_SELECTOR: &[u8; 32] =
    u8_slice!("0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925");

/// b"Findora"
pub const FRC20_NAME: &[u8; 96] = u8_slice!(
    "0x00000000000000000000000000000000000000000000000000000000000000200000000000000\
    00000000000000000000000000000000000000000000000000746696e646f7261000000000000000\
    00000000000000000000000000000000000"
);

/// b"FRA"
pub const FRC20_SYMBOL: &[u8; 96] = u8_slice!(
    "0x00000000000000000000000000000000000000000000000000000000000000200000000000000\
    00000000000000000000000000000000000000000000000000346524100000000000000000000000\
    00000000000000000000000000000000000"
);

// The gas used value is obtained according to the standard erc20 call.
// https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v4.3.2/contracts/token/ERC20/ERC20.sol
const GAS_NAME: u64 = 3283;
const GAS_SYMBOL: u64 = 3437;
const GAS_DECIMALS: u64 = 243;
const GAS_TOTAL_SUPPLY: u64 = 1003;
const GAS_BALANCE_OF: u64 = 1350;
const GAS_TRANSFER: u64 = 23661;
const GAS_ALLOWANCE: u64 = 1624;
const GAS_APPROVE: u64 = 20750;
const GAS_TRANSFER_FROM: u64 = 6610;

pub struct FRC20<C> {
    _marker: PhantomData<C>,
}

impl<C: Config> PrecompileId for FRC20<C> {
    fn contract_id() -> u64 {
        0x1000
    }
}

#[evm_precompile_utils::generate_function_selector]
#[derive(Debug, PartialEq, Eq, num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
pub enum Call {
    Name = "name()",
    Symbol = "symbol()",
    Decimals = "decimals()",
    TotalSupply = "totalSupply()",
    BalanceOf = "balanceOf(address)",
    Transfer = "transfer(address,uint256)",
    Allowance = "allowance(address,address)",
    Approve = "approve(address,uint256)",
    TransferFrom = "transferFrom(address,address,uint256)",
}

impl<C: Config> Precompile for FRC20<C> {
    fn execute(
        handle: &mut impl PrecompileHandle,
        state: &FinState,
    ) -> PrecompileResult {
        if CFG.checkpoint.disable_delegate_frc20 < state.header.height {
            let context = handle.context();
            let addr = context.address;
            if addr != H160::from_low_u64_be(Self::contract_id()) {
                return Err(PrecompileFailure::Error {
                    exit_status: error("No delegatecall support"),
                });
            }
        }
        let input = handle.input();
        let target_gas = handle.gas_limit();
        let context = handle.context();

        let mut input = EvmDataReader::new(input);
        let selector = match input.read_selector::<Call>() {
            Ok(v) => v,
            Err(e) => {
                return Err(PrecompileFailure::Error { exit_status: e });
            }
        };

        match &selector {
            Call::Name => match Self::name(input, target_gas) {
                Ok(v) => {
                    handle.record_cost(v.1)?;
                    for log in v.2 {
                        handle.log(log.address, log.topics, log.data)?;
                    }
                    Ok(v.0)
                }
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::Symbol => match Self::symbol(input, target_gas) {
                Ok(v) => {
                    handle.record_cost(v.1)?;
                    for log in v.2 {
                        handle.log(log.address, log.topics, log.data)?;
                    }
                    Ok(v.0)
                }
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::Decimals => match Self::decimals(input, target_gas) {
                Ok(v) => {
                    handle.record_cost(v.1)?;
                    for log in v.2 {
                        handle.log(log.address, log.topics, log.data)?;
                    }
                    Ok(v.0)
                }
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::TotalSupply => match Self::total_supply(state, input, target_gas) {
                Ok(v) => {
                    handle.record_cost(v.1)?;
                    for log in v.2 {
                        handle.log(log.address, log.topics, log.data)?;
                    }
                    Ok(v.0)
                }
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::BalanceOf => match Self::balance_of(state, input, target_gas) {
                Ok(v) => {
                    handle.record_cost(v.1)?;
                    for log in v.2 {
                        handle.log(log.address, log.topics, log.data)?;
                    }
                    Ok(v.0)
                }
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::Allowance => match Self::allowance(state, input, target_gas) {
                Ok(v) => {
                    handle.record_cost(v.1)?;
                    for log in v.2 {
                        handle.log(log.address, log.topics, log.data)?;
                    }
                    Ok(v.0)
                }
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::Approve => match Self::approve(state, input, target_gas, context) {
                Ok(v) => {
                    handle.record_cost(v.1)?;
                    for log in v.2 {
                        handle.log(log.address, log.topics, log.data)?;
                    }
                    Ok(v.0)
                }
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::Transfer => match Self::transfer(state, input, target_gas, context) {
                Ok(v) => {
                    handle.record_cost(v.1)?;
                    for log in v.2 {
                        handle.log(log.address, log.topics, log.data)?;
                    }
                    Ok(v.0)
                }
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::TransferFrom => {
                match Self::transfer_from(state, input, target_gas, context) {
                    Ok(v) => {
                        handle.record_cost(v.1)?;
                        for log in v.2 {
                            handle.log(log.address, log.topics, log.data)?;
                        }
                        Ok(v.0)
                    }
                    Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
                }
            }
        }
    }
}

impl<C: Config> FRC20<C> {
    /// Returns the name of the token.
    fn name(
        input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<(PrecompileOutput, u64, Vec<Log>)> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_NAME)?;

        input.expect_arguments(0)?;

        debug!(target: "evm", "FRC20#name: Findora");

        let cost = gasometer.used_gas();
        let logs = vec![];

        Ok((
            PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: EvmDataWriter::new().write_raw_bytes(FRC20_NAME).build(),
            },
            cost,
            logs,
        ))
    }

    /// Returns the symbol of the token, usually a shorter version of the name.
    fn symbol(
        input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<(PrecompileOutput, u64, Vec<Log>)> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_SYMBOL)?;

        input.expect_arguments(0)?;

        debug!(target: "evm", "FRC20#symbol: FRA");

        let cost = gasometer.used_gas();
        let logs = vec![];

        Ok((
            PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: EvmDataWriter::new().write_raw_bytes(FRC20_SYMBOL).build(),
            },
            cost,
            logs,
        ))
    }

    /// Returns the number of decimals used to get its user representation.
    /// Tokens usually opt for a value of 18.
    fn decimals(
        input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<(PrecompileOutput, u64, Vec<Log>)> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_DECIMALS)?;

        input.expect_arguments(0)?;

        debug!(target: "evm", "FRC20#decimals: 18");

        let cost = gasometer.used_gas();
        let logs = vec![];

        Ok((
            PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: EvmDataWriter::new().write(18_u8).build(),
            },
            cost,
            logs,
        ))
    }

    /// Returns the amount of tokens in existence.
    fn total_supply(
        state: &FinState,
        input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<(PrecompileOutput, u64, Vec<Log>)> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_TOTAL_SUPPLY)?;

        input.expect_arguments(0)?;

        let amount: U256 = C::AccountAsset::total_issuance(state);
        debug!(target: "evm", "FRC20#total_supply: {:?}", amount);

        let cost = gasometer.used_gas();
        let logs = vec![];

        Ok((
            PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: EvmDataWriter::new().write(amount).build(),
            },
            cost,
            logs,
        ))
    }

    /// Returns the amount of tokens owned by `owner`.
    fn balance_of(
        state: &FinState,
        mut input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<(PrecompileOutput, u64, Vec<Log>)> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_BALANCE_OF)?;

        input.expect_arguments(1)?;

        let owner: H160 = input.read::<Address>()?.into();
        let owner_id = C::AddressMapping::convert_to_account_id(owner);
        let amount: U256 = C::AccountAsset::balance(state, &owner_id);
        debug!(target: "evm", "FRC20#balance_of: owner: {:?}, amount: {:?} ", owner, amount);

        let cost = gasometer.used_gas();
        let logs = vec![];

        Ok((
            PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: EvmDataWriter::new().write(amount).build(),
            },
            cost,
            logs,
        ))
    }

    /// Returns the remaining number of tokens that `spender` will be allowed to spend on behalf
    /// of `owner` through {transferFrom}. This is zero by default.
    fn allowance(
        state: &FinState,
        mut input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<(PrecompileOutput, u64, Vec<Log>)> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_ALLOWANCE)?;

        input.expect_arguments(2)?;

        let owner: H160 = input.read::<Address>()?.into();
        let owner_id = C::AddressMapping::convert_to_account_id(owner);
        let spender: H160 = input.read::<Address>()?.into();
        let spender_id = C::AddressMapping::convert_to_account_id(spender);
        let amount: U256 = C::AccountAsset::allowance(state, &owner_id, &spender_id);
        debug!(target: "evm",
            "FRC20#allowance: owner: {:?}, spender: {:?}, allowance: {:?}",
            owner, spender, amount
        );

        let cost = gasometer.used_gas();
        let logs = vec![];

        Ok((
            PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: EvmDataWriter::new().write(amount).build(),
            },
            cost,
            logs,
        ))
    }

    /// Sets `amount` as the allowance of `spender` over the caller's tokens.
    ///
    /// Returns a boolean value indicating whether the operation succeeded.
    fn approve(
        state: &FinState,
        mut input: EvmDataReader,
        target_gas: Option<u64>,
        context: &Context,
    ) -> EvmResult<(PrecompileOutput, u64, Vec<Log>)> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_APPROVE)?;
        gasometer.record_log_costs_manual(3, 32)?;

        input.expect_arguments(2)?;

        let caller = C::AddressMapping::convert_to_account_id(context.caller);
        let spender: H160 = input.read::<Address>()?.into();
        if spender == H160::zero() {
            return Err(error("FRC20: approve to the zero address"));
        }
        let spender_id = C::AddressMapping::convert_to_account_id(spender);
        let amount: U256 = input.read()?;
        debug!(target: "evm",
            "FRC20#approve: sender: {:?}, spender: {:?}, amount: {:?}",
            context.caller, spender, amount
        );

        C::AccountAsset::approve(state, &caller, &spender_id, amount)
            .map_err(|e| error(format!("{e:?}")))?;

        let cost = gasometer.used_gas();
        let logs = LogsBuilder::new(context.address)
            .log3(
                APPROVAL_EVENT_SELECTOR,
                context.caller,
                spender,
                EvmDataWriter::new().write(amount).build(),
            )
            .build();

        Ok((
            PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: EvmDataWriter::new().write(true).build(),
            },
            cost,
            logs,
        ))
    }

    /// Moves `amount` tokens from the caller's account to `recipient`.
    ///
    /// Returns a boolean value indicating whether the operation succeeded.
    fn transfer(
        state: &FinState,
        mut input: EvmDataReader,
        target_gas: Option<u64>,
        context: &Context,
    ) -> EvmResult<(PrecompileOutput, u64, Vec<Log>)> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_TRANSFER)?;
        gasometer.record_log_costs_manual(3, 32)?;

        input.expect_arguments(2)?;

        let caller = C::AddressMapping::convert_to_account_id(context.caller);
        let recipient: H160 = input.read::<Address>()?.into();
        if recipient == H160::zero() {
            return Err(error("FRC20: transfer to the zero address"));
        }
        let recipient_id = C::AddressMapping::convert_to_account_id(recipient);
        let amount: U256 = input.read()?;
        debug!(target: "evm",
            "FRC20#transfer: sender: {:?}, to: {:?}, amount: {:?}",
            context.caller, recipient, amount
        );

        C::AccountAsset::transfer(state, &caller, &recipient_id, amount)
            .map_err(|e| error(format!("{e:?}")))?;

        let cost = gasometer.used_gas();
        let logs = LogsBuilder::new(context.address)
            .log3(
                TRANSFER_EVENT_SELECTOR,
                context.caller,
                recipient,
                EvmDataWriter::new().write(amount).build(),
            )
            .build();

        Ok((
            PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: EvmDataWriter::new().write(true).build(),
            },
            cost,
            logs,
        ))
    }

    /// Moves `amount` tokens from `sender` to `recipient` using the allowance mechanism.
    /// `amount` is then deducted from the caller's allowance.
    ///
    /// Returns a boolean value indicating whether the operation succeeded.
    fn transfer_from(
        state: &FinState,
        mut input: EvmDataReader,
        target_gas: Option<u64>,
        context: &Context,
    ) -> EvmResult<(PrecompileOutput, u64, Vec<Log>)> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_TRANSFER_FROM)?;
        gasometer.record_log_costs_manual(3, 32)?;
        gasometer.record_log_costs_manual(3, 32)?;

        input.expect_arguments(3)?;

        let caller = C::AddressMapping::convert_to_account_id(context.caller);
        let from: H160 = input.read::<Address>()?.into();
        if from == H160::zero() {
            return Err(error("FRC20: transfer from the zero address"));
        }
        let from_id = C::AddressMapping::convert_to_account_id(from);
        let recipient: H160 = input.read::<Address>()?.into();
        if recipient == H160::zero() {
            return Err(error("FRC20: transfer to the zero address"));
        }
        let recipient_id = C::AddressMapping::convert_to_account_id(recipient);
        let amount: U256 = input.read()?;
        let allowance = C::AccountAsset::allowance(state, &from_id, &caller);
        if allowance < amount {
            return Err(error("FRC20: transfer amount exceeds allowance"));
        }
        debug!(target: "evm",
            "FRC20#transfer_from: sender: {:?}, from: {:?}, to: {:?}, amount: {:?}",
            context.caller, from, recipient, amount
        );

        C::AccountAsset::transfer(state, &from_id, &recipient_id, amount)
            .map_err(|e| error(format!("{e:?}")))?;

        C::AccountAsset::approve(
            state,
            &from_id,
            &caller,
            allowance.saturating_sub(amount),
        )
        .map_err(|e| error(format!("{e:?}")))?;

        let cost = gasometer.used_gas();
        let logs = LogsBuilder::new(context.address)
            .log3(
                TRANSFER_EVENT_SELECTOR,
                from,
                recipient,
                EvmDataWriter::new().write(amount).build(),
            )
            .log3(
                APPROVAL_EVENT_SELECTOR,
                from,
                context.caller,
                EvmDataWriter::new()
                    .write(allowance.saturating_sub(amount))
                    .build(),
            )
            .build();

        Ok((
            PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: EvmDataWriter::new().write(true).build(),
            },
            cost,
            logs,
        ))
    }
}
