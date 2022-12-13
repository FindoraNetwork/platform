use super::stack::FindoraStackState;
// use crate::precompile::PrecompileSet;
use crate::{App, Config};
use ethereum_types::{H160, H256, U256};
use evm::{
    executor::stack::{StackExecutor, StackSubstateMetadata},
    ExitReason,
};
use fp_core::{
    macros::Get2,
    {context::Context, ensure},
};
use fp_evm::*;
use fp_traits::evm::{FeeCalculator, OnChargeEVMTransaction};
use fp_types::actions::evm::*;
use ruc::*;
use sha3::{Digest, Keccak256};
use std::marker::PhantomData;

#[derive(Default)]
pub struct ActionRunner<C: Config> {
    _marker: PhantomData<C>,
}

impl<C: Config> ActionRunner<C> {
    #[allow(clippy::too_many_arguments)]
    /// Execute an EVM operation.
    pub fn execute<'config, 'precompiles, F, R>(
        ctx: &Context,
        source: H160,
        value: U256,
        gas_limit: u64,
        gas_price: Option<U256>,
        nonce: Option<U256>,
        config: &'config evm::Config,
        precompiles: &'precompiles C::PrecompilesType,
        f: F,
    ) -> Result<ExecutionInfo<R>>
    where
        F: FnOnce(
            &mut StackExecutor<
                'config,
                'precompiles,
                FindoraStackState<'_, '_, 'config, C>,
                C::PrecompilesType,
            >,
        ) -> (ExitReason, R),
    {
        let min_gas_price = C::FeeCalculator::min_gas_price(ctx.header.height as u64);

        // Gas price check is skipped when performing a gas estimation.
        let gas_price = match gas_price {
            Some(gas_price) => {
                ensure!(gas_price >= min_gas_price, "GasPriceTooLow");
                gas_price
            }
            None => Default::default(),
        };

        let vicinity = Vicinity {
            gas_price,
            origin: source,
        };

        let metadata = StackSubstateMetadata::new(gas_limit, config);
        let state = FindoraStackState::new(ctx, &vicinity, metadata);
        let mut executor =
            StackExecutor::new_with_precompiles(state, config, precompiles);

        let total_fee = gas_price
            .checked_mul(U256::from(gas_limit))
            .ok_or(eg!("FeeOverflow"))?;
        let total_payment =
            value.checked_add(total_fee).ok_or(eg!("PaymentOverflow"))?;
        let source_account = App::<C>::account_basic(ctx, &source);

        if let Some(nonce) = nonce {
            ensure!(
                source_account.nonce == nonce,
                format!(
                    "InvalidNonce, expected: {}, actual: {}",
                    source_account.nonce, nonce
                )
            );
        }

        if !config.estimate {
            ensure!(source_account.balance >= total_payment, "BalanceLow");

            // Deduct fee from the `source` account.
            App::<C>::withdraw_fee(ctx, &source, total_fee)?;
        }

        // Execute the EVM call.
        let (reason, retv) = f(&mut executor);

        let used_gas = U256::from(executor.used_gas());
        let actual_fee = executor.fee(gas_price);
        tracing::debug!(
            target: "evm",
            "Execution {:?} [source: {:?}, value: {}, gas_price {}, gas_limit: {}, actual_fee: {}]",
            reason,
            source,
            value,
            gas_price,
            gas_limit,
            actual_fee
        );

        if !config.estimate {
            // Refund fees to the `source` account if deducted more before,
            App::<C>::correct_and_deposit_fee(ctx, &source, actual_fee, total_fee)?;
        }

        let state = executor.into_state();

        for address in state.substate.deletes {
            tracing::debug!(
                target: "evm",
                "Deleting account at {:?}",
                address
            );
            App::<C>::remove_account(ctx, &address.into())
        }

        for log in &state.substate.logs {
            tracing::trace!(
                target: "evm",
                "Inserting log for {:?}, topics ({}) {:?}, data ({}): {:?}]",
                log.address,
                log.topics.len(),
                log.topics,
                log.data.len(),
                log.data
            );
        }

        Ok(ExecutionInfo {
            value: retv,
            exit_reason: reason,
            used_gas,
            logs: state.substate.logs,
        })
    }

    // eip1559 support
    pub fn execute_eip1559<'config, 'precompiles, F, R>(
        ctx: &Context,
        source: H160,
        value: U256,
        gas_limit: u64,
        max_fee_per_gas: Option<U256>,
        max_priority_fee_per_gas: Option<U256>,
        nonce: Option<U256>,
        config: &'config evm::Config,
        precompiles: &'precompiles C::PrecompilesType,
        f: F,
    ) -> Result<ExecutionInfo<R>>
    where
        F: FnOnce(
            &mut StackExecutor<
                'config,
                'precompiles,
                FindoraStackState<'_, '_, 'config, C>,
                C::PrecompilesType,
            >,
        ) -> (ExitReason, R),
    {
        let base_fee = C::FeeCalculator::min_gas_price(ctx.header.height as u64);
        // Gas price check is skipped when performing a gas estimation.
        let max_fee_per_gas = match max_fee_per_gas {
            Some(max_fee_per_gas) => {
                ensure!(max_fee_per_gas >= base_fee, "GasPriceTooLow");
                max_fee_per_gas
            }
            None => Default::default(),
        };

        let vicinity = Vicinity {
            gas_price: max_fee_per_gas,
            origin: source,
        };

        let metadata = StackSubstateMetadata::new(gas_limit, config);
        let state = FindoraStackState::<C>::new(ctx, &vicinity, metadata);
        let mut executor =
            StackExecutor::new_with_precompiles(state, config, precompiles);

        // After eip-1559 we make sure the account can pay both the evm execution and priority fees.
        let max_base_fee = max_fee_per_gas
            .checked_mul(U256::from(gas_limit))
            .ok_or(eg!("FeeOverflow"))?;
        let max_priority_fee = if let Some(max_priority_fee) = max_priority_fee_per_gas {
            max_priority_fee
                .checked_mul(U256::from(gas_limit))
                .ok_or(eg!("FeeOverflow"))?
        } else {
            U256::zero()
        };

        let total_fee = max_base_fee
            .checked_add(max_priority_fee)
            .ok_or(eg!("FeeOverflow"))?;

        let total_payment =
            value.checked_add(total_fee).ok_or(eg!("PaymentOverflow"))?;
        let source_account = App::<C>::account_basic(ctx, &source);

        if let Some(nonce) = nonce {
            ensure!(
                source_account.nonce == nonce,
                format!(
                    "InvalidNonce, expected: {}, actual: {}",
                    source_account.nonce, nonce
                )
            );
        }
        if !config.estimate {
            ensure!(source_account.balance >= total_payment, "BalanceLow");

            // Deduct fee from the `source` account.
            App::<C>::withdraw_fee(ctx, &source, total_fee)?;
        }

        // Execute the EVM call.
        let (reason, retv) = f(&mut executor);

        let used_gas = U256::from(executor.used_gas());
        let (actual_fee, actual_priority_fee) =
            if let Some(max_priority_fee) = max_priority_fee_per_gas {
                let actual_priority_fee = max_priority_fee
                    .checked_mul(used_gas)
                    .ok_or(eg!("FeeOverflow"))?;
                let actual_fee = executor
                    .fee(base_fee)
                    .checked_add(actual_priority_fee)
                    .unwrap_or_else(U256::max_value);
                (actual_fee, Some(actual_priority_fee))
            } else {
                (executor.fee(base_fee), None)
            };
        tracing::trace!(
            target: "evm",
            "Execution {:?} [source: {:?}, value: {}, gas_limit: {}, actual_fee: {}]",
            reason,
            source,
            value,
            gas_limit,
            actual_fee
        );
        // The difference between initially withdrawn and the actual cost is refunded.
        //
        // Considered the following request:
        // +-----------+---------+--------------+
        // | Gas_limit | Max_Fee | Max_Priority |
        // +-----------+---------+--------------+
        // |        20 |      10 |            6 |
        // +-----------+---------+--------------+
        //
        // And execution:
        // +----------+----------+
        // | Gas_used | Base_Fee |
        // +----------+----------+
        // |        5 |        2 |
        // +----------+----------+
        //
        // Initially withdrawn (10 + 6) * 20 = 320.
        // Actual cost (2 + 6) * 5 = 40.
        // Refunded 320 - 40 = 280.
        // Tip 5 * 6 = 30.
        // Burned 320 - (280 + 30) = 10. Which is equivalent to gas_used * base_fee.
        if !config.estimate {
            App::<C>::correct_and_deposit_fee(ctx, &source, actual_fee, total_fee)?;
            if let Some(actual_priority_fee) = actual_priority_fee {
                App::<C>::pay_priority_fee(ctx, actual_priority_fee)?;
            }
        }

        let state = executor.into_state();

        for address in state.substate.deletes {
            tracing::trace!(
                target: "evm",
                "Deleting account at {:?}",
                address
            );
            App::<C>::remove_account(ctx, &address.into())
        }

        for log in &state.substate.logs {
            tracing::trace!(
                target: "evm",
                "Inserting log for {:?}, topics ({}) {:?}, data ({}): {:?}]",
                log.address,
                log.topics.len(),
                log.topics,
                log.data.len(),
                log.data
            );
        }

        Ok(ExecutionInfo {
            value: retv,
            exit_reason: reason,
            used_gas,
            logs: state.substate.logs,
        })
    }

    pub fn inital_system_contract(
        ctx: &Context,
        bytecode: Vec<u8>,
        gas_limit: u64,
        source: H160,
        salt: H256,
    ) -> Result<()> {
        let config = evm::Config::istanbul();

        let vicinity = Vicinity {
            gas_price: U256::zero(),
            origin: source,
        };
        let metadata = StackSubstateMetadata::new(gas_limit, &config);
        let state = FindoraStackState::<C>::new(ctx, &vicinity, metadata);

        let precompiles = C::PrecompilesValue::get(ctx.clone());
        let mut executor =
            StackExecutor::new_with_precompiles(state, &config, &precompiles);

        let access_list = Vec::new();
        let result = executor.transact_create2(
            source,
            U256::zero(),
            bytecode,
            salt,
            gas_limit,
            access_list,
        );

        let state = executor.into_state();

        for address in state.substate.deletes {
            tracing::debug!(
                target: "evm",
                "Deleting account at {:?}",
                address
            );
            App::<C>::remove_account(ctx, &address.into())
        }

        for log in &state.substate.logs {
            tracing::trace!(
                target: "evm",
                "Inserting log for {:?}, topics ({}) {:?}, data ({}): {:?}]",
                log.address,
                log.topics.len(),
                log.topics,
                log.data.len(),
                log.data
            );
        }

        if let ExitReason::Succeed(_) = result.0 {
            Ok(())
        } else {
            Err(eg!("Deploy system error: {:?}", result.0))
        }
    }

    pub fn execute_systemc_contract(
        ctx: &Context,
        input: Vec<u8>,
        source: H160,
        gas_limit: u64,
        target: H160,
        value: U256,
    ) -> Result<(Vec<u8>, Vec<Log>, U256)> {
        let config = evm::Config::istanbul();

        let vicinity = Vicinity {
            gas_price: U256::one(),
            origin: source,
        };
        let metadata = StackSubstateMetadata::new(gas_limit, &config);
        let state = FindoraStackState::<C>::new(ctx, &vicinity, metadata);

        let precompiles = C::PrecompilesValue::get(ctx.clone());
        let mut executor =
            StackExecutor::new_with_precompiles(state, &config, &precompiles);

        let access_list = Vec::new();
        let (result, data) =
            executor.transact_call(source, target, value, input, gas_limit, access_list);

        let gas_used = U256::from(executor.used_gas());
        let state = executor.into_state();

        for address in state.substate.deletes {
            tracing::debug!(
                target: "evm",
                "Deleting account at {:?}",
                address
            );
            App::<C>::remove_account(ctx, &address.into())
        }

        for log in &state.substate.logs {
            tracing::trace!(
                target: "evm",
                "Inserting log for {:?}, topics ({}) {:?}, data ({}): {:?}]",
                log.address,
                log.topics.len(),
                log.topics,
                log.data.len(),
                log.data
            );
        }

        if let ExitReason::Succeed(_) = result {
            Ok((data, state.substate.logs, gas_used))
        } else {
            // TODO: store error execution on pending tx later.
            Err(eg!(
                "Execute system error: {:?}, data is: {}",
                result,
                hex::encode(data)
            ))
        }
    }
}

impl<C: Config> Runner for ActionRunner<C> {
    fn call(ctx: &Context, args: Call, config: &evm::Config) -> Result<CallInfo> {
        let precompiles = C::PrecompilesValue::get(ctx.clone());
        let access_list = Vec::new();

        Self::execute(
            ctx,
            args.source,
            args.value,
            args.gas_limit,
            args.gas_price,
            args.nonce,
            config,
            &precompiles,
            |executor| {
                executor.transact_call(
                    args.source,
                    args.target,
                    args.value,
                    args.input,
                    args.gas_limit,
                    access_list,
                )
            },
        )
    }

    fn create(ctx: &Context, args: Create, config: &evm::Config) -> Result<CreateInfo> {
        let precompiles = C::PrecompilesValue::get(ctx.clone());
        let access_list = Vec::new();

        Self::execute(
            ctx,
            args.source,
            args.value,
            args.gas_limit,
            args.gas_price,
            args.nonce,
            config,
            &precompiles,
            |executor| {
                let address = executor.create_address(evm::CreateScheme::Legacy {
                    caller: args.source,
                });

                (
                    executor
                        .transact_create(
                            args.source,
                            args.value,
                            args.init,
                            args.gas_limit,
                            access_list,
                        )
                        .0,
                    address,
                )
            },
        )
    }

    fn create2(
        ctx: &Context,
        args: Create2,
        config: &evm::Config,
    ) -> Result<CreateInfo> {
        let code_hash = H256::from_slice(Keccak256::digest(&args.init).as_slice());
        let precompiles = C::PrecompilesValue::get(ctx.clone());
        let access_list = Vec::new();

        Self::execute(
            ctx,
            args.source,
            args.value,
            args.gas_limit,
            args.gas_price,
            args.nonce,
            config,
            &precompiles,
            |executor| {
                let address = executor.create_address(evm::CreateScheme::Create2 {
                    caller: args.source,
                    code_hash,
                    salt: args.salt,
                });
                (
                    executor
                        .transact_create2(
                            args.source,
                            args.value,
                            args.init,
                            args.salt,
                            args.gas_limit,
                            access_list,
                        )
                        .0,
                    address,
                )
            },
        )
    }

    fn call_eip1559(
        ctx: &Context,
        args: CallEip1559,
        config: &evm::Config,
    ) -> Result<CallInfo> {
        let precompiles = C::PrecompilesValue::get(ctx.clone());
        let access_list = Vec::new();
        Self::execute_eip1559(
            ctx,
            args.source,
            args.value,
            args.gas_limit,
            args.max_fee_per_gas,
            args.max_priority_fee_per_gas,
            args.nonce,
            config,
            &precompiles,
            |executor| {
                executor.transact_call(
                    args.source,
                    args.target,
                    args.value,
                    args.input,
                    args.gas_limit,
                    access_list,
                )
            },
        )
    }

    fn create_eip1559(
        ctx: &Context,
        args: CreateEip1559,
        config: &evm::Config,
    ) -> Result<CreateInfo> {
        let precompiles = C::PrecompilesValue::get(ctx.clone());
        let access_list = Vec::new();
        Self::execute_eip1559(
            ctx,
            args.source,
            args.value,
            args.gas_limit,
            args.max_fee_per_gas,
            args.max_priority_fee_per_gas,
            args.nonce,
            config,
            &precompiles,
            |executor| {
                let address = executor.create_address(evm::CreateScheme::Legacy {
                    caller: args.source,
                });
                (
                    executor
                        .transact_create(
                            args.source,
                            args.value,
                            args.init,
                            args.gas_limit,
                            access_list,
                        )
                        .0,
                    address,
                )
            },
        )
    }

    fn create2_eip1559(
        ctx: &Context,
        args: Create2Eip1559,
        config: &evm::Config,
    ) -> Result<CreateInfo> {
        let code_hash = H256::from_slice(Keccak256::digest(&args.init).as_slice());
        let precompiles = C::PrecompilesValue::get(ctx.clone());
        let access_list = Vec::new();
        Self::execute_eip1559(
            ctx,
            args.source,
            args.value,
            args.gas_limit,
            args.max_fee_per_gas,
            args.max_priority_fee_per_gas,
            args.nonce,
            config,
            &precompiles,
            |executor| {
                let address = executor.create_address(evm::CreateScheme::Create2 {
                    caller: args.source,
                    code_hash,
                    salt: args.salt,
                });
                (
                    executor
                        .transact_create2(
                            args.source,
                            args.value,
                            args.init,
                            args.salt,
                            args.gas_limit,
                            access_list,
                        )
                        .0,
                    address,
                )
            },
        )
    }
}
