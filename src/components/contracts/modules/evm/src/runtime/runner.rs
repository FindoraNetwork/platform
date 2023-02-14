use super::stack::FindoraStackState;
// use crate::precompile::PrecompileSet;
use crate::{App, Config};
use ethereum_types::{H160, H256, U256};
use evm::{
    executor::stack::{StackExecutor, StackSubstateMetadata},
    ExitReason,
};
use fp_core::{context::Context, ensure, macros::Get2};
use fp_evm::*;
use fp_traits::evm::{FeeCalculator, OnChargeEVMTransaction};
use fp_types::actions::evm::*;
use ruc::*;
use sha3::{Digest, Keccak256};
use std::marker::PhantomData;
use tracing::{debug, trace};

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
        // Gas price check is skipped when performing a gas estimation.
        let gas_price = match gas_price {
            Some(gas_price) => {
                ensure!(
                    gas_price >= C::FeeCalculator::min_gas_price(),
                    "GasPriceTooLow"
                );
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
            #[cfg(not(feature = "benchmark"))]
            ensure!(
                source_account.nonce == nonce,
                format!(
                    "InvalidNonce, expected: {}, actual: {}",
                    source_account.nonce, nonce
                )
            );
            let _ = nonce;
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
        debug!(
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
            debug!(
                target: "evm",
                "Deleting account at {:?}",
                address
            );
            App::<C>::remove_account(ctx, &address.into())
        }

        for log in &state.substate.logs {
            trace!(
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
            debug!(
                target: "evm",
                "Deleting account at {:?}",
                address
            );
            App::<C>::remove_account(ctx, &address.into())
        }

        for log in &state.substate.logs {
            trace!(
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
}
