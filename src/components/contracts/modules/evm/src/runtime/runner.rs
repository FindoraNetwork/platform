use super::stack::FindoraStackState;
use crate::{App, Config};
use ethereum_types::{H160, H256, U256};
use evm::{
    executor::{StackExecutor, StackSubstateMetadata},
    ExitReason,
};
use fp_core::{context::Context, ensure};
use fp_evm::*;
use fp_traits::evm::{DecimalsMapping, FeeCalculator, OnChargeEVMTransaction};
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
    pub fn execute<'config, F, R>(
        ctx: &Context,
        source: H160,
        value: U256,
        gas_limit: u64,
        gas_price: Option<U256>,
        nonce: Option<U256>,
        config: &'config evm::Config,
        f: F,
    ) -> Result<ExecutionInfo<R>>
    where
        F: FnOnce(
            &mut StackExecutor<'config, FindoraStackState<'_, '_, 'config, C>>,
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
            StackExecutor::new_with_precompile(state, config, C::Precompiles::execute);

        let total_fee = gas_price
            .checked_mul(U256::from(gas_limit))
            .ok_or(eg!("FeeOverflow"))?;
        let total_fee = C::DecimalsMapping::convert_to_native_token(total_fee);
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
        log::debug!(
            target: "evm",
            "Execution {:?} [source: {:?}, value: {}, gas_limit: {}, actual_fee: {}]",
            reason,
            source,
            value,
            gas_limit,
            actual_fee
        );

        if !config.estimate {
            let actual_fee = C::DecimalsMapping::convert_to_native_token(actual_fee);
            // Refund fees to the `source` account if deducted more before,
            App::<C>::correct_and_deposit_fee(ctx, &source, actual_fee, total_fee)?;
        }

        let state = executor.into_state();

        for address in state.substate.deletes {
            log::debug!(
                target: "evm",
                "Deleting account at {:?}",
                address
            );
            App::<C>::remove_account(ctx, &address)
        }

        for log in &state.substate.logs {
            log::trace!(
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
}

impl<C: Config> Runner for ActionRunner<C> {
    fn call(ctx: &Context, args: Call, config: &evm::Config) -> Result<CallInfo> {
        Self::execute(
            ctx,
            args.source,
            args.value,
            args.gas_limit,
            args.gas_price,
            args.nonce,
            config,
            |executor| {
                executor.transact_call(
                    args.source,
                    args.target,
                    args.value,
                    args.input,
                    args.gas_limit,
                )
            },
        )
    }

    fn create(ctx: &Context, args: Create, config: &evm::Config) -> Result<CreateInfo> {
        Self::execute(
            ctx,
            args.source,
            args.value,
            args.gas_limit,
            args.gas_price,
            args.nonce,
            config,
            |executor| {
                let address = executor.create_address(evm::CreateScheme::Legacy {
                    caller: args.source,
                });
                (
                    executor.transact_create(
                        args.source,
                        args.value,
                        args.init,
                        args.gas_limit,
                    ),
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
        Self::execute(
            ctx,
            args.source,
            args.value,
            args.gas_limit,
            args.gas_price,
            args.nonce,
            config,
            |executor| {
                let address = executor.create_address(evm::CreateScheme::Create2 {
                    caller: args.source,
                    code_hash,
                    salt: args.salt,
                });
                (
                    executor.transact_create2(
                        args.source,
                        args.value,
                        args.init,
                        args.salt,
                        args.gas_limit,
                    ),
                    address,
                )
            },
        )
    }
}
