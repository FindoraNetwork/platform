#![deny(warnings)]
#![allow(missing_docs)]

mod basic;
mod impls;

use evm::Config as EvmConfig;
use fp_core::{
    context::Context,
    macros::Get,
    module::AppModule,
    transaction::{ActionResult, Executable},
};
use fp_evm::Runner;
use fp_traits::{
    account::{AccountAsset, FeeCalculator},
    evm::DecimalsMapping,
};
use fp_types::{actions::xhub::Action, crypto::Address};
use primitive_types::U256;
use ruc::*;
use std::marker::PhantomData;

pub const MODULE_NAME: &str = "xhub";
static ISTANBUL_CONFIG: EvmConfig = EvmConfig::istanbul();

pub trait Config {
    /// Account module interface to read/write account assets.
    type AccountAsset: AccountAsset<Address>;
    /// Mapping from eth decimals to native token decimals.
    type DecimalsMapping: DecimalsMapping;
    /// The block gas limit. Can be a simple constant, or an adjustment algorithm in another pallet.
    type BlockGasLimit: Get<U256>;
    /// Calculator for current gas price.
    type FeeCalculator: FeeCalculator;
    /// EVM execution runner.
    type Runner: Runner;
    /// EVM config used in the module.
    fn config() -> &'static EvmConfig {
        &ISTANBUL_CONFIG
    }
}

mod storage {
    use fp_types::actions::xhub::NonConfidentialOutput;

    use fp_storage::*;

    // The following data is stored in non-state rocksdb
    // account balance transfer to utxo waiting to be mint.
    generate_storage!(XHub, PendingUTXOs => Value<Vec<NonConfidentialOutput>>);
}

pub struct App<C> {
    phantom: PhantomData<C>,
}

impl<C: Config> Default for App<C> {
    fn default() -> Self {
        App {
            phantom: Default::default(),
        }
    }
}

impl<C: Config> AppModule for App<C> {}

impl<C: Config> Executable for App<C> {
    type Origin = Address;
    type Call = Action;

    fn execute(
        origin: Option<Self::Origin>,
        call: Self::Call,
        ctx: &Context,
    ) -> Result<ActionResult> {
        match call {
            Action::NonConfidentialTransfer(action) => {
                if let Some(sender) = origin {
                    Self::transfer_to_nonconfidential_utxo(ctx, sender, action)
                } else {
                    Err(eg!("invalid transaction origin"))
                }
            }
            Action::ERC20ToUTXO(action) => {
                if let Some(sender) = origin {
                    Self::erc20_to_utxo(
                        ctx,
                        sender,
                        action.contractaddress,
                        action.input,
                        action.nonce,
                        action.outputs,
                    )
                } else {
                    Err(eg!("invalid transaction origin"))
                }
            }
        }
    }
}
