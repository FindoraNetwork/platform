#![deny(warnings)]
#![allow(missing_docs)]

mod basic;
mod impls;

use abci::{RequestQuery, ResponseQuery};
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
    use fp_types::actions::xhub::{NonConfidentialOutput, TxOutput};
    use ledger::data_model::AssetType as FindoraAsset;
    use primitive_types::H160;

    use fp_storage::*;

    // The following data is stored in non-state rocksdb
    // account balance transfer to utxo waiting to be mint.
    generate_storage!(XHub, PendingUTXOs => Value<Vec<NonConfidentialOutput>>);
    generate_storage!(Xhub, PendingUtxos2 => Value<Vec<TxOutput>>);

    // Findora custom asset associate with a Ethereum address
    generate_storage!(XHub, FindoraAssets => Map<H160, FindoraAsset>);
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

impl<C: Config> AppModule for App<C> {
    fn query_route(
        &self,
        ctx: Context,
        path: Vec<&str>,
        req: &RequestQuery,
    ) -> ResponseQuery {
        let mut resp = ResponseQuery::default();

        if path.len() != 1 {
            resp.code = 1;
            resp.log = format!("empty query path for {}", MODULE_NAME);
            return resp;
        }

        match path[0] {
            "info" => {
                if let Ok(address) = serde_json::from_slice(req.data.as_slice()) {
                    if let Some(asset) = Self::asset_of(&ctx, &address) {
                        resp.value = match serde_json::to_vec(&asset) {
                            Ok(value) => value,
                            Err(e) => {
                                resp.code = 2;
                                resp.log = format!("Internal error: {}", e.to_string());
                                return resp;
                            }
                        }
                    } else {
                        resp.code = 3;
                        resp.log = String::from("Not existed address")
                    }
                } else {
                    resp.code = 4;
                    resp.log = String::from("Invalid contract address");
                }
            }
            "unfinished_txs" => {
                resp.code = 5;
                resp.log = String::from("Internal error to fetch unfinished txs");
            }
            "contracts" => {
                if let Ok(value) = serde_json::to_vec(Self::assets(&ctx).as_slice()) {
                    resp.value = value;
                } else {
                    resp.code = 6;
                    resp.log = String::from("Internal error to fetch contracts");
                }
            }
            _ => {
                resp.code = 99;
                resp.log = format!("Invalid path for {}", MODULE_NAME);
            }
        }

        resp
    }
}

impl<C: Config> Executable for App<C> {
    type Origin = Address;
    type Call = Action;

    fn execute(
        origin: Option<Self::Origin>,
        call: Self::Call,
        ctx: &Context,
    ) -> Result<ActionResult> {
        let sender = if let Some(sender) = origin {
            sender
        } else {
            log::error!(target: "xhub", "invalid transaction origin");
            return Err(eg!("invalid transaction origin"));
        };

        match call {
            Action::NonConfidentialTransfer(action) => {
                Self::transfer_to_nonconfidential_utxo(ctx, sender, action)
            }
            Action::Erc20ToUtxo(action) => {
                Self::erc20_to_utxo(ctx, action.contract, action.outputs)
            }
        }
    }
}
