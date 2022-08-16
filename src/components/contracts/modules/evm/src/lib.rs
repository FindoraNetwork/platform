#![deny(warnings)]
#![allow(missing_docs)]

mod basic;
pub mod impls;
pub mod precompile;
pub mod runtime;

use abci::{RequestQuery, ResponseQuery};
use ethereum_types::U256;
use fp_core::{
    context::Context,
    macros::Get,
    module::AppModule,
    transaction::{ActionResult, Executable},
};
use fp_storage::Borrow;
use fp_traits::{
    account::AccountAsset,
    evm::{AddressMapping, BlockHashMapping, DecimalsMapping, FeeCalculator},
};
use fp_types::{
    actions::evm::Action,
    crypto::{Address, HA160},
};
use precompile::PrecompileSet;
use ruc::*;
use std::marker::PhantomData;

pub use runtime::*;

pub const MODULE_NAME: &str = "evm";

pub trait Config {
    /// Account module interface to read/write account assets.
    type AccountAsset: AccountAsset<Address>;
    /// Mapping from address to account id.
    type AddressMapping: AddressMapping;
    /// The block gas limit. Can be a simple constant, or an adjustment algorithm in another pallet.
    type BlockGasLimit: Get<U256>;
    /// Block number to block hash.
    type BlockHashMapping: BlockHashMapping;
    /// Chain ID of EVM.
    type ChainId: Get<u64>;
    /// Mapping from eth decimals to native token decimals.
    type DecimalsMapping: DecimalsMapping;
    /// Calculator for current gas price.
    type FeeCalculator: FeeCalculator;
    /// Precompiles associated with this EVM engine.
    type Precompiles: PrecompileSet;
}

pub mod storage {
    use ethereum_types::H256;
    use fp_storage::*;
    use fp_types::crypto::{HA160, HA256};

    // The code corresponding to the contract account.
    generate_storage!(EVM, AccountCodes => Map<HA160, Vec<u8>>);
    // Storage root hash related to the contract account.
    generate_storage!(EVM, AccountStorages => DoubleMap<HA160, HA256, H256>);
}

#[derive(Clone)]
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
        _req: &RequestQuery,
    ) -> ResponseQuery {
        let mut resp: ResponseQuery = Default::default();
        if path.len() != 1 {
            resp.code = 1;
            resp.log = String::from("account: invalid query path");
            return resp;
        }
        match path[0] {
            "contract-number" => {
                let contracts: Vec<(HA160, Vec<u8>)> =
                    storage::AccountCodes::iterate(ctx.state.read().borrow());
                resp.value = serde_json::to_vec(&contracts.len()).unwrap_or_default();
                resp
            }
            _ => resp,
        }
    }

    fn begin_block(&mut self, ctx: &mut Context, _req: &abci::RequestBeginBlock) {
        let height = CFG.checkpoint.prismxx_inital_height;

        if ctx.header.height == height {
            let bytecode_str = include_str!("../contracts/PrismXXProxy.bytecode");

            if let Err(e) =
                utils::deploy_contract::<C>(ctx, &self.contracts, bytecode_str)
            {
                pd!(e);
                return;
            }
            println!(
                "Bridge contract address: {:?}",
                self.contracts.bridge_address
            );

            if !ctx.state.write().cache_mut().good2_commit() {
                ctx.state.write().discard_session();
                pd!(eg!("ctx state commit no good"));
            } else {
                ctx.state.write().commit_session();
            }
        }
    }
}

impl<C: Config> Executable for App<C> {
    type Origin = Address;
    type Call = Action;

    fn execute(
        _origin: Option<Self::Origin>,
        _call: Self::Call,
        _ctx: &Context,
    ) -> Result<ActionResult> {
        Err(eg!("Unsupported evm action!"))
    }
}
