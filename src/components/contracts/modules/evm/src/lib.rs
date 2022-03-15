#![deny(warnings)]
#![allow(missing_docs)]

mod basic;
pub mod impls;
pub mod precompile;
pub mod runtime;
pub mod system_contracts;
pub mod utils;

use abci::{RequestQuery, ResponseQuery};
use config::abci::global_cfg::CFG;
use ethabi::Token;
use ethereum_types::{H160, U256};
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
    actions::{evm::Action, xhub::NonConfidentialOutput},
    crypto::{Address, HA160},
};
use precompile::PrecompileSet;
use ruc::*;
use runtime::runner::ActionRunner;
use std::marker::PhantomData;
use system_contracts::SystemContracts;

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

pub struct App<C> {
    phantom: PhantomData<C>,
    pub contracts: SystemContracts,
}

impl<C: Config> Default for App<C> {
    fn default() -> Self {
        App {
            phantom: Default::default(),
            contracts: pnk!(SystemContracts::new()),
        }
    }
}

impl<C: Config> App<C> {
    pub fn withdraw_frc20(
        &self,
        ctx: &Context,
        _asset: [u8; 32],
        _from: &Address,
        _to: &Address,
        _value: U256,
    ) -> Result<()> {
        let function = self.contracts.bridge.function("withdrawERC20").c(d!())?;

        let asset = Token::FixedBytes(Vec::from(_asset));

        let bytes: &[u8] = _from.as_ref();
        let from = Token::Address(H160::from_slice(&bytes[..20]));

        let bytes: &[u8] = _to.as_ref();
        let to = Token::Address(H160::from_slice(&bytes[..20]));

        let value = Token::Uint(_value);

        let input = function.encode_input(&[asset, from, to, value]).c(d!())?;

        let _ = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            H160::zero(),
            9999999,
            self.contracts.bridge_address,
            U256::zero(),
        )?;

        Ok(())
    }

    pub fn withdraw_fra(
        &self,
        ctx: &Context,
        _from: &Address,
        _to: &Address,
        _value: U256,
    ) -> Result<()> {
        let function = self.contracts.bridge.function("withdrawFRA").c(d!())?;

        let bytes: &[u8] = _from.as_ref();
        let from = Token::FixedBytes(bytes.to_vec());

        let bytes: &[u8] = _to.as_ref();

        let to = Token::Address(H160::from_slice(&bytes[4..24]));
        let value = Token::Uint(_value);

        println!("{:?}, {:?}, {:?}", from, to, value);

        let input = function.encode_input(&[from, to, value]).c(d!())?;

        let _ = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            H160::zero(),
            9999999,
            self.contracts.bridge_address,
            U256::zero(),
        )?;

        Ok(())
    }

    pub fn consume_mint(&self, ctx: &Context) -> Vec<NonConfidentialOutput> {
        let height = CFG.checkpoint.prismxx_inital_height;

        let mut pending_outputs = Vec::new();

        if height < ctx.header.height {
            if let Err(e) =
                utils::fetch_mint::<C>(ctx, &self.contracts, &mut pending_outputs)
            {
                log::error!("Collect mint ops error: {:?}", e);
            }
        }

        pending_outputs
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
            if let Err(e) = utils::deploy_contract::<C>(ctx, &mut self.contracts) {
                pd!(e);
                return;
            }
            println!(
                "Bridge contract address: {:?}",
                self.contracts.bridge_address
            );

            ctx.state.write().discard_session();
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
