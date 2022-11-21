#![deny(warnings)]
#![allow(missing_docs)]
#![allow(clippy::too_many_arguments)]

extern crate core;

mod basic;
pub mod impls;
pub mod precompile;
pub mod runtime;
pub mod system_contracts;
pub mod utils;

use abci::{RequestQuery, ResponseQuery};
use config::abci::global_cfg::CFG;
use ethabi::Token;
use ethereum_types::{Bloom, BloomInput, H160, H256, U256};
use fp_core::{
    context::Context,
    macros::Get,
    macros::Get2,
    module::AppModule,
    transaction::{ActionResult, Executable},
};
use fp_storage::Borrow;
use fp_traits::{
    account::AccountAsset,
    evm::{AddressMapping, BlockHashMapping, DecimalsMapping, FeeCalculator},
};

use fp_evm::TransactionStatus;

use evm::executor::stack::PrecompileSet as EvmPrecompileSet;

use ethereum::{
    Log, ReceiptV0 as Receipt, TransactionAction, TransactionSignature, TransactionV0,
};

use fp_types::{
    actions::{evm::Action, xhub::NonConfidentialOutput},
    crypto::{Address, HA160},
};
use noah::xfr::sig::XfrPublicKey;
use noah_algebra::serialization::NoahFromToBytes;
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
    type PrecompilesType: EvmPrecompileSet;
    type PrecompilesValue: Get2<Self::PrecompilesType, Context>;
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
        from: &XfrPublicKey,
        to: &H160,
        _value: U256,
        _lowlevel: Vec<u8>,
        transaction_index: u32,
        transaction_hash: H256,
    ) -> Result<(TransactionV0, TransactionStatus, Receipt)> {
        let function = self.contracts.bridge.function("withdrawAsset").c(d!())?;

        let asset = Token::FixedBytes(Vec::from(_asset));

        let from = Token::Bytes(from.noah_to_bytes());

        let to = Token::Address(*to);

        let value = Token::Uint(_value);

        let lowlevel = Token::Bytes(_lowlevel);

        let input = function
            .encode_input(&[asset, from, to, value, lowlevel])
            .c(d!())?;

        let from = H160::zero();
        let gas_limit = 9999999;
        let value = U256::zero();

        let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input.clone(),
            from,
            gas_limit,
            self.contracts.bridge_address,
            value,
        )?;

        let action = TransactionAction::Call(self.contracts.bridge_address);
        let gas_price = U256::one();

        Ok(Self::system_transaction(
            transaction_hash,
            input,
            value,
            action,
            U256::from(gas_limit),
            gas_price,
            used_gas,
            transaction_index,
            from,
            self.contracts.bridge_address,
            logs,
        ))
    }

    pub fn withdraw_fra(
        &self,
        ctx: &Context,
        from: &XfrPublicKey,
        to: &H160,
        _value: U256,
        _lowlevel: Vec<u8>,
        transaction_index: u32,
        transaction_hash: H256,
    ) -> Result<(TransactionV0, TransactionStatus, Receipt)> {
        let function = self.contracts.bridge.function("withdrawFRA").c(d!())?;

        let from = Token::Bytes(from.noah_to_bytes());

        // let to = Token::Address(H160::from_slice(&bytes[4..24]));
        let to = Token::Address(*to);
        let value = Token::Uint(_value);
        let lowlevel = Token::Bytes(_lowlevel);

        // println!("{:?}, {:?}, {:?}, {:?}", from, to, value, lowlevel);

        let input = function
            .encode_input(&[from, to, value, lowlevel])
            .c(d!())?;

        let gas_limit = 9999999;
        let value = U256::zero();
        let gas_price = U256::one();
        let from = H160::zero();

        let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input.clone(),
            from,
            gas_limit,
            self.contracts.bridge_address,
            value,
        )?;

        let action = TransactionAction::Call(self.contracts.bridge_address);

        Ok(Self::system_transaction(
            transaction_hash,
            input,
            value,
            action,
            U256::from(gas_limit),
            gas_price,
            used_gas,
            transaction_index,
            from,
            self.contracts.bridge_address,
            logs,
        ))
    }

    pub fn consume_mint(&self, ctx: &Context) -> Vec<NonConfidentialOutput> {
        let height = CFG.checkpoint.prismxx_inital_height;

        let mut pending_outputs = Vec::new();

        if height < ctx.header.height {
            if let Err(e) =
                utils::fetch_mint::<C>(ctx, &self.contracts, &mut pending_outputs)
            {
                tracing::error!("Collect mint ops error: {:?}", e);
            }
        }

        pending_outputs
    }

    fn logs_bloom(logs: &[ethereum::Log], bloom: &mut Bloom) {
        for log in logs {
            bloom.accrue(BloomInput::Raw(&log.address[..]));
            for topic in log.topics.iter() {
                bloom.accrue(BloomInput::Raw(&topic[..]));
            }
        }
    }

    fn system_transaction(
        transaction_hash: H256,
        input: Vec<u8>,
        value: U256,
        action: TransactionAction,
        gas_limit: U256,
        gas_price: U256,
        used_gas: U256,
        transaction_index: u32,
        from: H160,
        to: H160,
        logs: Vec<Log>,
    ) -> (TransactionV0, TransactionStatus, Receipt) {
        let signature_fake = H256([
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02,
        ]);
        let tx = TransactionV0 {
            nonce: U256::zero(),
            gas_price,
            gas_limit,
            value,
            signature: TransactionSignature::new(28, signature_fake, signature_fake)
                .unwrap(),
            input,
            action,
        };

        let mut logs_bloom = Bloom::default();
        Self::logs_bloom(&logs, &mut logs_bloom);

        let tx_status = TransactionStatus {
            transaction_hash,
            transaction_index,
            from,
            to: Some(to),
            contract_address: Some(to),
            logs,
            logs_bloom,
        };

        let receipt = Receipt {
            state_root: H256::from_low_u64_be(1), //ExitReason::Succeed(_) => H256::from_low_u64_be(1),
            used_gas,
            logs_bloom: tx_status.logs_bloom,
            logs: tx_status.logs.clone(),
        };

        (tx, tx_status, receipt)
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
