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
use ledger::staking::evm::EVM_STAKING_MINTS;
// use ledger::staking::evm::EVM_STAKING_MINTS;
use noah::xfr::sig::XfrPublicKey;
use noah_algebra::serialization::NoahFromToBytes;
use precompile::PrecompileSet;
use protobuf::RepeatedField;
use ruc::*;
use runtime::runner::ActionRunner;
use std::marker::PhantomData;
use system_contracts::SystemContracts;

pub use runtime::*;

use crate::utils::parse_evm_staking_mint_event;

// use crate::utils::build_undelegation_mints;

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
    pub abci_begin_block: abci::RequestBeginBlock,
    pub mint_ops: Vec<(H160, U256)>,
}

impl<C: Config> Default for App<C> {
    fn default() -> Self {
        App {
            phantom: Default::default(),
            contracts: pnk!(SystemContracts::new()),
            abci_begin_block: Default::default(),
            mint_ops: Default::default(),
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

    fn execute_staking_contract(
        &self,
        ctx: &Context,
        req: &abci::RequestBeginBlock,
    ) -> Result<()> {
        let input = utils::build_evm_staking_input(&self.contracts, req)?;

        let gas_limit = 9999999;
        let value = U256::zero();
        let from = H160::zero();

        let (_, logs, _) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;

        println!("Logs: {:?}", logs);

        let mut mints = vec![];
        for log in logs.into_iter() {
            match parse_evm_staking_mint_event(log.data) {
                Ok((pk, am)) => {
                    if am != 0 {
                        mints.push((pk, am));
                    }
                }
                Err(e) => {
                    tracing::warn!("Parse evm staking mint error: {}", e);
                }
            }
        }

        if mints.len() > 0 {
            EVM_STAKING_MINTS.lock().extend(mints);
        }

        Ok(())
    }

    pub fn stake(
        &self,
        ctx: &Context,
        from: H160,
        value: U256,
        validator: H160,
        td_pubkey: Vec<u8>,
        staker: H160,
        staker_pk: Vec<u8>,
        memo: String,
        rate: U256,
    ) -> Result<()> {
        println!(
            "Stake: from {:x} to {:x}, amount: {}",
            &staker, &validator, &value
        );

        let function = self.contracts.staking.function("stake").c(d!())?;

        let validator = Token::Address(validator);
        let td_pubkey = Token::Bytes(td_pubkey);
        let staker = Token::Address(staker);
        let staker_pk = Token::Bytes(staker_pk);
        let memo = Token::String(memo);
        let rate = Token::Uint(rate);

        let input = function
            .encode_input(&[validator, td_pubkey, staker, staker_pk, memo, rate])
            .c(d!())?;

        let gas_limit = 99999999999;

        let (_, logs, _) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;

        println!("Logs: {:?}", logs);

        Ok(())
    }

    pub fn delegate(
        &self,
        ctx: &Context,
        from: H160,
        validator: H160,
        delegator: H160,
        delegator_pk: Vec<u8>,
        amount: U256,
    ) -> Result<()> {
        println!(
            "Delegate from {:X} to {:X}, amount:{}",
            &delegator, &validator, &amount
        );

        let function = self.contracts.staking.function("delegate").c(d!())?;
        let validator = Token::Address(validator);
        let delegator = Token::Address(delegator);
        let delegator_pk = Token::Bytes(delegator_pk);
        let input = function
            .encode_input(&[validator, delegator, delegator_pk])
            .c(d!())?;

        let gas_limit = 99999999999;

        let (_, logs, _) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            from,
            gas_limit,
            self.contracts.staking_address,
            amount,
        )?;

        println!("Delegate logs: {:?}", logs);

        Ok(())
    }

    pub fn undelegate(
        &self,
        ctx: &Context,
        from: H160,
        validator: H160,
        delegator: H160,
        amount: U256,
    ) -> Result<()> {
        let function = self.contracts.staking.function("undelegate").c(d!())?;

        let validator = Token::Address(validator);
        let delegator = Token::Address(delegator);
        let amount = Token::Uint(amount);
        let input = function
            .encode_input(&[validator, delegator, amount])
            .c(d!())?;

        let gas_limit = 99999999999;
        let value = U256::zero();

        let (_, _, _) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;

        Ok(())
    }

    pub fn claim(&self, ctx: &Context, from: H160, amount: U256) -> Result<()> {
        let function = self.contracts.staking.function("claim").c(d!())?;

        let amount = Token::Uint(amount);
        let input = function.encode_input(&[amount]).c(d!())?;

        let gas_limit = 99999999999;
        let value = U256::zero();

        let (_, _, _) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;

        Ok(())
    }

    pub fn update_validator(
        &self,
        ctx: &Context,
        staker: H160,
        validator: H160,
        memo: String,
        rate: U256,
    ) -> Result<()> {
        let func = self.contracts.staking.function("updateValidator").c(d!())?;

        let staker = Token::Address(staker);
        let validator = Token::Address(validator);
        let memo = Token::String(memo);
        let rate = Token::Uint(rate);

        let input = func
            .encode_input(&[staker, validator, memo, rate])
            .c(d!())?;

        let gas_limit = 99999999999;
        let value = U256::zero();
        let from = H160::zero();

        let (_, _, _) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;

        Ok(())
    }

    fn get_validator_list(&self, ctx: &Context) -> Result<Vec<abci::ValidatorUpdate>> {
        let func = self
            .contracts
            .staking
            .function("getValidatorsList")
            .c(d!())?;
        let input = func.encode_input(&[]).c(d!())?;

        let gas_limit = 99999999999;
        let value = U256::zero();
        let from = H160::zero();

        let (data, _, _) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;

        utils::build_validator_updates(&self.contracts, &data)
    }

    pub fn get_claim_ops(&self, ctx: &Context) -> Result<Vec<(H160, U256)>> {
        let func = self.contracts.staking.function("getClaimOps").c(d!())?;
        let input = func.encode_input(&[]).c(d!())?;

        let gas_limit = 99999999999;
        let value = U256::zero();
        let from = H160::zero();

        let (data, _, _) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input,
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;

        utils::build_claim_ops(&self.contracts, &data)
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

    fn begin_block(&mut self, ctx: &mut Context, req: &abci::RequestBeginBlock) {
        let height = CFG.checkpoint.prismxx_inital_height;

        if ctx.header.height == height {
            let bytecode_str = include_str!("../contracts/PrismXXProxy.bytecode");

            if let Err(e) =
                utils::deploy_contract::<C>(ctx, &self.contracts, bytecode_str)
            {
                pd!(e);
                return;
            }
            tracing::info!(
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

        if ctx.header.height == CFG.checkpoint.evm_staking {
            let bytecode_str =
                include_str!("../contracts/EVMStakingSystemProxy.bytecode");

            if let Err(e) =
                utils::deploy_contract::<C>(ctx, &self.contracts, bytecode_str)
            {
                pd!(e);
                return;
            }
            tracing::info!(
                "EVMStaking system contract address: {:?}",
                self.contracts.bridge_address
            );

            if !ctx.state.write().cache_mut().good2_commit() {
                ctx.state.write().discard_session();
                pd!(eg!("ctx state commit no good"));
            } else {
                ctx.state.write().commit_session();
            }
        }

        if ctx.header.height > CFG.checkpoint.evm_staking {
            self.abci_begin_block = req.clone();
        }
    }

    fn end_block(
        &mut self,
        ctx: &mut Context,
        _req: &abci::RequestEndBlock,
    ) -> abci::ResponseEndBlock {
        let mut resp = abci::ResponseEndBlock::default();

        if ctx.header.height > CFG.checkpoint.evm_staking {
            if let Err(e) = self.execute_staking_contract(ctx, &self.abci_begin_block) {
                println!("Error on evm staking trigger: {}", &e);
                tracing::error!("Error on evm staking trigger: {}", e);
            }

            match self.get_validator_list(ctx) {
                Ok(r) => {
                    if !r.is_empty() {
                        resp.set_validator_updates(RepeatedField::from_vec(r));
                    }
                }
                Err(e) => tracing::error!("Error on get validator list: {}", e),
            }
        }

        resp
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
