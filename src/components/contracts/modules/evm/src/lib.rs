#![deny(warnings)]
#![allow(missing_docs)]

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
use ethereum::{
    Log, ReceiptV0 as Receipt, TransactionAction, TransactionSignature, TransactionV0,
};
use ethereum_types::U256;
use ethereum_types::{Bloom, BloomInput, H160, H256};
use evm::executor::stack::PrecompileSet as EvmPrecompileSet;
use fp_core::{
    context::Context,
    macros::Get,
    macros::Get2,
    module::AppModule,
    transaction::{ActionResult, Executable},
};
use fp_evm::TransactionStatus;
use fp_storage::BorrowMut;
use fp_traits::{
    account::AccountAsset,
    evm::{AddressMapping, BlockHashMapping, DecimalsMapping, FeeCalculator},
};
use fp_types::crypto::HA256;
use fp_types::{
    actions::evm::Action,
    crypto::{Address, HA160},
};
use ledger::staking::evm::EVM_STAKING_MINTS;
use ledger::staking::FRA_PRE_ISSUE_AMOUNT;
use module_ethereum::storage::{TransactionIndex, DELIVER_PENDING_TRANSACTIONS};
use precompile::PrecompileSet;
use protobuf::RepeatedField;
use ruc::*;
use runtime::runner::ActionRunner;
pub use runtime::*;
use std::marker::PhantomData;
use std::str::FromStr;
use system_contracts::{SystemContracts, SYSTEM_ADDR};
use zei::noah_algebra::serialization::NoahFromToBytes;
use zei::XfrPublicKey;

use crate::utils::{deposit_asset_event, parse_deposit_asset_event};

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
pub struct ValidatorParam {
    pub td_addr: H160,
    pub td_pubkey: Vec<u8>,
    pub keytype: U256,
    pub memo: String,
    pub rate: U256,
    pub staker: H160,
    pub staker_pk: Vec<u8>,
    pub power: U256,
    pub begin_block: U256,
}
pub struct DelegatorParam {
    pub validator: H160,
    pub delegator: H160,
    pub delegator_pk: Vec<u8>,
    pub bound_amount: U256,
    pub unbound_amount: U256,
}

pub struct UndelegationInfos {
    pub validator: H160,
    pub delegator: H160,
    pub amount: U256,
    pub height: U256,
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
    #[allow(clippy::too_many_arguments)]
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

        let from = H160::from_str(SYSTEM_ADDR).unwrap();
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
    #[allow(clippy::too_many_arguments)]
    pub fn withdraw_fra(
        &self,
        ctx: &Context,
        _from: &XfrPublicKey,
        to: &H160,
        _value: U256,
        _lowlevel: Vec<u8>,
        transaction_index: u32,
        transaction_hash: H256,
    ) -> Result<(TransactionV0, TransactionStatus, Receipt)> {
        let function = self.contracts.bridge.function("withdrawFRA").c(d!())?;

        let to = Token::Address(*to);
        let value = Token::Uint(_value);
        let lowlevel = Token::Bytes(_lowlevel);

        let input = function.encode_input(&[to, value, lowlevel]).c(d!())?;

        let gas_limit = 9999999;
        let value = U256::zero();
        let gas_price = U256::one();
        let from = H160::from_str(SYSTEM_ADDR).unwrap();

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

    fn logs_bloom(logs: &[ethereum::Log], bloom: &mut Bloom) {
        for log in logs {
            bloom.accrue(BloomInput::Raw(&log.address[..]));
            for topic in log.topics.iter() {
                bloom.accrue(BloomInput::Raw(&topic[..]));
            }
        }
    }
    #[allow(clippy::too_many_arguments)]
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
            state_root: H256::from_low_u64_be(1),
            used_gas,
            logs_bloom: tx_status.logs_bloom,
            logs: tx_status.logs.clone(),
        };

        (tx, tx_status, receipt)
    }
    fn execute_staking_contract(
        &self,
        ctx: &Context,
        req: &abci::RequestBeginBlock,
        ff_addr_balance: u64,
    ) -> Result<()> {
        let pre_issue_amount = FRA_PRE_ISSUE_AMOUNT - ff_addr_balance;
        let input =
            utils::build_evm_staking_input(&self.contracts, req, pre_issue_amount)?;

        let gas_limit = u64::MAX;
        let value = U256::zero();
        let from = H160::from_str(SYSTEM_ADDR).c(d!())?;

        tracing::info!(
            target: "evm staking",
            "trigger from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
            from,
            gas_limit,
            value,
            self.contracts.staking_address,
            hex::encode(&input)
        );

        let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input.clone(),
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;
        Self::store_transaction(
            ctx,
            U256::from(gas_limit),
            from,
            self.contracts.staking_address,
            value,
            input,
            &logs,
            used_gas,
        )?;
        let mut mints = vec![];
        let addr = H160::from_str(&CFG.checkpoint.prism_bridge_address).c(d!())?;
        let signature = deposit_asset_event().signature();
        for log in logs.iter() {
            if log.address != addr {
                continue;
            }
            match log.topics.get(0).cloned() {
                Some(v) => {
                    if v != signature {
                        continue;
                    }
                }
                None => continue,
            }
            if let Ok(output) = parse_deposit_asset_event(log.data.to_vec()) {
                mints.push((output.target, output.amount));
            }
        }
        if !mints.is_empty() {
            EVM_STAKING_MINTS.lock().extend(mints);
        }

        Ok(())
    }

    pub fn import_validators(
        &self,
        ctx: &Context,
        from: H160,
        validators: &[ValidatorParam],
    ) -> Result<()> {
        let function = self
            .contracts
            .staking
            .function("importValidators")
            .c(d!())?;
        let mut vss = vec![];
        {
            let vs = validators
                .iter()
                .map(|v| {
                    Token::Tuple(vec![
                        Token::Address(v.td_addr),
                        Token::Bytes(v.td_pubkey.clone()),
                        Token::Uint(v.keytype),
                        Token::String(v.memo.clone()),
                        Token::Uint(v.rate),
                        Token::Address(v.staker),
                        Token::Bytes(v.staker_pk.clone()),
                        Token::Uint(v.power),
                        Token::Uint(v.begin_block),
                    ])
                })
                .collect::<Vec<_>>();
            let mut tmp = vec![];
            for (index, v) in vs.iter().enumerate() {
                tmp.push(v.clone());
                if index > 0 && 0 == index % 60 {
                    vss.push(tmp.clone());
                    tmp.clear();
                }
            }
            if !tmp.is_empty() {
                vss.push(tmp);
            }
        }
        for vs in vss.iter() {
            let input = function
                .encode_input(&[Token::Array(vs.to_vec())])
                .c(d!())?;
            let gas_limit = u64::MAX;
            let value = U256::zero();
            tracing::info!(
                target: "evm staking",
                "importValidators from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
                from,
                gas_limit,
                value,
                self.contracts.staking_address,
                hex::encode(&input)
            );
            let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
                ctx,
                input.clone(),
                from,
                gas_limit,
                self.contracts.staking_address,
                value,
            )?;
            Self::store_transaction(
                ctx,
                U256::from(gas_limit),
                from,
                self.contracts.staking_address,
                value,
                input,
                &logs,
                used_gas,
            )?;
        }

        Ok(())
    }

    pub fn import_delegators(
        &self,
        ctx: &Context,
        from: H160,
        delegators: &[DelegatorParam],
    ) -> Result<()> {
        let function = self
            .contracts
            .staking
            .function("importDelegators")
            .c(d!())?;

        let mut dss = vec![];
        {
            let mut ds = vec![];
            for delegator in delegators.iter() {
                ds.push(Token::Tuple(vec![
                    Token::Address(delegator.validator),
                    Token::Address(delegator.delegator),
                    Token::Bytes(delegator.delegator_pk.clone()),
                    Token::Uint(delegator.bound_amount),
                    Token::Uint(delegator.unbound_amount),
                ]));
            }
            let mut tmp = vec![];
            for (index, v) in ds.iter().enumerate() {
                tmp.push(v.clone());
                if index > 0 && 0 == index % 60 {
                    dss.push(tmp.clone());
                    tmp.clear();
                }
            }
            if !tmp.is_empty() {
                dss.push(tmp);
            }
        }
        for ds in dss.iter() {
            let input = function
                .encode_input(&[Token::Array(ds.to_vec())])
                .c(d!())?;
            let gas_limit = u64::MAX;
            let value = U256::zero();
            tracing::info!(
                target: "evm staking",
                "importDelegators from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
                from,
                gas_limit,
                value,
                self.contracts.staking_address,
                hex::encode(&input)
            );
            let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
                ctx,
                input.clone(),
                from,
                gas_limit,
                self.contracts.staking_address,
                value,
            )?;
            Self::store_transaction(
                ctx,
                U256::from(gas_limit),
                from,
                self.contracts.staking_address,
                value,
                input,
                &logs,
                used_gas,
            )?;
        }

        Ok(())
    }

    pub fn import_undelegations(
        &self,
        ctx: &Context,
        from: H160,
        undelegationinfos: &[UndelegationInfos],
    ) -> Result<()> {
        let function = self
            .contracts
            .staking
            .function("importUndelegations")
            .c(d!())?;
        let mut infos = vec![];
        {
            let undelegation_infos = undelegationinfos
                .iter()
                .map(|v| {
                    Token::Tuple(vec![
                        Token::Address(v.validator),
                        Token::Address(v.delegator),
                        Token::Uint(v.amount),
                        Token::Uint(v.height),
                    ])
                })
                .collect::<Vec<_>>();
            let mut tmp = vec![];
            for (index, v) in undelegation_infos.iter().enumerate() {
                tmp.push(v.clone());
                if index > 0 && 0 == index % 60 {
                    infos.push(tmp.clone());
                    tmp.clear();
                }
            }
            if !tmp.is_empty() {
                infos.push(tmp);
            }
        }
        for info in infos.iter() {
            let input = function
                .encode_input(&[Token::Array(info.to_vec())])
                .c(d!())?;
            let gas_limit = u64::MAX;
            let value = U256::zero();
            tracing::info!(
                target: "evm staking",
                "importUndelegations from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
                from,
                gas_limit,
                value,
                self.contracts.staking_address,
                hex::encode(&input)
            );
            let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
                ctx,
                input.clone(),
                from,
                gas_limit,
                self.contracts.staking_address,
                value,
            )?;
            Self::store_transaction(
                ctx,
                U256::from(gas_limit),
                from,
                self.contracts.staking_address,
                value,
                input,
                &logs,
                used_gas,
            )?;
        }

        Ok(())
    }

    pub fn import_reward(
        &self,
        ctx: &Context,
        from: H160,
        rewards: &[(H160, U256)],
    ) -> Result<()> {
        let function = self.contracts.staking.function("importReward").c(d!())?;
        let mut reward_tokens = vec![];
        {
            let tokens = rewards
                .iter()
                .map(|(addr, amount)| {
                    Token::Tuple(vec![Token::Address(*addr), Token::Uint(*amount)])
                })
                .collect::<Vec<_>>();
            let mut tmp = vec![];
            for (index, v) in tokens.iter().enumerate() {
                tmp.push(v.clone());
                if index > 0 && 0 == index % 60 {
                    reward_tokens.push(tmp.clone());
                    tmp.clear();
                }
            }
            if !tmp.is_empty() {
                reward_tokens.push(tmp);
            }
        }
        for reward_token in reward_tokens.iter() {
            let input = function
                .encode_input(&[Token::Array(reward_token.to_vec())])
                .c(d!())?;
            let gas_limit = u64::MAX;
            let value = U256::zero();
            tracing::info!(
                target: "evm staking",
                "importReward from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
                from,
                gas_limit,
                value,
                self.contracts.staking_address,
                hex::encode(&input)
            );
            let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
                ctx,
                input.clone(),
                from,
                gas_limit,
                self.contracts.staking_address,
                value,
            )?;
            Self::store_transaction(
                ctx,
                U256::from(gas_limit),
                from,
                self.contracts.staking_address,
                value,
                input,
                &logs,
                used_gas,
            )?;
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
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
        let function = self.contracts.staking.function("systemStake").c(d!())?;

        let validator = Token::Address(validator);
        let td_pubkey = Token::Bytes(td_pubkey);
        let staker = Token::Address(staker);
        let staker_pk = Token::Bytes(staker_pk);
        let memo = Token::String(memo);
        let rate = Token::Uint(rate);

        let input = function
            .encode_input(&[validator, td_pubkey, staker, staker_pk, memo, rate])
            .c(d!())?;

        let gas_limit = u64::MAX;

        tracing::info!(
            target: "evm staking",
            "systemStake from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
            from,
            gas_limit,
            value,
            self.contracts.staking_address,
            hex::encode(&input)
        );
        let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input.clone(),
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;
        Self::store_transaction(
            ctx,
            U256::from(gas_limit),
            from,
            self.contracts.staking_address,
            value,
            input,
            &logs,
            used_gas,
        )?;
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

        let function = self.contracts.staking.function("systemDelegate").c(d!())?;
        let validator = Token::Address(validator);
        let delegator = Token::Address(delegator);
        let delegator_pk = Token::Bytes(delegator_pk);
        let input = function
            .encode_input(&[validator, delegator, delegator_pk])
            .c(d!())?;

        let gas_limit = u64::MAX;
        tracing::info!(
            target: "evm staking",
            "systemDelegate from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
            from,
            gas_limit,
            amount,
            self.contracts.staking_address,
            hex::encode(&input)
        );
        let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input.clone(),
            from,
            gas_limit,
            self.contracts.staking_address,
            amount,
        )?;

        Self::store_transaction(
            ctx,
            U256::from(gas_limit),
            from,
            self.contracts.staking_address,
            amount,
            input,
            &logs,
            used_gas,
        )?;
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
        let function = self
            .contracts
            .staking
            .function("systemUndelegate")
            .c(d!())?;

        let validator = Token::Address(validator);
        let delegator = Token::Address(delegator);
        let amount = Token::Uint(amount);
        let input = function
            .encode_input(&[validator, delegator, amount])
            .c(d!())?;

        let gas_limit = u64::MAX;
        let value = U256::zero();

        tracing::info!(
            target: "evm staking",
            "systemUndelegate from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
            from,
            gas_limit,
            value,
            self.contracts.staking_address,
            hex::encode(&input)
        );

        let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input.clone(),
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;
        Self::store_transaction(
            ctx,
            U256::from(gas_limit),
            from,
            self.contracts.staking_address,
            value,
            input,
            &logs,
            used_gas,
        )?;
        Ok(())
    }

    pub fn claim(
        &self,
        ctx: &Context,
        from: H160,
        validator: H160,
        delegator: H160,
    ) -> Result<()> {
        let function = self.contracts.staking.function("systemClaim").c(d!())?;
        let input = function
            .encode_input(&[Token::Address(validator), Token::Address(delegator)])
            .c(d!())?;

        let gas_limit = u64::MAX;
        let value = U256::zero();

        tracing::info!(
            target: "evm staking",
            "systemClaim from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
            from,
            gas_limit,
            value,
            self.contracts.staking_address,
            hex::encode(&input)
        );

        let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input.clone(),
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;

        Self::store_transaction(
            ctx,
            U256::from(gas_limit),
            from,
            self.contracts.staking_address,
            value,
            input,
            &logs,
            used_gas,
        )?;
        let mut mints = vec![];
        let addr = H160::from_str(&CFG.checkpoint.prism_bridge_address).c(d!())?;
        let signature = deposit_asset_event().signature();
        for log in logs.iter() {
            if log.address != addr {
                continue;
            }
            match log.topics.get(0).cloned() {
                Some(v) => {
                    if v != signature {
                        continue;
                    }
                }
                None => continue,
            }

            if let Ok(output) = parse_deposit_asset_event(log.data.to_vec()) {
                mints.push((output.target, output.amount));
            }
        }
        if !mints.is_empty() {
            EVM_STAKING_MINTS.lock().extend(mints);
        }
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
        let func = self
            .contracts
            .staking
            .function("systemUpdateValidator")
            .c(d!())?;

        let validator = Token::Address(validator);
        let staker = Token::Address(staker);
        let memo = Token::String(memo);
        let rate = Token::Uint(rate);

        let input = func
            .encode_input(&[validator, staker, memo, rate])
            .c(d!())?;

        let gas_limit = u64::MAX;
        let value = U256::zero();
        let from = H160::from_str(SYSTEM_ADDR).c(d!())?;

        tracing::info!(
            target: "evm staking",
            "systemUpdateValidator from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
            from,
            gas_limit,
            value,
            self.contracts.staking_address,
            hex::encode(&input)
        );

        let (_, logs, used_gas) = ActionRunner::<C>::execute_systemc_contract(
            ctx,
            input.clone(),
            from,
            gas_limit,
            self.contracts.staking_address,
            value,
        )?;

        Self::store_transaction(
            ctx,
            U256::from(gas_limit),
            from,
            self.contracts.staking_address,
            value,
            input,
            &logs,
            used_gas,
        )?;

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn store_transaction(
        ctx: &Context,
        gas_limit: U256,
        from: H160,
        staking_address: H160,
        value: U256,
        input: Vec<u8>,
        logs: &[Log],
        used_gas: U256,
    ) -> Result<()> {
        let transaction = TransactionV0 {
            nonce: U256::from(ctx.header.height),
            gas_price: U256::zero(),
            gas_limit,
            action: TransactionAction::Call(staking_address),
            value,
            input,
            signature: pnk!(TransactionSignature::new(
                27,
                H256::from_low_u64_be(1),
                H256::from_low_u64_be(2),
            )),
        };
        let transaction_hash = transaction.hash();
        tracing::info!(
            "generate Transaction: {}:{:?}",
            transaction_hash,
            transaction
        );
        let mut pending_txs = DELIVER_PENDING_TRANSACTIONS.lock().c(d!())?;
        let transaction_index = pending_txs.len() as u32;

        let status = TransactionStatus {
            transaction_hash,
            transaction_index,
            from,
            to: Some(staking_address),
            contract_address: None,
            logs: logs.to_vec(),
            logs_bloom: {
                let mut bloom: Bloom = Bloom::default();
                Self::logs_bloom(logs, &mut bloom);
                bloom
            },
        };
        tracing::info!("generate TransactionStatus: {:?}", status);

        let receipt = ethereum::ReceiptV0 {
            state_root: H256::from_low_u64_be(1),
            used_gas,
            logs_bloom: status.logs_bloom,
            logs: status.logs.clone(),
        };
        tracing::info!("generate TransactionReceipt: {:?}", receipt);

        pending_txs.push((transaction, status, receipt));

        TransactionIndex::insert(
            ctx.db.write().borrow_mut(),
            &HA256::new(transaction_hash),
            &(ctx.header.height.into(), transaction_index),
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

        let gas_limit = u64::MAX;
        let value = U256::zero();
        let from = H160::from_str(SYSTEM_ADDR).c(d!())?;

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
                    storage::AccountCodes::iterate(&ctx.state.read());
                resp.value = serde_json::to_vec(&contracts.len()).unwrap_or_default();
                resp
            }
            _ => resp,
        }
    }
    fn begin_block(&mut self, ctx: &mut Context, req: &abci::RequestBeginBlock) {
        if ctx.header.height > CFG.checkpoint.evm_staking_inital_height {
            self.abci_begin_block = req.clone();
        }
    }

    fn end_block(
        &mut self,
        ctx: &mut Context,
        _req: &abci::RequestEndBlock,
        ff_addr_balance: u64,
    ) -> abci::ResponseEndBlock {
        let mut resp = abci::ResponseEndBlock::default();

        if ctx.header.height > CFG.checkpoint.evm_staking_inital_height {
            match self.execute_staking_contract(
                ctx,
                &self.abci_begin_block,
                ff_addr_balance,
            ) {
                Ok(v) => v,
                Err(e) => {
                    tracing::error!("Error on evm staking trigger: {}", e);
                    Default::default()
                }
            };
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

pub fn get_claim_on_contract_address<C: Config>(
    contract: &SystemContracts,
    ctx: &Context,
    from: H160,
) -> Result<H160> {
    let function = contract
        .staking
        .function("getClaimOnContractAddress")
        .c(d!())?;
    let input = function.encode_input(&[]).c(d!())?;

    let gas_limit = u64::MAX;
    let value = U256::zero();

    tracing::info!(
        target: "evm staking",
        "getClaimOnContractAddress from:{:?} gas_limit:{} value:{} contracts_address:{:?} input:{}",
        from,
        gas_limit,
        value,
       contract.staking_address,
        hex::encode(&input)
    );

    let (data, _, _) = ActionRunner::<C>::execute_systemc_contract(
        ctx,
        input,
        from,
        gas_limit,
        contract.staking_address,
        value,
    )?;
    let ret = function.decode_output(&data).c(d!())?;

    if let Some(Token::Address(addr)) = ret.get(0) {
        Ok(*addr)
    } else {
        Err(eg!("address not found"))
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
