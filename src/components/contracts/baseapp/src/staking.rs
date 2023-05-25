use crate::BaseApp;
use ethereum_types::{H160, U256};
use fp_traits::{
    account::AccountAsset,
    evm::{DecimalsMapping, EthereumDecimalsMapping},
};
use fp_types::crypto::Address;
use ledger::staking::{evm::EVMStaking, Delegation, Validator, BLOCK_HEIGHT_MAX};
use module_evm::{
    system_contracts::EVM_SYSTEM_ADDR, DelegatorParam, UndelegationInfos, ValidatorParam,
};
use ruc::{d, Result, RucResult};
use sha3::{Digest, Keccak256};
use std::{collections::BTreeMap, str::FromStr};
use zei::xfr::sig::XfrPublicKey;

impl EVMStaking for BaseApp {
    fn import_validators(
        &self,
        validators: &[Validator],
        delegations: &BTreeMap<XfrPublicKey, Delegation>,
    ) -> Result<()> {
        let from = H160::from_str(EVM_SYSTEM_ADDR).c(d!())?;

        let mut vs = vec![];
        {
            for v in validators.iter() {
                let begin_block = delegations
                    .get(&v.id)
                    .map(|d| d.start_height)
                    .unwrap_or(self.deliver_state.header.height as u64);

                vs.push(ValidatorParam {
                    td_pubkey: v.td_pubkey.clone(),
                    keytype: U256::from(2),
                    memo: serde_json::to_string(&v.memo).c(d!())?,
                    rate: mapping_rate(v.commission_rate),
                    staker: mapping_address(&v.id),
                    power: U256::from(v.td_power),
                    begin_block: U256::from(begin_block),
                });
            }
        }
        let mut ds: BTreeMap<H160, BTreeMap<u64, Vec<(H160, U256)>>> = BTreeMap::new();
        {
            for delegation in delegations.values() {
                let d = delegation
                    .delegations
                    .iter()
                    .map(|(validator_addr, amount)| {
                        (mapping_address(validator_addr), U256::from(*amount))
                    })
                    .collect::<Vec<_>>();
                let mut v = BTreeMap::new();
                v.insert(delegation.end_height, d.clone());

                let address = delegation.receiver_pk.unwrap_or(delegation.id);
                ds.entry(mapping_address(&address))
                    .and_modify(|rem| {
                        rem.entry(delegation.end_height)
                            .and_modify(|rem1| rem1.extend(d.clone().into_iter()))
                            .or_insert(d);
                    })
                    .or_insert(v);
            }
        }
        let mut delegators = vec![];
        let mut undelegation_infos = vec![];
        {
            for (address, delegations) in ds.iter() {
                let mut bound_amount = vec![];
                let mut unbound_amount = vec![];
                for (end_height, values) in delegations.iter() {
                    let v = values.to_vec();
                    if BLOCK_HEIGHT_MAX == *end_height {
                        bound_amount.extend(v);
                    } else {
                        unbound_amount.extend(v);
                        let v = values
                            .iter()
                            .map(|(validator_addr, amount)| UndelegationInfos {
                                validator: *validator_addr,
                                delegator: *address,
                                amount: *amount,
                                height: U256::from(*end_height),
                            })
                            .collect::<Vec<_>>();
                        undelegation_infos.extend(v);
                    }
                }

                delegators.push(DelegatorParam {
                    delegator: *address,
                    bound_amount,
                    unbound_amount,
                });
            }
        }
        if let Err(e) = self.modules.evm_module.import_validators(
            &self.deliver_state,
            from,
            &vs,
            &delegators,
            &undelegation_infos,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        }

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();
        Ok(())
    }
    fn stake(
        &self,
        staker: &XfrPublicKey,
        amount: u64,
        td_addr: &[u8],
        td_pubkey: Vec<u8>,
        memo: String,
        rate: [u64; 2],
    ) -> Result<()> {
        let staker_pk = staker.as_bytes().to_vec();
        let staker_address = mapping_address(staker);

        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(amount)).c(d!())?;

        let from = H160::from_str(EVM_SYSTEM_ADDR).c(d!())?;

        module_account::App::<BaseApp>::mint(
            &self.deliver_state,
            &Address::from(from),
            amount,
        )?;

        if let Err(e) = self.modules.evm_module.stake(
            &self.deliver_state,
            from,
            amount,
            H160::from_slice(td_addr),
            td_pubkey,
            staker_address,
            staker_pk,
            memo,
            mapping_rate(rate),
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        }

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();
        Ok(())
    }

    fn delegate(
        &self,
        delegator: &XfrPublicKey,
        amount: u64,
        td_addr: &[u8],
    ) -> Result<()> {
        let delegator_pk = delegator.as_bytes().to_vec();
        let delegator_address = mapping_address(delegator);

        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(amount)).c(d!())?;

        let from = H160::from_str(EVM_SYSTEM_ADDR).c(d!())?;

        module_account::App::<BaseApp>::mint(
            &self.deliver_state,
            &Address::from(from),
            amount,
        )?;

        if let Err(e) = self.modules.evm_module.delegate(
            &self.deliver_state,
            from,
            H160::from_slice(td_addr),
            delegator_address,
            delegator_pk,
            amount,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        }

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();

        Ok(())
    }

    fn undelegate(
        &self,
        delegator: &XfrPublicKey,
        td_addr: &[u8],
        amount: u64,
    ) -> Result<()> {
        let delegator_address = mapping_address(delegator);

        let from = H160::from_str(EVM_SYSTEM_ADDR).c(d!())?;

        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(amount)).c(d!())?;

        if let Err(e) = self.modules.evm_module.undelegate(
            &self.deliver_state,
            from,
            H160::from_slice(td_addr),
            delegator_address,
            amount,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        };

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();

        Ok(())
    }

    fn update_validator(
        &self,
        staker: &XfrPublicKey,
        td_address: &[u8],
        memo: String,
        rate: [u64; 2],
    ) -> Result<()> {
        let staker_address = mapping_address(staker);
        let rate = mapping_rate(rate);
        let validator = H160::from_slice(td_address);

        if let Err(e) = self.modules.evm_module.update_validator(
            &self.deliver_state,
            staker_address,
            validator,
            memo,
            rate,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        }

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();
        Ok(())
    }

    fn claim(&self, delegator_pk: &XfrPublicKey, amount: u64) -> Result<()> {
        let delegator = mapping_address(delegator_pk);
        let amount = U256::from(amount);

        if let Err(e) = self.modules.evm_module.claim(
            &self.deliver_state,
            H160::zero(),
            delegator,
            delegator_pk,
            amount,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        }

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();
        Ok(())
    }
}
fn mapping_rate(rate: [u64; 2]) -> U256 {
    if rate[0] == 0 {
        return U256::zero();
    }

    let deciamls = 1_000_000_u64;
    U256::from(rate[0].saturating_mul(deciamls) / rate[1])
}

fn mapping_address(pk: &XfrPublicKey) -> H160 {
    let result = Keccak256::digest(pk.as_bytes());
    H160::from_slice(&result.as_slice()[..20])
}
