use crate::BaseApp;
use config::abci::global_cfg::CFG;
use ethereum_types::{H160, U256};
use fp_traits::{
    account::AccountAsset,
    evm::{DecimalsMapping, EthereumDecimalsMapping},
};
use fp_types::crypto::Address;
use ledger::staking::{
    evm::EVMStaking, Delegation, DelegationState, Validator, BLOCK_HEIGHT_MAX,
};
use module_evm::{
    get_claim_on_contract_address, system_contracts::SYSTEM_ADDR, DelegatorParam,
    UndelegationInfos, ValidatorParam,
};
use ruc::{d, eg, Result, RucResult};
use sha3::{Digest, Keccak256};
use std::{collections::BTreeMap, str::FromStr};
use zei::noah_algebra::prelude::NoahFromToBytes;
use zei::XfrPublicKey;

impl EVMStaking for BaseApp {
    fn import_validators(
        &self,
        validators: &[Validator],
        delegations: &BTreeMap<XfrPublicKey, Delegation>,
        coinbase_balance: u64,
    ) -> Result<()> {
        let from = H160::from_str(SYSTEM_ADDR).c(d!())?;

        let mut amount = U256::zero();
        let mut vs = vec![];
        {
            for v in validators.iter() {
                let begin_block = delegations
                    .get(&v.id)
                    .map(|d| d.start_height)
                    .unwrap_or(self.deliver_state.header.height as u64);
                let power =
                    EthereumDecimalsMapping::from_native_token(U256::from(v.td_power))
                        .c(d!())?;
                amount = amount.checked_add(power).c(d!())?;
                vs.push(ValidatorParam {
                    td_addr: H160::from_slice(&v.td_addr),
                    td_pubkey: v.td_pubkey.clone(),
                    keytype: U256::from(2),
                    memo: serde_json::to_string(&v.memo).c(d!())?,
                    rate: mapping_rate(v.commission_rate),
                    staker: mapping_address(&v.id),
                    staker_pk: v.id.noah_to_bytes(),
                    power,
                    begin_block: U256::from(begin_block),
                });
            }
        }
        let evm_staking_address =
            H160::from_str(CFG.checkpoint.evm_staking_address.as_str()).c(d!())?;
        module_account::App::<BaseApp>::mint(
            &self.deliver_state,
            &Address::from(evm_staking_address),
            amount,
        )?;

        if let Err(e) =
            self.modules
                .evm_module
                .import_validators(&self.deliver_state, from, &vs)
        {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            tracing::error!(target: "evm staking", "import_validators error:{:?}", e);
            return Err(e);
        }

        let mut ds: BTreeMap<(XfrPublicKey, H160), Vec<(u64, U256)>> = BTreeMap::new();
        {
            for delegation in delegations.values() {
                let public_key = delegation.receiver_pk.unwrap_or(delegation.id);

                for (validator, amount) in delegation.delegations.iter() {
                    let amount =
                        EthereumDecimalsMapping::from_native_token(U256::from(*amount))
                            .c(d!())?;
                    ds.entry((public_key, mapping_address(validator)))
                        .and_modify(|am| {
                            am.push((delegation.end_height, amount));
                        })
                        .or_insert(vec![(delegation.end_height, amount)]);
                }
            }
        }
        let mut delegators = vec![];
        let mut undelegation_infos = vec![];
        {
            for ((public_key, validator_address), value) in ds.iter() {
                let mut bound_amount = U256::zero();
                let mut unbound_amount = U256::zero();

                let delegator_address = mapping_address(public_key);

                for (end_height, amount) in value.iter() {
                    if BLOCK_HEIGHT_MAX == *end_height {
                        bound_amount = bound_amount.checked_add(*amount).c(d!())?;
                    } else {
                        unbound_amount = unbound_amount.checked_add(*amount).c(d!())?;
                        undelegation_infos.push(UndelegationInfos {
                            validator: *validator_address,
                            delegator: delegator_address,
                            amount: *amount,
                            height: U256::from(*end_height),
                        });
                    }
                }
                delegators.push(DelegatorParam {
                    validator: *validator_address,
                    delegator: delegator_address,
                    delegator_pk: public_key.noah_to_bytes(),
                    bound_amount,
                    unbound_amount,
                });
            }
        }
        undelegation_infos.sort_by(|a, b| a.height.cmp(&b.height));

        if let Err(e) = self.modules.evm_module.import_delegators(
            &self.deliver_state,
            from,
            &delegators,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            tracing::error!(target: "evm staking", "import_delegators error:{:?}", e);
            return Err(e);
        }
        if let Err(e) = self.modules.evm_module.import_undelegations(
            &self.deliver_state,
            from,
            &undelegation_infos,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            tracing::error!(target: "evm staking", "import_undelegations error:{:?}", e);
            return Err(e);
        }

        let reward = delegations
            .values()
            .filter(|d| d.state == DelegationState::Bond)
            .map(|d| {
                (
                    mapping_address(&d.receiver_pk.unwrap_or(d.id)),
                    EthereumDecimalsMapping::from_native_token(U256::from(d.rwd_amount))
                        .unwrap(),
                )
            })
            .collect::<Vec<_>>();
        if let Err(e) =
            self.modules
                .evm_module
                .import_reward(&self.deliver_state, from, &reward)
        {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            tracing::error!(target: "evm staking", "import_reward error:{:?}", e);
            return Err(e);
        }

        let claim_on_contract_address = get_claim_on_contract_address::<BaseApp>(
            &self.modules.evm_module.contracts,
            &self.deliver_state,
            from,
        )?;
        module_account::App::<BaseApp>::mint(
            &self.deliver_state,
            &Address::from(claim_on_contract_address),
            EthereumDecimalsMapping::from_native_token(U256::from(coinbase_balance))
                .c(d!())?,
        )?;

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
        let staker_pk = staker.noah_to_bytes();
        let staker_address = mapping_address(staker);

        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(amount)).c(d!())?;

        let from = H160::from_str(SYSTEM_ADDR).c(d!())?;

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
            tracing::error!(target: "evm staking", "stake error:{:?}", e);
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
        let delegator_pk = delegator.noah_to_bytes();
        let delegator_address = mapping_address(delegator);

        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(amount)).c(d!())?;

        let from = H160::from_str(SYSTEM_ADDR).c(d!())?;

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
            tracing::error!(target: "evm staking", "delegate error:{:?}", e);
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

        let from = H160::from_str(SYSTEM_ADDR).c(d!())?;

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
            tracing::error!(target: "evm staking", "undelegate error:{:?}", e);
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
            tracing::error!(target: "evm staking", "update_validator error:{:?}", e);
            return Err(e);
        }

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();
        Ok(())
    }

    fn claim(&self, td_addr: &[u8], delegator_pk: &XfrPublicKey) -> Result<()> {
        if td_addr.len() != 20 {
            return Err(eg!("td_addr length error"));
        }
        let validator = H160::from_slice(td_addr);
        let delegator = mapping_address(delegator_pk);
        let from = H160::from_str(SYSTEM_ADDR).c(d!())?;
        if let Err(e) = self.modules.evm_module.claim(
            &self.deliver_state,
            from,
            validator,
            delegator,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            tracing::error!(target: "evm staking", "claim error:{:?}", e);
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

pub fn mapping_address(pk: &XfrPublicKey) -> H160 {
    let result = Keccak256::digest(pk.noah_to_bytes());
    H160::from_slice(&result.as_slice()[..20])
}
