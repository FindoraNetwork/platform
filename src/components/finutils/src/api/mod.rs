use ledger::staking::{
    self, StakerMemo, TendermintAddr, MAX_POWER_PERCENT_PER_VALIDATOR,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ValidatorList {
    threshold: [u128; 2],
    validator_cnt: u64,
    cur_height: u64,
    validators: Vec<Validator>,
}

impl ValidatorList {
    pub fn new(cur_height: u64, validators: Vec<Validator>) -> Self {
        ValidatorList {
            threshold: MAX_POWER_PERCENT_PER_VALIDATOR,
            validator_cnt: validators.len() as u64,
            cur_height,
            validators,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Validator {
    addr: TendermintAddr,
    power: u64,
    commission_rate: [u64; 2],
    accept_delegation: bool,
    return_rate: [u128; 2],
    expected_annualization: [u128; 2],
    rank: u64,
    block_signed_cnt: u64,
    extra: StakerMemo,
}

impl Validator {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        addr: TendermintAddr,
        return_rate: [u128; 2],
        expected_annualization: [u128; 2],
        rank: u64,
        accept_delegation: bool,
        v: &staking::Validator,
    ) -> Self {
        Validator {
            addr,
            power: v.td_power,
            commission_rate: v.get_commission_rate(),
            accept_delegation,
            return_rate,
            expected_annualization,
            rank,
            block_signed_cnt: v.signed_cnt,
            extra: v.memo.clone(),
        }
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct ValidatorDetail {
    pub addr: TendermintAddr,
    pub kind: String,
    pub is_online: bool,
    pub voting_power: u64,
    pub voting_power_rank: usize,
    pub commission_rate: [u64; 2],
    pub self_staking: u64,
    pub fra_rewards: u64,
    pub memo: StakerMemo,
    pub start_height: u64,
    pub cur_height: u64,
    pub block_signed_cnt: u64,
    pub block_proposed_cnt: u64,
    pub expected_annualization: [u128; 2],
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct DelegatorInfo {
    pub addr: String,
    pub amount: u64,
}

impl DelegatorInfo {
    pub fn new(addr: String, amount: u64) -> Self {
        DelegatorInfo { addr, amount }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct DelegatorList {
    delegators: Vec<DelegatorInfo>,
}

impl DelegatorList {
    pub fn new(delegators: Vec<DelegatorInfo>) -> Self {
        DelegatorList { delegators }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct DelegationInfo {
    pub bond: u64,
    pub bond_entries: Vec<(String, u64)>,
    pub unbond: u64,
    pub rewards: u64,
    pub return_rate: [u128; 2],
    pub global_delegation: u64,
    pub global_staking: u64,
    pub start_height: u64,
    pub end_height: u64,
    pub current_height: u64,
    pub delegation_rwd_cnt: u64,
    pub proposer_rwd_cnt: u64,
}

impl DelegationInfo {
    fn default_x() -> Self {
        Self {
            return_rate: [0, 100],
            ..Self::default()
        }
    }

    pub fn new(
        bond: u64,
        bond_entries: Vec<(String, u64)>,
        unbond: u64,
        rewards: u64,
        return_rate: [u128; 2],
        global_delegation: u64,
        global_staking: u64,
    ) -> Self {
        Self {
            bond,
            bond_entries,
            unbond,
            rewards,
            return_rate,
            global_delegation,
            global_staking,
            ..Self::default_x()
        }
    }
}
