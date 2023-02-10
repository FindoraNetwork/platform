//!
//! This module defines findora ledger/query rpc apis for server and client.
//!

use {
    ledger::staking::{
        self, StakerMemo, TendermintAddr, MAX_POWER_PERCENT_PER_VALIDATOR,
    },
    serde::{Deserialize, Serialize},
};

/// A list of basic validator information of current height
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ValidatorList {
    threshold: [u128; 2],
    validator_cnt: u64,
    cur_height: u64,
    validators: Vec<Validator>,
}

impl ValidatorList {
    #[allow(missing_docs)]
    pub fn new(cur_height: u64, validators: Vec<Validator>) -> Self {
        ValidatorList {
            threshold: MAX_POWER_PERCENT_PER_VALIDATOR,
            validator_cnt: validators.len() as u64,
            cur_height,
            validators,
        }
    }
}

/// The basic inforamtion of a validator
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Validator {
    addr: TendermintAddr,
    power: u64,
    commission_rate: [u64; 2],
    accept_delegation: bool,
    rank: u64,
    extra: StakerMemo,
}

impl Validator {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        addr: TendermintAddr,
        rank: u64,
        accept_delegation: bool,
        v: &staking::Validator,
    ) -> Self {
        Validator {
            addr,
            power: v.td_power,
            commission_rate: v.get_commission_rate(),
            accept_delegation,
            rank,
            extra: v.memo.clone(),
        }
    }
}

/// The detail information of a validator which includes
/// staking information, expected annulation, and voting power etc.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct ValidatorDetail {
    /// tendermint node address
    pub addr: TendermintAddr,
    /// `staker` for external validator
    pub kind: String,
    /// if co-singed last block
    pub is_online: bool,
    /// voting power in current findora network
    pub voting_power: u64,
    /// the rank of voting power
    pub voting_power_rank: usize,
    /// commission rate of this staker
    pub commission_rate: [u64; 2],
    /// self-staking FRA amount
    pub self_staking: u64,
    /// rewards received
    pub fra_rewards: u64,
    /// staker information
    pub memo: StakerMemo,
    /// when this node becomes validator
    pub start_height: u64,
    /// current block height
    pub cur_height: u64,
    /// block co-singed by this validator
    pub block_signed_cnt: u64,
    /// block proposed by this validator
    pub block_proposed_cnt: u64,
    /// expected annulation of thi validator
    pub validator_realtime_apy: [u128; 2],
    /// expected annulation of thi validator (f64)
    pub validator_realtime_apy_float:f64,
    /// total number of its delegators
    pub delegator_cnt: u64,
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct DelegatorInfo {
    addr: String,
    amount: u64,
}

impl DelegatorInfo {
    #[allow(missing_docs)]
    pub fn new(addr: String, amount: u64) -> Self {
        DelegatorInfo { addr, amount }
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct DelegatorList {
    delegators: Vec<DelegatorInfo>,
}

#[allow(missing_docs)]
impl DelegatorList {
    pub fn new(delegators: Vec<DelegatorInfo>) -> Self {
        DelegatorList { delegators }
    }
}

/// Delegation information of a findora account which includes
/// total bond amount, bond entries, begin and end height of delegation, and reward info etc.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct DelegationInfo {
    /// total bond amount
    pub bond: u64,
    /// per-validator bond amount
    pub bond_entries: Vec<(String, u64)>,
    /// total unbond amount
    pub unbond: u64,
    /// total rewards uncalimed
    pub rewards: u64,
    /// current return rate
    pub return_rate: [u128; 2],
    /// the total delegation amount in current findora network
    pub global_delegation: u64,
    /// the total staking amount in current findora network
    pub global_staking: u64,
    /// the block height which this delegation starts
    pub start_height: u64,
    /// the block height which this delegation ends
    pub end_height: u64,
    /// current block height of findora network
    pub current_height: u64,
    /// how many times of rewards received
    pub delegation_rwd_cnt: u64,
    /// how many times of proposing this validator has performed
    pub proposer_rwd_cnt: u64,
}

impl DelegationInfo {
    fn default_x() -> Self {
        Self {
            return_rate: [0, 100],
            ..Self::default()
        }
    }

    #[allow(missing_docs)]
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

#[allow(missing_docs)]
pub trait NetworkRoute {
    fn route(&self) -> String;

    fn with_arg(&self, arg: &dyn std::fmt::Display) -> String {
        let mut endpoint = self.route();
        endpoint += &("/".to_owned() + &arg.to_string());
        endpoint
    }

    // e.g.
    // SubmissionRoutes::TxnStatus.with_arg_template("str") = "/submit_transaction/{str}"
    fn with_arg_template(&self, arg: &str) -> String {
        let mut endpoint = self.route();
        endpoint += &("/".to_owned() + "{" + arg + "}");
        endpoint
    }
}
