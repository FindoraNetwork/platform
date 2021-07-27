//!
//! # Staking
//!
//! - manage validator information
//! - manage delegation information
//! - manage the distribution of investment income
//! - manage on-chain governance
//! - manage the official re-distribution of FRA
//!

#![deny(warnings)]
#![deny(missing_docs)]

pub mod cosig;
pub mod init;
pub mod ops;

use crate::{
    data_model::{
        Operation, Transaction, TransferAsset, TxoRef, BLACK_HOLE_PUBKEY, FRA_DECIMALS,
    },
    store::{LedgerAccess, LedgerUpdate},
};
use cosig::CoSigRule;
use cryptohash::sha256::{self, Digest};
use lazy_static::lazy_static;
use ops::{fra_distribution::FraDistributionOps, mint_fra::MintKind};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use rand_core::{CryptoRng, RngCore};
use ruc::*;
use serde::{Deserialize, Serialize};
use sha2::Digest as _;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    convert::TryFrom,
    iter::FromIterator,
    mem,
};
use zei::xfr::{
    sig::{XfrKeyPair, XfrPublicKey},
    structs::XfrAmount,
};

/// Staking entry
///
/// Init:
/// 1. set_custom_block_height
/// 2. validator_set_at_height
///
/// Usage:
/// - validator_change_power ...
/// - validator_apply_at_height
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct Staking {
    // the main logic when updating:
    // - the new validator inherits the original vote power, if any
    vi: ValidatorInfo,
    // when the end-time of delegations arrived,
    // we will try to paid the rewards until all is successful.
    di: DelegationInfo,
    // current block height in the context of tendermint.
    cur_height: BlockHeight,
    // FRA CoinBase.
    coinbase: CoinBase,
}

impl Staking {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new() -> Self {
        Staking {
            // use '0' instead of '1' to
            // avoid conflicts with initial operations
            vi: map! {B 0 => ValidatorData::default()},
            di: DelegationInfo::new(),
            cur_height: 0,
            coinbase: CoinBase::gen(),
        }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn cur_height(&self) -> BlockHeight {
        self.cur_height
    }

    ///get the delegationInfo
    pub fn delegation_info_global_amount(&self) -> Amount {
        self.di.global_amount
    }

    /// Get the validators that exactly be setted at a specified height.
    #[inline(always)]
    pub fn validator_get_at_height(&self, h: BlockHeight) -> Option<Vec<&Validator>> {
        self.vi.get(&h).map(|v| v.body.values().collect())
    }

    // Check if there is some settings on a specified height.
    #[inline(always)]
    fn validator_has_settings_at_height(&self, h: BlockHeight) -> bool {
        self.vi.contains_key(&h)
    }

    /// Set the validators that will be used for the specified height.
    #[inline(always)]
    pub fn validator_set_at_height(
        &mut self,
        h: BlockHeight,
        v: ValidatorData,
    ) -> Result<()> {
        if self.validator_has_settings_at_height(h) {
            Err(eg!("already exists"))
        } else {
            self.validator_set_at_height_force(h, v);
            Ok(())
        }
    }

    /// Set the validators that will be used for the specified height,
    /// no matter if there is an existing set of validators at that height.
    #[inline(always)]
    pub fn validator_set_at_height_force(&mut self, h: BlockHeight, v: ValidatorData) {
        self.vi.insert(h, v);
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn validator_get_current(&self) -> Option<&ValidatorData> {
        self.validator_get_effective_at_height(self.cur_height)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn validator_get_current_mut(&mut self) -> Option<&mut ValidatorData> {
        self.validator_get_effective_at_height_mut(self.cur_height)
    }

    /// Get the validators that will be used for the specified height.
    #[inline(always)]
    pub fn validator_get_effective_at_height(
        &self,
        h: BlockHeight,
    ) -> Option<&ValidatorData> {
        self.vi.range(0..=h).last().map(|(_, v)| v)
    }

    /// Remove the validators that will be used for the specified height.
    #[inline(always)]
    pub fn validator_remove_at_height(
        &mut self,
        h: BlockHeight,
    ) -> Result<Vec<Validator>> {
        self.vi
            .remove(&h)
            .map(|v| v.body.into_iter().map(|(_, v)| v).collect())
            .c(d!("not exists"))
    }

    /// Get the validators that will be used for a specified height.
    #[inline(always)]
    pub fn validator_get_effective_at_height_mut(
        &mut self,
        h: BlockHeight,
    ) -> Option<&mut ValidatorData> {
        self.vi.range_mut(0..=h).last().map(|(_, v)| v)
    }

    /// Get the validators exactly on a specified height.
    #[inline(always)]
    pub fn validator_get_at_height_mut(
        &mut self,
        h: BlockHeight,
    ) -> Option<&mut ValidatorData> {
        self.vi.get_mut(&h)
    }

    /// Make the validators at current height to be effective.
    #[inline(always)]
    pub fn validator_apply_current(&mut self) {
        let h = self.cur_height;
        self.validator_apply_at_height(h);
    }

    /// Make the validators at a specified height to be effective.
    pub fn validator_apply_at_height(&mut self, h: BlockHeight) {
        if let Some(mut prev) = self.validator_get_effective_at_height(h - 1).cloned() {
            alt!(prev.body.is_empty(), return);

            // inherit the powers of previous settings
            // if new settings were found
            if let Some(vs) = self.validator_get_at_height_mut(h) {
                vs.body.iter_mut().for_each(|(k, v)| {
                    if let Some(pv) = prev.body.remove(k) {
                        v.td_power = pv.td_power;
                    }
                });
                // out-dated validators should be removed from tendermint,
                // set its power to zero, and let tendermint know the changes
                let mut addr_map = mem::take(&mut prev.addr_td_to_app)
                    .into_iter()
                    .map(|(addr, pk)| (pk, addr))
                    .collect::<HashMap<_, _>>();
                prev.body.into_iter().for_each(|(k, mut v)| {
                    v.td_power = 0;
                    vs.addr_td_to_app.insert(pnk!(addr_map.remove(&k)), k);
                    vs.body.insert(k, v);
                });
            }
            // copy previous settings
            // if new settings were not found.
            else {
                prev.height = h;
                self.validator_set_at_height_force(h, prev);
            }

            // clean old data before current height
            self.validator_clean_before_height(h.saturating_sub(1 + UNBOND_BLOCK_CNT));
        }
    }

    // Clean validator-info older than the specified height.
    #[inline(always)]
    fn validator_clean_before_height(&mut self, h: BlockHeight) {
        self.vi = self.vi.split_off(&h);
    }

    // Clean validators with zero power
    // after they have been removed from tendermint core.
    fn validator_clean_invalid_items(&mut self) {
        let h = self.cur_height;

        if UNBOND_BLOCK_CNT > h {
            return;
        }

        if let Some(old) = self
            .validator_get_effective_at_height(h - UNBOND_BLOCK_CNT)
            .map(|ovd| {
                ovd.body
                    .iter()
                    .filter(|(_, v)| 0 == v.td_power)
                    .map(|(k, _)| *k)
                    .collect::<HashSet<_>>()
            })
        {
            if let Some(vd) = self.validator_get_current_mut() {
                vd.body = mem::take(&mut vd.body)
                    .into_iter()
                    .filter(|(k, _)| !old.contains(k))
                    .collect();
                vd.addr_td_to_app = mem::take(&mut vd.addr_td_to_app)
                    .into_iter()
                    .filter(|(_, xfr_pk)| vd.body.contains_key(xfr_pk))
                    .collect();
            }
        }
    }

    /// increase/decrease the power of a specified validator.
    fn validator_change_power(
        &mut self,
        validator: &XfrPublicKey,
        power: Power,
        decrease: bool,
    ) -> Result<()> {
        if !decrease {
            self.validator_check_power(power, validator).c(d!())?;
        }

        self.validator_get_effective_at_height_mut(self.cur_height)
            .c(d!())
            .and_then(|cur| {
                cur.body
                    .get_mut(validator)
                    .map(|v| {
                        v.td_power = alt!(
                            decrease,
                            v.td_power.saturating_sub(power),
                            v.td_power.saturating_add(power)
                        );
                    })
                    .c(d!("validator not exists"))
            })
    }

    /// Get the power of a specified validator at current term.
    #[inline(always)]
    pub fn validator_get_power(&self, vldtor: &XfrPublicKey) -> Result<Power> {
        self.validator_get_current()
            .and_then(|vd| vd.body.get(vldtor))
            .map(|v| v.td_power)
            .c(d!())
    }

    #[inline(always)]
    fn validator_check_power(
        &self,
        new_power: Amount,
        vldtor: &XfrPublicKey,
    ) -> Result<()> {
        self.validator_get_power(vldtor)
            .c(d!())
            .and_then(|power| self.validator_check_power_x(new_power, power).c(d!()))
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn validator_check_power_x(
        &self,
        new_power: Amount,
        power: Amount,
    ) -> Result<()> {
        let global_power = self.validator_global_power() + new_power;
        if MAX_TOTAL_POWER < global_power {
            return Err(eg!("global power overflow"));
        }

        if ((power + new_power) as u128)
            .checked_mul(MAX_POWER_PERCENT_PER_VALIDATOR[1])
            .c(d!())?
            > MAX_POWER_PERCENT_PER_VALIDATOR[0]
                .checked_mul(global_power as u128)
                .c(d!())?
        {
            return Err(eg!("validator power overflow"));
        }

        Ok(())
    }

    /// calculate current global vote-power
    #[inline(always)]
    pub fn validator_global_power(&self) -> Power {
        self.validator_global_power_at_height(self.cur_height)
    }

    /// calculate current global vote-power
    #[inline(always)]
    pub fn validator_global_power_at_height(&self, h: BlockHeight) -> Power {
        self.validator_get_effective_at_height(h)
            .map(|vs| vs.body.values().map(|v| v.td_power).sum())
            .unwrap_or(0)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_custom_block_height(&mut self, h: BlockHeight) {
        self.cur_height = h;
    }

    #[allow(missing_docs)]
    pub fn validator_get_delegator_list(
        &self,
        validator: TendermintAddrRef,
        start: usize,
        mut end: usize,
    ) -> Result<Vec<(&XfrPublicKey, &u64)>> {
        let validator = self.validator_td_addr_to_app_pk(validator).c(d!())?;

        if let Some(vd) = self.di.addr_map.get(&validator) {
            if start >= vd.delegators.len() || start > end {
                return Err(eg!("Index out of range"));
            }
            if end > vd.delegators.len() {
                end = vd.delegators.len();
            }

            Ok((Vec::from_iter(&vd.delegators))[start..end].to_vec())
        } else {
            Err(eg!("Not a validator or non-existing node address"))
        }
    }

    /// Start a new delegation.
    /// - increase the vote power of the co-responding validator
    ///
    /// Validator must do self-delegatation first,
    /// and its delegation end_height must be `i64::MAX`.
    ///
    /// **NOTE:** It is the caller's duty to ensure that
    /// there is enough FRAs existing in the target address(owner).
    pub fn delegate(
        &mut self,
        owner: XfrPublicKey,
        validator: TendermintAddrRef,
        am: Amount,
    ) -> Result<()> {
        let validator = self.validator_td_addr_to_app_pk(validator).c(d!())?;
        let end_height = BLOCK_HEIGHT_MAX;

        check_delegation_amount(am, true).c(d!())?;

        if self.delegation_has_addr(&validator) || owner == validator {
            // `normal scene` or `do self-delegation`
        } else {
            return Err(eg!("self-delegation has not been finished"));
        }

        let h = self.cur_height;
        let new = || Delegation {
            entries: map! {B validator => 0},
            delegators: indexmap::IndexMap::new(),
            id: owner,
            receiver_pk: None,
            start_height: h,
            end_height,
            state: DelegationState::Bond,
            rwd_amount: 0,
            rwd_detail: map! {B},
            delegation_rwd_cnt: 0,
            proposer_rwd_cnt: 0,
        };

        let d = self.di.addr_map.entry(owner).or_insert_with(new);

        if DelegationState::Paid == d.state {
            *d = new();
        }

        if let Some(set) = self.di.end_height_map.get_mut(&d.end_height) {
            set.remove(&owner);
        }

        d.end_height = end_height;
        d.state = DelegationState::Bond;

        *d.entries.entry(validator).or_insert(0) += am;

        // update delegator entries for this validator
        if let Some(vd) = self.di.addr_map.get_mut(&validator) {
            if owner != validator {
                *vd.delegators.entry(owner).or_insert(0) += am;
                vd.delegators.sort_by(|_, v1, _, v2| v2.cmp(&v1));
            }
        }

        self.di
            .end_height_map
            .entry(end_height)
            .or_insert_with(BTreeSet::new)
            .insert(owner);

        self.validator_change_power(&validator, am, false).c(d!())?;

        // global amount of all delegations
        self.di.global_amount += am;

        // principals should be added to the balance of coinbase
        self.coinbase.principal_balance += am;

        Ok(())
    }

    /// When un-delegation happens,
    /// - decrease the vote power of the co-responding validator
    pub fn undelegate(
        &mut self,
        addr: &XfrPublicKey,
        partial_undelegation: Option<&PartialUnDelegation>,
    ) -> Result<()> {
        // partial un-delegation
        if let Some(pu) = partial_undelegation {
            return self.undelegate_partially(addr, pu).c(d!());
        }

        let h = self.cur_height;
        let mut orig_h = None;
        let mut is_validator = false;

        if let Some(d) = self.di.addr_map.get_mut(addr) {
            if BLOCK_HEIGHT_MAX == d.end_height {
                if d.end_height != h {
                    orig_h = Some(d.end_height);
                    d.end_height = h + UNBOND_BLOCK_CNT;
                }
            } else {
                return Err(eg!("delegator is not bonded"));
            }
            if self.addr_is_validator(addr) {
                // clear its power when a validator propose a complete undelegation
                // > `panic` should not happen without bug[s]
                pnk!(self.validator_change_power(addr, u64::MAX, true));
                is_validator = true;
            }
        } else {
            return Err(eg!("delegator not found"));
        }

        // undelegate for the related delegators automaticlly
        if is_validator {
            let mut auto_ud_list = vec![];

            // unwrap is safe here
            let v = self
                .validator_get_current()
                .unwrap()
                .body
                .get(addr)
                .unwrap();

            // unwrap is safe here
            self.di
                .addr_map
                .get(addr)
                .unwrap()
                .delegators
                .iter()
                .for_each(|(pk, am)| {
                    auto_ud_list.push((
                        *pk,
                        PartialUnDelegation::new(
                            *am,
                            gen_random_keypair().get_pk(),
                            v.td_addr.clone(),
                        ),
                    ));
                });

            auto_ud_list.iter().for_each(|(addr, pu)| {
                ruc::info_omit!(self.undelegate_partially(addr, pu));
            });
        }

        if let Some(orig_h) = orig_h {
            self.di
                .end_height_map
                .get_mut(&orig_h)
                .map(|set| set.remove(addr));
            self.di
                .end_height_map
                .entry(h + UNBOND_BLOCK_CNT)
                .or_insert_with(BTreeSet::new)
                .insert(*addr);
        }

        Ok(())
    }

    // A partial undelegation implementation:
    // - split the original delegator to two smaller instances
    // - do a complete undelegation to the new(tmp) delegation address
    fn undelegate_partially(
        &mut self,
        addr: &XfrPublicKey,
        pu: &PartialUnDelegation,
    ) -> Result<()> {
        if self.delegation_has_addr(&pu.new_delegator_id) {
            return Err(eg!("Receiver address already exists"));
        }

        let new_tmp_delegator;
        let h = self.cur_height;
        let is_validator = self.addr_is_validator(addr);

        let target_validator = self
            .validator_td_addr_to_app_pk(&td_addr_to_string(&pu.target_validator))
            .c(d!("Invalid target validator"))?;

        if let Some(d) = self.di.addr_map.get_mut(addr) {
            if is_validator
                && STAKING_VALIDATOR_MIN_POWER > d.amount().saturating_sub(pu.am)
            {
                return Err(eg!("Requested amount exceeds limits"));
            }

            let am = d
                .entries
                .get_mut(&target_validator)
                .c(d!("Target validator does not exist"))?;

            let actual_am = if pu.am > *am {
                ruc::pd!(format!(
                    "Amount exceeds limits, requested: {}, total: {}",
                    pu.am, *am
                ));
                *am
            } else {
                pu.am
            };

            if BLOCK_HEIGHT_MAX == d.end_height {
                *am = am.saturating_sub(pu.am);
                new_tmp_delegator = Delegation {
                    entries: map! {B target_validator => actual_am},
                    delegators: indexmap::IndexMap::new(),
                    id: pu.new_delegator_id,
                    receiver_pk: Some(d.id),
                    start_height: d.start_height,
                    end_height: h + UNBOND_BLOCK_CNT,
                    state: DelegationState::Bond,
                    rwd_amount: 0,
                    rwd_detail: map! {B},
                    delegation_rwd_cnt: 0,
                    proposer_rwd_cnt: 0,
                };
            } else {
                return Err(eg!("delegator is out of bond"));
            }
        } else {
            return Err(eg!("delegator not found"));
        }

        self.di
            .addr_map
            .insert(pu.new_delegator_id, new_tmp_delegator);
        self.di
            .end_height_map
            .entry(h + UNBOND_BLOCK_CNT)
            .or_insert_with(BTreeSet::new)
            .insert(pu.new_delegator_id);

        // update delegator entries for pu target_validator
        if let Some(vd) = self.di.addr_map.get_mut(&target_validator) {
            // add new_delegator_id to delegator list
            *vd.delegators.entry(pu.new_delegator_id).or_insert(0) += pu.am;

            // update delegation amount of current address
            // make sure previous delegation amount is bigger than pu.am above.
            if let Some(am) = vd.delegators.get_mut(addr) {
                *am -= pu.am;
            }
            vd.delegators.sort_by(|_, v1, _, v2| v2.cmp(&v1));
        }

        Ok(())
    }

    #[inline(always)]
    fn delegation_clean_paid(
        &mut self,
        addr: &XfrPublicKey,
        h: &BlockHeight,
    ) -> Result<Delegation> {
        let d = self.di.addr_map.remove(addr).c(d!("not exists"))?;
        if d.state == DelegationState::Paid {
            self.di
                .end_height_map
                .get_mut(h)
                .map(|addrs| addrs.remove(addr));
            Ok(d)
        } else {
            // we assume that this probability is very low
            self.di.addr_map.insert(addr.to_owned(), d);
            Err(eg!("unpaid delegation"))
        }
    }

    /// Expand delegation scale
    pub fn delegation_extend(
        &mut self,
        owner: &XfrPublicKey,
        end_height: BlockHeight,
    ) -> Result<()> {
        let addr = owner;
        let d = if let Some(d) = self.di.addr_map.get_mut(addr) {
            d
        } else {
            return Err(eg!("not exists"));
        };

        if end_height > d.end_height {
            let orig_h = d.end_height;
            d.end_height = end_height;
            self.di
                .end_height_map
                .get_mut(&orig_h)
                .c(d!())?
                .remove(addr);
            self.di
                .end_height_map
                .entry(end_height)
                .or_insert_with(BTreeSet::new)
                .insert(addr.to_owned());
            Ok(())
        } else {
            Err(eg!("new end_height must be bigger than the old one"))
        }
    }

    /// Get the delegation instance of `addr`.
    #[inline(always)]
    pub fn delegation_get(&self, addr: &XfrPublicKey) -> Option<&Delegation> {
        self.di.addr_map.get(&addr)
    }

    /// Get the delegation instance of `addr`.
    #[inline(always)]
    pub fn delegation_get_mut(
        &mut self,
        addr: &XfrPublicKey,
    ) -> Option<&mut Delegation> {
        self.di.addr_map.get_mut(&addr)
    }

    /// Check if the `addr` is in a state of delegation
    #[inline(always)]
    pub fn delegation_has_addr(&self, addr: &XfrPublicKey) -> bool {
        self.di.addr_map.contains_key(&addr)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_principal(&self) -> HashMap<XfrPublicKey, Amount> {
        self.delegation_get_global_principal_before_height(self.cur_height)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_principal_before_height(
        &self,
        h: BlockHeight,
    ) -> HashMap<XfrPublicKey, Amount> {
        self.delegation_get_freed_before_height(h)
            .into_iter()
            .map(|(k, d)| (k, d.amount()))
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_principal_with_receiver(
        &self,
    ) -> HashMap<XfrPublicKey, (Amount, Option<XfrPublicKey>)> {
        self.delegation_get_global_principal_before_height_with_receiver(self.cur_height)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_principal_before_height_with_receiver(
        &self,
        h: BlockHeight,
    ) -> HashMap<XfrPublicKey, (Amount, Option<XfrPublicKey>)> {
        self.delegation_get_freed_before_height(h)
            .into_iter()
            .map(|(k, d)| (k, (d.amount(), d.receiver_pk)))
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_rewards(&self) -> HashMap<XfrPublicKey, Amount> {
        self.delegation_get_global_rewards_before_height(self.cur_height)
    }

    /// Query delegation rewards before a specified height(included).
    #[inline(always)]
    pub fn delegation_get_global_rewards_before_height(
        &self,
        h: BlockHeight,
    ) -> HashMap<XfrPublicKey, Amount> {
        self.delegation_get_freed_before_height(h)
            .into_iter()
            .filter(|(_, d)| 0 < d.rwd_amount)
            .map(|(k, d)| (k, d.rwd_amount))
            .collect()
    }

    /// Query delegation rewards.
    #[inline(always)]
    pub fn delegation_get_rewards(&self, pk: &XfrPublicKey) -> Result<Amount> {
        self.di.addr_map.get(pk).map(|d| d.rwd_amount).c(d!())
    }

    /// Query delegation principal.
    #[inline(always)]
    pub fn delegation_get_principal(&self, pk: &XfrPublicKey) -> Result<Amount> {
        self.di.addr_map.get(pk).map(|d| d.amount()).c(d!())
    }

    /// Query all freed delegations.
    #[inline(always)]
    pub fn delegation_get_freed(&self) -> HashMap<XfrPublicKey, &Delegation> {
        self.delegation_get_freed_before_height(self.cur_height)
    }

    /// Query freed delegations before a specified height(included).
    #[inline(always)]
    pub fn delegation_get_freed_before_height(
        &self,
        h: BlockHeight,
    ) -> HashMap<XfrPublicKey, &Delegation> {
        self.di
            .end_height_map
            .range(..=h)
            .flat_map(|(_, addrs)| {
                addrs
                    .iter()
                    .flat_map(|addr| self.di.addr_map.get(addr).map(|d| (*addr, d)))
                    .filter(|(_, d)| matches!(d.state, DelegationState::Free))
            })
            .collect()
    }

    /// Clean delegation states along with each new block.
    #[inline(always)]
    pub fn delegation_process(&mut self) {
        let h = self.cur_height;

        self.di
            .end_height_map
            .range(..=h)
            .map(|(_, addr)| addr)
            .flatten()
            .copied()
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|addr| {
                let entries = if let Some(d) = self.di.addr_map.get_mut(&addr) {
                    if DelegationState::Bond == d.state {
                        d.state = DelegationState::Free;
                        Some(d.entries.clone())
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some(e) = entries {
                    e.into_iter().for_each(|(v, am)| {
                        // - reduce the power of the target validator
                        ruc::info_omit!(self.validator_change_power(&v, am, true));

                        // - reduce global amount of global delegations
                        self.di.global_amount -= am;

                        if let Some(vd) = self.di.addr_map.get_mut(&v) {
                            vd.delegators.remove(&addr);
                            vd.delegators.sort_by(|_, v1, _, v2| v2.cmp(&v1));
                        }
                    });
                }
            });

        self.delegation_process_finished_before_height(h);

        self.validator_clean_invalid_items();
    }

    // call this when:
    // - the unbond period expired
    // - rewards have been paid successfully.
    //
    // @param h: included
    fn delegation_process_finished_before_height(&mut self, h: BlockHeight) {
        self.di
            .end_height_map
            .range(0..=h)
            .map(|(k, v)| (k.to_owned(), (*v).clone()))
            .collect::<Vec<_>>()
            .iter()
            .for_each(|(h, addrs)| {
                addrs.iter().for_each(|addr| {
                    ruc::info_omit!(self.delegation_clean_paid(addr, h));
                });
                // this unwrap is safe
                if self.di.end_height_map.get(&h).unwrap().is_empty() {
                    self.di.end_height_map.remove(&h);
                }
            });
    }

    /// Penalize the FRAs by a specified address.
    #[inline(always)]
    pub fn governance_penalty(
        &mut self,
        addr: TendermintAddrRef,
        percent: [u64; 2],
    ) -> Result<()> {
        self.validator_td_addr_to_app_pk(addr)
            .c(d!())
            .and_then(|pk| self.governance_penalty_by_pubkey(&pk, percent).c(d!()))
    }

    fn governance_penalty_by_pubkey(
        &mut self,
        addr: &XfrPublicKey,
        percent: [u64; 2],
    ) -> Result<()> {
        if 0 == percent[1] || percent[1] > i64::MAX as Amount || percent[0] > percent[1]
        {
            return Err(eg!());
        }

        // punish itself
        let am = self.delegation_get(addr).c(d!())?.amount();
        self.governance_penalty_sub_amount(addr, am * percent[0] / percent[1])
            .c(d!())?;

        if self.addr_is_validator(addr) {
            // punish vote power if it is a validator
            self.validator_get_power(addr).c(d!()).and_then(|power| {
                self.validator_change_power(addr, power * percent[0] / percent[1], true)
                    .c(d!())
            })?;

            // punish related delegators
            let pl = || {
                self.di
                    .addr_map
                    .iter()
                    .filter(|(pk, d)| *pk != addr && d.validator_entry_exists(addr))
                    .map(|(pk, d)| (*pk, d.amount() * percent[0] / percent[1]))
                    .collect::<Vec<_>>()
            };

            pl().into_iter().for_each(|(pk, p_am)| {
                ruc::info_omit!(self.governance_penalty_sub_amount(&pk, p_am));
            });
        }

        Ok(())
    }

    #[inline(always)]
    fn governance_penalty_sub_amount(
        &mut self,
        addr: &XfrPublicKey,
        mut am: Amount,
    ) -> Result<()> {
        let d = if let Some(d) = self.di.addr_map.get_mut(addr) {
            d
        } else {
            return Err(eg!("not exists"));
        };

        let bond_am = d.amount();
        d.rwd_detail
            .entry(self.cur_height)
            .or_insert(DelegationRwdDetail {
                bond: bond_am,
                amount: 0,
                penalty_amount: 0,
                return_rate: None,
                commission_rate: None,
                global_delegation_percent: None,
                block_height: self.cur_height,
            })
            .penalty_amount += am;

        if DelegationState::Paid == d.state {
            return Err(eg!("delegation has been paid"));
        } else {
            // NOTE:
            // punish principal first
            d.entries.values_mut().for_each(|v| {
                if 0 < am {
                    let i = *v;
                    *v = v.saturating_sub(am);
                    am = am.saturating_sub(i);
                }
            });
            // NOTE:
            // punish rewards if principal is not enough
            d.rwd_amount = d.rwd_amount.saturating_sub(am);
        }

        Ok(())
    }

    /// Look up the `XfrPublicKey`
    /// co-responding to a specified 'tendermint node address'.
    #[inline(always)]
    pub fn validator_td_addr_to_app_pk(
        &self,
        addr: TendermintAddrRef,
    ) -> Result<XfrPublicKey> {
        self.validator_get_current()
            .c(d!())
            .and_then(|vd| vd.addr_td_to_app.get(addr).copied().c(d!()))
    }

    /// Lookup up the 'tendermint node address' by the 'XfrPublicKey'
    #[inline(always)]
    pub fn validator_app_pk_to_td_addr(
        &self,
        key: &XfrPublicKey,
    ) -> Result<TendermintAddr> {
        self.validator_get_current().c(d!()).and_then(|vd| {
            vd.body
                .get(key)
                .c(d!())
                .map(|v| td_addr_to_string(&v.td_addr))
        })
    }

    /// Generate sha256 digest.
    #[inline(always)]
    pub fn hash(&self) -> Result<Digest> {
        bincode::serialize(self)
            .c(d!())
            .map(|bytes| sha256::hash(&bytes))
    }

    /// Add new fra distribution plan.
    pub fn coinbase_config_fra_distribution(
        &mut self,
        ops: FraDistributionOps,
    ) -> Result<()> {
        let h = ops.hash().c(d!())?;

        if self.coinbase.distribution_hist.contains(&h) {
            return Err(eg!("already exists"));
        }

        // Update fra distribution history first.
        self.coinbase.distribution_hist.insert(h);

        let mut v;
        for (k, am) in ops.data.alloc_table.into_iter() {
            v = self.coinbase.distribution_plan.entry(k).or_insert(0);
            *v = v.checked_add(am).c(d!("overflow"))?;
        }

        Ok(())
    }

    /// Do the final payment on staking structures.
    ///
    /// NOTE:
    /// this function also serves as the checker of invalid tx
    /// sent from COIN_BASE_PRINCIPAL, every tx that can
    /// not pass this checker will be regarded as invalid.
    pub fn coinbase_check_and_pay(&mut self, tx: &Transaction) -> Result<()> {
        if !is_coinbase_tx(tx) {
            return Ok(());
        }

        self.coinbase_pay(tx).c(d!())
    }

    fn coinbase_pay(&mut self, tx: &Transaction) -> Result<()> {
        let mut cbb = self.coinbase.balance;
        let mut cbb_principal = self.coinbase.principal_balance;

        macro_rules! cbsub {
            ($am: expr) => {
                // has been checked in abci
                cbb = pnk!(cbb.checked_sub($am));
            };
            (@$am: expr) => {
                cbb_principal = pnk!(cbb_principal.checked_sub($am));
            };
        }

        for o in tx.body.operations.iter() {
            if let Operation::MintFra(ref ops) = o {
                for et in ops.entries.iter() {
                    if let Some(d) = self.delegation_get_mut(&et.target_pk) {
                        if DelegationState::Free == d.state {
                            if MintKind::UnStake == et.kind && d.amount() == et.amount {
                                cbsub!(@et.amount);
                                d.clean_amount();
                            } else if MintKind::Claim == et.kind
                                && d.rwd_amount == et.amount
                            {
                                cbsub!(et.amount);
                                d.rwd_amount = 0;
                            }
                            if 0 == d.rwd_amount && 0 == d.amount() {
                                d.state = DelegationState::Paid;
                            }
                        }
                    }
                    if let Some(am) =
                        self.coinbase.distribution_plan.get_mut(&et.target_pk)
                    {
                        if MintKind::Claim == et.kind && *am == et.amount {
                            cbsub!(et.amount);
                            *am = 0;
                        }
                    }
                }
            }
        }

        // clean 'completely paid' item
        self.coinbase.distribution_plan =
            mem::take(&mut self.coinbase.distribution_plan)
                .into_iter()
                .filter(|(_, am)| 0 < *am)
                .collect();

        self.coinbase.balance = cbb;
        self.coinbase.principal_balance = cbb_principal;

        Ok(())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn addr_is_in_distribution_plan(&self, pk: &XfrPublicKey) -> bool {
        self.coinbase.distribution_plan.contains_key(pk)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn addr_is_in_freed_delegation(&self, pk: &XfrPublicKey) -> bool {
        if let Some(dlg) = self.di.addr_map.get(pk) {
            matches!(dlg.state, DelegationState::Free)
        } else {
            false
        }
    }

    #[inline(always)]
    fn addr_is_validator(&self, pk: &XfrPublicKey) -> bool {
        self.validator_get_current()
            .map(|v| v.body.contains_key(pk))
            .unwrap_or(false)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn fra_distribution_get_plan(&self) -> &BTreeMap<XfrPublicKey, Amount> {
        &self.coinbase.distribution_plan
    }

    /// A helper for setting block rewards in ABCI.
    pub fn set_last_block_rewards<RNG: RngCore + CryptoRng>(
        la: &mut (impl LedgerUpdate<RNG> + LedgerAccess),
        addr: TendermintAddrRef,
        block_vote_percent: Option<[Power; 2]>,
    ) -> Result<()> {
        let gdp = Self::get_global_delegation_percent(la);
        let return_rate = Self::get_block_rewards_rate(la);

        let me = la.get_staking_mut();
        let pk = me.validator_td_addr_to_app_pk(addr).c(d!())?;

        let commission_rate = if let Some(Some(v)) =
            me.validator_get_current().map(|vd| vd.body.get(&pk))
        {
            v.commission_rate
        } else {
            return Err(eg!("not validator"));
        };

        let h = me.cur_height;
        let commissions = me
            .di
            .addr_map
            .values_mut()
            .filter(|d| d.validator_entry_exists(&pk))
            .map(|d| {
                d.set_delegation_rewards(&pk, h, return_rate, commission_rate, gdp, true)
            })
            .collect::<Result<Vec<_>>>()
            .c(d!())?;

        if let Some(v) = me.delegation_get_mut(&pk) {
            v.rwd_amount = v.rwd_amount.saturating_add(commissions.into_iter().sum());
        }

        if let Some(vote_percent) = block_vote_percent {
            me.set_proposer_rewards(&pk, vote_percent).c(d!())?;
        }

        Ok(())
    }

    /// Return rate defination for delegation rewards.
    #[inline(always)]
    pub fn get_block_rewards_rate(la: &impl LedgerAccess) -> [u128; 2] {
        let p = Self::get_global_delegation_percent(la);
        let p = [p[0] as u128, p[1] as u128];

        // This is an equal conversion of `1 / p% * 0.0201`
        let mut a0 = p[1] * 201;
        let mut a1 = p[0] * 10000;

        if a0 * 100 > a1 * 105 {
            // max value: 105%
            a0 = 105;
            a1 = 100;
        } else if a0 * 50 < a1 {
            // min value: 2%
            a0 = 2;
            a1 = 100;
        }

        [a0, a1]
    }

    fn set_proposer_rewards(
        &mut self,
        proposer: &XfrPublicKey,
        vote_percent: [u64; 2],
    ) -> Result<()> {
        let p = Self::get_proposer_rewards_rate(vote_percent).c(d!())?;
        let h = self.cur_height;
        self.delegation_get_mut(proposer)
            .c(d!())
            .and_then(|d| {
                d.set_delegation_rewards(proposer, h, p, [0, 100], [0, 0], false)
                    .c(d!())
            })
            .map(|_| ())
    }

    fn get_proposer_rewards_rate(vote_percent: [u64; 2]) -> Result<[u128; 2]> {
        let p = [vote_percent[0] as u128, vote_percent[1] as u128];
        if p[0] > p[1] || 0 == p[1] {
            let msg = format!("Invalid power percent: {}/{}", p[0], p[1]);
            return Err(eg!(msg));
        }
        for ([low, high], rate) in PROPOSER_REWARDS_RATE_RULE.iter().copied() {
            if p[0] * 100_0000 < p[1] * high && p[0] * 100_0000 >= p[1] * low {
                return Ok([rate, 100]);
            }
        }
        Err(eg!(@vote_percent))
    }

    /// Claim delegation rewards.
    pub fn claim(&mut self, pk: XfrPublicKey, am: Option<Amount>) -> Result<()> {
        let am = self.delegation_get_mut(&pk).c(d!()).and_then(|d| {
            if DelegationState::Paid == d.state {
                return Err(eg!("try to claim paid rewards"));
            }
            let am = if let Some(am) = am {
                if am > d.rwd_amount {
                    return Err(eg!("claim amount exceed total rewards"));
                }
                am
            } else {
                d.rwd_amount
            };
            d.rwd_amount -= am;
            Ok(am)
        })?;

        *self.coinbase.distribution_plan.entry(pk).or_insert(0) += am;

        Ok(())
    }

    /// new validators from public staking operations
    pub fn validator_add_staker(&mut self, h: BlockHeight, v: Validator) -> Result<()> {
        if let Some(vd) = self.validator_get_effective_at_height(h) {
            if vd.body.contains_key(&v.id)
                || vd
                    .addr_td_to_app
                    .contains_key(&td_addr_to_string(&v.td_addr))
            {
                return Err(eg!("already exists"));
            }

            let mut vd = vd.clone();
            vd.addr_td_to_app
                .insert(td_addr_to_string(&v.td_addr), v.id);
            vd.body.insert(v.id, v);

            self.validator_set_at_height_force(h, vd);
        } else {
            return Err(eg!("system error: no initial settings"));
        }

        Ok(())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn coinbase_balance(&self) -> Amount {
        self.coinbase.balance
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn coinbase_principal_balance(&self) -> Amount {
        self.coinbase.principal_balance
    }

    // Total amount of all freed FRAs, aka 'are not being locked in any way'.
    #[inline(always)]
    fn get_global_unlocked_amount(la: &impl LedgerAccess) -> Amount {
        FRA_TOTAL_AMOUNT
            - FF_PK_LIST
                .iter()
                .chain([*BLACK_HOLE_PUBKEY].iter())
                .map(|pk| get_nonconfidential_balance(la, pk))
                .sum::<Amount>()
            - la.get_staking().coinbase_balance()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_global_delegation_percent(la: &impl LedgerAccess) -> [u64; 2] {
        [
            la.get_staking().get_global_delegation_amount(),
            Self::get_global_unlocked_amount(la),
        ]
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_global_delegation_amount(&self) -> Amount {
        self.di.global_amount
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_global_delegation_amount_mut(&mut self) -> &mut Amount {
        &mut self.di.global_amount
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Reserved accounts of Findora Foundation.
pub const FF_ADDR_LIST: [&str; 8] = [
    "fra1s9c6p0656as48w8su2gxntc3zfuud7m66847j6yh7n8wezazws3s68p0m9",
    "fra1zjfttcnvyv9ypy2d4rcg7t4tw8n88fsdzpggr0y2h827kx5qxmjshwrlx7",
    "fra18rfyc9vfyacssmr5x7ku7udyd5j5vmfkfejkycr06e4as8x7n3dqwlrjrc",
    "fra1kvf8z5f5m8wmp2wfkscds45xv3yp384eszu2mpre836x09mq5cqsknltvj",
    "fra1w8s3e7v5a78623t8cq43uejtw90yzd0xctpwv63um5amtv72detq95v0dy",
    "fra1ukju0dhmx0sjwzcgjzgg3e7n6f755jkkfl9akq4hleulds9a0hgq4uzcp5",
    "fra1mjdr0mgn2e0670hxptpzu9tmf0ary8yj8nv90znjspwdupv9aacqwrg3dx",
    "fra1whn756rtqt3gpsmdlw6pvns75xdh3ttqslvxaf7eefwa83pcnlhsree9gv",
];

lazy_static! {
    /// Reserved accounts of Findora Foundation or System.
    pub static ref FF_PK_LIST: Vec<XfrPublicKey> = FF_ADDR_LIST
        .iter()
        .map(|addr| pnk!(wallet::public_key_from_bech32(addr)))
        .collect();
}

/// SEE:
/// - https://www.notion.so/findora/PoS-Stage-1-Consensus-Rewards-Penalties-72f5c9a697ff461c89c3728e34348834#3d2f1b8ff8244632b715abdd42b6a67b
pub const PROPOSER_REWARDS_RATE_RULE: [([u128; 2], u128); 6] = [
    ([0, 66_6667], 0),
    ([66_6667, 75_0000], 1),
    ([75_0000, 83_3333], 2),
    ([83_3333, 91_6667], 3),
    ([91_6667, 100_0000], 4),
    ([100_0000, 100_0001], 5),
];

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Apply new validator config every N blocks.
///
/// Update the validator list every 4 blocks to ensure that
/// the validator list obtained from `abci::LastCommitInfo` is exactly
/// the same as the current block.
/// So we can use it to filter out non-existing entries.
pub const VALIDATOR_UPDATE_BLOCK_ITV: i64 = 4;

/// How many FRA units per FRA
pub const FRA: Amount = 10_u64.pow(FRA_DECIMALS as u32);

/// Total amount of FRA-units issuance.
pub const FRA_TOTAL_AMOUNT: Amount = 210_0000_0000 * FRA;

/// Minimum allowable delegation amount.
pub const MIN_DELEGATION_AMOUNT: Amount = 1;
/// Maximum allowable delegation amount.
pub const MAX_DELEGATION_AMOUNT: Amount = FRA_TOTAL_AMOUNT;

/// The minimum investment to become a validator through staking.
pub const STAKING_VALIDATOR_MIN_POWER: Power = 88_8888 * FRA;

/// The highest height in the context of tendermint.
pub const BLOCK_HEIGHT_MAX: u64 = i64::MAX as u64;

/// A limitation from
/// [tendermint](https://docs.tendermint.com/v0.33/spec/abci/apps.html#validator-updates)
///
/// > Note that the maximum global power of the validator set
/// > is bounded by MaxTotalVotingPower = MaxInt64 / 8.
/// > Applications are responsible for ensuring
/// > they do not make changes to the validator set
/// > that cause it to exceed this limit.
pub const MAX_TOTAL_POWER: Amount = Amount::MAX / 8;

/// The max vote power of any validator
/// can not exceed 20% of global power.
pub const MAX_POWER_PERCENT_PER_VALIDATOR: [u128; 2] = [1, 5];

/// Block time interval, in seconds.
#[cfg(not(any(feature = "debug_env", feature = "abci_mock")))]
pub const BLOCK_INTERVAL: u64 = 15 + 1;

/// used in test/mock env
#[cfg(any(feature = "debug_env", feature = "abci_mock"))]
pub const BLOCK_INTERVAL: u64 = 5 + 1;

/// The lock time after the delegation expires, about 21 days.
#[cfg(not(any(feature = "debug_env", feature = "abci_mock")))]
pub const UNBOND_BLOCK_CNT: u64 = 3600 * 24 * 21 / BLOCK_INTERVAL;

/// used in test/mock env
#[cfg(any(feature = "debug_env", feature = "abci_mock"))]
pub const UNBOND_BLOCK_CNT: u64 = 5;

// minimal number of validators
pub(crate) const VALIDATORS_MIN: usize = 6;

/// The minimum weight threshold required
/// when updating validator information, 2/3.
pub const COSIG_THRESHOLD_DEFAULT: [u64; 2] = [2, 3];

/// self-description of staker
pub type StakerMemo = String;

/// block height of tendermint
pub type BlockHeight = u64;

type Amount = u64;
type Power = u64;

/// Node PubKey in base64 format
pub type TendermintPubKey = String;
type TendermintPubKeyRef<'a> = &'a str;

/// sha256(pubkey)[:20] in hex format
pub type TendermintAddr = String;
/// ref `TendermintAddr`
pub type TendermintAddrRef<'a> = &'a str;

/// sha256(pubkey)[:20]
pub type TendermintAddrBytes = Vec<u8>;
// type TendermintAddrBytesRef<'a> = &'a [u8];

type ValidatorInfo = BTreeMap<BlockHeight, ValidatorData>;

/// Data of the effective validators on a specified height.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ValidatorData {
    pub(crate) height: BlockHeight,
    pub(crate) cosig_rule: CoSigRule,
    /// major data of validators.
    pub body: BTreeMap<XfrPublicKey, Validator>,
    // <tendermint validator address> => XfrPublicKey
    addr_td_to_app: BTreeMap<TendermintAddr, XfrPublicKey>,
}

impl Default for ValidatorData {
    fn default() -> Self {
        ValidatorData {
            height: 1,
            cosig_rule: pnk!(Self::gen_cosig_rule()),
            body: BTreeMap::new(),
            addr_td_to_app: BTreeMap::new(),
        }
    }
}

impl ValidatorData {
    #[allow(missing_docs)]
    pub fn new(h: BlockHeight, v_set: Vec<Validator>) -> Result<Self> {
        if h < 1 {
            return Err(eg!("invalid start height"));
        }

        let mut body = BTreeMap::new();
        let mut addr_td_to_app = BTreeMap::new();
        for v in v_set.into_iter() {
            addr_td_to_app.insert(td_pubkey_to_td_addr(&v.td_pubkey), v.id);
            if body.insert(v.id, v).is_some() {
                return Err(eg!("duplicate entries"));
            }
        }

        let cosig_rule = Self::gen_cosig_rule().c(d!())?;

        Ok(ValidatorData {
            height: h,
            cosig_rule,
            body,
            addr_td_to_app,
        })
    }

    fn gen_cosig_rule() -> Result<CoSigRule> {
        CoSigRule::new(COSIG_THRESHOLD_DEFAULT)
    }

    /// The initial weight of every validators is equal(vote power == 1).
    pub fn set_cosig_rule(&mut self) -> Result<()> {
        Self::gen_cosig_rule().c(d!()).map(|rule| {
            self.cosig_rule = rule;
        })
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_cosig_rule(&self) -> &CoSigRule {
        &self.cosig_rule
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_cosig_rule_mut(&mut self) -> &mut CoSigRule {
        &mut self.cosig_rule
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_validators(&self) -> &BTreeMap<XfrPublicKey, Validator> {
        &self.body
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_validator_by_id(&self, id: &XfrPublicKey) -> Option<&Validator> {
        self.body.get(id)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_powered_validator_by_id(&self, id: &XfrPublicKey) -> Option<&Validator> {
        self.get_validator_by_id(id)
            .and_then(|v| alt!(0 < v.td_power, Some(v), None))
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_validator_addr_map(&self) -> &BTreeMap<TendermintAddr, XfrPublicKey> {
        &self.addr_td_to_app
    }
}

// the same address is not allowed to delegate twice at the same time,
// so it is feasible to use `XfrPublicKey` as the map key.
#[derive(Clone, Debug, Default, Eq, PartialEq, Deserialize, Serialize)]
struct DelegationInfo {
    global_amount: Amount,
    addr_map: BTreeMap<XfrPublicKey, Delegation>,
    end_height_map: BTreeMap<BlockHeight, BTreeSet<XfrPublicKey>>,
}

impl DelegationInfo {
    fn new() -> Self {
        DelegationInfo {
            global_amount: 0,
            addr_map: BTreeMap::new(),
            end_height_map: BTreeMap::new(),
        }
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum ValidatorKind {
    Staker,
    Initor,
}

impl std::fmt::Display for ValidatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValidatorKind::Staker => write!(f, "Staker"),
            ValidatorKind::Initor => write!(f, "Initor"),
        }
    }
}

/// Validator info
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Validator {
    /// public key of validator, aka 'Validator ID'.
    ///
    /// staking rewards will be paid to this addr
    /// - eg.. self-delegation rewards
    /// - eg.. block rewards
    pub id: XfrPublicKey,
    /// pubkey in the context of tendermint
    pub td_pubkey: Vec<u8>,
    /// node address in the context of tendermint
    pub td_addr: Vec<u8>,
    /// vote power in the context of Staking
    pub td_power: Amount,
    // During registration the Validator,
    // Candidate/Validator will specifiy a % commission which will be publicly recorded on the blockchain,
    // so FRA owners can make an informed choice on which validator to use;
    // % commision is the % of FRA incentives the validator will take out as a commission fee
    // for helping FRA owners stake their tokens.
    commission_rate: [u64; 2],
    /// optional descriptive information
    pub memo: Option<StakerMemo>,
    kind: ValidatorKind,
    /// use this field to mark
    /// if this validator signed last block
    pub signed_last_block: bool,
    /// how many blocks has the validator signed
    pub signed_cnt: u64,
}

impl Validator {
    #[allow(missing_docs)]
    pub fn new(
        td_pubkey: Vec<u8>,
        td_power: Amount,
        id: XfrPublicKey,
        commission_rate: [u64; 2],
        memo: Option<StakerMemo>,
        kind: ValidatorKind,
    ) -> Result<Self> {
        if 0 == commission_rate[1] || commission_rate[0] > commission_rate[1] {
            return Err(eg!());
        }
        let td_addr = td_pubkey_to_td_addr_bytes(&td_pubkey);
        Ok(Validator {
            td_pubkey,
            td_addr,
            td_power,
            id,
            commission_rate,
            memo,
            kind,
            signed_last_block: false,
            signed_cnt: 0,
        })
    }

    /// use this fn when propose an advanced `Delegation`, aka Staking.
    pub fn new_staker(
        td_pubkey: Vec<u8>,
        id: XfrPublicKey,
        commission_rate: [u64; 2],
        memo: Option<StakerMemo>,
    ) -> Result<Self> {
        Self::new(
            td_pubkey,
            0,
            id,
            commission_rate,
            memo,
            ValidatorKind::Staker,
        )
        .c(d!())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn kind(&self) -> String {
        self.kind.to_string()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_commission_rate(&self) -> [u64; 2] {
        self.commission_rate
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn staking_is_basic_valid(&self) -> bool {
        self.td_power == 0
            && self.td_addr == td_pubkey_to_td_addr_bytes(&self.td_pubkey)
            && self.commission_rate[0] < self.commission_rate[1]
    }
}

/// FRA delegation, include:
/// - user delegation
/// - validator's self-delegation
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Delegation {
    /// - the target validator
    /// - `NonConfidential` FRAs amount
    pub entries: BTreeMap<XfrPublicKey, Amount>,

    /// - delegator entries
    /// - only valid for a validator
    pub delegators: indexmap::IndexMap<XfrPublicKey, Amount>,

    /// delegation rewards will be paid to this pk by default
    pub id: XfrPublicKey,
    /// optional receiver address,
    /// if this one exists, tokens will be paid to it instead of id
    pub receiver_pk: Option<XfrPublicKey>,
    /// the joint height of the delegtator
    pub start_height: BlockHeight,
    /// the height at which the delegation ends
    ///
    /// **NOTE:** before users can actually get the rewards,
    /// they need to wait for an extra `UNBOND_BLOCK_CNT` period
    pub end_height: BlockHeight,
    #[allow(missing_docs)]
    pub state: DelegationState,
    /// set this field when `Bond` state finished
    pub rwd_amount: Amount,
    /// rewards history
    pub rwd_detail: BTreeMap<BlockHeight, DelegationRwdDetail>,
    /// how many times you get proposer rewards
    pub proposer_rwd_cnt: u64,
    /// how many times you get delegation rewards
    pub delegation_rwd_cnt: u64,
}

/// Detail of each reward entry.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct DelegationRwdDetail {
    bond: Amount,
    amount: Amount,
    penalty_amount: Amount,
    return_rate: Option<[u128; 2]>,
    commission_rate: Option<[u64; 2]>,
    global_delegation_percent: Option<[u64; 2]>,
    block_height: BlockHeight,
}

impl Delegation {
    /// Total amout of a delegator.
    #[inline(always)]
    pub fn amount(&self) -> Amount {
        self.entries.values().sum()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn start_height(&self) -> BlockHeight {
        self.start_height
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn end_height(&self) -> BlockHeight {
        self.end_height
    }

    #[inline(always)]
    fn validator_entry(&self, validator: &XfrPublicKey) -> Result<Amount> {
        self.entries.get(validator).copied().c(d!())
    }

    #[inline(always)]
    fn validator_entry_exists(&self, validator: &XfrPublicKey) -> bool {
        self.entries.contains_key(validator)
    }

    // #[inline(always)]
    // fn validator_entry_mut(&mut self, validator: &XfrPublicKey) -> Result<&mut Amount> {
    //     self.entries.get_mut(validator).c(d!())
    // }

    #[inline(always)]
    fn clean_amount(&mut self) {
        self.entries.values_mut().for_each(|v| {
            *v = 0;
        });
    }

    // > **NOTE:**
    // > use 'AssignAdd' instead of 'Assign'
    // > to keep compatible with the logic of governance penalty.
    fn set_delegation_rewards(
        &mut self,
        validator: &XfrPublicKey,
        cur_height: BlockHeight,
        return_rate: [u128; 2],
        commission_rate: [u64; 2],
        global_delegation_percent: [u64; 2],
        is_delegation_rwd: bool,
    ) -> Result<u64> {
        if self.end_height < cur_height || DelegationState::Bond != self.state {
            return Ok(0);
        }

        if 0 == commission_rate[1] || commission_rate[0] > commission_rate[1] {
            return Err(eg!());
        }

        if is_delegation_rwd {
            self.delegation_rwd_cnt += 1;
        } else {
            self.proposer_rwd_cnt += 1;
        }

        self.validator_entry(validator)
            .c(d!())
            .and_then(|mut am| {
                if 0 < am {
                    // APY
                    am += self.rwd_amount.saturating_mul(am) / self.amount();
                    calculate_delegation_rewards(am, return_rate).c(d!())
                } else {
                    Err(eg!())
                }
            })
            .and_then(|mut n| {
                let commission =
                    n.saturating_mul(commission_rate[0]) / commission_rate[1];
                n = n.checked_sub(commission).c(d!())?;
                if is_delegation_rwd {
                    self.rwd_detail.insert(
                        cur_height,
                        DelegationRwdDetail {
                            bond: self.amount(),
                            amount: n,
                            penalty_amount: 0,
                            return_rate: Some(return_rate),
                            commission_rate: Some(commission_rate),
                            global_delegation_percent: Some(global_delegation_percent),
                            block_height: cur_height,
                        },
                    );
                }
                self.rwd_amount.checked_add(n).c(d!()).map(|i| {
                    self.rwd_amount = i;
                    commission
                })
            })
    }
}

/// Calculate the amount(in FRA units) that
/// should be paid to the owner of this delegation.
pub fn calculate_delegation_rewards(
    amount: Amount,
    return_rate: [u128; 2],
) -> Result<Amount> {
    let am = amount as u128;
    let block_itv = BLOCK_INTERVAL as u128;

    am.checked_mul(return_rate[0])
        .and_then(|i| i.checked_mul(block_itv))
        .and_then(|i| {
            return_rate[1]
                .checked_mul(365 * 24 * 3600)
                .and_then(|j| i.checked_div(j))
        })
        .c(d!("overflow"))
        .and_then(|n| u64::try_from(n).c(d!()))
}

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum DelegationState {
    /// during delegation, include extra 21 days
    Bond,
    /// it's time to pay principals and rewards
    Free,
    /// principals and rewards have been paid successfully
    Paid,
}

impl Default for DelegationState {
    fn default() -> Self {
        DelegationState::Bond
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub struct PartialUnDelegation {
    am: Amount,
    new_delegator_id: XfrPublicKey,
    target_validator: TendermintAddrBytes,
}

impl PartialUnDelegation {
    #[allow(missing_docs)]
    pub fn new(
        am: Amount,
        new_delegator_id: XfrPublicKey,
        target_validator: TendermintAddrBytes,
    ) -> Self {
        PartialUnDelegation {
            am,
            new_delegator_id,
            target_validator,
        }
    }
}

// All transactions sent from CoinBase must support idempotence.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
struct CoinBase {
    distribution_hist: BTreeSet<Digest>,
    distribution_plan: BTreeMap<XfrPublicKey, Amount>,

    // mint limit of CoinBase for rewards
    balance: Amount,

    // this will be updated dynamiclly along with txs
    principal_balance: Amount,
}

impl Default for CoinBase {
    fn default() -> Self {
        Self::gen()
    }
}

impl CoinBase {
    fn gen() -> Self {
        CoinBase {
            distribution_hist: BTreeSet::new(),
            distribution_plan: BTreeMap::new(),

            balance: ops::mint_fra::MINT_AMOUNT_LIMIT,
            principal_balance: 0,
        }
    }
}

/// sha256(pubkey)[:20]
#[inline(always)]
pub fn td_pubkey_to_td_addr(pubkey: &[u8]) -> String {
    hex::encode_upper(&sha2::Sha256::digest(pubkey)[..20])
}

#[inline(always)]
#[allow(missing_docs)]
pub fn td_pubkey_to_td_addr_bytes(pubkey: &[u8]) -> Vec<u8> {
    sha2::Sha256::digest(pubkey)[..20].to_vec()
}

#[inline(always)]
#[allow(missing_docs)]
pub fn td_pubkey_to_string(td_pubkey: &[u8]) -> TendermintPubKey {
    base64::encode(td_pubkey)
}

#[inline(always)]
#[allow(missing_docs)]
pub fn td_key_to_bytes(td_key: TendermintPubKeyRef) -> Result<Vec<u8>> {
    base64::decode(td_key).c(d!())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn td_addr_to_string(td_addr: &[u8]) -> TendermintAddr {
    hex::encode_upper(td_addr)
}

#[inline(always)]
#[allow(missing_docs)]
pub fn td_addr_to_bytes(td_addr: TendermintAddrRef) -> Result<Vec<u8>> {
    hex::decode(td_addr).c(d!())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn check_delegation_amount(am: Amount, is_append: bool) -> Result<()> {
    let lowb = alt!(
        is_append,
        MIN_DELEGATION_AMOUNT,
        STAKING_VALIDATOR_MIN_POWER
    );
    if (lowb..=MAX_DELEGATION_AMOUNT).contains(&am) {
        Ok(())
    } else {
        let msg = format!(
            "Invalid delegation amount: {} (min: {}, max: {})",
            am, lowb, MAX_DELEGATION_AMOUNT
        );
        Err(eg!(msg))
    }
}

#[inline(always)]
#[allow(missing_docs)]
pub fn is_valid_tendermint_addr(addr: TendermintAddrRef) -> bool {
    // hex::encode_upper(sha256(pubkey[:20]))
    const TENDERMINT_HEX_ADDR_LEN: usize = 40;

    TENDERMINT_HEX_ADDR_LEN == addr.len()
        && addr.chars().all(|i| i.is_numeric() || i.is_uppercase())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn has_relative_inputs(x: &TransferAsset) -> bool {
    x.body
        .inputs
        .iter()
        .any(|i| matches!(i, TxoRef::Relative(_)))
}

#[inline(always)]
#[allow(missing_docs)]
pub fn deny_relative_inputs(x: &TransferAsset) -> Result<()> {
    if has_relative_inputs(x) {
        Err(eg!("Relative inputs are not allowed"))
    } else {
        Ok(())
    }
}

#[inline(always)]
#[allow(missing_docs)]
pub fn is_coinbase_tx(tx: &Transaction) -> bool {
    tx.body
        .operations
        .iter()
        .any(|o| matches!(o, Operation::MintFra(_)))
}

#[inline(always)]
#[allow(missing_docs)]
pub fn gen_random_keypair() -> XfrKeyPair {
    XfrKeyPair::generate(&mut ChaChaRng::from_entropy())
}

fn get_nonconfidential_balance(la: &impl LedgerAccess, addr: &XfrPublicKey) -> u64 {
    la.get_owned_utxos(addr)
        .values()
        .map(|(utxo, _)| {
            if let XfrAmount::NonConfidential(am) = utxo.0.record.amount {
                am
            } else {
                0
            }
        })
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;
    use rand::random;

    // **NOTE**
    //
    // `block rewards rate` will be tested in `abci`
    #[test]
    fn staking_return_rate() {
        check_proposer_rewards_rate();
    }

    fn check_proposer_rewards_rate() {
        (0..100).for_each(|_| {
            pnk!(Staking::get_proposer_rewards_rate([
                3990000000000000,
                4208000000000000
            ]));

            PROPOSER_REWARDS_RATE_RULE.iter().for_each(
                |([lower_bound, upper_bound], rate)| {
                    assert_eq!(
                        pnk!(Staking::get_proposer_rewards_rate(
                            gen_round_vote_percent(
                                *lower_bound as u64,
                                *upper_bound as u64
                            )
                        )),
                        [*rate, 100]
                    );
                },
            );
        });
    }

    fn gen_round_vote_percent(lower_bound: u64, upper_bound: u64) -> [u64; 2] {
        let itv = upper_bound - lower_bound;
        let lb = if 0 == itv {
            lower_bound
        } else {
            lower_bound + random::<u64>() % itv
        };

        [lb, 100_0000]
    }
}
