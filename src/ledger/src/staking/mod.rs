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
#![allow(clippy::upper_case_acronyms)]

#[cfg(not(target_arch = "wasm32"))]
use {num_bigint::BigUint, std::convert::TryFrom};

pub mod cosig;
pub mod init;
pub mod ops;

use {
    crate::{
        data_model::{
            ConsensusRng, Operation, Transaction, TransferAsset, TxoRef, FRA_DECIMALS,
        },
        SNAPSHOT_ENTRIES_DIR,
    },
    config::abci::global_cfg::CFG,
    cosig::CoSigRule,
    cryptohash::sha256::{self, Digest},
    fbnc::{new_mapx, Mapx},
    globutils::wallet,
    indexmap::IndexMap,
    lazy_static::lazy_static,
    ops::{
        fra_distribution::FraDistributionOps,
        mint_fra::{MintKind, MINT_AMOUNT_LIMIT},
    },
    parking_lot::Mutex,
    rand::random,
    ruc::*,
    serde::{Deserialize, Serialize},
    sha2::Digest as _,
    std::{
        collections::{BTreeMap, BTreeSet},
        env, mem,
        sync::{
            mpsc::{channel, Receiver, Sender},
            Arc,
        },
    },
    zei::xfr::sig::{XfrKeyPair, XfrPublicKey},
};

// height, reward rate
type GRH = (BlockHeight, [u128; 2]);
type GRHCP = (Arc<Mutex<Sender<GRH>>>, Arc<Mutex<Receiver<GRH>>>);
// pk, height, delegation_amount
type SDH = (XfrPublicKey, BlockHeight, Amount);
type SDHCP = (Arc<Mutex<Sender<SDH>>>, Arc<Mutex<Receiver<SDH>>>);
// pk, height, delegation_amount
type DAH = (XfrPublicKey, BlockHeight, Amount);
type DAHCP = (Arc<Mutex<Sender<DAH>>>, Arc<Mutex<Receiver<DAH>>>);
// pk, height, <struct DelegationRwdDetail>
type DRH = (XfrPublicKey, BlockHeight, DelegationRwdDetail);
type DRHCP = (Arc<Mutex<Sender<DRH>>>, Arc<Mutex<Receiver<DRH>>>);

macro_rules! chan {
    () => {{
        let (s, r) = channel();
        (Arc::new(Mutex::new(s)), Arc::new(Mutex::new(r)))
    }};
}

lazy_static! {
    /// will be set in `findorad` together with '--enable-query-server' option,
    /// full-nodes may need this feature, meaningless in other kinds of node.
    pub static ref KEEP_HIST: bool = env::var("FINDORAD_KEEP_HIST").is_ok();

    /// Reserved accounts of EcoSystem.
    pub static ref FF_PK_LIST: Vec<XfrPublicKey> = FF_ADDR_LIST
        .iter()
        .map(|addr| pnk!(wallet::public_key_from_bech32(addr)))
        .collect();

    /// Reserved accounts of Findora Foundation.
    pub static ref FF_PK_EXTRA_120_0000: XfrPublicKey =
        pnk!(wallet::public_key_from_bech32(FF_ADDR_EXTRA_120_0000));

    #[allow(missing_docs)]
    pub static ref CHAN_GLOB_RATE_HIST: GRHCP = chan!();
    #[allow(missing_docs)]
    pub static ref CHAN_V_SELF_D_HIST: SDHCP = chan!();
    #[allow(missing_docs)]
    pub static ref CHAN_D_AMOUNT_HIST: DAHCP = chan!();
    #[allow(missing_docs)]
    pub static ref CHAN_D_RWD_HIST: DRHCP = chan!();
}

// Reserved accounts of Findora Foundation.
const FF_ADDR_LIST: [&str; 8] = [
    "fra1s9c6p0656as48w8su2gxntc3zfuud7m66847j6yh7n8wezazws3s68p0m9",
    "fra1zjfttcnvyv9ypy2d4rcg7t4tw8n88fsdzpggr0y2h827kx5qxmjshwrlx7",
    "fra18rfyc9vfyacssmr5x7ku7udyd5j5vmfkfejkycr06e4as8x7n3dqwlrjrc",
    "fra1kvf8z5f5m8wmp2wfkscds45xv3yp384eszu2mpre836x09mq5cqsknltvj",
    "fra1w8s3e7v5a78623t8cq43uejtw90yzd0xctpwv63um5amtv72detq95v0dy",
    "fra1ukju0dhmx0sjwzcgjzgg3e7n6f755jkkfl9akq4hleulds9a0hgq4uzcp5",
    "fra1mjdr0mgn2e0670hxptpzu9tmf0ary8yj8nv90znjspwdupv9aacqwrg3dx",
    "fra1whn756rtqt3gpsmdlw6pvns75xdh3ttqslvxaf7eefwa83pcnlhsree9gv",
];

const FF_ADDR_EXTRA_120_0000: &str =
    "fra1dkn9w5c674grdl6gmvj0s8zs0z2nf39zrmp3dpq5rqnnf9axwjrqexqnd6";

/// SEE: <https://www.notion.so/findora/PoS-Stage-1-Consensus-Rewards-Penalties-72f5c9a697ff461c89c3728e34348834#3d2f1b8ff8244632b715abdd42b6a67b>
pub const PROPOSER_REWARDS_RATE_RULE: [([u128; 2], u128); 6] = [
    ([0, 66_6667], 0),
    ([66_6667, 75_0000], 1),
    ([75_0000, 83_3333], 2),
    ([83_3333, 91_6667], 3),
    ([91_6667, 100_0000], 4),
    ([100_0000, 100_0001], 5),
];

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
pub const FRA_PRE_ISSUE_AMOUNT: Amount = 210_0000_0000 * FRA;

/// <Total amount of FRA-units issuance> + <token pool of CoinBase>.
pub const FRA_TOTAL_AMOUNT: Amount = FRA_PRE_ISSUE_AMOUNT + MINT_AMOUNT_LIMIT;

/// Minimum allowable delegation amount.
pub const MIN_DELEGATION_AMOUNT: Amount = 1;
/// Maximum allowable delegation amount.
pub const MAX_DELEGATION_AMOUNT: Amount = FRA_TOTAL_AMOUNT;

/// The minimum investment to become a validator through staking.
pub const STAKING_VALIDATOR_MIN_POWER: Power = 1_0000 * FRA;

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

lazy_static! {
    /// Block time interval, in seconds.
    pub static ref BLOCK_INTERVAL: u64 = {
        1 + env::var("FINDORA_BLOCK_ITV")
            .ok()
            .as_deref()
            .unwrap_or("15")
            .parse::<u64>().unwrap()
    };
}

/// The lock time after the delegation expires, about 21 days.
//pub const UNBOND_BLOCK_CNT: u64 = 3600 * 24 * 21 / BLOCK_INTERVAL;

// minimal number of validators
pub const VALIDATORS_MIN: usize = 5;

/// The minimum weight threshold required
/// when updating validator information, 9/10.
pub const COSIG_THRESHOLD_DEFAULT: [u64; 2] = [9, 10];

/// block height of tendermint
pub type BlockHeight = u64;

/// Amount of token units
pub type Amount = u64;
pub(crate) type Power = u64;

/// Node PubKey in base64 format
pub type TendermintPubKey = String;
type TendermintPubKeyRef<'a> = &'a str;

/// `sha256(pubkey)[..20]` in hex format
pub type TendermintAddr = String;
/// ref `TendermintAddr`
pub type TendermintAddrRef<'a> = &'a str;

/// `sha256(pubkey)[..20]`
pub type TendermintAddrBytes = Vec<u8>;
// type TendermintAddrBytesRef<'a> = &'a [u8];

type ValidatorInfo = BTreeMap<BlockHeight, ValidatorData>;

/// Staking entry
///
/// Init:
/// 1. set_custom_block_height
/// 2. validator_set_at_height
///
/// Usage:
/// - validator_change_power ...
/// - validator_apply_at_height
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Staking {
    // the main logic when updating:
    // - the new validator inherits the original vote power, if any
    #[serde(rename = "vi")]
    validator_info: ValidatorInfo,
    // when the end-time of delegations arrived,
    // we will try to paid the rewards until all is successful.
    #[serde(rename = "di")]
    pub(crate) delegation_info: DelegationInfo,
    // current block height in the context of tendermint.
    pub(crate) cur_height: BlockHeight,
    // FRA CoinBase.
    coinbase: CoinBase,
    cr: ConsensusRng,
}

impl Default for Staking {
    fn default() -> Self {
        Self::new()
    }
}

impl Staking {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn has_been_inited(&self) -> bool {
        !self.validator_info.is_empty()
            && 0 != self.validator_info.keys().next().copied().unwrap()
    }

    #[inline(always)]
    fn gen_consensus_tmp_pubkey(cr: &mut ConsensusRng) -> XfrPublicKey {
        XfrKeyPair::generate(cr).get_pk()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new() -> Self {
        Staking {
            // use '0' instead of '1' to
            // avoid conflicts with initial operations
            validator_info: map! {B 0 => ValidatorData::default()},
            delegation_info: DelegationInfo::new(),
            cur_height: 0,
            coinbase: CoinBase::gen(),
            cr: ConsensusRng::default(),
        }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn cur_height(&self) -> BlockHeight {
        self.cur_height
    }

    /// record block reward rate aka return_rate of current block height
    #[inline(always)]
    pub fn record_block_rewards_rate(&mut self, rate: [u128; 2]) {
        if *KEEP_HIST {
            CHAN_GLOB_RATE_HIST
                .0
                .lock()
                .send((self.cur_height, rate))
                .unwrap();
        }
    }

    ///get the delegationInfo
    pub fn delegation_info_global_amount(&self) -> Amount {
        self.delegation_info.global_amount
    }

    /// Get the validators that exactly be setted at a specified height.
    #[inline(always)]
    pub fn validator_get_at_height(&self, h: BlockHeight) -> Option<Vec<&Validator>> {
        self.validator_info
            .get(&h)
            .map(|v| v.body.values().collect())
    }

    // Check if there is some settings on a specified height.
    #[inline(always)]
    fn validator_has_settings_at_height(&self, h: BlockHeight) -> bool {
        self.validator_info.contains_key(&h)
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
        self.validator_info.insert(h, v);
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn validator_get_current(&self) -> Option<&ValidatorData> {
        self.validator_get_effective_at_height(self.cur_height)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn validator_get_current_one_by_id(
        &self,
        id: &XfrPublicKey,
    ) -> Option<&Validator> {
        self.validator_get_current().and_then(|vd| vd.body.get(id))
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn validator_get_current_mut(&mut self) -> Option<&mut ValidatorData> {
        self.validator_get_effective_at_height_mut(self.cur_height)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn validator_get_current_mut_one_by_id(
        &mut self,
        id: &XfrPublicKey,
    ) -> Option<&mut Validator> {
        self.validator_get_current_mut()
            .and_then(|vd| vd.body.get_mut(id))
    }

    /// Get the validators that will be used for the specified height.
    #[inline(always)]
    pub fn validator_get_effective_at_height(
        &self,
        h: BlockHeight,
    ) -> Option<&ValidatorData> {
        self.validator_info
            .range(0..=h)
            .rev()
            .next()
            .map(|(_, v)| v)
    }

    /// Remove the validators that will be used for the specified height.
    #[inline(always)]
    pub fn validator_remove_at_height(
        &mut self,
        h: BlockHeight,
    ) -> Result<Vec<Validator>> {
        self.validator_info
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
        self.validator_info
            .range_mut(0..=h)
            .rev()
            .next()
            .map(|(_, v)| v)
    }

    /// Get the validators exactly on a specified height.
    #[inline(always)]
    pub fn validator_get_at_height_mut(
        &mut self,
        h: BlockHeight,
    ) -> Option<&mut ValidatorData> {
        self.validator_info.get_mut(&h)
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
                    .collect::<BTreeMap<_, _>>();
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
            self.validator_clean_before_height(h.saturating_sub(1));
        }
    }

    // Clean validator-info older than the specified height.
    #[inline(always)]
    fn validator_clean_before_height(&mut self, h: BlockHeight) {
        self.validator_info = self.validator_info.split_off(&h);
    }

    // Clean validators with zero power
    // after they have been removed from tendermint core.
    fn validator_clean_invalid_items(&mut self) {
        let h = self.cur_height;

        if CFG.checkpoint.unbond_block_cnt > h {
            return;
        }

        if let Some(old) = self
            .validator_get_effective_at_height(h - CFG.checkpoint.unbond_block_cnt)
            .map(|ovd| {
                ovd.body
                    .iter()
                    .filter(|(_, v)| 0 == v.td_power)
                    .map(|(k, _)| *k)
                    .collect::<BTreeSet<_>>()
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
            .c(d!("failed to get effective validators at current height"))
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
            .map(|_| self.validator_align_power(validator))
    }

    // functions:
    // - align its power to the total amount of delegations, include self-delegation.
    // - change its kind to `ValidatorKind::Staker` if it is a `ValidatorKind::Initor`
    //
    // trigger conditions:
    //   - the target validator have done its self-delegation
    //   - its current power is bigger than zero
    fn validator_align_power(&mut self, vid: &XfrPublicKey) {
        if let Some(self_delegation_am) = self
            .delegation_get(vid)
            .and_then(|d| d.validator_entry(vid))
        {
            if let Some(v) = self.validator_get_current_mut_one_by_id(vid) {
                if 0 < v.td_power {
                    v.td_power = self_delegation_am + v.delegators.values().sum::<u64>();
                    v.kind = ValidatorKind::Staker;
                }
            }
        }
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
        self.validator_get_current_one_by_id(vldtor)
            .c(d!("validator not found"))
            .and_then(|v| {
                if ValidatorKind::Staker == v.kind {
                    self.validator_check_power_x(new_power, v.td_power)
                        .c(d!("validator power check failed"))
                } else {
                    Ok(())
                }
            })
    }

    #[inline(always)]
    /// update staker
    pub fn update_staker(&mut self, new: &Validator) -> Result<()> {
        let vd = self.validator_get_current_mut().c(d!())?;
        let res = vd.body.values_mut().any(|v| {
            if v.id == new.id {
                v.memo = new.memo.clone();
                v.commission_rate = new.commission_rate;
                return true;
            }
            false
        });

        if res {
            Ok(())
        } else {
            Err(eg!("Cannot update staker"))
        }
    }

    ///replace_staker
    pub fn check_and_replace_staker(
        &mut self,
        original_pk: &XfrPublicKey,
        new_public_key: XfrPublicKey,
        new_tendermint_params: Option<(Vec<u8>, Vec<u8>)>,
    ) -> Result<()> {
        for (h, entry) in self.delegation_info.end_height_map.iter() {
            if (entry.contains(original_pk) || entry.contains(&new_public_key))
                && *h != BLOCK_HEIGHT_MAX
            {
                return Err(eg!("Can't replace staker during unstaking."));
            }
        }

        //check if it exists a validator here.
        let validators_data = self
            .validator_get_current_mut()
            .ok_or_else(|| eg!("No validator at all."))?;

        //can't override existing validator.
        if validators_data.body.contains_key(&new_public_key)
            && (*original_pk != new_public_key)
        {
            return Err(eg!("Staker already exists."));
        }

        let mut validator;

        if let Some((new_td_addr, new_td_pubkey)) = new_tendermint_params {
            validator = validators_data
                .body
                .get(original_pk)
                .ok_or_else(|| eg!("Validator not found."))?
                .clone();

            //remove old td address.
            validators_data
                .addr_td_to_app
                .remove(&td_addr_to_string(&validator.td_addr));
            //insert new one.
            validators_data
                .addr_td_to_app
                .insert(td_addr_to_string(&new_td_addr), new_public_key);

            debug_assert!(&validator.id == original_pk);

            let v_old = validators_data.body.get_mut(original_pk).unwrap();
            v_old.td_power = 0;
            v_old.delegators.clear();

            //change the td addr
            validator.td_addr = new_td_addr;
            validator.td_pubkey = new_td_pubkey;
        } else {
            validator = validators_data
                .body
                .remove(original_pk)
                .ok_or_else(|| eg!("Validator not found."))?;

            *validators_data
                .addr_td_to_app
                .get_mut(&td_addr_to_string(&validator.td_addr))
                .c(d!())? = new_public_key;
        }

        //replace staker
        validator.id = new_public_key;

        //replace delegator
        if let Some(am) = validator.delegators.remove(original_pk) {
            validator.delegators.insert(new_public_key, am);
        }

        validators_data.body.insert(new_public_key, validator);

        let delegation_info = &mut self.delegation_info;

        //deal with delegation
        let mut d = delegation_info
            .global_delegation_records_map
            .remove(original_pk)
            .c(d!(eg!("impossible")))?;

        //change id
        d.id = new_public_key;
        delegation_info
            .global_delegation_records_map
            .insert(new_public_key, d);

        for (_pk, d) in delegation_info.global_delegation_records_map.iter_mut() {
            if let Some(am) = d.delegations.remove(original_pk) {
                d.delegations.insert(new_public_key, am);
            }
        }
        Ok(())
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

        // check everything in advance before changing the data
        {
            if self.delegation_has_addr(&validator) || owner == validator {
                // `normal scene` or `do self-delegation`
            } else {
                return Err(eg!("self-delegation has not been finished"));
            }
            check_delegation_amount(am, true).c(d!())?;
            self.validator_check_power(am, &validator).c(d!())?;
        }

        let h = self.cur_height;
        let new = || Delegation {
            delegations: map! {B validator => 0},
            id: owner,
            receiver_pk: None,
            tmp_delegators: map! {B},
            start_height: h,
            end_height,
            state: DelegationState::Bond,
            rwd_amount: 0,
            delegation_rwd_cnt: 0,
            proposer_rwd_cnt: 0,
        };

        let d = self
            .delegation_info
            .global_delegation_records_map
            .entry(owner)
            .or_insert_with(new);

        if DelegationState::Paid == d.state {
            *d = new();
        }

        if let Some(set) = self.delegation_info.end_height_map.get_mut(&d.end_height) {
            set.remove(&owner);
        }

        d.end_height = end_height;
        d.state = DelegationState::Bond;

        *d.delegations.entry(validator).or_insert(0) += am;

        // record self-delegation amount for a validator
        if owner == validator && *KEEP_HIST {
            CHAN_V_SELF_D_HIST
                .0
                .lock()
                .send((d.id, self.cur_height, d.delegations.values().sum()))
                .unwrap();
        }

        // update delegator entries for this validator
        if let Some(v) = self.validator_get_current_mut_one_by_id(&validator) {
            if owner != validator {
                *v.delegators.entry(owner).or_insert(0) += am;
                v.delegators.sort_by(|_, v1, _, v2| v2.cmp(&v1));
                if *KEEP_HIST {
                    CHAN_D_AMOUNT_HIST
                        .0
                        .lock()
                        .send((v.id, h, v.delegators.values().sum()))
                        .unwrap();
                }
            }
        }

        self.delegation_info
            .end_height_map
            .entry(end_height)
            .or_insert_with(BTreeSet::new)
            .insert(owner);

        // There should be no failure here !!
        pnk!(self.validator_change_power(&validator, am, false));

        // global amount of all delegations
        self.delegation_info.global_amount += am;

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

        if let Some(d) = self
            .delegation_info
            .global_delegation_records_map
            .get_mut(addr)
        {
            if BLOCK_HEIGHT_MAX == d.end_height {
                if d.end_height != h {
                    orig_h = Some(d.end_height);
                    d.end_height = h + CFG.checkpoint.unbond_block_cnt;
                }
            } else {
                return Err(eg!("delegator is not bonded"));
            }
            if self.addr_is_validator(addr) {
                // clear its power when a validator propose a complete undelegation
                //   - `panic` should not happen without bug[s]
                //
                // is this logic reasonable ?
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
            let v = self.validator_get_current_one_by_id(addr).unwrap();

            // unwrap is safe here
            let mut cr = self.cr;
            self.validator_get_current_one_by_id(addr)
                .unwrap()
                .delegators
                .iter()
                .for_each(|(pk, am)| {
                    auto_ud_list.push((
                        *pk,
                        PartialUnDelegation::new(
                            *am,
                            Self::gen_consensus_tmp_pubkey(&mut cr),
                            v.td_addr.clone(),
                        ),
                    ));
                });
            self.cr = cr;

            auto_ud_list.iter().for_each(|(addr, pu)| {
                ruc::info_omit!(self.undelegate_partially(addr, pu));
            });
        }

        if let Some(orig_h) = orig_h {
            self.delegation_info
                .end_height_map
                .get_mut(&orig_h)
                .map(|set| set.remove(addr));
            self.delegation_info
                .end_height_map
                .entry(h + CFG.checkpoint.unbond_block_cnt)
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

        let actual_am;
        if let Some(d) = self
            .delegation_info
            .global_delegation_records_map
            .get_mut(addr)
        {
            if is_validator
                && STAKING_VALIDATOR_MIN_POWER > d.amount().saturating_sub(pu.am)
            {
                return Err(eg!("Requested amount exceeds limits"));
            }

            let am = d
                .delegations
                .get_mut(&target_validator)
                .c(d!("Target validator does not exist"))?;

            actual_am = if pu.am > *am {
                ruc::pd!(format!(
                    "Amount exceeds limits, requested: {}, total: {}, use the value of `total`",
                    pu.am, *am
                ));
                *am
            } else {
                pu.am
            };

            if BLOCK_HEIGHT_MAX == d.end_height {
                *am = am.saturating_sub(pu.am);
                new_tmp_delegator = Delegation {
                    delegations: map! {B target_validator => actual_am},
                    id: pu.new_delegator_id,
                    receiver_pk: Some(d.id),
                    tmp_delegators: map! {B},
                    start_height: d.start_height,
                    end_height: h + CFG.checkpoint.unbond_block_cnt,
                    state: DelegationState::Bond,
                    rwd_amount: 0,
                    delegation_rwd_cnt: 0,
                    proposer_rwd_cnt: 0,
                };
                // record per-block-height self-delegation amount for a validator
                if target_validator == *addr && *KEEP_HIST {
                    CHAN_V_SELF_D_HIST
                        .0
                        .lock()
                        .send((d.id, self.cur_height, d.delegations.values().sum()))
                        .unwrap();
                }
            } else {
                return Err(eg!("delegator is out of bond"));
            }

            // tracking temporary partial undelegations in original delegation
            d.tmp_delegators.insert(pu.new_delegator_id, actual_am);
        } else {
            return Err(eg!("delegator not found"));
        }

        self.delegation_info
            .global_delegation_records_map
            .insert(pu.new_delegator_id, new_tmp_delegator);
        self.delegation_info
            .end_height_map
            .entry(h + CFG.checkpoint.unbond_block_cnt)
            .or_insert_with(BTreeSet::new)
            .insert(pu.new_delegator_id);

        // update delegator entries for pu target_validator
        if let Some(v) = self.validator_get_current_mut_one_by_id(&target_validator) {
            // add new_delegator_id to delegator list
            *v.delegators.entry(pu.new_delegator_id).or_insert(0) += actual_am;

            // update delegation amount of current address
            if let Some(am) = v.delegators.get_mut(addr) {
                *am -= actual_am;
            }
            v.delegators.sort_by(|_, v1, _, v2| v2.cmp(&v1));
        }

        Ok(())
    }

    #[inline(always)]
    fn delegation_clean_paid(
        &mut self,
        addr: &XfrPublicKey,
        h: &BlockHeight,
    ) -> Result<Delegation> {
        let d = self
            .delegation_info
            .global_delegation_records_map
            .remove(addr)
            .c(d!("not exists"))?;
        if d.state == DelegationState::Paid {
            self.delegation_info
                .end_height_map
                .get_mut(h)
                .map(|addrs| addrs.remove(addr));

            // If this is a temporary delegation, remove it from original one
            if let Some(receiver) = d.receiver_pk {
                if let Some(orig_d) = self
                    .delegation_info
                    .global_delegation_records_map
                    .get_mut(&receiver)
                {
                    orig_d.tmp_delegators.remove(addr);
                }
            }
            Ok(d)
        } else {
            // we assume that this probability is very low
            self.delegation_info
                .global_delegation_records_map
                .insert(addr.to_owned(), d);
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
        let d = if let Some(d) = self
            .delegation_info
            .global_delegation_records_map
            .get_mut(addr)
        {
            d
        } else {
            return Err(eg!("not exists"));
        };

        if end_height > d.end_height {
            let orig_h = d.end_height;
            d.end_height = end_height;
            self.delegation_info
                .end_height_map
                .get_mut(&orig_h)
                .c(d!())?
                .remove(addr);
            self.delegation_info
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
        self.delegation_info
            .global_delegation_records_map
            .get(&addr)
    }

    /// Get the delegation instance of `addr`.
    #[inline(always)]
    pub fn delegation_get_mut(
        &mut self,
        addr: &XfrPublicKey,
    ) -> Option<&mut Delegation> {
        self.delegation_info
            .global_delegation_records_map
            .get_mut(&addr)
    }

    /// Check if the `addr` is in a state of delegation
    #[inline(always)]
    pub fn delegation_has_addr(&self, addr: &XfrPublicKey) -> bool {
        self.delegation_info
            .global_delegation_records_map
            .contains_key(&addr)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_principal(&self) -> BTreeMap<XfrPublicKey, Amount> {
        self.delegation_get_global_principal_before_height(self.cur_height)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_principal_before_height(
        &self,
        h: BlockHeight,
    ) -> BTreeMap<XfrPublicKey, Amount> {
        self.delegation_get_freed_before_height(h)
            .into_iter()
            .map(|(k, d)| (k, d.amount()))
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_principal_with_receiver(
        &self,
    ) -> BTreeMap<XfrPublicKey, (Amount, Option<XfrPublicKey>)> {
        self.delegation_get_global_principal_before_height_with_receiver(self.cur_height)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_principal_before_height_with_receiver(
        &self,
        h: BlockHeight,
    ) -> BTreeMap<XfrPublicKey, (Amount, Option<XfrPublicKey>)> {
        self.delegation_get_freed_before_height(h)
            .into_iter()
            .map(|(k, d)| (k, (d.amount(), d.receiver_pk)))
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn delegation_get_global_rewards(&self) -> BTreeMap<XfrPublicKey, Amount> {
        self.delegation_get_global_rewards_before_height(self.cur_height)
    }

    /// Query delegation rewards before a specified height(included).
    #[inline(always)]
    pub fn delegation_get_global_rewards_before_height(
        &self,
        h: BlockHeight,
    ) -> BTreeMap<XfrPublicKey, Amount> {
        self.delegation_get_freed_before_height(h)
            .into_iter()
            .filter(|(_, d)| 0 < d.rwd_amount)
            .map(|(k, d)| (k, d.rwd_amount))
            .collect()
    }

    /// Query delegation rewards.
    #[inline(always)]
    pub fn delegation_get_rewards(&self, pk: &XfrPublicKey) -> Result<Amount> {
        self.delegation_info
            .global_delegation_records_map
            .get(pk)
            .map(|d| d.rwd_amount)
            .c(d!())
    }

    /// Query delegation principal.
    #[inline(always)]
    pub fn delegation_get_principal(&self, pk: &XfrPublicKey) -> Result<Amount> {
        self.delegation_info
            .global_delegation_records_map
            .get(pk)
            .map(|d| d.amount())
            .c(d!())
    }

    /// Query all freed delegations.
    #[inline(always)]
    pub fn delegation_get_freed(&self) -> BTreeMap<XfrPublicKey, &Delegation> {
        self.delegation_get_freed_before_height(self.cur_height)
    }

    /// Query freed delegations before a specified height(included).
    #[inline(always)]
    pub fn delegation_get_freed_before_height(
        &self,
        h: BlockHeight,
    ) -> BTreeMap<XfrPublicKey, &Delegation> {
        self.delegation_info
            .end_height_map
            .range(..=h)
            .flat_map(|(_, addrs)| {
                addrs
                    .iter()
                    .flat_map(|addr| {
                        self.delegation_info
                            .global_delegation_records_map
                            .get(addr)
                            .map(|d| (*addr, d))
                    })
                    .filter(|(_, d)| matches!(d.state, DelegationState::Free))
            })
            .collect()
    }

    /// Clean delegation states along with each new block.
    #[inline(always)]
    pub fn delegation_process(&mut self) {
        let h = self.cur_height;

        self.delegation_info
            .end_height_map
            .range(..=h)
            .flat_map(|(_, addr)| addr)
            .copied()
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|addr| {
                let entries = if let Some(d) = self
                    .delegation_info
                    .global_delegation_records_map
                    .get_mut(&addr)
                {
                    if DelegationState::Bond == d.state {
                        d.state = DelegationState::Free;
                        Some(d.delegations.clone())
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some(e) = entries {
                    e.into_iter().for_each(|(vid, am)| {
                        if let Some(v) = self.validator_get_current_mut_one_by_id(&vid) {
                            v.delegators.remove(&addr);
                            v.delegators.sort_by(|_, v1, _, v2| v2.cmp(&v1));
                            if *KEEP_HIST {
                                CHAN_D_AMOUNT_HIST
                                    .0
                                    .lock()
                                    .send((v.id, h, v.delegators.values().sum()))
                                    .unwrap();
                            }
                        }

                        // reduce global amount of global delegations
                        self.delegation_info.global_amount -= am;

                        // reduce the power of the target validator
                        // NOTE: set this operation after cleaning delegators!
                        ruc::info_omit!(self.validator_change_power(&vid, am, true));
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
        let r = if CFG.checkpoint.fix_unpaid_delegation_height > h {
            self.delegation_info.end_height_map.range(0..=h)
        } else {
            self.delegation_info.end_height_map.range(0..h)
        };

        r.map(|(k, v)| (k.to_owned(), (*v).clone()))
            .collect::<Vec<_>>()
            .iter()
            .for_each(|(h, addrs)| {
                addrs.iter().for_each(|addr| {
                    ruc::info_omit!(self.delegation_clean_paid(addr, h));
                });
                // this unwrap is safe
                if self
                    .delegation_info
                    .end_height_map
                    .get(&h)
                    .unwrap()
                    .is_empty()
                {
                    self.delegation_info.end_height_map.remove(&h);
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
            // punish related delegators
            let pl = || {
                self.validator_get_current_one_by_id(addr)
                    .unwrap()
                    .delegators
                    .iter()
                    .map(|(pk, am)| (*pk, am * percent[0] / percent[1]))
                    .collect::<Vec<_>>()
            };

            pl().into_iter().for_each(|(pk, p_am)| {
                ruc::info_omit!(self.governance_penalty_sub_amount(&pk, p_am));
            });

            // punish its vote power
            self.validator_get_power(addr).c(d!()).and_then(|power| {
                self.validator_change_power(addr, power * percent[0] / percent[1], true)
                    .c(d!())
            })?;
        }

        Ok(())
    }

    #[inline(always)]
    fn governance_penalty_sub_amount(
        &mut self,
        addr: &XfrPublicKey,
        mut am: Amount,
    ) -> Result<()> {
        let d = if let Some(d) = self
            .delegation_info
            .global_delegation_records_map
            .get_mut(addr)
        {
            d
        } else {
            return Err(eg!("not exists"));
        };

        if *KEEP_HIST {
            let r = DelegationRwdDetail {
                bond: d.amount(),
                amount: 0,
                penalty_amount: am,
                return_rate: None,
                commission_rate: None,
                global_delegation_percent: None,
                block_height: self.cur_height,
            };
            CHAN_D_RWD_HIST
                .0
                .lock()
                .send((d.id, self.cur_height, r))
                .unwrap();
        }

        if DelegationState::Paid == d.state {
            return Err(eg!("delegation has been paid"));
        } else {
            // NOTE:
            // punish principal first
            d.delegations.values_mut().for_each(|v| {
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
            .c(d!("No validators at current height"))
            .and_then(|vd| {
                vd.addr_td_to_app
                    .get(addr)
                    .copied()
                    .c(d!(format!("Failed to get pk {}", addr)))
            })
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

        if self.coinbase.distribution_hist.contains_key(&h) {
            return Err(eg!("already exists"));
        }

        // Update fra distribution history first.
        self.coinbase.distribution_hist.insert(h, false);

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
    pub fn coinbase_check_and_pay(&mut self, tx: &Transaction) {
        if !tx.is_coinbase_tx() {
            return;
        }
        self.coinbase_pay(tx);
    }

    fn coinbase_pay(&mut self, tx: &Transaction) {
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
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn addr_is_in_distribution_plan(&self, pk: &XfrPublicKey) -> bool {
        self.coinbase.distribution_plan.contains_key(pk)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn addr_is_in_freed_delegation(&self, pk: &XfrPublicKey) -> bool {
        if let Some(dlg) = self.delegation_info.global_delegation_records_map.get(pk) {
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

    #[cfg(not(target_arch = "wasm32"))]
    /// set_proposer_rewards sets the rewards for the block proposer
    /// All rewards are allocated to the proposer only
    pub(crate) fn set_proposer_rewards(
        &mut self,
        proposer: &XfrPublicKey,
        vote_percent: [u64; 2],
    ) -> Result<()> {
        // Get rate from based on LastCommitInfo and hardcoded PROPOSER_REWARDS_RATE_RULE
        // This is rewards B (bonus proposer reward )
        let p = Self::get_proposer_rewards_rate(vote_percent).c(d!())?;
        let h = self.cur_height;
        let cbl = self.coinbase_balance();
        let total_delegation_amount_of_validator = self
            .validator_get_current_one_by_id(proposer)
            .c(d!())?
            .delegators
            .values()
            .sum::<Amount>();
        let gda = self.get_global_delegation_amount();
        self.delegation_get_mut(proposer)
            .c(d!())
            .and_then(|d| {
                // Self delegation + total_delegation_amount_of_validator
                let tdaov = d.delegations.get(proposer).copied().unwrap_or(0)
                    + total_delegation_amount_of_validator;
                d.set_delegation_rewards(
                    proposer,
                    h,
                    p,
                    [0, 100],
                    [0, 0],
                    tdaov,
                    gda,
                    false,
                    cbl,
                )
                .c(d!())
            })
            .map(|_| ())
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn get_proposer_rewards_rate(vote_percent: [u64; 2]) -> Result<[u128; 2]> {
        let p = [vote_percent[0] as u128, vote_percent[1] as u128];
        // p[0] = Validator power which voted for this block
        // p[1] = Total Validator power including those that did not vote
        if p[0] > p[1] || 0 == p[1] {
            let msg = format!("Invalid power percent: {}/{}", p[0], p[1]);
            return Err(eg!(msg));
        }
        // Voted = Power voted in last block
        // Total = Power total validator in validator set
        // Condition
        // Voted * 100_0000 < Total * high
        // Voted * 100_0000 > Total * low
        // Return rate from matching the table below

        // Where [high ,low , rate ]
        // (0, 66_6667, 0),
        // (66_6667, 75_0000, 1),
        // (75_0000, 83_3333, 2),
        // (83_3333, 91_6667, 3),
        // (91_6667, 100_0000, 4),
        // (100_0000, 100_0001, 5),
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

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_global_delegation_amount(&self) -> Amount {
        self.delegation_info.global_amount
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_global_delegation_amount_mut(&mut self) -> &mut Amount {
        &mut self.delegation_info.global_amount
    }

    ///Get all the delegation records.
    pub fn get_global_delegation_records(&self) -> &BTreeMap<XfrPublicKey, Delegation> {
        &self.delegation_info.global_delegation_records_map
    }
}

/// self-description of staker
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct StakerMemo {
    /// Name of the Staker, like "FastNode"
    pub name: String,
    /// Description of the Staker
    pub desc: String,
    /// URL of the Staker
    pub website: String,
    /// Logo image url of the Staker
    pub logo: String,
}

impl Default for StakerMemo {
    fn default() -> Self {
        StakerMemo {
            name: format!("NULL_{}", random::<u16>() ^ random::<u16>()),
            desc: "NULL".to_owned(),
            website: "NULL".to_owned(),
            logo: "https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png".to_owned(),
        }
    }
}

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
pub(crate) struct DelegationInfo {
    pub(crate) global_amount: Amount,
    // validator pubkey => delegation info
    // addr_map contains an entry for every delegation on the network .
    // Self Delegations and Regular Delegation
    #[serde(rename = "addr_map")]
    pub(crate) global_delegation_records_map: BTreeMap<XfrPublicKey, Delegation>,
    pub(crate) end_height_map: BTreeMap<BlockHeight, BTreeSet<XfrPublicKey>>,
}

impl DelegationInfo {
    fn new() -> Self {
        DelegationInfo {
            global_amount: 0,
            global_delegation_records_map: BTreeMap::new(),
            end_height_map: BTreeMap::new(),
        }
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum ValidatorKind {
    Staker,
    #[serde(rename = "Initor")]
    Initiator,
}

impl std::fmt::Display for ValidatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValidatorKind::Staker => write!(f, "Staker"),
            ValidatorKind::Initiator => write!(f, "Initiator"),
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
    /// During registration the Validator,
    /// Candidate/Validator will specifiy a % commission which will be publicly recorded on the blockchain,
    /// so FRA owners can make an informed choice on which validator to use;
    /// % commision is the % of FRA incentives the validator will take out as a commission fee
    /// for helping FRA owners stake their tokens.
    pub commission_rate: [u64; 2],
    /// optional descriptive information
    pub memo: StakerMemo,
    /// Which kind of validator it is
    pub kind: ValidatorKind,
    /// use this field to mark
    /// if this validator signed last block
    pub signed_last_block: bool,
    /// how many blocks has the validator signed
    pub signed_cnt: u64,

    /// delegator pubkey => amount
    ///   - delegator entries on current block height
    pub delegators: IndexMap<XfrPublicKey, Amount>,
}

impl Validator {
    #[allow(missing_docs)]
    pub fn new(
        td_pubkey: Vec<u8>,
        td_power: Amount,
        id: XfrPublicKey,
        commission_rate: [u64; 2],
        memo: StakerMemo,
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
            delegators: IndexMap::new(),
        })
    }

    /// use this fn when propose an advanced `Delegation`, aka Staking.
    pub fn new_staker(
        td_pubkey: Vec<u8>,
        id: XfrPublicKey,
        commission_rate: [u64; 2],
        memo: StakerMemo,
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
    /// validator pubkey => amount
    ///   - `NonConfidential` FRAs amount
    ///   - valid for all delegators
    /// delegations contains an entry for each validator this `delegator` has delegated to
    /// XfrPublicKey here has to be the public key of a validator
    #[serde(rename = "entries")]
    pub delegations: BTreeMap<XfrPublicKey, Amount>,

    /// delegation rewards will be paid to this pk by default
    pub id: XfrPublicKey,
    /// optional receiver address,
    /// if this one exists, tokens will be paid to it instead of id
    pub receiver_pk: Option<XfrPublicKey>,
    /// Temporary partial undelegations of current id
    pub tmp_delegators: BTreeMap<XfrPublicKey, Amount>,
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
    /// how many times you get proposer rewards
    pub proposer_rwd_cnt: u64,
    /// how many times you get delegation rewards
    pub delegation_rwd_cnt: u64,
}

/// Detail of each reward entry.
#[derive(Clone, Default, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct DelegationRwdDetail {
    /// total bonds
    pub bond: Amount,
    /// total rewards
    pub amount: Amount,
    /// total penalties
    pub penalty_amount: Amount,
    /// global reward rate
    pub return_rate: Option<[u128; 2]>,
    /// commission rate of the target validator
    pub commission_rate: Option<[u64; 2]>,
    /// global delegations / global issuances
    pub global_delegation_percent: Option<[u64; 2]>,
    /// current block height
    pub block_height: BlockHeight,
}

impl Delegation {
    /// Total amout of a delegator.
    #[inline(always)]
    pub fn amount(&self) -> Amount {
        self.delegations.values().sum()
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
    fn validator_entry(&self, validator: &XfrPublicKey) -> Option<Amount> {
        self.delegations.get(validator).copied()
    }

    #[inline(always)]
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn validator_entry_exists(&self, validator: &XfrPublicKey) -> bool {
        self.delegations.contains_key(validator)
    }

    // #[inline(always)]
    // fn validator_entry_mut(&mut self, validator: &XfrPublicKey) -> Result<&mut Amount> {
    //     self.entries.get_mut(validator).c(d!())
    // }

    #[inline(always)]
    fn clean_amount(&mut self) {
        self.delegations.values_mut().for_each(|v| {
            *v = 0;
        });
    }

    // > **NOTE:**
    // > use 'AssignAdd' instead of 'Assign'
    // > to keep compatible with the logic of governance penalty.
    #[cfg(not(target_arch = "wasm32"))]
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn set_delegation_rewards(
        &mut self,
        validator: &XfrPublicKey,
        cur_height: BlockHeight,
        return_rate: [u128; 2],
        commission_rate: [u64; 2],
        global_delegation_percent: [u64; 2],
        total_delegation_amount_of_validator: Amount,
        global_delegation_amount: Amount,
        is_delegation_rwd: bool,
        coinbase_bl: Amount,
    ) -> Result<u64> {
        // #[cfg(feature = "debug_env")]
        // const ZERO_AMOUNT_FIX_HEIGHT: BlockHeight = 0;
        //
        // #[cfg(not(feature = "debug_env"))]
        // const ZERO_AMOUNT_FIX_HEIGHT: BlockHeight = 120_0000;

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
                if 0 == am {
                    if CFG.checkpoint.zero_amount_fix_height < cur_height {
                        return Ok(0);
                    } else {
                        return Err(eg!("set rewards on zero amount"));
                    }
                }

                // **APY**,
                // NOTE: the `div` calculation is safe here
                am += self.rwd_amount.saturating_mul(am) / self.amount();
                calculate_delegation_rewards(
                    return_rate,
                    am,
                    total_delegation_amount_of_validator,
                    global_delegation_amount,
                    is_delegation_rwd,
                    cur_height,
                )
                .c(d!())
                .map(|n| alt!(n > coinbase_bl, coinbase_bl, n))
            })
            .and_then(|mut n| {
                let commission =
                    n.saturating_mul(commission_rate[0]) / commission_rate[1];
                n = n.checked_sub(commission).c(d!())?;
                if is_delegation_rwd && *KEEP_HIST {
                    let r = DelegationRwdDetail {
                        bond: self.amount(),
                        amount: n,
                        penalty_amount: 0,
                        return_rate: Some(return_rate),
                        commission_rate: Some(commission_rate),
                        global_delegation_percent: Some(global_delegation_percent),
                        block_height: cur_height,
                    };
                    CHAN_D_RWD_HIST
                        .0
                        .lock()
                        .send((self.id, cur_height, r))
                        .unwrap();
                }
                self.rwd_amount.checked_add(n).c(d!()).map(|i| {
                    self.rwd_amount = i;
                    commission
                })
            })
    }
}

// Calculate the amount(in FRA units) that
// should be paid to the owner of this delegation.
#[cfg(not(target_arch = "wasm32"))]
fn calculate_delegation_rewards(
    return_rate: [u128; 2],
    amount: Amount,
    total_amount: Amount,
    global_amount: Amount,
    is_delegation_rwd: bool,
    cur_height: BlockHeight,
) -> Result<Amount> {
    // #[cfg(feature = "debug_env")]
    // const APY_FIX_HEIGHT: BlockHeight = 0;

    // #[cfg(feature = "debug_env")]
    // const OVERFLOW_FIX_HEIGHT: BlockHeight = 0;

    // #[cfg(feature = "debug_env")]
    // const SECOND_FIX_HEIGHT: BlockHeight = 0;

    // #[cfg(not(feature = "debug_env"))]
    // const APY_FIX_HEIGHT: BlockHeight = 117_7000;

    // logic apply at about 2021-11-11 14:30
    // #[cfg(not(feature = "debug_env"))]
    // const OVERFLOW_FIX_HEIGHT: BlockHeight = 124_7000;

    // #[cfg(not(feature = "debug_env"))]
    // const SECOND_FIX_HEIGHT: BlockHeight = 142_9000;

    if CFG.checkpoint.overflow_fix_height < cur_height {
        let am = BigUint::from(amount);
        let total_am = BigUint::from(total_amount);
        let global_am = BigUint::from(global_amount);
        let block_itv = *BLOCK_INTERVAL as u128;

        let second_per_year: u128 = if CFG.checkpoint.second_fix_height < cur_height {
            365 * 24 * 3600
        } else {
            356 * 24 * 3600
        };

        let calculate_self_only = || {
            let a1 = am.clone() * return_rate[0] * block_itv;
            let a2 = return_rate[1] * second_per_year;

            a1 / a2
        };

        let n = if CFG.checkpoint.apy_fix_height < cur_height {
            if is_delegation_rwd {
                // global_amount * am * return_rate[0] * block_itv / (return_rate[1] * (365 * 24 * 3600) * total_amount)
                let a1 = global_am * am * return_rate[0] * block_itv;
                let a2 = total_am * second_per_year * return_rate[1];
                a1 / a2
            } else {
                calculate_self_only()
            }
        } else {
            // compitable with old logic, an incorrect logic
            calculate_self_only()
        };

        u64::try_from(n).c(d!())
    } else {
        // compitable with old logic, an incorrect logic
        let am = amount as u128;
        let total_am = total_amount as u128;
        let global_am = global_amount as u128;
        let block_itv = *BLOCK_INTERVAL as u128;

        let calculate_self_only = || {
            am.checked_mul(return_rate[0])
                .and_then(|i| i.checked_mul(block_itv))
                .and_then(|i| {
                    return_rate[1]
                        .checked_mul(365 * 24 * 3600)
                        .and_then(|j| i.checked_div(j))
                })
        };

        if CFG.checkpoint.apy_fix_height < cur_height {
            if is_delegation_rwd {
                // # For delegation rewards:
                //
                // <A>. (am / total_amount) * (global_amount * ((return_rate[0] / return_rate[1]) / ((365 * 24 * 3600) / block_itv)))
                // <B>. global_amount * am * return_rate[0] * block_itv / (return_rate[1] * (365 * 24 * 3600) * total_amount)
                //
                // We want A,
                // and B is equal to A,
                // but B is easier to be calculated.
                global_am
                    .checked_mul(am)
                    .and_then(|i| i.checked_mul(return_rate[0]))
                    .and_then(|i| i.checked_mul(block_itv))
                    .and_then(|i| {
                        return_rate[1]
                            .checked_mul(365 * 24 * 3600)
                            .and_then(|j| j.checked_mul(total_am))
                            .and_then(|j| i.checked_div(j))
                    })
            } else {
                // # For proposer rewards:
                //
                // <A>. am * ((return_rate[0] / return_rate[1]) / ((365 * 24 * 3600) / block_itv))
                // <B>. am * return_rate[0] * block_itv / (return_rate[1] * (365 * 24 * 3600))
                //
                // We want A,
                // and B is equal to A,
                // but B is easier to be calculated.
                calculate_self_only()
            }
        } else {
            // compitable with old logic, an incorrect logic
            calculate_self_only()
        }
        .c(d!("overflow"))
        .and_then(|n| u64::try_from(n).c(d!()))
    }
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
    distribution_hist: Mapx<Digest, bool>,
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
            distribution_hist: new_mapx!(&format!(
                "{}/staking/coinbase/distribution_hist",
                SNAPSHOT_ENTRIES_DIR.as_str()
            )),
            distribution_plan: BTreeMap::new(),
            balance: ops::mint_fra::MINT_AMOUNT_LIMIT,
            principal_balance: 0,
        }
    }
}

/// `sha256(pubkey)[..20]`
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

#[cfg(test)]
#[allow(missing_docs)]
mod test {
    use super::*;

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
