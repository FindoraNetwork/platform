//!
//! # On-Chain Governance
//!
//! propose a governance operation against some byzantine nodes
//! by using a multi-signature transaction.
//!
//! **NOTE**: always use the same multi-signature rules as `UpdateValidator`.
//!

use {
    crate::{
        data_model::NoReplayToken,
        staking::{cosig::CoSigOp, Staking, TendermintAddrRef, BLOCK_HEIGHT_MAX},
    },
    lazy_static::lazy_static,
    ruc::*,
    serde::{Deserialize, Serialize},
    std::collections::BTreeMap,
    zei::{XfrKeyPair, XfrPublicKey},
};

lazy_static! {
    // The current MVP version is a fixed rule,
    // and it will be upgraded to a mechanism
    // that can update rules by sending a specific transaction.
    static ref RULES: BTreeMap<ByzantineKind, Rule> = {
        map! { B
            ByzantineKind::DuplicateVote => Rule::new([5, 100]),
            ByzantineKind::LightClientAttack => Rule::new([1, 100]),
            ByzantineKind::Unknown => Rule::new([30, 100]),
            // we should set this percent to a very small value
            ByzantineKind::OffLine => Rule::new([1, 1000_0000]),
        }
    };
}

/// Used as the inner object of a `Governance Operation`.
pub type GovernanceOps = CoSigOp<Data>;

impl GovernanceOps {
    /// Check the validity of an operation by running it in a staking simulator.
    #[inline(always)]
    pub fn check_run(&self, staking_simulator: &mut Staking) -> Result<()> {
        self.apply(staking_simulator).c(d!())
    }

    /// Apply new governance to the target `Staking` instance.
    pub fn apply(&self, staking: &mut Staking) -> Result<()> {
        self.verify(staking)
            .c(d!())
            .and_then(|_| RULES.get(&self.data.kind).ok_or(eg!()))
            .and_then(|rule| {
                staking
                    .governance_penalty_by_pubkey(
                        &self.data.byzantine_id,
                        self.data
                            .custom_percent
                            .unwrap_or_else(|| rule.gen_penalty_percent()),
                    )
                    .c(d!())
            })
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_related_pubkeys(&self) -> Vec<XfrPublicKey> {
        self.cosigs
            .keys()
            .chain([self.data.byzantine_id].iter())
            .copied()
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        kps: &[&XfrKeyPair],
        byzantine_id: XfrPublicKey,
        kind: ByzantineKind,
        custom_percent: Option<[u64; 2]>,
        nonce: NoReplayToken,
    ) -> Result<Self> {
        if let Some(p) = custom_percent {
            if 0 == p[1] || p[1] > i64::MAX as u64 || p[0] > p[1] {
                return Err(eg!());
            }
        }

        let mut op =
            CoSigOp::create(Data::new(kind, byzantine_id, custom_percent), nonce);
        op.batch_sign(kps).c(d!()).map(|_| op)
    }
}

/// Informances about a `Governance Operation`.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Data {
    kind: ByzantineKind,
    byzantine_id: XfrPublicKey,
    custom_percent: Option<[u64; 2]>,
}

impl Data {
    #[inline(always)]
    fn new(
        kind: ByzantineKind,
        byzantine_id: XfrPublicKey,
        custom_percent: Option<[u64; 2]>,
    ) -> Self {
        Data {
            kind,
            byzantine_id,
            custom_percent,
        }
    }
}

/// Kinds of byzantine behavior and corresponding punishment mechanism.
pub type RuleSet = BTreeMap<ByzantineKind, Rule>;

/// Kinds of byzantine behaviors:
/// - `DuplicateVote` and `LightClientAttack` can be auto-detected by tendermint
/// - other attack kinds need to be defined and applied on the application side
#[non_exhaustive]
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum ByzantineKind {
    DuplicateVote,
    LightClientAttack,
    OffLine,
    Unknown,
}

/// Punishment mechanism for each kind of byzantine behavior.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Rule {
    penalty_percent: [u64; 2],
}

impl Rule {
    fn new(penalty_percent: [u64; 2]) -> Self {
        Rule { penalty_percent }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn gen_penalty_percent(&self) -> [u64; 2] {
        self.penalty_percent
    }
}

/// Penalize the FRAs by a specified address.
///
/// Any validator who has unstaked itself should not be punished,
/// its delegators should not be punished also.
#[inline(always)]
pub fn governance_penalty_tendermint_auto(
    staking: &mut Staking,
    addr: TendermintAddrRef,
    bz_kind: &ByzantineKind,
) -> Result<()> {
    let rule = RULES.get(bz_kind).ok_or(eg!())?;
    staking
        .validator_td_addr_to_app_pk(addr)
        .c(d!())
        .and_then(|pk| {
            staking
                .delegation_get(&pk)
                .map(|d| d.end_height)
                .c(d!())
                .and_then(|h| {
                    if BLOCK_HEIGHT_MAX != h {
                        return Ok(());
                    }
                    staking
                        .governance_penalty_by_pubkey(&pk, rule.gen_penalty_percent())
                        .c(d!())
                })
        })
}
