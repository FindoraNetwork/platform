//!
//! # FRA Distribution
//!
//! Used to support the distribution of the official token FRA.
//!
//! **NOTE**: always use the same multi-signature rules as `UpdateValidator`.
//!

use {
    crate::{
        data_model::{NoReplayToken, Operation, Transaction},
        staking::{cosig::CoSigOp, Staking},
    },
    ruc::*,
    serde::{Deserialize, Serialize},
    std::collections::BTreeMap,
    zei::{XfrKeyPair, XfrPublicKey},
};

/// Used as the inner object of a `FraDistribution Operation`.
pub type FraDistributionOps = CoSigOp<Data>;

impl FraDistributionOps {
    /// Check the validity of an operation by running it in a staking simulator.
    #[inline(always)]
    pub fn check_run(
        &self,
        staking_simulator: &mut Staking,
        tx: &Transaction,
    ) -> Result<()> {
        self.apply(staking_simulator, tx).c(d!())
    }

    /// Apply new settings to the target `Staking` instance.
    #[inline(always)]
    pub fn apply(&self, staking: &mut Staking, tx: &Transaction) -> Result<()> {
        self.verify(staking)
            .c(d!())
            .and_then(|_| Self::check_context(tx).c(d!()))
            .and_then(|_| {
                staking
                    .coinbase_config_fra_distribution(self.clone())
                    .c(d!())
            })
    }

    #[inline(always)]
    fn check_context(tx: &Transaction) -> Result<()> {
        check_fra_distribution_context(tx).c(d!())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_related_pubkeys(&self) -> Vec<XfrPublicKey> {
        self.cosigs
            .keys()
            .chain(self.data.alloc_table.keys())
            .copied()
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        kps: &[&XfrKeyPair],
        alloc_table: BTreeMap<XfrPublicKey, u64>,
        nonce: NoReplayToken,
    ) -> Result<Self> {
        let mut op = CoSigOp::create(Data::new(alloc_table), nonce);
        op.batch_sign(kps).c(d!()).map(|_| op)
    }
}

/// The body of a `FraDistribution Operation`.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Data {
    /// How many FRAs to pay for each address.
    pub alloc_table: BTreeMap<XfrPublicKey, u64>,
}

impl Data {
    #[inline(always)]
    fn new(alloc_table: BTreeMap<XfrPublicKey, u64>) -> Self {
        Data { alloc_table }
    }
}

#[inline(always)]
fn check_fra_distribution_context(tx: &Transaction) -> Result<()> {
    if tx
        .body
        .operations
        .iter()
        .any(|op| matches!(op, Operation::FraDistribution(_)))
    {
        Ok(())
    } else {
        Err(eg!())
    }
}
