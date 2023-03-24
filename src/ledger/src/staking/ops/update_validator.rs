//!
//! # Update Validator Infomation
//!
//! update the verifier information at a certain block height
//! by using a multi-signature transaction.
//!

use {
    crate::{
        data_model::NoReplayToken,
        staking::{
            cosig::CoSigOp, BlockHeight, Staking, Validator, ValidatorData,
            COSIG_THRESHOLD_DEFAULT, VALIDATORS_MIN,
        },
    },
    ruc::*,
    zei::{XfrKeyPair, XfrPublicKey},
};

/// Used as the inner object of a `UpdateValidator Operation`.
pub type UpdateValidatorOps = CoSigOp<Data>;

impl UpdateValidatorOps {
    /// Check the validity of an operation by running it in a staking simulator.
    #[inline(always)]
    pub fn check_run(&self, staking_simulator: &mut Staking) -> Result<()> {
        self.apply(staking_simulator).c(d!())
    }

    /// Apply new settings to the target `Staking` instance,
    /// will fail if existing info is found at the same height.
    pub fn apply(&self, staking: &mut Staking) -> Result<()> {
        self.verify(staking)
            .c(d!())
            .and_then(|_| self.check_context().c(d!()))
            .and_then(|_| {
                staking
                    .validator_set_at_height(self.data.height, self.data.clone())
                    .c(d!())
            })
    }

    /// Apply new settings to the target `Staking` instance,
    /// ignore existing settings at the same height.
    #[inline(always)]
    pub fn apply_force(self, staking: &mut Staking) -> Result<()> {
        self.verify(staking)
            .c(d!())
            .and_then(|_| self.check_context().c(d!()))
            .map(|_| staking.validator_set_at_height_force(self.data.height, self.data))
    }

    #[inline(always)]
    fn check_context(&self) -> Result<()> {
        if VALIDATORS_MIN > self.data.body.len() {
            return Err(eg!("too few validators"));
        }

        let t1 = self.data.cosig_rule.threshold;
        let t2 = COSIG_THRESHOLD_DEFAULT;

        // threshold must be bigger than COSIG_THRESHOLD_DEFAULT
        if t1[0] * t2[1] < t1[1] * t2[0] {
            return Err(eg!("invalid cosig threshold"));
        }

        Ok(())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_related_pubkeys(&self) -> Vec<XfrPublicKey> {
        self.cosigs
            .keys()
            .chain(self.data.body.keys())
            .copied()
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        kps: &[&XfrKeyPair],
        h: BlockHeight,
        v_set: Vec<Validator>,
        nonce: NoReplayToken,
    ) -> Result<Self> {
        Data::new(h, v_set)
            .c(d!())
            .map(|d| CoSigOp::create(d, nonce))
            .and_then(|mut op| op.batch_sign(kps).c(d!()).map(|_| op))
    }
}

/// The body of a `UpdateValidator Operation`.
type Data = ValidatorData;
