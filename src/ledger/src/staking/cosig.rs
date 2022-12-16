//!
//! # CoSignature
//!
//! Aka Multi-Signature, it is originally used to support `Governance` and `ValidatorUpdate`.
//!

use {
    super::MAX_TOTAL_POWER,
    crate::{
        data_model::NoReplayToken,
        staking::{Staking, ValidatorData},
    },
    cryptohash::sha256::{self, Digest},
    ruc::*,
    serde::{Deserialize, Serialize},
    std::{
        collections::BTreeMap,
        fmt::{self, Debug},
    },
    zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSignature},
};

/// A common structure for data with co-signatures.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(bound = "")]
pub struct CoSigOp<T>
where
    T: Debug + Serialize + for<'a> Deserialize<'a>,
{
    pub(crate) data: T,
    pub(crate) cosigs: BTreeMap<XfrPublicKey, CoSig>,
    nonce: NoReplayToken,
}

impl<T> CoSigOp<T>
where
    T: Debug + Serialize + for<'a> Deserialize<'a>,
{
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn create(msg: T, nonce: NoReplayToken) -> Self {
        CoSigOp {
            data: msg,
            nonce,
            cosigs: BTreeMap::new(),
        }
    }

    /// Attach a new signature.
    #[inline(always)]
    pub fn sign(&mut self, kp: &XfrKeyPair) -> Result<()> {
        bincode::serialize(&(self.nonce, &self.data))
            .c(d!())
            .map(|msg| {
                let k = kp.get_pk();
                let v = CoSig::new(kp.get_pk(), kp.sign(&msg));
                self.cosigs.insert(k, v);
            })
    }

    /// Attach some new signatures in a batch mode.
    #[inline(always)]
    pub fn batch_sign(&mut self, kps: &[&XfrKeyPair]) -> Result<()> {
        let msg = bincode::serialize(&(self.nonce, &self.data)).c(d!())?;
        kps.iter().for_each(|kp| {
            let k = kp.get_pk();
            let v = CoSig::new(kp.get_pk(), kp.sign(&msg));
            self.cosigs.insert(k, v);
        });
        Ok(())
    }

    /// Check if a cosig is valid.
    pub fn check_cosigs(&self, vd: &ValidatorData) -> Result<()> {
        if vd.body.is_empty() {
            return Ok(());
        }

        self.check_existence(vd)
            .c(d!())
            .and_then(|_| self.check_weight(vd).c(d!()))
            .and_then(|_| {
                let msg = bincode::serialize(&(self.nonce, &self.data)).c(d!())?;
                if self
                    .cosigs
                    .values()
                    .any(|sig| sig.pk.verify(&msg, &sig.sig).is_err())
                {
                    Err(eg!(CoSigErr::SigInvalid))
                } else {
                    Ok(())
                }
            })
    }

    #[inline(always)]
    fn check_existence(&self, vd: &ValidatorData) -> Result<()> {
        if self.cosigs.keys().any(|k| !vd.body.contains_key(k)) {
            Err(eg!(CoSigErr::KeyUnknown))
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    fn check_weight(&self, vd: &ValidatorData) -> Result<()> {
        let rule_weights = vd.body.values().map(|v| v.td_power as u128).sum::<u128>();
        let actual_weights = self
            .cosigs
            .values()
            .flat_map(|s| vd.body.get(&s.pk).map(|v| v.td_power as u128))
            .sum::<u128>();

        let rule = [
            vd.cosig_rule.threshold[0] as u128,
            vd.cosig_rule.threshold[1] as u128,
        ];

        if actual_weights.checked_mul(rule[1]).ok_or(eg!())?
            < rule[0].checked_mul(rule_weights).ok_or(eg!())?
        {
            return Err(eg!(CoSigErr::WeightInsufficient));
        }

        Ok(())
    }

    /// Verify co-signatures based on current validators.
    pub fn verify(&self, staking: &Staking) -> Result<()> {
        staking
            .validator_get_current()
            .ok_or(eg!())
            .and_then(|vd| self.check_cosigs(vd).c(d!()))
    }

    /// Generate sha256 digest.
    #[inline(always)]
    pub fn hash(&self) -> Result<Digest> {
        bincode::serialize(self)
            .c(d!())
            .map(|bytes| sha256::hash(&bytes))
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.nonce = nonce;
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_nonce(&self) -> NoReplayToken {
        self.nonce
    }
}

/// The rule for a kind of data.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct CoSigRule {
    /// check rule:
    /// - `[actual weight].sum() / [rule weight].sum() >= threshold%`
    /// - threshold% = `numerator / denominator` = `threshold[0] / threshold[1]`
    ///
    /// which equal to:
    /// - `[actual weight].sum() * threshold[1] >= threshold[0] * [rule weight].sum()`
    /// - convert to `i128` to avoid integer overflow
    pub threshold: [u64; 2],
}

impl CoSigRule {
    #[allow(missing_docs)]
    pub fn new(threshold: [u64; 2]) -> Result<Self> {
        if threshold[0] > threshold[1] || threshold[1] > MAX_TOTAL_POWER {
            return Err(eg!("invalid threshold"));
        }

        Ok(CoSigRule {
            threshold: [threshold[0], threshold[1]],
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub(crate) struct CoSig {
    pk: XfrPublicKey,
    sig: XfrSignature,
}

impl CoSig {
    #[inline(always)]
    fn new(pk: XfrPublicKey, sig: XfrSignature) -> Self {
        CoSig { pk, sig }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
enum CoSigErr {
    KeyUnknown,
    SigInvalid,
    WeightInsufficient,
}

impl fmt::Display for CoSigErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self {
            CoSigErr::KeyUnknown => "found keys outside of the predefined rules",
            CoSigErr::WeightInsufficient => "total weight is lower than the threshold",
            CoSigErr::SigInvalid => "invalid signature",
        };
        write!(f, "{}", msg)
    }
}

#[cfg(test)]
#[allow(missing_docs)]
mod test {
    use {
        super::*,
        crate::staking::{Validator, ValidatorKind},
        rand_chacha::ChaChaRng,
        rand_core::SeedableRng,
        ruc::pnk,
    };

    #[derive(Default, Debug, Deserialize, Serialize)]
    struct Data {
        a: [i32; 12],
        b: [f32; 3],
        c: String,
        d: (),
    }

    fn gen_keypairs(n: u8) -> Vec<XfrKeyPair> {
        let mut prng = ChaChaRng::from_entropy();
        (0..n).map(|_| XfrKeyPair::generate(&mut prng)).collect()
    }

    fn no_replay_token() -> NoReplayToken {
        let rand_n = rand::random::<u64>();
        let seq_id = rand::random::<u64>();
        NoReplayToken::unsafe_new(rand_n, seq_id)
    }

    #[test]
    fn staking_cosig() {
        let kps = gen_keypairs(100);
        let vs = kps
            .iter()
            .map(|kp| {
                Validator::new(
                    vec![],
                    999,
                    kp.get_pk(),
                    [1, 5],
                    Default::default(),
                    ValidatorKind::Initiator,
                )
            })
            .collect::<Result<Vec<_>>>();
        let mut vd = pnk!(ValidatorData::new(1, pnk!(vs)));

        // threshold: 75%
        vd.cosig_rule = pnk!(CoSigRule::new([75, 100]));

        assert!(CoSigRule::new([200, 100]).is_err());
        assert!(CoSigRule::new([200, 1 + MAX_TOTAL_POWER]).is_err());

        let mut data = CoSigOp::create(Data::default(), no_replay_token());
        pnk!(data.batch_sign(&kps.iter().skip(10).collect::<Vec<_>>()));
        assert!(data.check_cosigs(&vd).is_ok());

        kps.iter().skip(10).for_each(|kp| {
            pnk!(data.sign(kp));
        });
        assert!(data.check_cosigs(&vd).is_ok());

        data.data.a = [9; 12];
        assert!(data.check_cosigs(&vd).is_err());
        data.data.a = [0; 12];
        assert!(data.check_cosigs(&vd).is_ok());

        let mut data = CoSigOp::create(Data::default(), no_replay_token());
        pnk!(data.batch_sign(&kps.iter().skip(25).collect::<Vec<_>>()));
        assert!(data.check_cosigs(&vd).is_ok());

        kps.iter().skip(25).for_each(|kp| {
            pnk!(data.sign(kp));
        });
        assert!(data.check_cosigs(&vd).is_ok());

        let mut data = CoSigOp::create(Data::default(), no_replay_token());
        pnk!(data.batch_sign(&kps.iter().skip(45).collect::<Vec<_>>()));
        assert!(data.check_cosigs(&vd).is_err());

        kps.iter().skip(45).for_each(|kp| {
            pnk!(data.sign(kp));
        });
        assert!(data.check_cosigs(&vd).is_err());
    }
}
