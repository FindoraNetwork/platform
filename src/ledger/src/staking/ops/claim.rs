//!
//! # Claim
//!
//! Take back some partial rewards of a valid delegation.
//!

use config::abci::global_cfg::CFG;

use crate::staking::evm::EVM_STAKING;

use {
    crate::{data_model::NoReplayToken, staking::Staking},
    noah::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSignature},
    ruc::*,
    serde::{Deserialize, Serialize},
};

/// Used as the inner object of a `Claim Operation`.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ClaimOps {
    pub(crate) body: Data,
    pub(crate) pubkey: XfrPublicKey,
    signature: XfrSignature,
}

impl ClaimOps {
    /// Check the validity of an operation by running it in a staking simulator.
    #[inline(always)]
    pub fn check_run(&self, staking_simulator: &mut Staking) -> Result<()> {
        self.apply(staking_simulator).c(d!())
    }

    /// Apply new claim to the target `Staking` instance.
    pub fn apply(&self, staking: &mut Staking) -> Result<()> {
        let cur_height = staking.cur_height() as i64;
        if cur_height < CFG.checkpoint.evm_staking {
            self.verify()
                .c(d!())
                .and_then(|_| staking.claim(self.pubkey, self.body.amount).c(d!()))
        } else {
            self.verify()?;
            let am = self.body.amount.c(d!(eg!("Missing amount.")))?;
            EVM_STAKING.get().c(d!())?.write().claim(&self.pubkey, am)?;
            Ok(())
        }
    }

    /// Verify signature.
    #[inline(always)]
    pub fn verify(&self) -> Result<()> {
        self.pubkey
            .verify(&self.body.to_bytes(), &self.signature)
            .c(d!())
    }

    #[inline(always)]
    /// The publickey which issued ClaimOps
    pub fn get_claim_publickey(&self) -> XfrPublicKey {
        self.pubkey
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_related_pubkeys(&self) -> Vec<XfrPublicKey> {
        vec![self.pubkey]
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(keypair: &XfrKeyPair, amount: Option<u64>, nonce: NoReplayToken) -> Self {
        let body = Data::new(amount, nonce);
        let signature = keypair.sign(&body.to_bytes()).unwrap();
        ClaimOps {
            body,
            pubkey: keypair.get_pk(),
            signature,
        }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.body.set_nonce(nonce);
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_nonce(&self) -> NoReplayToken {
        self.body.get_nonce()
    }
}

/// The body of a claim operation.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Data {
    /// how much to claim
    pub amount: Option<u64>,
    nonce: NoReplayToken,
}

impl Data {
    #[inline(always)]
    fn new(amount: Option<u64>, nonce: NoReplayToken) -> Self {
        Data { amount, nonce }
    }

    #[inline(always)]
    fn to_bytes(&self) -> Vec<u8> {
        pnk!(bincode::serialize(self))
    }

    #[inline(always)]
    fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.nonce = nonce;
    }

    #[inline(always)]
    fn get_nonce(&self) -> NoReplayToken {
        self.nonce
    }
}
