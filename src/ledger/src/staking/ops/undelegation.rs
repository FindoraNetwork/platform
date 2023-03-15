//!
//! # Un-Delegation Operation
//!
//! Data representation required when users propose a un-delegation.
//!

use {
    crate::{
        data_model::{NoReplayToken, Operation, Transaction},
        staking::{PartialUnDelegation, Staking},
    },
    ruc::*,
    serde::{Deserialize, Serialize},
    zei::{XfrKeyPair, XfrPublicKey, XfrSignature},
};

/// Used as the inner object of a `UnDelegation Operation`.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct UnDelegationOps {
    body: Data,
    pub(crate) pubkey: XfrPublicKey,
    signature: XfrSignature,
}

impl UnDelegationOps {
    /// Check the validity of an operation by running it in a staking simulator.
    #[inline(always)]
    pub fn check_run(
        &self,
        staking_simulator: &mut Staking,
        tx: &Transaction,
    ) -> Result<()> {
        self.apply(staking_simulator, tx).c(d!())
    }

    /// Apply new delegation to the target `Staking` instance.
    pub fn apply(&self, staking: &mut Staking, tx: &Transaction) -> Result<()> {
        self.verify()
            .c(d!())
            .and_then(|_| Self::check_context(tx).c(d!()))
            .and_then(|pu| staking.undelegate(&self.pubkey, pu).c(d!()))
    }

    /// Verify signature.
    #[inline(always)]
    pub fn verify(&self) -> Result<()> {
        self.pubkey
            .verify(&self.body.to_bytes(), &self.signature)
            .c(d!())
    }

    #[inline(always)]
    fn check_context(tx: &Transaction) -> Result<Option<&PartialUnDelegation>> {
        check_undelegation_context(tx).c(d!())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_related_pubkeys(&self) -> Vec<XfrPublicKey> {
        vec![self.pubkey]
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        keypair: &XfrKeyPair,
        nonce: NoReplayToken,
        pu: Option<PartialUnDelegation>,
    ) -> Self {
        let body = Data::new(nonce, pu);
        let signature = keypair.sign(&body.to_bytes()).unwrap();
        UnDelegationOps {
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

// The body of a delegation operation.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
struct Data {
    pu: Option<PartialUnDelegation>,
    nonce: NoReplayToken,
}

impl Data {
    #[inline(always)]
    fn new(nonce: NoReplayToken, pu: Option<PartialUnDelegation>) -> Self {
        Data { pu, nonce }
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

#[inline(always)]
fn check_undelegation_context(tx: &Transaction) -> Result<Option<&PartialUnDelegation>> {
    let ud = tx
        .body
        .operations
        .iter()
        .filter_map(|op| {
            if let Operation::UnDelegation(ud) = op {
                Some(ud)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    if 1 == ud.len() {
        Ok(ud[0].body.pu.as_ref())
    } else {
        Err(eg!())
    }
}
