//!
//! # Staker Update Operation
//!
//! Data representation required when users want to update information of a staker.
//!

use {
    crate::{
        data_model::{NoReplayToken, Transaction},
        staking::{
            evm::EVM_STAKING, td_addr_to_string, Staking, TendermintAddr, Validator,
        },
    },
    config::abci::global_cfg::CFG,
    ed25519_dalek::Signer,
    ruc::*,
    serde::{Deserialize, Serialize},
    tendermint::{signature::Ed25519Signature, PrivateKey, PublicKey, Signature},
    zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSignature},
};

/// Used as the inner object of a `Staker Update Operation`.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct UpdateStakerOps {
    pub(crate) body: Box<Data>,
    pub(crate) pubkey: XfrPublicKey,
    signature: XfrSignature,
    v_signature: Option<Ed25519Signature>,
}

impl UpdateStakerOps {
    /// Check the validity of an operation by running it in a staking simulator.
    #[inline(always)]
    pub fn check_run(
        &self,
        staking_simulator: &mut Staking,
        tx: &Transaction,
    ) -> Result<()> {
        self.apply(staking_simulator, tx).c(d!())
    }

    fn apply(&self, staking: &mut Staking, _tx: &Transaction) -> Result<()> {
        let cur_height = staking.cur_height() as i64;
        if cur_height > CFG.checkpoint.evm_staking_inital_height {
            self.pubkey
                .verify(&self.body.to_bytes(), &self.signature)
                .c(d!())?;
            EVM_STAKING.get().c(d!())?.write().update_validator(
                &self.pubkey,
                &self.body.new_validator.td_addr,
                serde_json::to_string(&self.body.new_validator.memo).c(d!())?,
                self.body.new_validator.commission_rate,
            )
        } else {
            self.verify()
                .c(d!())
                .and_then(|_| self.check_update_context(staking).c(d!()))
                .and_then(|_| staking.update_staker(&self.body.new_validator).c(d!()))
        }
    }

    /// verify signature
    #[inline(always)]
    pub fn verify(&self) -> Result<()> {
        // verify New Validator's signature
        let v = &self.body.new_validator;
        let v_sig = self
            .v_signature
            .as_ref()
            .ok_or(eg!("missing validator signature"))?;
        if PublicKey::from_raw_ed25519(&v.td_pubkey)
            .c(d!())?
            .verify(&self.body.to_bytes(), &Signature::from(*v_sig))
            .is_err()
        {
            return Err(eg!("tendermint key verification failed"));
        }
        self.pubkey
            .verify(&self.body.to_bytes(), &self.signature)
            .c(d!())
    }

    #[inline(always)]
    fn check_update_context(&self, staking: &mut Staking) -> Result<()> {
        let v = &self.body.new_validator;
        staking
            .validator_td_addr_to_app_pk(&td_addr_to_string(&v.td_addr))
            .c(d!())?;
        Ok(())
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
        vltor_key: &PrivateKey,
        validator: TendermintAddr,
        new_validator: Validator,
        nonce: NoReplayToken,
    ) -> Self {
        let body = Box::new(Data::new(validator, new_validator, nonce));
        let signature = keypair.sign(&body.to_bytes());
        let v_signature: Option<Ed25519Signature> = vltor_key
            .ed25519_keypair()
            .map(|k| k.sign(&body.to_bytes()));
        UpdateStakerOps {
            body,
            pubkey: keypair.get_pk(),
            signature,
            v_signature,
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

/// The body of a delegation operation.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Data {
    /// the target validator to update
    pub validator: TendermintAddr,
    /// The new validator information
    pub new_validator: Validator,
    nonce: NoReplayToken,
}

impl Data {
    #[inline(always)]
    fn new(v: TendermintAddr, new_validator: Validator, nonce: NoReplayToken) -> Self {
        Data {
            validator: v,
            new_validator,
            nonce,
        }
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
