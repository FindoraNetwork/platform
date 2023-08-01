//! Staker replace operation
//!
//! Data representation required when users want to replace their secert key and public key.
//!

use {
    crate::{
        data_model::{NoReplayToken, Transaction},
        staking::{evm::EVM_STAKING, Staking},
    },
    config::abci::global_cfg::CFG,
    fp_types::H160,
    ruc::*,
    serde::{Deserialize, Serialize},
    zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSignature},
};

/// Used for `Staker Replace Operation`.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ReplaceStakerOps {
    body: Data,
    pubkey: XfrPublicKey,
    signature: XfrSignature,
}

impl ReplaceStakerOps {
    ///create a new replace operation.
    pub fn new(
        keypair: &XfrKeyPair,
        new_delegator: H160,
        new_delegator_pk: Option<Vec<u8>>,
        td_addr: Vec<u8>,
        nonce: NoReplayToken,
    ) -> Self {
        let body = Data {
            new_public_key: XfrPublicKey::default(),
            new_tendermint_params: None,
            new_delegator: Some(new_delegator),
            new_delegator_pk,
            td_addr: Some(td_addr),
            nonce,
        };

        let signature = keypair.sign(&body.to_bytes());

        ReplaceStakerOps {
            body,
            pubkey: keypair.get_pk(),
            signature,
        }
    }

    ///verify the body with the public key
    pub fn verify(&self) -> Result<()> {
        if let Some(new_params) = &self.body.new_tendermint_params {
            let _ = tendermint::PublicKey::from_raw_ed25519(&new_params.pubkey)
                .c(d!("Invalid tendermint public key."))?;

            if new_params.address.len() != 20 {
                return Err(eg!("Invalid tendermint address."));
            }
        }
        self.pubkey
            .verify(&self.body.to_bytes(), &self.signature)
            .c(d!("Verification failed."))?;
        Ok(())
    }

    #[allow(missing_docs)]
    pub fn check_run(
        &self,
        staking_simulator: &mut Staking,
        _tx: &Transaction,
    ) -> Result<()> {
        self.verify()?;
        let cur_height = staking_simulator.cur_height() as i64;
        if cur_height > CFG.checkpoint.evm_staking_inital_height {
            let validator = self
                .body
                .td_addr
                .clone()
                .ok_or(eg!("replace staker validator not found"))?;

            let new_delegator_address = self
                .body
                .new_delegator
                .ok_or(eg!("replace staker new_staker_address not found"))?;

            EVM_STAKING.get().c(d!())?.write().replace_delegator(
                &validator,
                &self.pubkey,
                new_delegator_address,
                self.body.new_delegator_pk.clone(),
            )
        } else {
            dbg!(staking_simulator.check_and_replace_staker(
                &self.pubkey,
                self.body.new_public_key,
                self.body
                    .new_tendermint_params
                    .clone()
                    .map(|p| (p.address, p.pubkey)),
            ))
        }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.body.nonce = nonce;
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_nonce(&self) -> NoReplayToken {
        self.body.nonce
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_related_pubkeys(&self) -> Vec<XfrPublicKey> {
        vec![self.pubkey]
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Data {
    pub new_public_key: XfrPublicKey,
    pub new_tendermint_params: Option<TendermintParams>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub new_delegator: Option<H160>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub new_delegator_pk: Option<Vec<u8>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub td_addr: Option<Vec<u8>>,
    nonce: NoReplayToken,
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct TendermintParams {
    address: Vec<u8>,
    pubkey: Vec<u8>,
}

impl Data {
    #[inline(always)]
    fn to_bytes(&self) -> Vec<u8> {
        pnk!(bincode::serialize(self))
    }
}
