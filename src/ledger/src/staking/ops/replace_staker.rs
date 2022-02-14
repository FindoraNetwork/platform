//! Staker replace operation
//!
//! Data representation required when users want to replace their secert key and public key.
//!

use {
    crate::data_model::{NoReplayToken, Transaction},
    crate::staking::Staking,
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
        new_public_key: XfrPublicKey,
        new_tendermint_addr: Option<Vec<u8>>,
        nonce: NoReplayToken,
    ) -> Self {
        let body = Data {
            new_public_key,
            new_tendermint_addr,
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
        if let Some(new_td_addr) = &self.body.new_tendermint_addr {
            if new_td_addr.len() != 20 {
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
        dbg!(staking_simulator.check_and_replace_staker(
            &self.pubkey,
            self.body.new_public_key,
            self.body.new_tendermint_addr.clone(),
        ))
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
    pub new_tendermint_addr: Option<Vec<u8>>,
    nonce: NoReplayToken,
}

impl Data {
    #[inline(always)]
    fn to_bytes(&self) -> Vec<u8> {
        pnk!(bincode::serialize(self))
    }
}
