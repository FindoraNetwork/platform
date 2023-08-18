//!
//! # Delegation Operation
//!
//! Data representation required when users propose a delegation.
//!

use {
    crate::{
        data_model::{
            NoReplayToken, Operation, Transaction, ASSET_TYPE_FRA,
            BLACK_HOLE_PUBKEY_STAKING,
        },
        staking::{
            deny_relative_inputs, evm::EVM_STAKING, td_addr_to_bytes, td_addr_to_string,
            Amount, Staking, TendermintAddr, Validator, STAKING_VALIDATOR_MIN_POWER,
        },
    },
    config::abci::global_cfg::CFG,
    ed25519_dalek::Signer,
    ruc::*,
    serde::{Deserialize, Serialize},
    std::collections::HashSet,
    tendermint::{signature::Ed25519Signature, PrivateKey, PublicKey, Signature},
    zei::{
        noah_api::xfr::structs::{XfrAmount, XfrAssetType},
        {XfrKeyPair, XfrPublicKey, XfrSignature},
    },
};

/// Used as the inner object of a `Delegation Operation`.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct DelegationOps {
    pub(crate) body: Box<Data>,
    pub(crate) pubkey: XfrPublicKey,
    signature: XfrSignature,
    v_signature: Option<Ed25519Signature>,
}

impl DelegationOps {
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
        let cur_height = staking.cur_height() as i64;
        self.verify()?;
        if cur_height > CFG.checkpoint.evm_staking_inital_height {
            let am = check_delegation_context(tx).c(d!())?;
            if let Some(new_validator) = self.body.new_validator.as_ref() {
                let memo = serde_json::to_string(&new_validator.memo).c(d!())?;
                EVM_STAKING.get().c(d!())?.write().stake(
                    &self.pubkey,
                    am,
                    &new_validator.td_addr,
                    new_validator.td_pubkey.to_owned(),
                    memo,
                    new_validator.commission_rate,
                )?;
            } else {
                EVM_STAKING.get().c(d!())?.write().delegate(
                    &self.pubkey,
                    am,
                    &td_addr_to_bytes(&self.body.validator)?,
                )?;
            }

            Ok(())
        } else {
            self.check_set_context(staking, tx).c(d!()).and_then(|am| {
                staking
                    .delegate(self.pubkey, &self.body.validator, am)
                    .c(d!())
            })
        }
    }

    /// Verify signature.
    #[inline(always)]
    pub fn verify(&self) -> Result<()> {
        // verify New Validator's signature
        if let Some(v) = self.body.new_validator.as_ref() {
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
        }
        self.pubkey
            .verify(&self.body.to_bytes(), &self.signature)
            .c(d!())
    }

    #[inline(always)]
    fn check_set_context(
        &self,
        staking: &mut Staking,
        tx: &Transaction,
    ) -> Result<Amount> {
        let am = check_delegation_context(tx).c(d!())?;

        // Self Staking - New Validator
        if let Some(v) = self.body.new_validator.as_ref() {
            let h = staking.cur_height;

            if !v.staking_is_basic_valid()
                || am < STAKING_VALIDATOR_MIN_POWER
                || self.body.validator != td_addr_to_string(&v.td_addr)
            {
                return Err(eg!("invalid"));
            }

            staking
                .validator_check_power_x(am, 0)
                .c(d!())
                .and_then(|_| staking.validator_add_staker(h, v.clone()).c(d!()))?;
        }

        Ok(am)
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
        vltor_key: Option<&PrivateKey>,
        amount: Amount,
        validator: TendermintAddr,
        new_validator: Option<Validator>,
        nonce: NoReplayToken,
    ) -> Self {
        let body = Box::new(Data::new(validator, new_validator, amount, nonce));
        let signature = keypair.sign(&body.to_bytes()).unwrap();
        let v_signature: Option<Ed25519Signature> = vltor_key
            .and_then(|pk| pk.ed25519_keypair().map(|k| k.sign(&body.to_bytes())));
        DelegationOps {
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
    /// the target validator to delegated to
    pub validator: TendermintAddr,
    /// if set this field, then enter staking flow
    pub new_validator: Option<Validator>,
    /// amount of current delegation
    pub amount: Amount,
    nonce: NoReplayToken,
}

impl Data {
    #[inline(always)]
    fn new(
        v: TendermintAddr,
        new_validator: Option<Validator>,
        amount: Amount,
        nonce: NoReplayToken,
    ) -> Self {
        Data {
            validator: v,
            new_validator,
            amount,
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

fn check_delegation_context(tx: &Transaction) -> Result<Amount> {
    let owner = tx
        .body
        .operations
        .iter()
        .flat_map(|op| {
            if let Operation::Delegation(ref x) = op {
                Some((x.pubkey, x.body.amount))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    // only one delegation operation is allowed per transaction
    if 1 != owner.len() {
        return Err(eg!());
    }

    check_delegation_context_principal(tx, owner[0])
        .c(d!("delegation amount is not paid correctly"))
}

fn check_delegation_context_principal(
    tx: &Transaction,
    owner: (XfrPublicKey, Amount),
) -> Result<Amount> {
    let target_pk = XfrPublicKey::from_noah(&BLACK_HOLE_PUBKEY_STAKING);

    let am = tx
        .body
        .operations
        .iter()
        .map(|op| {
            if let Operation::TransferAsset(ref x) = op {
                deny_relative_inputs(x).c(d!())?;

                if x.body.outputs.iter().any(|o| {
                    matches!(o.record.asset_type, XfrAssetType::Confidential(_))
                        || matches!(o.record.amount, XfrAmount::Confidential(_))
                }) {
                    return Err(eg!(
                        "Confidential TXO outputs is not allowed in delegation"
                    ));
                }

                let keynum = x
                    .body
                    .transfer
                    .inputs
                    .iter()
                    .map(|i| i.public_key)
                    .collect::<HashSet<_>>()
                    .len();

                // make sure:
                //
                // - all inputs are owned by a same address
                // - the owner of all inputs is same as the delegator
                if 1 == keynum && owner.0 == x.body.transfer.inputs[0].public_key {
                    let am = x
                        .body
                        .outputs
                        .iter()
                        .flat_map(|o| {
                            if let XfrAssetType::NonConfidential(ty) =
                                o.record.asset_type
                            {
                                if ty == ASSET_TYPE_FRA
                                    && target_pk == o.record.public_key
                                {
                                    if let XfrAmount::NonConfidential(i_am) =
                                        o.record.amount
                                    {
                                        return Some(i_am);
                                    }
                                }
                            }
                            None
                        })
                        .sum::<u64>();

                    return Ok(am);
                }
            }
            Ok(0)
        })
        .collect::<Result<Vec<_>>>()
        .c(d!())?
        .iter()
        .sum();

    alt!(
        0 < am && am == owner.1,
        Ok(am),
        Err(eg!("Invalid delegation principal"))
    )
}
