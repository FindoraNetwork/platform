//! Multi Signer operation for transaction.

use crate::converter::ASSET_TYPE_FRA;
use crate::data_model::{
    NoReplayToken,
    Operation,
    Transaction,
    BLACK_HOLE_PUBKEY_STAKING, //ASSET_TYPE_FRA,
};
use fp_types::crypto::MultiSigner;
use ruc::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use zei::xfr::{
    sig::{XfrKeyPair, XfrPublicKey, XfrSignature},
    structs::{XfrAmount, XfrAssetType}, //AssetType
};
use zeiutils::serialization::ZeiFromToBytes;

/// Use this operation to transfer.
///
/// This operation only support binded xfr_address is sender address.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct TransferERC20 {
    /// transaction body
    pub data: Data,
    /// transaction signer
    pub public: XfrPublicKey,
    /// transaction signature
    pub signature: XfrSignature,
}

#[allow(missing_docs)]
impl TransferERC20 {
    pub fn new(
        keypair: &XfrKeyPair,
        nonce: NoReplayToken,
        address: MultiSigner,
        input: Vec<u8>,
    ) -> Self {
        let data = Data::new(nonce, address, input);
        let public = keypair.get_pk();
        let signature = keypair.sign(&data.to_bytes());
        Self {
            data,
            public,
            signature,
        }
    }

    pub fn verify(&self) -> Result<()> {
        self.public
            .verify(&self.data.to_bytes(), &self.signature)
            .c(d!())
    }

    pub fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.data.nonce = nonce;
    }

    pub fn get_nonce(&self) -> NoReplayToken {
        self.data.nonce
    }

    pub fn get_related_address(&self) -> XfrPublicKey {
        self.public
    }
}

/// The body of TranserToAccount.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Data {
    /// transaction nonce
    pub nonce: NoReplayToken,
    /// receiver address
    pub address: MultiSigner,
    /// input
    pub input: Vec<u8>,
}

#[allow(missing_docs)]
impl Data {
    pub fn new(nonce: NoReplayToken, address: MultiSigner, input: Vec<u8>) -> Self {
        Data {
            nonce,
            address,
            input,
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        pnk!(bincode::serialize(self))
    }
}

#[allow(missing_docs)]
pub fn is_transfer_erc20_tx(tx: &Transaction) -> bool {
    for op in &tx.body.operations {
        if let Operation::TransferERC20(_) = op {
            return true;
        }
    }
    false
}

#[allow(missing_docs)]
pub fn check_erc20_tx(tx: &Transaction) -> Result<(Vec<u8>, MultiSigner, u64, Vec<u8>)> {
    let mut owner = None;
    let mut nonce = None;
    let mut input = None;
    let mut signer = None;

    let mut assets = HashMap::new();

    for op in &tx.body.operations {
        if let Operation::TransferERC20(ca) = op {
            if owner.is_some() {
                return Err(eg!("tx must have 1 convert account"));
            }
            owner = Some(ca.data.address.clone());
            nonce = Some(ca.get_nonce());
            input = Some(ca.data.input.clone());
            signer = Some(ca.get_related_address().zei_to_bytes())
        }
        if let Operation::TransferAsset(t) = op {
            for o in &t.body.outputs {
                if matches!(o.record.asset_type, XfrAssetType::Confidential(_))
                    || matches!(o.record.amount, XfrAmount::Confidential(_))
                {
                    return Err(eg!(
                        "asset cross ledger transfer not support confidential"
                    ));
                }
                if let XfrAssetType::NonConfidential(ty) = o.record.asset_type {
                    if o.record.public_key == *BLACK_HOLE_PUBKEY_STAKING
                        && ty == ASSET_TYPE_FRA
                    {
                        if let XfrAmount::NonConfidential(i_am) = o.record.amount {
                            if let Some(amount) = assets.get_mut(&ty) {
                                *amount += i_am;
                            } else {
                                assets.insert(ty, i_am);
                            }
                        }
                    }
                }
            }
        }
    }
    if owner.is_none() {
        return Err(eg!("this tx isn't a convert tx"));
    }
    Ok((
        signer.unwrap(),
        owner.unwrap(),
        nonce.unwrap().get_seq_id(),
        input.unwrap(),
    ))
}
