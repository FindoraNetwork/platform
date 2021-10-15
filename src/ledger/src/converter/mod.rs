//! Multi Signer operation for transaction.

use crate::data_model::{
    NoReplayToken, Operation, Transaction, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY_STAKING,
};
use fp_types::crypto::MultiSigner;
use ruc::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use zei::xfr::{
    sig::{XfrKeyPair, XfrPublicKey, XfrSignature},
    structs::{AssetType, XfrAmount, XfrAssetType},
};

/// Use this operation to transfer.
///
/// This operation only support binded xfr_address is sender address.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ConvertAccount {
    /// transaction body
    pub data: Data,
    /// transaction signer
    pub public: XfrPublicKey,
    /// transaction signature
    pub signature: XfrSignature,
}

#[allow(missing_docs)]
impl ConvertAccount {
    pub fn new(
        keypair: &XfrKeyPair,
        nonce: NoReplayToken,
        address: MultiSigner,
    ) -> Self {
        let data = Data::new(nonce, address);
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
}

#[allow(missing_docs)]
impl Data {
    pub fn new(nonce: NoReplayToken, address: MultiSigner) -> Self {
        Data { nonce, address }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        pnk!(bincode::serialize(self))
    }
}

#[allow(missing_docs)]
pub fn is_convert_tx(tx: &Transaction) -> bool {
    for op in &tx.body.operations {
        if let Operation::ConvertAccount(_) = op {
            return true;
        }
    }
    false
}

#[allow(missing_docs)]
pub fn check_convert_tx(
    tx: &Transaction,
) -> Result<(MultiSigner, HashMap<AssetType, u64>)> {
    let mut owner = None;

    let mut assets = HashMap::new();

    let mut has_sig = false;

    for op in &tx.body.operations {
        if let Operation::ConvertAccount(ca) = op {
            if owner.is_some() {
                return Err(eg!("TransferUTXOsToEVM error: UXTOs can only be transferred to one evm account"));
            }
            if ca.data.nonce != tx.body.no_replay_token {
                return Err(eg!(
                    "TransferUTXOsToEVM error: nonce mismatch no_replay_token"
                ));
            }

            has_sig = has_sig || tx.check_has_signature(&ca.public).is_ok();

            owner = Some(ca.data.address.clone())
        }
        if let Operation::TransferAsset(t) = op {
            for o in &t.body.outputs {
                if matches!(o.record.asset_type, XfrAssetType::Confidential(_))
                    || matches!(o.record.amount, XfrAmount::Confidential(_))
                {
                    return Err(eg!(
                        "TransferUTXOsToEVM error: only support non-confidential UTXOs transfer to an evm account"
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
    if owner.is_none() || !has_sig {
        return Err(eg!(
            "TransferUTXOsToEVM error: not found the evm target account"
        ));
    }
    Ok((owner.unwrap(), assets))
}
