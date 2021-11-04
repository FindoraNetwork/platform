//! Multi Signer operation for transaction.

use crate::converter::ASSET_TYPE_FRA;
use crate::data_model::{
    AssetTypeCode, NoReplayToken, Operation, Transaction, BLACK_HOLE_PUBKEY_STAKING,
};
use crate::store::LedgerState;
use fp_types::crypto::MultiSigner;
use ruc::*;
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use zei::xfr::{
    sig::{XfrKeyPair, XfrPublicKey, XfrSignature},
    structs::XfrAssetType, //AssetType
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
    if tx.body.operations.len() == 2 {
        return matches!(
            tx.body.operations[..],
            [Operation::TransferAsset(_), Operation::TransferERC20(_)]
        );
    }
    false
}

/// check asset if suitable for a uto->erc20 transaction
pub fn check_valid_asset(tx: &Transaction, la: &LedgerState) -> bool {
    let address;
    if let Operation::TransferERC20(te) = &tx.body.operations[1] {
        address = &te.data.address
    } else {
        return false;
    }

    if let Operation::TransferAsset(ta) = &tx.body.operations[0] {
        for txo in &ta.body.outputs {
            if txo.record.is_public() {
                // unwrap is safe here, because current asset is public
                let asset = txo.record.asset_type.get_asset_type().unwrap();
                if let Some(at) = la.get_asset_type(&AssetTypeCode { val: asset }) {
                    if let Ok(signer) = MultiSigner::from_str(&at.properties.memo.0) {
                        if &signer != address {
                            // asset and erc20 address are not bound
                            return false;
                        }
                    } else {
                        // Not binding with a valid address
                        return false;
                    }
                } else {
                    // non-existed asset type
                    return false;
                }
            } else {
                // confidential type or amount
                return false;
            }
        }
    }
    true
}

#[allow(missing_docs)]
pub fn check_erc20_tx(tx: &Transaction) -> Result<(Vec<u8>, MultiSigner, u64, Vec<u8>)> {
    let mut owner = None;
    let mut nonce = None;
    let mut input = None;
    let mut signer = None;

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
                // both amount and asset type are no-confidential
                // FIXME: check if transferable
                if !o.record.is_public() {
                    return Err(eg!(
                        "Findora custom asset cross ledger transfer not support confidential"
                    ));
                }
                // check utxo
                if let XfrAssetType::NonConfidential(ty) = o.record.asset_type {
                    if !(o.record.public_key == *BLACK_HOLE_PUBKEY_STAKING
                        && ty != ASSET_TYPE_FRA)
                    {
                        return Err(eg!("Only Findora custom asset is supported"));
                    }
                }
            }
        }
    }
    if owner.is_none() {
        return Err(eg!("this isn't a valid utxo-to-erc20 tx"));
    }
    Ok((
        signer.unwrap(),
        owner.unwrap(),
        nonce.unwrap().get_seq_id(),
        input.unwrap(),
    ))
}
