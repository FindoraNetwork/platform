//! Multi Signer operation for transaction.

use crate::data_model::{
    NoReplayToken, Operation, Transaction, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY_STAKING,
};
use fp_types::crypto::MultiSigner;
use ruc::*;
use serde::{Deserialize, Serialize};
use noah::xfr::{
    sig::XfrPublicKey,
    structs::{AssetType, XfrAmount, XfrAssetType},
};

/// Use this operation to transfer.
///
/// This operation only support binded xfr_address is sender address.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ConvertAccount {
    /// transaction signer
    pub signer: XfrPublicKey,
    /// transaction nonce
    pub nonce: NoReplayToken,
    /// receiver address
    pub receiver: MultiSigner,
    /// convert UTXOs value
    #[serde(with = "serde_strz")]
    pub value: u64,

    /// convert asset type.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub asset_type: Option<AssetType>,

    /// convert asset lowlevel data.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lowlevel_data: Option<Vec<u8>>,
}

#[allow(missing_docs)]
impl ConvertAccount {
    pub fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.nonce = nonce;
    }

    pub fn get_nonce(&self) -> NoReplayToken {
        self.nonce
    }

    pub fn get_related_address(&self) -> XfrPublicKey {
        self.signer
    }
}

#[allow(missing_docs)]
pub fn is_convert_account(tx: &Transaction) -> bool {
    tx.body.operations.len() == 2
        && matches!(
            tx.body.operations.first(),
            Some(Operation::TransferAsset(_))
        )
        && matches!(
            tx.body.operations.last(),
            Some(Operation::ConvertAccount(_))
        )
}

#[allow(missing_docs)]
pub fn check_convert_account(
    tx: &Transaction,
) -> Result<(XfrPublicKey, MultiSigner, u64, AssetType, Vec<u8>)> {
    let signer;
    let target;
    let expected_value;
    let expected_asset;
    let expected_lowlevel;

    if let Some(Operation::ConvertAccount(ca)) = tx.body.operations.last() {
        if ca.nonce != tx.body.no_replay_token {
            return Err(eg!(
                "TransferUTXOsToEVM error: nonce mismatch no_replay_token"
            ));
        }
        if tx.check_has_signature(&ca.signer).is_err() {
            return Err(eg!("TransferUTXOsToEVM error: invalid signature"));
        }
        if let MultiSigner::Xfr(_pk) = ca.receiver {
            return Err(eg!("TransferUTXOsToEVM error: invalid receiver address"));
        }

        signer = ca.signer;
        target = ca.receiver.clone();
        expected_value = ca.value;
        if let Some(at) = ca.asset_type {
            expected_asset = at;
        } else {
            expected_asset = ASSET_TYPE_FRA;
        }
        if let Some(l) = &ca.lowlevel_data {
            expected_lowlevel = l.clone();
        } else {
            expected_lowlevel = Vec::new();
        }
    } else {
        return Err(eg!(
            "TransferUTXOsToEVM error: invalid ConvertAccount operation"
        ));
    }

    if let Some(Operation::TransferAsset(t)) = tx.body.operations.first() {
        let has_signer = t.get_owner_addresses().iter().any(|&pk| pk == signer);
        if !has_signer {
            return Err(eg!("TransferUTXOsToEVM error: not found signer"));
        }

        let mut convert_amount = 0_u64;
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
                    && ty == expected_asset
                {
                    if let XfrAmount::NonConfidential(amount) = o.record.amount {
                        convert_amount += amount;
                    }
                }
            }
        }
        if expected_value != convert_amount {
            return Err(eg!("TransferUTXOsToEVM error: invalid convert value"));
        }
    } else {
        return Err(eg!(
            "TransferUTXOsToEVM error: invalid TransferAsset operation"
        ));
    }

    Ok((
        signer,
        target,
        expected_value,
        expected_asset,
        expected_lowlevel,
    ))
}
