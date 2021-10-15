//! Multi Signer operation for transaction.

use crate::data_model::{
    NoReplayToken, Operation, Transaction, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY_STAKING,
};
use fp_types::crypto::MultiSigner;
use ruc::*;
use serde::{Deserialize, Serialize};
use zei::xfr::{
    sig::XfrPublicKey,
    structs::{XfrAmount, XfrAssetType},
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
    pub value: u64,
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
    if let Some(Operation::ConvertAccount(_)) = tx.body.operations.first() {
        true
    } else {
        false
    }
}

#[allow(missing_docs)]
pub fn check_convert_account(tx: &Transaction) -> Result<(MultiSigner, u64)> {
    let mut signer = None;
    let mut target = None;
    let mut expected_value = 0_u64;
    let mut convert_amount = 0_u64;
    let mut has_sig = false;
    let mut has_signer = false;

    for op in &tx.body.operations {
        match op {
            Operation::ConvertAccount(ca) => {
                if target.is_some() {
                    return Err(eg!("TransferUTXOsToEVM error: UXTOs can only be transferred to one evm account"));
                }
                if ca.nonce != tx.body.no_replay_token {
                    return Err(eg!(
                        "TransferUTXOsToEVM error: nonce mismatch no_replay_token"
                    ));
                }

                signer = Some(ca.signer);
                target = Some(ca.receiver.clone());
                expected_value = ca.value;

                has_sig = tx.check_has_signature(&ca.signer).is_ok();
            }
            Operation::TransferAsset(t) => {
                if !has_sig || signer.is_none() {
                    return Err(eg!("TransferUTXOsToEVM error: invalid signature"));
                }
                if !has_signer {
                    has_signer = t
                        .get_owner_addresses()
                        .iter()
                        .any(|&pk| pk == signer.unwrap());
                }

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
                            if let XfrAmount::NonConfidential(amount) = o.record.amount {
                                convert_amount += amount;
                            }
                        }
                    }
                }
            }
            _ => {
                return Err(eg!("TransferUTXOsToEVM error: invalid operation"));
            }
        }
    }

    if expected_value != convert_amount {
        return Err(eg!("TransferUTXOsToEVM error: invalid convert value"));
    }
    if !has_signer {
        return Err(eg!("TransferUTXOsToEVM error: not found signer"));
    }
    if target.is_none() {
        return Err(eg!(
            "TransferUTXOsToEVM error: not found the evm target account"
        ));
    }

    Ok((target.unwrap(), expected_value))
}
