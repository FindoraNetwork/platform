//! Multi Signer operation for transaction.

use crate::converter::erc20::is_transfer_erc20_tx;
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

pub mod erc20;

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

/// tx converting type between findora and evm
pub enum ConvertingType {
    /// convert native and EVM token
    ConvertAccount,
    /// convert ERC20 asset and findora custom asset
    ERC20,
    /// convert FRC20 asset and findora custom asset
    FRC20,
    /// handle findora custom asset in xhub module
    FindoraAsset,
}

fn is_findora_asset_tx(tx: &Transaction) -> bool {
    tx.body.operations.iter().any(|op| {
        matches!(op, Operation::DefineAsset(_))
            || matches!(op, Operation::IssueAsset(_))
            //|| matches!(op, Operation::TransferAsset(_))
            || matches!(op, Operation::UpdateMemo(_))
    })
}

/// check if it's a converting-related tx
pub fn check_converting_tx_type(tx: &Transaction) -> Option<ConvertingType> {
    if is_convert_account(tx) {
        Some(ConvertingType::ConvertAccount)
    } else if is_transfer_erc20_tx(tx) {
        Some(ConvertingType::ERC20)
    } else if is_findora_asset_tx(tx) {
        Some(ConvertingType::FindoraAsset)
    } else {
        None
    }
}

#[allow(missing_docs)]
pub fn check_convert_account(tx: &Transaction) -> Result<(MultiSigner, u64)> {
    let signer;
    let target;
    let expected_value;

    if let Some(Operation::ConvertAccount(ca)) = tx.body.operations.last() {
        if ca.nonce != tx.body.no_replay_token {
            return Err(eg!(
                "TransferUTXOsToEVM error: nonce mismatch no_replay_token"
            ));
        }
        if tx.check_has_signature(&ca.signer).is_err() {
            return Err(eg!("TransferUTXOsToEVM error: invalid signature"));
        }

        signer = ca.signer;
        target = ca.receiver.clone();
        expected_value = ca.value;
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
                    && ty == ASSET_TYPE_FRA
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

    Ok((target, expected_value))
}
