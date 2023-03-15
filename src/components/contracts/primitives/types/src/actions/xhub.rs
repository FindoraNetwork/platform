use serde::{Deserialize, Serialize};
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::AssetType;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    NonConfidentialTransfer(NonConfidentialTransfer),
}

/// Findora evm account balance transfer to NonConfidential utxo.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct NonConfidentialTransfer {
    pub input_value: u64,
    pub outputs: Vec<NonConfidentialOutput>,
}

/// Evm account balance convert to NonConfidential utxo.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct NonConfidentialOutput {
    pub asset: AssetType,
    pub amount: u64,
    pub target: XfrPublicKey,
}
