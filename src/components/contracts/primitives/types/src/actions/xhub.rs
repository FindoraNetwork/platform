use noah::xfr::structs::AssetType;
use serde::{Deserialize, Serialize};
use zei::XfrPublicKey;

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
    #[serde(skip)]
    pub decimal: u8,
    #[serde(skip)]
    pub max_supply: u64,
}
