use noah::keys::{PublicKey as XfrPublicKey, PublicKey};
use noah::xfr::structs::AssetType;
use serde::{Deserialize, Serialize};

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
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct NonConfidentialOutput {
    pub asset: AssetType,
    pub amount: u64,
    pub target: XfrPublicKey,
    #[serde(skip)]
    pub decimal: u8,
    #[serde(skip)]
    pub max_supply: u64,
}

impl Default for NonConfidentialOutput {
    fn default() -> Self {
        NonConfidentialOutput {
            asset: Default::default(),
            amount: 0,
            target: PublicKey::default_ed25519(),
            decimal: 0,
            max_supply: 0,
        }
    }
}
