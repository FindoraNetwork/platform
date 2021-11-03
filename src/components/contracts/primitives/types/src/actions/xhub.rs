use primitive_types::{H160, U256};
use serde::{Deserialize, Serialize};
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::AssetType as ZeiAssetType;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    NonConfidentialTransfer(NonConfidentialTransfer),
    ERC20ToUTXO(ERC20ToUTXO),
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
    pub asset: ZeiAssetType,
    pub amount: u64,
    pub target: XfrPublicKey,
}

/// ERC20 -> UTXO
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ERC20ToUTXO {
    pub nonce: U256,
    pub gas_price: U256,
    pub gas_limit: U256,
    pub contractaddress: H160,
    pub input: Vec<u8>,
    pub amount: U256,
    pub outputs: Vec<NonConfidentialOutput>,
}
