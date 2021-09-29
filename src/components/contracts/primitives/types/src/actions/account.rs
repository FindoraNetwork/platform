use crate::crypto::Address;
use primitive_types::U256;
use serde::{Deserialize, Serialize};
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::AssetType;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Transfer(FinerTransfer),
    TransferToUTXO(TransferToUTXO),
}

/// Account balance convert to utxo balance.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct MintOutput {
    pub asset: AssetType,
    pub amount: u64,
    pub target: XfrPublicKey,
}

/// Findora or Ethereum account address balance transfer to utxo.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TransferToUTXO {
    pub outputs: Vec<MintOutput>,
}

/// Findora native account address balance transfer to another account address.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FinerTransfer {
    pub to: Address,
    pub amount: U256,
}
