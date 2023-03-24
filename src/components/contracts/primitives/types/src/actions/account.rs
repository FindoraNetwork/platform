use zei::noah_api::xfr::structs::AssetType;
use serde::{Deserialize, Serialize};
use zei::XfrPublicKey;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
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
