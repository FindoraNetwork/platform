use crate::crypto::Address;
use primitive_types::H160;
use primitive_types::U256;
use serde::{Deserialize, Serialize};
use zei::xfr::{sig::XfrPublicKey, structs::AssetType as ZeiAssetType};
// use std::io::Bytes;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    TransferToUTXO(TransferToUTXO),
    ERC20ToUTXO(ERC20ToUTXO),
}

/// Account balance convert to utxo balance.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct MintOutput {
    pub asset: ZeiAssetType,
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

/// ERC20 -> UTXO
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ERC20ToUTXO {
    pub nonce: U256,
    pub gas_price: U256,
    pub gas_limit: U256,
    pub contractaddress: H160,
    pub input: Vec<u8>,
    pub amount: U256,
    pub outputs: Vec<MintOutput>,
}
