use primitive_types::{H160, U256};
use serde::{Deserialize, Serialize};
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::AssetType as ZeiAssetType;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    NonConfidentialTransfer(NonConfidentialTransfer),
    Erc20ToUtxo(Erc20ToUtxo),
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

#[inline(always)]
fn is_default<T: Default + PartialEq>(x: &T) -> bool {
    x == &T::default()
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct TxOutput {
    pub asset: ZeiAssetType,
    pub amount: u64,
    pub target: XfrPublicKey,
    #[serde(skip_serializing_if = "is_default")]
    pub confidential_am: bool,
    #[serde(skip_serializing_if = "is_default")]
    pub confidential_ty: bool,
}

impl From<NonConfidentialOutput> for TxOutput {
    fn from(o: NonConfidentialOutput) -> Self {
        Self {
            asset: o.asset,
            amount: o.amount,
            target: o.target,
            ..Default::default()
        }
    }
}

/// ERC20 -> UTXO
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Erc20ToUtxo {
    pub contract: H160,
    pub amount: U256,
    pub outputs: Vec<TxOutput>,
}
