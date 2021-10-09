use primitive_types::U256;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SmartAccount {
    /// Account nonce.
    pub nonce: U256,
    /// Account balance(native asset). Note: decimals is 6.
    pub balance: U256,
    /// Balance which is reserved and may not be used.
    /// such as: staking deposit, transaction fee
    pub reserved: U256,
}
