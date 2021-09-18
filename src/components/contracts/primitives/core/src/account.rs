use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SmartAccount {
    /// Account nonce.
    pub nonce: u64,
    /// Account balance(native asset). Note: decimals is 6.
    pub balance: u128,
    /// Balance which is reserved and may not be used.
    /// such as: staking deposit
    pub reserved: u128,
}
