use fp_core::context::Context;
use fp_types::crypto::Address;
use primitive_types::{H160, H256, U256};
use ruc::Result;
use std::convert::TryFrom;

pub trait AddressMapping {
    fn convert_to_account_id(address: H160) -> Address;
}

/// Ethereum address mapping.
pub struct EthereumAddressMapping;

impl AddressMapping for EthereumAddressMapping {
    fn convert_to_account_id(address: H160) -> Address {
        let mut data = [0u8; 32];
        data[0..20].copy_from_slice(&address[..]);
        Address::try_from(&data[..]).unwrap()
    }
}

/// A trait for getting a block hash by number.
pub trait BlockHashMapping {
    fn block_hash(ctx: &Context, number: U256) -> Option<H256>;
}

pub trait DecimalsMapping {
    fn from_native_token(balance: U256) -> Option<U256>;

    fn convert_to_native_token(balance: U256) -> U256;
}

/// FRA decimals
const FRA_DECIMALS: u32 = 6;

/// ETH decimals
const ETH_DECIMALS: u32 = 18;

/// Ethereum decimals mapping.
pub struct EthereumDecimalsMapping;

impl DecimalsMapping for EthereumDecimalsMapping {
    fn from_native_token(balance: U256) -> Option<U256> {
        balance.checked_mul(U256::from(10_u64.pow(ETH_DECIMALS - FRA_DECIMALS)))
    }

    fn convert_to_native_token(balance: U256) -> U256 {
        balance
            .checked_div(U256::from(10_u64.pow(ETH_DECIMALS - FRA_DECIMALS)))
            .unwrap_or_else(U256::zero)
    }
}

/// Trait that outputs the current transaction gas price.
pub trait FeeCalculator {
    /// Return the minimal required gas price.
    fn min_gas_price() -> U256;
}

impl FeeCalculator for () {
    fn min_gas_price() -> U256 {
        // 100 GWEI
        U256::from(1000_0000_0000_u64)
    }
}

/// Handle withdrawing, refunding and depositing of transaction fees.
pub trait OnChargeEVMTransaction {
    /// Before the transaction is executed the payment of the transaction fees
    /// need to be secured.
    fn withdraw_fee(ctx: &Context, who: &H160, fee: U256) -> Result<()>;

    /// After the transaction was executed the actual fee can be calculated.
    /// This function should refund any overpaid fees.
    fn correct_and_deposit_fee(
        ctx: &Context,
        who: &H160,
        corrected_fee: U256,
        already_withdrawn: U256,
    ) -> Result<()>;
}
