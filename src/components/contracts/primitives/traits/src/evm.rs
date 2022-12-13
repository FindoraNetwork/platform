use config::abci::global_cfg::CFG;
use core::{convert::From, ops::Div};
use fp_core::context::Context;
use fp_types::crypto::Address;
use primitive_types::{H160, H256, U256};
use ruc::Result;

const PROPER_GAS_PRICE: u64 = 100_0000_0000_u64;

pub trait AddressMapping {
    fn convert_to_account_id(address: H160) -> Address;
}

/// Ethereum address mapping.
pub struct EthereumAddressMapping;

impl AddressMapping for EthereumAddressMapping {
    fn convert_to_account_id(address: H160) -> Address {
        Address::from(address)
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
        balance.div(U256::from(10_u64.pow(ETH_DECIMALS - FRA_DECIMALS)))
    }
}

/// Trait that outputs the current transaction gas price.
pub trait FeeCalculator {
    /// Return the minimal required gas price.
    fn min_gas_price(height: u64) -> U256;
}

impl FeeCalculator for () {
    fn min_gas_price(height: u64) -> U256 {
        // 10 GWEI, min gas limit: 21000, min gas price must > 50_0000_0000
        if height > CFG.checkpoint.proper_gas_set_height {
            U256::from(PROPER_GAS_PRICE)
        } else {
            U256::from(100_0000_0000_u64)
        }
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

    /// Introduced in EIP1559 to handle the priority tip payment to the block Author.
    fn pay_priority_fee(ctx: &Context, tip: U256) -> Result<()>;
}
