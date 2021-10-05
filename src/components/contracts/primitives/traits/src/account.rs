use fp_core::{account::SmartAccount, context::Context};
use primitive_types::U256;
use ruc::Result;

pub trait AccountAsset<Address> {
    /// The smart account info of `who`.
    fn account_of(
        ctx: &Context,
        who: &Address,
        height: Option<u64>,
    ) -> Option<SmartAccount>;

    /// The balance of `who`.
    fn balance(ctx: &Context, who: &Address) -> U256;

    /// The nonce of `who`.
    fn nonce(ctx: &Context, who: &Address) -> U256;

    /// The account executes new transactions and increase nonce
    fn inc_nonce(ctx: &Context, who: &Address) -> Result<U256>;

    /// Transfer some balance from `sender` to `dest`
    fn transfer(
        ctx: &Context,
        sender: &Address,
        dest: &Address,
        balance: U256,
    ) -> Result<()>;

    /// Mints `value` to the free balance of `who`.
    fn mint(ctx: &Context, target: &Address, balance: U256) -> Result<()>;

    /// Burns `value` to the free balance of `who`.
    fn burn(ctx: &Context, target: &Address, balance: U256) -> Result<()>;

    /// Removes some balance from `who` account.
    fn withdraw(ctx: &Context, who: &Address, value: U256) -> Result<()>;

    /// Refund some balance from `who` account.
    fn refund(ctx: &Context, who: &Address, value: U256) -> Result<()>;
}

/// Outputs the current transaction fee.
pub trait FeeCalculator {
    fn min_fee() -> U256;
}

impl FeeCalculator for () {
    fn min_fee() -> U256 {
        U256::zero()
    }
}
