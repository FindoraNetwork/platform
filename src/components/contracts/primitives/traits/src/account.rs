use fp_core::{account::SmartAccount, context::Context};
use ruc::Result;

pub trait AccountAsset<Address> {
    /// The smart account info of `who`.
    fn account_of(ctx: &Context, who: &Address) -> Option<SmartAccount>;

    /// The balance of `who`.
    fn balance(ctx: &Context, who: &Address) -> u128;

    /// The nonce of `who`.
    fn nonce(ctx: &Context, who: &Address) -> u64;

    /// The account executes new transactions and increase nonce
    fn inc_nonce(ctx: &Context, who: &Address) -> Result<u64>;

    /// Transfer some balance from `sender` to `dest`
    fn transfer(
        ctx: &Context,
        sender: &Address,
        dest: &Address,
        balance: u128,
    ) -> Result<()>;

    /// Mints `value` to the free balance of `who`.
    fn mint(ctx: &Context, target: &Address, balance: u128) -> Result<()>;

    /// Burns `value` to the free balance of `who`.
    fn burn(ctx: &Context, target: &Address, balance: u128) -> Result<()>;

    /// Removes some balance from `who` account.
    fn withdraw(ctx: &Context, who: &Address, value: u128) -> Result<()>;

    /// Refund some balance from `who` account.
    fn refund(ctx: &Context, who: &Address, value: u128) -> Result<()>;
}

/// Outputs the current transaction fee.
pub trait FeeCalculator {
    fn min_fee() -> u64;
}

impl FeeCalculator for () {
    fn min_fee() -> u64 {
        0
    }
}
