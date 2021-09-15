use crate::storage::*;
use crate::{App, Config};
use ethereum_types::{H160, H256, U256};
use fp_core::context::Context;
use fp_evm::Account;
use fp_traits::{
    account::AccountAsset,
    evm::{AddressMapping, OnChargeEVMTransaction},
};
use fp_utils::proposer_converter;
use ruc::Result;

impl<C: Config> App<C> {
    /// Check whether an account is empty.
    pub fn is_account_empty(ctx: &Context, address: &H160) -> bool {
        let account = Self::account_basic(ctx, address);
        let code_len = AccountCodes::decode_len(ctx.store.clone(), address).unwrap_or(0);

        account.nonce == U256::zero() && account.balance == U256::zero() && code_len == 0
    }

    /// Remove an account.
    pub fn remove_account(ctx: &Context, address: &H160) {
        AccountCodes::remove(ctx.store.clone(), address);
        AccountStorages::remove_prefix(ctx.store.clone(), address);
    }

    /// Create an account.
    pub fn create_account(ctx: &Context, address: H160, code: Vec<u8>) -> Result<()> {
        if code.is_empty() {
            return Ok(());
        }

        AccountCodes::insert(ctx.store.clone(), &address, &code)
    }

    /// Get the account code
    pub fn account_codes(ctx: &Context, address: &H160) -> Option<Vec<u8>> {
        AccountCodes::get(ctx.store.clone(), address)
    }

    /// Get the account storage
    pub fn account_storages(
        ctx: &Context,
        address: &H160,
        index: &H256,
    ) -> Option<H256> {
        AccountStorages::get(ctx.store.clone(), address, index)
    }

    /// Get the account basic in EVM format.
    pub fn account_basic(ctx: &Context, address: &H160) -> Account {
        let account_id = C::AddressMapping::convert_to_account_id(*address);
        let nonce = U256::from(C::AccountAsset::nonce(ctx, &account_id));
        let balance = U256::from(C::AccountAsset::balance(ctx, &account_id));

        Account { balance, nonce }
    }

    /// Get the block proposer.
    pub fn find_proposer(ctx: &Context) -> H160 {
        // TODO
        proposer_converter(ctx.header.proposer_address.clone()).unwrap_or_default()
    }
}

/// Implements the transaction payment for a module implementing the `Currency`
/// trait (eg. the pallet_balances) using an unbalance handler (implementing
/// `OnUnbalanced`).
impl<C: Config> OnChargeEVMTransaction for App<C> {
    fn withdraw_fee(ctx: &Context, who: &H160, fee: U256) -> Result<()> {
        // TODO fee pay to block author
        let account_id = C::AddressMapping::convert_to_account_id(*who);
        C::AccountAsset::withdraw(ctx, &account_id, fee.low_u128())
    }

    fn correct_and_deposit_fee(
        ctx: &Context,
        who: &H160,
        corrected_fee: U256,
        already_withdrawn: U256,
    ) -> Result<()> {
        let account_id = C::AddressMapping::convert_to_account_id(*who);
        let refund_amount = already_withdrawn.saturating_sub(corrected_fee);
        C::AccountAsset::refund(ctx, &account_id, refund_amount.low_u128())
    }
}
