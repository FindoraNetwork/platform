use crate::storage::*;
use crate::{App, Config};
use ethereum_types::{H160, H256, U256};
use fp_core::context::Context;
use fp_evm::Account;
use fp_storage::{Borrow, BorrowMut};
use fp_traits::{
    account::AccountAsset,
    evm::{AddressMapping, OnChargeEVMTransaction},
};
use fp_types::crypto::{HA160, HA256};
use fp_utils::proposer_converter;
use ruc::Result;

impl<C: Config> App<C> {
    /// Check whether an account is empty.
    pub fn is_account_empty(ctx: &Context, address: &HA160) -> bool {
        let account = Self::account_basic(ctx, address);
        let code_len =
            AccountCodes::decode_len(ctx.state.read().borrow(), address).unwrap_or(0);

        account.nonce == U256::zero() && account.balance == U256::zero() && code_len == 0
    }

    /// Remove an account.
    pub fn remove_account(ctx: &Context, address: &HA160) {
        AccountCodes::remove(ctx.state.write().borrow_mut(), address);
        AccountStorages::remove_prefix(ctx.state.write().borrow_mut(), address);
    }

    /// Create an account.
    pub fn create_account(ctx: &Context, address: HA160, code: Vec<u8>) -> Result<()> {
        if code.is_empty() {
            return Ok(());
        }
        AccountCodes::insert_bytes(ctx.state.write().borrow_mut(), &address, code)
    }

    /// Get the account code
    pub fn account_codes(
        ctx: &Context,
        address: &HA160,
        height: Option<u64>,
    ) -> Option<Vec<u8>> {
        if address.0 == H160::from_low_u64_be(0x1000) {
            return Some(b"fra".to_vec());
        }

        let version = height.unwrap_or(0);
        if version == 0 {
            AccountCodes::get_bytes(ctx.state.read().borrow(), address)
        } else {
            AccountCodes::get_ver_bytes(ctx.state.read().borrow(), address, version)
        }
    }

    /// Get the account storage
    pub fn account_storages(
        ctx: &Context,
        address: &HA160,
        index: &HA256,
        height: Option<u64>,
    ) -> Option<H256> {
        let version = height.unwrap_or(0);
        if version == 0 {
            AccountStorages::get(ctx.state.read().borrow(), address, index)
        } else {
            AccountStorages::get_ver(ctx.state.read().borrow(), address, index, version)
        }
    }

    /// Get the account basic in EVM format.
    pub fn account_basic(ctx: &Context, address: &H160) -> Account {
        let account_id = C::AddressMapping::convert_to_account_id(*address);
        let account =
            C::AccountAsset::account_of(ctx, &account_id, None).unwrap_or_default();

        Account {
            balance: account.balance,
            nonce: account.nonce,
        }
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
        C::AccountAsset::withdraw(ctx, &account_id, fee)
    }

    fn correct_and_deposit_fee(
        ctx: &Context,
        who: &H160,
        corrected_fee: U256,
        already_withdrawn: U256,
    ) -> Result<()> {
        let account_id = C::AddressMapping::convert_to_account_id(*who);
        C::AccountAsset::refund(ctx, &account_id, already_withdrawn)?;
        C::AccountAsset::burn(ctx, &account_id, corrected_fee)
    }
}
