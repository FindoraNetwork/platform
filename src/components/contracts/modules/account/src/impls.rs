use crate::storage::*;
use crate::{App, Config};
use config::abci::global_cfg::CFG;
use fp_core::{account::SmartAccount, context::Context};
use fp_storage::{Borrow, BorrowMut};
use fp_traits::account::AccountAsset;
use fp_types::crypto::Address;
use primitive_types::{H160, U256};
use ruc::*;
use zei::xfr::sig::XfrPublicKey;

impl<C: Config> AccountAsset<Address> for App<C> {
    fn total_issuance(ctx: &Context) -> U256 {
        TotalIssuance::get(ctx.state.read().borrow()).unwrap_or_default()
    }

    fn account_of(
        ctx: &Context,
        who: &Address,
        height: Option<u64>,
    ) -> Option<SmartAccount> {
        let version = height.unwrap_or(0);
        if version == 0 {
            AccountStore::get(ctx.state.read().borrow(), who)
        } else {
            AccountStore::get_ver(ctx.state.read().borrow(), who, version)
        }
    }

    fn balance(ctx: &Context, who: &Address) -> U256 {
        Self::account_of(ctx, who, None).unwrap_or_default().balance
    }

    fn reserved_balance(ctx: &Context, who: &Address) -> U256 {
        Self::account_of(ctx, who, None)
            .unwrap_or_default()
            .reserved
    }

    fn nonce(ctx: &Context, who: &Address) -> U256 {
        Self::account_of(ctx, who, None).unwrap_or_default().nonce
    }

    fn inc_nonce(ctx: &Context, who: &Address) -> Result<U256> {
        let mut sa = if ctx.header.height as u64 >= CFG.checkpoint.nonce_bug_fix_height {
            Self::account_of(ctx, who, None).unwrap_or_default()
        } else {
            Self::account_of(ctx, who, None).c(d!("account does not exist"))?
        };

        sa.nonce = sa.nonce.saturating_add(U256::one());
        AccountStore::insert(ctx.state.write().borrow_mut(), who, &sa).map(|()| sa.nonce)
    }

    fn transfer(
        ctx: &Context,
        sender: &Address,
        dest: &Address,
        balance: U256,
    ) -> Result<()> {
        if balance.is_zero() || sender == dest {
            return Ok(());
        }

        let mut from_account =
            Self::account_of(ctx, sender, None).c(d!("sender does not exist"))?;
        let mut to_account = Self::account_of(ctx, dest, None).unwrap_or_default();

        from_account.balance = from_account
            .balance
            .checked_sub(balance)
            .c(d!("insufficient balance"))?;
        to_account.balance = to_account
            .balance
            .checked_add(balance)
            .c(d!("balance overflow"))?;
        AccountStore::insert(ctx.state.write().borrow_mut(), sender, &from_account)?;
        AccountStore::insert(ctx.state.write().borrow_mut(), dest, &to_account)
    }

    fn mint(ctx: &Context, target: &Address, balance: U256) -> Result<()> {
        if balance.is_zero() {
            return Ok(());
        }

        let mut target_account = Self::account_of(ctx, target, None).unwrap_or_default();
        target_account.balance = target_account
            .balance
            .checked_add(balance)
            .c(d!("balance overflow"))?;

        AccountStore::insert(ctx.state.write().borrow_mut(), target, &target_account)?;

        let issuance = Self::total_issuance(ctx)
            .checked_add(balance)
            .c(d!("issuance overflow"))?;
        TotalIssuance::put(ctx.state.write().borrow_mut(), &issuance)
    }

    fn burn(ctx: &Context, target: &Address, balance: U256) -> Result<()> {
        if balance.is_zero() {
            return Ok(());
        }

        let mut target_account = Self::account_of(ctx, target, None)
            .c(d!(format!("account: {} does not exist", target)))?;
        target_account.balance = target_account
            .balance
            .checked_sub(balance)
            .c(d!("insufficient balance"))?;

        AccountStore::insert(ctx.state.write().borrow_mut(), target, &target_account)?;

        let issuance = Self::total_issuance(ctx)
            .checked_sub(balance)
            .c(d!("insufficient issuance"))?;
        TotalIssuance::put(ctx.state.write().borrow_mut(), &issuance)
    }

    fn withdraw(ctx: &Context, who: &Address, value: U256) -> Result<()> {
        if value.is_zero() {
            return Ok(());
        }

        let mut sa = Self::account_of(ctx, who, None).c(d!("account does not exist"))?;
        sa.balance = sa
            .balance
            .checked_sub(value)
            .c(d!("insufficient balance"))?;
        sa.reserved = sa
            .reserved
            .checked_add(value)
            .c(d!("reserved balance overflow"))?;

        AccountStore::insert(ctx.state.write().borrow_mut(), who, &sa)
    }

    fn refund(ctx: &Context, who: &Address, value: U256) -> Result<()> {
        if value.is_zero() {
            return Ok(());
        }

        let mut sa = Self::account_of(ctx, who, None).c(d!("account does not exist"))?;
        sa.reserved = sa
            .reserved
            .checked_sub(value)
            .c(d!("insufficient reserved balance"))?;
        sa.balance = sa.balance.checked_add(value).c(d!("balance overflow"))?;
        AccountStore::insert(ctx.state.write().borrow_mut(), who, &sa)
    }

    fn allowance(ctx: &Context, owner: &Address, spender: &Address) -> U256 {
        Allowances::get(ctx.state.read().borrow(), owner, spender).unwrap_or_default()
    }

    fn approve(
        ctx: &Context,
        owner: &Address,
        spender: &Address,
        amount: U256,
    ) -> Result<()> {
        Allowances::insert(ctx.state.write().borrow_mut(), owner, spender, &amount)
    }
}

/// impl outside of trait
impl<C: Config> App<C> {
    pub fn insert_evm_fra_address_mapping(
        ctx: &Context,
        fra_pk: &XfrPublicKey,
        evm_address: &H160,
    ) -> Result<()> {
        EvmFraAddressMapping::insert(ctx.state.write().borrow_mut(), evm_address, fra_pk)
    }
}
