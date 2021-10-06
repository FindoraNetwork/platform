use crate::storage::*;
use crate::{App, Config};
use fp_core::{
    account::SmartAccount, context::Context, ensure, transaction::ActionResult,
};
use fp_storage::{Borrow, BorrowMut};
use fp_traits::{
    account::AccountAsset,
    evm::{DecimalsMapping, EthereumDecimalsMapping},
};
use fp_types::{actions::account::MintOutput, crypto::Address};
use ledger::data_model::ASSET_TYPE_FRA;
use primitive_types::U256;
use ruc::*;

impl<C: Config> AccountAsset<Address> for App<C> {
    fn total_issuance(ctx: &Context) -> U256 {
        TotalIssuance::get(ctx.state.read().borrow()).unwrap_or_default()
    }

    fn account_of(
        ctx: &Context,
        who: &Address,
        height: Option<u64>,
    ) -> Option<SmartAccount> {
        match height {
            Some(ver) => AccountStore::get_ver(ctx.state.read().borrow(), who, ver),
            None => AccountStore::get(ctx.state.read().borrow(), who),
        }
    }

    fn balance(ctx: &Context, who: &Address) -> U256 {
        Self::account_of(ctx, who).unwrap_or_default().balance
    }

    fn reserved_balance(ctx: &Context, who: &Address) -> U256 {
        Self::account_of(ctx, who).unwrap_or_default().reserved
    }

    fn nonce(ctx: &Context, who: &Address) -> U256 {
        Self::account_of(ctx, who).unwrap_or_default().nonce
    }

    fn inc_nonce(ctx: &Context, who: &Address) -> Result<U256> {
        let mut sa = Self::account_of(ctx, who).c(d!("account does not exist"))?;
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
            Self::account_of(ctx, sender).c(d!("sender does not exist"))?;
        let mut to_account = Self::account_of(ctx, dest).unwrap_or_default();

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

        let mut target_account = Self::account_of(ctx, target).unwrap_or_default();
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

        let mut target_account = Self::account_of(ctx, target)
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

        let mut sa = Self::account_of(ctx, who).c(d!("account does not exist"))?;
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

        let mut sa = Self::account_of(ctx, who).c(d!("account does not exist"))?;
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
        if amount.is_zero() {
            return Ok(());
        }

        Allowances::insert(ctx.state.write().borrow_mut(), owner, spender, &amount)
    }
}

impl<C: Config> App<C> {
    pub fn transfer_to_utxo(
        ctx: &Context,
        sender: Address,
        outputs: Vec<MintOutput>,
    ) -> Result<ActionResult> {
        let mut asset_amount = 0;
        for output in &outputs {
            ensure!(
                output.asset == ASSET_TYPE_FRA,
                "Invalid asset type only support FRA"
            );
            asset_amount += output.amount;
        }

        log::debug!(target: "account", "transfer to UTXO amount is: {} FRA", asset_amount);

        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(asset_amount))
                .ok_or_else(|| eg!("The transfer to UTXO amount is too large"))?;

        let sa = Self::account_of(ctx, &sender, None).c(d!("account does not exist"))?;
        if sa.balance < amount {
            return Err(eg!("insufficient balance"));
        }

        if !amount.is_zero() {
            Self::burn(ctx, &sender, amount)?;
            Self::add_mint(ctx, outputs)?;
        }
        Ok(ActionResult::default())
    }

    pub(crate) fn add_mint(ctx: &Context, mut outputs: Vec<MintOutput>) -> Result<()> {
        let ops = if let Some(mut ori_outputs) = MintOutputs::get(ctx.db.read().borrow())
        {
            ori_outputs.append(&mut outputs);
            ori_outputs
        } else {
            outputs
        };
        MintOutputs::put(ctx.db.write().borrow_mut(), &ops)
    }

    pub fn consume_mint(ctx: &Context, size: usize) -> Result<Vec<MintOutput>> {
        let mut outputs = MintOutputs::get(ctx.db.read().borrow()).unwrap_or_default();
        if outputs.len() > size {
            let vec2 = outputs.split_off(size);
            MintOutputs::put(ctx.db.write().borrow_mut(), &vec2)?;
        } else {
            MintOutputs::delete(ctx.db.write().borrow_mut());
        }
        Ok(outputs)
    }
}
