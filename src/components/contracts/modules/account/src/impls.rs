use crate::storage::*;
use crate::{App, Config};
use fp_core::{
    account::SmartAccount, context::Context, ensure, transaction::ActionResult,
};
use fp_traits::account::AccountAsset;
use fp_types::{actions::account::MintOutput, crypto::Address};
use ledger::data_model::ASSET_TYPE_FRA;
use ruc::*;

impl<C: Config> AccountAsset<Address> for App<C> {
    fn account_of(ctx: &Context, who: &Address) -> Option<SmartAccount> {
        AccountStore::get(ctx.store.clone(), who)
    }

    fn balance(ctx: &Context, who: &Address) -> u128 {
        let who_account: SmartAccount =
            AccountStore::get(ctx.store.clone(), who).unwrap_or_default();
        who_account.balance
    }

    fn nonce(ctx: &Context, who: &Address) -> u64 {
        let who_account: SmartAccount =
            AccountStore::get(ctx.store.clone(), who).unwrap_or_default();
        who_account.nonce
    }

    fn inc_nonce(ctx: &Context, who: &Address) -> Result<u64> {
        let mut sa: SmartAccount =
            AccountStore::get(ctx.store.clone(), who).c(d!("account does not exist"))?;
        sa.nonce = sa.nonce.checked_add(1).c(d!("balance overflow"))?;
        AccountStore::insert(ctx.store.clone(), who, &sa).map(|()| sa.nonce)
    }

    fn transfer(
        ctx: &Context,
        sender: &Address,
        dest: &Address,
        balance: u128,
    ) -> Result<()> {
        if balance == 0 || sender == dest {
            return Ok(());
        }
        let mut from_account: SmartAccount =
            AccountStore::get(ctx.store.clone(), sender)
                .c(d!("sender does not exist"))?;

        let mut to_account: SmartAccount =
            AccountStore::get(ctx.store.clone(), dest).unwrap_or_default();
        from_account.balance = from_account
            .balance
            .checked_sub(balance)
            .c(d!("insufficient balance"))?;
        to_account.balance = to_account
            .balance
            .checked_add(balance)
            .c(d!("balance overflow"))?;
        AccountStore::insert(ctx.store.clone(), sender, &from_account)?;
        AccountStore::insert(ctx.store.clone(), dest, &to_account)
    }

    fn mint(ctx: &Context, target: &Address, balance: u128) -> Result<()> {
        let mut target_account: SmartAccount =
            AccountStore::get(ctx.store.clone(), target).unwrap_or_default();
        target_account.balance = target_account.balance.checked_add(balance).c(d!())?;

        AccountStore::insert(ctx.store.clone(), target, &target_account)
    }

    fn burn(ctx: &Context, target: &Address, balance: u128) -> Result<()> {
        let mut target_account: SmartAccount =
            Self::account_of(ctx, target).c(d!("account does not exist"))?;
        target_account.balance = target_account
            .balance
            .checked_sub(balance)
            .c(d!("insufficient balance"))?;

        AccountStore::insert(ctx.store.clone(), target, &target_account)
    }

    fn withdraw(ctx: &Context, who: &Address, value: u128) -> Result<()> {
        let mut sa: SmartAccount =
            AccountStore::get(ctx.store.clone(), who).c(d!("account does not exist"))?;
        sa.balance = sa
            .balance
            .checked_sub(value)
            .c(d!("insufficient balance"))?;
        AccountStore::insert(ctx.store.clone(), who, &sa)
    }

    fn refund(ctx: &Context, who: &Address, value: u128) -> Result<()> {
        let mut sa: SmartAccount =
            AccountStore::get(ctx.store.clone(), who).c(d!("account does not exist"))?;
        sa.balance = sa.balance.checked_add(value).c(d!("balance overflow"))?;
        AccountStore::insert(ctx.store.clone(), who, &sa)
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

        log::debug!(target: "account", "this tx's amount is: FRA: {}", asset_amount);

        let sa = Self::account_of(ctx, &sender).c(d!("account does not exist"))?;
        if sa.balance < asset_amount as u128 {
            return Err(eg!("insufficient balance"));
        }

        if asset_amount > 0 {
            Self::burn(ctx, &sender, asset_amount as u128)?;
            Self::add_mint(ctx, outputs)?;
        }
        Ok(ActionResult::default())
    }

    pub(crate) fn add_mint(ctx: &Context, mut outputs: Vec<MintOutput>) -> Result<()> {
        let ops = if let Some(mut ori_outputs) = MintOutputs::get(ctx.store.clone()) {
            ori_outputs.append(&mut outputs);
            ori_outputs
        } else {
            outputs
        };
        MintOutputs::put(ctx.store.clone(), &ops)
    }

    pub fn consume_mint(ctx: &Context, size: usize) -> Result<Vec<MintOutput>> {
        let mut outputs = MintOutputs::get(ctx.store.clone()).unwrap_or_default();
        if outputs.len() > size {
            let vec2 = outputs.split_off(size);
            MintOutputs::put(ctx.store.clone(), &vec2)?;
        } else {
            MintOutputs::delete(ctx.store.clone());
        }
        Ok(outputs)
    }
}
