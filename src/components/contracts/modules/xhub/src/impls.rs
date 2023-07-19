use crate::storage::*;
use crate::{App, Config};
use fp_core::{context::Context, ensure, transaction::ActionResult};
use fp_storage::BorrowMut;
use fp_traits::{account::AccountAsset, evm::DecimalsMapping};
use fp_types::actions::xhub::NonConfidentialTransfer;
use fp_types::{actions::xhub::NonConfidentialOutput, crypto::Address};
use ledger::data_model::ASSET_TYPE_FRA;
use primitive_types::U256;
use ruc::*;
use tracing::debug;

impl<C: Config> App<C> {
    pub fn transfer_to_nonconfidential_utxo(
        ctx: &Context,
        sender: Address,
        call: NonConfidentialTransfer,
    ) -> Result<ActionResult> {
        let mut transfer_amount = 0;
        for output in &call.outputs {
            ensure!(
                output.asset == ASSET_TYPE_FRA,
                "Invalid asset type only support FRA"
            );
            transfer_amount += output.amount;
        }

        debug!(target: "xhub", "transfer to UTXO {} FRA", transfer_amount);

        ensure!(
            call.input_value == transfer_amount,
            "Input value mismatch utxo output"
        );

        let amount = C::DecimalsMapping::from_native_token(U256::from(transfer_amount))
            .ok_or_else(|| eg!("the transfer to UTXO amount is too large"))?;

        let sa = C::AccountAsset::account_of(ctx, &sender, None)
            .c(d!("account does not exist"))?;
        if sa.balance < amount {
            return Err(eg!("insufficient balance"));
        }

        if !amount.is_zero() {
            C::AccountAsset::burn(ctx, &sender, amount)?;
            Self::add_mint(ctx, call.outputs)?;
        }
        Ok(ActionResult::default())
    }

    pub(crate) fn add_mint(
        ctx: &Context,
        mut outputs: Vec<NonConfidentialOutput>,
    ) -> Result<()> {
        let ops = if let Some(mut ori_outputs) = PendingUTXOs::get(&ctx.db.read()) {
            ori_outputs.append(&mut outputs);
            ori_outputs
        } else {
            outputs
        };
        PendingUTXOs::put(ctx.db.write().borrow_mut(), &ops)
    }

    pub fn consume_mint(ctx: &Context) -> Option<Vec<NonConfidentialOutput>> {
        PendingUTXOs::take(ctx.db.write().borrow_mut())
    }
}
