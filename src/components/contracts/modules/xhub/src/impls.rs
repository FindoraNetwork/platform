use crate::storage::*;
use crate::{App, Config};
use evm::ExitReason;
use fp_core::{context::Context, ensure, transaction::ActionResult};
use fp_evm::Runner;
use fp_storage::{Borrow, BorrowMut};
use fp_traits::{account::AccountAsset, evm::DecimalsMapping};
use fp_types::{
    actions::{
        evm::Call,
        xhub::{NonConfidentialOutput, NonConfidentialTransfer},
    },
    crypto::Address,
};
use fp_utils::proposer_converter;
use ledger::data_model::ASSET_TYPE_FRA;
use log::debug;
use primitive_types::{H160, U256};
use ruc::*;

impl<C: Config> App<C> {
    #![allow(clippy::too_many_arguments)]
    pub fn erc20_to_utxo(
        ctx: &Context,
        sender: Address,
        contractaddress: H160,
        gas_price: U256,
        gas_limit: U256,
        input: Vec<u8>,
        _nonce: U256,
        outputs: Vec<NonConfidentialOutput>,
    ) -> Result<ActionResult> {
        let mut asset_amount = 0;
        for output in &outputs {
            ensure!(
                output.asset == ASSET_TYPE_FRA,
                "Invalid asset type only support FRA"
            );
            asset_amount += output.amount;
        }

        log::debug!(target: "xhub", "transfer to UTXO amount is: {} FRA", asset_amount);
        let amount = C::DecimalsMapping::from_native_token(U256::from(asset_amount))
            .ok_or_else(|| eg!("The transfer to UTXO amount is too large"))?;

        let sa = C::AccountAsset::account_of(ctx, &sender, None)
            .c(d!("account does not exist"))?;
        if sa.balance < amount {
            return Err(eg!("insufficient balance"));
        }

        if !amount.is_zero() {
            // call ERC20 contract burn method
            let mut config = C::config().clone();
            config.estimate = true;

            let v: [u8; 32] = *sender.as_ref();
            let source = proposer_converter(v.to_vec()).unwrap();
            let call = Call {
                source,
                target: contractaddress,
                input,
                value: U256::zero(),
                gas_limit: gas_limit.low_u64(),
                gas_price: Some(gas_price),
                nonce: None,
            };

            let info = C::Runner::call(ctx, call, &config).c(d!("Evm runner failed"))?;
            match info.exit_reason {
                ExitReason::Succeed(_) => {
                    // mint UTXO
                    Self::add_mint(ctx, outputs)?;
                }
                _ => return Err(eg!("Failed to execute evm tx")),
            }
        }

        Ok(ActionResult::default())
    }

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
        let ops =
            if let Some(mut ori_outputs) = PendingUTXOs::get(ctx.db.read().borrow()) {
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
