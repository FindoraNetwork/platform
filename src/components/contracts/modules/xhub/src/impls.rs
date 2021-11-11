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
use ledger::data_model::{AssetType as FindoraAsset, ASSET_TYPE_FRA};
use log::debug;
use primitive_types::{H160, U256};
use ruc::*;

impl<C: Config> App<C> {
    #![allow(clippy::too_many_arguments)]
    pub fn erc20_to_utxo(
        ctx: &Context,
        sender: Address,
        contract: H160,
        gas_price: U256,
        gas_limit: U256,
        input: Vec<u8>,
        _nonce: U256,
        outputs: Vec<NonConfidentialOutput>,
    ) -> Result<ActionResult> {
        let mut asset_amount = 0;
        let mut asset = None;
        for output in &outputs {
            ensure!(
                output.asset != ASSET_TYPE_FRA,
                "Only Findora custom asset type is supported"
            );
            if asset.is_none() {
                asset = Some(output.asset)
            } else if asset.unwrap() != output.asset {
                return Err(eg!(
                    "Only one kind of findora custom asset type is supported"
                ));
            }
            asset_amount += output.amount;
        }

        if asset.is_none() || asset_amount == 0 {
            return Err(eg!("Invalid utxo output"));
        }

        ensure!(
            Self::asset_of(ctx, &contract).map(|x| x.properties.code.val) == asset,
            "Not binding asset"
        );

        log::debug!(target: "xhub", "transfer to UTXO amount is: {}", asset_amount);
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

            // check burn input
            let burn = ledger::converter::erc20::ERC20_CONSTRUCTOR
                .abi
                .function("burn")
                .c(d!("No burn function"))?
                .encode_input(&[
                    ethabi::Token::Address(contract),
                    ethabi::Token::Uint(U256::from(asset_amount)),
                ])
                .c(d!("Failed to encode burn input"))?;

            ensure!(burn == input, "Not a valid burn input");

            let v: [u8; 32] = *sender.as_ref();
            let source = proposer_converter(v.to_vec()).unwrap();
            let call = Call {
                source,
                target: contract,
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

    pub fn add_asset(ctx: &Context, address: &H160, asset: &FindoraAsset) -> Result<()> {
        FindoraAssets::insert(ctx.db.write().borrow_mut(), address, asset)
    }

    //pub fn remove_asset(ctx: &Context, address: &H160) {
    //    FindoraAssets::remove(ctx.db.write().borrow_mut(), address)
    //}

    pub fn asset_of(ctx: &Context, address: &H160) -> Option<FindoraAsset> {
        FindoraAssets::get(ctx.db.read().borrow(), address)
    }
}
