///! Transaction signature extension for transaction verification and validity check.
use crate::BaseApp;
use config::abci::global_cfg::CFG;
use fp_core::{
    context::Context,
    transaction::{ActionResult, SignedExtension},
};
use fp_traits::account::{AccountAsset, FeeCalculator};
use fp_types::crypto::Address;
use primitive_types::U256;
use ruc::*;
use serde::{Deserialize, Serialize};

pub type SignedExtra = (CheckNonce, CheckFee);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CheckNonce(U256);

impl CheckNonce {
    pub fn new(nonce: U256) -> Self {
        CheckNonce(nonce)
    }
}

impl SignedExtension for CheckNonce {
    type AccountId = Address;
    type Pre = ();

    fn validate(&self, ctx: &Context, who: &Self::AccountId) -> Result<()> {
        let nonce = module_account::App::<BaseApp>::nonce(ctx, who);
        if self.0 < nonce {
            Err(eg!(format!(
                "InvalidNonce, expected: {}, actual: {}",
                nonce, self.0
            )))
        } else {
            Ok(())
        }
    }

    fn pre_execute(self, ctx: &Context, who: &Self::AccountId) -> Result<Self::Pre> {
        let nonce = module_account::App::<BaseApp>::nonce(ctx, who);
        if self.0 != nonce {
            return Err(eg!(format!(
                "InvalidNonce, expected: {}, actual: {}",
                nonce, self.0
            )));
        }
        module_account::App::<BaseApp>::inc_nonce(ctx, who)?;
        Ok(())
    }

    fn post_execute(
        ctx: &Context,
        _pre: Self::Pre,
        result: &ActionResult,
    ) -> Result<()> {
        if ctx.header.height >= CFG.checkpoint.evm_checktx_nonce && result.code != 0 {
            let prev = result
                .source
                .as_ref()
                .map(|who| module_account::App::<BaseApp>::nonce(ctx, who))
                .unwrap_or_default();

            ctx.state.write().discard_session();

            let current = result
                .source
                .as_ref()
                .map(|who| module_account::App::<BaseApp>::nonce(ctx, who))
                .unwrap_or_default();

            if prev.gt(&current) {
                if let Some(who) = result.source.as_ref() {
                    log::debug!(
                        target:
                        "baseapp",
                        "In post_execute: source {} {} {}",
                        who,
                        prev,
                        current
                    );
                    // Keep it same with `pre_execute`
                    let _ = module_account::App::<BaseApp>::inc_nonce(ctx, who);
                } else {
                    unreachable!();
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CheckFee(Option<U256>);

impl CheckFee {
    pub fn new(fee: Option<U256>) -> Self {
        CheckFee(fee)
    }
}

impl SignedExtension for CheckFee {
    type AccountId = Address;
    type Pre = (Address, U256);

    fn validate(&self, ctx: &Context, who: &Self::AccountId) -> Result<()> {
        let min_fee = <BaseApp as module_account::Config>::FeeCalculator::min_fee();
        let tx_fee = match self.0 {
            None => min_fee,
            Some(fee) => {
                if fee < min_fee {
                    return Err(eg!("The transaction fee is too low."));
                }
                fee
            }
        };

        // check tx fee
        let amount = module_account::App::<BaseApp>::balance(ctx, who);
        if amount < tx_fee {
            return Err(eg!("Insufficient balance for transaction fee."));
        }

        Ok(())
    }

    fn pre_execute(self, ctx: &Context, who: &Self::AccountId) -> Result<Self::Pre> {
        let min_fee = <BaseApp as module_account::Config>::FeeCalculator::min_fee();
        let tx_fee = match self.0 {
            None => min_fee,
            Some(fee) => {
                if fee < min_fee {
                    return Err(eg!("The transaction fee is too low."));
                }
                fee
            }
        };
        module_account::App::<BaseApp>::burn(ctx, who, tx_fee)?;
        Ok((who.clone(), tx_fee))
    }

    fn post_execute(
        _ctx: &Context,
        _pre: Self::Pre,
        _result: &ActionResult,
    ) -> Result<()> {
        // TODO
        // let (who, tx_fee) = pre;
        // let refund_fee = tx_fee.saturating_sub(result.gas_used);
        // module_account::App::<BaseApp>::refund(ctx, &who, refund_fee as u128)?;
        Ok(())
    }
}
