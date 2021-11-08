use std::collections::HashMap;
use std::convert::TryFrom;
use std::str::FromStr;

use super::*;
use crate::BaseApp;
use abci::*;
use evm::ExitReason;
use fp_core::{
    context::Context,
    module::AppModule,
    transaction::{
        ActionResult, Applyable, Executable, SignedExtension, ValidateUnsigned,
    },
};
use fp_evm::Runner;
use fp_traits::evm::{DecimalsMapping, FeeCalculator};
use fp_types::{
    actions::{self, evm::Call},
    assemble::{convert_unsigned_transaction, CheckedTransaction, UncheckedTransaction},
    crypto::{Address, MultiSigner},
};
use fp_utils::proposer_converter;
use ledger::{
    converter::{check_convert_account, erc20::check_erc20_tx, ConvertingType},
    data_model::{Operation, Transaction as FindoraTransaction},
};
use ruc::*;
use serde::Serialize;

#[derive(Default)]
pub struct ModuleManager {
    // Ordered module list
    pub(crate) account_module: module_account::App<BaseApp>,
    pub(crate) ethereum_module: module_ethereum::App<BaseApp>,
    pub(crate) evm_module: module_evm::App<BaseApp>,
    pub(crate) xhub_module: module_xhub::App<BaseApp>,
    pub(crate) template_module: module_template::App<BaseApp>,
}

impl ModuleManager {
    pub fn query(
        &self,
        ctx: Context,
        mut path: Vec<&str>,
        req: &RequestQuery,
    ) -> ResponseQuery {
        let mut resp: ResponseQuery = Default::default();
        if path.is_empty() {
            resp.code = 1;
            resp.log = "Invalid custom query path without module route!".to_string();
            return resp;
        }

        // Note: adding new modules may need to be updated.
        let module_name = path.remove(0);
        if module_name == module_account::MODULE_NAME {
            self.account_module.query_route(ctx, path, req)
        } else if module_name == module_ethereum::MODULE_NAME {
            self.ethereum_module.query_route(ctx, path, req)
        } else if module_name == module_evm::MODULE_NAME {
            self.evm_module.query_route(ctx, path, req)
        } else if module_name == module_xhub::MODULE_NAME {
            self.xhub_module.query_route(ctx, path, req)
        } else if module_name == module_template::MODULE_NAME {
            self.template_module.query_route(ctx, path, req)
        } else {
            resp.code = 1;
            resp.log = format!("Invalid query module route: {}!", module_name);
            resp
        }
    }

    pub fn begin_block(&mut self, ctx: &mut Context, req: &RequestBeginBlock) {
        // Note: adding new modules need to be updated.
        self.account_module.begin_block(ctx, req);
        self.ethereum_module.begin_block(ctx, req);
        self.evm_module.begin_block(ctx, req);
        self.xhub_module.begin_block(ctx, req);
        self.template_module.begin_block(ctx, req);
    }

    pub fn end_block(
        &mut self,
        ctx: &mut Context,
        req: &RequestEndBlock,
    ) -> ResponseEndBlock {
        let mut resp: ResponseEndBlock = Default::default();
        // Note: adding new modules need to be updated.
        self.account_module.end_block(ctx, req);
        self.ethereum_module.end_block(ctx, req);
        self.evm_module.end_block(ctx, req);
        self.xhub_module.end_block(ctx, req);
        let resp_template = self.template_module.end_block(ctx, req);
        if !resp_template.validator_updates.is_empty() {
            resp.validator_updates = resp_template.validator_updates;
        }
        resp
    }

    pub fn process_tx<
        Extra: Clone + Serialize + SignedExtension<AccountId = Address>,
    >(
        &mut self,
        ctx: Context,
        tx: UncheckedTransaction<Extra>,
    ) -> Result<ActionResult> {
        let checked = tx.clone().check()?;
        match tx.function.clone() {
            actions::Action::Ethereum(action) => Self::dispatch::<
                actions::ethereum::Action,
                module_ethereum::App<BaseApp>,
                Extra,
            >(&ctx, action, checked),
            _ => Self::dispatch::<actions::Action, BaseApp, Extra>(
                &ctx,
                tx.function,
                checked,
            ),
        }
    }

    pub fn process_findora_tx(
        &mut self,
        ctx: &Context,
        tx: &FindoraTransaction,
        tx_type: ConvertingType,
    ) -> Result<()> {
        match tx_type {
            ConvertingType::ConvertAccount => {
                if let Ok((owner, amount)) = check_convert_account(tx) {
                    let balance =
                        EthereumDecimalsMapping::from_native_token(U256::from(amount))
                            .ok_or_else(|| {
                            eg!("The transfer to account amount is too large")
                        })?;
                    module_account::App::<BaseApp>::mint(
                        ctx,
                        &Address::from(owner),
                        balance,
                    )
                } else {
                    Err(eg!("Invalid check_convert_account"))
                }
            }
            ConvertingType::ERC20 => self.process_findora_erc20(ctx, tx),
            ConvertingType::FindoraAsset => self.process_findora_asset(ctx, tx),
            ConvertingType::FRC20 => {
                todo!()
            }
        }
    }

    fn process_findora_asset(
        &mut self,
        ctx: &Context,
        tx: &FindoraTransaction,
    ) -> Result<()> {
        let mut new_assets = HashMap::new();
        let mut issue_assets = HashMap::new();
        let mut update_memos = HashMap::new();

        for op in &tx.body.operations {
            match op {
                Operation::DefineAsset(da) => {
                    let code = da.body.asset.code;
                    if new_assets.contains_key(&code)
                        || issue_assets.contains_key(&code)
                        || update_memos.contains_key(&code)
                    {
                        return Err(eg!("duplicate asset type code"));
                    }
                    new_assets.insert(code, *da.body.asset.clone());
                }
                Operation::IssueAsset(ia) => {
                    let code = ia.body.code;
                    if issue_assets.contains_key(&code) {
                        return Err(eg!("duplicate asset type code when issuing"));
                    }
                    // TODO: calculate issuance amount
                    issue_assets.insert(code, 0);
                }
                Operation::TransferAsset(_ta) => {
                    todo!()
                }
                Operation::UpdateMemo(um) => {
                    let code = um.body.asset_type;
                    let memo = um.body.new_memo.clone();
                    if update_memos.contains_key(&code) {
                        return Err(eg!("duplicate asset type code when updating memo"));
                    }
                    update_memos.insert(code, memo);
                }
                _ => {
                    log::debug!(target: "baseapp", "Unsupported findora asset operation");
                }
            }
        }

        for da in new_assets {
            if let Ok(MultiSigner::Ethereum(address)) =
                MultiSigner::from_str(da.1.memo.0.as_str())
            {
                if module_xhub::App::<BaseApp>::asset_of(ctx, &address).is_some() {
                    return Err(eg!("Existed findora asset"));
                }
                return module_xhub::App::<BaseApp>::add_asset(ctx, &address, &da.1)
                    .c(d!("Failed to add new asset"));
            } else {
                log::info!(target: "baseapp", "Skipping invalid new asset");
            };
        }

        // TODO: handle other asset operations

        Ok(())
    }

    // findora utxo -> erc20
    fn process_findora_erc20(
        &self,
        ctx: &Context,
        tx: &FindoraTransaction,
    ) -> Result<()> {
        let (signer, owner, _nonce, input) = check_erc20_tx(tx)?;
        let signer = proposer_converter(signer).unwrap();
        let target = H160::try_from(owner).unwrap();

        // call mint method
        let mut config = <BaseApp as module_ethereum::Config>::config().clone();
        config.estimate = true;

        let account = module_evm::App::<BaseApp>::account_basic(ctx, &target);
        let call = Call {
            source: signer,
            target,
            input,
            value: U256::zero(),
            gas_limit: <BaseApp as module_evm::Config>::BlockGasLimit::get().as_u64(),
            gas_price: Some(
                <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price(),
            ),
            nonce: Some(account.nonce),
        };

        let info =
            <BaseApp as module_ethereum::Config>::Runner::call(ctx, call, &config)
                .c(d!("Evm runner failed!"))?;
        match info.exit_reason {
            ExitReason::Succeed(_) => Ok(()),
            _ => Err(eg!("Failed to execute evm transaction")),
        }
    }
}

impl ModuleManager {
    fn dispatch<Call, Module, Extra>(
        ctx: &Context,
        action: Call,
        tx: CheckedTransaction<Extra>,
    ) -> Result<ActionResult>
    where
        Module: ValidateUnsigned<Call = Call>,
        Module: Executable<Origin = Address, Call = Call>,
        Extra: SignedExtension<AccountId = Address> + Clone,
    {
        let origin_tx = convert_unsigned_transaction::<Call, Extra>(action, tx);

        origin_tx.validate::<Module>(ctx)?;

        if RunTxMode::Deliver == ctx.run_mode {
            return origin_tx.apply::<Module>(ctx);
        }
        Ok(ActionResult::default())
    }
}
