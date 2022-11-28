use super::*;
use abci::*;
use config::abci::global_cfg::CFG;
use fp_core::{
    context::Context,
    module::AppModule,
    transaction::{
        ActionResult, Applyable, Executable, SignedExtension, ValidateUnsigned,
    },
};
use fp_traits::evm::DecimalsMapping;
use fp_types::{
    actions,
    assemble::{convert_unsigned_transaction, CheckedTransaction, UncheckedTransaction},
    crypto::{Address, HA256},
};
use ledger::{
    converter::check_convert_account,
    data_model::{Transaction as FindoraTransaction, ASSET_TYPE_FRA},
};
use module_ethereum::storage::{TransactionIndex, DELIVER_PENDING_TRANSACTIONS};
use ruc::*;
use serde::Serialize;

#[derive(Default, Clone)]
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
        let mresp = self.evm_module.end_block(ctx, req);

        if !mresp.validator_updates.is_empty() {
            resp.validator_updates = mresp.validator_updates;
        }

        self.xhub_module.end_block(ctx, req);
        self.template_module.end_block(ctx, req);
        // if !resp_template.validator_updates.is_empty() {
        // resp.validator_updates = resp_template.validator_updates;
        // }
        resp
    }

    pub fn commit(
        &mut self,
        ctx: &mut Context,
        height: u64,
        root_hash: &[u8],
    ) -> Result<()> {
        self.ethereum_module
            .commit(ctx, U256::from(height), root_hash)
    }

    pub fn process_tx<
        Extra: Clone + Serialize + SignedExtension<AccountId = Address>,
    >(
        &self,
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
        hash: H256,
    ) -> Result<()> {
        let (from, owner, amount, asset, lowlevel) =
            check_convert_account(tx, ctx.header.height)?;

        if CFG.checkpoint.prismxx_inital_height < ctx.header.height {
            let mut pending_txs = DELIVER_PENDING_TRANSACTIONS.lock().c(d!())?;
            // if let Some(pending_txs)
            // let transaction_index = pending_txs.as_ref().unwrap_or_default().len() as u32;
            let transaction_index = pending_txs.len() as u32;

            let (tx, tx_status, receipt) = if asset == ASSET_TYPE_FRA {
                let balance =
                    EthereumDecimalsMapping::from_native_token(U256::from(amount))
                        .ok_or_else(|| {
                            eg!("The transfer to account amount is too large")
                        })?;
                let bridge_address = self.evm_module.contracts.bridge_address;
                let ba = Address::from(bridge_address);

                module_account::App::<BaseApp>::mint(ctx, &ba, balance)?;
                match self.evm_module.withdraw_fra(
                    ctx,
                    &from,
                    &owner,
                    balance,
                    lowlevel,
                    transaction_index,
                    hash,
                ) {
                    Ok(r) => r,
                    Err(e) => {
                        // Revert mint in failed.
                        ctx.state.write().discard_session();
                        return Err(e);
                    }
                }
            } else {
                self.evm_module.withdraw_frc20(
                    ctx,
                    asset.0,
                    &from,
                    &owner,
                    U256::from(amount),
                    lowlevel,
                    transaction_index,
                    hash,
                )?
            };

            TransactionIndex::insert(
                &mut *ctx.db.write(),
                &HA256::new(tx_status.transaction_hash),
                &(U256::from(ctx.header.height), tx_status.transaction_index),
            )?;

            pending_txs.push((tx, tx_status, receipt));

            Ok(())
        } else {
            let balance = EthereumDecimalsMapping::from_native_token(U256::from(amount))
                .ok_or_else(|| eg!("The transfer to account amount is too large"))?;
            module_account::App::<BaseApp>::mint(ctx, &Address::from(owner), balance)
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
