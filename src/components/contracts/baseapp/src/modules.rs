use std::convert::TryFrom;

use super::*;
use crate::BaseApp;
use abci::*;
use fp_core::{
    context::Context,
    module::AppModule,
    transaction::{
        ActionResult, Applyable, Executable, SignedExtension, ValidateUnsigned,
    },
};
use fp_evm::Runner;
use fp_traits::evm::{DecimalsMapping, FeeCalculator};
use fp_types::actions::evm::Call;
use fp_types::{
    actions,
    assemble::{convert_unsigned_transaction, CheckedTransaction, UncheckedTransaction},
    crypto::Address,
};
use fp_utils::proposer_converter;
use ledger::converter::erc20::check_erc20_tx;
use ledger::{
    converter::check_convert_account, data_model::Transaction as FindoraTransaction,
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
    ) -> Result<()> {
        let (owner, amount) = check_convert_account(tx)?;
        let balance = EthereumDecimalsMapping::from_native_token(U256::from(amount))
            .ok_or_else(|| eg!("The transfer to account amount is too large"))?;
        module_account::App::<BaseApp>::mint(ctx, &Address::from(owner), balance)
    }

    /// process findora tx -> erc20
    pub fn process_findora_erc20(
        &mut self,
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

        let res =
            <BaseApp as module_ethereum::Config>::Runner::call(ctx, call, &config)?;

        log::debug!("erc20: {:?}", res);

        Ok(())
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
