//! BaseApp is an ABCI application, this instance manages state storage and all module functions,
//! and provides a state query interface to the outside.

#![deny(warnings)]
#![allow(missing_docs)]
#![allow(clippy::field_reassign_with_default)]

mod app;
pub mod extensions;
mod modules;
mod notify;
pub mod tm_events;

use crate::modules::ModuleManager;
use abci::Header;
use ethereum::BlockV0 as Block;
use evm_precompile::{self, FindoraPrecompiles};
use fin_db::{FinDB, RocksDB};
use fp_core::context::Context as Context2;
use fp_core::{
    account::SmartAccount,
    context::{Context, RunTxMode},
    ensure, parameter_types,
    transaction::{ActionResult, Executable, ValidateUnsigned},
};
use fp_evm::BlockId;
use fp_traits::{
    account::{AccountAsset, FeeCalculator},
    base::BaseProvider,
    evm::{EthereumAddressMapping, EthereumDecimalsMapping},
};
use fp_types::{actions::Action, crypto::Address};
use lazy_static::lazy_static;
use ledger::data_model::Transaction as FindoraTransaction;
use notify::*;
use parking_lot::RwLock;
use primitive_types::{H160, H256, U256};
use ruc::{eg, Result};
use std::{borrow::BorrowMut, path::Path, sync::Arc};
use storage::state::{ChainState, ChainStateOpts};
use tracing::{error, info};

lazy_static! {
    /// An identifier that distinguishes different EVM chains.
    static ref EVM_CAHIN_ID: u64 = std::env::var("EVM_CHAIN_ID").map(
        |id| id.as_str().parse::<u64>().unwrap()).unwrap_or(2152);

}

const APP_NAME: &str = "findora";
const CHAIN_STATE_PATH: &str = "state.db";
const CHAIN_HISTORY_DATA_PATH: &str = "history.db";
const BLOCKS_IN_DAY: u64 = 4 * 60 * 24;
const SNAPSHOT_INTERVAL: u64 = 10 * 24;


#[derive(Clone)]
pub struct BaseApp {
    /// application name from abci.Info
    pub name: String,
    /// application's version string
    pub version: String,
    /// application's protocol version that increments on every upgrade
    /// if BaseApp is passed to the upgrade keeper's NewKeeper method.
    pub app_version: u64,
    /// Chain persistent state with merkle root hash
    pub chain_state: Arc<RwLock<ChainState<FinDB>>>,
    /// Chain persistent state without merkle root hash
    pub chain_db: Arc<RwLock<ChainState<RocksDB>>>,
    /// volatile states
    ///
    /// check_state is set on InitChain and reset on Commit
    /// deliver_state is set on InitChain and BeginBlock and set to nil on Commit
    pub check_state: Context,
    pub deliver_state: Context,
    /// Ordered module set
    pub modules: ModuleManager,
    /// New Block event notify
    pub event_notify: Arc<Notifications<BlockId>>,
}

impl module_template::Config for BaseApp {}

pub struct StableTxFee;

impl FeeCalculator for StableTxFee {
    fn min_fee() -> U256 {
        // TX_FEE_MIN
        U256::from(1_0000_0000_0000_0000_u64)
    }
}

impl module_account::Config for BaseApp {
    type FeeCalculator = StableTxFee;
}

parameter_types! {
    pub ChainId: u64 = *EVM_CAHIN_ID;
    pub BlockGasLimit: U256 = U256::from(u32::max_value());
    pub const BlockHashCount: u32 = 256;
}

impl module_ethereum::Config for BaseApp {
    type AccountAsset = module_account::App<Self>;
    type AddressMapping = EthereumAddressMapping;
    type BlockGasLimit = BlockGasLimit;
    type BlockHashCount = BlockHashCount;
    type ChainId = ChainId;
    type DecimalsMapping = EthereumDecimalsMapping;
    type FeeCalculator = ();
    type Runner = module_evm::runtime::runner::ActionRunner<Self>;
}

// parameter_types! {
//     pub PrecompilesValue: FindoraPrecompiles<BaseApp> = FindoraPrecompiles::<_>::new();
// }

pub struct PrecompilesValue;

impl PrecompilesValue {
    #[doc = " Returns the value of this parameter type."]
    pub fn get(ctx: Context2) -> FindoraPrecompiles<BaseApp> {
        FindoraPrecompiles::<_>::new(ctx)
    }
}
impl<I: From<FindoraPrecompiles<BaseApp>>> fp_core::macros::Get2<I, Context2>
    for PrecompilesValue
{
    fn get(ctx: Context2) -> I {
        I::from(FindoraPrecompiles::<_>::new(ctx))
    }
}

impl module_evm::Config for BaseApp {
    type AccountAsset = module_account::App<Self>;
    type AddressMapping = EthereumAddressMapping;
    type BlockGasLimit = BlockGasLimit;
    type BlockHashMapping = module_ethereum::App<Self>;
    type ChainId = ChainId;
    type DecimalsMapping = EthereumDecimalsMapping;
    type FeeCalculator = ();
    type Precompiles = (
        evm_precompile_basic::ECRecover,
        evm_precompile_basic::Sha256,
        evm_precompile_basic::Ripemd160,
        evm_precompile_basic::Identity,
        evm_precompile_modexp::Modexp,
        evm_precompile_basic::ECRecoverPublicKey,
        evm_precompile_sha3fips::Sha3FIPS256,
        evm_precompile_sha3fips::Sha3FIPS512,
        evm_precompile_frc20::FRC20<Self>,
        evm_precompile_eth_pairings::EthPairing,
    );
    type PrecompilesType = FindoraPrecompiles<Self>;
    type PrecompilesValue = PrecompilesValue;
}

impl module_xhub::Config for BaseApp {
    type AccountAsset = module_account::App<Self>;
    type DecimalsMapping = EthereumDecimalsMapping;
}

impl BaseApp {
    pub fn new(
        basedir: &Path,
        empty_block: bool,
        trace: u16,
        is_fresh: bool,
    ) -> Result<Self> {
        info!(
            target: "baseapp",
            "create new baseapp with basedir {:?}, empty_block {}, history {} days, is_fresh {}",
            basedir, empty_block, trace, is_fresh
        );

        // Creates a fresh chain state db and history db
        let fdb_path = basedir.join(CHAIN_STATE_PATH);
        let fdb = FinDB::open(fdb_path.as_path())?;

        let opts = ChainStateOpts {
            name: Some("findora_db".to_owned()),
            ver_window: BLOCKS_IN_DAY * trace as u64,
            cleanup_aux: is_fresh,
            interval: SNAPSHOT_INTERVAL * trace as u64,
            ..Default::default()
        };
        let chain_state = Arc::new(RwLock::new(ChainState::create_with_opts(fdb, opts)));

        let rdb_path = basedir.join(CHAIN_HISTORY_DATA_PATH);
        let rdb = RocksDB::open(rdb_path.as_path())?;
        let chain_db =
            Arc::new(RwLock::new(ChainState::new(rdb, "rocks_db".to_owned(), 0)));

        //Migrate any existing data from one database to the other.
        BaseApp::migrate_initial_db(chain_state.clone(), chain_db.clone())?;

        Ok(BaseApp {
            name: APP_NAME.to_string(),
            version: "1.0.0".to_string(),
            app_version: 1,
            chain_state: chain_state.clone(),
            chain_db: chain_db.clone(),
            check_state: Context::new(chain_state.clone(), chain_db.clone()),
            deliver_state: Context::new(chain_state, chain_db),
            modules: ModuleManager {
                ethereum_module: module_ethereum::App::<Self>::new(empty_block),
                ..Default::default()
            },
            event_notify: Arc::new(Notifications::new()),
        })
    }

    pub fn derive_app(&self) -> Self {
        let chain_state = self.chain_state.clone();
        let chain_db = self.chain_db.clone();

        BaseApp {
            name: APP_NAME.to_string(),
            version: "1.0.0".to_string(),
            app_version: 1,
            chain_state: chain_state.clone(),
            chain_db: chain_db.clone(),
            check_state: Context::new(chain_state.clone(), chain_db.clone()),
            deliver_state: Context::new(chain_state, chain_db),
            modules: ModuleManager::default(),
            event_notify: self.event_notify.clone(),
        }
    }

    //Migrate any pre-existing data from one database to the other if necessary
    pub fn migrate_initial_db(
        state_merkle: Arc<RwLock<ChainState<FinDB>>>,
        state_db: Arc<RwLock<ChainState<RocksDB>>>,
    ) -> Result<()> {
        //Create context.
        let mut ctx = Context::new(state_merkle, state_db);
        let height = ctx.db.read().height()?;

        //Migrate data for ethereum module.
        if module_ethereum::App::<Self>::migrate(ctx.borrow_mut()).is_err() {
            ctx.db.write().discard_session();
        };
        ctx.db.write().commit(height)?;
        Ok(())
    }
}

impl ValidateUnsigned for BaseApp {
    type Call = Action;

    fn pre_execute(_ctx: &Context, _call: &Self::Call) -> Result<()> {
        Ok(())
    }

    fn validate_unsigned(_ctx: &Context, _call: &Self::Call) -> Result<()> {
        Err(eg!(
            "Could not find an unsigned validator for the unsigned transaction"
        ))
    }
}

impl Executable for BaseApp {
    type Origin = Address;
    type Call = Action;

    fn execute(
        origin: Option<Self::Origin>,
        call: Self::Call,
        ctx: &Context,
    ) -> Result<ActionResult> {
        match call {
            Action::Ethereum(action) => {
                module_ethereum::App::<Self>::execute(origin, action, ctx)
            }
            Action::Evm(action) => module_evm::App::<Self>::execute(origin, action, ctx),
            Action::XHub(action) => {
                module_xhub::App::<Self>::execute(origin, action, ctx)
            }
            Action::Template(action) => {
                module_template::App::<Self>::execute(origin, action, ctx)
            }
        }
    }
}

impl BaseApp {
    pub fn create_query_context(
        &self,
        height: Option<u64>,
        prove: bool,
    ) -> Result<Context> {
        if let Some(h) = height {
            if h <= 1 && prove {
                return Err(eg!(
                    "cannot query with proof when height <= 1; please provide a valid height"
                ));
            }
        }

        // query from pending state if height is not provided
        // query from latest state otherwise, including versioned data
        if height.is_none() {
            Ok(self.check_state.copy_with_state())
        } else {
            Ok(self.check_state.copy_with_new_state())
        }
    }

    pub fn create_context_at(&self, height: u64) -> Option<Context> {
        self.check_state.state_at(height)
    }

    /// retrieve the context for the txBytes and other memoized values.
    pub fn retrieve_context(&mut self, mode: RunTxMode) -> &mut Context {
        let ctx = if mode == RunTxMode::Deliver {
            &mut self.deliver_state
        } else {
            &mut self.check_state
        };
        ctx.run_mode = mode;
        ctx
    }

    fn validate_height(&self, height: i64) -> Result<()> {
        ensure!(height >= 1, format!("invalid height: {}", height));
        let mut expected_height =
            self.chain_state.read().height().unwrap_or_default() as i64;
        if expected_height == 0 {
            expected_height = height;
        } else {
            expected_height += 1;
        }
        ensure!(
            height == expected_height,
            format!("invalid height: {}; expected: {}", height, expected_height)
        );
        Ok(())
    }

    fn update_state(ctx: &mut Context, header: Header, header_hash: Vec<u8>) {
        ctx.run_mode = RunTxMode::None;
        ctx.header_hash = header_hash;
        ctx.header = header;
    }

    fn update_deliver_state_cache(&mut self) {
        // Clone newest cache from check_state to deliver_state
        // Oldest cache will be dropped, currently drop cache two blocks ago
        self.deliver_state.eth_cache.current = Default::default();
        self.deliver_state.eth_cache.history_n =
            self.deliver_state.eth_cache.history_1.clone();
        // Deliver_state history_1 cache will share the newest transactions from check_state
        self.deliver_state.eth_cache.history_1 =
            self.check_state.eth_cache.current.clone();
    }

    pub fn deliver_findora_tx(
        &mut self,
        tx: &FindoraTransaction,
        hash: &[u8],
    ) -> Result<()> {
        self.modules
            .process_findora_tx(&self.deliver_state, tx, H256::from_slice(hash))
    }
}

impl BaseProvider for BaseApp {
    fn account_of(&self, who: &Address, height: Option<u64>) -> Result<SmartAccount> {
        let ctx = self.create_query_context(height, false)?;
        module_account::App::<Self>::account_of(&ctx, who, height)
            .ok_or(eg!(format!("account does not exist: {}", who)))
    }

    fn current_block(&self, id: Option<BlockId>) -> Option<Block> {
        if let Ok(ctx) = self.create_query_context(Some(0), false) {
            self.modules.ethereum_module.current_block(&ctx, id)
        } else {
            None
        }
    }

    fn current_block_number(&self) -> Option<U256> {
        if let Ok(ctx) = self.create_query_context(Some(0), false) {
            module_ethereum::App::<Self>::current_block_number(&ctx)
        } else {
            None
        }
    }

    fn current_transaction_statuses(
        &self,
        id: Option<BlockId>,
    ) -> Option<Vec<fp_evm::TransactionStatus>> {
        if let Ok(ctx) = self.create_query_context(Some(0), false) {
            self.modules
                .ethereum_module
                .current_transaction_statuses(&ctx, id)
        } else {
            None
        }
    }

    fn current_receipts(&self, id: Option<BlockId>) -> Option<Vec<ethereum::ReceiptV0>> {
        if let Ok(ctx) = self.create_query_context(Some(0), false) {
            self.modules.ethereum_module.current_receipts(&ctx, id)
        } else {
            None
        }
    }

    fn block_hash(&self, id: Option<BlockId>) -> Option<H256> {
        if let Ok(ctx) = self.create_query_context(Some(0), false) {
            module_ethereum::App::<Self>::block_hash(&ctx, id)
        } else {
            None
        }
    }

    fn transaction_index(&self, hash: H256) -> Option<(U256, u32)> {
        if let Ok(ctx) = self.create_query_context(Some(0), false) {
            module_ethereum::App::<Self>::transaction_index(&ctx, hash)
        } else {
            None
        }
    }

    fn account_code_at(&self, address: H160, height: Option<u64>) -> Option<Vec<u8>> {
        if let Ok(ctx) = self.create_query_context(Some(0), false) {
            module_evm::App::<Self>::account_codes(&ctx, &address.into(), height)
        } else {
            None
        }
    }

    fn account_storage_at(
        &self,
        address: H160,
        index: H256,
        height: Option<u64>,
    ) -> Option<H256> {
        if let Ok(ctx) = self.create_query_context(None, false) {
            module_evm::App::<Self>::account_storages(
                &ctx,
                &address.into(),
                &index.into(),
                height,
            )
        } else {
            None
        }
    }
}
