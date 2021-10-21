//! BaseApp is an ABCI application, this instance manages state storage and all module functions,
//! and provides a state query interface to the outside.

#![deny(warnings)]
#![allow(missing_docs)]
#![allow(clippy::field_reassign_with_default)]

mod app;
pub mod extensions;
mod modules;
mod notify;

use crate::modules::ModuleManager;
use abci::Header;
use ethereum::BlockV0 as Block;
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
use fp_types::{actions::xhub::NonConfidentialOutput, actions::Action, crypto::Address};
use lazy_static::lazy_static;
use ledger::data_model::Transaction as FindoraTransaction;
use notify::*;
use parking_lot::RwLock;
use primitive_types::{H160, H256, U256};
use ruc::{eg, Result};
use std::path::Path;
use std::sync::Arc;
use storage::{
    db::{FinDB, RocksDB},
    state::ChainState,
};

lazy_static! {
    /// An identifier that distinguishes different EVM chains.
    static ref EVM_CAHIN_ID: u64 = std::env::var("EVM_CHAIN_ID").map(
        |id| id.as_str().parse::<u64>().unwrap()).unwrap_or(523);
}

const APP_NAME: &str = "findora";
const CHAIN_STATE_PATH: &str = "state.db";
const CHAIN_HISTORY_DATA_PATH: &str = "history.db";
const CHAIN_STATE_MIN_VERSIONS: u64 = 4 * 60 * 24 * 60;

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
    );
}

impl module_xhub::Config for BaseApp {
    type AccountAsset = module_account::App<Self>;
    type DecimalsMapping = EthereumDecimalsMapping;
}

impl BaseApp {
    pub fn new(basedir: &Path, empty_block: bool) -> Result<Self> {
        // Creates a fresh chain state db and history db
        let fdb_path = basedir.join(CHAIN_STATE_PATH);
        let fdb = FinDB::open(fdb_path.as_path())?;
        let chain_state = Arc::new(RwLock::new(ChainState::new(
            fdb,
            "findora_db".to_owned(),
            CHAIN_STATE_MIN_VERSIONS,
        )));

        let rdb_path = basedir.join(CHAIN_HISTORY_DATA_PATH);
        let rdb = RocksDB::open(rdb_path.as_path())?;
        let chain_db =
            Arc::new(RwLock::new(ChainState::new(rdb, "rocks_db".to_owned(), 0)));

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
    pub fn create_query_context(&self, mut height: u64, prove: bool) -> Result<Context> {
        // when a client did not provide a query height, manually inject the latest
        if height == 0 {
            height = self.chain_state.read().height()?;
        }
        if height <= 1 && prove {
            return Err(eg!(
                "cannot query with proof when height <= 1; please provide a valid height"
            ));
        }

        Ok(self.check_state.copy_with_new_state())
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

    pub fn deliver_findora_tx(&mut self, tx: &FindoraTransaction) -> Result<()> {
        self.modules.process_findora_tx(&self.deliver_state, tx)
    }

    pub fn consume_mint(&mut self) -> Option<Vec<NonConfidentialOutput>> {
        module_xhub::App::<BaseApp>::consume_mint(&self.deliver_state)
    }
}

impl BaseProvider for BaseApp {
    fn account_of(
        &self,
        who: &Address,
        height: Option<u64>,
        ctx: Option<Context>,
    ) -> Result<SmartAccount> {
        let ctx = match ctx {
            None => self.create_query_context(0, false)?,
            Some(ctx) => ctx,
        };
        module_account::App::<Self>::account_of(&ctx, who, height)
            .ok_or(eg!(format!("account does not exist: {}", who)))
    }

    fn current_block(&self, id: Option<BlockId>) -> Option<Block> {
        if let Ok(ctx) = self.create_query_context(0, false) {
            self.modules.ethereum_module.current_block(&ctx, id)
        } else {
            None
        }
    }

    fn current_block_number(&self) -> Option<U256> {
        if let Ok(ctx) = self.create_query_context(0, false) {
            module_ethereum::App::<Self>::current_block_number(&ctx)
        } else {
            None
        }
    }

    fn current_transaction_statuses(
        &self,
        id: Option<BlockId>,
    ) -> Option<Vec<fp_evm::TransactionStatus>> {
        if let Ok(ctx) = self.create_query_context(0, false) {
            self.modules
                .ethereum_module
                .current_transaction_statuses(&ctx, id)
        } else {
            None
        }
    }

    fn current_receipts(&self, id: Option<BlockId>) -> Option<Vec<ethereum::Receipt>> {
        if let Ok(ctx) = self.create_query_context(0, false) {
            self.modules.ethereum_module.current_receipts(&ctx, id)
        } else {
            None
        }
    }

    fn block_hash(&self, id: Option<BlockId>) -> Option<H256> {
        if let Ok(ctx) = self.create_query_context(0, false) {
            module_ethereum::App::<Self>::block_hash(&ctx, id)
        } else {
            None
        }
    }

    fn transaction_index(&self, hash: H256) -> Option<(U256, u32)> {
        if let Ok(ctx) = self.create_query_context(0, false) {
            module_ethereum::App::<Self>::transaction_index(&ctx, hash)
        } else {
            None
        }
    }

    fn account_code_at(&self, address: H160, height: Option<u64>) -> Option<Vec<u8>> {
        if let Ok(ctx) = self.create_query_context(0, false) {
            module_evm::App::<Self>::account_codes(&ctx, &address, height)
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
        if let Ok(ctx) = self.create_query_context(0, false) {
            module_evm::App::<Self>::account_storages(&ctx, &address, &index, height)
        } else {
            None
        }
    }
}
