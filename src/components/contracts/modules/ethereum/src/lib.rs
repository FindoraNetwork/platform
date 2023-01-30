#![deny(warnings)]
#![allow(missing_docs)]

mod basic;
mod impls;

use config::abci::global_cfg::CFG;
use ethereum_types::{H160, H256, U256};
use evm::Config as EvmConfig;
use fp_core::context::RunTxMode;
use fp_core::{
    context::Context,
    ensure,
    macros::Get,
    module::AppModule,
    transaction::{ActionResult, Executable, ValidateUnsigned},
};
use fp_events::*;
use fp_evm::{BlockId, Runner};
use fp_traits::{
    account::AccountAsset,
    evm::{AddressMapping, BlockHashMapping, DecimalsMapping, FeeCalculator},
};
use fp_types::{actions::ethereum::Action, crypto::Address};
use ruc::*;
use std::marker::PhantomData;
use tendermint_proto::abci::{RequestEndBlock, ResponseEndBlock};

pub const MODULE_NAME: &str = "ethereum";

static ISTANBUL_CONFIG: EvmConfig = EvmConfig::istanbul();

pub trait Config {
    /// Account module interface to read/write account assets.
    type AccountAsset: AccountAsset<Address>;
    /// Mapping from address to account id.
    type AddressMapping: AddressMapping;
    /// The block gas limit. Can be a simple constant, or an adjustment algorithm in another pallet.
    type BlockGasLimit: Get<U256>;
    /// Maximum number of block number to block hash mappings to keep (oldest pruned first).
    type BlockHashCount: Get<u32>;
    /// Chain ID of EVM.
    type ChainId: Get<u64>;
    /// Mapping from eth decimals to native token decimals.
    type DecimalsMapping: DecimalsMapping;
    /// Calculator for current gas price.
    type FeeCalculator: FeeCalculator;
    /// EVM execution runner.
    type Runner: Runner;
    /// EVM config used in the module.
    fn config() -> &'static EvmConfig {
        &ISTANBUL_CONFIG
    }
}

pub mod storage {
    use ethereum::{
        BlockV0 as Block, ReceiptV0 as Receipt, TransactionV0 as Transaction,
    };
    use ethereum_types::U256;
    use fp_evm::TransactionStatus;
    use fp_storage::*;
    use fp_types::crypto::HA256;
    use lazy_static::lazy_static;
    use std::sync::Mutex;

    // Mapping for transaction hash and at block number with index.
    generate_storage!(Ethereum, TransactionIndex => Map<HA256, (U256, u32)>);

    // The current Ethereum block number.
    generate_storage!(Ethereum, CurrentBlockNumber => Value<U256>);
    // Mapping for block number and hashes.
    generate_storage!(Ethereum, BlockHash => Map<U256, HA256>);
    // The ethereum history blocks with block number.
    generate_storage!(Ethereum, CurrentBlock => Map<HA256, Block>);
    // The ethereum history receipts with block number.
    generate_storage!(Ethereum, CurrentReceipts => Map<HA256, Vec<Receipt>>);
    // The ethereum history transaction statuses with block number.
    generate_storage!(Ethereum, CurrentTransactionStatuses => Map<HA256, Vec<TransactionStatus>>);

    // The following data is stored in in-memory array
    // Current building block's transactions and receipts.
    type PendingTransactions = Mutex<Vec<(Transaction, TransactionStatus, Receipt)>>;

    lazy_static! {
        pub static ref DELIVER_PENDING_TRANSACTIONS: PendingTransactions =
            Mutex::new(vec![]);
    }
}

#[derive(Event)]
pub struct TransactionExecuted {
    sender: H160,
    to: H160,
    contract_address: H160,
    transaction_hash: H256,
    reason: evm::ExitReason,
}

#[derive(Event)]
pub struct ContractLog {
    pub address: H160,
    pub topics: Vec<H256>,
    pub data: Vec<u8>,
}

#[derive(Clone)]
pub struct App<C> {
    disable_eth_empty_blocks: bool,
    phantom: PhantomData<C>,
}

impl<C: Config> App<C> {
    pub fn new(empty_block: bool) -> Self {
        App {
            disable_eth_empty_blocks: empty_block,
            phantom: Default::default(),
        }
    }
}

impl<C: Config> Default for App<C> {
    fn default() -> Self {
        App {
            disable_eth_empty_blocks: false,
            phantom: Default::default(),
        }
    }
}

impl<C: Config> AppModule for App<C> {
    fn end_block(
        &mut self,
        ctx: &mut Context,
        req: &RequestEndBlock,
    ) -> ResponseEndBlock {
        let _ = ruc::info!(self.store_block(ctx, U256::from(req.height)));
        Default::default()
    }
}

impl<C: Config> Executable for App<C> {
    type Origin = Address;
    type Call = Action;

    fn execute(
        origin: Option<Self::Origin>,
        call: Self::Call,
        ctx: &Context,
    ) -> Result<ActionResult> {
        ensure!(origin.is_none(), "InvalidTransaction: IllegalOrigin");

        match call {
            Action::Transact(tx) => Self::do_transact(ctx, tx),
        }
    }
}

impl<C: Config> ValidateUnsigned for App<C> {
    type Call = Action;

    fn pre_execute(ctx: &Context, call: &Self::Call) -> Result<()> {
        if ctx.header.height >= CFG.checkpoint.evm_checktx_nonce
            && ctx.run_mode == RunTxMode::Check
        {
            let Action::Transact(transaction) = call;
            let origin = Self::recover_signer_fast(ctx, transaction)
                .ok_or_else(|| eg!("ExecuteTransaction: InvalidSignature"))?;
            let account_id = C::AddressMapping::convert_to_account_id(origin);
            C::AccountAsset::inc_nonce(ctx, &account_id)?;
        }

        Ok(())
    }

    fn validate_unsigned(ctx: &Context, call: &Self::Call) -> Result<()> {
        let Action::Transact(transaction) = call;
        if let Some(chain_id) = transaction.signature.chain_id() {
            if chain_id != C::ChainId::get() {
                return Err(eg!(format!(
                    "InvalidChainId, got {}, but expected {}",
                    chain_id,
                    C::ChainId::get()
                )));
            }
        } else {
            return Err(eg!("Must provide chainId".to_string()));
        }

        let origin = Self::recover_signer_fast(ctx, transaction)
            .ok_or_else(|| eg!("ExecuteTransaction: InvalidSignature"))?;

        // Same as go ethereum, Min gas limit is 21000.
        if transaction.gas_limit < U256::from(21000)
            || transaction.gas_limit > C::BlockGasLimit::get()
        {
            return Err(eg!(format!(
                "InvalidGasLimit: got {}, the gas limit must be in range [21000, {}]",
                transaction.gas_limit,
                C::BlockGasLimit::get()
            )));
        }

        if transaction.gas_price < C::FeeCalculator::min_gas_price() {
            return Err(eg!(format!(
                "InvalidGasPrice: got {}, but the minimum gas price is {}",
                transaction.gas_price,
                C::FeeCalculator::min_gas_price()
            )));
        }

        let account_id = C::AddressMapping::convert_to_account_id(origin);
        let account =
            C::AccountAsset::account_of(ctx, &account_id, None).unwrap_or_default();
        let nonce = account.nonce;
        let balance = account.balance;

        if transaction.nonce < nonce {
            #[cfg(not(feature = "benchmark"))]
            return Err(eg!(format!(
                "InvalidNonce: origin: {:?}, got {}, but expected {}",
                origin, transaction.nonce, nonce
            )));
        }

        let fee = transaction.gas_price.saturating_mul(transaction.gas_limit);
        let total_payment = transaction.value.saturating_add(fee);
        if balance < total_payment {
            return Err(eg!(format!(
                "InsufficientBalance, origin: {:?}, actual balance {}, but expected payment {}",
                origin, balance, total_payment
            )));
        }

        Ok(())
    }
}

impl<C: Config> BlockHashMapping for App<C> {
    fn block_hash(ctx: &Context, number: U256) -> Option<H256> {
        Self::block_hash(ctx, Some(BlockId::Number(number)))
    }
}
