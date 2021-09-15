#![deny(warnings)]
#![allow(missing_docs)]

mod precompile;

use ethereum_types::{Bloom, H160, H256, U256};
use evm::ExitReason;
use fp_core::context::Context;
use fp_types::actions::evm::{Call, Create, Create2};
use ruc::*;
use serde::{Deserialize, Serialize};

pub use evm::backend::{Basic as Account, Log};
pub use precompile::{LinearCostPrecompile, Precompile, PrecompileSet};

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, Default)]
/// External input from the transaction.
pub struct Vicinity {
    /// Current transaction gas price.
    pub gas_price: U256,
    /// Origin of the transaction.
    pub origin: H160,
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct ExecutionInfo<T> {
    pub exit_reason: ExitReason,
    pub value: T,
    pub used_gas: U256,
    pub logs: Vec<Log>,
}

pub type CallInfo = ExecutionInfo<Vec<u8>>;
pub type CreateInfo = ExecutionInfo<H160>;

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum CallOrCreateInfo {
    Call(CallInfo),
    Create(CreateInfo),
}

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct TransactionStatus {
    pub transaction_hash: H256,
    pub transaction_index: u32,
    pub from: H160,
    pub to: Option<H160>,
    pub contract_address: Option<H160>,
    pub logs: Vec<Log>,
    pub logs_bloom: Bloom,
}

impl Default for TransactionStatus {
    fn default() -> Self {
        TransactionStatus {
            transaction_hash: H256::default(),
            transaction_index: 0_u32,
            from: H160::default(),
            to: None,
            contract_address: None,
            logs: Vec::new(),
            logs_bloom: Bloom::default(),
        }
    }
}

pub trait Runner {
    fn call(ctx: &Context, args: Call, config: &evm::Config) -> Result<CallInfo>;

    fn create(ctx: &Context, args: Create, config: &evm::Config) -> Result<CreateInfo>;

    fn create2(ctx: &Context, args: Create2, config: &evm::Config)
        -> Result<CreateInfo>;
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum BlockId {
    /// Identify by block header hash.
    Hash(H256),
    /// Identify by block number.
    Number(U256),
}

impl BlockId {
    /// Create a block ID from a hash.
    pub fn hash(hash: H256) -> Self {
        BlockId::Hash(hash)
    }

    /// Create a block ID from a number.
    pub fn number(number: U256) -> Self {
        BlockId::Number(number)
    }
}

impl core::fmt::Display for BlockId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{:?}", self)
    }
}
