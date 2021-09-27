use ethereum::{BlockV0 as Block, Receipt};
use fp_core::{account::SmartAccount, context::Context};
use fp_evm::BlockId;
use fp_types::crypto::Address;
use primitive_types::{H160, H256, U256};
use ruc::*;

/// Provide query and call interface provided by each module.
pub trait BaseProvider {
    fn account_of(&self, who: &Address, ctx: Option<Context>) -> Result<SmartAccount>;

    fn current_block(&self, id: Option<BlockId>) -> Option<Block>;

    fn current_block_number(&self) -> Option<U256>;

    fn current_transaction_statuses(
        &self,
        id: Option<BlockId>,
    ) -> Option<Vec<fp_evm::TransactionStatus>>;

    fn current_receipts(&self, id: Option<BlockId>) -> Option<Vec<Receipt>>;

    fn block_hash(&self, id: Option<BlockId>) -> Option<H256>;

    fn account_code_at(&self, address: H160) -> Option<Vec<u8>>;

    fn account_storage_at(&self, address: H160, index: H256) -> Option<H256>;
}
