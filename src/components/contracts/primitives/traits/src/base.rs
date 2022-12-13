use ethereum::{BlockV2 as Block, ReceiptV0 as Receipt};
use fp_core::account::SmartAccount;
use fp_evm::BlockId;
use fp_types::crypto::Address;
use primitive_types::{H160, H256, U256};
use ruc::*;

/// Provide query and call interface provided by each module.
pub trait BaseProvider {
    fn account_of(&self, who: &Address, height: Option<u64>) -> Result<SmartAccount>;

    fn current_block(&self, id: Option<BlockId>) -> Option<Block>;

    fn current_block_number(&self) -> Option<U256>;

    fn current_transaction_statuses(
        &self,
        id: Option<BlockId>,
    ) -> Option<Vec<fp_evm::TransactionStatus>>;

    fn current_receipts(&self, id: Option<BlockId>) -> Option<Vec<Receipt>>;

    fn block_hash(&self, id: Option<BlockId>) -> Option<H256>;

    fn transaction_index(&self, hash: H256) -> Option<(U256, u32)>;

    fn account_code_at(&self, address: H160, height: Option<u64>) -> Option<Vec<u8>>;

    fn account_storage_at(
        &self,
        address: H160,
        index: H256,
        height: Option<u64>,
    ) -> Option<H256>;
}
