use crate::internal_err;
use ethereum_types::U256;
use fp_rpc_core::types::{Filter, FilterChanges, Index, Log};
use fp_rpc_core::EthFilterApi;
use jsonrpc_core::Result;

pub struct EthFilterApiImpl;

impl EthFilterApiImpl {
    pub fn new() -> Self {
        Self
    }
}

impl Default for EthFilterApiImpl {
    fn default() -> Self {
        EthFilterApiImpl::new()
    }
}

impl EthFilterApi for EthFilterApiImpl {
    fn new_filter(&self, _filter: Filter) -> Result<U256> {
        Ok(U256::zero())
    }

    fn new_block_filter(&self) -> Result<U256> {
        Ok(U256::zero())
    }

    fn new_pending_transaction_filter(&self) -> Result<U256> {
        Err(internal_err(
            "new_pending_transaction_filter method not available.",
        ))
    }

    fn filter_changes(&self, _index: Index) -> Result<FilterChanges> {
        Err(internal_err("filter_changes method not available."))
    }

    fn filter_logs(&self, _index: Index) -> Result<Vec<Log>> {
        Err(internal_err("filter_logs method not available."))
    }

    fn uninstall_filter(&self, _index: Index) -> Result<bool> {
        Err(internal_err("uninstall_filter method not available."))
    }
}
