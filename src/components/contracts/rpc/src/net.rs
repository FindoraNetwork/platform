use baseapp::BaseApp;
use fp_rpc_core::types::PeerCount;
use fp_rpc_core::NetApi;
use jsonrpc_core::{BoxFuture, Result};
use tracing::warn;

pub struct NetApiImpl;

impl NetApiImpl {
    pub fn new() -> Self {
        Self
    }
}

impl Default for NetApiImpl {
    fn default() -> Self {
        NetApiImpl::new()
    }
}

impl NetApi for NetApiImpl {
    fn version(&self) -> BoxFuture<Result<String>> {
        Box::pin(async move {
            let chain_id = <BaseApp as module_evm::Config>::ChainId::get();
            Ok(chain_id.to_string())
        })
    }

    fn peer_count(&self) -> BoxFuture<Result<PeerCount>> {
        Box::pin(async move {
            warn!(target: "eth_rpc", "NetApi::peer_count");
            Ok(PeerCount::String(format!("0x{:x}", 1)))
        })
    }

    fn is_listening(&self) -> BoxFuture<Result<bool>> {
        Box::pin(async move {
            warn!(target: "eth_rpc", "NetApi::is_listening");
            Ok(true)
        })
    }
}
