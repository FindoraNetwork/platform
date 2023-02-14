use ethereum_types::H256;
use fp_rpc_core::{types::Bytes, Web3Api};
use jsonrpc_core::Result;
use rustc_version::version;
use sha3::{Digest, Keccak256};

pub struct Web3ApiImpl;

impl Web3ApiImpl {
    pub fn new() -> Self {
        Self
    }
}

impl Default for Web3ApiImpl {
    fn default() -> Self {
        Web3ApiImpl::new()
    }
}

impl Web3Api for Web3ApiImpl {
    fn client_version(&self) -> Result<String> {
        Ok(format!(
            "findora-web3-engine/{}-{}-{}",
            version().unwrap_or_else(|_| semver::Version::parse("1.55.0").unwrap()),
            std::env::var("CARGO_CFG_TARGET_ARCH")
                .unwrap_or_else(|_| "amd64".to_string()),
            std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_else(|_| "linux".to_string())
        ))
    }

    fn sha3(&self, input: Bytes) -> Result<H256> {
        Ok(H256::from_slice(
            Keccak256::digest(input.into_vec()).as_slice(),
        ))
    }
}
