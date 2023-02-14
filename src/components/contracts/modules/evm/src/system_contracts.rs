use std::str::FromStr;

use config::abci::global_cfg::CFG;
use ethabi::Contract;
use ethereum_types::H160;
use ruc::*;
use serde::{Deserialize, Serialize};

pub static SYSTEM_ADDR: &str = "0x0000000000000000000000000000000000002000";

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SystemContracts {
    pub bridge: Contract,
    pub bridge_address: H160,
}

impl SystemContracts {
    pub fn new() -> Result<Self> {
        let abi_str = include_str!("../contracts/PrismXXBridge.abi.json");
        let bridge = Contract::load(abi_str.as_bytes()).c(d!())?;
        let bridge_address =
            H160::from_str(&CFG.checkpoint.prism_bridge_address).unwrap_or_default();

        Ok(Self {
            bridge,
            bridge_address,
        })
    }
}
