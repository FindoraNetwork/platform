use std::str::FromStr;

use config::abci::global_cfg::CFG;
use ethabi::Contract;
use ethereum_types::{H160, H256};
use fp_utils::hashing::keccak_256;
use ruc::*;
use serde::{Deserialize, Serialize};

use crate::utils;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SystemContracts {
    pub bridge: Contract,
    pub bridge_address: H160,
    pub owner: H160,
    pub salt: H256,
}

impl SystemContracts {
    pub fn new() -> Result<Self> {
        let abi_str = include_str!("../contracts/PrismXXBridge.abi.json");
        let bridge = Contract::load(abi_str.as_bytes()).c(d!())?;

        let owner =
            H160::from_str("0xBB6ed61e686090a4718A1FD8dD0035D41C48Fa84").c(d!())?;

        let salt = H256::zero();

        let bridge_address = if CFG.checkpoint.prism_bridge_address.is_empty() {
            // Driect use this bytecode, beacuse we will remove on mainnet
            let bytecode_str = include_str!("../contracts/PrismXXProxy.bytecode");

            let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

            let code_hash = keccak_256(&bytecode);

            utils::compute_create2(owner, salt, H256::from_slice(&code_hash))
        } else {
            H160::from_str(&CFG.checkpoint.prism_bridge_address).c(d!())?
        };

        Ok(Self {
            bridge,
            bridge_address,
            owner,
            salt,
        })
    }
}
