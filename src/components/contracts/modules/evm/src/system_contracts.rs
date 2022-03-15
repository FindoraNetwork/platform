use std::str::FromStr;

use ethabi::Contract;
use ethereum_types::{H160, H256};
use fp_utils::hashing::keccak_256;
use ruc::*;
use serde::{Deserialize, Serialize};

use crate::utils;

#[derive(Serialize, Deserialize, Debug)]
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

        let owner = H160::from_str("0xe95034bE56fbd7D70000B310323B6Be684A49acb").c(d!())?;

        let bytecode_str = include_str!("../contracts/PrismXXBridge.bytecode");

        let salt = H256::zero();

        let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

        let code_hash = keccak_256(&bytecode);

        let bridge_address = utils::compute_create2(owner, salt, H256::from_slice(&code_hash));

        Ok(Self {
            bridge,
            bridge_address,
            owner,
            salt
        })
    }
}
