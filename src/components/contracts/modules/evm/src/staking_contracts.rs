use std::str::FromStr;

use config::abci::global_cfg::CFG;
use ethabi::Contract;
use ethereum_types::{H160, H256};
use fp_utils::hashing::keccak_256;
use ruc::*;
use serde::{Deserialize, Serialize};

use crate::utils;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct StakingContracts {
    pub staking: Contract,
    pub staking_address: H160,
    pub owner: H160,
    pub salt: H256,
}

impl StakingContracts {
    pub fn new() -> Result<Self> {
        let abi_str = include_str!("../contracts/StakingSystem.abi.json");
        let staking = Contract::load(abi_str.as_bytes()).c(d!())?;

        let owner =
            H160::from_str("0x72488bAa718F52B76118C79168E55c209056A2E6").c(d!())?;

        let salt = H256::zero();

        let staking_address = if CFG.checkpoint.evm_staking_address.is_empty() {
            // Driect use this bytecode, beacuse we will remove on mainnet
            let bytecode_str = include_str!("../contracts/StakingProxy.bytecode");

            let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

            let code_hash = keccak_256(&bytecode);

            utils::compute_create2(owner, salt, H256::from_slice(&code_hash))
        } else {
            H160::from_str(&CFG.checkpoint.evm_staking_address).c(d!())?
        };

        Ok(Self {
            staking,
            staking_address,
            owner,
            salt,
        })
    }
}
