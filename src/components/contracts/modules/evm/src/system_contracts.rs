use ethabi::Contract;
use ethereum_types::H160;
use ruc::*;

pub struct SystemContracts {
    pub bridge: Contract,
    pub bridge_address: H160,
}

impl SystemContracts {
    pub fn new() -> Result<Self> {
        let abi_str = include_str!("../contracts/PrismXXBridge.abi.json");
        let bridge = Contract::load(abi_str.as_bytes()).c(d!())?;

        Ok(Self {
            bridge,
            bridge_address: H160::default(),
        })
    }
}
