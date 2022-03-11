use ethabi::Contract;
use ruc::*;

pub struct SystemContracts {
    pub asset: Contract,
    pub ledger: Contract,
    pub bridge: Contract,
}

impl SystemContracts {
    pub fn new() -> Result<Self> {
        let abi_str = include_str!("../contracts/PrismXXAsset.abi.json");
        let asset = Contract::load(abi_str.as_bytes()).c(d!())?;

        let abi_str = include_str!("../contracts/PrismXXBridge.abi.json");
        let bridge = Contract::load(abi_str.as_bytes()).c(d!())?;

        let abi_str = include_str!("../contracts/PrismXXLedger.abi.json");
        let ledger = Contract::load(abi_str.as_bytes()).c(d!())?;

        Ok(Self {
            asset,
            ledger,
            bridge,
        })
    }
}
