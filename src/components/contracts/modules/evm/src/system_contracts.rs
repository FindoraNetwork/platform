use ethabi::Contract;
use ethereum_types::H160;
use ruc::*;

pub struct SystemContracts {
    pub asset: Contract,
    pub asset_address: H160,

    pub ledger: Contract,
    pub ledger_address: H160,

    pub bridge: Contract,
    pub bridge_address: H160,
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
            asset_address: H160::default(),
            ledger,
            ledger_address: H160::default(),
            bridge,
            bridge_address: H160::default(),
        })
    }
}
