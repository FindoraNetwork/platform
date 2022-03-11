use ethereum_types::{H160, H256};
use fp_core::context::Context;
use ruc::*;

use crate::{runner::ActionRunner, Config, system_contracts::SystemContracts};

pub fn deploy_contract<C: Config>(ctx: &Context, contracts: &mut SystemContracts) -> Result<()> {
    let source = H160::zero();

    let salt = H256::zero();

    // Deploy Asset here.
    let bytecode_str = include_str!("../contracts/PrismXXAsset.bytecode");

    let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

    let addr = ActionRunner::<C>::inital_system_contract(ctx, bytecode, 9999999, source, salt)?;
    contracts.asset_address = addr;

    // Deploy Ledger here.
    let bytecode_str = include_str!("../contracts/PrismXXLedger.bytecode");

    let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

    let addr = ActionRunner::<C>::inital_system_contract(ctx, bytecode, 9999999, source, salt)?;
    contracts.ledger_address = addr;

    // Deploy Bridge here.
    let bytecode_str = include_str!("../contracts/PrismXXBridge.bytecode");

    let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

    let addr = ActionRunner::<C>::inital_system_contract(ctx, bytecode, 9999999, source, salt)?;
    contracts.bridge_address = addr;

    // TODO: inital value here.

    Ok(())
}
