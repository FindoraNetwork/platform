use ethereum_types::{H160, H256};
use fp_core::context::Context;
use ruc::*;

use crate::{runner::ActionRunner, Config};

pub fn deploy_contract<C: Config>(ctx: &Context) -> Result<()> {
    let source = H160::zero();

    let salt = H256::zero();

    // Deploy Asset here.
    let bytecode_str = include_str!("../contracts/PrismXXAsset.bytecode");

    let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

    ActionRunner::<C>::inital_system_contract(ctx, bytecode, 9999999, source, salt)?;

    // Deploy Ledger here.
    let bytecode_str = include_str!("../contracts/PrismXXLedger.bytecode");

    let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

    ActionRunner::<C>::inital_system_contract(ctx, bytecode, 9999999, source, salt)?;

    // Deploy Bridge here.
    let bytecode_str = include_str!("../contracts/PrismXXBridge.bytecode");

    let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

    ActionRunner::<C>::inital_system_contract(ctx, bytecode, 9999999, source, salt)?;

    // TODO: inital value here.

    Ok(())
}
