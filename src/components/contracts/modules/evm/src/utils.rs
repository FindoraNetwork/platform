use std::str::FromStr;

use ethabi::Token;
use ethereum_types::{H160, H256, U256};
use fp_core::context::Context;
use fp_traits::evm::{EthereumDecimalsMapping, DecimalsMapping};
use fp_types::actions::xhub::NonConfidentialOutput;
use ledger::data_model::ASSET_TYPE_FRA;
use ruc::*;
use zei::{
    serialization::ZeiFromToBytes,
    xfr::{sig::XfrPublicKey, structs::AssetType},
};

use crate::{runner::ActionRunner, system_contracts::SystemContracts, Config};

pub fn deploy_contract<C: Config>(
    ctx: &Context,
    contracts: &mut SystemContracts,
) -> Result<()> {
    let source = H160::from_str("0xe95034bE56fbd7D70000B310323B6Be684A49acb").c(d!())?;

    let salt = H256::zero();

    // Deploy Bridge here.
    let bytecode_str = include_str!("../contracts/PrismXXBridge.bytecode");

    let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

    let addr = ActionRunner::<C>::inital_system_contract(
        ctx, bytecode, 9999999999, source, salt,
    )?;
    contracts.bridge_address = addr;

    Ok(())
}

pub fn fetch_mint<C: Config>(
    ctx: &Context,
    contracts: &SystemContracts,
    outputs: &mut Vec<NonConfidentialOutput>,
) -> Result<()> {
    let function = contracts.bridge.function("consumeMint").c(d!())?;
    let input = function.encode_input(&[]).c(d!())?;

    let source = H160::zero();
    let target = contracts.bridge_address;

    let ret = ActionRunner::<C>::execute_systemc_contract(
        ctx,
        input,
        source,
        99999999,
        target,
        U256::zero(),
    )
    .c(d!())?;

    let result = function.decode_output(&ret).c(d!())?;

    for v1 in result {
        if let Token::Array(tokens) = v1 {
            for token in tokens {
                if let Token::Tuple(tuple) = token {
                    let output = parse_truple_result(tuple)?;

                    log::info!("Got issue output: {:?}", output);

                    outputs.push(output);
                }
            }
        }
    }

    Ok(())
}

fn parse_truple_result(tuple: Vec<Token>) -> Result<NonConfidentialOutput> {
    let asset = if let Some(v) = tuple.get(0) {
        if let Token::FixedBytes(bytes) = v {
            let mut inner = [0u8; 32];

            inner.copy_from_slice(bytes);

            AssetType(inner)
        } else {
            return Err(eg!("Asset Must be FixedBytes"));
        }
    } else {
        return Err(eg!("No asset in index 1"));
    };

    let target = if let Some(v) = tuple.get(1) {
        if let Token::FixedBytes(bytes) = v {
            XfrPublicKey::zei_from_bytes(bytes)?
        } else {
            return Err(eg!("Asset Must be FixedBytes"));
        }
    } else {
        return Err(eg!("No asset in index 1"));
    };

    let amount = if let Some(v) = tuple.get(2) {
        if let Token::Uint(i) = v {
            i
        } else {
            return Err(eg!("Asset Must be FixedBytes"));
        }
    } else {
        return Err(eg!("No asset in index 1"));
    };

    let amount = if asset == ASSET_TYPE_FRA {
        EthereumDecimalsMapping::convert_to_native_token(*amount).as_u64()
    } else {
        amount.as_u64()
    };

    Ok(NonConfidentialOutput {
        asset,
        amount,
        target,
    })
}
