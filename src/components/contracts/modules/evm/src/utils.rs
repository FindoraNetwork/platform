use ethabi::Token;
use ethereum_types::{H160, H256, U256};
use fp_core::context::Context;
use fp_traits::evm::{DecimalsMapping, EthereumDecimalsMapping};
use fp_types::actions::xhub::NonConfidentialOutput;
use ledger::data_model::ASSET_TYPE_FRA;
use noah::xfr::{sig::XfrPublicKey, structs::AssetType};
use noah_algebra::serialization::NoahFromToBytes;
use ruc::*;
use sha3::{Digest, Keccak256};

use crate::{runner::ActionRunner, system_contracts::SystemContracts, Config};

pub fn deploy_contract<C: Config>(
    ctx: &Context,
    contracts: &SystemContracts,
    bytecode_str: &str,
) -> Result<()> {
    // Deploy Bridge here.
    let bytecode = hex::decode(bytecode_str[2..].trim()).c(d!())?;

    ActionRunner::<C>::inital_system_contract(
        ctx,
        bytecode,
        9999999999,
        contracts.owner,
        contracts.salt,
    )?;

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

    let (ret, _, _) = ActionRunner::<C>::execute_systemc_contract(
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
    let asset = if let Token::FixedBytes(bytes) =
        tuple.get(0).ok_or(eg!("Asset Must be FixedBytes"))?
    {
        let mut inner = [0u8; 32];

        inner.copy_from_slice(bytes);

        AssetType(inner)
    } else {
        return Err(eg!("Asset Must be FixedBytes"));
    };

    let target = if let Token::FixedBytes(bytes) =
        tuple.get(1).ok_or(eg!("Target must be FixedBytes"))?
    {
        XfrPublicKey::noah_from_bytes(bytes)?
    } else {
        return Err(eg!("Asset Must be FixedBytes"));
    };

    let amount =
        if let Token::Uint(i) = tuple.get(2).ok_or(eg!("No asset in index 2"))? {
            i
        } else {
            return Err(eg!("Amount must be uint"));
        };

    let amount = if asset == ASSET_TYPE_FRA {
        EthereumDecimalsMapping::convert_to_native_token(*amount).as_u64()
    } else {
        amount.as_u64()
    };

    let decimal =
        if let Token::Uint(decimal) = tuple.get(3).ok_or(eg!("No asset in index 3"))? {
            decimal.as_u64() as u8
        } else {
            return Err(eg!("Decimal must be uint"));
        };

    let max_supply =
        if let Token::Uint(num) = tuple.get(4).ok_or(eg!("No asset in index 4"))? {
            EthereumDecimalsMapping::convert_to_native_token(*num).as_u64()
        } else {
            return Err(eg!("Max supply must be uint"));
        };

    Ok(NonConfidentialOutput {
        asset,
        amount,
        target,
        decimal,
        max_supply,
    })
}

pub fn compute_create2(caller: H160, salt: H256, code_hash: H256) -> H160 {
    let mut hasher = Keccak256::new();
    hasher.update([0xff]);
    hasher.update(&caller[..]);
    hasher.update(&salt[..]);
    hasher.update(&code_hash[..]);
    H256::from_slice(hasher.finalize().as_slice()).into()
}
