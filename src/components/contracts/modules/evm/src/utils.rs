use ethabi::Token;
use ethereum_types::{H160, H256, U256};
use fp_core::context::Context;
use fp_traits::evm::{DecimalsMapping, EthereumDecimalsMapping};
use fp_types::actions::xhub::{ClaimOpsInfo, NonConfidentialOutput, ValidatorInfo};
use ledger::{
    data_model::ASSET_TYPE_FRA,
    staking::{ops::governance::ByzantineKind, Amount},
};
use ruc::*;
use sha3::{Digest, Keccak256};
use zei::xfr::{sig::XfrPublicKey, structs::AssetType};
use zei_algebra::serialization::ZeiFromToBytes;

use crate::{
    prismxx_contracts::PrismXXContracts, runner::ActionRunner,
    staking_contracts::StakingContracts, Config,
};

pub fn deploy_prismxx_contract<C: Config>(
    ctx: &Context,
    contracts: &PrismXXContracts,
    bytecode_str: &str,
) -> Result<()> {
    // Deploy Bridge here.
    let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

    ActionRunner::<C>::inital_system_contract(
        ctx,
        bytecode,
        9999999999,
        contracts.owner,
        contracts.salt,
    )?;

    Ok(())
}
pub fn deploy_staking_contract<C: Config>(
    ctx: &Context,
    contracts: &StakingContracts,
    bytecode_str: &str,
) -> Result<()> {
    // Deploy Bridge here.
    let bytecode = hex::decode(&bytecode_str[2..].trim()).c(d!())?;

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
    contracts: &PrismXXContracts,
    outputs: &mut Vec<NonConfidentialOutput>,
) -> Result<()> {
    let function = contracts.bridge.function("consumeMint").c(d!())?;
    let input = function.encode_input(&[]).c(d!())?;

    let source = H160::zero();
    let target = contracts.bridge_address;

    let result = ActionRunner::<C>::execute_systemc_contract(
        ctx,
        input,
        source,
        99999999,
        target,
        U256::zero(),
    )
    .c(d!())?;

    let result = function.decode_output(&result.data).c(d!())?;

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
        XfrPublicKey::zei_from_bytes(bytes)?
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
    hasher.update(&[0xff]);
    hasher.update(&caller[..]);
    hasher.update(&salt[..]);
    hasher.update(&code_hash[..]);
    H256::from_slice(hasher.finalize().as_slice()).into()
}

pub fn fetch_validator_info<C: Config>(
    ctx: &Context,
    contracts: &StakingContracts,
    outputs: &mut Vec<ValidatorInfo>,
) -> Result<()> {
    let function = contracts.staking.function("getValidatorInfoList").c(d!())?;
    let input = function.encode_input(&[]).c(d!())?;

    let source = H160::zero();
    let target = contracts.staking_address;

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
                    let output = parse_validator_truple_result(tuple)?;

                    log::info!("Got issue output: {:?}", output);

                    outputs.push(output);
                }
            }
        }
    }

    Ok(())
}

fn parse_validator_truple_result(tuple: Vec<Token>) -> Result<ValidatorInfo> {
    let public_key = if let Token::Bytes(bytes) =
        tuple.get(0).ok_or(eg!("Public Key Must be Bytes"))?
    {
        bytes.clone()
    } else {
        return Err(eg!("Public Key Must be Bytes"));
    };

    let address =
        if let Token::Address(addr) = tuple.get(1).ok_or(eg!("addr must be Address"))? {
            *addr
        } else {
            return Err(eg!("addr Must be Address"));
        };

    let power = if let Token::Uint(i) = tuple.get(2).ok_or(eg!("No asset in index 2"))? {
        i.as_u64()
    } else {
        return Err(eg!("Power must be uint"));
    };

    Ok(ValidatorInfo {
        public_key,
        address,
        power,
    })
}

pub fn staking_block_trigger<C: Config>(
    ctx: &Context,
    contracts: &StakingContracts,
    proposer: H160,
    signed: Vec<H160>,
    amount: Amount,
    byztines: Vec<H160>,
    behaviors: Vec<ByzantineKind>,
) -> Result<()> {
    let function = contracts.staking.function("blockTrigger").c(d!())?;
    let proposer = Token::Address(proposer);
    let mut tmp_token: Vec<Token> = vec![];
    signed.iter().for_each(|address| {
        tmp_token.push(Token::Address(*address));
    });
    let signed = Token::Array(tmp_token);
    let mut tmp_token: Vec<Token> = vec![];
    byztines.iter().for_each(|address| {
        tmp_token.push(Token::Address(*address));
    });
    let circulation_amount = Token::Uint(U256::from(amount));
    let byztines = Token::Array(tmp_token);
    let mut tmp_token: Vec<Token> = vec![];
    behaviors.iter().for_each(|behavior| {
        let num = match behavior {
            ByzantineKind::DuplicateVote => Token::Uint(U256::from(0)),
            ByzantineKind::LightClientAttack => Token::Uint(U256::from(1)),
            _ => Token::Uint(U256::from(2)),
        };
        tmp_token.push(num);
    });
    let behavior = Token::Array(tmp_token);

    let input = function
        .encode_input(&[proposer, signed, circulation_amount, byztines, behavior])
        .c(d!())?;

    let source = H160::zero();
    let target = contracts.staking_address;
    ActionRunner::<C>::execute_systemc_contract(
        ctx,
        input,
        source,
        99999999,
        target,
        U256::zero(),
    )
    .c(d!())?;
    Ok(())
}

pub fn fetch_claim_ops<C: Config>(
    ctx: &Context,
    contracts: &StakingContracts,
    outputs: &mut Vec<ClaimOpsInfo>,
) -> Result<()> {
    let function = contracts.staking.function("getClaimOps").c(d!())?;
    let input = function.encode_input(&[]).c(d!())?;

    let source = H160::zero();
    let target = contracts.staking_address;

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
                    let output = parse_claim_ops_truple_result(tuple)?;

                    log::info!("Got issue output: {:?}", output);

                    outputs.push(output);
                }
            }
        }
    }

    Ok(())
}

fn parse_claim_ops_truple_result(tuple: Vec<Token>) -> Result<ClaimOpsInfo> {
    let address =
        if let Token::Address(addr) = tuple.get(0).ok_or(eg!("addr must be Address"))? {
            *addr
        } else {
            return Err(eg!("addr must be Address"));
        };

    let amount =
        if let Token::Uint(i) = tuple.get(1).ok_or(eg!("No asset in index 1"))? {
            *i
        } else {
            return Err(eg!("Amount must be uint"));
        };

    Ok(ClaimOpsInfo { address, amount })
}
