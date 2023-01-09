use ethabi::{Event, EventParam, ParamType, RawLog, Token};
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

                    tracing::info!("Got issue output: {:?}", output);

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

    let target = if let Token::Bytes(bytes) =
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
            num.as_u64()
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

fn build_address(address: &[u8]) -> Result<Token> {
    if address.len() != 20 {
        return Err(eg!("Wrong address length from tendermint"));
    }

    Ok(Token::Address(H160::from_slice(address)))
}

pub fn build_evm_staking_input(
    sc: &SystemContracts,
    req: &abci::RequestBeginBlock,
) -> Result<Vec<u8>> {
    let header = req.get_header();
    let commit_info = req.get_last_commit_info();
    let vote_infos = commit_info.get_votes();
    let byzantine_validators = req.get_byzantine_validators();

    let proposer = build_address(header.get_proposer_address())?;

    let mut signed = Vec::with_capacity(vote_infos.len());
    let mut unsigned = Vec::with_capacity(vote_infos.len());

    for info in vote_infos {
        let validator = info.get_validator();
        let address = build_address(validator.get_address())?;

        if info.signed_last_block {
            signed.push(address);
        } else {
            unsigned.push(address);
        }
    }

    let mut byzantines = Vec::with_capacity(byzantine_validators.len());
    let mut behaviors = Vec::with_capacity(byzantine_validators.len());

    for evidence in byzantine_validators {
        let ty: u64 = match evidence.get_field_type() {
            "DUPLICATE_VOTE" => 0,
            "LIGHT_CLIENT_ATTACK" => 1,
            "UNKNOWN" => 2,
            _ => return Err(eg!()),
        };
        behaviors.push(Token::Int(U256::from(ty)));

        let validator = evidence.get_validator();
        let addr = build_address(validator.get_address())?;
        byzantines.push(addr);
    }

    let func = sc.staking.function("trigger").c(d!())?;

    let input = func
        .encode_input(&[
            proposer,
            Token::Array(signed),
            Token::Array(unsigned),
            Token::Array(byzantines),
            Token::Array(behaviors),
        ])
        .c(d!())?;

    Ok(input)
}

fn build_update_info(tk: &Token) -> Result<abci::ValidatorUpdate> {
    if let Token::Tuple(v) = tk {
        let mut update = abci::ValidatorUpdate::default();

        let mut pub_key = abci::PubKey::default();

        if let Token::Bytes(pk) = v.get(0).ok_or(eg!("update info 0 must bytes"))? {
            pub_key.set_data(pk.clone());
        } else {
            return Err(eg!("Error type of public key"));
        }

        if let Token::Uint(ty) = v.get(1).ok_or(eg!("update info 1 must int"))? {
            let ty = match ty.as_u32() {
                1 => "secp256k1",
                2 => "ed25519",
                _ => return Err(eg!("Error number of public key type")),
            };

            pub_key.set_field_type(String::from(ty));
        } else {
            return Err(eg!("Error type of public key type"));
        }

        if let Token::Uint(p) = v.get(3).ok_or(eg!("update info 3 must int"))? {
            update.set_power(p.as_u64() as i64);
        } else {
            return Err(eg!("Error type of public key type"));
        }

        update.set_pub_key(pub_key);

        Ok(update)
    } else {
        Err(eg!(
            "Parse staking contract abi error: Update info must be a tuple"
        ))
    }
}

pub fn build_validator_updates(
    sc: &SystemContracts,
    data: &[u8],
) -> Result<Vec<abci::ValidatorUpdate>> {
    let func = sc.staking.function("getValidatorsList").c(d!())?;
    let dp = func.decode_output(data).c(d!())?;

    if let Token::Array(output) = dp.get(0).c(d!())? {
        let mut res = Vec::with_capacity(output.len());

        for o in output.iter() {
            let r = build_update_info(o)?;
            res.push(r);
        }

        Ok(res)
    } else {
        Err(eg!("Parse staking contract abi error"))
    }
}

pub fn evm_staking_min_event() -> Event {
    Event {
        name: "MintOps".to_owned(),
        inputs: vec![
            EventParam {
                name: "public_key".to_owned(),
                kind: ParamType::Bytes,
                indexed: false,
            },
            EventParam {
                name: "amount".to_owned(),
                kind: ParamType::Uint(256),
                indexed: false,
            },
        ],
        anonymous: false,
    }
}

pub fn parse_evm_staking_mint_event(data: Vec<u8>) -> Result<(XfrPublicKey, u64)> {
    let event = evm_staking_min_event();
    let log = RawLog {
        topics: vec![event.signature()],
        data,
    };

    let result = event.parse_log(log).c(d!())?;

    let public_key_bytes = result.params[0]
        .value
        .clone()
        .into_bytes()
        .unwrap_or_default();

    let public_key = XfrPublicKey::noah_from_bytes(public_key_bytes.as_slice())?;

    let amount = result.params[1]
        .value
        .clone()
        .into_uint()
        .unwrap_or_default()
        .as_u64();

    Ok((public_key, amount))
}

pub fn build_undelegation_mints(
    sc: &SystemContracts,
    data: &[u8],
) -> Result<Vec<(XfrPublicKey, u64)>> {
    let trigger = sc.staking.function("trigger").c(d!())?;
    let dp = trigger.decode_output(data).c(d!())?;

    if let Token::Array(output) = dp.get(0).c(d!())? {
        let mut res = Vec::with_capacity(output.len());

        for o in output.iter() {
            if let Some(r) = build_mints_pairs(o)? {
                res.push(r);
            }
        }
        Ok(res)
    } else {
        Err(eg!("Parse staking contract abi error"))
    }
}

fn build_claim_info(tk: &Token) -> Result<(H160, U256)> {
    if let Token::Tuple(v) = tk {
        let addr = if let Token::Address(addr) =
            v.get(0).ok_or(eg!("update info 0 must bytes"))?
        {
            *addr
        } else {
            return Err(eg!("Error type of public key"));
        };

        let amount = if let Token::Uint(amount) =
            v.get(1).ok_or(eg!("update info 1 must int"))?
        {
            *amount
        } else {
            return Err(eg!("Error type of public key type"));
        };

        Ok((addr, amount))
    } else {
        Err(eg!(
            "Parse staking contract abi error: Update info must be a truple"
        ))
    }
}

pub fn build_claim_ops(sc: &SystemContracts, data: &[u8]) -> Result<Vec<(H160, U256)>> {
    let func = sc.staking.function("getClaimOps").c(d!())?;
    let dp = func.decode_output(data).c(d!())?;

    if let Token::Array(output) = dp.get(0).c(d!())? {
        let mut res = Vec::with_capacity(output.len());

        for o in output.iter() {
            let r = build_claim_info(o)?;
            res.push(r);
        }

        Ok(res)
    } else {
        Err(eg!("Parse staking contract abi error"))
    }
}

fn build_mints_pairs(tk: &Token) -> Result<Option<(XfrPublicKey, u64)>> {
    if let Token::Tuple(v) = tk {
        let amount: u64;
        if let Token::Uint(am) = v.get(1).ok_or(eg!("parameters 1 must uint256."))? {
            amount = am.as_u64();
        } else {
            return Err(eg!("Error type of uint256."));
        }
        if amount == 0 {
            return Ok(None);
        }
        let pub_key: XfrPublicKey;
        if let Token::Bytes(pk) = v.get(0).ok_or(eg!("parameters 0 must bytes."))? {
            pub_key = XfrPublicKey::from_bytes(pk)?;
        } else {
            return Err(eg!("Error type of public key"));
        };

        Ok(Some((pub_key, amount)))
    } else {
        Err(eg!(
            "Parse staking contract abi error: Update info must be a tuple."
        ))
    }
}
