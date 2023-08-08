use crate::system_contracts::SystemContracts;
use ethabi::{Contract, Event, EventParam, ParamType, RawLog, Token};
use ethereum::Log;
use ethereum_types::{H160, H256, U256};
use fp_traits::evm::{DecimalsMapping, EthereumDecimalsMapping};
use fp_types::actions::xhub::NonConfidentialOutput;
use ledger::data_model::ASSET_TYPE_FRA;
use ruc::*;
use zei::{
    noah_algebra::serialization::NoahFromToBytes,
    noah_api::xfr::structs::{
        AssetType, ASSET_TYPE_LENGTH
    },
    XfrPublicKey
};

pub fn deposit_asset_event() -> Event {
    Event {
        name: "DepositAsset".to_owned(),
        inputs: vec![
            EventParam {
                name: "asset".to_owned(),
                kind: ParamType::FixedBytes(32),
                indexed: false,
            },
            EventParam {
                name: "receiver".to_owned(),
                kind: ParamType::Bytes,
                indexed: false,
            },
            EventParam {
                name: "amount".to_owned(),
                kind: ParamType::Uint(256),
                indexed: false,
            },
            EventParam {
                name: "decimal".to_owned(),
                kind: ParamType::Uint(8),
                indexed: false,
            },
            EventParam {
                name: "max_supply".to_owned(),
                kind: ParamType::Uint(256),
                indexed: false,
            },
        ],
        anonymous: false,
    }
}

pub fn deposit_asset_event_topic_str() -> String {
    let topic = deposit_asset_event().signature();
    let temp = hex::encode(topic.as_bytes());
    "[0x".to_owned() + &*temp + &*"]".to_owned()
}

pub fn parse_deposit_asset_event(data: Vec<u8>) -> Result<NonConfidentialOutput> {
    let event = deposit_asset_event();
    let log = RawLog {
        topics: vec![event.signature()],
        data,
    };
    let result = event.parse_log(log).c(d!())?;

    let asset = result.params[0].value.clone().into_fixed_bytes().c(d!())?;
    let mut temp = [0u8; ASSET_TYPE_LENGTH];
    temp.copy_from_slice(asset.as_slice());
    let asset_type = AssetType(temp);

    let receiver = result.params[1]
        .value
        .clone()
        .into_bytes()
        .unwrap_or_default();
    let target = XfrPublicKey::noah_from_bytes(receiver.as_slice()).c(d!())?;

    let amount = result.params[2].value.clone().into_uint().c(d!())?;

    let amount = if asset_type == ASSET_TYPE_FRA {
        EthereumDecimalsMapping::convert_to_native_token(amount).as_u64()
    } else {
        amount.as_u64()
    };

    let decimal = result.params[3].value.clone().into_uint().c(d!())?;
    let max_supply = result.params[4].value.clone().into_uint().c(d!())?;

    Ok(NonConfidentialOutput {
        asset: asset_type,
        amount,
        target,
        decimal: decimal.as_u64() as u8,
        max_supply: max_supply.as_u64(),
    })
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
    pre_issue_amount: u64,
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
            Token::Uint(U256::from(pre_issue_amount)),
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

pub fn parse_evm_staking_mint_event(
    staking_contracts: &Contract,
    log: Log,
) -> Result<(XfrPublicKey, u64)> {
    let event = staking_contracts.event("MintOps").map_err(|e| eg!(e))?;
    let log = RawLog {
        topics: log.topics,
        data: log.data,
    };

    let result = event.parse_log(log).map_err(|e| eg!(e))?;
    let public_key_bytes = result.params[0].value.clone().into_bytes().c(d!())?;

    let public_key =
        XfrPublicKey::noah_from_bytes(public_key_bytes.as_slice()).c(d!())?;

    let amount = result.params[1].value.clone().into_uint().c(d!())?.as_u64();

    Ok((public_key, amount))
}

pub fn coinbase_mint_event() -> Event {
    Event {
        name: "CoinbaseMint".to_owned(),
        inputs: vec![
            EventParam {
                name: "validator".to_owned(),
                kind: ParamType::Address,
                indexed: true,
            },
            EventParam {
                name: "delegator".to_owned(),
                kind: ParamType::Address,
                indexed: true,
            },
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

pub fn coinbase_mint_event_topic_str() -> String {
    let topic = coinbase_mint_event().signature();
    let temp = hex::encode(topic.as_bytes());
    "[0x".to_owned() + &*temp + &*"]".to_owned()
}

pub fn parse_evm_staking_coinbase_mint_event(
    event: &Event,
    topics: Vec<H256>,
    data: Vec<u8>,
) -> Result<(H160, Option<XfrPublicKey>, u64)> {
    let log = RawLog { topics, data };
    let result = event.parse_log(log).map_err(|e| eg!(e))?;
    let delegator = result.params[1].value.clone().into_address().c(d!())?;

    let public_key_bytes = result.params[2].value.clone().into_bytes().c(d!())?;
    let amount = result.params[3].value.clone().into_uint().c(d!())?.as_u64();

    if public_key_bytes.is_empty() {
        return Ok((delegator, None, amount));
    }
    let public_key =
        XfrPublicKey::noah_from_bytes(public_key_bytes.as_slice()).c(d!())?;

    Ok((delegator, Some(public_key), amount))
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
