use ethabi::{Event, EventParam, ParamType, RawLog};
use fp_traits::evm::{DecimalsMapping, EthereumDecimalsMapping};
use fp_types::actions::xhub::NonConfidentialOutput;
use ledger::data_model::ASSET_TYPE_FRA;
use noah::xfr::structs::ASSET_TYPE_LENGTH;
use noah::xfr::{sig::XfrPublicKey, structs::AssetType};
use noah_algebra::serialization::NoahFromToBytes;
use ruc::*;

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

    let asset = result.params[0]
        .value
        .clone()
        .into_fixed_bytes()
        .unwrap_or_default();
    let mut temp = [0u8; ASSET_TYPE_LENGTH];
    temp.copy_from_slice(asset.as_slice());
    let asset_type = AssetType(temp);

    let receiver = result.params[1]
        .value
        .clone()
        .into_bytes()
        .unwrap_or_default();
    let target = XfrPublicKey::noah_from_bytes(receiver.as_slice()).unwrap_or_default();

    let amount = result.params[2]
        .value
        .clone()
        .into_uint()
        .unwrap_or_default();

    let amount = if asset_type == ASSET_TYPE_FRA {
        EthereumDecimalsMapping::convert_to_native_token(amount).as_u64()
    } else {
        amount.as_u64()
    };

    let decimal = result.params[3]
        .value
        .clone()
        .into_uint()
        .unwrap_or_default();
    let max_supply = result.params[4]
        .value
        .clone()
        .into_uint()
        .unwrap_or_default();

    Ok(NonConfidentialOutput {
        asset: asset_type,
        amount,
        target,
        decimal: decimal.as_u64() as u8,
        max_supply: max_supply.as_u64(),
    })
}
