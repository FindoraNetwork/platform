//!
//! Initial Config
//!

use super::{
    td_addr_to_bytes, BlockHeight, Power, Validator, ValidatorKind,
    STAKING_VALIDATOR_MIN_POWER,
};
use ruc::*;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

// The initial power of an initor.
const DEFAULT_POWER: Power = STAKING_VALIDATOR_MIN_POWER;

/// Generate config during compiling time.
#[derive(Serialize, Deserialize)]
pub struct InitialValidatorInfo {
    height: Option<BlockHeight>,
    /// predefined validators
    pub valiators: Vec<ValidatorStr>,
}

/// Used for parsing config from disk.
#[derive(Serialize, Deserialize, Eq, PartialEq)]
pub struct ValidatorStr {
    /// `XfrPublicKey` in base64 format
    pub id: String,
    // Tendermint Addr, in hex format
    td_addr: String,
    // Tendermint PubKey, in base64 format
    td_pubkey: String,
    td_power: Option<Power>,
    commission_rate: Option<[u64; 2]>,
    memo: Option<String>,
    kind: Option<ValidatorKind>,
}

impl TryFrom<ValidatorStr> for Validator {
    type Error = Box<dyn ruc::RucError>;
    fn try_from(v: ValidatorStr) -> Result<Validator> {
        Ok(Validator {
            td_pubkey: base64::decode(&v.td_pubkey).c(d!())?,
            td_addr: td_addr_to_bytes(&v.td_addr).c(d!())?,
            td_power: v.td_power.unwrap_or(DEFAULT_POWER),
            commission_rate: v.commission_rate.unwrap_or([1, 100]),
            id: wallet::public_key_from_base64(&v.id).c(d!())?,
            memo: v.memo,
            kind: v.kind.unwrap_or(ValidatorKind::Initor),
            signed_last_block: false,
            signed_cnt: 0,
        })
    }
}

/// generate the initial validator-set
pub fn get_inital_validators() -> Result<Vec<Validator>> {
    get_cfg_data().c(d!()).and_then(|i| {
        i.valiators
            .into_iter()
            .map(|v| Validator::try_from(v).c(d!()))
            .collect::<Result<Vec<_>>>()
            .c(d!())
    })
}

#[allow(missing_docs)]
#[cfg(not(any(feature = "debug_env", feature = "abci_mock")))]
pub fn get_cfg_data() -> Result<InitialValidatorInfo> {
    serde_json::from_slice(&include_bytes!("staking_config.json")[..]).c(d!())
}

#[allow(missing_docs)]
#[cfg(feature = "debug_env")]
pub fn get_cfg_data() -> Result<InitialValidatorInfo> {
    serde_json::from_slice(&include_bytes!("staking_config_debug_env.json")[..]).c(d!())
}

#[allow(missing_docs)]
#[cfg(feature = "abci_mock")]
pub fn get_cfg_data() -> Result<InitialValidatorInfo> {
    serde_json::from_slice(&include_bytes!("staking_config_abci_mock.json")[..]).c(d!())
}

/// used in `cfg_generator` binary
#[cfg(not(any(feature = "debug_env", feature = "abci_mock")))]
pub fn get_cfg_path() -> Option<&'static str> {
    option_env!("STAKING_INITIAL_VALIDATOR_CONFIG")
}

#[allow(missing_docs)]
#[cfg(feature = "debug_env")]
pub fn get_cfg_path() -> Option<&'static str> {
    option_env!("STAKING_INITIAL_VALIDATOR_CONFIG_DEBUG_ENV")
}

#[allow(missing_docs)]
#[cfg(feature = "abci_mock")]
pub fn get_cfg_path() -> Option<&'static str> {
    option_env!("STAKING_INITIAL_VALIDATOR_CONFIG_ABCI_MOCK")
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::staking::td_pubkey_to_td_addr;

    #[test]
    fn staking_tendermint_addr_conversion() {
        let data = pnk!(get_cfg_data()).valiators;
        data.into_iter().for_each(|v| {
            let pk = pnk!(base64::decode(&v.td_pubkey));
            assert_eq!(v.td_addr, td_pubkey_to_td_addr(&pk));
        });
    }
}
