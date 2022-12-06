//!
//! Initial Config
//!

use globutils::wallet;
use noah::xfr::sig::XfrKeyPair;

use {
    super::{
        td_addr_to_bytes, BlockHeight, Power, Validator, ValidatorKind,
        STAKING_VALIDATOR_MIN_POWER,
    },
    indexmap::IndexMap,
    ruc::*,
    serde::{Deserialize, Serialize},
    std::convert::TryFrom,
};

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
    /// Tendermint Addr, in hex format
    pub td_addr: String,
    // Tendermint PubKey, in base64 format
    td_pubkey: String,
    td_power: Option<Power>,
    commission_rate: Option<[u64; 2]>,
    memo: Option<String>,
    kind: Option<ValidatorKind>,
}

#[derive(Deserialize)]
/// Use for init staking with mnemonic
pub struct ValidatorMnemonic {
    /// Mnemonic
    pub mnemonic: String,
    ///Tendermint address.
    pub td_addr: String,
    /// Tendermint PubKey, in base64 format
    pub td_pubkey: String,
    td_power: Option<Power>,
    commission_rate: Option<[u64; 2]>,
}

impl TryFrom<ValidatorStr> for Validator {
    type Error = Box<dyn ruc::RucError>;
    fn try_from(v: ValidatorStr) -> Result<Validator> {
        Ok(Validator {
            td_pubkey: base64::decode(&v.td_pubkey).c(d!())?,
            td_addr: td_addr_to_bytes(&v.td_addr).c(d!())?,
            td_power: v.td_power.unwrap_or(DEFAULT_POWER),
            commission_rate: v.commission_rate.unwrap_or([1, 100]),
            id: globutils::wallet::public_key_from_base64(&v.id).c(d!())?,
            memo: v.memo.map_or(Default::default(), |s| {
                serde_json::from_str(s.as_str()).unwrap_or_default()
            }),
            kind: v.kind.unwrap_or(ValidatorKind::Initiator),
            signed_last_block: false,
            signed_cnt: 0,
            delegators: IndexMap::new(),
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

/// generate the initial validator-set with mnemonics
pub fn get_inital_validators_with_mnemonics(
    staking_mnemonic_list: &[u8],
) -> Result<(Vec<Validator>, Vec<XfrKeyPair>)> {
    let vms: Vec<ValidatorMnemonic> =
        serde_json::from_slice(staking_mnemonic_list).c(d!())?;

    let mut validators = vec![];
    let mut key_pairs = vec![];

    for vm in vms.into_iter() {
        let key_pair = wallet::restore_keypair_from_mnemonic_default(&vm.mnemonic)?;
        let v = Validator {
            td_pubkey: base64::decode(&vm.td_pubkey).c(d!())?,
            td_addr: td_addr_to_bytes(&vm.td_addr).c(d!())?,
            td_power: vm.td_power.unwrap_or(DEFAULT_POWER),
            commission_rate: vm.commission_rate.unwrap_or([1, 100]),
            id: key_pair.get_pk(),
            memo: Default::default(),
            kind: ValidatorKind::Initiator,
            signed_last_block: false,
            signed_cnt: 0,
            delegators: IndexMap::new(),
        };
        validators.push(v);
        key_pairs.push(key_pair);
    }
    Ok((validators, key_pairs))
}

#[allow(missing_docs)]
#[cfg(not(feature = "debug_env"))]
pub fn get_cfg_data() -> Result<InitialValidatorInfo> {
    serde_json::from_slice(&include_bytes!("staking_config.json")[..]).c(d!())
}

#[allow(missing_docs)]
#[cfg(feature = "debug_env")]
pub fn get_cfg_data() -> Result<InitialValidatorInfo> {
    serde_json::from_slice(&include_bytes!("staking_config_debug_env.json")[..]).c(d!())
}

/// used in `cfg_generator` binary
#[cfg(not(feature = "debug_env"))]
pub fn get_cfg_path() -> Option<&'static str> {
    option_env!("STAKING_INITIAL_VALIDATOR_CONFIG")
}

#[allow(missing_docs)]
#[cfg(feature = "debug_env")]
pub fn get_cfg_path() -> Option<&'static str> {
    option_env!("STAKING_INITIAL_VALIDATOR_CONFIG_DEBUG_ENV")
}

#[cfg(test)]
#[allow(missing_docs)]
mod test {
    use {super::*, crate::staking::td_pubkey_to_td_addr, ruc::pnk};

    #[test]
    fn staking_tendermint_addr_conversion() {
        let data = pnk!(get_cfg_data()).valiators;
        data.into_iter().for_each(|v| {
            let pk = pnk!(base64::decode(&v.td_pubkey));
            assert_eq!(v.td_addr, td_pubkey_to_td_addr(&pk));
        });
    }
}
