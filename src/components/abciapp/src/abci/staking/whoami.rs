//!
//! # Tendermint Node Address
//!
//! - `sha256(pubkey)[..20]`
//!

use {
    config::abci::global_cfg::CFG, lazy_static::lazy_static,
    ledger::staking::td_addr_to_bytes, ruc::*, serde::Deserialize, std::fs,
};

pub fn get_self_addr() -> Result<Vec<u8>> {
    from_env().c(d!()).or_else(|_| from_config_file().c(d!()))
}

fn from_env() -> Result<Vec<u8>> {
    CFG.tendermint_node_self_addr
        .as_ref()
        .c(d!())
        .and_then(|td_addr| td_addr_to_bytes(td_addr).c(d!()))
}

fn from_config_file() -> Result<Vec<u8>> {
    // the config path in the abci container
    const CFG_PATH_FF: &str = "/root/.tendermint/config/priv_validator_key.json";
    lazy_static! {
        static ref CFG_PATH: &'static str = CFG
            .tendermint_node_key_config_path
            .as_deref()
            .unwrap_or(CFG_PATH_FF);
    }

    fs::read_to_string(*CFG_PATH)
        .c(d!())
        .and_then(|cfg| serde_json::from_str::<SelfAddr>(&cfg).c(d!()))
        .and_then(|sa| td_addr_to_bytes(&sa.address).c(d!()))
}

//
// Structure:
//
// ```
// {
//   "address": "8DB4CBD00D8E6621826BE6A840A98C28D7F27CD9",
//   "pub_key": {
//     "type": "tendermint/PubKeyEd25519",
//     "value": "BSiMm6HFCzWBPB8s1ZOEqtWm6u6dj2Ftamm1s4msg24="
//   },
//   "priv_key": {
//     "type": "tendermint/PrivKeyEd25519",
//     "value": "ON4RyK6Pevf5UrXJZ7uoPdH3RmnJUKyJlwuHQcEijHAFKIybocULNYE8HyzVk4Sq1abq7p2PYW1qabWziayDbg=="
//   }
// }
// ```
#[derive(Deserialize)]
struct SelfAddr {
    address: String,
}
