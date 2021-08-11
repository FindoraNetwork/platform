//!
//! ## issue
//!
//! - abci process need a very long time to finish a restarting
//!
//! ## reason
//!
//! #### 1. incorrect pulse count
//!
//! - pulse count will not be stored to disk
//! until there are some real transactions
//! - this will cause to send a block-height smaller
//! than the real one to tendermint in `ABCI::info` callback
//! - and this will cause to replay many unnecessary blocks
//! - and this will take a long time ...
//!
//! #### 2. replay all real transactions at starting
//!
//! - TODO: implement a state snapshot to avoid replay
//!
//! ## fix
//!
//! - cache block-height to disk along with the `ABCI::commit` callback
//! - send this cached block height to tendermint when restarting
//!

use crate::abci::config::global_cfg::CFG;
use lazy_static::lazy_static;
use ledger::staking::Staking;
use ruc::*;
use serde_json::Value;
use std::{convert::TryInto, fs};

lazy_static! {
    static ref PATH: (String, String, String) = {
        let ld = CFG.ledger_dir.unwrap_or("/tmp");
        pnk!(fs::create_dir_all(ld));

        let height_cache = format!("{}/.__tendermint_height__", &ld);
        let staking_cache = format!("{}/.____staking____", &ld);
        let block_pulse_cache = format!("{}/.__block_pulse__", &ld);

        (height_cache, staking_cache, block_pulse_cache)
    };
}

pub(super) fn write_height(h: i64) -> Result<()> {
    fs::write(&PATH.0, i64::to_ne_bytes(h)).c(d!())
}

pub(super) fn read_height() -> Result<i64> {
    let read_tendermint_state_height = || {
        // the real-time state path in the abci container
        const STATE_PATH_FF: &str = "/root/.tendermint/data/priv_validator_state.json";
        lazy_static! {
            static ref STATE_PATH: &'static str =
                CFG.tendermint_node_key_config_path.unwrap_or(STATE_PATH_FF);
        }

        fs::read(&*STATE_PATH)
            .c(d!())
            .and_then(|s| serde_json::from_slice::<Value>(&s).c(d!()))
            .and_then(|v| {
                v["height"]
                    .as_str()
                    .ok_or(eg!())
                    .and_then(|h_str| h_str.parse::<i64>().c(d!()))
                    .and_then(|h| alt!(2 > h, Err(eg!()), Ok(h)))
            })
    };

    fs::read(&PATH.0)
        .c(d!())
        .map(|b| i64::from_ne_bytes(b.try_into().unwrap()))
        .or_else(|e| {
            e.print();
            read_tendermint_state_height().c(d!(e))
        })
}

pub(super) fn write_staking(s: &Staking) -> Result<()> {
    serde_json::to_vec(s)
        .c(d!())
        .and_then(|bytes| fs::write(&PATH.1, bytes).c(d!()))
}

pub(super) fn read_staking() -> Result<Staking> {
    fs::read(&PATH.1)
        .c(d!())
        .and_then(|bytes| serde_json::from_slice(&bytes).c(d!()))
}

pub(super) fn write_block_pulse(cnt: u64) -> Result<()> {
    fs::write(&PATH.2, u64::to_ne_bytes(cnt)).c(d!())
}

pub(super) fn read_block_pulse() -> Result<u64> {
    fs::read(&PATH.2)
        .c(d!())
        .map(|b| u64::from_ne_bytes(b.try_into().unwrap()))
}
