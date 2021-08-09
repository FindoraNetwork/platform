use ruc::*;
use std::{fs, str};

const QA01_GENESIS_URL: &str = "https://dev-qa01.dev.findora.org:26657/genesis";
const TESTNET_GENESIS_URL: &str = "https://prod-testnet.prod.findora.org:26657/genesis";
const MAINNET_GENESIS_URL: &str = "https://prod-mainnet.prod.findora.org:26657/genesis";

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InitMode {
    Dev,
    Testnet,
    Mainnet,
    Qa01,
}

pub fn save_genesis(url: &str, path: &str) -> Result<()> {
    let resp = attohttpc::get(url).send().c(d!())?;
    if resp.is_success() {
        let object: serde_json::Value = resp.json_utf8().c(d!())?;
        let rpc_result = object.get("result").c(d!())?;
        let genesis = rpc_result.get("genesis").c(d!())?;
        let genesis_str = serde_json::to_vec_pretty(genesis).c(d!())?;
        fs::write(path, genesis_str).c(d!())?;
        Ok(())
    } else {
        Err(eg!("Request failed"))
    }
}

pub fn init_genesis(mode: InitMode, path: &str) -> Result<()> {
    match mode {
        InitMode::Testnet => save_genesis(TESTNET_GENESIS_URL, path)?,
        InitMode::Mainnet => save_genesis(MAINNET_GENESIS_URL, path)?,
        InitMode::Qa01 => save_genesis(QA01_GENESIS_URL, path)?,
        InitMode::Dev => {}
    }
    Ok(())
}

pub fn generate_tendermint_config(mode: InitMode, path: &str) -> Result<()> {
    let config_str = fs::read_to_string(path).c(d!())?;
    let rp_tx_ak = str::replace(
        &config_str,
        "index_all_keys = false",
        "index_all_keys = true",
    );
    let rpc_laddr = str::replace(
        &rp_tx_ak,
        "laddr = \"tcp://127.0.0.1:26657\"",
        "laddr = \"tcp://0.0.0.0:26657\"",
    );

    let result = match mode {
        InitMode::Testnet => {
            let cebi = str::replace(
                &rpc_laddr,
                "create_empty_blocks_interval = \"0s\"",
                "create_empty_blocks_interval = \"15s\"",
            );
            str::replace(&cebi, "persistent_peers = \"\"",
                "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@prod-testnet-us-west-2-sentry-000-public.prod.findora.org:26656\"")
        }
        InitMode::Mainnet => {
            let cebi = str::replace(
                &rpc_laddr,
                "create_empty_blocks_interval = \"0s\"",
                "create_empty_blocks_interval = \"15s\"",
            );
            str::replace(&cebi, "persistent_peers = \"\"",
                "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@prod-mainnet-us-west-2-sentry-000-public.prod.findora.org:26656\"")
        }
        InitMode::Qa01 => {
            let cebi = str::replace(
                &rpc_laddr,
                "create_empty_blocks_interval = \"0s\"",
                "create_empty_blocks_interval = \"15s\"",
            );
            str::replace(&cebi, "persistent_peers = \"\"",
                "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@dev-qa01-us-west-2-sentry-000-public.dev.findora.org:26656\"")
        }
        InitMode::Dev => rpc_laddr,
    };
    fs::write(path, result).c(d!())?;
    Ok(())
}
