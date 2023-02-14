//!
//! Implementations of the `fn dev` subcommand.
//!
//! - @Author: hui@findora.org
//!

#![deny(warnings)]
#![allow(missing_docs)]

mod init;

use super::ddev::Ports;
use chaindev::tm_dev::{
    self, CustomOps, EnvMeta, EnvName, EnvOpts, Node, NodeOptsGenerator, Op,
};
use init::{BankAccount, InitialValidator};
use rucv3::*;
use serde::{Deserialize, Serialize};
use std::{fmt::Write, thread};

#[derive(Debug, Default)]
pub struct EnvCfg {
    // the name of this env
    pub name: EnvName,

    // which operation to trigger,
    // default value: `Ops::Show`
    pub ops: Ops,
}

impl From<EnvCfg> for tm_dev::EnvCfg<(), CustomData, Ports, InitOps> {
    fn from(cfg: EnvCfg) -> Self {
        let op = match cfg.ops {
            Ops::Create {
                block_itv_secs,
                initial_validator_num,
                host_ip,
                abcid_bin,
                tendermint_bin,
                abcid_extra_flags,
                tendermint_extra_flags,
                force_create,
                evm_chain_id,
                checkpoint_file,
            } => {
                let opts = EnvOpts {
                    host_ip: host_ip.unwrap_or_else(|| "127.0.0.1".to_owned()),
                    block_itv_secs: block_itv_secs.into(),
                    initial_validator_num,
                    app_bin_path: abcid_bin.unwrap_or_else(|| "abcid".to_owned()),
                    app_extra_opts: abcid_extra_flags.unwrap_or_default(),
                    tendermint_bin_path: tendermint_bin
                        .unwrap_or_else(|| "tendermint".to_owned()),
                    tendermint_extra_opts: tendermint_extra_flags.unwrap_or_default(),
                    force_create,
                    app_state: (),
                    custom_data: CustomData {
                        evm_chain_id,
                        checkpoint_file,
                        bank_account: BankAccount::default(),
                        initial_validator_num,
                        initial_validators: Vec::new(),
                    },
                };
                Op::Create(opts)
            }
            Ops::Destroy => Op::Destroy,
            Ops::DestroyAll => Op::DestroyAll,
            Ops::Start => Op::Start,
            Ops::StartAll => Op::StartAll,
            Ops::Stop => Op::Stop,
            Ops::StopAll => Op::StopAll,
            Ops::PushNode => Op::PushNode,
            Ops::PopNode => Op::PopNode,
            Ops::Init => Op::Custom(InitOps::Init),
            Ops::InitAll => Op::Custom(InitOps::InitAll),
            Ops::Show => Op::Show,
            Ops::ShowAll => Op::ShowAll,
            Ops::List => Op::List,
        };
        Self { name: cfg.name, op }
    }
}

impl EnvCfg {
    pub fn exec(self) -> Result<()> {
        tm_dev::EnvCfg::from(self).exec(OptsGenerator).c(d!())
    }
}

#[derive(Debug)]
pub enum Ops {
    Create {
        // seconds between two blocks,
        // default value: 3
        block_itv_secs: f32,

        // how many initial validators should be created
        initial_validator_num: u8,

        // initialized once in `Ops::Create`,
        host_ip: Option<String>,

        // specify this option if you want to use a custom version of abcid
        abcid_bin: Option<String>,

        // specify this option if you want to use a custom version of tendermint
        tendermint_bin: Option<String>,

        // only used in `Ops::Create`
        abcid_extra_flags: Option<String>,

        // only used in `Ops::Create`
        tendermint_extra_flags: Option<String>,

        force_create: bool,

        // default value: 2152
        evm_chain_id: u64,

        // only used in `Ops::Create`
        // used in `Ops::Create`
        checkpoint_file: Option<String>,
    },
    Destroy,
    DestroyAll,
    Start,
    StartAll,
    Stop,
    StopAll,
    PushNode,
    PopNode,
    Init,
    InitAll,
    Show,
    ShowAll,
    List,
}

impl Default for Ops {
    fn default() -> Self {
        Self::Show
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct OptsGenerator;

impl NodeOptsGenerator<Node<Ports>, EnvMeta<CustomData, Node<Ports>>> for OptsGenerator {
    fn app_opts(
        &self,
        n: &Node<Ports>,
        m: &EnvMeta<CustomData, Node<Ports>>,
    ) -> (String, String) {
        let vars = format!(
            "EVM_CHAIN_ID={} FINDORA_BLOCK_ITV={}",
            m.custom_data.evm_chain_id,
            1 + f32::from(m.block_itv_secs) as u64
        );
        let mut opts = format!(
            "\
            --enable-query-service \
            --enable-eth-api-service \
            --tendermint-host {0} \
            --tendermint-port {1} \
            --abcid-port {2} \
            --submission-service-port {3} \
            --ledger-service-port {4} \
            --evm-http-port {5} \
            --evm-ws-port {6} \
            --ledger-dir {7}/__findora__ \
            --tendermint-node-key-config-path {7}/config/priv_validator_key.json \
            ",
            &m.host_ip,
            n.ports.tm_rpc,
            n.ports.app_abci,
            n.ports.app_8669,
            n.ports.app_8668,
            n.ports.web3_http,
            n.ports.web3_ws,
            &n.home,
        );

        if let Some(cp) = m.custom_data.checkpoint_file.as_ref() {
            write!(opts, " --checkpoint-file {cp}",).unwrap();
        }
        write!(opts, " {}", &m.app_extra_opts).unwrap();

        (vars, opts)
    }
    fn tendermint_opts(
        &self,
        n: &Node<Ports>,
        m: &EnvMeta<CustomData, Node<Ports>>,
    ) -> (String, String) {
        (
            "".to_owned(),
            format!("node --home {} {}", &n.home, &m.tendermint_extra_opts),
        )
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
enum InitOps {
    Init,
    InitAll,
}

type Env = EnvMeta<CustomData, Node<Ports>>;

impl CustomOps for InitOps {
    fn exec(&self, env_name: &EnvName) -> Result<()> {
        match self {
            InitOps::Init => Env::load_env_by_name(env_name)
                .c(d!())
                .and_then(|env| env.c(d!()))
                .and_then(|env| init(env).c(d!())),
            InitOps::InitAll => init_all().c(d!()),
        }
    }
}

// 1. get validator list by ':26657/validators'
// 2. generate coresponding Xfr keypairs by `common::gen_key()`
// 3. send out the initial staking transaction
fn init(mut env: tm_dev::Env<CustomData, Ports, OptsGenerator>) -> Result<()> {
    if !env.meta.custom_data.initial_validators.is_empty() {
        eprintln!(
            "[ {} ] \x1b[31;01mAlready initialized!\x1b[00m",
            &env.meta.name
        );
        return Ok(());
    }
    init::init(&mut env.meta)
        .map_err(|e| eg!(e))
        .and_then(|_| env.write_cfg().c(d!("fail to update meta info")))
}

// apply the `init` operatio to all existing ENVs
fn init_all() -> Result<()> {
    let env_list = Env::get_env_list().c(d!())?;
    thread::scope(|s| {
        for en in env_list.iter() {
            s.spawn(|| {
                let env = pnk!(Env::load_env_by_name::<OptsGenerator>(en));
                info_omit!(init(pnk!(env)));
            });
        }
    });
    Ok(())
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct CustomData {
    // default value: 2152
    evm_chain_id: u64,

    // path of the checkpoint file
    checkpoint_file: Option<String>,

    // FRA tokens will be issued to this account
    bank_account: BankAccount,

    initial_validator_num: u8,

    #[serde(rename = "initial_pos_settings")]
    initial_validators: Vec<InitialValidator>,
}
