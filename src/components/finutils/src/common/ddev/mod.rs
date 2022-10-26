//!
//! Implementations of the `fn dev` subcommand.
//!
//! - @Author: hui@findora.org
//!

#![deny(warnings)]
#![allow(missing_docs)]

mod init;

use chaindev::tm_ddev::{
    self, CustomOp, EnvMeta, EnvName, EnvOpts, Node, NodeOptsGenerator, NodePorts, Op,
};
use lazy_static::lazy_static;
use ledger::staking::{
    td_addr_to_bytes, Validator as StakingValidator, ValidatorKind, FRA,
};
use noah::xfr::sig::XfrKeyPair;
use rucv3::*;
use serde::{Deserialize, Serialize};
use std::{env, fmt::Write, thread};

lazy_static! {
    static ref DDEV_HOSTS: Option<String> = env::var("FN_DDEV_HOSTS").ok();
}

#[derive(Debug, Default)]
pub struct EnvCfg {
    // the name of this env
    pub name: EnvName,

    // which operation to trigger,
    // default value: `Ops::Show`
    pub ops: Ops,
}

impl From<EnvCfg> for tm_ddev::EnvCfg<(), CustomData, Ports, InitOps> {
    fn from(cfg: EnvCfg) -> Self {
        let op = match cfg.ops {
            Ops::Create {
                block_itv_secs,
                initial_validator_num,
                hosts,
                abcid_bin,
                tendermint_bin,
                abcid_extra_flags,
                tendermint_extra_flags,
                force_create,
                evm_chain_id,
                checkpoint_file,
            } => {
                let opts = EnvOpts {
                    hosts: pnk!(
                        hosts.or_else(|| DDEV_HOSTS.clone()),
                        "No hosts registered! Use `--hosts` or $FN_DDEV_HOSTS to set."
                    )
                    .into(),
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
            Ops::HostPutFile {
                local_path,
                remote_path,
                hosts,
            } => Op::HostPutFile {
                local_path,
                remote_path,
                hosts: hosts.or_else(|| DDEV_HOSTS.clone()).map(|hs| hs.into()),
            },
            Ops::HostGetFile {
                remote_path,
                local_base_dir,
                hosts,
            } => Op::HostGetFile {
                remote_path,
                local_base_dir,
                hosts: hosts.or_else(|| DDEV_HOSTS.clone()).map(|hs| hs.into()),
            },
            Ops::HostExec {
                cmd,
                script_path,
                hosts,
            } => Op::HostExec {
                cmd,
                script_path,
                hosts: hosts.or_else(|| DDEV_HOSTS.clone()).map(|hs| hs.into()),
            },
            Ops::NodeCollectLogs { local_base_dir } => {
                Op::NodeCollectLogs { local_base_dir }
            }
        };
        Self { name: cfg.name, op }
    }
}

impl EnvCfg {
    pub fn exec(self) -> Result<()> {
        tm_ddev::EnvCfg::from(self).exec(OptsGenerator).c(d!())
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
        hosts: Option<String>,

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
    HostPutFile {
        local_path: String,
        remote_path: Option<String>,
        hosts: Option<String>,
    },
    HostGetFile {
        remote_path: String,
        local_base_dir: Option<String>,
        hosts: Option<String>,
    },
    HostExec {
        cmd: Option<String>,
        script_path: Option<String>,
        hosts: Option<String>,
    },
    NodeCollectLogs {
        local_base_dir: Option<String>,
    },
}

impl Default for Ops {
    fn default() -> Self {
        Self::Show
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Ports {
    #[serde(rename = "web3_http_service")]
    web3_http: u16,
    #[serde(rename = "web3_websocket_service")]
    web3_ws: u16,
    #[serde(rename = "abcid_ledger_query_service")]
    app_8668: u16,
    #[serde(rename = "abcid_submission_service")]
    app_8669: u16,
    #[serde(rename = "tendermint_p2p_service")]
    tm_p2p: u16,
    #[serde(rename = "tendermint_rpc_service")]
    tm_rpc: u16,
    #[serde(rename = "abcid_abci_service")]
    app_abci: u16,
}

impl NodePorts for Ports {
    fn app_reserved() -> Vec<u16> {
        vec![8545, 8546, 8668, 8669]
    }
    fn try_create(ports: &[u16]) -> Result<Self> {
        if ports.len() != Self::reserved().len() {
            return Err(eg!("invalid length"));
        }
        Ok(Self {
            web3_http: ports[0],
            web3_ws: ports[1],
            app_8668: ports[2],
            app_8669: ports[3],
            tm_p2p: ports[4],
            tm_rpc: ports[5],
            app_abci: ports[6],
        })
    }
    fn get_port_list(&self) -> Vec<u16> {
        vec![
            self.web3_http,
            self.web3_ws,
            self.app_8668,
            self.app_8669,
            self.tm_p2p,
            self.tm_rpc,
            self.app_abci,
        ]
    }
    fn get_sys_p2p(&self) -> u16 {
        self.tm_p2p
    }
    fn get_sys_rpc(&self) -> u16 {
        self.tm_rpc
    }
    fn get_sys_abci(&self) -> u16 {
        self.app_abci
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
            &n.host.addr,
            n.ports.tm_rpc,
            n.ports.app_abci,
            n.ports.app_8669,
            n.ports.app_8668,
            n.ports.web3_http,
            n.ports.web3_ws,
            &n.home,
        );

        if let Some(cp) = m.custom_data.checkpoint_file.as_ref() {
            write!(opts, " --checkpoint-file {}", cp).unwrap();
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

impl CustomOp for InitOps {
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
fn init(mut env: tm_ddev::Env<CustomData, Ports, OptsGenerator>) -> Result<()> {
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

#[derive(Debug, Clone, Deserialize, Serialize)]
struct BankAccount {
    wallet_address: String,
    public_key: String,
    secret_key: String,
    mnemonic_words: String,
}

impl BankAccount {
    const BANK_ACCOUNT_ADDR: &str =
        "fra18xkez3fum44jq0zhvwq380rfme7u624cccn3z56fjeex6uuhpq6qv9e4g5";
    const BANK_ACCOUNT_PUBKEY: &str = "Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ=";
    const BANK_ACCOUNT_SECKEY: &str = "Ew9fMaryTL44ZXnEhcF7hQ-AB-fxgaC8vyCH-hCGtzg=";
    const BANK_ACCOUNT_MNEMONIC: &str = "field ranch pencil chest effort coyote april move injury illegal forest amount bid sound mixture use second pet embrace twice total essay valve loan";
}

impl Default for BankAccount {
    fn default() -> Self {
        Self {
            wallet_address: Self::BANK_ACCOUNT_ADDR.to_owned(),
            public_key: Self::BANK_ACCOUNT_PUBKEY.to_owned(),
            secret_key: Self::BANK_ACCOUNT_SECKEY.to_owned(),
            mnemonic_words: Self::BANK_ACCOUNT_MNEMONIC.to_owned(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct InitialValidator {
    tendermint_addr: String,
    tendermint_pubkey: String,

    xfr_keypair: XfrKeyPair,
    xfr_mnemonic: String,
    xfr_wallet_addr: String,
}

impl From<&InitialValidator> for StakingValidator {
    fn from(v: &InitialValidator) -> StakingValidator {
        StakingValidator {
            td_pubkey: base64::decode(&v.tendermint_pubkey).unwrap(),
            td_addr: td_addr_to_bytes(&v.tendermint_addr).unwrap(),
            td_power: 400_0000 * FRA,
            commission_rate: [1, 100],
            id: v.xfr_keypair.get_pk(),
            memo: Default::default(),
            kind: ValidatorKind::Initiator,
            signed_last_block: false,
            signed_cnt: 0,
            delegators: Default::default(),
        }
    }
}
