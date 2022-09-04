//!
//! Implementations of the `fn dev` subcommand.
//!
//! - @Author: hui@findora.org
//!

#![deny(warnings)]
#![allow(missing_docs)]

mod init;

use ledger::staking::{
    td_addr_to_bytes, Validator as StakingValidator, ValidatorKind, FRA, VALIDATORS_MIN,
};
use nix::{
    sys::socket::{
        bind, setsockopt, socket, sockopt, AddressFamily, SockFlag, SockType, SockaddrIn,
    },
    unistd::{close, fork, ForkResult},
};
use ruc::{cmd, *};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write as _,
    fs::{self, OpenOptions},
    io::{ErrorKind, Write},
    path::PathBuf,
    process::{exit, Command, Stdio},
    str::FromStr,
    thread,
};
use tendermint::{
    config::{PrivValidatorKey as TmValidatorKey, TendermintConfig as TmConfig},
    validator::Info as TmValidator,
    vote::Power as TmPower,
};
use toml_edit::{value as toml_value, Document};
use zei::xfr::sig::XfrKeyPair;

type NodeId = u32;

const ENV_BASE_DIR: &str = "/tmp/__FINDORA_DEV__";
const ENV_NAME_DEFAULT: &str = "DEFAULT";

const INIT_POWER: u32 = 1;

const MB: i64 = 1024 * 1024;
const GB: i64 = 1024 * MB;

const BANK_ACCOUNT_ADDR: &str =
    "fra18xkez3fum44jq0zhvwq380rfme7u624cccn3z56fjeex6uuhpq6qv9e4g5";
const BANK_ACCOUNT_PUBKEY: &str = "Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ=";
const BANK_ACCOUNT_SECKEY: &str = "Ew9fMaryTL44ZXnEhcF7hQ-AB-fxgaC8vyCH-hCGtzg=";
const BANK_ACCOUNT_MNEMONIC: &str = "field ranch pencil chest effort coyote april move injury illegal forest amount bid sound mixture use second pet embrace twice total essay valve loan";

const EXEC_LOG_NAME: &str = "fn_dev.log";

#[derive(Debug)]
pub struct EnvCfg {
    // the name of this env
    pub name: String,

    // which operation to trigger,
    // default value: `Ops::Info`
    pub ops: Ops,

    // seconds between two blocks,
    // default value: 3
    pub block_itv_secs: u8,

    // how many initial validators should be created
    pub initial_validator_num: u8,

    // default value: 2152
    pub evm_chain_id: u64,

    // only used in `Ops::Create`
    pub checkpoint_file: Option<String>,

    // only used in `Ops::Create`
    pub abcid_extra_flags: Option<String>,

    // initialized once in `Ops::Create`,
    // default value: "127.0.0.1"
    pub host_ip: Option<String>,
}

impl Default for EnvCfg {
    fn default() -> Self {
        Self {
            name: ENV_NAME_DEFAULT.to_owned(),
            ops: Ops::default(),
            block_itv_secs: 3,
            initial_validator_num: VALIDATORS_MIN as u8,
            evm_chain_id: 2152,
            checkpoint_file: None,
            abcid_extra_flags: None,
            host_ip: None,
        }
    }
}

impl EnvCfg {
    pub fn exec(&self) -> Result<Option<Env>> {
        match self.ops {
            Ops::Create => Env::create(self).c(d!()).map(Some),
            Ops::Destroy => Env::load_cfg(self)
                .c(d!())
                .and_then(|env| env.destroy().c(d!()))
                .map(|_| None),
            Ops::DestroyAll => Env::destroy_all().c(d!()).map(|_| None),
            Ops::Start => Env::load_cfg(self)
                .c(d!())
                .and_then(|mut env| env.start(None).c(d!()))
                .map(|_| None),
            Ops::Stop => Env::load_cfg(self)
                .c(d!())
                .and_then(|env| env.stop().c(d!()))
                .map(|_| None),
            Ops::AddNode => Env::load_cfg(self)
                .c(d!())
                .and_then(|mut env| env.attach_node().c(d!()))
                .map(|_| None),
            Ops::DelNode => Env::load_cfg(self)
                .c(d!())
                .and_then(|mut env| env.kick_node().c(d!()))
                .map(|_| None),
            Ops::Info => Env::load_cfg(self).c(d!()).map(|env| {
                env.print_info();
                None
            }),
            Ops::InfoAll => Env::info_all().c(d!()).map(|_| None),
            Ops::List => Env::list_all().c(d!()).map(|_| None),
            Ops::Init => Env::load_cfg(self)
                .c(d!())
                .and_then(|mut env| env.init().c(d!()))
                .map(|_| None),
            Ops::InitAll => Env::init_all().c(d!()).map(|_| None),
        }
    }
}

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct Env {
    // the name of this env
    #[serde(rename = "env_name")]
    name: String,
    // data path of this env
    #[serde(rename = "env_home_dir")]
    home: String,

    // default value: "127.0.0.1"
    host_ip: String,

    // FRA tokens will be issued to this account
    bank_account: BankAccount,

    // validator informations related to POS
    #[serde(rename = "initial_validator_number")]
    initial_validator_num: u8,
    #[serde(rename = "initial_pos_settings")]
    initial_validators: Vec<InitialValidator>,

    // seconds between two blocks,
    // default value: 3
    #[serde(rename = "block_interval")]
    block_itv_secs: u8,

    // default value: 2152
    evm_chain_id: u64,

    // path of the checkpoint file, if any
    checkpoint_file: Option<String>,

    // eg,
    // - `--disable-eth-empty-blocks`
    // - ...
    abcid_extra_flags: Option<String>,

    #[serde(rename = "seed_nodes")]
    seeds: BTreeMap<NodeId, Node>,
    #[serde(rename = "validator_or_full_nodes")]
    nodes: BTreeMap<NodeId, Node>,

    // the contents of `genesis.json` of all nodes
    #[serde(rename = "tendermint_genesis_config")]
    genesis: String,

    // the latest/max id of current nodes
    next_node_id: NodeId,
}

impl Env {
    // - initilize a new env
    // - `genesis.json` will be created
    fn create(cfg: &EnvCfg) -> Result<Env> {
        let home = format!("{}/envs/{}", ENV_BASE_DIR, &cfg.name);

        if fs::metadata(&home).is_ok() {
            return Err(eg!("Another env with the same name exists!"));
        }

        let mut env = Env {
            name: cfg.name.clone(),
            home,
            block_itv_secs: cfg.block_itv_secs,
            evm_chain_id: cfg.evm_chain_id,
            checkpoint_file: cfg.checkpoint_file.clone(),
            abcid_extra_flags: cfg.abcid_extra_flags.clone(),
            initial_validator_num: cfg.initial_validator_num,
            host_ip: cfg.host_ip.as_deref().unwrap_or("127.0.0.1").to_owned(),
            ..Self::default()
        };

        fs::create_dir_all(&env.home).c(d!())?;

        macro_rules! add_initial_nodes {
            ($kind: tt) => {{
                let id = env.next_node_id();
                env.alloc_resources(id, Kind::$kind).c(d!())?;
            }};
        }

        add_initial_nodes!(Seed);
        for _ in 0..cfg.initial_validator_num {
            add_initial_nodes!(Node);
        }

        env.gen_genesis()
            .c(d!())
            .and_then(|_| env.apply_genesis(None).c(d!()))
            .and_then(|_| env.start(None).c(d!()))
            .map(|_| env)
    }

    // start one or all nodes
    fn start(&mut self, n: Option<NodeId>) -> Result<()> {
        let ids = n.map(|id| vec![id]).unwrap_or_else(|| {
            self.seeds
                .keys()
                .chain(self.nodes.keys())
                .copied()
                .collect()
        });

        self.update_peer_cfg()
            .c(d!())
            .and_then(|_| self.write_cfg().c(d!()))?;

        for i in ids.iter() {
            if let Some(n) = self.nodes.get_mut(i) {
                n.start(
                    &self.host_ip,
                    self.block_itv_secs,
                    self.evm_chain_id,
                    self.checkpoint_file.as_deref(),
                    self.abcid_extra_flags.as_deref().unwrap_or_default(),
                )
                .c(d!())?;
            } else if let Some(n) = self.seeds.get_mut(i) {
                n.start(
                    &self.host_ip,
                    self.block_itv_secs,
                    self.evm_chain_id,
                    self.checkpoint_file.as_deref(),
                    self.abcid_extra_flags.as_deref().unwrap_or_default(),
                )
                .c(d!())?;
            } else {
                return Err(eg!("not exist"));
            }
        }

        Ok(())
    }

    // - stop all processes
    fn stop(&self) -> Result<()> {
        self.nodes
            .values()
            .chain(self.seeds.values())
            .map(|n| n.stop().c(d!()))
            .collect::<Result<Vec<_>>>()
            .map(|_| ())
    }

    // destroy all nodes
    // - stop all running processes
    // - delete the data of every nodes
    fn destroy(&self) -> Result<()> {
        info_omit!(self.stop());
        sleep_ms!(10);

        for n in self.seeds.values().chain(self.nodes.values()) {
            n.clean().c(d!())?;
        }

        fs::remove_dir_all(&self.home).c(d!())
    }

    // destroy all existing ENVs
    fn destroy_all() -> Result<()> {
        for env in Self::get_all_envs().c(d!())?.iter() {
            Self::read_cfg(env)
                .c(d!())?
                .c(d!("BUG: env not found!"))?
                .destroy()
                .c(d!())?;
        }
        fs::remove_dir_all(ENV_BASE_DIR).c(d!())
    }

    // seed nodes are kept by system for now,
    // so only the other nodes can be added on demand
    fn attach_node(&mut self) -> Result<()> {
        let id = self.next_node_id();
        let kind = Kind::Node;
        self.alloc_resources(id, kind)
            .c(d!())
            .and_then(|_| self.apply_genesis(Some(id)).c(d!()))
            .and_then(|_| self.start(Some(id)).c(d!()))
    }

    // the first node(validator) can not removed
    fn kick_node(&mut self) -> Result<()> {
        self.nodes
            .keys()
            .skip(1)
            .rev()
            .copied()
            .next()
            .c(d!())
            .and_then(|k| self.nodes.remove(&k).c(d!()))
            .and_then(|n| n.stop().c(d!()).and_then(|_| n.clean().c(d!())))
            .and_then(|_| self.write_cfg().c(d!()))
    }

    // 1. get validator list by ':26657/validators'
    // 2. generate coresponding Xfr keypairs by `common::gen_key()`
    // 3. send out the initial staking transaction
    fn init(&mut self) -> Result<()> {
        if !self.initial_validators.is_empty() {
            println!("[ {} ] \x1b[31;01mAlready initialized!\x1b[00m", &self.name);
            return Ok(());
        }

        init::init(self)
            .c(d!())
            .and_then(|_| self.write_cfg().c(d!()))
    }

    // apply the `init` operatio to all existing ENVs
    fn init_all() -> Result<()> {
        let env_list = Self::get_all_envs().c(d!())?;
        thread::scope(|s| {
            for env in env_list.iter() {
                s.spawn(|| {
                    let env = pnk!(Self::read_cfg(env)).c(d!("BUG: env not found!"));
                    let mut env = pnk!(env);
                    info_omit!(env.init());
                });
            }
        });
        Ok(())
    }

    // 1. allocate ports
    // 2. change configs: ports, seed address, etc.
    // 3. insert new node to the meta of env
    // 4. write new configs of tendermint to disk
    fn alloc_resources(&mut self, id: NodeId, kind: Kind) -> Result<()> {
        // 1.
        let ports = alloc_ports(&kind, &self.name).c(d!())?;

        // 2.
        let home = format!("{}/{}", self.home, id);
        fs::create_dir_all(&home).c(d!())?;

        let cfg_path = format!("{}/config/config.toml", &home);
        let mut cfg = fs::read_to_string(&cfg_path)
            .c(d!())
            .or_else(|_| {
                cmd::exec_output(&format!("tendermint init --home {}", &home))
                    .c(d!())
                    .and_then(|_| fs::read_to_string(&cfg_path).c(d!()))
            })
            .and_then(|c| c.parse::<Document>().c(d!()))?;

        cfg["proxy_app"] =
            toml_value(format!("tcp://{}:{}", &self.host_ip, ports.app_abci));
        cfg["rpc"]["laddr"] =
            toml_value(format!("tcp://{}:{}", &self.host_ip, ports.tm_rpc));

        cfg["p2p"]["addr_book_strict"] = toml_value(false);
        cfg["p2p"]["allow_duplicate_ip"] = toml_value(true);
        cfg["p2p"]["persistent_peers_max_dial_period"] = toml_value("3s");
        cfg["p2p"]["send_rate"] = toml_value(64 * MB);
        cfg["p2p"]["recv_rate"] = toml_value(64 * MB);
        cfg["p2p"]["laddr"] =
            toml_value(format!("tcp://{}:{}", &self.host_ip, ports.tm_p2p));

        cfg["consensus"]["timeout_propose"] = toml_value("16s");
        cfg["consensus"]["timeout_propose_delta"] = toml_value("100ms");
        cfg["consensus"]["timeout_prevote"] = toml_value("2s");
        cfg["consensus"]["timeout_prevote_delta"] = toml_value("100ms");
        cfg["consensus"]["timeout_precommit"] = toml_value("2s");
        cfg["consensus"]["timeout_precommit_delta"] = toml_value("100ms");
        cfg["consensus"]["timeout_commit"] =
            toml_value(self.block_itv_secs.to_string() + "s");
        cfg["consensus"]["skip_timeout_commit"] = toml_value(false);
        cfg["consensus"]["create_empty_blocks"] = toml_value(false);
        // cfg["consensus"]["create_empty_blocks_interval"] = toml_value("30s");
        cfg["consensus"]["create_empty_blocks_interval"] =
            toml_value(self.block_itv_secs.to_string() + "s");

        cfg["mempool"]["recheck"] = toml_value(false);

        cfg["moniker"] = toml_value(format!("{}-{}", &self.name, id));

        match kind {
            Kind::Node => {
                cfg["p2p"]["pex"] = toml_value(true);
                cfg["p2p"]["seed_mode"] = toml_value(false);
                cfg["p2p"]["max_num_inbound_peers"] = toml_value(40);
                cfg["p2p"]["max_num_outbound_peers"] = toml_value(10);
                cfg["mempool"]["broadcast"] = toml_value(true);
                cfg["mempool"]["size"] = toml_value(200_0000);
                cfg["mempool"]["max_txs_bytes"] = toml_value(5 * GB);
                cfg["tx_index"]["indexer"] = toml_value("kv");
                cfg["rpc"]["max_open_connections"] = toml_value(10_0000);
            }
            Kind::Seed => {
                cfg["p2p"]["pex"] = toml_value(true);
                cfg["p2p"]["seed_mode"] = toml_value(true);
                cfg["p2p"]["max_num_inbound_peers"] = toml_value(400);
                cfg["p2p"]["max_num_outbound_peers"] = toml_value(100);
                cfg["mempool"]["broadcast"] = toml_value(false);
                cfg["tx_index"]["indexer"] = toml_value("null");
            }
        }

        // 3.
        let node = Node {
            id,
            tm_id: TmConfig::load_toml_file(&cfg_path)
                .map_err(|e| eg!(e))?
                .load_node_key(&home)
                .map_err(|e| eg!(e))?
                .node_id()
                .to_string()
                .to_lowercase(),
            home: format!("{}/{}", &self.home, id),
            kind,
            ports,
        };

        match kind {
            Kind::Node => self.nodes.insert(id, node),
            Kind::Seed => self.seeds.insert(id, node),
        };

        // 4.
        fs::write(cfg_path, cfg.to_string()).c(d!())
    }

    fn update_peer_cfg(&self) -> Result<()> {
        for n in self.nodes.values() {
            let cfg_path = format!("{}/config/config.toml", &n.home);
            let mut cfg = fs::read_to_string(&cfg_path)
                .c(d!())
                .and_then(|c| c.parse::<Document>().c(d!()))?;
            cfg["p2p"]["seeds"] = toml_value(
                self.seeds
                    .values()
                    .map(|n| {
                        format!("{}@{}:{}", &n.tm_id, &self.host_ip, n.ports.tm_p2p)
                    })
                    .collect::<Vec<_>>()
                    .join(","),
            );
            cfg["p2p"]["persistent_peers"] = toml_value(
                self.nodes
                    .values()
                    .map(|n| {
                        format!("{}@{}:{}", &n.tm_id, &self.host_ip, n.ports.tm_p2p)
                    })
                    .collect::<Vec<_>>()
                    .join(","),
            );
            fs::write(cfg_path, cfg.to_string()).c(d!())?;
        }

        Ok(())
    }

    // Allocate unique IDs for nodes within the scope of an env
    fn next_node_id(&mut self) -> NodeId {
        let id = self.next_node_id;
        self.next_node_id += 1;
        id
    }

    // Generate a new `genesis.json`
    // based on the collection of initial validators.
    fn gen_genesis(&mut self) -> Result<()> {
        let tmp_id = NodeId::MAX;
        let tmp_home = format!("{}/{}", &self.home, tmp_id);

        let gen = |genesis_file: String| {
            self.nodes
                .values()
                .map(|n| {
                    TmConfig::load_toml_file(&format!("{}/config/config.toml", &n.home))
                        .map_err(|e| eg!(e))
                        .and_then(|cfg| {
                            cfg.priv_validator_key_file
                                .as_ref()
                                .c(d!())
                                .and_then(|f| {
                                    PathBuf::from_str(&n.home).c(d!()).map(|p| {
                                        p.join(f).to_string_lossy().into_owned()
                                    })
                                })
                                .and_then(|p| {
                                    TmValidatorKey::load_json_file(&p)
                                        .map_err(|e| eg!(e))
                                })
                        })
                        .map(|key| {
                            TmValidator::new(key.pub_key, TmPower::from(INIT_POWER))
                        })
                })
                .collect::<Result<Vec<_>>>()
                .and_then(|vs| serde_json::to_value(&vs).c(d!()))
                .and_then(|mut vs| {
                    vs.as_array_mut().c(d!())?.iter_mut().enumerate().for_each(
                        |(i, v)| {
                            v["power"] = Value::String(INIT_POWER.to_string());
                            v["name"] = Value::String(format!("node-{}", i));
                        },
                    );

                    fs::read_to_string(format!("{}/{}", tmp_home, genesis_file))
                        .c(d!())
                        .and_then(|g| serde_json::from_str::<Value>(&g).c(d!()))
                        .map(|mut g| {
                            g["validators"] = vs;
                            self.genesis = g.to_string();
                        })
                })
        };

        cmd::exec_output(&format!("tendermint init --home {}", &tmp_home))
            .c(d!())
            .and_then(|_| {
                TmConfig::load_toml_file(&format!("{}/config/config.toml", &tmp_home))
                    .map_err(|e| eg!(e))
            })
            .and_then(|cfg| cfg.genesis_file.to_str().map(|f| f.to_owned()).c(d!()))
            .and_then(gen)
            .and_then(|_| fs::remove_dir_all(tmp_home).c(d!()))
    }

    // apply genesis to all nodes in the same env
    fn apply_genesis(&mut self, n: Option<NodeId>) -> Result<()> {
        let nodes = n.map(|id| vec![id]).unwrap_or_else(|| {
            self.seeds
                .keys()
                .chain(self.nodes.keys())
                .copied()
                .collect()
        });

        for n in nodes.iter() {
            self.nodes
                .get(n)
                .or_else(|| self.seeds.get(n))
                .c(d!())
                .and_then(|n| {
                    TmConfig::load_toml_file(&format!("{}/config/config.toml", &n.home))
                        .map_err(|e| eg!(e))
                        .and_then(|cfg| {
                            PathBuf::from_str(&n.home)
                                .c(d!())
                                .map(|home| home.join(&cfg.genesis_file))
                        })
                        .and_then(|genesis_path| {
                            fs::write(genesis_path, &self.genesis).c(d!())
                        })
                })?;
        }

        Ok(())
    }

    fn print_info(&self) {
        println!("{}", pnk!(serde_json::to_string_pretty(self)));
    }

    // show the details of all existing ENVs
    fn info_all() -> Result<()> {
        for (idx, env) in Self::get_all_envs().c(d!())?.iter().enumerate() {
            println!("\x1b[31;01m====== ENV No.{} ======\x1b[00m", idx);
            Self::read_cfg(env)
                .c(d!())?
                .c(d!("BUG: env not found!"))?
                .print_info();
            println!();
        }
        Ok(())
    }

    // list the names of all existing ENVs
    fn list_all() -> Result<()> {
        let list = Self::get_all_envs().c(d!())?;

        if list.is_empty() {
            println!("\x1b[31;01mNo existing env!\x1b[00m");
        } else {
            println!("\x1b[31;01mEnv list:\x1b[00m");
            list.into_iter().for_each(|env| {
                println!("  {}", env);
            });
        }

        Ok(())
    }

    fn get_all_envs() -> Result<Vec<String>> {
        let mut list = vec![];

        let data_dir = format!("{}/envs", ENV_BASE_DIR);
        fs::create_dir_all(&data_dir).c(d!())?;

        for entry in fs::read_dir(&data_dir).c(d!())? {
            let entry = entry.c(d!())?;
            let path = entry.path();
            if path.is_dir() {
                let env = path.file_name().c(d!())?.to_string_lossy().into_owned();
                list.push(env);
            }
        }

        list.sort();

        Ok(list)
    }

    fn load_cfg(cfg: &EnvCfg) -> Result<Env> {
        Self::read_cfg(&cfg.name).c(d!()).and_then(|env| match env {
            Some(env) => Ok(env),
            None => {
                let msg = "ENV not found";
                println!();
                println!("********************");
                println!("\x1b[01mHINTS: \x1b[33;01m{}\x1b[00m", msg);
                println!("********************");
                Err(eg!(msg))
            }
        })
    }

    fn read_cfg(cfg_name: &str) -> Result<Option<Env>> {
        let p = format!("{}/envs/{}/config.json", ENV_BASE_DIR, cfg_name);
        match fs::read_to_string(&p) {
            Ok(d) => Ok(serde_json::from_str(&d).c(d!())?),
            Err(e) => match e.kind() {
                ErrorKind::NotFound => Ok(None),
                _ => Err(eg!(e)),
            },
        }
    }

    fn write_cfg(&self) -> Result<()> {
        serde_json::to_vec_pretty(self)
            .c(d!())
            .and_then(|d| fs::write(format!("{}/config.json", &self.home), d).c(d!()))
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct BankAccount {
    wallet_address: String,
    public_key: String,
    secret_key: String,
    mnemonic_words: String,
}

impl Default for BankAccount {
    fn default() -> Self {
        Self {
            wallet_address: BANK_ACCOUNT_ADDR.to_owned(),
            public_key: BANK_ACCOUNT_PUBKEY.to_owned(),
            secret_key: BANK_ACCOUNT_SECKEY.to_owned(),
            mnemonic_words: BANK_ACCOUNT_MNEMONIC.to_owned(),
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

impl TryFrom<&InitialValidator> for StakingValidator {
    type Error = Box<dyn ruc::RucError>;
    fn try_from(v: &InitialValidator) -> Result<StakingValidator> {
        Ok(StakingValidator {
            td_pubkey: base64::decode(&v.tendermint_pubkey).c(d!())?,
            td_addr: td_addr_to_bytes(&v.tendermint_addr).c(d!())?,
            td_power: 400_0000 * FRA,
            commission_rate: [1, 100],
            id: v.xfr_keypair.get_pk(),
            memo: Default::default(),
            kind: ValidatorKind::Initiator,
            signed_last_block: false,
            signed_cnt: 0,
            delegators: Default::default(),
        })
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Node {
    id: NodeId,
    tm_id: String,
    home: String,
    kind: Kind,
    ports: Ports,
}

impl Node {
    // - start node
    // - collect results
    // - update meta
    fn start(
        &mut self,
        host_ip: &str,
        block_itv: u8,
        evm_chain_id: u64,
        checkpoint_file: Option<&str>,
        abcid_extra_flags: &str,
    ) -> Result<()> {
        self.stop().c(d!())?;
        match unsafe { fork() } {
            Ok(ForkResult::Child) => {
                let mut cmd = format!(
                    "tendermint node --home {9} >>{9}/tendermint.log 2>&1 & \
                    EVM_CHAIN_ID={0} FINDORA_BLOCK_ITV={1} abcid \
                        --enable-query-service \
                        --enable-eth-api-service \
                        --tendermint-host {2} \
                        --tendermint-port {3} \
                        --abcid-port {4} \
                        --submission-service-port {5} \
                        --ledger-service-port {6} \
                        --evm-http-port {7} \
                        --evm-ws-port {8} \
                        --ledger-dir {9}/__findora__ \
                        --tendermint-node-key-config-path {9}/config/priv_validator_key.json \
                        {10}",
                    evm_chain_id,
                    block_itv,
                    host_ip,
                    self.ports.tm_rpc,
                    self.ports.app_abci,
                    self.ports.app_8669,
                    self.ports.app_8668,
                    self.ports.web3_http,
                    self.ports.web3_ws,
                    &self.home,
                    abcid_extra_flags,
                );
                if let Some(checkpoint) = checkpoint_file {
                    write!(cmd, r" --checkpoint-file {} \", checkpoint).unwrap();
                }
                write!(cmd, " >>{}/app.log 2>&1 &", &self.home).unwrap();

                pnk!(self.write_cmd_log(&cmd));
                pnk!(exec_spawn(&cmd));

                exit(0);
            }
            Ok(_) => Ok(()),
            Err(_) => Err(eg!("fork failed!")),
        }
    }

    fn stop(&self) -> Result<()> {
        let cmd = format!(
            "for i in \
                $(ps ax -o pid,args \
                    | grep '{}' \
                    | grep -v 'grep' \
                    | grep -Eo '^ *[0-9]+' \
                    | sed 's/ //g' \
                ); \
             do kill -9 $i; done",
            &self.home
        );
        let outputs = cmd::exec_output(&cmd).c(d!())?;
        let contents = format!("{}\n{}", &cmd, outputs.as_str());
        self.write_cmd_log(&contents).c(d!())
    }

    fn write_cmd_log(&self, cmd: &str) -> Result<()> {
        OpenOptions::new()
            .read(true)
            .write(true)
            .append(true)
            .create(true)
            .open(format!("{}/{}", &self.home, EXEC_LOG_NAME))
            .c(d!())
            .and_then(|mut f| {
                f.write_all(format!("\n\n[ {} ]\n", datetime!()).as_bytes())
                    .c(d!())
                    .and_then(|_| f.write_all(cmd.as_bytes()).c(d!()))
            })
    }

    // - release all occupied ports
    // - remove all files related to this node
    fn clean(&self) -> Result<()> {
        for port in [
            self.ports.web3_http,
            self.ports.web3_ws,
            self.ports.tm_rpc,
            self.ports.tm_p2p,
            self.ports.app_abci,
            self.ports.app_8669,
            self.ports.app_8668,
        ] {
            PortsCache::remove(port).c(d!())?;
        }

        fs::remove_dir_all(&self.home).c(d!())
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
enum Kind {
    Node,
    Seed,
}

// Active ports of a node
#[derive(Default, Debug, Clone, Deserialize, Serialize)]
struct Ports {
    web3_http: u16,
    web3_ws: u16,
    tm_p2p: u16,
    tm_rpc: u16,
    app_abci: u16,
    app_8669: u16,
    app_8668: u16,
}

#[derive(Debug)]
pub enum Ops {
    Create,
    Destroy,
    DestroyAll,
    Start,
    Stop,
    AddNode,
    DelNode,
    Info,
    InfoAll,
    List,
    Init,
    InitAll,
}

impl Default for Ops {
    fn default() -> Self {
        Self::Info
    }
}

// global alloctor for ports
fn alloc_ports(node_kind: &Kind, env_name: &str) -> Result<Ports> {
    // web3_http, web3_ws, tm_p2p, tm_rpc, app_abci, app_8669, app_8668
    const RESERVED_PORTS: [u16; 7] = [8545, 8546, 26656, 26657, 26658, 8669, 8668];

    let mut res = vec![];
    if matches!(node_kind, Kind::Node)
        && ENV_NAME_DEFAULT == env_name
        && !PortsCache::contains(RESERVED_PORTS[0]).c(d!())?
    {
        res = RESERVED_PORTS.to_vec();
    } else {
        let mut cnter = 10000;
        while RESERVED_PORTS.len() > res.len() {
            let p = 20000 + rand::random::<u16>() % (65535 - 20000);
            if !RESERVED_PORTS.contains(&p)
                && !RESERVED_PORTS.contains(&(p - 1))
                && !RESERVED_PORTS.contains(&(p + 1))
                && !PortsCache::contains(p).c(d!())?
                && !PortsCache::contains(p - 1).c(d!())?
                && !PortsCache::contains(p + 1).c(d!())?
                && port_is_free(p)
            {
                res.push(p);
            }
            cnter -= 1;
            alt!(0 == cnter, return Err(eg!("ports can not be allocated")))
        }
    }

    PortsCache::set(res.as_slice()).c(d!())?;

    Ok(Ports {
        web3_http: res[0],
        web3_ws: res[1],
        tm_p2p: res[2],
        tm_rpc: res[3],
        app_abci: res[4],
        app_8669: res[5],
        app_8668: res[6],
    })
}

fn port_is_free(port: u16) -> bool {
    info!(check_port(port)).is_ok()
}

fn check_port(port: u16) -> Result<()> {
    let fd = socket(
        AddressFamily::Inet,
        SockType::Datagram,
        SockFlag::empty(),
        None,
    )
    .c(d!())?;

    setsockopt(fd, sockopt::ReuseAddr, &true)
        .c(d!())
        .and_then(|_| setsockopt(fd, sockopt::ReusePort, &true).c(d!()))
        .and_then(|_| bind(fd, &SockaddrIn::new(0, 0, 0, 0, port)).c(d!()))
        .and_then(|_| close(fd).c(d!()))
}

#[derive(Debug, Serialize, Deserialize)]
struct PortsCache {
    file_path: String,
    port_set: BTreeSet<u16>,
}

impl PortsCache {
    fn new() -> Self {
        Self {
            file_path: Self::file_path(),
            port_set: BTreeSet::new(),
        }
    }

    fn file_path() -> String {
        format!("{}/ports_cache", ENV_BASE_DIR)
    }

    fn load() -> Result<Self> {
        match fs::read_to_string(Self::file_path()) {
            Ok(c) => serde_json::from_str(&c).c(d!()),
            Err(e) => {
                if ErrorKind::NotFound == e.kind() {
                    Ok(Self::new())
                } else {
                    Err(e).c(d!())
                }
            }
        }
    }

    fn write(&self) -> Result<()> {
        serde_json::to_string(self)
            .c(d!())
            .and_then(|c| fs::write(&self.file_path, c).c(d!()))
    }

    fn contains(port: u16) -> Result<bool> {
        Self::load().c(d!()).map(|i| i.port_set.contains(&port))
    }

    fn set(ports: &[u16]) -> Result<()> {
        let mut i = Self::load().c(d!())?;
        for p in ports {
            i.port_set.insert(*p);
        }
        i.write().c(d!())
    }

    fn remove(port: u16) -> Result<()> {
        let mut i = Self::load().c(d!())?;
        i.port_set.remove(&port);
        i.write().c(d!())
    }
}

fn exec_spawn(cmd: &str) -> Result<()> {
    let cmd = format!("ulimit -n 100000; {}", cmd);
    Command::new("bash")
        .arg("-c")
        .arg(cmd)
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .c(d!())?
        .wait()
        .c(d!())
        .and_then(|exit_status| {
            if let Some(code) = exit_status.code() {
                if 0 == code {
                    return Ok(());
                }
            }
            Err(eg!("{}", exit_status))
        })
}
