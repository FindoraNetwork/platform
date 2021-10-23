//!
//! # `stt env` SubCommand
//!
//! - make sure all names and ports are unique
//!     - keep a meta file in ${ENV_BASE_DIR}
//! - write ports to the running-dir of every env
//!

mod test;

use {
    super::{ENV_BASE_DIR, ENV_NAME_DEFAULT},
    ledger::store::fbnc::{new_mapxnk, Mapxnk},
    nix::{
        sys::{
            signal::{kill as nixkill, Signal},
            socket::{
                bind, setsockopt, socket, sockopt, AddressFamily, InetAddr, IpAddr,
                SockAddr, SockFlag, SockType,
            },
        },
        unistd::{close, fork, ForkResult, Pid as NixPid},
    },
    parking_lot::Mutex,
    ruc::{cmd, *},
    serde::{Deserialize, Serialize},
    serde_json::Value,
    std::{
        collections::BTreeMap,
        fs,
        path::PathBuf,
        process::{self, Command, Stdio},
        str::FromStr,
        sync::Arc,
    },
    tendermint::{
        config::{PrivValidatorKey as TmValidatorKey, TendermintConfig as TmConfig},
        validator::Info as TmValidator,
        vote::Power as TmPower,
    },
    toml_edit::{value as toml_value, Document},
};

lazy_static::lazy_static! {
    static ref PORT_DB: Arc<Mutex<Mapxnk<u16, bool>>> =
        Arc::new(Mutex::new(new_mapxnk!(format!("{}/ports", ENV_BASE_DIR))));
}

type Pid = i32;
type NodeId = u32;

#[derive(Default)]
pub struct EnvCfg {
    // the name of this env
    pub name: String,
    // which operation to trigger
    pub ops: Ops,
    // set it when it is not zero
    pub block_itv: u32,
    // set it when it is not zero
    pub unbond_blocks: u32,
}

impl EnvCfg {
    pub fn exec(&self) -> Result<Option<Env>> {
        match self.ops {
            Ops::Create => Env::create(self).c(d!()).map(Some),
            Ops::Destroy => Env::load_cfg(self)
                .c(d!())
                .and_then(|env| env.destroy().c(d!()))
                .map(|_| None),
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
            Ops::RunTest => Env::load_cfg(self)
                .c(d!())
                .and_then(|env| env.run_test().c(d!()))
                .map(|_| None),
            Ops::Info => Env::load_cfg(self).c(d!()).map(|env| {
                env.print_info();
                Some(env)
            }),
        }
    }
}

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct Env {
    // the name of this env
    name: String,
    // data path of this env
    home: String,
    // the contents of `genesis.json` of all nodes
    genesis: Vec<u8>,

    seed_nodes: BTreeMap<NodeId, Node>,
    pub full_nodes: BTreeMap<NodeId, Node>,
    validator_nodes: BTreeMap<NodeId, Node>,

    // the latest/max id of current nodes
    latest_id: NodeId,
}

impl Env {
    // - initilize a new env
    // - `genesis.json` will be created
    fn create(cfg: &EnvCfg) -> Result<Env> {
        let mut env = Env {
            name: cfg.name.clone(),
            home: format!("{}/{}", ENV_BASE_DIR, &cfg.name),
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
        add_initial_nodes!(Full);
        for _ in 0..3 {
            add_initial_nodes!(Validator);
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
            self.seed_nodes
                .keys()
                .chain(self.full_nodes.keys())
                .chain(self.validator_nodes.keys())
                .copied()
                .collect()
        });

        self.update_seeds()
            .c(d!())
            .and_then(|_| self.write_cfg().c(d!()))?;

        for i in ids.iter() {
            if let Some(n) = self.validator_nodes.get_mut(i) {
                n.start().c(d!())?;
            } else if let Some(n) = self.full_nodes.get_mut(i) {
                n.start().c(d!())?;
            } else if let Some(n) = self.seed_nodes.get_mut(i) {
                n.start().c(d!())?;
            } else {
                return Err(eg!("not exist"));
            }
        }

        Ok(())
    }

    // - stop all processes
    // - release all occupied ports
    fn stop(&self) -> Result<()> {
        self.validator_nodes
            .values()
            .chain(self.full_nodes.values())
            .chain(self.seed_nodes.values())
            .map(|n| n.stop().c(d!()))
            .collect::<Result<Vec<_>>>()
            .map(|_| ())
    }

    // destroy all nodes
    // - stop all running processes
    // - delete the data of every nodes
    fn destroy(&self) -> Result<()> {
        self.stop().c(d!()).and_then(|_| {
            sleep_ms!(10);
            fs::remove_dir_all(&self.home).c(d!())
        })
    }

    // seed nodes and full nodes are kept by system for now,
    // so only the validator nodes can be added on demand
    fn attach_node(&mut self) -> Result<()> {
        let id = self.next_node_id();
        let kind = Kind::Validator;
        self.alloc_resources(id, kind)
            .c(d!())
            .and_then(|_| self.apply_genesis(Some(id)).c(d!()))
            .and_then(|_| self.start(Some(id)).c(d!()))
    }

    fn kick_node(&mut self) -> Result<()> {
        self.validator_nodes
            .keys()
            .copied()
            .next()
            .c(d!())
            .and_then(|k| self.validator_nodes.remove(&k).c(d!()))
            .and_then(|n| n.stop().c(d!()).and_then(|_| n.delete().c(d!())))
    }

    fn run_test(&self) -> Result<()> {
        test::run(self).c(d!())
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

        cfg["p2p"]["addr_book_strict"] = toml_value(false);
        cfg["p2p"]["allow_duplicate_ip"] = toml_value(true);
        cfg["p2p"]["pex"] = toml_value(true);
        cfg["p2p"]["persistent_peers_max_dial_period"] = toml_value("3s");

        cfg["consensus"]["timeout_propose"] = toml_value("3s");
        cfg["consensus"]["timeout_propose_delta"] = toml_value("500ms");
        cfg["consensus"]["timeout_prevote"] = toml_value("1s");
        cfg["consensus"]["timeout_prevote_delta"] = toml_value("500ms");
        cfg["consensus"]["timeout_precommit"] = toml_value("1s");
        cfg["consensus"]["timeout_precommit_delta"] = toml_value("500ms");
        cfg["consensus"]["timeout_commit"] = toml_value("1s");
        cfg["consensus"]["create_empty_blocks"] = toml_value(true);
        cfg["consensus"]["create_empty_blocks_interval"] = toml_value("0s");

        cfg["p2p"]["laddr"] =
            toml_value(format!("tcp://127.0.0.1:{}", ports.tendermint_p2p));
        cfg["rpc"]["laddr"] =
            toml_value(format!("tcp://127.0.0.1:{}", ports.tendermint_rpc));
        cfg["proxy_app"] = toml_value(format!("tcp://127.0.0.1:{}", ports.abci));
        cfg["moniker"] = toml_value(format!("{}-{}", &self.name, id));

        if matches!(kind, Kind::Seed) {
            cfg["p2p"]["seed_mode"] = toml_value(true)
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
            ppid: 0, // will be set in the `start` method
        };

        match kind {
            Kind::Validator => self.validator_nodes.insert(id, node),
            Kind::Full => self.full_nodes.insert(id, node),
            Kind::Seed => self.seed_nodes.insert(id, node),
        };

        // 4.
        fs::write(cfg_path, cfg.to_string()).c(d!())
    }

    fn update_seeds(&self) -> Result<()> {
        for n in self
            .validator_nodes
            .values()
            .chain(self.full_nodes.values())
        {
            let cfg_path = format!("{}/config/config.toml", &n.home);
            let mut cfg = fs::read_to_string(&cfg_path)
                .c(d!())
                .and_then(|c| c.parse::<Document>().c(d!()))?;
            cfg["p2p"]["seeds"] = toml_value(
                self.seed_nodes
                    .values()
                    .map(|n| {
                        format!("{}@127.0.0.1:{}", &n.tm_id, n.ports.tendermint_p2p)
                    })
                    .collect::<Vec<_>>()
                    .join(","),
            );
            fs::write(cfg_path, cfg.to_string()).c(d!())?;
        }

        Ok(())
    }

    // allocate unique IDs for nodes within the scope of an env
    fn next_node_id(&mut self) -> NodeId {
        self.latest_id = self.latest_id.overflowing_add(1).0;
        self.latest_id
    }

    // generate a new `genesis.json` based on the collection of validators
    fn gen_genesis(&mut self) -> Result<()> {
        let tmp_id = self.next_node_id();
        let tmp_home = format!("{}/{}", &self.home, tmp_id);

        let gen = |genesis_file: String| {
            self.validator_nodes
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
                        .map(|key| TmValidator::new(key.pub_key, TmPower::new(1)))
                })
                .collect::<Result<Vec<_>>>()
                .and_then(|vs| serde_json::to_value(&vs).c(d!()))
                .and_then(|mut vs| {
                    vs.as_array_mut().c(d!())?.iter_mut().enumerate().for_each(
                        |(i, v)| {
                            v["power"] = Value::String(100.to_string());
                            v["name"] = Value::String(format!("node-{}", i));
                        },
                    );
                    fs::read_to_string(format!("{}/{}", tmp_home, genesis_file))
                        .c(d!())
                        .and_then(|g| serde_json::from_str::<Value>(&g).c(d!()))
                        .map(|mut g| {
                            g["validators"] = vs;
                            self.genesis = g.to_string().into_bytes();
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
    }

    // apply genesis to all nodes in the same env
    fn apply_genesis(&mut self, n: Option<NodeId>) -> Result<()> {
        let nodes = n.map(|id| vec![id]).unwrap_or_else(|| {
            self.seed_nodes
                .keys()
                .chain(self.full_nodes.keys())
                .chain(self.validator_nodes.keys())
                .copied()
                .collect()
        });

        for n in nodes.iter() {
            self.validator_nodes
                .get(n)
                .or_else(|| self.full_nodes.get(n))
                .or_else(|| self.seed_nodes.get(n))
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

    fn load_cfg(cfg: &EnvCfg) -> Result<Env> {
        let p = format!("{}/{}/config.json", ENV_BASE_DIR, &cfg.name);
        fs::read_to_string(&p)
            .c(d!())
            .and_then(|d| serde_json::from_str(&d).c(d!()))
    }

    fn write_cfg(&self) -> Result<()> {
        serde_json::to_vec(self)
            .c(d!())
            .and_then(|d| fs::write(format!("{}/config.json", &self.home), d).c(d!()))
    }

    fn print_info(&self) {
        println!("Env name: {}", &self.name);
        println!("Env home: {}", &self.home);
        println!("Seed nodes: {:#?}", &self.seed_nodes);
        println!("Full nodes: {:#?}", &self.full_nodes);
        println!("Validator nodes: {:#?}", &self.validator_nodes);
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Node {
    id: NodeId,
    tm_id: String,
    home: String,
    kind: Kind,
    pub ports: Ports,
    ppid: Pid,
}

impl Node {
    // - start node
    // - collect results
    // - update meta
    fn start(&mut self) -> Result<()> {
        match unsafe { fork() } {
            Ok(ForkResult::Parent { child, .. }) => {
                self.ppid = child.as_raw() as Pid;
                Ok(())
            }
            Ok(ForkResult::Child) => {
                let cmd = format!(
                    "cd {0} && findorad node -q -b {0} -P {1} -p {2} -L {3} -S {4}",
                    &self.home,
                    self.ports.tendermint_rpc,
                    self.ports.abci,
                    self.ports.query,
                    self.ports.submission
                );
                Command::new("sh")
                    .arg("-c")
                    .arg(cmd)
                    .stdin(Stdio::null())
                    .stdout(Stdio::inherit())
                    .stderr(Stdio::inherit())
                    .spawn()
                    .c(d!())?
                    .wait()
                    .c(d!())
                    .map(|exit_status| println!("{}", exit_status))?;

                ctrlc::set_handler(move || {
                    info_omit!(kill_grp());
                })
                .unwrap();

                process::exit(0);
            }
            Err(_) => Err(eg!("fork failed!")),
        }
    }

    fn stop(&self) -> Result<()> {
        kill(self.ppid).c(d!()).map(|_| {
            let mut db = PORT_DB.lock();
            [
                self.ports.query,
                self.ports.submission,
                self.ports.tendermint_rpc,
                self.ports.tendermint_p2p,
                self.ports.abci,
            ]
            .iter()
            .for_each(|port| {
                db.remove(port);
            });
        })
    }

    fn delete(self) -> Result<()> {
        fs::remove_dir_all(self.home).c(d!())
    }
}

impl Default for Node {
    fn default() -> Self {
        Node {
            id: 0,
            tm_id: "".to_owned(),
            home: "".to_owned(),
            kind: Kind::Validator,
            ports: Ports::default(),
            ppid: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
enum Kind {
    Seed,
    Validator,
    Full,
}

/// Active ports of a node
#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct Ports {
    pub query: u16,
    pub submission: u16,
    tendermint_p2p: u16,
    pub tendermint_rpc: u16,
    abci: u16,
}

pub enum Ops {
    Create,
    Destroy,
    Start,
    Stop,
    AddNode,
    DelNode,
    RunTest,
    Info,
}

impl Default for Ops {
    fn default() -> Self {
        Self::Info
    }
}

impl Ops {
    pub fn from_string(ops: &str) -> Result<Self> {
        match ops.to_lowercase().as_str() {
            "create" => Ok(Self::Create),
            "destroy" => Ok(Self::Destroy),
            "start" => Ok(Self::Start),
            "stop" => Ok(Self::Stop),
            "addnode" => Ok(Self::AddNode),
            "delnode" => Ok(Self::DelNode),
            "runtest" => Ok(Self::RunTest),
            "info" => Ok(Self::Info),
            _ => Err(eg!("Invalid operations")),
        }
    }
}

fn kill(pid: Pid) -> Result<()> {
    nixkill(NixPid::from_raw(pid), Signal::SIGINT).c(d!())
}

fn kill_grp() -> Result<()> {
    nixkill(NixPid::from_raw(0), Signal::SIGKILL).c(d!())
}

// global alloctor for ports
fn alloc_ports(node_kind: &Kind, env_name: &str) -> Result<Ports> {
    const RESERVED_PORTS: [u16; 5] = [8668, 8669, 26656, 26657, 26658];

    let mut res = vec![];
    if matches!(node_kind, Kind::Full) && ENV_NAME_DEFAULT == env_name {
        res = RESERVED_PORTS.to_vec();
    } else {
        let mut cnter = 10000;
        let db = PORT_DB.lock();
        while RESERVED_PORTS.len() > res.len() {
            let p = 20000 + rand::random::<u16>() % (65535 - 20000);
            if db.get(&p).is_none() && check_port(p).is_ok() {
                res.push(p);
            }
            cnter -= 1;
            alt!(0 == cnter, return Err(eg!("ports can not be allocated")))
        }
    }

    Ok(Ports {
        query: res[0],
        submission: res[1],
        tendermint_p2p: res[2],
        tendermint_rpc: res[3],
        abci: res[4],
    })
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
        .and_then(|_| {
            bind(
                fd,
                &SockAddr::Inet(InetAddr::new(IpAddr::new_v4(0, 0, 0, 0), port)),
            )
            .c(d!())
        })
        .and_then(|_| close(fd).c(d!()))
}
