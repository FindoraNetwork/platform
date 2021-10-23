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
        sys::signal::{kill as nixkill, Signal},
        unistd::{fork, ForkResult, Pid as NixPid},
    },
    parking_lot::Mutex,
    ruc::{cmd, *},
    serde::{Deserialize, Serialize},
    std::{collections::HashMap, fs, process, str::FromStr, sync::Arc},
    tendermint::{
        config::TendermintConfig as TdConfig, net::Address as TdAddr,
        Timeout as TdTimeout,
    },
};

lazy_static::lazy_static! {
    static ref PORT_DB: Arc<Mutex<Mapxnk<u16, bool>>> =
        Arc::new(Mutex::new(new_mapxnk!(format!("{}/ports", ENV_BASE_DIR))));
}

type Pid = i32;

pub struct EnvCfg {
    // the name of this env
    pub name: String,
    pub ops: Ops,
    // set it when it is not zero
    pub block_itv: u32,
    // set it when it is not zero
    pub unbond_blocks: u32,
}

impl EnvCfg {
    pub fn run(&self) -> Result<()> {
        match self.ops {
            Ops::Create => Env::create(self).c(d!()),
            Ops::Destroy => Env::load_from_file(self)
                .c(d!())
                .and_then(|env| env.destroy().c(d!())),
            Ops::Start => Env::load_from_file(self)
                .c(d!())
                .and_then(|mut env| env.start().c(d!())),
            Ops::Stop => Env::load_from_file(self)
                .c(d!())
                .and_then(|env| env.stop().c(d!())),
            Ops::AddNode => Env::load_from_file(self)
                .c(d!())
                .and_then(|mut env| env.add_node().c(d!())),
            Ops::DelNode => Env::load_from_file(self)
                .c(d!())
                .and_then(|mut env| env.del_node().c(d!())),
            Ops::RunTest => Env::load_from_file(self)
                .c(d!())
                .and_then(|env| env.run_test().c(d!())),
        }
    }
}

pub enum Ops {
    Create,
    Destroy,
    Start,
    Stop,
    AddNode,
    DelNode,
    RunTest,
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
            _ => Err(eg!("Invalid operations")),
        }
    }
}

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
struct Env {
    // the name of this env
    name: String,
    // data path of this env
    path: String,

    seed_nodes: HashMap<u32, Node>,
    full_nodes: HashMap<u32, Node>,
    validator_nodes: HashMap<u32, Node>,

    // the latest/max id of current nodes
    latest_id: u32,
}

impl Env {
    fn create(cfg: &EnvCfg) -> Result<()> {
        let mut env = Env {
            name: cfg.name.clone(),
            path: format!("{}/{}", ENV_BASE_DIR, &cfg.name),
            ..Self::default()
        };

        fs::create_dir_all(&env.path).c(d!())?;

        let mut id;
        for _ in 0..3 {
            id = env.next_node_id();
            env.validator_nodes.insert(
                id,
                Node {
                    id,
                    kind: Kind::Validator,
                    ..Default::default()
                },
            );
        }

        id = env.next_node_id();
        env.full_nodes.insert(
            id,
            Node {
                id,
                kind: Kind::Full,
                ..Default::default()
            },
        );

        id = env.next_node_id();
        env.seed_nodes.insert(
            id,
            Node {
                id,
                kind: Kind::Seed,
                ..Default::default()
            },
        );

        env.start()
            .c(d!())
            .and_then(|_| env.write_to_file().c(d!()))
    }

    // destroy all running processes, delete all their data
    fn destroy(&self) -> Result<()> {
        self.stop()
            .c(d!())
            .and_then(|_| fs::remove_dir_all(&self.path).c(d!()))
    }

    // run all nodes
    fn start(&mut self) -> Result<()> {
        for id in self
            .validator_nodes
            .keys()
            .copied()
            .collect::<Vec<_>>()
            .into_iter()
        {
            self.add_node_x(id, Kind::Validator).c(d!())?;
        }

        for id in self
            .full_nodes
            .keys()
            .copied()
            .collect::<Vec<_>>()
            .into_iter()
        {
            self.add_node_x(id, Kind::Full).c(d!())?;
        }

        for id in self
            .seed_nodes
            .keys()
            .copied()
            .collect::<Vec<_>>()
            .into_iter()
        {
            self.add_node_x(id, Kind::Seed).c(d!())?;
        }

        Ok(())
    }

    fn stop(&self) -> Result<()> {
        self.validator_nodes
            .values()
            .chain(self.full_nodes.values())
            .chain(self.seed_nodes.values())
            .map(|n| n.stop().c(d!()))
            .collect::<Result<Vec<_>>>()
            .map(|_| ())
    }

    fn next_node_id(&mut self) -> u32 {
        self.latest_id = self.latest_id.overflowing_add(1).0;
        self.latest_id
    }

    fn add_node(&mut self) -> Result<()> {
        let id = self.next_node_id();
        let kind = Kind::Validator;
        self.add_node_x(id, kind).c(d!())
    }

    // - allocate ports
    // - change configs
    // - start processes
    // - collect results, update meta
    fn add_node_x(&mut self, id: u32, kind: Kind) -> Result<()> {
        let ports = alloc_ports(&kind, &self.name)
            .c(d!())
            .and_then(|ports| Ports::try_from(ports.as_slice()).c(d!()))?;

        let path = format!("{}/{}", self.path, id);
        fs::create_dir_all(&path).c(d!())?;

        let cfg_path = format!("{}/config/conig.toml", &path);

        let mut cfg = TdConfig::load_toml_file(&cfg_path).or_else(|_| {
            cmd::exec_output(&format!("tendermint init --home {}", &path))
                .c(d!())
                .and_then(|_| TdConfig::load_toml_file(&cfg_path).map_err(|e| eg!(e)))
        })?;

        // TODO: custom config for each node
        cfg.p2p.addr_book_strict = false;
        cfg.p2p.seeds = vec![];
        cfg.p2p.laddr =
            TdAddr::from_str(&format!("tcp://0.0.0.0:{}", ports.tendermint_p2p))
                .map_err(|e| eg!(e))?;
        cfg.consensus.timeout_commit = TdTimeout::from_str("2s").map_err(|e| eg!(e))?;

        toml::to_vec(&cfg)
            .c(d!())
            .and_then(|c| fs::write(cfg_path, c).c(d!()))?;

        // TODO:
        // - fork new processes to run nodes,
        // - set a real ppid

        match unsafe { fork() } {
            Ok(ForkResult::Parent { child, .. }) => {
                let node = Node {
                    id,
                    path,
                    kind,
                    ports,
                    ppid: child.as_raw() as Pid,
                };
                match kind {
                    Kind::Validator => self.validator_nodes.insert(id, node),
                    Kind::Full => self.full_nodes.insert(id, node),
                    Kind::Seed => self.seed_nodes.insert(id, node),
                };
                Ok(())
            }
            Ok(ForkResult::Child) => {
                // TODO: run nodes
                ctrlc::set_handler(move || {
                    info_omit!(kill_grp());
                })
                .unwrap();
                process::exit(0);
            }
            Err(_) => Err(eg!("fork failed!")),
        }
    }

    fn del_node(&mut self) -> Result<()> {
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

    fn load_from_file(cfg: &EnvCfg) -> Result<Env> {
        let path = format!("{}/{}/config.json", ENV_BASE_DIR, &cfg.name);
        fs::read_to_string(path)
            .c(d!())
            .and_then(|d| serde_json::from_str(&d).c(d!()))
    }

    fn write_to_file(&self) -> Result<()> {
        let path = format!("{}/{}/config.json", ENV_BASE_DIR, &self.name);
        serde_json::to_vec(self)
            .c(d!())
            .and_then(|d| fs::write(path, d).c(d!()))
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Node {
    id: u32,
    path: String,
    kind: Kind,
    ports: Ports,
    ppid: Pid,
}

impl Node {
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
            .into_iter()
            .for_each(|port| {
                db.remove(&port);
            });
        })
    }

    fn delete(self) -> Result<()> {
        fs::remove_dir_all(self.path).c(d!())
    }
}

impl Default for Node {
    fn default() -> Self {
        Node {
            id: 0,
            path: "".to_owned(),
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

// ports of a node
#[derive(Default, Debug, Clone, Deserialize, Serialize)]
struct Ports {
    query: u16,
    submission: u16,
    tendermint_rpc: u16,
    tendermint_p2p: u16,
    abci: u16,
}

impl TryFrom<&[u16]> for Ports {
    type Error = Box<dyn ruc::RucError>;
    fn try_from(ports: &[u16]) -> Result<Self> {
        alt!(5 != ports.len(), return Err(eg!()));
        Ok(Ports {
            query: ports[0],
            submission: ports[1],
            tendermint_rpc: ports[2],
            tendermint_p2p: ports[3],
            abci: ports[4],
        })
    }
}

fn kill(pid: Pid) -> Result<()> {
    nixkill(NixPid::from_raw(pid), Signal::SIGINT).c(d!())
}

fn kill_grp() -> Result<()> {
    kill(0).c(d!())
}

// global alloctor for ports
fn alloc_ports(node_kind: &Kind, env_name: &str) -> Result<Vec<u16>> {
    const RESERVED_PORTS: [u16; 5] = [8668, 8669, 26656, 26657, 26658];

    if matches!(node_kind, Kind::Full) && ENV_NAME_DEFAULT == env_name {
        return Ok(RESERVED_PORTS.to_vec());
    }

    let mut res = vec![];
    let mut cnter = 10000;
    let db = PORT_DB.lock();
    while 4 > res.len() {
        let p = 26659 + rand::random::<u16>() % (65535 - 26659);
        if db.get(&p).is_none() {
            res.push(p);
        }
        cnter -= 1;
        alt!(0 == cnter, return Err(eg!("ports can not be allocated")))
    }

    Ok(res)
}
