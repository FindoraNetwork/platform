pub mod init {
    use {
        ruc::*,
        std::{fs, str},
    };

    const QA01_GENESIS_URL: &str = "https://dev-qa01.dev.findora.org:26657/genesis";
    const QA02_GENESIS_URL: &str = "https://dev-qa02.dev.findora.org:26657/genesis";
    const QA03_GENESIS_URL: &str = "https://dev-qa03.dev.findora.org:26657/genesis";
    const TESTNET_GENESIS_URL: &str =
        "https://prod-testnet.prod.findora.org:26657/genesis";
    const MAINNET_GENESIS_URL: &str =
        "https://prod-mainnet.prod.findora.org:26657/genesis";

    #[allow(dead_code)]
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum InitMode {
        Dev,
        Testnet,
        Mainnet,
        Qa01,
        Qa02,
        Qa03,
    }

    impl Default for InitMode {
        fn default() -> Self {
            InitMode::Mainnet
        }
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
            InitMode::Qa02 => save_genesis(QA02_GENESIS_URL, path)?,
            InitMode::Qa03 => save_genesis(QA03_GENESIS_URL, path)?,
            InitMode::Dev => {}
        }
        Ok(())
    }

    pub fn generate_tendermint_config(mode: InitMode, path: &str) -> Result<()> {
        let config = fs::read_to_string(path).c(d!())?;

        let orig_cfg = [
            "index_all_keys = false",
            "laddr = \"tcp://127.0.0.1:26657\"",
            "timeout_propose = \"3s\"",
            "timeout_propose_delta = \"500ms\"",
            "timeout_prevote = \"1s\"",
            "timeout_prevote_delta = \"500ms\"",
            "timeout_precommit = \"1s\"",
            "timeout_precommit_delta = \"500ms\"",
            "timeout_commit = \"1s\"",
            "recheck = true",
            "fast_sync = true",
            "size = 5000",
            "prometheus = false",
        ];

        let target_cfg = [
            "index_all_keys = true",
            "laddr = \"tcp://0.0.0.0:26657\"",
            "timeout_propose = \"8s\"",
            "timeout_propose_delta = \"100ms\"",
            "timeout_prevote = \"4s\"",
            "timeout_prevote_delta = \"100ms\"",
            "timeout_precommit = \"4s\"",
            "timeout_precommit_delta = \"100ms\"",
            "timeout_commit = \"15s\"",
            "recheck = false",
            "fast_sync = false",
            "size = 2000",
            "prometheus = true",
        ];

        let config = orig_cfg
            .iter()
            .zip(target_cfg.iter())
            .fold(config, |acc, pair| acc.replace(pair.0, pair.1));

        let result = match mode {
            InitMode::Mainnet => {
                config.replace(
                    "seeds = \"\"",
                    "seeds = \"bd518151ac767d5dd77e228d34f31c581140fbe0@prod-mainnet-us-west-2-seed-000-public.prod.findora.org:26656,03deb91289c430fecf6883caaf69c69bb66f7d8e@prod-mainnet-us-west-2-seed-001-public.prod.findora.org:26656\"",
                )
            }
            InitMode::Testnet => {
                config.replace(
                    "seeds = \"\"",
                    "seeds = \"bd518151ac767d5dd77e228d34f31c581140fbe0@prod-testnet-us-west-2-seed-000-public.prod.findora.org:26656,03deb91289c430fecf6883caaf69c69bb66f7d8e@prod-testnet-us-west-2-seed-001-public.prod.findora.org:26656\"",
                )
            }
            InitMode::Qa01 => {
                config.replace(
                    "seeds = \"\"",
                    "seeds = \"bd518151ac767d5dd77e228d34f31c581140fbe0@dev-qa01-us-west-2-seed-000-public.dev.findora.org:26656,03deb91289c430fecf6883caaf69c69bb66f7d8e@dev-qa01-us-west-2-seed-001-public.dev.findora.org:26656\"",
                )
            }
            InitMode::Qa02 => {
                config.replace(
                    "seeds = \"\"",
                    "seeds = \"bd518151ac767d5dd77e228d34f31c581140fbe0@dev-qa02-us-west-2-seed-000-public.dev.findora.org:26656,03deb91289c430fecf6883caaf69c69bb66f7d8e@dev-qa02-us-west-2-seed-001-public.dev.findora.org:26656\"",
                )
            }
            InitMode::Qa03 => {
                config.replace(
                    "seeds = \"\"",
                    "seeds = \"bd518151ac767d5dd77e228d34f31c581140fbe0@dev-qa03-us-west-2-seed-000-public.dev.findora.org:26656,03deb91289c430fecf6883caaf69c69bb66f7d8e@dev-qa03-us-west-2-seed-001-public.dev.findora.org:26656\"",
                )
            }
            InitMode::Dev => config,
        };
        fs::write(path, result).c(d!())?;

        Ok(())
    }
}

pub mod config {
    use {
        super::init::InitMode,
        clap::{arg, crate_authors, Arg, ArgGroup, Command},
        lazy_static::lazy_static,
        ruc::*,
        std::env,
    };

    lazy_static! {
        pub static ref CFG: Config = pnk!(get_config());
    }

    #[derive(Default)]
    pub struct Config {
        pub tendermint_host: String,
        pub tendermint_port: u16,
        pub submission_service_port: u16,
        pub ledger_service_port: u16,
        pub enable_query_service: bool,
        pub enable_eth_api_service: bool,
        pub disable_eth_empty_blocks: bool,
        pub no_fast_sync: bool,
        pub tendermint_node_self_addr: Option<String>,
        pub tendermint_node_key_config_path: Option<String>,
        pub ledger_dir: String,
        pub tendermint_home: String,
        pub tendermint_config: Option<String>,
        pub command: String,
        pub init_mode: InitMode,
        pub enable_snapshot: bool,
        pub snapshot_list: bool,
        pub checkpoint_file: Option<String>,
        pub snapshot_target: Option<String>,
        pub snapshot_itv: Option<String>,
        pub snapshot_cap: Option<String>,
        pub snapshot_mode: Option<String>,
        pub snapshot_algo: Option<String>,
        pub snapshot_rollback: bool,
        pub snapshot_rollback_to: Option<String>,
        pub snapshot_rollback_to_exact: Option<String>,
    }

    fn get_config() -> Result<Config> {
        let mut cmd = {
            let node = Command::new("node")
                .about("Start findora node.")
                .args([
                    arg!(-c --config <FILE> "Path to $TMHOM/config/config.toml"),
                    arg!(-H --"tendermint-host" <"Tendermint Node IP">),
                    arg!(-P --"tendermint-port" <"Tendermint Node Port">),
                    arg!(--"submission-service-port" <"Submission Service Port">),
                    arg!(--"ledger-service-port" <"Ledger Service Port">),
                    arg!(-q --"enable-query-service"),
                    arg!(--"enable-eth-api-service"),
                    arg!(--"disable-eth-empty-blocks"),
                    arg!(-N --"no-fast-sync"),
                    arg!(--"tendermint-node-self-addr" <"Address"> "the address of your tendermint node, in upper-hex format"),
                    arg!(--"tendermint-node-key-config-path" <"Path"> "such as: ${HOME}/.tendermint/config/priv_validator_key.json"),
                    arg!(-d --"ledger-dir" <"Path">),
                    arg!(-b --"base-dir" <"DIR"> "The root directory for tendermint config, aka $TENDERMINT_HOME."),
                    arg!(--"enable-snapshot" "global switch for enabling snapshot functions"),
                    arg!(--"snapshot-list" "list all available snapshots in the form of block height"),
                    arg!(--"snapshot-target" <"TargetPath"> "a data volume containing both ledger data and tendermint data"),
                    arg!(--"snapshot-itv" <"Iterval"> "interval between adjacent snapshots, default to 10 blocks"),
                    arg!(--"snapshot-cap" <"Capacity"> "the maximum number of snapshots that will be stored, default to 100"),
                    arg!(--"snapshot-mode" <"Mode"> "zfs/btrfs/external, will try a guess if missing"),
                    arg!(--"snapshot-algo" <"Algo"> "fair/fade, default to `fair`"),
                    arg!(--"snapshot-rollback" "rollback to the last available snapshot"),
                    arg!(-r --"snapshot-rollback-to" <"Height"> "rollback to a custom height, will try the closest smaller height if the target does not exist"),
                    arg!(-R --"snapshot-rollback-to-exact" <"Height"> "rollback to a custom height exactly, an error will be reported if the target does not exist"),
                ]);

            let init = Command::new("init")
                .about("Initialize the configurations of findorad")
                .args([
                    arg!(--devnet "Initialize for Findora Local DevNet."),
                    arg!(--testnet "Initialize for Findora TestNet."),
                    arg!(--mainnet "Initialize for Findora MainNet."),
                    arg!(--qa01 "Initialize for Findora QA01."),
                    arg!(--qa02 "Initialize for Findora QA02."),
                    arg!(--qa03 "Initialize for Findora QA03."),
                ])
                .group(ArgGroup::new("environment").args(&["devnet", "testnet", "mainnet", "qa01", "qa02", "qa03"]))
                .arg(
                    arg!(-b --"base-dir" <"DIR"> "The root directory for tendermint config, aka $TENDERMINT_HOME."),
                );

            let pack = Command::new("pack");

            Command::new("findorad")
                .version(env!("VERGEN_SHA"))
                .author(crate_authors!())
                .about("An ABCI node implementation of FindoraNetwork.")
                .subcommand(node)
                .subcommand(init)
                .subcommand(pack)
                .arg(Arg::new("_a").long("ignored").hide(true))
                .arg(Arg::new("_b").long("nocapture").hide(true))
                .arg(Arg::new("_c").long("test-threads").hide(true))
                .arg(Arg::new("INPUT").hide(true))
        };
        let matches = cmd.clone().get_matches();

        let mut tcfg = Default::default();
        let mut tdir = Default::default();
        let mut th = Default::default();
        let mut tp = Default::default();
        let mut ssp = Default::default();
        let mut lsp = Default::default();
        let mut eqs = Default::default();
        let mut eeas = Default::default();
        let mut deeb = Default::default();
        let mut nfs = Default::default();
        let mut tnsa = Default::default();
        let mut tnkcp = Default::default();
        let mut ld = Default::default();
        let mut cpf = Default::default();
        let mut init_mode = Default::default();
        let mut enable_snapshot = Default::default();
        let mut snapshot_list = Default::default();
        let mut snapshot_target = Default::default();
        let mut snapshot_itv = Default::default();
        let mut snapshot_cap = Default::default();
        let mut snapshot_mode = Default::default();
        let mut snapshot_algo = Default::default();
        let mut snapshot_rollback = Default::default();
        let mut snapshot_rollback_to = Default::default();
        let mut snapshot_rollback_to_exact = Default::default();

        let cmd_str = match matches.subcommand() {
            Some(("init", m)) => {
                init_mode = if m.get_flag("devnet") {
                    InitMode::Dev
                } else if m.get_flag("testnet") {
                    InitMode::Testnet
                } else if m.get_flag("mainnet") {
                    InitMode::Mainnet
                } else if m.get_flag("qa01") {
                    InitMode::Qa01
                } else if m.get_flag("qa02") {
                    InitMode::Qa02
                } else if m.get_flag("qa03") {
                    InitMode::Qa03
                } else {
                    InitMode::Dev
                };
                tdir = m
                    .get_one::<String>("base-dir")
                    .map(|v| v.to_owned())
                    .unwrap_or_else(|| {
                        env::var("TENDERMINT_HOME").unwrap_or_else(|_| {
                            format!("{}/.tendermint", pnk!(env::var("HOME")))
                        })
                    });
                "init"
            }
            Some(("node", m)) => {
                tcfg = m
                    .get_one::<String>("config")
                    .map(|v| v.to_owned())
                    .or_else(|| env::var("TENDERMINT_CONFIG").ok());
                tdir = m
                    .get_one::<String>("base-dir")
                    .map(|v| v.to_owned())
                    .unwrap_or_else(|| {
                        env::var("TENDERMINT_HOME").unwrap_or_else(|_| {
                            format!("{}/.tendermint", pnk!(env::var("HOME")))
                        })
                    });
                th = m
                    .get_one::<String>("tendermint-host")
                    .map(|v| v.to_owned())
                    .or_else(|| env::var("TENDERMINT_HOST").ok())
                    .unwrap_or_else(|| "localhost".to_owned());
                tp = m
                    .get_one::<String>("tendermint-port")
                    .map(|v| v.to_owned())
                    .or_else(|| env::var("TENDERMINT_PORT").ok())
                    .unwrap_or_else(|| "26657".to_owned())
                    .parse::<u16>()
                    .c(d!())?;
                ssp = m
                    .get_one::<String>("submission-service-port")
                    .map(|v| v.to_owned())
                    .or_else(|| env::var("SUBMISSION_PORT").ok())
                    .unwrap_or_else(|| "8669".to_owned())
                    .parse::<u16>()
                    .c(d!())?;
                lsp = m
                    .get_one::<String>("ledger-service-port")
                    .map(|v| v.to_owned())
                    .or_else(|| env::var("LEDGER_PORT").ok())
                    .unwrap_or_else(|| "8668".to_owned())
                    .parse::<u16>()
                    .c(d!())?;
                eqs = m.get_flag("enable-query-service")
                    || env::var("ENABLE_QUERY_SERVICE").is_ok();

                eeas = m.get_flag("enable-eth-api-service")
                    || env::var("ENABLE_ETH_API_SERVICE").is_ok();

                deeb = m.get_flag("disable-eth-empty-blocks")
                    || env::var("DISABLE_ETH_EMPTY_BLOCKS").is_ok();

                nfs = m.get_flag("no-fast-sync") || env::var("NO_FAST_SYNC").is_ok();

                tnsa = m
                    .get_one::<String>("tendermint-node-self-addr")
                    .map(|v| v.to_owned())
                    .or_else(|| env::var("TD_NODE_SELF_ADDR").ok());
                tnkcp = m
                    .get_one::<String>("tendermint-node-key-config-path")
                    .map(|v| v.to_owned())
                    .or_else(|| env::var("TENDERMINT_NODE_KEY_CONFIG_PATH").ok());
                ld = m
                    .get_one::<String>("ledger-dir")
                    .map(|v| v.to_owned())
                    .unwrap_or_else(|| {
                        env::var("LEDGER_DIR")
                            .unwrap_or_else(|_| format!("{}/__findora__", &tdir))
                    });
                cpf = m
                    .get_one::<String>("checkpoint-file")
                    .map(|v| v.to_owned())
                    .or_else(|| env::var("CHECKPOINT_FILE").ok());

                enable_snapshot = m.get_flag("enable-snapshot")
                    || env::var("ENABLE_BTM_SNAPSHOT").is_ok();
                snapshot_list = m.get_flag("snapshot-list");
                snapshot_target =
                    m.get_one::<String>("snapshot-target").map(|v| v.to_owned());
                snapshot_itv = m.get_one::<String>("snapshot-itv").map(|v| v.to_owned());
                snapshot_cap = m.get_one::<String>("snapshot-cap").map(|v| v.to_owned());
                snapshot_mode =
                    m.get_one::<String>("snapshot-mode").map(|v| v.to_owned());
                snapshot_algo =
                    m.get_one::<String>("snapshot-algo").map(|v| v.to_owned());
                snapshot_rollback = m.get_flag("snapshot-rollback");
                snapshot_rollback_to = m
                    .get_one::<String>("snapshot-rollback-to")
                    .map(|v| v.to_owned());
                snapshot_rollback_to_exact = m
                    .get_one::<String>("snapshot-rollback-to-exact")
                    .map(|v| v.to_owned());
                "node"
            }
            Some(("pack", _)) => "pack",
            Some((_, _)) => {
                cmd.print_help().unwrap();
                return Err(eg!("no this command"));
            }
            None => {
                cmd.print_help().unwrap();
                return Err(eg!("sub command not found"));
            }
        };

        let res = Config {
            tendermint_host: th,
            tendermint_port: tp,
            submission_service_port: ssp,
            ledger_service_port: lsp,
            enable_query_service: eqs,
            enable_eth_api_service: eeas,
            disable_eth_empty_blocks: deeb,
            no_fast_sync: nfs,
            tendermint_node_self_addr: tnsa,
            tendermint_node_key_config_path: tnkcp,
            ledger_dir: ld,
            command: cmd_str.to_owned(),
            tendermint_config: tcfg,
            tendermint_home: tdir,
            checkpoint_file: cpf,
            init_mode,
            enable_snapshot,
            snapshot_list,
            snapshot_target,
            snapshot_itv,
            snapshot_cap,
            snapshot_mode,
            snapshot_algo,
            snapshot_rollback,
            snapshot_rollback_to,
            snapshot_rollback_to_exact,
        };

        Ok(res)
    }
}
