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

        let config = fs::read_to_string(path).c(d!())?;

        let config = if mode == InitMode::Dev {
            // If local single node, use default tendermint.
            config
        } else {
            orig_cfg
                .iter()
                .zip(target_cfg.iter())
                .fold(config, |acc, pair| acc.replace(pair.0, pair.1))
        };

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
        clap::{crate_authors, App, Arg, ArgGroup, ArgMatches, SubCommand},
        lazy_static::lazy_static,
        ruc::*,
        std::{env, process},
    };

    lazy_static! {
        pub static ref CFG: Config = pnk!(get_config());
    }

    #[derive(Default)]
    pub struct Config {
        pub tendermint_host: String,
        pub tendermint_port: u16,
        pub arc_history: (u16, Option<u16>),
        pub arc_fresh: bool,
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
        let matches = {
            let node = SubCommand::with_name("node")
                .about("Start findora node.")
                .arg_from_usage("-c, --config=[FILE] 'Path to $TMHOM/config/config.toml'")
                .arg_from_usage("--arc-history=[EVM archive node tracing history, format \"PERIOD,INTERVAL\" in days]")
                .arg_from_usage("--arc-fresh 'EVM archive node with fresh tracing history'")
                .arg_from_usage("-H, --tendermint-host=[Tendermint Node IP]")
                .arg_from_usage("-P, --tendermint-port=[Tendermint Node Port]")
                .arg_from_usage("--submission-service-port=[Submission Service Port]")
                .arg_from_usage("--ledger-service-port=[Ledger Service Port]")
                .arg_from_usage("-q, --enable-query-service")
                .arg_from_usage("--enable-eth-api-service")
                .arg_from_usage("--disable-eth-empty-blocks")
                .arg_from_usage("-N, --no-fast-sync")
                .arg_from_usage("--tendermint-node-self-addr=[Address] 'the address of your tendermint node, in upper-hex format'")
                .arg_from_usage("--tendermint-node-key-config-path=[Path] 'such as: ${HOME}/.tendermint/config/priv_validator_key.json'")
                .arg_from_usage("-d, --ledger-dir=[Path]")
                .arg_from_usage(
                    "-b, --base-dir=[DIR] 'The root directory for tendermint config, aka $TENDERMINT_HOME'",
                )
                .arg_from_usage("--enable-snapshot 'global switch for enabling snapshot functions'")
                .arg_from_usage("--snapshot-list 'list all available snapshots in the form of block height'")
                .arg_from_usage("--snapshot-target=[TargetPath] 'a data volume containing both ledger data and tendermint data'")
                .arg_from_usage("--snapshot-itv=[Iterval] 'interval between adjacent snapshots, default to 10 blocks'")
                .arg_from_usage("--snapshot-cap=[Capacity] 'the maximum number of snapshots that will be stored, default to 100'")
                .arg_from_usage("--snapshot-mode=[Mode] 'zfs/btrfs/external, will try a guess if missing'")
                .arg_from_usage("--snapshot-algo=[Algo] 'fair/fade, default to `fair`'")
                .arg_from_usage("--snapshot-rollback 'rollback to the last available snapshot'")
                .arg_from_usage("-r, --snapshot-rollback-to=[Height] 'rollback to a custom height, will try the closest smaller height if the target does not exist'")
                .arg_from_usage("--checkpoint-file=[Path]")
                .arg_from_usage("-R, --snapshot-rollback-to-exact=[Height] 'rollback to a custom height exactly, an error will be reported if the target does not exist'");

            let init = SubCommand::with_name("init")
                .about("Initialize the configurations of findorad")
                .arg_from_usage("--devnet 'Initialize for Findora Local DevNet.'")
                .arg_from_usage("--testnet 'Initialize for Findora TestNet.'")
                .arg_from_usage("--mainnet 'Initialize for Findora MainNet.'")
                .arg_from_usage("--qa01 'Initialize for Findora QA01.'")
                .arg_from_usage("--qa02 'Initialize for Findora QA02.'")
                .arg_from_usage("--qa03 'Initialize for Findora QA03.'")
                .group(ArgGroup::with_name("environment").args(&["devnet", "testnet", "mainnet", "qa01", "qa02", "qa03"]))
                .arg_from_usage(
                    "-b, --base-dir=[DIR] 'The root directory for tendermint config, aka $TENDERMINT_HOME'",
                );

            let pack = SubCommand::with_name("pack");

            App::new("findorad")
                .version(env!("VERGEN_SHA"))
                .author(crate_authors!())
                .about("An ABCI node implementation of FindoraNetwork.")
                .subcommand(node)
                .subcommand(init)
                .subcommand(pack)
                .arg(Arg::with_name("_a").long("ignored").hidden(true))
                .arg(Arg::with_name("_b").long("nocapture").hidden(true))
                .arg(Arg::with_name("_c").long("test-threads").hidden(true))
                .arg(Arg::with_name("INPUT").multiple(true).hidden(true))
                .get_matches()
        };

        let (cmd, sub_matches) = matches.subcommand();

        if sub_matches.is_none() {
            print_version(matches);
            return Err(eg!("no this command"));
        }

        let m = if let Some(m) = sub_matches {
            m
        } else {
            print_version(matches);
            return Err(eg!("no this command"));
        };

        let tcfg = m
            .value_of("config")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_CONFIG").ok());
        let tdir = m
            .value_of("base-dir")
            .map(|v| v.to_owned())
            .unwrap_or_else(|| {
                env::var("TENDERMINT_HOME").unwrap_or_else(|_| {
                    format!("{}/.tendermint", pnk!(env::var("HOME")))
                })
            });

        let th = m
            .value_of("tendermint-host")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_HOST").ok())
            .unwrap_or_else(|| "localhost".to_owned());
        let tp = m
            .value_of("tendermint-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_PORT").ok())
            .unwrap_or_else(|| "26657".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let arh = {
            let trace = m
                .value_of("arc-history")
                .map(|v| v.to_owned())
                .or_else(|| env::var("ARC_HISTORY").ok())
                .unwrap_or_else(|| "90,10".to_string())
                .trim()
                .to_owned();
            if trace.is_empty() {
                return Err(eg!("empty trace"));
            }
            if trace.contains(',') {
                let t = trace.split(',').collect::<Vec<_>>();
                let trace = t
                    .first()
                    .expect("missing trace period")
                    .parse::<u16>()
                    .c(d!("invalid trace period"))?;
                let interval = Some(
                    t.get(1)
                        .expect("missing trace interval")
                        .parse::<u16>()
                        .c(d!("invalid trace interval"))?,
                );
                (trace, interval)
            } else if !trace.is_empty() {
                let trace = trace.parse::<u16>().c(d!("invalid trace period"))?;
                (trace, None)
            } else {
                return Err(eg!("invalid trace"));
            }
        };
        let arf = m.is_present("arc-fresh");
        let ssp = m
            .value_of("submission-service-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("SUBMISSION_PORT").ok())
            .unwrap_or_else(|| "8669".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let lsp = m
            .value_of("ledger-service-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("LEDGER_PORT").ok())
            .unwrap_or_else(|| "8668".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let eqs = m.is_present("enable-query-service")
            || env::var("ENABLE_QUERY_SERVICE").is_ok();
        let eeas = m.is_present("enable-eth-api-service")
            || env::var("ENABLE_ETH_API_SERVICE").is_ok();
        let deeb = m.is_present("disable-eth-empty-blocks")
            || env::var("DISABLE_ETH_EMPTY_BLOCKS").is_ok();
        let nfs = m.is_present("no-fast-sync") || env::var("NO_FAST_SYNC").is_ok();
        let tnsa = m
            .value_of("tendermint-node-self-addr")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TD_NODE_SELF_ADDR").ok());
        let tnkcp = m
            .value_of("tendermint-node-key-config-path")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_NODE_KEY_CONFIG_PATH").ok());
        let ld = m
            .value_of("ledger-dir")
            .map(|v| v.to_owned())
            .unwrap_or_else(|| {
                env::var("LEDGER_DIR")
                    .unwrap_or_else(|_| format!("{}/__findora__", &tdir))
            });
        let cpf = m
            .value_of("checkpoint-file")
            .map(|v| v.to_owned())
            .or_else(|| env::var("CHECKPOINT_FILE").ok());

        let init_mode = if m.is_present("devnet") {
            InitMode::Dev
        } else if m.is_present("testnet") {
            InitMode::Testnet
        } else if m.is_present("mainnet") {
            InitMode::Mainnet
        } else if m.is_present("qa01") {
            InitMode::Qa01
        } else if m.is_present("qa02") {
            InitMode::Qa02
        } else if m.is_present("qa03") {
            InitMode::Qa03
        } else {
            InitMode::Dev
        };

        let res = Config {
            tendermint_host: th,
            tendermint_port: tp,
            arc_history: arh,
            arc_fresh: arf,
            submission_service_port: ssp,
            ledger_service_port: lsp,
            enable_query_service: eqs,
            enable_eth_api_service: eeas,
            disable_eth_empty_blocks: deeb,
            no_fast_sync: nfs,
            tendermint_node_self_addr: tnsa,
            tendermint_node_key_config_path: tnkcp,
            ledger_dir: ld,
            command: cmd.to_owned(),
            tendermint_config: tcfg,
            tendermint_home: tdir,
            checkpoint_file: cpf,
            init_mode,
            enable_snapshot: m.is_present("enable-snapshot")
                || env::var("ENABLE_BTM_SNAPSHOT").is_ok(),
            snapshot_list: m.is_present("snapshot-list"),
            snapshot_target: m.value_of("snapshot-target").map(|v| v.to_owned()),
            snapshot_itv: m.value_of("snapshot-itv").map(|v| v.to_owned()),
            snapshot_cap: m.value_of("snapshot-cap").map(|v| v.to_owned()),
            snapshot_mode: m.value_of("snapshot-mode").map(|v| v.to_owned()),
            snapshot_algo: m.value_of("snapshot-algo").map(|v| v.to_owned()),
            snapshot_rollback: m.is_present("snapshot-rollback"),
            snapshot_rollback_to: m
                .value_of("snapshot-rollback-to")
                .map(|v| v.to_owned()),
            snapshot_rollback_to_exact: m
                .value_of("snapshot-rollback-to-exact")
                .map(|v| v.to_owned()),
        };

        Ok(res)
    }

    fn print_version(m: ArgMatches) {
        if m.is_present("version") {
            println!("{}", env!("VERGEN_SHA"));
            process::exit(0);
        }
    }
}
