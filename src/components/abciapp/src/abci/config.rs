use global_cfg::CFG;
use ruc::*;
use serde::Deserialize;
use std::{convert::TryFrom, fs, path::Path};

#[derive(Debug)]
pub struct ABCIConfig {
    pub tendermint_host: String,
    pub tendermint_port: u16,
    pub submission_port: u16,
    pub ledger_port: u16,
    pub query_port: u16,
}

#[derive(Deserialize)]
pub struct ABCIConfigStr {
    pub tendermint_host: String,
    pub tendermint_port: String,
    pub submission_port: String,
    pub ledger_port: String,
}

impl TryFrom<ABCIConfigStr> for ABCIConfig {
    type Error = Box<dyn RucError>;
    fn try_from(cfg: ABCIConfigStr) -> Result<Self> {
        let ledger_port = cfg.ledger_port.parse::<u16>().c(d!())?;
        let query_port = ledger_port - 1;
        Ok(ABCIConfig {
            tendermint_host: cfg.tendermint_host,
            tendermint_port: cfg.tendermint_port.parse::<u16>().c(d!())?,
            submission_port: cfg.submission_port.parse::<u16>().c(d!())?,
            ledger_port,
            query_port,
        })
    }
}

impl ABCIConfig {
    pub fn from_env() -> Result<ABCIConfig> {
        let tendermint_host = CFG.tendermint_host.to_owned();
        let tendermint_port = CFG.tendermint_port;

        // client ------> abci(host, port, for submission)
        let submission_port = CFG.submission_service_port;

        // client ------> abci(host, port, for ledger access)
        let ledger_port = CFG.ledger_service_port;

        let query_port = ledger_port - 1;

        Ok(ABCIConfig {
            tendermint_host,
            tendermint_port,
            submission_port,
            ledger_port,
            query_port,
        })
    }

    pub fn from_file() -> Result<ABCIConfig> {
        let config_path = Path::new(&CFG.ledger_dir).join("abci.toml");
        let file_contents = fs::read_to_string(config_path).c(d!())?;
        let toml_string = toml::from_str::<ABCIConfigStr>(&file_contents).c(d!())?;
        let config = ABCIConfig::try_from(toml_string).c(d!())?;
        Ok(config)
    }
}

pub(crate) mod global_cfg {
    #[cfg(not(test))]
    use clap::{crate_authors, App, Arg, ArgGroup, ArgMatches, SubCommand};
    use lazy_static::lazy_static;
    use ruc::*;
    #[cfg(not(test))]
    use std::env;

    use crate::abci::init::InitMode;

    lazy_static! {
        /// Global config.
        pub static ref CFG: Config = pnk!(get_config());
    }

    #[derive(Default)]
    pub struct Config {
        pub tendermint_host: String,
        pub tendermint_port: u16,
        pub submission_service_port: u16,
        pub ledger_service_port: u16,
        pub enable_ledger_service: bool,
        pub enable_query_service: bool,
        pub tendermint_node_self_addr: Option<String>,
        pub tendermint_node_key_config_path: Option<String>,
        pub ledger_dir: String,
        pub tendermint_home: String,
        pub tendermint_config: Option<String>,
        pub command: String,
        pub init_mode: InitMode,
    }

    #[cfg(test)]
    fn get_config() -> Result<Config> {
        Ok(Config::default())
    }

    #[cfg(not(test))]
    fn get_config() -> Result<Config> {
        let matches = {
            let node = SubCommand::with_name("node")
                .about("Start findora node.")
                .arg_from_usage("-c, --config=[FILE] 'Path to $TMHOM/config/config.toml'")
                .arg_from_usage("-H, --tendermint-host=[Tendermint Node IP]")
                .arg_from_usage("-P, --tendermint-port=[Tendermint Node Port]")
                .arg_from_usage("--submission-service-port=[Submission Service Port]")
                .arg_from_usage("--ledger-service-port=[Ledger Service Port]")
                .arg_from_usage("-l, --enable-ledger-service")
                .arg_from_usage("-q, --enable-query-service")
                .arg_from_usage("--tendermint-node-self-addr=[Address] 'the address of your tendermint node, in upper-hex format'")
                .arg_from_usage("--tendermint-node-key-config-path=[Path] 'such as: ${HOME}/.tendermint/config/priv_validator_key.json'")
                .arg_from_usage("-d, --ledger-dir=[Path]")
                .arg_from_usage(
                    "-b, --base-dir=[DIR] 'The root directory for tendermint config, aka $TENDERMINT_HOME'",
                );

            let init = SubCommand::with_name("init")
                .about("Initialize the configurations of findorad")
                .arg_from_usage("--dev-net 'Initialize for Findora Local DevNet.'")
                .arg_from_usage("--test-net 'Initialize for Findora TestNet.'")
                .arg_from_usage("--main-net 'Initialize for Findora MainNet.'")
                .arg_from_usage("--qa01-net 'Initialize for Findora QA01.'")
                .group(ArgGroup::with_name("environment").args(&["dev-net", "test-net", "main-net", "qa01-net"]))
                .arg_from_usage(
                    "-b, --base-dir=[DIR] 'The root directory for tendermint config, aka $TENDERMINT_HOME'",
                );

            App::new("findorad")
                .version(env!("VERGEN_SHA"))
                .author(crate_authors!())
                .about("An ABCI node implementation of FindoraNetwork.")
                .subcommand(node)
                .subcommand(init)
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
        let els = m.is_present("enable-ledger-service")
            || env::var("ENABLE_LEDGER_SERVICE").is_ok();
        let eqs = m.is_present("enable-query-service")
            || env::var("ENABLE_QUERY_SERVICE").is_ok();
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

        let init_mode = if m.is_present("dev-net") {
            InitMode::Dev
        } else if m.is_present("test-net") {
            InitMode::Testnet
        } else if m.is_present("main-net") {
            InitMode::Mainnet
        } else if m.is_present("qa01-net") {
            InitMode::Qa01
        } else {
            InitMode::Dev
        };

        let res = Config {
            tendermint_host: th,
            tendermint_port: tp,
            submission_service_port: ssp,
            ledger_service_port: lsp,
            enable_ledger_service: els,
            enable_query_service: eqs,
            tendermint_node_self_addr: tnsa,
            tendermint_node_key_config_path: tnkcp,
            ledger_dir: ld,
            command: cmd.to_owned(),
            tendermint_config: tcfg,
            tendermint_home: tdir,
            init_mode,
        };

        Ok(res)
    }

    #[cfg(not(test))]
    fn print_version(m: ArgMatches) {
        if m.is_present("version") {
            println!("{}", env!("VERGEN_SHA"));
            std::process::exit(0);
        }
    }
}
