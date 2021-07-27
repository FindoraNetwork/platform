use global_cfg::CFG;
use ruc::*;
use serde_derive::Deserialize;
use std::{convert::TryFrom, env, fs, path::Path};

pub struct ABCIConfig {
    pub tendermint_host: String,
    pub tendermint_port: u16,
    pub abci_host: String,
    pub abci_port: u16,
    pub submission_port: u16,
    pub ledger_port: u16,
    pub query_port: u16,
}

#[derive(Deserialize)]
pub struct ABCIConfigStr {
    pub tendermint_host: String,
    pub tendermint_port: String,
    pub abci_host: String,
    pub abci_port: String,
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
            abci_host: cfg.abci_host,
            abci_port: cfg.abci_port.parse::<u16>().c(d!())?,
            submission_port: cfg.submission_port.parse::<u16>().c(d!())?,
            ledger_port,
            query_port,
        })
    }
}

impl ABCIConfig {
    pub fn from_env() -> Result<ABCIConfig> {
        // abci ----> tendermint(host, port)
        let tendermint_host = CFG.tendermint_host.to_owned();
        let tendermint_port = CFG.tendermint_port;

        // tendermint -------> abci(host, port)
        let abci_host = CFG.abci_host.to_owned();
        let abci_port = CFG.abci_port;

        // client ------> abci(host, port, for submission)
        let submission_port = CFG.submission_service_port;

        // client ------> abci(host, port, for ledger access)
        let ledger_port = CFG.ledger_service_port;

        let query_port = ledger_port - 1;

        Ok(ABCIConfig {
            tendermint_host,
            tendermint_port,
            abci_host,
            abci_port,
            submission_port,
            ledger_port,
            query_port,
        })
    }

    pub fn from_file() -> Result<ABCIConfig> {
        env::args()
            .nth(1)
            .map(|p| Path::new(&p).join("abci").join("abci.toml"))
            .ok_or_else(|| eg!())
            .and_then(|p| fs::read_to_string(p).c(d!()))
            .and_then(|contents| toml::from_str::<ABCIConfigStr>(&contents).c(d!()))
            .and_then(|cfg_str| ABCIConfig::try_from(cfg_str).c(d!()))
    }
}

pub(crate) mod global_cfg {
    #![deny(warnings)]

    use clap::{crate_authors, App, Arg, ArgMatches};
    use lazy_static::lazy_static;
    use ruc::*;
    use std::{env, process};

    lazy_static! {
        /// Global config.
        pub static ref CFG: Config = pnk!(get_config());

        static ref ABCI_HOST: Option<String> = env::var("ABCI_HOST").ok();
        static ref ABCI_PORT: Option<String> = env::var("ABCI_PORT").ok();
        static ref TENDERMINT_HOST: Option<String> = env::var("TENDERMINT_HOST").ok();
        static ref TENDERMINT_PORT: Option<String> = env::var("TENDERMINT_PORT").ok();
        static ref SERVER_HOST: Option<String> = env::var("SERVER_HOST").ok();
        static ref SUBMISSION_PORT: Option<String> = env::var("SUBMISSION_PORT").ok();
        static ref LEDGER_PORT: Option<String> = env::var("LEDGER_PORT").ok();
        static ref LEDGER_DIR: Option<String> = env::var("LEDGER_DIR").ok();
        static ref ENABLE_LEDGER_SERVICE: Option<String> = env::var("ENABLE_LEDGER_SERVICE").ok();
        static ref ENABLE_QUERY_SERVICE: Option<String> = env::var("ENABLE_QUERY_SERVICE").ok();
        static ref TD_NODE_SELF_ADDR: Option<String> = env::var("TD_NODE_SELF_ADDR").ok();
        static ref TENDERMINT_NODE_KEY_CONFIG_PATH: Option<String> = env::var("TENDERMINT_NODE_KEY_CONFIG_PATH").ok();

        static ref M: ArgMatches<'static> = App::new("abci_validator_node")
            .version(env!("VERGEN_SHA"))
            .author(crate_authors!())
            .about("An ABCI node implementation of FindoraNetwork.")
            .arg_from_usage("-v, --version")
            .arg_from_usage("-H, --tendermint-host=[Tendermint Node IP]")
            .arg_from_usage("-P, --tendermint-port=[Tendermint Node Port]")
            .arg_from_usage("--abci-host=[ABCI IP]")
            .arg_from_usage("--abci-port=[ABCI Port]")
            .arg_from_usage("--submission-service-port=[Submission Service Port]")
            .arg_from_usage("--ledger-service-port=[Ledger Service Port]")
            .arg_from_usage("-l, --enable-ledger-service")
            .arg_from_usage("-q, --enable-query-service")
            .arg_from_usage("--tendermint-node-self-addr=[Address] 'the address of your tendermint node, in upper-hex format'")
            .arg_from_usage("--tendermint-node-key-config-path=[Path] 'such as: ${HOME}/.tendermint/config/priv_validator_key.json'")
            .arg_from_usage("-d, --ledger-dir=[Path]")
            .arg(Arg::with_name("INPUT").hidden(true))
            .arg(Arg::with_name("_a").long("ignored").hidden(true))
            .arg(Arg::with_name("_b").long("nocapture").hidden(true))
            .arg(Arg::with_name("_c").long("test-threads").hidden(true))
            .get_matches();
    }

    pub struct Config {
        pub tendermint_host: &'static str,
        pub tendermint_port: u16,
        pub abci_host: &'static str,
        pub abci_port: u16,
        pub submission_service_port: u16,
        pub ledger_service_port: u16,
        pub enable_ledger_service: bool,
        pub enable_query_service: bool,
        pub tendermint_node_self_addr: Option<&'static str>,
        pub tendermint_node_key_config_path: Option<&'static str>,
        pub ledger_dir: Option<&'static str>,
    }

    fn get_config() -> Result<Config> {
        print_version();

        let th = M
            .value_of("tendermint-host")
            .or_else(|| TENDERMINT_HOST.as_deref())
            .unwrap_or("localhost");
        let tp = M
            .value_of("tendermint-port")
            .or_else(|| TENDERMINT_PORT.as_deref())
            .unwrap_or("26657")
            .parse::<u16>()
            .c(d!())?;
        let ah = M
            .value_of("abci-host")
            .or_else(|| ABCI_HOST.as_deref())
            .unwrap_or("0.0.0.0");
        let ap = M
            .value_of("abci-port")
            .or_else(|| ABCI_PORT.as_deref())
            .unwrap_or("26658")
            .parse::<u16>()
            .c(d!())?;
        let ssp = M
            .value_of("submission-service-port")
            .or_else(|| SUBMISSION_PORT.as_deref())
            .unwrap_or("8669")
            .parse::<u16>()
            .c(d!())?;
        let lsp = M
            .value_of("ledger-service-port")
            .or_else(|| LEDGER_PORT.as_deref())
            .unwrap_or("8668")
            .parse::<u16>()
            .c(d!())?;
        let els =
            M.is_present("enable-ledger-service") || ENABLE_LEDGER_SERVICE.is_some();
        let eqs = M.is_present("enable-query-service") || ENABLE_QUERY_SERVICE.is_some();
        let tnsa = M
            .value_of("tendermint-node-self-addr")
            .or_else(|| TD_NODE_SELF_ADDR.as_deref());
        let tnkcp = M
            .value_of("tendermint-node-key-config-path")
            .or_else(|| TENDERMINT_NODE_KEY_CONFIG_PATH.as_deref());
        let ld = M.value_of("ledger-dir").or_else(|| LEDGER_DIR.as_deref());

        let res = Config {
            tendermint_host: th,
            tendermint_port: tp,
            abci_host: ah,
            abci_port: ap,
            submission_service_port: ssp,
            ledger_service_port: lsp,
            enable_ledger_service: els,
            enable_query_service: eqs,
            tendermint_node_self_addr: tnsa,
            tendermint_node_key_config_path: tnkcp,
            ledger_dir: ld,
        };

        Ok(res)
    }

    fn print_version() {
        if M.is_present("version") {
            println!("{}", env!("VERGEN_SHA"));
            process::exit(0);
        }
    }
}
