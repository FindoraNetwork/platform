use global_cfg::CFG;
use ruc::*;
use serde::Deserialize;
use std::{convert::TryFrom, env, fs, path::Path};

#[derive(Debug)]
pub struct ABCIConfig {
    pub abci_host: String,
    pub abci_port: u16,
    pub tendermint_host: String,
    pub tendermint_port: u16,
    pub submission_port: u16,
    pub ledger_port: u16,
    pub query_port: u16,
    pub evm_http_port: u16,
    pub evm_ws_port: u16,
    pub ledger_dir: String,
}

#[derive(Deserialize)]
pub struct ABCIConfigStr {
    pub abci_host: String,
    pub abci_port: String,
    pub tendermint_host: String,
    pub tendermint_port: String,
    pub submission_port: String,
    pub ledger_port: String,
    pub evm_http_port: String,
    pub evm_ws_port: String,
    #[serde(skip)]
    pub ledger_dir: Option<String>,
}

impl TryFrom<ABCIConfigStr> for ABCIConfig {
    type Error = Box<dyn RucError>;
    fn try_from(cfg: ABCIConfigStr) -> Result<Self> {
        let ledger_port = cfg.ledger_port.parse::<u16>().c(d!())?;
        let query_port = ledger_port - 1;
        let evm_http_port = cfg.evm_http_port.parse::<u16>().c(d!())?;
        let evm_ws_port = cfg.evm_ws_port.parse::<u16>().c(d!())?;
        Ok(ABCIConfig {
            abci_host: cfg.abci_host,
            abci_port: cfg.abci_port.parse::<u16>().c(d!())?,
            tendermint_host: cfg.tendermint_host,
            tendermint_port: cfg.tendermint_port.parse::<u16>().c(d!())?,
            submission_port: cfg.submission_port.parse::<u16>().c(d!())?,
            ledger_port,
            query_port,
            evm_http_port,
            evm_ws_port,
            ledger_dir: cfg.ledger_dir.unwrap_or(pnk!(env::var("LEDGER_DIR"))),
        })
    }
}

impl ABCIConfig {
    pub fn from_env() -> Result<ABCIConfig> {
        let abci_host = CFG.abci_host.to_owned();
        let abci_port = CFG.abci_port;

        let tendermint_host = CFG.tendermint_host.to_owned();
        let tendermint_port = CFG.tendermint_port;

        // client ------> abci(host, port, for submission)
        let submission_port = CFG.submission_service_port;

        // client ------> abci(host, port, for ledger access)
        let ledger_port = CFG.ledger_service_port;

        let query_port = ledger_port - 1;

        Ok(ABCIConfig {
            abci_host,
            abci_port,
            tendermint_host,
            tendermint_port,
            submission_port,
            ledger_port,
            query_port,
            evm_http_port: CFG.evm_http_port,
            evm_ws_port: CFG.evm_ws_port,
            ledger_dir: CFG.ledger_dir.clone(),
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
    use clap::{crate_authors, App, Arg, ArgMatches};
    use lazy_static::lazy_static;
    use ruc::*;
    #[cfg(not(test))]
    use std::env;

    lazy_static! {
        /// Global config.
        pub static ref CFG: Config = pnk!(get_config());
    }

    #[derive(Default)]
    pub struct Config {
        pub abci_host: String,
        pub abci_port: u16,
        pub tendermint_host: String,
        pub tendermint_port: u16,
        pub submission_service_port: u16,
        pub ledger_service_port: u16,
        pub enable_query_service: bool,
        pub enable_eth_empty_blocks: bool,
        pub enable_eth_api_service: bool,
        pub evm_http_port: u16,
        pub evm_ws_port: u16,
        pub tendermint_node_self_addr: Option<String>,
        pub tendermint_node_key_config_path: Option<String>,
        pub ledger_dir: String,
    }

    #[cfg(test)]
    fn get_config() -> Result<Config> {
        Ok(Config::default())
    }

    #[cfg(not(test))]
    fn get_config() -> Result<Config> {
        let m = App::new("abcid")
                .version(env!("VERGEN_SHA"))
                .author(crate_authors!())
                .about("An ABCI node implementation of FindoraNetwork.")
                .arg_from_usage("--abcid-host=[ABCId IP]")
                .arg_from_usage("--abcid-port=[ABCId Port]")
                .arg_from_usage("--tendermint-host=[Tendermint IP]")
                .arg_from_usage("--tendermint-port=[Tendermint Port]")
                .arg_from_usage("--submission-service-port=[Submission Service Port]")
                .arg_from_usage("--ledger-service-port=[Ledger Service Port]")
                .arg_from_usage("-q, --enable-query-service")
                .arg_from_usage("--enable-eth-empty-blocks 'whether to generate empty ethereum blocks when no evm contract transaction'")
                .arg_from_usage("--enable-eth-api-service")
                .arg_from_usage("--evm-http-port=[EVM Web3 Http Port]")
                .arg_from_usage("--evm-ws-port=[EVM Web3 WS Port]")
                .arg_from_usage("--tendermint-node-self-addr=[Address] 'the address of your tendermint node, in upper-hex format'")
                .arg_from_usage("--tendermint-node-key-config-path=[Path] 'such as: ${HOME}/.tendermint/config/priv_validator_key.json'")
                .arg_from_usage("-d, --ledger-dir=[Path]")
                .arg(Arg::with_name("_a").long("ignored").hidden(true))
                .arg(Arg::with_name("_b").long("nocapture").hidden(true))
                .arg(Arg::with_name("_c").long("test-threads").hidden(true))
                .arg(Arg::with_name("INPUT").multiple(true).hidden(true))
                .get_matches();

        print_version(&m);

        let ah = m
            .value_of("abcid-host")
            .map(|v| v.to_owned())
            .or_else(|| env::var("ABCI_HOST").ok())
            .unwrap_or_else(|| "0.0.0.0".to_owned());
        let ap = m
            .value_of("abcid-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("ABCI_PORT").ok())
            .unwrap_or_else(|| "26658".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let th = m
            .value_of("tendermint-host")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_HOST").ok())
            .unwrap_or_else(|| "0.0.0.0".to_owned());
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
                env::var("LEDGER_DIR").unwrap_or_else(|_| {
                    format!("{}/.tendermint/__findora__", pnk!(env::var("HOME")))
                })
            });

        let eeb = m.is_present("enable-eth-empty-blocks")
            || env::var("ENABLE_ETH_EMPTY_BLOCKS").is_ok();
        let eas = m.is_present("enable-eth-api-service")
            || env::var("ENABLE_ETH_API_SERVICE").is_ok();
        let ehp = m
            .value_of("evm-http-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("EVM_HTTP_PORT").ok())
            .unwrap_or_else(|| "8545".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let ewp = m
            .value_of("evm-ws-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("EVM_WS_PORT").ok())
            .unwrap_or_else(|| "8546".to_owned())
            .parse::<u16>()
            .c(d!())?;

        let res = Config {
            abci_host: ah,
            abci_port: ap,
            tendermint_host: th,
            tendermint_port: tp,
            submission_service_port: ssp,
            ledger_service_port: lsp,
            enable_query_service: eqs,
            enable_eth_empty_blocks: eeb,
            enable_eth_api_service: eas,
            evm_http_port: ehp,
            evm_ws_port: ewp,
            tendermint_node_self_addr: tnsa,
            tendermint_node_key_config_path: tnkcp,
            ledger_dir: ld,
        };

        Ok(res)
    }

    #[cfg(not(test))]
    fn print_version(m: &ArgMatches) {
        if m.is_present("version") {
            println!("{}", env!("VERGEN_SHA"));
            std::process::exit(0);
        }
    }
}
