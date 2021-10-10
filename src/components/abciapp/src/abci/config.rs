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
    #[serde(skip)]
    pub ledger_dir: Option<String>,
}

impl TryFrom<ABCIConfigStr> for ABCIConfig {
    type Error = Box<dyn RucError>;
    fn try_from(cfg: ABCIConfigStr) -> Result<Self> {
        let ledger_port = cfg.ledger_port.parse::<u16>().c(d!())?;
        let query_port = ledger_port - 1;
        Ok(ABCIConfig {
            abci_host: cfg.abci_host,
            abci_port: cfg.abci_port.parse::<u16>().c(d!())?,
            tendermint_host: cfg.tendermint_host,
            tendermint_port: cfg.tendermint_port.parse::<u16>().c(d!())?,
            submission_port: cfg.submission_port.parse::<u16>().c(d!())?,
            ledger_port,
            query_port,
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
    use crate::abci::snap::SnapCfg;
    #[cfg(not(test))]
    use crate::abci::snap::SnapMode;
    #[cfg(not(test))]
    use clap::{crate_authors, App, Arg, ArgMatches};
    use lazy_static::lazy_static;
    use ruc::*;
    #[cfg(not(test))]
    use std::{env, process::exit, str::FromStr};

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
        pub tendermint_node_self_addr: Option<String>,
        pub tendermint_node_key_config_path: Option<String>,
        pub ledger_dir: String,
        pub snapcfg: SnapCfg,
    }

    #[cfg(test)]
    fn get_config() -> Result<Config> {
        Ok(Config {
            ledger_dir: globutils::fresh_tmp_dir().to_string_lossy().into_owned(),
            ..Default::default()
        })
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
                .arg_from_usage("--tendermint-node-self-addr=[Address] 'the address of your tendermint node, in upper-hex format'")
                .arg_from_usage("--tendermint-node-key-config-path=[Path] 'such as: ${HOME}/.tendermint/config/priv_validator_key.json'")
                .arg_from_usage("-d, --ledger-dir=[Path]")

                .arg_from_usage("--enable-snapshot 'global switch for enabling snapshot functions'")
                .arg_from_usage("--snapshot-list 'list all available snapshots in the form of block height'")
                .arg_from_usage("--snapshot-target=[TargetPath] 'a data volume containing both ledger data and tendermint data'")
                .arg_from_usage("--snapshot-itv=[Iterval] 'interval between adjacent snapshots, default to 10 blocks'")
                .arg_from_usage("--snapshot-cap=[Capacity] 'the maximum number of snapshots that will be stored, default to 100'")
                .arg_from_usage("--snapshot-mode=[Mode] 'zfs/btrfs/external, will try a guess if missing'")
                .arg_from_usage("--snapshot-rollback 'rollback to the last available snapshot'")
                .arg_from_usage("-r, --snapshot-rollback-to=[Height] 'rollback to a custom height, will try the closest smaller height if the target does not exist'")
                .arg_from_usage("-R, --snapshot-rollback-to-exact=[Height] 'rollback to a custom height exactly, an error will be reported if the target does not exist'")
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

        let res = Config {
            abci_host: ah,
            abci_port: ap,
            tendermint_host: th,
            tendermint_port: tp,
            submission_service_port: ssp,
            ledger_service_port: lsp,
            enable_query_service: eqs,
            tendermint_node_self_addr: tnsa,
            tendermint_node_key_config_path: tnkcp,
            ledger_dir: ld,
            snapcfg: parse_snapcfg(&m).c(d!())?,
        };

        Ok(res)
    }

    #[cfg(not(test))]
    fn print_version(m: &ArgMatches) {
        if m.is_present("version") {
            println!("{}", env!("VERGEN_SHA"));
            exit(0);
        }
    }

    #[cfg(not(test))]
    fn parse_snapcfg(m: &ArgMatches) -> Result<SnapCfg> {
        let mut res = SnapCfg::new();

        res.enable = m.is_present("enable-snapshot");

        if res.enable
            || m.is_present("snapshot-list")
            || m.is_present("snapshot-rollback")
            || m.is_present("snapshot-rollback-to")
            || m.is_present("snapshot-rollback-to-exact")
        {
            // this field should be parsed at the top
            res.target = m.value_of("snapshot-target").c(d!())?.to_owned();
        }

        res.itv = m
            .value_of("snapshot-itv")
            .unwrap_or("10")
            .parse::<u32>()
            .c(d!())?;
        res.cap = m
            .value_of("snapshot-cap")
            .unwrap_or("100")
            .parse::<u32>()
            .c(d!())?;

        if let Some(si) = m.value_of("snapshot-mode") {
            res.mode = SnapMode::from_str(si).c(d!())?;
        } else {
            res.mode = res.guess_mode().c(d!())?;
        }

        if m.is_present("snapshot-list") {
            list_snapshots(&res).c(d!())?;
        }

        check_rollback(&m, &res).c(d!())?;

        Ok(res)
    }

    #[cfg(not(test))]
    fn list_snapshots(cfg: &SnapCfg) -> Result<()> {
        println!("Available snapshots are listed below:");
        cfg.get_sorted_snapshots()
            .c(d!())?
            .into_iter()
            .for_each(|h| {
                println!("    {}", h);
            });
        exit(0);
    }

    #[cfg(not(test))]
    fn check_rollback(m: &ArgMatches, cfg: &SnapCfg) -> Result<()> {
        const HINTS: &str = r#"    NOTE:
            before executing the rollback,
            all related processes must be exited,
            such as findorad, abcid, tendermint, etc.
        "#;

        if m.is_present("snapshot-rollback")
            || m.is_present("snapshot-rollback-to")
            || m.is_present("snapshot-rollback-to-exact")
        {
            println!("\x1b[31;01m\n{}\x1b[00m", HINTS);

            let (h, strict) = m
                .value_of("snapshot-rollback-to-exact")
                .map(|h| (Some(h), true))
                .or_else(|| m.value_of("snapshot-rollback-to").map(|h| (Some(h), false)))
                .unwrap_or((None, false));
            let h = if let Some(h) = h {
                Some(h.parse::<u64>().c(d!())?)
            } else {
                None
            };
            cfg.rollback(h, strict).c(d!())?;

            exit(0);
        }
        Ok(())
    }
}
