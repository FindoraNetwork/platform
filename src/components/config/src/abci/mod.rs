use {
    global_cfg::CFG,
    ruc::*,
    serde::{Deserialize, Serialize},
    std::{
        convert::TryFrom,
        env,
        fs::{self, File},
        io::{ErrorKind, Read, Write},
        path::Path,
    },
    toml,
};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[allow(missing_docs)]
pub struct CheckPointConfig {
    // https://github.com/FindoraNetwork/platform/pull/211
    // Enable evm substate.
    pub evm_substate_height: i64,

    // https://github.com/FindoraNetwork/platform/pull/178
    // Disable evm.
    pub disable_evm_block_height: i64,

    // https://github.com/FindoraNetwork/platform/pull/181
    // fix node crash and reset update height.
    pub enable_frc20_height: i64,

    // Commit: d449b7c97850b225cf26c72c8b19ed284d6d7101
    pub evm_first_block_height: i64,

    // Commit: a1cfa708074df18379e0cf01d4df48794c5d100d
    // Fix a BUG in the calculation of commission.
    pub zero_amount_fix_height: u64,

    // Commit: 69ca4865842a3a1eef628a5ceab5e856c3e866c4,
    // Rename: a1cfa708074df18379e0cf01d4df48794c5d100d
    pub apy_fix_height: u64,

    // https://github.com/FindoraNetwork/platform/pull/98
    // Fix delegation rewards overflow using bigint.
    pub overflow_fix_height: u64,

    // Commit c9d2b4f5760cb5bd79848b451fca56c023b1cc71
    // Sync APY v7 upgrade block height.
    pub second_fix_height: u64,

    // https://github.com/FindoraNetwork/platform/pull/97
    // Change APY modifier based on v7 rate.
    pub apy_v7_upgrade_height: u64,

    // Commit: a1cfa708074df18379e0cf01d4df48794c5d100d
    // Add an extra `reserved` address; fix a BUG in the calculation of commission.
    pub ff_addr_extra_fix_height: u64,

    // https://github.com/FindoraNetwork/platform/pull/93
    // Fix incorrect calculations about nonconfidential balances.
    pub nonconfidential_balance_fix_height: u64,

    pub unbond_block_cnt: u64,

    // https://github.com/FindoraNetwork/platform/pull/307
    // Fix unpaid delegation.
    pub fix_unpaid_delegation_height: u64,

    // https://github.com/FindoraNetwork/platform/pull/430
    // Fix missing rewards within 21 days after undelegation.
    pub fix_undelegation_missing_reward_height: i64,

    // https://github.com/FindoraNetwork/platform/pull/316
    // FO-968: Increment Nonce for CheckTx run mode without executing EVM transaction PORT TO MAIN.
    pub evm_checktx_nonce: i64,

    // https://github.com/FindoraNetwork/platform/pull/345
    // Fix the problem of utxo transaction body without signature.
    pub utxo_checktx_height: i64,

    // https://github.com/FindoraNetwork/platform/pull/434
    // Fix the amount in the delegators that staking did not modify when it punished the validator.
    pub fix_delegators_am_height: u64,
    pub validators_limit_v2_height: u64,
}

impl CheckPointConfig {
    /// load configuration of checkpoints from file.
    pub fn from_file(file_path: &str) -> Option<CheckPointConfig> {
        let mut f = match File::open(file_path) {
            Ok(file) => file,
            Err(error) => {
                if error.kind() == ErrorKind::NotFound {
                    match File::create(file_path) {
                        Ok(mut file) => {
                            #[cfg(feature = "debug_env")]
                            let config = CheckPointConfig {
                                evm_substate_height: 0,
                                disable_evm_block_height: 0,
                                enable_frc20_height: 0,
                                evm_first_block_height: 0,
                                zero_amount_fix_height: 0,
                                apy_fix_height: 0,
                                overflow_fix_height: 0,
                                second_fix_height: 0,
                                apy_v7_upgrade_height: 0,
                                ff_addr_extra_fix_height: 0,
                                nonconfidential_balance_fix_height: 0,
                                unbond_block_cnt: 3600 * 24 * 21 / 16,
                                fix_unpaid_delegation_height: 0,
                                fix_undelegation_missing_reward_height: 0,
                                evm_checktx_nonce: 0,
                                utxo_checktx_height: 0,
                                fix_delegators_am_height: 0,
                                validators_limit_v2_height: 0,
                            };
                            #[cfg(not(feature = "debug_env"))]
                            let config = CheckPointConfig {
                                evm_substate_height: 1802500,
                                disable_evm_block_height: 1483286,
                                enable_frc20_height: 1501000,
                                evm_first_block_height: 0,
                                zero_amount_fix_height: 1200000,
                                apy_fix_height: 1177000,
                                overflow_fix_height: 1247000,
                                second_fix_height: 1429000,
                                apy_v7_upgrade_height: 1429000,
                                ff_addr_extra_fix_height: 1200000,
                                nonconfidential_balance_fix_height: 1210000,
                                unbond_block_cnt: 3600 * 24 * 21 / 16,
                                fix_undelegation_missing_reward_height: 3000000,
                                fix_unpaid_delegation_height: 2261885,
                                evm_checktx_nonce: 30000000,
                                utxo_checktx_height: 30000000,
                                fix_delegators_am_height: 30000000,
                                validators_limit_v2_height: 30000000,
                            };
                            let content = toml::to_string(&config).unwrap();
                            file.write_all(content.as_bytes()).unwrap();
                            return Some(config);
                        }
                        Err(error) => {
                            panic!("failed to create file: {:?}", error)
                        }
                    };
                } else {
                    panic!("failed to open file: {:?}", error)
                }
            }
        };

        let mut content = String::new();
        f.read_to_string(&mut content).unwrap();
        let config: CheckPointConfig = toml::from_str(content.as_str())
            .or_else(|_| serde_json::from_str(content.as_str()))
            .unwrap();
        Some(config)
    }
}

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
            ledger_dir: cfg
                .ledger_dir
                .unwrap_or_else(|| pnk!(env::var("LEDGER_DIR"))),
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

pub mod global_cfg {
    use crate::abci::CheckPointConfig;
    #[cfg(target_os = "linux")]
    use btm::BtmCfg;
    #[cfg(not(test))]
    #[cfg(target_os = "linux")]
    use btm::{SnapAlgo, SnapMode, STEP_CNT};
    #[cfg(not(test))]
    use clap::{arg, crate_authors, Arg, ArgMatches, Command};
    use lazy_static::lazy_static;
    use ruc::*;
    #[cfg(not(test))]
    use std::{env, process::exit};

    lazy_static! {
        /// Global abci config.
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
        pub disable_eth_empty_blocks: bool,
        pub enable_eth_api_service: bool,
        pub evm_http_port: u16,
        pub evm_ws_port: u16,
        pub tendermint_node_self_addr: Option<String>,
        pub tendermint_node_key_config_path: Option<String>,
        pub ledger_dir: String,
        #[cfg(target_os = "linux")]
        pub btmcfg: BtmCfg,
        pub checkpoint: CheckPointConfig,
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
        let m = Command::new("abcid")
            .bin_name("abcid")
            .version(env!("VERGEN_GIT_SHA"))
            .author(crate_authors!())
            .about("An ABCI node implementation of FindoraNetwork.")
            .args([
                arg!(--"abcid-host" <"ABCId IP">),
                arg!(--"abcid-port" <"ABCId Port">),
                arg!(--"tendermint-host" <"Tendermint IP">),
                arg!(--"tendermint-port" <"Tendermint Port">),
                arg!(--"submission-service-port" <"Submission Service Port">),
                arg!(--"ledger-service-port" <"Ledger Service Port">),
                arg!(-q --"enable-query-service"),
                arg!(--"disable-eth-empty-blocks" "not generate empty ethereum blocks when no evm transaction"),
                arg!(--"enable-eth-api-service"),
                arg!(--"evm-http-port" <"EVM Web3 Http Port">),
                arg!(--"evm-ws-port" <"EVM Web3 WS Port">),
                arg!(--"tendermint-node-self-addr" <"Address"> "the address of your tendermint node, in upper-hex format"),
                arg!(--"tendermint-node-key-config-path" <"Path"> "such as: ${HOME}/.tendermint/config/priv_validator_key.json"),
                arg!(-d --"ledger-dir" <"Path">),
                arg!(--"checkpoint-file" <"Path">),
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
            ])
            .arg(Arg::new("_a").long("ignored").hide(true))
            .arg(Arg::new("_b").long("nocapture").hide(true))
            .arg(Arg::new("_c").long("test-threads").hide(true))
            .arg(Arg::new("INPUT").hide(true))
            .get_matches();
        let ah = m
            .get_one::<String>("abcid-host")
            .map(|v| v.to_owned())
            .or_else(|| env::var("ABCI_HOST").ok())
            .unwrap_or_else(|| "0.0.0.0".to_owned());
        let ap = m
            .get_one::<String>("abcid-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("ABCI_PORT").ok())
            .unwrap_or_else(|| "26658".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let th = m
            .get_one::<String>("tendermint-host")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_HOST").ok())
            .unwrap_or_else(|| "0.0.0.0".to_owned());
        let tp = m
            .get_one::<String>("tendermint-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_PORT").ok())
            .unwrap_or_else(|| "26657".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let ssp = m
            .get_one::<String>("submission-service-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("SUBMISSION_PORT").ok())
            .unwrap_or_else(|| "8669".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let lsp = m
            .get_one::<String>("ledger-service-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("LEDGER_PORT").ok())
            .unwrap_or_else(|| "8668".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let eqs = m.get_flag("enable-query-service")
            || env::var("ENABLE_QUERY_SERVICE").is_ok();
        let tnsa = m
            .get_one::<String>("tendermint-node-self-addr")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TD_NODE_SELF_ADDR").ok());
        let tnkcp = m
            .get_one::<String>("tendermint-node-key-abci-path")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_NODE_KEY_CONFIG_PATH").ok());
        let ld = m
            .get_one::<String>("ledger-dir")
            .map(|v| v.to_owned())
            .unwrap_or_else(|| {
                env::var("LEDGER_DIR").unwrap_or_else(|_| {
                    format!("{}/.tendermint/__findora__", pnk!(env::var("HOME")))
                })
            });
        let eeb = m.get_flag("disable-eth-empty-blocks")
            || env::var("DISABLE_ETH_EMPTY_BLOCKS").is_ok();
        let eas = m.get_flag("enable-eth-api-service")
            || env::var("ENABLE_ETH_API_SERVICE").is_ok();
        let ehp = m
            .get_one::<String>("evm-http-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("EVM_HTTP_PORT").ok())
            .unwrap_or_else(|| "8545".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let ewp = m
            .get_one::<String>("evm-ws-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("EVM_WS_PORT").ok())
            .unwrap_or_else(|| "8546".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let checkpoint_path = m
            .get_one::<String>("checkpoint-file")
            .map(|v| v.to_owned())
            .unwrap_or_else(|| String::from("./checkpoint.toml"));

        let res = Config {
            abci_host: ah,
            abci_port: ap,
            tendermint_host: th,
            tendermint_port: tp,
            submission_service_port: ssp,
            ledger_service_port: lsp,
            enable_query_service: eqs,
            disable_eth_empty_blocks: eeb,
            enable_eth_api_service: eas,
            evm_http_port: ehp,
            evm_ws_port: ewp,
            tendermint_node_self_addr: tnsa,
            tendermint_node_key_config_path: tnkcp,
            ledger_dir: ld,
            #[cfg(target_os = "linux")]
            btmcfg: parse_btmcfg(&m).c(d!())?,
            checkpoint: CheckPointConfig::from_file(&checkpoint_path).unwrap(),
        };

        Ok(res)
    }

    #[cfg(not(test))]
    #[cfg(target_os = "linux")]
    fn parse_btmcfg(m: &ArgMatches) -> Result<BtmCfg> {
        let mut res = BtmCfg::new();
        res.enable = m.get_flag("enable-snapshot");
        if res.enable {
            res.itv = m
                .get_one::<String>("snapshot-itv")
                .unwrap_or(&"10".to_string())
                .parse::<u64>()
                .c(d!())?;
            res.cap = m
                .get_one::<String>("snapshot-cap")
                .unwrap_or(&"100".to_string())
                .parse::<u64>()
                .c(d!())?;

            if let Some(sm) = m.get_one::<String>("snapshot-mode") {
                res.mode = SnapMode::from_string(sm).c(d!())?;
                if !matches!(res.mode, SnapMode::External) {
                    res.target =
                        m.get_one::<String>("snapshot-target").c(d!())?.to_owned();
                }
            } else {
                res.target = m.get_one::<String>("snapshot-target").c(d!())?.to_owned();
                res.mode = res.guess_mode().c(d!())?;
            }

            if let Some(sa) = m.get_one::<String>("snapshot-algo") {
                res.algo = SnapAlgo::from_string(sa).c(d!())?;
                res.itv.checked_pow(STEP_CNT as u32).c(d!())?;
            }
        }
        if m.get_flag("snapshot-list")
            || m.get_flag("snapshot-rollback")
            || m.get_one::<String>("snapshot-rollback-to").is_some()
            || m.get_one::<String>("snapshot-rollback-to-exact").is_some()
        {
            // this field should be parsed at the top
            res.target = m.get_one::<String>("snapshot-target").c(d!())?.to_owned();

            // the guess should always success in this scene
            res.mode = res.guess_mode().c(d!())?;

            if m.get_flag("snapshot-list") {
                list_snapshots(&res).c(d!())?;
            }

            check_rollback(m, &res).c(d!())?;
        }
        println!("3");
        Ok(res)
    }

    #[cfg(not(test))]
    #[cfg(target_os = "linux")]
    fn list_snapshots(cfg: &BtmCfg) -> Result<()> {
        println!("Available snapshots are listed below:");
        cfg.get_sorted_snapshots()
            .c(d!())?
            .into_iter()
            .rev()
            .for_each(|h| {
                println!("    {}", h);
            });
        exit(0);
    }

    #[cfg(not(test))]
    #[cfg(target_os = "linux")]
    fn check_rollback(m: &ArgMatches, cfg: &BtmCfg) -> Result<()> {
        const HINTS: &str = r#"    NOTE:
            before executing the rollback,
            all related processes must be exited,
            such as findorad, abcid, tendermint, etc.
        "#;

        if m.get_flag("snapshot-rollback")
            || m.get_one::<String>("snapshot-rollback-to").is_some()
            || m.get_one::<String>("snapshot-rollback-to-exact").is_some()
        {
            println!("\x1b[31;01m\n{}\x1b[00m", HINTS);

            let (h, strict) = m
                .get_one::<String>("snapshot-rollback-to-exact")
                .map(|h| (Some(h), true))
                .or_else(|| {
                    m.get_one::<String>("snapshot-rollback-to")
                        .map(|h| (Some(h), false))
                })
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
