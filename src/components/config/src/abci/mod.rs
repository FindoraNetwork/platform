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
    pub evm_substate_height: i64,
    pub disable_evm_block_height: i64,
    pub enable_frc20_height: i64,
    pub evm_first_block_height: i64,
    pub zero_amount_fix_height: u64,
    pub apy_fix_height: u64,
    pub overflow_fix_height: u64,
    pub second_fix_height: u64,
    pub apy_v7_upgrade_height: u64,
    pub ff_addr_extra_fix_height: u64,
    pub nonconfidential_balance_fix_height: u64,
    pub unbond_block_cnt: u64,
    pub prismxx_inital_height: i64,
    pub enable_triple_masking_height: i64,
    // Note: This field only used to qa02.
    pub fix_unpaid_delegation_height: u64,
    pub evm_checktx_nonce: i64,
    pub utxo_checktx_height: i64,
    pub utxo_asset_prefix_height: u64,
    pub prism_bridge_address: String,
    pub nonce_bug_fix_height: u64,
    pub ci_test_value: u64,
    pub proper_gas_set_height: u64,
    pub ci_test_value: u64,
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
                                prismxx_inital_height: 1,
                                enable_triple_masking_height: 0,
                                fix_unpaid_delegation_height: 0,
                                evm_checktx_nonce: 0,
                                utxo_checktx_height: 0,
                                utxo_asset_prefix_height: 0,
                                nonce_bug_fix_height: 0,
                                prism_bridge_address: String::new(),
                                ci_test_value: 0,
                                proper_gas_set_height: 0,
                                ci_test_value: 0,
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
                                prismxx_inital_height: 30000000,
                                enable_triple_masking_height: 30000000,
                                fix_unpaid_delegation_height: 2261885,
                                evm_checktx_nonce: 30000000,
                                utxo_checktx_height: 30000000,
                                utxo_asset_prefix_height: 30000000,
                                nonce_bug_fix_height: 30000000,
                                prism_bridge_address: String::new(),
                                ci_test_value: 30000000,
                                proper_gas_set_height: 30000000,
                                ci_test_value: 30000000,
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
        let config: CheckPointConfig = toml::from_str(content.as_str()).unwrap();
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
        let ledger_port = cfg
            .ledger_port
            .parse::<u16>()
            .c(d!("Invalid ledger port."))?;
        let query_port = ledger_port - 1;
        let evm_http_port = cfg
            .evm_http_port
            .parse::<u16>()
            .c(d!("Invalid evm http port."))?;
        let evm_ws_port = cfg
            .evm_ws_port
            .parse::<u16>()
            .c(d!("Invalid evm ws port."))?;
        Ok(ABCIConfig {
            abci_host: cfg.abci_host,
            abci_port: cfg.abci_port.parse::<u16>().c(d!("Invalid abci port."))?,
            tendermint_host: cfg.tendermint_host,
            tendermint_port: cfg
                .tendermint_port
                .parse::<u16>()
                .c(d!("Invalid tendermint port."))?,
            submission_port: cfg
                .submission_port
                .parse::<u16>()
                .c(d!("Invalid submission port."))?,
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
        let file_contents = fs::read_to_string(config_path).map_err(|e| d!("{}", e))?;
        let toml_string =
            toml::from_str::<ABCIConfigStr>(&file_contents).map_err(|e| d!("{}", e))?;
        let config = ABCIConfig::try_from(toml_string).map_err(|e| d!("{}", e))?;
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
    use clap::{crate_authors, App, Arg, ArgMatches};
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
            .arg_from_usage("--disable-eth-empty-blocks 'not generate empty ethereum blocks when no evm transaction'")
            .arg_from_usage("--enable-eth-api-service")
            .arg_from_usage("--evm-http-port=[EVM Web3 Http Port]")
            .arg_from_usage("--evm-ws-port=[EVM Web3 WS Port]")
            .arg_from_usage("--tendermint-node-self-addr=[Address] 'the address of your tendermint node, in upper-hex format'")
            .arg_from_usage("--tendermint-node-key-config-path=[Path] 'such as: ${HOME}/.tendermint/config/priv_validator_key.json'")
            .arg_from_usage("-d, --ledger-dir=[Path]")
            .arg_from_usage("--checkpoint-file=[Path]")
            .arg_from_usage("--enable-snapshot 'global switch for enabling snapshot functions'")
            .arg_from_usage("--snapshot-list 'list all available snapshots in the form of block height'")
            .arg_from_usage("--snapshot-target=[TargetPath] 'a data volume containing both ledger data and tendermint data'")
            .arg_from_usage("--snapshot-itv=[Iterval] 'interval between adjacent snapshots, default to 10 blocks'")
            .arg_from_usage("--snapshot-cap=[Capacity] 'the maximum number of snapshots that will be stored, default to 100'")
            .arg_from_usage("--snapshot-mode=[Mode] 'zfs/btrfs/external, will try a guess if missing'")
            .arg_from_usage("--snapshot-algo=[Algo] 'fair/fade, default to `fair`'")
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
            .c(d!("Invalid `abcid-port`."))?;
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
            .c(d!("Invalid `tendermint-port`."))?;
        let ssp = m
            .value_of("submission-service-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("SUBMISSION_PORT").ok())
            .unwrap_or_else(|| "8669".to_owned())
            .parse::<u16>()
            .c(d!("Invalid `submission-service-port`."))?;
        let lsp = m
            .value_of("ledger-service-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("LEDGER_PORT").ok())
            .unwrap_or_else(|| "8668".to_owned())
            .parse::<u16>()
            .c(d!("Invalid `ledger-service-port`."))?;
        let eqs = m.is_present("enable-query-service")
            || env::var("ENABLE_QUERY_SERVICE").is_ok();
        let tnsa = m
            .value_of("tendermint-node-self-addr")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TD_NODE_SELF_ADDR").ok());
        let tnkcp = m
            .value_of("tendermint-node-key-abci-path")
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
        let eeb = m.is_present("disable-eth-empty-blocks")
            || env::var("DISABLE_ETH_EMPTY_BLOCKS").is_ok();
        let eas = m.is_present("enable-eth-api-service")
            || env::var("ENABLE_ETH_API_SERVICE").is_ok();
        let ehp = m
            .value_of("evm-http-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("EVM_HTTP_PORT").ok())
            .unwrap_or_else(|| "8545".to_owned())
            .parse::<u16>()
            .c(d!("Invalid `evm-http-port`."))?;
        let ewp = m
            .value_of("evm-ws-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("EVM_WS_PORT").ok())
            .unwrap_or_else(|| "8546".to_owned())
            .parse::<u16>()
            .c(d!("Invalid `evm-ws-port`."))?;
        let checkpoint_path = m
            .value_of("checkpoint-file")
            .map(|v| v.to_owned())
            .unwrap_or_else(|| String::from("./checkpoint.toml"));
        println!("{}", checkpoint_path);

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
            btmcfg: parse_btmcfg(&m).map_err(|e| d!("{}", e))?,
            checkpoint: CheckPointConfig::from_file(&checkpoint_path).unwrap(),
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
    #[cfg(target_os = "linux")]
    fn parse_btmcfg(m: &ArgMatches) -> Result<BtmCfg> {
        let mut res = BtmCfg::new();

        res.enable = m.is_present("enable-snapshot");

        if res.enable {
            res.itv = m
                .value_of("snapshot-itv")
                .unwrap_or("10")
                .parse::<u64>()
                .c(d!("Invalid `snapshot-itv`."))?;
            res.cap = m
                .value_of("snapshot-cap")
                .unwrap_or("100")
                .parse::<u64>()
                .c(d!("Invalid `snapshot-cap`."))?;

            if let Some(sm) = m.value_of("snapshot-mode") {
                res.mode = SnapMode::from_string(sm).map_err(|e| d!("{}", e))?;
                if !matches!(res.mode, SnapMode::External) {
                    res.target = m
                        .value_of("snapshot-target")
                        .c(d!("Missing `snapshot-target`."))?
                        .to_owned();
                }
            } else {
                res.target = m
                    .value_of("snapshot-target")
                    .c(d!("Missing `snapshot-target`."))?
                    .to_owned();
                res.mode = res.guess_mode().map_err(|e| d!("{}", e))?;
            }

            if let Some(sa) = m.value_of("snapshot-algo") {
                res.algo = SnapAlgo::from_string(sa).map_err(|e| d!("{}", e))?;
                res.itv.checked_pow(STEP_CNT as u32).c(d!())?;
            }
        }

        if m.is_present("snapshot-list")
            || m.is_present("snapshot-rollback")
            || m.is_present("snapshot-rollback-to")
            || m.is_present("snapshot-rollback-to-exact")
        {
            // this field should be parsed at the top
            res.target = m
                .value_of("snapshot-target")
                .c(d!("Missing `snapshot-target`."))?
                .to_owned();

            // the guess should always success in this scene
            res.mode = res.guess_mode().map_err(|e| d!("{}", e))?;

            if m.is_present("snapshot-list") {
                list_snapshots(&res).map_err(|e| d!("{}", e))?;
            }

            check_rollback(m, &res).map_err(|e| d!("{}", e))?;
        }

        Ok(res)
    }

    #[cfg(not(test))]
    #[cfg(target_os = "linux")]
    fn list_snapshots(cfg: &BtmCfg) -> Result<()> {
        println!("Available snapshots are listed below:");
        cfg.get_sorted_snapshots()
            .map_err(|e| d!("{}", e))?
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
                Some(h.parse::<u64>().c(d!("Invalid height."))?)
            } else {
                None
            };
            cfg.rollback(h, strict).map_err(|e| d!("{}", e))?;
            exit(0);
        }
        Ok(())
    }
}
