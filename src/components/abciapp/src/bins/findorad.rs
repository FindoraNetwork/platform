//!
//! # Findorad
//!
//! A fake wrapper with same functions of the prevous `findorad`,
//! use this program to keep compatible with the old CI process.
//!

#![deny(warnings)]

use config::CFG;
use lazy_static::lazy_static;
use nix::{
    sys::signal::{kill, Signal},
    unistd::{truncate, Pid},
};
use ruc::*;
use std::{
    convert::TryFrom,
    env,
    fs::{self, metadata, set_permissions, File, OpenOptions, Permissions},
    io::{self, prelude::*, BufReader, Read, Seek, SeekFrom},
    mem::size_of,
    os::unix::fs::PermissionsExt,
    path::PathBuf,
    process::{Command, Stdio},
};

const U64L: usize = size_of::<u64>();
const PAD_SIZE: usize = 128;

lazy_static! {
    static ref SUFFIX: u32 = rand::random();
}

fn node_command() -> Result<()> {
    let mut abcid = Command::new(format!("/tmp/abcid_{}", *SUFFIX));

    macro_rules! convert_arg {
        ($arg: tt) => {{
            if let Some(v) = CFG.$arg.as_deref() {
                abcid
                    .arg("--".to_owned() + &stringify!($arg).replace("_", "-"))
                    .arg(v);
            }
        }};
    }

    abcid
        .arg("--submission-service-port")
        .arg(CFG.submission_service_port.to_string())
        .arg("--ledger-service-port")
        .arg(CFG.ledger_service_port.to_string())
        .arg("--ledger-dir")
        .arg(&CFG.ledger_dir);

    for (condition, action) in [
        (CFG.enable_query_service, "--enable-query-service"),
        (CFG.enable_eth_api_service, "--enable-eth-api-service"),
        (CFG.disable_eth_empty_blocks, "--disable-eth-empty-blocks"),
        (CFG.enable_snapshot, "--enable-snapshot"),
        (CFG.snapshot_list, "--snapshot-list"),
        (CFG.snapshot_rollback, "--snapshot-rollback"),
    ] {
        if condition {
            abcid.arg(action);
        }
    }

    convert_arg!(tendermint_node_self_addr);
    convert_arg!(tendermint_node_key_config_path);
    convert_arg!(snapshot_target);
    convert_arg!(snapshot_itv);
    convert_arg!(snapshot_cap);
    convert_arg!(snapshot_mode);
    convert_arg!(snapshot_algo);
    convert_arg!(snapshot_rollback_to);
    convert_arg!(snapshot_rollback_to_exact);

    let mut abcid_child = abcid
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .c(d!())?;

    if CFG.snapshot_list
        || CFG.snapshot_rollback
        || CFG.snapshot_rollback_to.is_some()
        || CFG.snapshot_rollback_to_exact.is_some()
    {
        return Ok(());
    }

    let mut tendermint = Command::new(format!("/tmp/tendermint_{}", *SUFFIX));

    tendermint
        .arg("node")
        .arg("--home")
        .arg(&CFG.tendermint_home);

    if CFG.no_fast_sync {
        tendermint.arg("--fast_sync=false");
    } else {
        tendermint.arg("--fast_sync=true");
    }

    let mut tendermint_child = tendermint
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .c(d!())?;

    abcid_child.wait().c(d!()).map(|s| println!("{}", s))?;
    tendermint_child.wait().c(d!()).map(|s| println!("{}", s))?;

    ctrlc::set_handler(move || {
        info_omit!(kill(Pid::from_raw(0), Signal::SIGINT));
        info_omit!(kill(Pid::from_raw(0), Signal::SIGINT));
    })
    .c(d!())
}

fn init_command() -> Result<()> {
    Command::new(format!("/tmp/tendermint_{}", *SUFFIX))
        .arg("init")
        .arg("validator")
        .arg("--home")
        .arg(&CFG.tendermint_home)
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .c(d!())?;
    sleep_ms!(200);
    init::init_genesis(
        CFG.init_mode,
        &(CFG.tendermint_home.clone() + "/config/genesis.json"),
    )?;
    init::generate_tendermint_config(
        CFG.init_mode,
        &(CFG.tendermint_home.clone() + "/config/config.toml"),
    )?;
    Ok(())
}

fn pack() -> Result<()> {
    let bin_path_orig = get_bin_path().c(d!())?;
    let bin_name = bin_path_orig.file_name().c(d!())?.to_str().c(d!())?;
    let bin_path = format!("/tmp/{}", bin_name);
    fs::copy(bin_path_orig, &bin_path).c(d!())?;

    let mut f = OpenOptions::new().append(true).open(bin_path).c(d!())?;
    let mut f_tendermint = File::open("tendermint").c(d!())?;
    let mut f_abcid = File::open("abcid").c(d!())?;

    f.write(&[0u8; PAD_SIZE][..]).c(d!())?;
    io::copy(&mut f_tendermint, &mut f).c(d!())?;
    f.write(&[0u8; PAD_SIZE][..]).c(d!())?;
    io::copy(&mut f_abcid, &mut f).c(d!())?;
    f.write(&[0u8; PAD_SIZE][..]).c(d!())?;

    let tendermint_len = metadata("tendermint").c(d!())?.len();
    f.write(&tendermint_len.to_ne_bytes()[..]).c(d!())?;

    let abcid_len = metadata("abcid").c(d!())?.len();
    f.write(&abcid_len.to_ne_bytes()[..]).c(d!()).map(|_| ())
}

fn unpack() -> Result<()> {
    let bin_path = get_bin_path().c(d!())?;

    let mut f = File::open(bin_path).c(d!())?;
    let mut tendermint_len = [0u8; U64L];
    let mut abcid_len = [0u8; U64L];
    f.seek(SeekFrom::End(-2 * U64L as i64)).c(d!())?;
    f.read(&mut tendermint_len).c(d!())?;
    f.read(&mut abcid_len).c(d!())?;
    let tendermint_len = u64::from_ne_bytes(tendermint_len) as usize;
    let abcid_len = u64::from_ne_bytes(abcid_len) as usize;

    let mut abcid_reader = BufReader::with_capacity(abcid_len, f);
    i64::try_from(abcid_len + PAD_SIZE + 2 * U64L)
        .c(d!())
        .and_then(|siz| abcid_reader.seek(SeekFrom::End(-siz)).c(d!()))
        .and_then(|_| abcid_reader.fill_buf().c(d!()))?;
    let mut abcid_writer = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(format!("/tmp/abcid_{}", *SUFFIX))
        .c(d!())?;
    io::copy(&mut abcid_reader, &mut abcid_writer)
        .c(d!())
        .and_then(|_| {
            set_permissions(
                format!("/tmp/abcid_{}", *SUFFIX),
                Permissions::from_mode(0o755),
            )
            .c(d!())
        })?;

    let mut tendermint_reader =
        BufReader::with_capacity(tendermint_len, abcid_reader.into_inner());
    i64::try_from(tendermint_len + PAD_SIZE + abcid_len + PAD_SIZE + 2 * U64L)
        .c(d!())
        .and_then(|siz| tendermint_reader.seek(SeekFrom::End(-siz)).c(d!()))
        .and_then(|_| tendermint_reader.fill_buf().c(d!()))?;
    let mut tendermint_writer = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(format!("/tmp/tendermint_{}", *SUFFIX))
        .c(d!())?;
    io::copy(&mut tendermint_reader, &mut tendermint_writer)
        .c(d!())
        .and_then(|_| {
            set_permissions(
                format!("/tmp/tendermint_{}", *SUFFIX),
                Permissions::from_mode(0o755),
            )
            .c(d!())
        })?;

    truncate(
        format!("/tmp/tendermint_{}", *SUFFIX).as_str(),
        tendermint_len as i64,
    )
    .c(d!())
}

fn get_bin_path() -> Result<PathBuf> {
    let bin_path = env::current_exe().c(d!())?;
    let bin_size = metadata(&bin_path).c(d!())?.len() as usize;
    if (2 * U64L + 3 * PAD_SIZE) > bin_size {
        return Err(eg!("Invalid binary size"));
    }
    Ok(bin_path)
}

fn main() {
    let res = match CFG.command.as_str() {
        "init" => unpack().c(d!()).and_then(|_| init_command().c(d!())),
        "node" => unpack().c(d!()).and_then(|_| node_command().c(d!())),
        "pack" => pack().c(d!()),
        _ => Err(eg!("The available options are 'node'/'init'")),
    };

    pnk!(res);
}

mod init {
    use ruc::*;
    use std::{fs, str};

    const QA01_GENESIS_URL: &str = "https://dev-qa01.dev.findora.org:26657/genesis";
    const QA02_GENESIS_URL: &str = "https://dev-qa02.dev.findora.org:26657/genesis";
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
        ];

        let config = orig_cfg
            .iter()
            .zip(target_cfg.iter())
            .fold(config, |acc, pair| acc.replace(pair.0, pair.1));

        let result = match mode {
            InitMode::Testnet => {
                config.replace(
                    "persistent_peers = \"\"",
                    "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@prod-testnet-us-west-2-sentry-000-public.prod.findora.org:26656,d0c6e3e1589695ae6d650b288caf2efe9a998a50@prod-testnet-us-west-2-sentry-001-public.prod.findora.org:26656,78661a9979c100e8f1303cbd121cb1b326ff694f@prod-testnet-us-west-2-sentry-002-public.prod.findora.org:26656,6723af6a3aef14cd7eb5ee8d5d0ac227af1e9651@prod-testnet-us-west-2-sentry-003-public.prod.findora.org:26656\"",
                )
            }
            InitMode::Mainnet => {
                config.replace(
                    "persistent_peers = \"\"",
                    "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@prod-mainnet-us-west-2-sentry-000-public.prod.findora.org:26656,d0c6e3e1589695ae6d650b288caf2efe9a998a50@prod-mainnet-us-west-2-sentry-001-public.prod.findora.org:26656,78661a9979c100e8f1303cbd121cb1b326ff694f@prod-mainnet-us-west-2-sentry-002-public.prod.findora.org:26656,6723af6a3aef14cd7eb5ee8d5d0ac227af1e9651@prod-mainnet-us-west-2-sentry-003-public.prod.findora.org:26656\"",
                )
            }
            InitMode::Qa01 => {
                config.replace(
                    "persistent_peers = \"\"",
                    "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@dev-qa01-us-west-2-sentry-000-public.dev.findora.org:26656\"",
                )
            }
            InitMode::Qa02 => {
                config.replace(
                    "persistent_peers = \"\"",
                    "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@dev-qa02-us-west-2-sentry-000-public.dev.findora.org:26656\"",
                )
            }
            InitMode::Dev => config,
        };
        fs::write(path, result).c(d!())?;

        Ok(())
    }
}

mod config {
    use super::init::InitMode;
    use clap::{crate_authors, App, Arg, ArgGroup, ArgMatches, SubCommand};
    use lazy_static::lazy_static;
    use ruc::*;
    use std::{env, process};

    lazy_static! {
        pub(crate) static ref CFG: Config = pnk!(get_config());
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
                .arg_from_usage("-R, --snapshot-rollback-to-exact=[Height] 'rollback to a custom height exactly, an error will be reported if the target does not exist'");

            let init = SubCommand::with_name("init")
                    .about("Initialize the configurations of findorad")
                    .arg_from_usage("--devnet 'Initialize for Findora Local DevNet.'")
                    .arg_from_usage("--testnet 'Initialize for Findora TestNet.'")
                    .arg_from_usage("--mainnet 'Initialize for Findora MainNet.'")
                    .arg_from_usage("--qa01 'Initialize for Findora QA01.'")
                    .arg_from_usage("--qa02 'Initialize for Findora QA02.'")
                    .group(ArgGroup::with_name("environment").args(&["devnet", "testnet", "mainnet", "qa01", "qa02"]))
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
        } else {
            InitMode::Dev
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
            command: cmd.to_owned(),
            tendermint_config: tcfg,
            tendermint_home: tdir,
            init_mode,
            enable_snapshot: m.is_present("enable-snapshot"),
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
