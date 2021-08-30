//!
//! # Findorad
//!
//! A fake wrapper with same functions of the prevous `findorad`,
//! use this program to keep compatible with the old CI process.
//!

#![deny(warnings)]

use config::CFG;
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

fn node_command() -> Result<()> {
    unpack().c(d!())?;

    let mut abcid = Command::new("/tmp/abcid__");

    abcid
        .arg("--submission-service-port")
        .arg(CFG.submission_service_port.to_string())
        .arg("ledger-service-port")
        .arg(CFG.ledger_service_port.to_string())
        .arg("--ledger-dir")
        .arg(&CFG.ledger_dir);

    if let Some(v) = CFG.tendermint_node_self_addr.as_deref() {
        abcid.arg("--tendermint-node-self-addr").arg(v);
    }

    if let Some(v) = CFG.tendermint_node_key_config_path.as_deref() {
        abcid.arg("--tendermint-node-key-config-path").arg(v);
    }

    if CFG.enable_ledger_service {
        abcid.arg("--enable_ledger_service");
    }

    if CFG.enable_query_service {
        abcid.arg("--enable_query_service");
    }

    let mut abcid_child = abcid
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .c(d!())?;

    let mut tendermint_child = Command::new("/tmp/tendermint__")
        .arg("node")
        .arg("--home")
        .arg(&CFG.tendermint_home)
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
    tendermint_sys::init_home(&CFG.tendermint_home).unwrap();
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
        .open("/tmp/abcid__")
        .c(d!())?;
    io::copy(&mut abcid_reader, &mut abcid_writer)
        .c(d!())
        .and_then(|_| {
            set_permissions("/tmp/abcid__", Permissions::from_mode(0o755)).c(d!())
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
        .open("/tmp/tendermint__")
        .c(d!())?;
    io::copy(&mut tendermint_reader, &mut tendermint_writer)
        .c(d!())
        .and_then(|_| {
            set_permissions("/tmp/tendermint__", Permissions::from_mode(0o755)).c(d!())
        })?;

    truncate("/tmp/tendermint__", tendermint_len as i64).c(d!())
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
    match CFG.command.as_str() {
        "init" => pnk!(init_command()),
        "node" => pnk!(node_command()),
        "pack" => pnk!(pack()),
        _ => pnk!(Err(eg!("The available options are 'node'/'init'"))),
    }
}

mod init {
    use ruc::*;
    use std::{fs, str};

    const QA01_GENESIS_URL: &str = "https://dev-qa01.dev.findora.org:26657/genesis";
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
            InitMode::Dev => {}
        }
        Ok(())
    }

    pub fn generate_tendermint_config(mode: InitMode, path: &str) -> Result<()> {
        let config = fs::read_to_string(path).c(d!())?;
        let config =
            str::replace(&config, "index_all_keys = false", "index_all_keys = true");
        let config = str::replace(
            &config,
            "laddr = \"tcp://127.0.0.1:26657\"",
            "laddr = \"tcp://0.0.0.0:26657\"",
        );

        let result = match mode {
            InitMode::Testnet => {
                str::replace(
                    &config,
                    "persistent_peers = \"\"",
                    "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@prod-testnet-us-west-2-sentry-000-public.prod.findora.org:26656\"",
                )
            }
            InitMode::Mainnet => {
                str::replace(
                    &config,
                    "persistent_peers = \"\"",
                    "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@prod-mainnet-us-west-2-sentry-000-public.prod.findora.org:26656\"",
                )
            }
            InitMode::Qa01 => {
                str::replace(
                    &config,
                    "persistent_peers = \"\"",
                    "persistent_peers = \"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@dev-qa01-us-west-2-sentry-000-public.dev.findora.org:26656\"",
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

    fn print_version(m: ArgMatches) {
        if m.is_present("version") {
            println!("{}", env!("VERGEN_SHA"));
            process::exit(0);
        }
    }
}
