//!
//! # Findorad
//!
//! A fake wrapper with same functions of the prevous `findorad`,
//! use this program to keep compatible with the old CI process.
//!

#![deny(warnings)]

use {
    config::findora::{config::CFG, init},
    lazy_static::lazy_static,
    nix::{
        sys::signal::{kill, Signal},
        unistd::{truncate, Pid},
    },
    regex::Regex,
    ruc::*,
    std::{
        convert::TryFrom,
        env,
        fs::{self, metadata, set_permissions, File, OpenOptions, Permissions},
        io::{self, prelude::*, BufReader, Read, Seek, SeekFrom},
        mem::size_of,
        os::unix::fs::PermissionsExt,
        path::PathBuf,
        process::{Command, Stdio},
    },
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

    let checkpoint_file = CFG
        .checkpoint_file
        .clone()
        .unwrap_or_else(|| String::from("./checkpoint.toml"));
    let arc_history_arg = {
        if let Some(interval) = CFG.arc_history.1 {
            format!("{},{}", CFG.arc_history.0, interval)
        } else {
            format!("{}", CFG.arc_history.0)
        }
    };

    abcid
        .arg("--submission-service-port")
        .arg(CFG.submission_service_port.to_string())
        .arg("--ledger-service-port")
        .arg(CFG.ledger_service_port.to_string())
        .arg("--checkpoint-file")
        .arg(&checkpoint_file)
        .arg("--ledger-dir")
        .arg(&CFG.ledger_dir)
        .arg("--arc-history")
        .arg(&arc_history_arg);

    if CFG.enable_enterprise_web3 {
        abcid.arg("--enable-enterprise-web3");
    }

    for (condition, action) in [
        (CFG.enable_query_service, "--enable-query-service"),
        (CFG.enable_eth_api_service, "--enable-eth-api-service"),
        (CFG.disable_eth_empty_blocks, "--disable-eth-empty-blocks"),
        (CFG.enable_snapshot, "--enable-snapshot"),
        (CFG.snapshot_list, "--snapshot-list"),
        (CFG.snapshot_rollback, "--snapshot-rollback"),
        (CFG.arc_fresh, "--arc-fresh"),
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

    {
        let src_cfg = [
            "timeout_propose(.*)\"(.*)\"",
            "timeout_propose_delta(.*)\"(.*)\"",
            "timeout_prevote(.*)\"(.*)\"",
            "timeout_prevote_delta(.*)\"(.*)\"",
            "timeout_precommit(.*)\"(.*)\"",
            "timeout_precommit_delta(.*)\"(.*)\"",
            "timeout_commit(.*)\"(.*)\"",
        ];

        let target_cfg = [
            "timeout_propose = \"6s\"",
            "timeout_propose_delta = \"500ms\"",
            "timeout_prevote = \"1s\"",
            "timeout_prevote_delta = \"500ms\"",
            "timeout_precommit = \"1s\"",
            "timeout_precommit_delta = \"500ms\"",
            "timeout_commit = \"15s\"",
        ];
        let path = format!("{}/config/config.toml", CFG.tendermint_home);
        fs::read_to_string(&path)
            .and_then(|config| {
                let cfg = src_cfg.iter().zip(target_cfg.iter()).fold(
                    config,
                    |acc, (src, target)| {
                        let re = Regex::new(src).unwrap();
                        acc.replace(re.find(&acc).unwrap().as_str(), target)
                    },
                );
                fs::write(path, cfg)
            })
            .unwrap();
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

    abcid_child.wait().c(d!()).map(|s| println!("{s}"))?;
    tendermint_child.wait().c(d!()).map(|s| println!("{s}"))?;

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
    let bin_path = format!("/tmp/{bin_name}");
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
