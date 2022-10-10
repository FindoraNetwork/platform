#[macro_use]
extern crate log;

use clap::{App, Arg, SubCommand};

use ruc::{d, Result, RucResult};
use zei::xfr::{
    asset_record::open_blind_asset_record,
    sig::{XfrKeyPair, XfrPublicKey},
    structs::{OpenAssetRecord, XfrAssetType},
};

use rayon::prelude::*;

use stopwatch::Stopwatch;

use zei::anon_xfr::structs::Commitment;

use finutils::common::{self, transfer_asset_batch_x, utils};
use globutils::wallet;

use ledger::data_model::{Transaction, TxoSID, ASSET_TYPE_FRA};

use serde::{Deserialize, Serialize};

use std::fs;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::Duration;

fn main() -> Result<()> {
    env_logger::init();

    let _matches = App::new("Triple masking bench")
        .subcommand(
            SubCommand::with_name("gen-utxos")
                .arg(
                    Arg::with_name("mnemA")
                        .long("mnemA")
                        .value_name("Mnemonic File")
                        .required(true)
                        .help("Mnemonic file")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("mnemB")
                        .long("mnemB")
                        .value_name("Mnemonic File")
                        .help("Mnemonic file")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("batch-size")
                        .short("n")
                        .long("batch-size")
                        .value_name("Batch Size")
                        .help("Numbers of transfer in a transaction.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("keys")
                        .long("keys")
                        .value_name("KEY FILE")
                        .help("File that contains a list of mnemonics.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("amount")
                        .long("amount")
                        .value_name("amount to transfer")
                        .help("Amount of FRA to transfer.")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("interval_secs")
                        .long("interval_secs")
                        .value_name("Anon-key File")
                        .help("Interval of sending transaction, seconds.")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("threads")
                        .long("threads")
                        .value_name("Threads")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("confidential_am")
                        .long("confidential_am")
                        .value_name("Boolean")
                        .help("Is confidential amount?.")
                        .takes_value(false),
                )
                .arg(
                    Arg::with_name("confidential_ty")
                        .long("confidential_ty")
                        .value_name("Boolean")
                        .help("Is confidential Asset type?.")
                        .takes_value(false),
                ),
        )
        .subcommand(
            SubCommand::with_name("cross-transfer")
                .arg(
                    Arg::with_name("batch-size")
                        .short("n")
                        .long("batch-size")
                        .value_name("Batch Size")
                        .help("Numbers of transfer in a transaction.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("keys")
                        .long("keys")
                        .value_name("KEY FILE")
                        .help("File that contains a list of mnemonics.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("amount")
                        .long("amount")
                        .value_name("amount to transfer")
                        .help("Amount of FRA to transfer.")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("interval_secs")
                        .long("interval_secs")
                        .required(true)
                        .value_name("Anon-key File")
                        .help("Interval of sending transaction, seconds.")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("threads")
                        .long("threads")
                        .required(true)
                        .value_name("Threads")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("confidential_am")
                        .long("confidential_am")
                        .value_name("Boolean")
                        .help("Is confidential amount?.")
                        .takes_value(false),
                )
                .arg(
                    Arg::with_name("confidential_ty")
                        .long("confidential_ty")
                        .value_name("Boolean")
                        .help("Is confidential Asset type?.")
                        .takes_value(false),
                ),
        )
        .subcommand(
            SubCommand::with_name("gen-keys").arg(
                Arg::with_name("n")
                    .short("n")
                    .required(true)
                    .value_name("Number")
                    .help("Generate n mnemonics.")
                    .takes_value(true),
            ),
        )
        .subcommand(
            SubCommand::with_name("bar2abar")
                .arg(
                    Arg::with_name("keys")
                        .long("keys")
                        .value_name("Key File")
                        .help("mnemonics list.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("anon-keys")
                        .long("anon-keys")
                        .value_name("Anon-key File")
                        .help("File contains anon-key.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("interval_secs")
                        .long("interval_secs")
                        .required(true)
                        .value_name("Anon-key File")
                        .help("Interval of sending transaction, seconds.")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("threads")
                        .long("threads")
                        .required(true)
                        .value_name("Threads")
                        .takes_value(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("abar2bar")
                .arg(
                    Arg::with_name("commitments")
                        .long("commitments")
                        .value_name("Commitments File")
                        .help("list that contains public keys and commitments.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("anon-keys")
                        .long("anon-keys")
                        .value_name("Anon-key File")
                        .help("File contains anon-key.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("interval_secs")
                        .long("interval_secs")
                        .value_name("Anon-key File")
                        .help("Interval of sending transaction, seconds.")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("threads")
                        .long("threads")
                        .value_name("Threads")
                        .takes_value(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("anon-transfer")
                .arg(
                    Arg::with_name("commitments")
                        .long("commitments")
                        .value_name("Commitments File")
                        .help("list that contains public keys and commitments.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("fee-commitments")
                        .long("fee-commitments")
                        .value_name("Commitments File")
                        .help("list that contains public keys and commitments.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("anon-keys")
                        .long("anon-keys")
                        .value_name("Anon-key File")
                        .help("File contains anon-key.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("to-anon-keys")
                        .long("to-anon-keys")
                        .value_name("Anon-key File")
                        .help("File contains anon-key.")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("interval_secs")
                        .long("interval_secs")
                        .required(true)
                        .value_name("Anon-key File")
                        .help("Interval of sending transaction, seconds.")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("threads")
                        .long("threads")
                        .required(true)
                        .value_name("Threads")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("amount")
                        .long("amount")
                        .value_name("Amount")
                        .help("Amount to transfer.")
                        .takes_value(true),
                ),
        )
        .get_matches();

    if let Some(matches) = _matches.subcommand_matches("gen-utxos") {
        let key_pair_a = get_keypair(matches.value_of("mnemA").unwrap())?;
        let key_pair_b = match matches.value_of("mnemB") {
            Some(m) => Some(get_keypair(m)?),
            None => None,
        };
        let key_file = matches.value_of("keys").unwrap();

        let keys = read_keys(key_file)?;

        let interval: u64 = matches
            .value_of("interval_secs")
            .unwrap_or("16")
            .parse()
            .unwrap();
        let amount: u64 = matches
            .value_of("amount")
            .unwrap_or("100000000")
            .parse()
            .unwrap();

        let confidential_am = matches.is_present("confidential_am");
        let confidential_ty = matches.is_present("confidential_ty");

        println!(
            "Is confidential amount:{}, confidential asset type {}",
            confidential_am, confidential_ty
        );

        let batch_size: usize = matches.value_of("batch-size").unwrap().parse().unwrap();

        for i in 0..2 {
            println!("Generating Utxos, Round {}.", i + 1);
            generate_utxos(
                &key_pair_a,
                key_pair_b.as_ref(),
                &keys,
                batch_size,
                amount,
                Duration::from_secs(interval),
                confidential_am,
                confidential_ty,
            )?;
        }
    } else if let Some(matches) = _matches.subcommand_matches("gen-keys") {
        let n: u64 = matches.value_of("n").unwrap().parse().unwrap();
        let mut buff = String::new();
        for _ in 0..n {
            let mne = wallet::generate_mnemonic_custom(24, "en")?;
            buff.push_str(&mne);
            buff.push('\n');
        }
        fs::write("mnemonices.txt", &buff).unwrap();
    } else if let Some(matches) = _matches.subcommand_matches("bar2abar") {
        let key_file = matches.value_of("keys").unwrap();

        let keys = read_keys(key_file)?;

        let interval: u64 = matches.value_of("interval_secs").unwrap().parse().unwrap();

        let anon_keys =
            parse_anon_key_from_path(matches.value_of("anon-keys").unwrap())?;

        let threads: usize = matches.value_of("threads").unwrap().parse().unwrap();

        for i in 0..2 {
            println!("Bar to Abar Round {}", i + 1);
            test_bar2abar(&keys, &anon_keys, i, Duration::from_secs(interval), threads)?;
            std::thread::sleep(Duration::from_secs(16));
        }
    } else if let Some(matches) = _matches.subcommand_matches("abar2bar") {
        let anon_keys =
            parse_anon_key_from_path(matches.value_of("anon-keys").unwrap())?;

        let threads: usize = matches.value_of("threads").unwrap_or("4").parse().unwrap();

        let interval: u64 = matches
            .value_of("interval_secs")
            .unwrap_or("4")
            .parse()
            .unwrap();

        let commitments_file = matches.value_of("commitments").unwrap();
        let commitments: Vec<PubkeyCommitment> = read_pk_commitments(commitments_file)?;

        test_abar2bar(
            anon_keys,
            commitments,
            Duration::from_secs(interval),
            threads,
        )?;
    } else if let Some(matches) = _matches.subcommand_matches("anon-transfer") {
        let commitments = read_pk_commitments(matches.value_of("commitments").unwrap())?;
        let fee_commitments =
            read_pk_commitments(matches.value_of("fee-commitments").unwrap())?;

        let anon_keys =
            parse_anon_key_from_path(matches.value_of("anon-keys").unwrap())?;
        let to_anon_keys =
            parse_anon_key_from_path(matches.value_of("to-anon-keys").unwrap())?;

        let interval: u64 = matches.value_of("interval_secs").unwrap().parse().unwrap();

        let threads: usize = matches.value_of("threads").unwrap().parse().unwrap();

        let amount = matches.value_of("amount").unwrap_or("1000000");

        let axfr_amount = amount.parse::<u64>().c(d!("error parsing amount"))?;

        test_anon_transfer(
            anon_keys,
            commitments,
            fee_commitments,
            to_anon_keys,
            axfr_amount,
            Duration::from_secs(interval),
            threads,
        )?;
    } else if let Some(matches) = _matches.subcommand_matches("cross-transfer") {
        let threads: usize = matches.value_of("threads").unwrap().parse().unwrap();
        let amount: u64 = matches
            .value_of("amount")
            .unwrap_or("1000000")
            .parse()
            .unwrap();

        let key_file = matches.value_of("keys").unwrap();

        let key_pairs = read_keys(key_file)?;
        let interval: u64 = matches.value_of("interval_secs").unwrap().parse().unwrap();

        let batch_size: usize = matches.value_of("batch-size").unwrap().parse().unwrap();

        let confidential_am = matches.is_present("confidential_am");
        let confidential_ty = matches.is_present("confidential_ty");

        println!(
            "Is confidential amount:{}, confidential asset type {}",
            confidential_am, confidential_ty
        );

        cross_transfer(
            &key_pairs,
            batch_size,
            threads,
            amount,
            Duration::from_secs(interval),
            confidential_am,
            confidential_ty,
        )?;
    } else {
        panic!("Unknown subcommands.")
    }

    Ok(())
}

fn get_keypair(m_path: impl AsRef<Path>) -> Result<XfrKeyPair> {
    let words = fs::read_to_string(m_path.as_ref()).c(d!(format!(
        "can not read mnemonic from '{:?}'",
        m_path.as_ref()
    )))?;

    let k = words.trim();
    wallet::restore_keypair_from_mnemonic_default(k).c(d!("invalid 'owner-mnemonic'"))
}

fn read_keys(path: impl AsRef<Path>) -> Result<Vec<XfrKeyPair>> {
    let mnemonics = fs::read_to_string(path).c(d!())?;
    let mut keys = Vec::with_capacity(1024);

    for words in mnemonics.split("\n") {
        if words.len() != 0 {
            keys.push(
                wallet::restore_keypair_from_mnemonic_default(words)
                    .c(d!("invalid 'owner-mnemonic'"))?,
            );
        }
    }

    return Ok(keys);
}

fn generate_utxos(
    key_pair_a: &XfrKeyPair,
    key_pair_b: Option<&XfrKeyPair>,
    keys: &[XfrKeyPair],
    batch_size: usize,
    am: u64,
    interval: Duration,
    confidential_am: bool,
    confidential_ty: bool,
) -> Result<()> {
    let len = keys.len();
    for i in 0..(len / batch_size) {
        let pks: Vec<XfrPublicKey> = (&keys[i * batch_size..(i + 1) * batch_size])
            .iter()
            .map(|kp| kp.get_pk())
            .collect();

        transfer_asset_batch_x(key_pair_a, &pks, None, am, false, false)?;
        if let Some(kp) = key_pair_b {
            transfer_asset_batch_x(kp, &pks, None, am, false, false)?;
        }

        std::thread::sleep(interval);
    }

    if len % batch_size != 0 {
        let left = len % batch_size;
        let pks: Vec<XfrPublicKey> = (&keys[len - left..len])
            .iter()
            .map(|kp| kp.get_pk())
            .collect();
        transfer_asset_batch_x(
            &key_pair_a,
            &pks,
            None,
            am,
            confidential_am,
            confidential_ty,
        )?;
    }

    Ok(())
}

fn cross_transfer(
    key_pairs: &[XfrKeyPair],
    batch_size: usize,
    threads: usize,
    am: u64,
    interval: Duration,
    confidential_am: bool,
    confidential_ty: bool,
) -> Result<()> {
    let len = key_pairs.len();
    let mut txes: Vec<_> = Vec::with_capacity(1024);
    info!("Generating tx...");
    for (i, from) in key_pairs.iter().enumerate() {
        let mut targets: Vec<(_, _)> = Vec::with_capacity(32);
        for j in (i + 1)..(i + 1 + batch_size) {
            targets.push((key_pairs.get(j % len).unwrap().get_pk(), am));
        }

        let targets: Vec<_> = targets.iter().map(|(pk, am)| (pk, *am)).collect();
        txes.push(utils::transfer_batch_tx(
            from,
            targets,
            None,
            confidential_am,
            confidential_ty,
        ));

        if (i + 1) % 10 == 0 {
            info!("Generated {}.", i + 1)
        }
    }

    let txes = Arc::new(Mutex::new(txes));

    let mut handles = Vec::new();
    for i in 0..threads {
        let txes = txes.clone();
        let h = std::thread::spawn(move || loop {
            let tx = { txes.lock().unwrap().pop() };
            if tx.is_none() {
                return;
            }
            match tx.unwrap() {
                Ok(t) => {
                    info!("Thread-{} Sending tx ...", i);
                    let sw = Stopwatch::start_new();
                    if utils::send_tx(&t).is_err() {
                        if let Err(e) = utils::send_tx(&t) {
                            error!("{}", e);
                        }
                    }
                    info!("Thread-{} Sending tx took {}ms", i, sw.elapsed_ms());
                }
                Err(e) => error!("{}", e),
            }
            std::thread::sleep(interval);
        });

        handles.push(h);
    }

    for h in handles.into_iter() {
        h.join().unwrap();
    }

    Ok(())
}

fn test_bar2abar(
    keys: &[XfrKeyPair],
    anon_keys: &AnonKeys,
    round: usize,
    interval: Duration,
    threads: usize,
) -> Result<()> {
    let mut valid_keys: Vec<(XfrKeyPair, TxoSID, OpenAssetRecord)> =
        Vec::with_capacity(1024);

    for kp in keys.into_iter() {
        let mut utxo = utils::get_owned_utxos(&kp.pub_key);
        if utxo.is_err() {
            utxo = utils::get_owned_utxos(&kp.pub_key);
        }
        match utxo {
            Ok(u) => {
                let mut list: Vec<(TxoSID, OpenAssetRecord)> = u
                    .iter()
                    .filter(|a| {
                        // Filter by FRA
                        match a.1.clone().0 .0.record.asset_type {
                            XfrAssetType::Confidential(_) => false,
                            XfrAssetType::NonConfidential(x) => ASSET_TYPE_FRA == x,
                        }
                    })
                    .map(|(a, (utxo, owner_memo))| {
                        let oar =
                            open_blind_asset_record(&utxo.0.record, &owner_memo, &kp)
                                .unwrap();
                        (*a, oar)
                    })
                    .collect();

                if list.len() >= 2 {
                    let last = list.pop().unwrap();
                    valid_keys.push((kp.clone(), last.0, last.1));
                    println!(
                        "keypair No.{}: utxo count: {}",
                        valid_keys.len(),
                        list.len() + 1
                    );
                } else {
                    println!("omit...");
                }
            }
            Err(e) => println!("{:?}, omit...", e),
        }
    }

    let commitments: Arc<Mutex<Vec<PubkeyCommitment>>> =
        Arc::new(Mutex::new(Vec::with_capacity(1024)));

    let target_addr = &anon_keys.axfr_public_key;
    let _owner_enc_key = &anon_keys.enc_key;

    let to = wallet::anon_public_key_from_base64(target_addr)
        .c(d!("invalid 'target-addr'"))?;


    println!("Generating transactions...");
    let txes: Vec<(_, _, _)> = valid_keys
        .into_iter()
        .map(|(from, sid, oar)| {
            match utils::generate_bar2abar_tx(&from, &to, sid, &oar,  true) {
                Ok((tx, c)) => Ok((tx, c, from)),
                Err(e) => {
                    println!("{}", &e);
                    Err(e)
                }
            }
        })
        .filter(|r| r.is_ok())
        .map(|r| r.unwrap())
        .collect();

    let txes = Arc::new(Mutex::new(txes));
    let mut handles = vec![];
    for i in 0..threads {
        let txes = txes.clone();
        let commitments = commitments.clone();
        let h = std::thread::spawn(move || loop {
            let res = { txes.lock().unwrap().pop() };
            if res.is_none() {
                return;
            }
            let (tx, c, from) = res.unwrap();
            //Generate the transaction and transmit it to network
            info!("Thread-{} Sending tx ...", i);
            let sw = Stopwatch::start_new();
            match utils::send_tx(&tx) {
                Ok(_) => {
                    commitments.lock().unwrap().push(PubkeyCommitment {
                        pubkey: from.pub_key,
                        commitment: c,
                    });
                }
                Err(_) => match utils::send_tx(&tx) {
                    Ok(_) => {
                        commitments.lock().unwrap().push(PubkeyCommitment {
                            pubkey: from.pub_key,
                            commitment: c,
                        });
                    }
                    Err(e) => {
                        error!("{}", e);
                    }
                },
            }
            info!("Thread-{} Sending tx took {}ms", i, sw.elapsed_ms());
            std::thread::sleep(interval);
        });
        handles.push(h);
    }
    for h in handles {
        h.join().unwrap();
    }
    write_commitments(&*commitments.lock().unwrap(), round)?;
    Ok(())
}

fn test_abar2bar(
    anon_keys: AnonKeys,
    commitments: Vec<PubkeyCommitment>,
    interval: Duration,
    threads: usize,
) -> Result<()> {
    let axfr_secret_key = anon_keys.axfr_secret_key;

    println!("Generating transactions...");
    let txes: Vec<Result<Transaction>> = commitments
        .into_par_iter()
        .map(|pk_commitment| {
            let commitment = &pk_commitment.commitment;
            let to = &pk_commitment.pubkey;
            // Build transaction and submit to network
            let tx = common::abar2bar_tx(
                axfr_secret_key.clone(),
                commitment,
                to,
                false,
                false,
            ).c(d!());

            tx
        })
        .collect();

    let txes = Arc::new(Mutex::new(txes));
    let mut handles = Vec::new();
    for i in 0..threads {
        let txes = txes.clone();
        let h = std::thread::spawn(move || loop {
            let tx = { txes.lock().unwrap().pop() };
            if tx.is_none() {
                return;
            }
            match tx.unwrap() {
                Ok(t) => {
                    info!("Thread-{} Sending tx ...", i);
                    let sw = Stopwatch::start_new();
                    if utils::send_tx(&t).is_err() {
                        if let Err(e) = utils::send_tx(&t) {
                            println!("{}", e);
                        }
                    }
                    info!("Thread-{} Sending tx took {}ms", i, sw.elapsed_ms());
                }
                Err(e) => println!("{}", e),
            }
            std::thread::sleep(interval);
        });

        handles.push(h);
    }

    for h in handles.into_iter() {
        h.join().unwrap();
    }
    Ok(())
}

fn test_anon_transfer(
    anon_keys: AnonKeys,
    commitments: Vec<PubkeyCommitment>,
    fee_commitments: Vec<PubkeyCommitment>,
    to_anon_keys: AnonKeys,
    axfr_amount: u64,
    interval: Duration,
    threads: usize,
) -> Result<()> {
    info!("Generating transactions...");
    let txes: Vec<Result<_>> = commitments
        .into_par_iter()
        .zip(fee_commitments.into_par_iter())
        .map(|(com1, com2)| {
            common::gen_anon_transfer_tx(
                &anon_keys.axfr_secret_key,
                &com1.commitment,
                Some(&com2.commitment),
                axfr_amount,
                &to_anon_keys.axfr_public_key,
            )
        })
        .collect();

    let txes = Arc::new(Mutex::new(txes));
    let mut handles = vec![];
    for i in 0..threads {
        let txes = txes.clone();
        let h = std::thread::spawn(move || loop {
            let tx = { txes.lock().unwrap().pop() };
            if tx.is_none() {
                return;
            }

            match tx.unwrap() {
                Ok((t, _, _)) => {
                    info!("Thread-{} Sending tx ...", i);
                    let sw = Stopwatch::start_new();
                    if let Err(_) = utils::send_tx(&t) {
                        if let Err(e) = utils::send_tx(&t) {
                            println!("{}", e);
                        }
                    }
                    info!("Thread-{} Sending tx took {}ms", i, sw.elapsed_ms());
                }
                Err(e) => println!("{}", e),
            };
            std::thread::sleep(interval);
        });
        handles.push(h);
    }

    for h in handles.into_iter() {
        h.join().unwrap();
    }

    Ok(())
}

fn parse_anon_key_from_path(path: &str) -> Result<AnonKeys> {
    let f = fs::read(path).c(d!())?;
    serde_json::from_slice::<AnonKeys>(&f).c(d!())
}

fn write_commitments(cmts: &[PubkeyCommitment], round: usize) -> Result<()> {
    let data = serde_json::to_string(&cmts).c(d!())?;
    fs::write(format!("commitments_{}.json", round), &data).c(d!())?;
    Ok(())
}

#[derive(Clone, Deserialize, Serialize)]
pub struct AnonKeys {
    pub axfr_secret_key: String,
    pub axfr_public_key: String,
    pub enc_key: String,
    pub dec_key: String,
}

#[derive(Clone, Deserialize, Serialize)]
struct PubkeyCommitment {
    pubkey: XfrPublicKey,
    commitment: Commitment,
}

fn read_pk_commitments(path: impl AsRef<Path>) -> Result<Vec<PubkeyCommitment>> {
    let data = fs::read(path).c(d!())?;
    let commitments: Vec<PubkeyCommitment> = serde_json::from_slice(&data).c(d!())?;

    Ok(commitments)
}
