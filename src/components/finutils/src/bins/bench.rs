use clap::{App, Arg, SubCommand};

use ruc::{d, Result, RucResult};
use zei::xfr::{
    asset_record::open_blind_asset_record,
    sig::{XfrKeyPair, XfrPublicKey},
    structs::{OpenAssetRecord, XfrAssetType},
};

use zei::anon_xfr::structs::Commitment;

use finutils::common::{self, transfer_asset_batch_x, utils};
use globutils::wallet;

use ledger::data_model::{
    gen_random_keypair, ATxoSID, AssetRules, AssetTypeCode, Transaction, TxoSID, Utxo,
    ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY_STAKING,
};

use serde::{Deserialize, Serialize};

use std::fs;
use std::path::Path;
use std::time::Duration;

fn main() -> Result<()> {
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
                        .required(true)
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
                ),
        )
        .get_matches();

    if let Some(matches) = _matches.subcommand_matches("gen-utxos") {
        let key_pair_a = get_keypair(matches.value_of("mnemA").unwrap())?;
        let key_pair_b = get_keypair(matches.value_of("mnemB").unwrap())?;
        let key_file = matches.value_of("keys").unwrap();

        let keys = read_keys(key_file)?;

        let batch_size: usize = matches.value_of("batch-size").unwrap().parse().unwrap();
        println!("Generating UTXOs.");
        for i in 0..2 {
            println!("Round {}.", i + 1);
            generate_utxos(&key_pair_a, &key_pair_b, &keys, batch_size)?;
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

        let anon_keys =
            parse_anon_key_from_path(matches.value_of("anon-keys").unwrap())?;

        test_bar2abar(keys, anon_keys)?;
    } else if let Some(matches) = _matches.subcommand_matches("abar2bar") {
        let anon_keys =
            parse_anon_key_from_path(matches.value_of("anon-keys").unwrap())?;

        let commitments_file = matches.value_of("commitments").unwrap();
        let data = fs::read(commitments_file).c(d!())?;
        let commitments: Vec<PubkeyCommitment> =
            serde_json::from_slice(&data).c(d!())?;

        test_abar2bar(anon_keys, commitments)?;
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
    key_pair_b: &XfrKeyPair,
    keys: &[XfrKeyPair],
    batch_size: usize,
) -> Result<()> {
    let am = 10_000000;
    let len = keys.len();
    for i in 0..(len / batch_size) {
        let pks: Vec<XfrPublicKey> = (&keys[i * batch_size..(i + 1) * batch_size])
            .iter()
            .map(|kp| kp.get_pk())
            .collect();
        if i % 2 == 0 {
            transfer_asset_batch_x(key_pair_a, &pks, None, am, false, false)?;
        } else {
            transfer_asset_batch_x(key_pair_b, &pks, None, am, false, false)?;
            std::thread::sleep(Duration::from_secs(16));
        }
    }

    if len % batch_size != 0 {
        let left = len % batch_size;
        let pks: Vec<XfrPublicKey> = (&keys[len - left..len])
            .iter()
            .map(|kp| kp.get_pk())
            .collect();
        transfer_asset_batch_x(&key_pair_a, &pks, None, am, false, false)?;
    }

    Ok(())
}

fn test_bar2abar(keys: Vec<XfrKeyPair>, anon_keys: AnonKeys) -> Result<()> {
    let mut valid_keys: Vec<(XfrKeyPair, TxoSID, OpenAssetRecord)> =
        Vec::with_capacity(1024);

    for kp in keys.into_iter() {
        let mut list: Vec<(TxoSID, OpenAssetRecord)> =
            utils::get_owned_utxos(&kp.pub_key)?
                .iter()
                .filter(|a| {
                    // Filter by FRA
                    match a.1.clone().0 .0.record.asset_type {
                        XfrAssetType::Confidential(_) => false,
                        XfrAssetType::NonConfidential(x) => ASSET_TYPE_FRA == x,
                    }
                })
                .map(|(a, (utxo, owner_memo))| {
                    let oar = open_blind_asset_record(&utxo.0.record, &owner_memo, &kp)
                        .unwrap();
                    (*a, oar)
                })
                .collect();

        if list.len() >= 2 {
            let last = list.pop().unwrap();
            valid_keys.push((kp, last.0, last.1));
            println!("kp {}:{}", valid_keys.len(), list.len() + 1);
        }
    }

    let mut commitments: Vec<PubkeyCommitment> = Vec::with_capacity(1024);

    let target_addr = anon_keys.axfr_public_key;
    let owner_enc_key = anon_keys.enc_key;

    let to = wallet::anon_public_key_from_base64(&target_addr)
        .c(d!("invalid 'target-addr'"))?;
    // parse receiver XPubKey
    let enc_key = wallet::x_public_key_from_base64(&owner_enc_key)
        .c(d!("invalid owner_enc_key"))?;

    for (from, sid, oar) in valid_keys.into_iter() {
        //Generate the transaction and transmit it to network
        match utils::generate_bar2abar_op(&from, &to, sid, &oar, &enc_key, true) {
            Ok(c) => {
                commitments.push(PubkeyCommitment {
                    pubkey: from.pub_key,
                    commitment: c,
                });
                println!("sid: {}", sid);
            }
            Err(e) => {
                println!("{}", e);
            }
        }
    }

    write_commitments(&commitments)?;
    Ok(())
}

fn test_abar2bar(anon_keys: AnonKeys, commitments: Vec<PubkeyCommitment>) -> Result<()> {
    let axfr_secret_key = anon_keys.axfr_secret_key;
    let dec_key = anon_keys.dec_key;
    let mut txes = Vec::with_capacity(1024);

    for pk_commitment in commitments {
        let commitment = &pk_commitment.commitment;
        let to = &pk_commitment.pubkey;
        // Build transaction and submit to network
        let tx = common::abar2bar_tx(
            axfr_secret_key.clone(),
            commitment,
            dec_key.clone(),
            to,
            false,
            false,
        )
        .c(d!())?;

        txes.push(tx);
    }

    for tx in txes {
        utils::send_tx(&tx).c(d!())?;
    }

    Ok(())
}

fn parse_anon_key_from_path(path: &str) -> Result<AnonKeys> {
    let f = fs::read(path).c(d!())?;
    serde_json::from_slice::<AnonKeys>(&f).c(d!())
}

fn write_commitments(cmts: &[PubkeyCommitment]) -> Result<()> {
    let data = serde_json::to_string(&cmts).c(d!())?;
    fs::write("commitments.json", &data).c(d!())?;
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
