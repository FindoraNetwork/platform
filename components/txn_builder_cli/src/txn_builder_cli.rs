#![deny(warnings)]
use clap::{App, Arg, SubCommand};
use env_logger::{Env, Target};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AccountAddress, AssetTypeCode, TransferType, TxOutput, TxoRef, TxoSID};
use log::trace; // Other options: debug, info, warn
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::env;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::exit;
use submission_server::{TxnHandle, TxnStatus};
use txn_builder::{BuildsTransactions, TransactionBuilder, TransferOperationBuilder};
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record, AssetRecordType};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{AssetRecord, BlindAssetRecord};

extern crate exitcode;

const HOST: &str = "testnet.findora.org";
const SUBMIT_PORT: &str = "8669";

// TODO (Keyao): Table to store data

//
// Load functions
//
fn load_txn_builder_from_file(file_path: &str) -> Result<TransactionBuilder, PlatformError> {
  let mut file;
  match File::open(file_path) {
    Ok(f) => {
      file = f;
    }
    Err(_) => {
      println!("Transaction file {} does not exist. Try subcommand create.",
               file_path);
      exit(exitcode::NOINPUT)
    }
  }
  let mut txn = String::new();
  if file.read_to_string(&mut txn).is_err() {
    println!("Failed to read transaction file {}", file_path);
    exit(exitcode::NOINPUT)
  }
  println!("Parsing builder from file contents: \"{}\"", &txn);
  let builder = serde_json::from_str(&txn)?;
  Ok(builder)
}

fn load_key_pair_from_file(file_path: &str) -> Result<XfrKeyPair, PlatformError> {
  let mut file;
  match File::open(file_path) {
    Ok(f) => {
      file = f;
    }
    Err(_) => {
      println!("Key pair file {} does not exist. Try subcommand keygen.",
               file_path);
      exit(exitcode::NOINPUT)
    }
  }

  let kp: XfrKeyPair;
  let mut kp_byte_buffer = Vec::new();
  match file.read_to_end(&mut kp_byte_buffer) {
    Ok(_len) => {
      kp = XfrKeyPair::zei_from_bytes(&kp_byte_buffer);
    }
    Err(_e) => {
      println!("Failed to read key file {}", file_path);
      exit(exitcode::NOINPUT)
    }
  }
  Ok(kp)
}

fn load_pub_key_from_file(file_path: &str) -> Result<XfrPublicKey, PlatformError> {
  let mut file;
  match File::open(file_path) {
    Ok(f) => {
      file = f;
    }
    Err(_) => {
      println!("Public key file {} does not exist. Try subcommand pubkeygen.",
               file_path);
      exit(exitcode::NOINPUT)
    }
  }

  let key: XfrPublicKey;
  let mut key_byte_buffer = Vec::new();
  match file.read_to_end(&mut key_byte_buffer) {
    Ok(_len) => {
      key = XfrPublicKey::zei_from_bytes(&key_byte_buffer);
    }
    Err(_e) => {
      println!("Failed to read key file {}", file_path);
      exit(exitcode::NOINPUT)
    }
  }
  Ok(key)
}

fn split_arg(string: &str) -> Vec<&str> {
  string.split(',').collect::<Vec<&str>>()
}

fn load_sids_from_file(file_path: &str) -> Result<Vec<TxoRef>, PlatformError> {
  let mut file;
  match File::open(file_path) {
    Ok(f) => {
      file = f;
    }
    Err(_) => {
      println!("Sids file {} does not exist. Try subcommand store --sids.",
               file_path);
      exit(exitcode::NOINPUT)
    }
  }

  let mut sids_str = String::new();
  if file.read_to_string(&mut sids_str).is_err() {
    println!("Failed to read sids file {}", file_path);
    exit(exitcode::NOINPUT)
  }

  let mut txo_refs = Vec::new();
  for sid_str in split_arg(&sids_str) {
    if let Ok(sid) = sid_str.trim().parse::<u64>() {
      txo_refs.push(TxoRef::Absolute(TxoSID(sid)));
    } else {
      println!("Improperly formatted sid.");
      exit(exitcode::USAGE)
    }
  }

  Ok(txo_refs)
}

fn load_blind_asset_record_from_file(file_path: &str) -> Result<BlindAssetRecord, PlatformError> {
  let file_path = file_path.trim();
  let mut file;
  match File::open(file_path) {
    Ok(f) => {
      file = f;
    }
    Err(_) => {
      println!("Blind asset record file {} does not exist. Try subcommand store --blind_asset_record.",
                 file_path);
      exit(exitcode::NOINPUT)
    }
  }

  let mut blind_asset_record_str = String::new();
  if file.read_to_string(&mut blind_asset_record_str).is_err() {
    println!("Failed to read blind asset record file {}", file_path);
    exit(exitcode::NOINPUT)
  }

  if let Ok(blind_asset_record) = serde_json::from_str(&blind_asset_record_str) {
    Ok(blind_asset_record)
  } else {
    println!("Improperly formatted blind asset record.");
    exit(exitcode::USAGE)
  }
}

fn load_blind_asset_records_from_files(file_paths: &str)
                                       -> Result<Vec<BlindAssetRecord>, PlatformError> {
  let mut blind_asset_records = Vec::new();

  for mut file_path in split_arg(file_paths) {
    file_path = file_path.trim();
    let mut file;
    match File::open(file_path) {
      Ok(f) => {
        file = f;
      }
      Err(_) => {
        println!("Blind asset record file {} does not exist. Try subcommand store --blind_asset_record.",
                 file_path);
        exit(exitcode::NOINPUT)
      }
    }

    let mut blind_asset_record_str = String::new();
    if file.read_to_string(&mut blind_asset_record_str).is_err() {
      println!("Failed to read blind asset record file {}", file_path);
      exit(exitcode::NOINPUT)
    }

    if let Ok(blind_asset_record) = serde_json::from_str(&blind_asset_record_str) {
      blind_asset_records.push(blind_asset_record);
    } else {
      println!("Improperly formatted blind asset record.");
      exit(exitcode::USAGE)
    }
  }

  Ok(blind_asset_records)
}

fn load_addresses_from_files(file_paths: &str) -> Result<Vec<AccountAddress>, PlatformError> {
  let mut addresses = Vec::new();

  for file_path in split_arg(file_paths) {
    let address_key;
    match load_pub_key_from_file(file_path.trim()) {
      Ok(key) => {
        address_key = key;
      }
      Err(_) => {
        println!("Failed to load address key from file {}", file_path);
        exit(exitcode::NOINPUT)
      }
    }
    addresses.push(AccountAddress { key: address_key });
  }

  Ok(addresses)
}

//
// Store functions
//
fn store_txn_builder_to_file(file_path: &str,
                             txn: &TransactionBuilder)
                             -> Result<(), PlatformError> {
  if let Ok(as_json) = serde_json::to_string(txn) {
    if fs::write(file_path, &as_json).is_err() {
      println!("Transaction file {} could not be created", file_path);
      exit(exitcode::CANTCREAT)
    };
  }

  Ok(())
}

// Write a new key pair to the given paths.
// Create subdirectories as needed.
// Move aside any extant files at the given paths.
// Assumes tilde expansion has already been done on paths.
fn store_key_pair_to_file(path_str: &str) -> Result<(), PlatformError> {
  let file_path = Path::new(path_str);
  match fs::create_dir_all(&file_path.parent().unwrap()) {
    Ok(()) => {
      let mut prng: ChaChaRng;
      prng = ChaChaRng::from_seed([0u8; 32]);
      let key_pair = XfrKeyPair::generate(&mut prng);
      match fs::write(&file_path, key_pair.zei_to_bytes()) {
        Ok(_) => {}
        Err(error) => {
          println!("Key pair file {:?} could not be created: {}",
                   file_path, error);
          exit(exitcode::CANTCREAT)
        }
      };
    }
    Err(error) => {
      println!("Failed to create directories for {}: {}",
               &file_path.display(),
               error);
      exit(exitcode::CANTCREAT)
    }
  }

  Ok(())
}

// Write a new public key to the given paths.
// Create subdirectories as needed.
// Move aside any extant files at the given paths.
// Assumes tilde expansion has already been done on paths.
fn store_pub_key_to_file(path_str: &str) -> Result<(), PlatformError> {
  let file_path = Path::new(path_str);
  match fs::create_dir_all(&file_path.parent().unwrap()) {
    Ok(()) => {
      let mut prng = ChaChaRng::from_seed([0u8; 32]);
      let key_pair = XfrKeyPair::generate(&mut prng);
      match fs::write(&file_path, key_pair.get_pk_ref().as_bytes()) {
        Ok(_) => {}
        Err(error) => {
          println!("Public key file {:?} could not be created: {}",
                   file_path, error);
          exit(exitcode::CANTCREAT)
        }
      };
    }
    Err(error) => {
      println!("Failed to create directories for {}: {}",
               &file_path.display(),
               error);
      exit(exitcode::CANTCREAT)
    }
  }

  Ok(())
}

fn store_sids_to_file(file_path: &str, sids: &str) -> Result<(), PlatformError> {
  if fs::write(file_path, sids).is_err() {
    println!("Sids file {} could not be created", file_path);
    exit(exitcode::CANTCREAT)
  };
  Ok(())
}

fn store_blind_asset_record(file_path: &str,
                            amount: &str,
                            asset_type: &str,
                            pub_key_path: &str,
                            confidential_amount: bool,
                            confidential_asset: bool)
                            -> Result<(), PlatformError> {
  let mut asset_type_arr = [0u8; 16];
  let bytes = asset_type.as_bytes();
  asset_type_arr.copy_from_slice(&bytes[..16]);

  let asset_record = AssetRecord::new(amount.parse::<u64>().unwrap(),
                                      asset_type_arr,
                                      load_pub_key_from_file(pub_key_path).unwrap()).unwrap();

  let blind_asset_record =
    build_blind_asset_record(&mut ChaChaRng::from_entropy(),
                             &PublicParams::new().pc_gens,
                             &asset_record,
                             AssetRecordType::from_booleans(confidential_amount,
                                                            confidential_asset),
                             &None);

  if let Ok(as_json) = serde_json::to_string(&blind_asset_record) {
    if fs::write(file_path, &as_json).is_err() {
      println!("Blind asset record file {} could not be created", file_path);
      exit(exitcode::CANTCREAT)
    };
  }

  Ok(())
}

fn create_directory_if_missing(path_to_file_in_dir: &str) {
  let as_path = Path::new(path_to_file_in_dir);
  if as_path.exists() {
    return;
  }

  if let Some(parent) = as_path.parent() {
    if parent.exists() {
      return;
    }
    if let Err(_e) = fs::create_dir_all(&parent) {}
  }
}

const BACKUP_COUNT_MAX: i32 = 10000; // Arbitrary choice.

// Find a backup file name not currently in use.
// Assumes the extension of path can be replaced by n.
// Assumes it is safe to check the existence of the path after doing so.
// This implies all path components of path must exist and be readable.
// Assumes recursion won't hurt us here.
fn find_available_path(path: &Path, n: i32) -> Result<PathBuf, ()> {
  if n < BACKUP_COUNT_MAX {
    let path_n = path.with_extension(&n.to_string());
    if path_n.exists() {
      find_available_path(path, n + 1)
    } else {
      Ok(path_n)
    }
  } else {
    println!("Too many backups for {:?}. Use --path to specify another path.",
             path);
    exit(exitcode::IOERR)
  }
}

// Return a backup file path derived from path or exit with usage message if an
// unused path cannot be derived. The path must not be empty
// and must not be dot (".").
fn next_path(path: &Path) -> Result<PathBuf, ()> {
  fn add_backup_extension(path: &Path) -> PathBuf {
    let mut pb = PathBuf::from(path);
    pb.set_file_name(format!("{}.0",
                             path.file_name()
                                 .unwrap_or_else(|| OsStr::new(""))
                                 .to_str()
                                 .unwrap_or("")));
    pb
  }

  if let Some(ext) = path.extension() {
    if let Ok(n) = ext.to_str().unwrap().parse::<i32>() {
      // Has a numeric extension
      find_available_path(path, n)
    } else {
      // Doesn't have a numeric extension
      find_available_path(&add_backup_extension(&path), 0)
    }
  } else {
    // Doesn't have any extension.
    if path.components().next() == None {
      println!("Is empty: {:?}. Specify a file path.", path);
      exit(exitcode::USAGE)
    } else if path.file_name() == None {
      println!("Is directory: {:?}. Specify a file path.", path);
      exit(exitcode::USAGE)
    } else {
      find_available_path(&add_backup_extension(&path), 0)
    }
  }
}

fn rename_existing_path(path: &Path) {
  if let Ok(next) = next_path(path) {
    trace!("Next path for {:?} is {:?}", &path, &next);
    if let Err(error) = fs::rename(path, next.as_path()) {
      println!("Failed to rename path {} to {}: {}",
               path.to_str().unwrap(),
               next.to_str().unwrap(),
               error);
    }
  }
}

fn get_amount(amount_arg: &str) -> Result<u64, ()> {
  if let Ok(amount) = amount_arg.trim().parse::<u64>() {
    Ok(amount)
  } else {
    exit(exitcode::USAGE)
  }
}

fn get_amounts(amounts_arg: &str) -> Result<Vec<u64>, ()> {
  let amounts_str = split_arg(amounts_arg);
  let mut amounts = Vec::new();
  for amount_str in amounts_str {
    if let Ok(amount) = amount_str.trim().parse::<u64>() {
      amounts.push(amount);
    } else {
      exit(exitcode::USAGE)
    }
  }
  Ok(amounts)
}

fn submit(protocol: &str, transaction_file_name: &str) -> Result<(), PlatformError> {
  // Submit transaction
  let txn_builder = &mut load_txn_builder_from_file(transaction_file_name).unwrap();
  let client = reqwest::Client::new();
  let txn = txn_builder.transaction().clone();
  let mut res = client.post(&format!("{}://{}:{}/{}",
                                     protocol, HOST, SUBMIT_PORT, "submit_transaction"))
                      .json(&txn)
                      .send()
                      .unwrap();
  // Log body
  println!("Response: {}",
           res.json::<TxnHandle>().expect("<Invalid JSON>"));
  println!("Status: {}", res.status());
  println!("Headers:\n{:?}", res.headers());
  Ok(())
}
fn submit_and_get_sid(protocol: &str,
                      transaction_file_name: &str)
                      -> Result<TxoSID, PlatformError> {
  // Submit transaction
  let txn_builder = &mut load_txn_builder_from_file(transaction_file_name).unwrap();
  let client = reqwest::Client::new();
  let txn = txn_builder.transaction().clone();
  let mut res = client.post(&format!("{}://{}:{}/{}",
                                     protocol, HOST, SUBMIT_PORT, "submit_transaction"))
                      .json(&txn)
                      .send()
                      .unwrap();
  // Log body
  println!("Response: {}",
           res.json::<TxnHandle>().expect("<Invalid JSON>"));
  println!("Status: {}", res.status());
  println!("Headers:\n{:?}", res.headers());

  // Get sid
  let handle = serde_json::from_str::<TxnHandle>(&res.text().unwrap()).unwrap();

  let res = query(protocol, "txn_status", &handle.0);
  match serde_json::from_str::<TxnStatus>(&res).unwrap() {
    TxnStatus::Committed((_sid, txos)) => Ok(txos[0]),
    _ => Err(PlatformError::DeserializationError),
  }
}

fn query(protocol: &str, item: &str, value: &str) -> String {
  let mut res =
    reqwest::get(&format!("{}://{}:{}/{}/{}", protocol, HOST, SUBMIT_PORT, item, value)).unwrap();

  // Log body
  println!("Response: {}",
           res.json::<TxnHandle>().expect("<Invalid JSON>"));
  println!("Status: {}", res.status());
  println!("Headers:\n{:?}", res.headers());

  res.text().unwrap()
}

fn issue_and_transfer(sid: TxoRef,
                      blind_asset_record: BlindAssetRecord,
                      issuer_key_pair: &XfrKeyPair,
                      recipient_pub_key: &XfrPublicKey,
                      amount: u64,
                      token_code: AssetTypeCode,
                      transaction_file_name: &str,
                      seq_num: u64)
                      -> Result<(), PlatformError> {
  let txn_builder = &mut load_txn_builder_from_file(&transaction_file_name).unwrap();

  // Transfer Operation
  let xfr_op =
    TransferOperationBuilder::new().add_input(sid,
                                              open_asset_record(&blind_asset_record,
                                                                issuer_key_pair.get_sk_ref())?,
                                              amount)?
                                   .add_output(amount, recipient_pub_key, token_code)?
                                   .balance()?
                                   .create(TransferType::Standard)?
                                   .sign(issuer_key_pair)?
                                   .transaction()?;

  // Issue and Transfer transaction
  txn_builder.add_operation_issue_asset(issuer_key_pair,
                                        &token_code,
                                        seq_num,
                                        &[TxOutput(blind_asset_record)])?
             .add_operation(xfr_op)
             .transaction();

  if let Err(e) = store_txn_builder_to_file(&transaction_file_name, &txn_builder) {
    println!("Failed to store txn builder to file.");
    return Err(e);
  };
  Ok(())
}

fn merge_records(key_pair: &XfrKeyPair,
                 sid1: TxoRef,
                 sid2: TxoRef,
                 blind_asset_record1: BlindAssetRecord,
                 blind_asset_record2: BlindAssetRecord,
                 token_code: AssetTypeCode,
                 transaction_file_name: &str)
                 -> Result<(), PlatformError> {
  let txn_builder = &mut load_txn_builder_from_file(&transaction_file_name).unwrap();
  let oar1 = open_asset_record(&blind_asset_record1, key_pair.get_sk_ref())?;
  let oar2 = open_asset_record(&blind_asset_record2, key_pair.get_sk_ref())?;
  let amount1 = *oar1.get_amount();
  let amount2 = *oar2.get_amount();

  // Transfer Operation
  let xfr_op =
    TransferOperationBuilder::new().add_input(sid1, oar1, amount1)?
                                   .add_input(sid2, oar2, amount2)?
                                   .add_output(amount1 + amount2,
                                               key_pair.get_pk_ref(),
                                               token_code)?
                                   .create(TransferType::Standard)?
                                   .sign(key_pair)?
                                   .transaction()?;

  // Merge records
  txn_builder.add_operation(xfr_op).transaction();

  if let Err(e) = store_txn_builder_to_file(&transaction_file_name, &txn_builder) {
    println!("Failed to store txn builder to file.");
    return Err(e);
  };
  Ok(())
}

fn load_funds(sid_pre: TxoRef,
              sid_new: TxoRef,
              blind_asset_record_pre: BlindAssetRecord,
              blind_asset_record_new: BlindAssetRecord,
              issuer_key_pair: &XfrKeyPair,
              recipient_key_pair: &XfrKeyPair,
              amount: u64,
              token_code: AssetTypeCode,
              transaction_file_name: &str,
              sequence_num: u64,
              protocol: &str)
              -> Result<(), PlatformError> {
  // Issue and transfer asset
  match issue_and_transfer(sid_new,
                           blind_asset_record_new,
                           issuer_key_pair,
                           recipient_key_pair.get_pk_ref(),
                           amount,
                           token_code,
                           transaction_file_name,
                           sequence_num)
  {
    Ok(_) => {}
    Err(error) => {
      println!("Failed to issue and transfer");
      return Err(error);
    }
  };
  println!("612");
  // Submit transaction
  let sid_next = match submit_and_get_sid(protocol, transaction_file_name) {
    Ok(sid) => {
      println!("Next sid: {}", sid.0);
      sid
    }
    _ => {
      return Err(PlatformError::SubmissionServerError(Some("Failed to submit transaction".into())));
    }
  };
  println!("622");

  // Get next blind asset record
  let res = query(protocol, "utxo_sid", &format!("{}", sid_next.0));
  let blind_asset_record_next = serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| {
                                  return Err(PlatformError::DeserializationError);
                                })
                                .unwrap();

  // Merge records
  merge_records(recipient_key_pair,
                sid_pre,
                TxoRef::Absolute(sid_next),
                blind_asset_record_pre,
                blind_asset_record_next,
                token_code,
                transaction_file_name).unwrap();

  // Submit transaction
  if submit(protocol, transaction_file_name).is_err() {
    return Err(PlatformError::SubmissionServerError(Some("Failed to submit transaction".into())));
  };
  Ok(())
}

// Use environment variable RUST_LOG to select log level and filter
// output by module or regex. For example,
//
//    RUST_LOG=ledger::data_model=info,main=trace/rec[ie]+ve ./main
//
// TODO Verify that this comment is correct.
//
// By default, log everything "trace" level or greater to stdout.
// TODO switch to using from_default_env()
fn init_logging() {
  env_logger::from_env(Env::default().default_filter_or("trace")).target(Target::Stdout)
                                                                 .init();
}

fn main() -> Result<(), PlatformError> {
  init_logging();
  let inputs = App::new("Transaction Builder")
    .version("0.0.1")
    .about("Copyright 2019 © Findora. All rights reserved.")
    .arg(Arg::with_name("config")
      .short("c")
      .long("config")
      .value_name("PATH/TO/FILE")
      .help("Specify a custom config file (default: \"$FINDORA_DIR/config.toml\")")
      .takes_value(true))
    .arg(Arg::with_name("findora_dir")
      .short("d")
      .long("dir")
      .value_name("PATH")
      .help("Directory for configuaration, security, and temporary files; must be writable")
      .takes_value(true)
      .env("FINDORA_DIR"))
    .arg(Arg::with_name("key_pair_path")
      .short("k")
      .long("key_pair")
      .value_name("PATH/TO/FILE")
      .help("Path to key pair)")
      .takes_value(true))
    .arg(Arg::with_name("txn")
      .long("txn")
      .value_name("FILE")
      .help("Use a name transaction file (will always be under findora_dir)")
      .takes_value(true))
    .subcommand(SubCommand::with_name("create")
      .about("By default, will rename previous file with a .<number> suffix")
      .arg(Arg::with_name("name")
        .short("n")
        .long("name")
        .value_name("FILE")
        .help("Specify a name for newly created transaction file")
        .takes_value(true))
      .arg(Arg::with_name("overwrite")
        .long("force")
        .alias("overwrite")
        .short("f")
        .help("If specified, the existing file with the same name will be overwritten.")))
    .subcommand(SubCommand::with_name("store")
      .subcommand(SubCommand::with_name("sids")
        .arg(Arg::with_name("path")
          .short("p")
          .long("path")
          .takes_value(true)
          .help("Path to store the sids. If not specified, a default path will be given."))
        .arg(Arg::with_name("overwrite")
          .long("force")
          .alias("overwrite")
          .short("f")
          .help("If specified, the existing file with the same name will be overwritten."))
        .arg(Arg::with_name("indices")
          .short("is")
          .long("indices")
          .takes_value(true)
          .help("Required: Input TxoSID indices. Separate by comma (\",\")")))
      .subcommand(SubCommand::with_name("blind_asset_record")
        .arg(Arg::with_name("path")
          .short("p")
          .long("path")
          .takes_value(true)
          .help("Path to store the blind asset record. If not specified, a default path will be given."))
        .arg(Arg::with_name("overwrite")
          .long("force")
          .alias("overwrite")
          .short("f")
          .help("If specified, the existing file with the same name will be overwritten."))
        .arg(Arg::with_name("amount")
          .short("a")
          .long("amount")
          .takes_value(true)
          .help("Required: Asset amount"))
        .arg(Arg::with_name("asset_type")
          .short("t")
          .long("asset_type")
          .takes_value(true)
          .help("Required: String representation of the asset type"))
        .arg(Arg::with_name("pub_key_path")
          .short("k")
          .long("pub_key_path")
          .takes_value(true)
          .help("Required: Path to the public key"))
        .arg(Arg::with_name("confidential_amount")
          .short("m")
          .long("confidential_amount")
          .help("If specified, the amount will be confidential"))
        .arg(Arg::with_name("confidential_asset")
          .short("s")
          .long("confidential_asset")
          .help("If specified, the asset will be confidential"))))
    .subcommand(SubCommand::with_name("add")
      .subcommand(SubCommand::with_name("define_asset")
        .arg(Arg::with_name("token_code")
          .long("token_code")
          .short("c")
          .help("Required: Explicit 16 character token code for the new asset; must be a unique name. If specified code is already in use, transaction will fail. If not specified, will display automatically generated token code.")
          .takes_value(true))
        .arg(Arg::with_name("allow_updates")
          .short("u")
          .long("allow_updates")
          .help("If specified, updates may be made to asset memo"))
        .arg(Arg::with_name("traceable")
          .short("trace")
          .long("traceable")
          .help("If specified, asset transfers can be traced by the issuer "))
        .arg(Arg::with_name("memo")
          .short("m")
          .long("memo")
          .takes_value(true)
          .help("Required: Memo as Json, with escaped quotation marks")
          .required(true))
        .arg(Arg::with_name("confidential")
          .short("xx")
          .long("confidential")
          .help("Make memo confidential"))
        .arg(Arg::with_name("with_policy")
          .short("p")
          .help("TODO: add support for policies")))
      .subcommand(SubCommand::with_name("issue_asset")
        .arg(Arg::with_name("token_code")
          .short("c")
          .long("token_code")
          .takes_value(true)
          .help("Required: Token code of the asset to be issued. The transaction will fail if no asset with the token code exists."))
        .arg(Arg::with_name("sequence_number")
          .short("seq")
          .long("sequence_number")
          .takes_value(true)
          .help("Required: Sequence number for the issue transaction. Used to prevent replay attacks."))
        .arg(Arg::with_name("amount")
          .short("amt")
          .long("amount")
          .takes_value(true)
          .help("Required: Amount of tokens to issue.")))
      .subcommand(SubCommand::with_name("transfer_asset")
        .arg(Arg::with_name("sids_path")
          .short("ssp")
          .long("sids_path")
          .takes_value(true)
          .help("Required: Path to the file where input TxoSID indices are stored."))
        .arg(Arg::with_name("blind_asset_record_paths")
          .short("barps")
          .long("blind_asset_record_paths")
          .takes_value(true)
          .help("Required: Path to the files where blind asset records are stored. Separate by comma (\",\")."))
        .arg(Arg::with_name("input_amounts")
          .short("iamts")
          .long("input_amounts")
          .takes_value(true)
          .help("Required: the amount to transfer from each record. Separate by comma (\",\")."))
        .arg(Arg::with_name("output_amounts")
          .short("oamts")
          .long("output_amounts")
          .takes_value(true)
          .help("Required: the amount to transfer to each account. Separate by comma (\",\")."))
        .arg(Arg::with_name("address_paths")
          .short("asp")
          .long("address_paths")
          .takes_value(true)
          .help("Required: Path to the files where address keys are stored. If no such file, try pubkeygen subcommand."))))
    .subcommand(SubCommand::with_name("serialize"))
    .subcommand(SubCommand::with_name("drop"))
    .subcommand(SubCommand::with_name("keygen")
      .arg(Arg::with_name("create_key_pair_path")
        .short("n")
        .long("name")
        .help("specify the path and name for the key pair file.")
        .takes_value(true))
      .arg(Arg::with_name("overwrite")
        .long("force")
        .alias("overwrite")
        .short("f")
        .help("If specified, the existing file with the same name will be overwritten.")))
    .subcommand(SubCommand::with_name("pubkeygen")
      .arg(Arg::with_name("create_pubkey_path")
        .short("n")
        .long("name")
        .help("specify the path and name for the public key file.")
        .takes_value(true))
      .arg(Arg::with_name("overwrite")
        .long("force")
        .alias("overwrite")
        .short("f")
        .help("If specified, the existing file with the same name will be overwritten.")))
    .subcommand(SubCommand::with_name("submit")
      .arg(Arg::with_name("protocol")
        .long("http")
        .takes_value(false)
        .help("specify that http, not https should be used.")))
    .subcommand(SubCommand::with_name("load_funds")
      .arg(Arg::with_name("sid_pre")
        .short("p")
        .long("sid_pre")
        .takes_value(true))
      .arg(Arg::with_name("sid_new")
        .short("n")
        .long("sid_new")
        .takes_value(true))
      .arg(Arg::with_name("blind_asset_record_pre_path")
        .long("blind_asset_record_pre_path")
        .takes_value(true))
      .arg(Arg::with_name("blind_asset_record_new_path")
        .long("blind_asset_record_new_path")
        .takes_value(true))
      .arg(Arg::with_name("recipient_key_pair_path")
        .short("r")
        .long("recipient_key_pair_path")
        .takes_value(true))
      .arg(Arg::with_name("amount")
        .short("a")
        .long("amount")
        .takes_value(true))
      .arg(Arg::with_name("token_code")
        .short("tc")
        .long("token_code")
        .takes_value(true))
      .arg(Arg::with_name("sequence_number")
        .short("s")
        .long("sequence_number")
        .takes_value(true))
      .arg(Arg::with_name("protocol")
        .long("http")
        .takes_value(false)
        .help("specify that http, not https should be used.")))
    .get_matches();
  process_inputs(inputs)
}

fn process_inputs(inputs: clap::ArgMatches) -> Result<(), PlatformError> {
  let _config_file_path: String;
  let key_pair_file_path: String;
  let transaction_file_name: String;
  let findora_dir = if let Some(dir) = inputs.value_of("findora_dir") {
    dir.to_string()
  } else if let Ok(dir) = env::var("FINDORA_DIR") {
    dir
  } else {
    let home_dir = dirs::home_dir().unwrap_or_else(|| Path::new(".").to_path_buf());
    format!("{}/.findora", home_dir.to_str().unwrap_or("./.findora"))
  };

  if let Some(cfg) = inputs.value_of("config") {
    _config_file_path = cfg.to_string();
  } else {
    _config_file_path = format!("{}/config.toml", findora_dir);
  }

  if let Some(key_pair) = inputs.value_of("key_pair_path") {
    key_pair_file_path = key_pair.to_string();
  } else {
    key_pair_file_path = format!("{}/keypair/default.keypair", findora_dir);
  }

  if let Some(txn_store) = inputs.value_of("txn") {
    transaction_file_name = txn_store.to_string();
  } else {
    transaction_file_name = format!("{}/txn/default.txn", findora_dir);
  }

  match inputs.subcommand() {
    ("create", Some(create_matches)) => process_create_cmd(create_matches,
                                                           &key_pair_file_path,
                                                           &transaction_file_name,
                                                           &findora_dir),
    ("store", Some(store_matches)) => process_store_cmd(store_matches, &findora_dir),
    ("add", Some(add_matches)) => {
      process_add_cmd(add_matches, &key_pair_file_path, &transaction_file_name)
    }
    ("serialize", Some(_serialize_matches)) => {
      let txn_builder = load_txn_builder_from_file(&transaction_file_name).or_else(|e| {
                          println!("Failed to load txn builder from file {}.",
                                   transaction_file_name);
                          Err(e)
                        })
                        .unwrap();
      match serde_json::to_string(txn_builder.transaction()) {
        Ok(as_json) => {
          println!("{}", as_json);
          Ok(())
        }
        Err(_) => {
          println!("Failed to serialize txn.");
          Err(PlatformError::SerializationError)
        }
      }
    }
    ("drop", Some(_drop_matches)) => match std::fs::remove_file(&transaction_file_name) {
      Ok(_) => {
        println!("Deleted transaction file {}", transaction_file_name);
        Ok(())
      }
      Err(e) => {
        println!("Error deleting file: {:?} ", e);
        exit(exitcode::IOERR)
      }
    },
    ("keygen", Some(keygen_matches)) => {
      let new_key_pair_path =
        if let Some(new_key_pair_path_in) = keygen_matches.value_of("create_key_pair_path") {
          new_key_pair_path_in.to_string()
        } else {
          format!("{}/keypair/default.keypair", &findora_dir)
        };
      let expand_str = shellexpand::tilde(&new_key_pair_path).to_string();
      let overwrite = keygen_matches.is_present("overwrite");
      println!("Storing key pair to {}", expand_str);
      create_directory_and_rename_path(&expand_str, overwrite);
      store_key_pair_to_file(&expand_str)
    }
    ("pubkeygen", Some(pubkeygen_matches)) => {
      let new_key_path =
        if let Some(new_key_path_in) = pubkeygen_matches.value_of("create_pubkey_path") {
          new_key_path_in.to_string()
        } else {
          format!("{}/pubkey/default.pubkey", &findora_dir)
        };
      let expand_str = shellexpand::tilde(&new_key_path).to_string();
      println!("Storing public key to {}", expand_str);
      let overwrite = pubkeygen_matches.is_present("overwrite");
      create_directory_and_rename_path(&expand_str, overwrite);
      store_pub_key_to_file(&expand_str)
    }
    ("submit", Some(submit_matches)) => process_submit_cmd(submit_matches, &transaction_file_name),
    ("load_funds", Some(load_funds_matches)) => process_load_funds_cmd(load_funds_matches,
                                                                       &key_pair_file_path,
                                                                       &transaction_file_name),
    _ => {
      println!("Subcommand missing or not recognized. Try --help");
      exit(exitcode::USAGE)
    }
  }
}

fn process_submit_cmd(submit_matches: &clap::ArgMatches,
                      transaction_file_name: &str)
                      -> Result<(), PlatformError> {
  // Get protocol, host and port.
  let protocol = if submit_matches.is_present("http") {
    // Allow HTTP which may be useful for running a ledger locally.
    "http"
  } else {
    // Default to HTTPS
    "https"
  };

  // serialize txn
  if let Err(error) = submit(protocol, &transaction_file_name) {
    return Err(error);
  }
  Ok(())
}

// Create the specific file if missing
// Rename the existing path if necessary
fn create_directory_and_rename_path(path_str: &str, overwrite: bool) {
  let path = Path::new(&path_str);
  create_directory_if_missing(&path_str.clone());
  if path.exists() && !overwrite {
    rename_existing_path(&path);
  }
}

fn process_create_cmd(create_matches: &clap::ArgMatches,
                      _keys_file_path: &str,
                      transaction_file_name: &str,
                      _findora_dir: &str)
                      -> Result<(), PlatformError> {
  let name = create_matches.value_of("name");
  let overwrite = create_matches.is_present("overwrite");
  let file_str = if let Some(name) = name {
    name.to_string()
  } else {
    transaction_file_name.to_string()
  };
  let expand_str = shellexpand::tilde(&file_str).to_string();
  create_directory_and_rename_path(&expand_str, overwrite);
  let txn_builder = TransactionBuilder::default();
  store_txn_builder_to_file(&expand_str, &txn_builder)
}

fn process_store_cmd(store_matches: &clap::ArgMatches,
                     findora_dir: &str)
                     -> Result<(), PlatformError> {
  match store_matches.subcommand() {
    ("sids", Some(sids_matches)) => {
      let path = if let Some(path_arg) = sids_matches.value_of("path") {
        path_arg.to_string()
      } else {
        format!("{}/values/default.sids", &findora_dir)
      };
      let path_expand = shellexpand::tilde(&path).to_string();
      println!("Storing sids to {}", path_expand);
      let overwrite = sids_matches.is_present("overwrite");
      create_directory_and_rename_path(&path_expand, overwrite);
      let sids;
      if let Some(sids_arg) = sids_matches.value_of("indices") {
        sids = sids_arg
      } else {
        println!("TxoSID indices are required. Use --indices.");
        exit(exitcode::USAGE)
      }
      store_sids_to_file(&path_expand, sids)
    }

    ("blind_asset_record", Some(blind_asset_record_path_matches)) => {
      let path = if let Some(path_arg) = blind_asset_record_path_matches.value_of("path") {
        path_arg.to_string()
      } else {
        format!("{}/values/default.blind_asset_record", &findora_dir)
      };
      let path_expand = shellexpand::tilde(&path).to_string();
      println!("Storing blind asset records to {}", path_expand);
      let overwrite = blind_asset_record_path_matches.is_present("overwrite");
      create_directory_and_rename_path(&path_expand, overwrite);
      let amount;
      if let Some(amount_arg) = blind_asset_record_path_matches.value_of("amount") {
        amount = amount_arg
      } else {
        println!("Amount is required. Use --amount.");
        exit(exitcode::USAGE)
      }
      let asset_type;
      if let Some(asset_type_arg) = blind_asset_record_path_matches.value_of("asset_type") {
        asset_type = asset_type_arg
      } else {
        println!("Asset type is required. Use --asset_type.");
        exit(exitcode::USAGE)
      }
      let pub_key_path;
      if let Some(pub_key_path_arg) = blind_asset_record_path_matches.value_of("pub_key_path") {
        pub_key_path = pub_key_path_arg
      } else {
        println!("File to public key is required. If no such file, try pubkeygen subcommand.");
        exit(exitcode::USAGE)
      }
      let confidential_amount = blind_asset_record_path_matches.is_present("confidential_amount");
      let confidential_asset = blind_asset_record_path_matches.is_present("confidential_asset");
      store_blind_asset_record(&path_expand,
                               amount,
                               asset_type,
                               pub_key_path,
                               confidential_amount,
                               confidential_asset)
    }

    _ => {
      println!("Subcommand missing or not recognized. Try store --help");
      exit(exitcode::USAGE)
    }
  }
}

fn process_add_cmd(add_matches: &clap::ArgMatches,
                   key_pair_file_path: &str,
                   transaction_file_name: &str)
                   -> Result<(), PlatformError> {
  println!("{}", key_pair_file_path);
  let key_pair: XfrKeyPair;
  match load_key_pair_from_file(&key_pair_file_path) {
    Ok(kp) => {
      key_pair = kp;
    }
    Err(e) => {
      println!("Valid keyfile required for this command; if no keyfile currently exists, try running \"findora_txn_builder keygen\"");
      return Err(e);
    }
  }
  match add_matches.subcommand() {
    ("define_asset", Some(define_asset_matches)) => {
      let token_code = define_asset_matches.value_of("token_code");
      let memo = define_asset_matches.value_of("memo")
                                     .unwrap_or("{}")
                                     .to_string();
      let allow_updates = define_asset_matches.is_present("allow_updates");
      let traceable = define_asset_matches.is_present("traceable");
      let mut txn_builder = load_txn_builder_from_file(&transaction_file_name).or_else(|e| {
                              println!("Failed to load txn builder from file {}.",
                                       transaction_file_name);
                              Err(e)
                            })
                            .unwrap();
      let asset_token: AssetTypeCode;
      if let Some(token_code) = token_code {
        asset_token = AssetTypeCode::new_from_str(token_code);
      } else {
        asset_token = AssetTypeCode::gen_random();
        println!("Creating asset with token code {:?}", asset_token.val);
      }
      if let Err(e) = txn_builder.add_operation_create_asset(&key_pair,
                                                             Some(asset_token),
                                                             allow_updates,
                                                             traceable,
                                                             &memo)
      {
        println!("Failed to add operation to transaction.");
        return Err(e);
      }
      if let Err(e) = store_txn_builder_to_file(&transaction_file_name, &txn_builder) {
        println!("Failed to store txn builder to file.");
        return Err(e);
      };
      Ok(())
    }
    ("issue_asset", Some(issue_asset_matches)) => {
      let asset_token: AssetTypeCode;
      if let Some(token_code_arg) = issue_asset_matches.value_of("token_code") {
        asset_token = AssetTypeCode::new_from_str(token_code_arg);
      } else {
        println!("Token code is required to issue asset. Use --token_code.");
        exit(exitcode::USAGE)
      }
      let seq_num;
      if let Some(sequence_number_arg) = issue_asset_matches.value_of("sequence_number") {
        if let Ok(seq_num_parsed) = sequence_number_arg.parse::<u64>() {
          seq_num = seq_num_parsed;
        } else {
          println!("Improperly formatted sequence number.");
          exit(exitcode::USAGE)
        }
      } else {
        println!("Sequence number is required to issue asset. Use --sequence_number.");
        exit(exitcode::USAGE)
      }
      let amount;
      if let Some(amount_arg) = issue_asset_matches.value_of("amount") {
        if let Ok(amount_parsed) = amount_arg.parse::<u64>() {
          amount = amount_parsed;
        } else {
          println!("Improperly formatted amount.");
          exit(exitcode::USAGE)
        }
      } else {
        println!("Amount is required to issue asset. Use --amount.");
        exit(exitcode::USAGE)
      }
      let mut txn_builder = load_txn_builder_from_file(&transaction_file_name).or_else(|e| {
                              println!("Failed to load txn builder from file {}.",
                                       transaction_file_name);
                              Err(e)
                            })
                            .unwrap();
      if let Err(e) =
        txn_builder.add_basic_issue_asset(&key_pair, &None, &asset_token, seq_num, amount)
      {
        println!("Failed to add basic issue asset.");
        return Err(e);
      }
      if let Err(e) = store_txn_builder_to_file(&transaction_file_name, &txn_builder) {
        println!("Failed to store txn builder to file.");
        return Err(e);
      }
      Ok(())
    }
    ("transfer_asset", Some(transfer_asset_matches)) => {
      // Compose transfer_from for add_basic_transfer_asset
      let txo_refs;
      if let Some(sids_path) = transfer_asset_matches.value_of("sids_path") {
        match load_sids_from_file(sids_path) {
          Ok(result) => {
            txo_refs = result;
          }
          Err(error) => {
            println!("Error loading txo_refs from {}: {}", sids_path, error);
            return Err(error);
          }
        }
      } else {
        println!("Path to sids file is required to transfer asset. Use --sids_path");
        exit(exitcode::USAGE)
      }
      let blind_asset_records;
      if let Some(blind_asset_record_paths) =
        transfer_asset_matches.value_of("blind_asset_record_paths")
      {
        match load_blind_asset_records_from_files(blind_asset_record_paths) {
          Ok(result) => {
            blind_asset_records = result;
          }
          Err(error) => {
            println!("Error loading blind_asset_records from {}: {}",
                     blind_asset_record_paths, error);
            return Err(error);
          }
        }
      } else {
        println!("Paths to blind asset records are required to transfer asset. Use --blind_asset_record_paths");
        exit(exitcode::USAGE)
      }
      let input_amounts;
      if let Some(input_amounts_arg) = transfer_asset_matches.value_of("input_amounts") {
        input_amounts = get_amounts(input_amounts_arg).unwrap();
      } else {
        println!("Input amounts are required to transfer asset. Use --input_amounts.");
        exit(exitcode::USAGE)
      }
      let mut count = txo_refs.len();
      if blind_asset_records.len() != count || input_amounts.len() != count {
        println!("Size of input sids, blind asset records, and input amounts should match.");
        exit(exitcode::USAGE)
      }
      let mut transfer_from = Vec::new();
      let mut txo_refs_iter = txo_refs.iter();
      let mut blind_asset_records_iter = blind_asset_records.iter();
      let mut input_amounts_iter = input_amounts.iter();
      while count > 0 {
        transfer_from.push((txo_refs_iter.next().unwrap(),
                            blind_asset_records_iter.next().unwrap(),
                            *input_amounts_iter.next().unwrap()));
        count -= 1;
      }

      // Compose transfer_to for add_basic_transfer_asset
      let output_amounts;
      if let Some(output_amounts_arg) = transfer_asset_matches.value_of("output_amounts") {
        output_amounts = get_amounts(output_amounts_arg).unwrap();
      } else {
        println!("Output amounts are required to transfer asset. Use --output_amounts.");
        exit(exitcode::USAGE)
      }
      let addresses;
      if let Some(addresses_path) = transfer_asset_matches.value_of("address_paths") {
        match load_addresses_from_files(addresses_path) {
          Ok(result) => {
            addresses = result;
          }
          Err(error) => {
            println!("Error loading addresses: {}", error);
            return Err(error);
          }
        }
      } else {
        println!("Paths to address keys are required to transfer asset. Use --address_paths");
        exit(exitcode::USAGE)
      }
      let mut count = output_amounts.len();
      if addresses.len() != count {
        println!("Size of output amounts and addresses should match.");
        exit(exitcode::USAGE)
      }
      let mut transfer_to = Vec::new();
      let mut output_amounts_iter = output_amounts.iter();
      let mut addresses_iter = addresses.iter();
      while count > 0 {
        transfer_to.push((*output_amounts_iter.next().unwrap(), addresses_iter.next().unwrap()));
        count -= 1;
      }

      // Transfer asset
      let mut txn_builder = load_txn_builder_from_file(&transaction_file_name).or_else(|e| {
                              println!("Failed to load txn builder from file {}.",
                                       transaction_file_name);
                              Err(e)
                            })
                            .unwrap();
      if let Err(e) =
        txn_builder.add_basic_transfer_asset(&load_key_pair_from_file(key_pair_file_path).unwrap(),
                                             &transfer_from[..],
                                             &transfer_to[..])
      {
        println!("Failed to add operation to transaction.");
        return Err(e);
      };
      if let Err(e) = store_txn_builder_to_file(&transaction_file_name, &txn_builder) {
        println!("Failed to store txn builder to file.");
        return Err(e);
      }
      Ok(())
    }
    _ => {
      println!("Subcommand missing or not recognized. Try add --help");
      exit(exitcode::USAGE)
    }
  }
}

fn process_load_funds_cmd(load_funds_matches: &clap::ArgMatches,
                          key_pair_file_path: &str,
                          transaction_file_name: &str)
                          -> Result<(), PlatformError> {
  let sid_pre = if let Some(sid_pre_arg) = load_funds_matches.value_of("sid_pre") {
    TxoRef::Absolute(TxoSID(get_amount(sid_pre_arg).unwrap()))
  } else {
    println!("Previous sid is required to load funds. Use --sid_pre.");
    exit(exitcode::USAGE)
  };
  let sid_new = if let Some(sid_new_arg) = load_funds_matches.value_of("sid_new") {
    TxoRef::Absolute(TxoSID(get_amount(sid_new_arg).unwrap()))
  } else {
    println!("New sid is required to load funds. Use --sid_new.");
    exit(exitcode::USAGE)
  };
  let blind_asset_record_pre = if let Some(blind_asset_record_pre_path_arg) =
    load_funds_matches.value_of("blind_asset_record_pre_path")
  {
    load_blind_asset_record_from_file(blind_asset_record_pre_path_arg).unwrap()
  } else {
    println!("Path to the previous blind asset record is required to load funds. Use --blind_asset_record_pre_path.");
    exit(exitcode::USAGE)
  };
  let blind_asset_record_new = if let Some(blind_asset_record_new_path_arg) =
    load_funds_matches.value_of("blind_asset_record_new_path")
  {
    load_blind_asset_record_from_file(blind_asset_record_new_path_arg).unwrap()
  } else {
    println!("Path to the new blind asset record is required to load funds. Use --blind_asset_record_new_path.");
    exit(exitcode::USAGE)
  };
  let recipient_key_pair = if let Some(recipient_key_pair_path_arg) =
    load_funds_matches.value_of("recipient_key_pair_path")
  {
    load_key_pair_from_file(recipient_key_pair_path_arg).unwrap()
  } else {
    println!("Path to the recipient's key pair is required to load funds. Use --recipient_key_pair_path.");
    exit(exitcode::USAGE)
  };
  let amount = if let Some(amount_arg) = load_funds_matches.value_of("amount") {
    get_amount(amount_arg).unwrap()
  } else {
    println!("Amount is required to load funds. Use --amount.");
    exit(exitcode::USAGE)
  };
  let token_code = if let Some(token_code_arg) = load_funds_matches.value_of("token_code") {
    AssetTypeCode::new_from_str(token_code_arg)
  } else {
    println!("Token code is required to load funds. Use --token_code.");
    exit(exitcode::USAGE)
  };
  let sequence_num =
    if let Some(sequence_number_arg) = load_funds_matches.value_of("sequence_number") {
      if let Ok(seq_num_parsed) = sequence_number_arg.parse::<u64>() {
        seq_num_parsed
      } else {
        println!("Improperly formatted sequence number.");
        exit(exitcode::USAGE)
      }
    } else {
      println!("Sequence number is required to load funds. Use --sequence_number.");
      exit(exitcode::USAGE)
    };
  let protocol = if load_funds_matches.is_present("http") {
    // Allow HTTP which may be useful for running a ledger locally.
    "http"
  } else {
    // Default to HTTPS
    "https"
  };

  load_funds(sid_pre,
             sid_new,
             blind_asset_record_pre,
             blind_asset_record_new,
             &load_key_pair_from_file(key_pair_file_path).unwrap(),
             &recipient_key_pair,
             amount,
             token_code,
             transaction_file_name,
             sequence_num,
             protocol)
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::str::from_utf8;

  fn check_next_path(input: &str, expected: &str) {
    let as_path = Path::new(input);
    if let Ok(result) = next_path(as_path) {
      let as_str = result.to_str().unwrap();
      if as_str != expected {
        panic!("{} failed:  {}", input, as_str);
      }
    }
  }

  // Note: creates and removes a file of the given name.
  // If such a file was present, it gets overwritten
  // and then removed.
  fn check_next_path_typical(input: &str, expected: &str) {
    trace!("check_next_path_typical({}, {})", input, expected);
    if let Err(e) = fs::write(input, "txn_builder_cli next_path() test detritus") {
      panic!("write error: {:?}", e);
    }
    check_next_path(input, expected);
    if let Err(e) = fs::remove_file(input) {
      panic!("remove_file error: {:?}", e);
    }
  }

  fn check_next_path_nonextant(input: &str, expected: &str) {
    check_next_path(input, expected)
  }

  #[test]
  fn test_next_path() {
    check_next_path_typical("1000", "1000.0");
    check_next_path_nonextant("1000", "1000.0");

    check_next_path_typical("abc", "abc.0");
    check_next_path_nonextant("abc", "abc.0");

    check_next_path_typical("abc.def", "abc.def.0");
    check_next_path_nonextant("abc.def", "abc.def.0");

    check_next_path_typical("a.12", "a.13");
    check_next_path_nonextant("a.12", "a.12");

    check_next_path_typical(".12", ".12.0");
    check_next_path_nonextant(".12", ".12.0");

    check_next_path_typical("abc.12", "abc.13");
    check_next_path_nonextant("abc.12", "abc.12");

    check_next_path_typical("abc.0", "abc.1");
    check_next_path_nonextant("abc.0", "abc.0");
  }

  #[test]
  fn test_store_and_load_sids() {
    let paths = vec!["sids1", "sids2", "sids3"];
    let sids = vec!["1,2,4", "1,2, 4", "1,a,4"];

    for i in 0..3 {
      store_sids_to_file(paths[i], sids[i]).unwrap();
    }

    let expected_txo_refs = vec![TxoRef::Absolute(TxoSID(1)),
                                 TxoRef::Absolute(TxoSID(2)),
                                 TxoRef::Absolute(TxoSID(4))];

    // Verify that load_sids_from_file succeeds with correctly formatted input
    assert_eq!(load_sids_from_file(paths[0]).unwrap(), expected_txo_refs);
    assert_eq!(load_sids_from_file(paths[1]).unwrap(), expected_txo_refs);

    paths.into_iter()
         .map(|path| fs::remove_file(path).unwrap())
         .collect()
  }

  #[test]
  fn test_store_and_load_blind_asset_records() {
    // Set fields for constructing blind asset records
    let paths = vec!["file1", "file2", "file3"];
    let paths_str = "file1,file2, file3";
    let pub_key_paths = vec!["pub1", "pub2", "pub3"];
    let amounts = vec![100, 200, 300];
    let asset_types = vec![[0u8; 16], [0u8; 16], [0u8; 16]];
    let confidential_amount_bools = vec![false, false, true];
    let confidential_asset_bools = vec![false, true, false];

    // Store each blind asset record
    for i in 0..3 {
      let _ = store_pub_key_to_file(pub_key_paths[i]);
      store_blind_asset_record(paths[i],
                               &amounts[i].to_string(),
                               from_utf8(&asset_types[i]).unwrap(),
                               pub_key_paths[i],
                               confidential_amount_bools[i],
                               confidential_asset_bools[i]).unwrap();
    }

    // Load all the blind asset records
    let blind_asset_records = load_blind_asset_records_from_files(&paths_str).unwrap();

    // Verify the field of the first blind asset record
    assert_eq!(blind_asset_records[0].amount, Some(amounts[0]));
    assert_eq!(blind_asset_records[0].asset_type, Some(asset_types[0]));
    fs::remove_file(paths[0]).unwrap();
    fs::remove_file(pub_key_paths[0]).unwrap();

    // Verify the field of the second blind asset record
    // Asset type should be None because it's set as confidential
    assert_eq!(blind_asset_records[1].amount, Some(amounts[1]));
    assert_eq!(blind_asset_records[1].asset_type, None);
    fs::remove_file(paths[1]).unwrap();
    fs::remove_file(pub_key_paths[1]).unwrap();

    // Verify the field of the third blind asset record
    // Amount should be None because the it's set as confidential
    assert_eq!(blind_asset_records[2].amount, None);
    assert_eq!(blind_asset_records[2].asset_type, Some(asset_types[2]));
    fs::remove_file(paths[2]).unwrap();
    fs::remove_file(pub_key_paths[2]).unwrap();
  }

  #[test]
  fn test_get_amounts() {
    let amounts_arg = "1, 2,4";
    let expected_amounts = vec![1, 2, 4];

    assert_eq!(get_amounts(amounts_arg).unwrap(), expected_amounts);
  }

  #[test]
  fn test_issue_and_transfer() {
    // Create txn builder and key pairs
    let txn_builder_path = "tb_issue_and_transfer";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
    let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
    let issuer_key_pair = XfrKeyPair::generate(&mut prng);
    let recipient_key_pair = XfrKeyPair::generate(&mut prng);

    // Build blind asset record
    let params = PublicParams::new();
    let art = AssetRecordType::PublicAmount_PublicAssetType;
    let code = AssetTypeCode::gen_random();
    let amount = 1000;
    let ar = AssetRecord::new(amount, code.val, issuer_key_pair.get_pk()).unwrap();
    let bar = build_blind_asset_record(&mut prng, &params.pc_gens, &ar, art, &None);

    // Issue and transfer asset
    assert!(issue_and_transfer(TxoRef::Relative(1),
                               bar,
                               &issuer_key_pair,
                               &recipient_key_pair.get_pk(),
                               amount,
                               code,
                               txn_builder_path,
                               1).is_ok());

    fs::remove_file(txn_builder_path).unwrap();
  }

  #[test]
  fn test_merge_records() {
    // Create txn builder and key pair
    let txn_builder_path = "tb_merge";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
    let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
    let key_pair = XfrKeyPair::generate(&mut prng);

    // Build blind asset records
    let params = PublicParams::new();
    let art = AssetRecordType::PublicAmount_PublicAssetType;
    let code = AssetTypeCode::gen_random();
    let ar1 = AssetRecord::new(1000, code.val, key_pair.get_pk()).unwrap();
    let ar2 = AssetRecord::new(1000, code.val, key_pair.get_pk()).unwrap();
    let bar1 = build_blind_asset_record(&mut prng, &params.pc_gens, &ar1, art, &None);
    let bar2 = build_blind_asset_record(&mut prng, &params.pc_gens, &ar2, art, &None);

    // Merge records
    assert!(merge_records(&key_pair,
                          TxoRef::Absolute(TxoSID(1)),
                          TxoRef::Absolute(TxoSID(2)),
                          bar1,
                          bar2,
                          code,
                          txn_builder_path).is_ok());

    fs::remove_file(txn_builder_path).unwrap();
  }

  #[test]
  fn test_submit() {
    let txn_builder_path = "tb_submit";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
    let res = submit("https", txn_builder_path);
    fs::remove_file(txn_builder_path).unwrap();
    assert!(res.is_ok());
  }

  // TODO (Keyao): Tests below don't pass. Fix them.
  // They both fail when submitting transaction.

  //   #[test]
  //   fn test_define_and_submit() {
  //     let txn_builder_path = "tb_define_and_submit";
  //     store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
  //     let mut txn_builder = load_txn_builder_from_file(&txn_builder_path).unwrap();

  //     let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
  //     let key_pair = XfrKeyPair::generate(&mut prng);
  //     let token_code = AssetTypeCode::gen_random();

  //     txn_builder.add_operation_create_asset(&key_pair, Some(token_code), false, false, "")
  //                .unwrap();
  //     store_txn_builder_to_file(&txn_builder_path, &txn_builder).unwrap();

  //     let res = submit("https", txn_builder_path);
  //     fs::remove_file(txn_builder_path).unwrap();
  //     assert!(res.is_ok());
  //   }

  //   #[test]
  //   fn test_load_funds() {
  //     // Create txn builder and key pairs
  //     let txn_builder_path = "tb_load_funds";
  //     store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
  //     let mut prng = ChaChaRng::from_seed([0u8; 32]);
  //     let issuer_key_pair = XfrKeyPair::generate(&mut prng);
  //     let recipient_key_pair = XfrKeyPair::generate(&mut prng);

  //     // Build blind asset records
  //     let params = PublicParams::new();
  //     let art = AssetRecordType::PublicAmount_PublicAssetType;
  //     let amount_pre = 1000;
  //     let amount_new = 500;
  //     let code = AssetTypeCode::gen_random();
  //     let ar_pre = AssetRecord::new(amount_pre, code.val, recipient_key_pair.get_pk()).unwrap();
  //     let ar_new = AssetRecord::new(amount_new, code.val, issuer_key_pair.get_pk()).unwrap();
  //     let bar_pre = build_blind_asset_record(&mut prng, &params.pc_gens, &ar_pre, art, &None);
  //     let bar_new = build_blind_asset_record(&mut prng, &params.pc_gens, &ar_new, art, &None);

  //     // Load funds
  //     let res = load_funds(TxoRef::Absolute(TxoSID(1)),
  //                          TxoRef::Relative(2),
  //                          bar_pre,
  //                          bar_new,
  //                          &issuer_key_pair,
  //                          &recipient_key_pair,
  //                          amount_new,
  //                          code,
  //                          txn_builder_path,
  //                          1,
  //                          "https");
  //     fs::remove_file(txn_builder_path).unwrap();
  //     assert!(res.is_ok());
  //   }
}
