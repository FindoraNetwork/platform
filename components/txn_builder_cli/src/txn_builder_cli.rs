#![deny(warnings)]
use clap::{App, Arg, SubCommand};
use env_logger::{Env, Target};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AccountAddress, AssetTypeCode, TxoRef, TxoSID};
use ledger_app::TxnHandle;
use log::{error, trace}; // Other options: debug, info, warn
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::env;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::prelude::*;
use std::io::Error;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use txn_builder::{BuildsTransactions, TransactionBuilder};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::BlindAssetRecord;

fn load_txn_builder_from_file(file_name: &str) -> Result<TransactionBuilder, PlatformError> {
  let mut file = File::open(file_name).or_else(|_e| {
                                        println!("Transaction file {} does not exist", file_name);
                                        Err(PlatformError::IoError("missing file".to_string()))
                                      })?;
  let mut contents = String::new();
  file.read_to_string(&mut contents).or_else(|_e| {
                                       println!("Failed to read transaction file {}", file_name);
                                       Err(PlatformError::IoError("cannot read".to_string()))
                                     })?;
  println!("Parsing builder from file contents: \"{}\"", &contents);
  let builder = serde_json::from_str(&contents)?;
  Ok(builder)
}

fn load_key_pair_from_files(key_file_name: &str) -> Result<XfrKeyPair, PlatformError> {
  let mut key_file = File::open(key_file_name).or_else(|_e| {
                                                println!("Failed to open key file {}",
                                                         key_file_name);
                                                Err(PlatformError::DeserializationError)
                                              })?;

  let kp: XfrKeyPair;
  let mut kp_byte_buffer = Vec::new();
  match key_file.read_to_end(&mut kp_byte_buffer) {
    Ok(_len) => {
      kp = XfrKeyPair::zei_from_bytes(&kp_byte_buffer);
    }
    Err(_e) => {
      println!("Failed to read key file {}", key_file_name);
      return Err(PlatformError::IoError("unable to read".to_string()));
    }
  }

  Ok(kp)
}

fn store_txn_builder_to_file(file_name: &str, txn: &TransactionBuilder) {
  if let Ok(as_json) = serde_json::to_string(txn) {
    let _skip = fs::write(file_name, &as_json).or_else(|_e| {
                  println!("Transaction file {} could not be created", file_name);
                  Err(PlatformError::IoError("unable to write".to_string()))
                });
  }
}

// Write a new key pair to the given paths.
// Create subdirectories as needed.
// Move aside any extant files at the given paths.
// Reports errors rather than returning them.
// Assumes tilde expansion has already been done on paths.
fn create_key_files(key_path: &Path) {
  match fs::create_dir_all(&key_path.parent().unwrap()) {
    Ok(()) => {
      if let Err(error) = rename_existing_path(&key_path) {
        error!("Cannot rename key {:?}: {}", &key_path, error);
      }
      let mut prng: ChaChaRng;
      prng = ChaChaRng::from_seed([0u8; 32]);
      let keypair = XfrKeyPair::generate(&mut prng);
      match fs::write(&key_path, keypair.zei_to_bytes()) {
        Ok(_) => {}
        Err(error) => {
          error!("Key file {:?} could not be created: {}", key_path, error);
        }
      };
    }
    Err(error) => {
      error!("Failed to create directories for {}: {}",
             &key_path.display(),
             error);
    }
  }
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
fn find_available_path(path: &Path, n: i32) -> Result<PathBuf, std::io::Error> {
  if n < BACKUP_COUNT_MAX {
    let path_n = path.with_extension(&n.to_string());
    if path_n.exists() {
      find_available_path(path, n + 1)
    } else {
      Ok(path_n)
    }
  } else {
    Err(std::io::Error::new(std::io::ErrorKind::AlreadyExists,
                            format!("Too many backups for {:?}", path)))
  }
}

// Return a backup file path derived from path or an error if an
// unused path cannot be derived. The path must not be empty
// and must not be dot (".").
fn next_path(path: &Path) -> Result<PathBuf, std::io::Error> {
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
      Err(std::io::Error::new(std::io::ErrorKind::InvalidData,
                              format!("Is empty: {:?}", path)))
    } else if path.file_name() == None {
      Err(std::io::Error::new(std::io::ErrorKind::InvalidData,
                              format!("Is directory: {:?}", path)))
    } else {
      find_available_path(&add_backup_extension(&path), 0)
    }
  }
}

fn rename_existing_path(path: &Path) -> std::result::Result<(), std::io::Error> {
  match next_path(path) {
    Ok(next) => {
      trace!("Next path for {:?} is {:?}", &path, &next);
      fs::rename(path, next.as_path())
    }
    Err(error) => Err(error),
  }
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

fn main() {
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
    .arg(Arg::with_name("keys_path")
      .short("k")
      .long("keys")
      .value_name("PATH/TO/FILE")
      .help("Path to keys)")
      .takes_value(true))
    .arg(Arg::with_name("txn")
      .long("txn")
      .value_name("FILE")
      .help("Use a named transaction file (will always be under findora_dir)")
      .takes_value(true))
    .subcommand(SubCommand::with_name("create")
      .about("By default, will rename previous file with a .<number> suffix")
      .arg(Arg::with_name("named")
        .short("n")
        .value_name("FILE")
        .help("Specify a name for newly created transaction file")
        .takes_value(true))
      .arg(Arg::with_name("overwrite")
        .long("force")
        .alias("overwrite")
        .short("f")
        .help("Overwrite the default or named transaction file")))
    .subcommand(SubCommand::with_name("add")
    // TODO (Keyao): Add "Required" to the help message for required arguments
      .subcommand(SubCommand::with_name("define_asset")
        .arg(Arg::with_name("token_code")
          .long("token_code")
          .alias("tc")
          .help("Specify an explicit 16 character token code for the new asset; must be a unique name. If specified code is already in use, transaction will fail. If not specified, will display automatically generated token code.")
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
          .help("Memo as Json, with escaped quotation marks")
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
          .short("tc")
          .long("token_code")
          .takes_value(true)
          .help("Specify the token code of the asset to be issued. The transaction will fail if no asset with the token code exists."))
        .arg(Arg::with_name("sequence_number")
          .short("seq")
          .long("sequence_number")
          .takes_value(true)
          .help("Sequence number for the issue transaction. Used to prevent replay attacks."))
        .arg(Arg::with_name("amount")
          .short("amt")
          .long("amount")
          .takes_value(true)
          .help("Amount of tokens to issue.")))
      .subcommand(SubCommand::with_name("transfer_asset")
        .arg(Arg::with_name("sids")
          .short("ss")
          .long("sids")
          .takes_value(true)
          .help("Required: input TxoSID indeces. Separate by semicolon (\";\")."))
        .arg(Arg::with_name("blind_asset_records")
          .short("bars")
          .long("blind_asset_records")
          .takes_value(true)
          .help("Required: JSON serializations of the blind asset records of the assets to be transferred. Separate by semicolon (\";\")."))
        .arg(Arg::with_name("input_amounts")
          .short("iamts")
          .long("input_amounts")
          .takes_value(true)
          .help("Required: the amount to transfer from each record. Separate by semicolon (\";\")."))
        .arg(Arg::with_name("output_amounts")
          .short("oamts")
          .long("output_amounts")
          .takes_value(true)
          .help("Required: the amount to transfer to each account. Separate by semicolon (\";\")."))
        .arg(Arg::with_name("addresses")
          .short("ads")
          .long("addresses")
          .takes_value(true)
          .help("Required: addresses to send tokens to. Separate by semicolon (\";\")."))))
    .subcommand(SubCommand::with_name("serialize"))
    .subcommand(SubCommand::with_name("drop"))
    .subcommand(SubCommand::with_name("keygen")
      .arg(Arg::with_name("create_keys_path")
        .short("n")
        .long("name")
        .help("specify the path and name for the private key file; if the name has the form \"path/to/file_name.private\", the public key file will be \"path/to/file_name.pub\"; otherwise, \".pub\" will be appended to the name")
        .takes_value(true)))
    .subcommand(SubCommand::with_name("submit")
        .arg(Arg::with_name("port")
            .short("P")
            .long("port")
            .takes_value(true)
            .help("specify ledger standalone port (e.g. 8669)"))
        .arg(Arg::with_name("host")
            .short("H")
            .long("host")
            .takes_value(true)
            .help("specify ledger standalone host (e.g. localhost)")))
    .get_matches();
  process_inputs(inputs)
}

fn process_inputs(inputs: clap::ArgMatches) {
  let _config_file_path: String;
  let keys_file_path: String;
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

  if let Some(key) = inputs.value_of("keys_path") {
    keys_file_path = key.to_string();
  } else {
    keys_file_path = format!("{}/keys/default.keys", findora_dir);
  }

  if let Some(txn_store) = inputs.value_of("txn") {
    transaction_file_name = txn_store.to_string();
  } else {
    transaction_file_name = format!("{}/current.txn", findora_dir);
  }

  match inputs.subcommand() {
    ("create", Some(create_matches)) => {
      process_create_cmd(create_matches,
                         &keys_file_path,
                         &transaction_file_name,
                         &findora_dir);
    }
    ("add", Some(add_matches)) => {
      process_add_cmd(add_matches,
                      &keys_file_path,
                      &transaction_file_name,
                      &findora_dir);
    }
    ("serialize", Some(_serialize_matches)) => {
      if let Ok(txn_builder) = load_txn_builder_from_file(&transaction_file_name) {
        if let Ok(as_json) = serde_json::to_string(txn_builder.transaction()) {
          println!("{}", as_json);
        }
      }
    }
    ("drop", Some(_drop_matches)) => match std::fs::remove_file(&transaction_file_name) {
      Ok(_) => println!("Deleted transaction file {}", transaction_file_name),
      Err(e) => println!("Error deleting file: {:?} ", e),
    },
    ("keygen", Some(keygen_matches)) => {
      let new_keys_path =
        if let Some(new_keys_path_in) = keygen_matches.value_of("create_keys_path") {
          new_keys_path_in.to_string()
        } else {
          format!("{}/keys/default.keys", &findora_dir)
        };
      let file_str = shellexpand::tilde(&new_keys_path).to_string();
      let file_path = Path::new(&file_str);
      create_key_files(&file_path);
    }
    ("submit", Some(submit_matches)) => {
      process_submit_cmd(submit_matches, &transaction_file_name);
    }
    _ => {}
  }
}

fn process_submit_cmd(submit_matches: &clap::ArgMatches, transaction_file_name: &str) {
  // get host and port
  let host;
  if let Some(host_arg) = submit_matches.value_of("host") {
    host = host_arg;
  } else {
    error!("Standalone host must be specified (e.g. localhost)");
    return;
  }
  let port;
  if let Some(port_arg) = submit_matches.value_of("port") {
    port = port_arg;
  } else {
    error!("Standalone port must be specified (e.g. 8668)");
    return;
  }

  // serialize txn
  let txn;
  if let Ok(txn_builder) = load_txn_builder_from_file(&transaction_file_name) {
    txn = txn_builder.transaction().clone();
  } else {
    error!("Cannot deserialize transaction builder file at {}",
           &transaction_file_name);
    return;
  }

  // submit
  let client = reqwest::Client::new();
  let mut res = client.post(&format!("http://{}:{}/{}", &host, &port, "submit_transaction"))
                      .json(&txn)
                      .send()
                      .unwrap();

  // log body
  println!("Response: {}",
           res.json::<TxnHandle>().expect("<Invalid JSON>"));
  println!("Status: {}", res.status());
  println!("Headers:\n{:?}", res.headers());
}

fn process_create_cmd(create_matches: &clap::ArgMatches,
                      _keys_file_path: &str,
                      transaction_file_name: &str,
                      _findora_dir: &str) {
  let named = create_matches.value_of("named");
  let overwrite = create_matches.is_present("overwrite");
  let file_str = if let Some(named) = named {
    named.to_string()
  } else {
    transaction_file_name.to_string()
  };
  let expand_str = shellexpand::tilde(&file_str).to_string();
  let file_path = Path::new(&expand_str);
  create_directory_if_missing(&expand_str);
  if !overwrite {
    if let Err(error) = rename_existing_path(&file_path) {
      error!("Cannot rename file {:?}: {}", &file_path, error);
    }
  }
  let txn_builder = TransactionBuilder::default();
  store_txn_builder_to_file(&expand_str, &txn_builder);
}

fn split_arg(string: &str) -> Vec<&str> {
  string.split(';').collect::<Vec<&str>>()
}

fn get_txo_refs(sids_arg: &str) -> std::result::Result<Vec<TxoRef>, std::io::Error> {
  let sids_str = split_arg(sids_arg);
  let mut txo_refs = Vec::new();
  for sid_str in sids_str {
    if let Ok(sid) = sid_str.trim().parse::<u64>() {
      txo_refs.push(TxoRef::Absolute(TxoSID(sid)));
    } else {
      return Err(Error::new(ErrorKind::InvalidInput, "Improperly formatted sids"));
    }
  }
  Ok(txo_refs)
}

fn get_blind_asset_records(blind_asset_records_arg: &str)
                           -> std::result::Result<Vec<BlindAssetRecord>, std::io::Error> {
  let blind_asset_records_str = split_arg(blind_asset_records_arg);
  let mut blind_asset_records = Vec::new();
  for blind_asset_record_str in blind_asset_records_str {
    if let Ok(blind_asset_record) = serde_json::from_str(blind_asset_record_str.trim()) {
      blind_asset_records.push(blind_asset_record);
    } else {
      return Err(Error::new(ErrorKind::InvalidInput,
                            "Improperly formatted blind asset records"));
    }
  }
  Ok(blind_asset_records)
}

fn get_amounts(amounts_arg: &str) -> std::result::Result<Vec<u64>, std::io::Error> {
  let amounts_str = split_arg(amounts_arg);
  let mut amounts = Vec::new();
  for amount_str in amounts_str {
    if let Ok(amount) = amount_str.trim().parse::<u64>() {
      amounts.push(amount);
    } else {
      return Err(Error::new(ErrorKind::InvalidInput, "Improperly formatted amounts"));
    }
  }
  Ok(amounts)
}

fn get_addresses(addresses_arg: &str) -> Vec<AccountAddress> {
  let addresses_str = split_arg(addresses_arg);
  let mut addresses = Vec::new();
  for address_str in addresses_str {
    addresses.push(AccountAddress { key: XfrPublicKey::zei_from_bytes(address_str.trim()
                                                                                 .as_bytes()) })
  }
  addresses
}

fn process_add_cmd(add_matches: &clap::ArgMatches,
                   keys_file_path: &str,
                   transaction_file_name: &str,
                   _findora_dir: &str) {
  // TODO (Keyao): Handle invalid key pair input?
  println!("{}", keys_file_path);
  let key_pair: XfrKeyPair;
  if let Ok(kp) = load_key_pair_from_files(&keys_file_path) {
    key_pair = kp;
  } else {
    error!("Valid keyfile required for this command; if no keyfile currently exists, try running \"findora_txn_builder keygen\"");
    return;
  }
  match add_matches.subcommand() {
    ("define_asset", Some(define_asset_matches)) => {
      let token_code = define_asset_matches.value_of("token_code");
      let memo = define_asset_matches.value_of("memo")
                                     .unwrap_or("{}")
                                     .to_string();
      let allow_updates = define_asset_matches.is_present("allow_updates");
      let traceable = define_asset_matches.is_present("traceable");
      if let Err(e) = load_txn_builder_from_file(&transaction_file_name) {
        println!("{:?}", e);
      }
      if let Ok(mut txn_builder) = load_txn_builder_from_file(&transaction_file_name) {
        let asset_token: AssetTypeCode;
        if let Some(token_code) = token_code {
          asset_token = AssetTypeCode::new_from_str(token_code);
        } else {
          asset_token = AssetTypeCode::gen_random();
          println!("Creating asset with token code {:?}", asset_token.val);
        }
        if let Ok(_res) = txn_builder.add_operation_create_asset(&key_pair,
                                                                 Some(asset_token),
                                                                 allow_updates,
                                                                 traceable,
                                                                 &memo)
        {
          store_txn_builder_to_file(&transaction_file_name, &txn_builder);
        } else {
          println!("Failed to add operation to transaction.");
        }
      }
    }
    ("issue_asset", Some(issue_asset_matches)) => {
      let token_code = issue_asset_matches.value_of("token_code");
      let seq_num;
      if let Some(sequence_number_arg) = issue_asset_matches.value_of("sequence_number") {
        if let Ok(seq_num_parsed) = sequence_number_arg.parse::<u64>() {
          seq_num = seq_num_parsed;
        } else {
          println!("Improperly formatted sequence number.");
          return;
        }
      } else {
        println!("Sequence number is required to issue asset.");
        return;
      }
      let amount;
      if let Some(amount_arg) = issue_asset_matches.value_of("amount") {
        if let Ok(amount_parsed) = amount_arg.parse::<u64>() {
          amount = amount_parsed;
        } else {
          println!("Improperly formatted amount.");
          return;
        }
      } else {
        println!("Amount is required to issue asset.");
        return;
      }
      if let Ok(mut txn_builder) = load_txn_builder_from_file(&transaction_file_name) {
        let asset_token: AssetTypeCode;
        if let Some(token_code) = token_code {
          asset_token = AssetTypeCode::new_from_str(token_code);
        } else {
          println!("Token code is required to issue asset.");
          return;
        }

        if let Ok(_res) =
          txn_builder.add_basic_issue_asset(&key_pair, &None, &asset_token, seq_num, amount)
        {
          store_txn_builder_to_file(&transaction_file_name, &txn_builder);
        } else {
          println!("Failed to add operation to transaction.");
        }
      }
    }
    ("transfer_asset", Some(transfer_asset_matches)) => {
      // Compose transfer_from for add_basic_transfer_asset
      let txo_refs;
      if let Some(sids_arg) = transfer_asset_matches.value_of("sids") {
        txo_refs = get_txo_refs(sids_arg).unwrap();
      } else {
        println!("TxoSID sids are required to transfer asset.");
        return;
      }
      let blind_asset_records;
      if let Some(blind_asset_records_arg) = transfer_asset_matches.value_of("blind_asset_record") {
        blind_asset_records = get_blind_asset_records(blind_asset_records_arg).unwrap();
      } else {
        println!("Blind asset records are required to transfer asset.");
        return;
      }
      let input_amounts;
      if let Some(input_amounts_arg) = transfer_asset_matches.value_of("input_amounts") {
        input_amounts = get_amounts(input_amounts_arg).unwrap();
      } else {
        println!("Input amounts are required to transfer asset.");
        return;
      }
      let mut count = txo_refs.len();
      if blind_asset_records.len() != count || input_amounts.len() != count {
        println!("Size of input sids, blind asset records, and input amounts should match.");
        return;
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
        println!("Output amounts are required to transfer asset.");
        return;
      }
      let addresses;
      if let Some(addresses_arg) = transfer_asset_matches.value_of("addresses") {
        addresses = get_addresses(addresses_arg);
      } else {
        println!("Addresses are required to transfer asset.");
        return;
      }
      let mut count = output_amounts.len();
      if addresses.len() != count {
        println!("Size of output amounts and addresses should match.");
        return;
      }
      let mut transfer_to = Vec::new();
      let mut output_amounts_iter = output_amounts.iter();
      let mut addresses_iter = addresses.iter();
      while count > 0 {
        transfer_to.push((*output_amounts_iter.next().unwrap(), addresses_iter.next().unwrap()));
        count -= 1;
      }

      // Transfer asset
      if let Ok(mut txn_builder) = load_txn_builder_from_file(&transaction_file_name) {
        if let Ok(_res) =
          txn_builder.add_basic_transfer_asset(&XfrKeyPair::zei_from_bytes(keys_file_path.as_bytes()),
                                               &transfer_from[..],
                                               &transfer_to[..])
        {
          store_txn_builder_to_file(&transaction_file_name, &txn_builder);
        } else {
          println!("Failed to add operation to transaction.");
        }
      }
    }
    _ => unreachable!(),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::{build_blind_asset_record, AssetRecordType};
  use zei::xfr::structs::AssetRecord;

  fn check_next_path(input: &str, expected: &str) {
    let as_path = Path::new(input);
    match next_path(as_path) {
      Ok(result) => {
        let as_str = result.to_str().unwrap();
        if as_str != expected.to_string() {
          panic!("{} failed:  {}", input, as_str);
        }
      }
      Err(error) => {
        panic!("next_path returned an error: {}", error);
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

    let as_path = Path::new(".");
    if let Err(_) = next_path(as_path) {
      // This is the error:
      // Custom { kind: InvalidData, error: "Is directory: \".\"" }
    } else {
      panic!("Expecting directory error");
    }
  }

  #[test]
  fn test_rename_existing_path() {
    // Path name shoud be different from those in test_next_path()
    // Otherwise may cause test_next_path() to fail
    let as_path = Path::new("10");

    // Remove the file if it exists
    if as_path.exists() {
      fs::remove_file(as_path).unwrap();
    }

    assert_eq!(rename_existing_path(as_path).map_err(|e| e.kind()),
               Err(ErrorKind::NotFound));
  }

  #[test]
  fn test_get_txo_refs() {
    let sids_arg_num = "1;2;4";
    let sids_arg_num_space = "1; 2;4";
    let sids_arg_num_letter = "1;2;a";

    let expected_sids = vec![TxoRef::Absolute(TxoSID(1)),
                             TxoRef::Absolute(TxoSID(2)),
                             TxoRef::Absolute(TxoSID(4))];

    assert_eq!(get_txo_refs(sids_arg_num).unwrap(), expected_sids);
    assert_eq!(get_txo_refs(sids_arg_num_space).unwrap(), expected_sids);
    assert_eq!(get_txo_refs(sids_arg_num_letter).map_err(|e| e.kind()),
               Err(ErrorKind::InvalidInput));
  }

  #[test]
  fn test_get_addresses() {
    let mut addresses_arg = String::from_utf8(vec![0; 32]).unwrap();
    addresses_arg.push_str(";");
    addresses_arg.push_str(&String::from_utf8(vec![1; 32]).unwrap());

    let expected_addresses = vec![AccountAddress { key: XfrPublicKey::zei_from_bytes(&[0; 32]) },
                                  AccountAddress { key: XfrPublicKey::zei_from_bytes(&[1; 32]) }];

    assert_eq!(get_addresses(&addresses_arg), expected_addresses);
  }

  #[test]
  fn test_get_blind_asset_records() {
    let art_0 = AssetRecordType::ConfidentialAmount_ConfidentialAssetType;
    let blind_asset_record_0 =
      build_blind_asset_record(&mut ChaChaRng::from_entropy(),
                               &PublicParams::new().pc_gens,
                               &AssetRecord::new(10,
                                                 [0x1; 16],
                                                 XfrPublicKey::zei_from_bytes(&[0; 32])).unwrap(),
                               art_0,
                               &None);
    let art_1 = AssetRecordType::PublicAmount_PublicAssetType;
    let blind_asset_record_1 =
      build_blind_asset_record(&mut ChaChaRng::from_entropy(),
                               &PublicParams::new().pc_gens,
                               &AssetRecord::new(100,
                                                 [0x0; 16],
                                                 XfrPublicKey::zei_from_bytes(&[1; 32])).unwrap(),
                               art_1,
                               &None);

    let mut blind_asset_records_arg = serde_json::to_string(&blind_asset_record_0).unwrap();
    blind_asset_records_arg.push_str(";");
    blind_asset_records_arg.push_str(&serde_json::to_string(&blind_asset_record_1).unwrap());

    assert!(get_blind_asset_records(&blind_asset_records_arg).is_ok());
  }

  #[test]
  fn test_get_amounts() {
    let amounts_arg = "1; 2;4";
    let expected_amounts = vec![1, 2, 4];

    assert_eq!(get_amounts(amounts_arg).unwrap(), expected_amounts);
  }
}
