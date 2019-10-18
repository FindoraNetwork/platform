use clap::{App, Arg, SubCommand};
use env_logger::{Env, Target};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AccountAddress, AssetTypeCode, IssuerPublicKey, TxoRef, TxoSID};
use log::{error, trace}; // Other options: debug, info, warn
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use std::env;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use txn_builder::{BuildsTransactions, TransactionBuilder};
use zei::basic_crypto::signatures::{XfrKeyPair, XfrPublicKey, XfrSecretKey};
use zei::serialization::ZeiFromToBytes;
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

fn store_txn_builder_to_file(file_name: &str, txn: &TransactionBuilder) {
  if let Ok(as_json) = serde_json::to_string(txn) {
    let _skip = fs::write(file_name, &as_json).or_else(|_e| {
                  println!("Transaction file {} could not be created", file_name);
                  Err(PlatformError::IoError("unable to write".to_string()))
                });
  }
}

fn load_key_pair_from_files(priv_file_name: &str)
                            -> Result<(XfrPublicKey, XfrSecretKey), PlatformError> {
  let mut priv_file = File::open(priv_file_name).or_else(|_e| {
                                                  println!("Failed to open private key file {}",
                                                           priv_file_name);
                                                  Err(PlatformError::DeserializationError)
                                                })?;

  let sk: XfrSecretKey;
  let mut sk_byte_buffer = Vec::new();
  match priv_file.read_to_end(&mut sk_byte_buffer) {
    Ok(_len) => {
      sk = XfrSecretKey::zei_from_bytes(&sk_byte_buffer);
    }
    Err(_e) => {
      println!("Failed to read private key file {}", priv_file_name);
      return Err(PlatformError::IoError("unable to read".to_string()));
    }
  }

  let pub_file_path = if priv_file_name.ends_with(".private") {
    let (begin, _) = priv_file_name.split_at(priv_file_name.len() - "private".len());
    begin.to_string() + "pub"
  } else {
    priv_file_name.to_string() + "pub"
  };

  let mut pub_file = File::open(&pub_file_path).or_else(|_e| {
                       println!("Failed to open public key file {}", &pub_file_path);
                       Err(PlatformError::IoError("cannot read".to_string()))
                     })?;

  let pk: XfrPublicKey;
  let mut pk_byte_buffer = Vec::new();
  match pub_file.read_to_end(&mut pk_byte_buffer) {
    Ok(_len) => {
      pk = XfrPublicKey::zei_from_bytes(&pk_byte_buffer);
    }
    Err(_e) => {
      println!("Failed to read public key file {}", pub_file_path);
      return Err(PlatformError::IoError("cannot read".to_string()));
    }
  }

  Ok((pk, sk))
}

// Write a new key pair to the given paths.
// Create subdirectories as needed.
// Move aside any extant files at the given paths.
// Reports errors rather than returning them.
// Assumes tilde expansion has already been done on paths.
fn create_key_files(priv_file_path: &Path, pub_file_path: &Path) {
  trace!("private key path: {:?}", priv_file_path);
  trace!("public key path: {:?}", pub_file_path);
  match fs::create_dir_all(&priv_file_path.parent().unwrap()) {
    Ok(()) => {
      if let Err(error) = rename_existing_path(&priv_file_path) {
        error!("Cannot rename private key {:?}: {}", &priv_file_path, error);
      }
      if let Err(error) = rename_existing_path(&pub_file_path) {
        error!("Cannot rename public key {:?}: {}", &pub_file_path, error);
      }
      let mut prng: ChaChaRng;
      prng = ChaChaRng::from_seed([0u8; 32]);
      let keypair = XfrKeyPair::generate(&mut prng);
      match fs::write(&priv_file_path, keypair.get_sk_ref().zei_to_bytes()) {
        Ok(_) => {
          if let Err(error) = fs::write(&pub_file_path, keypair.get_pk_ref().zei_to_bytes()) {
            error!("Public key file {:?} could not be created: {}",
                   &pub_file_path, error);
          }
        }
        Err(error) => {
          error!("Private key file {:?} could not be created: {}",
                 priv_file_path, error);
        }
      };
    }
    Err(error) => {
      error!("Failed to create directories for {}: {}",
             &priv_file_path.display(),
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
// unused path cannot cannot be derived. The path must not be empty
// and must not be dot (".").
fn next_path(path: &Path) -> Result<PathBuf, std::io::Error> {
  fn add_backup_extension(path: &Path) -> PathBuf {
    let mut pb = PathBuf::from(path);
    pb.set_file_name(format!("{}.0",
                             path.file_name()
                                 .unwrap_or(OsStr::new(""))
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
      .help("Path to private key (will extrapolate public key)")
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
          .help("If specified, asset transfers can be traced by the issuert "))
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
          .long("token_code")
          .alias("tc")
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
        .arg(Arg::with_name("blind_asset_record")
          .short("bar")
          .long("blind_asset_record")
          .takes_value(true)
          .help("Specify a string representing the JSON serialization of the blind asset record of the asset to be transferred."))
        .arg(Arg::with_name("index")
          .short("idx")
          .long("index")
          .takes_value(true)
          .help("Specify TxoSID index."))
        .arg(Arg::with_name("address")
          .short("addr")
          .long("address")
          .takes_value(true)
          .help("Specify address to send tokens to."))
        .arg(Arg::with_name("transfer_amount")
          .short("tfr_amt")
          .long("transfer_amount")
          .takes_value(true)
          .help("Amount of tokens to transfer."))))
    .subcommand(SubCommand::with_name("serialize"))
    .subcommand(SubCommand::with_name("drop"))
    .subcommand(SubCommand::with_name("keygen")
      .arg(Arg::with_name("create_keys_path")
        .short("n")
        .long("name")
        .help("specify the path and name for the private key file; if the name has the form \"path/to/file_name.private\", the public key file will be \"path/to/file_name.pub\"; otherwise, \".pub\" will be appended to the name")
        .takes_value(true)))
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

  if let Some(priv_key) = inputs.value_of("keys_path") {
    keys_file_path = priv_key.to_string();
  } else {
    keys_file_path = format!("{}/keys/default.private", findora_dir);
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
          format!("{}/keys/default.private", &findora_dir)
        };
      let priv_file_str = shellexpand::tilde(&new_keys_path).to_string();
      let priv_file_path = Path::new(&priv_file_str);
      let pub_file_path = priv_file_path.with_extension("pub");
      create_key_files(&priv_file_path, &pub_file_path);
    }
    _ => {}
  }
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

fn process_add_cmd(add_matches: &clap::ArgMatches,
                   keys_file_path: &str,
                   transaction_file_name: &str,
                   _findora_dir: &str) {
  let pub_key: XfrPublicKey;
  let priv_key: XfrSecretKey;
  if let Ok((pub_key_out, priv_key_out)) = load_key_pair_from_files(&keys_file_path) {
    pub_key = pub_key_out;
    priv_key = priv_key_out;
  } else {
    println!("Valid keyfile required for this command; if no keyfile currently exists, try running \"findora_txn_builder keygen\"");
    return;
  }
  match add_matches.subcommand() {
    ("define_asset", Some(define_asset_matches)) => {
      let token_code = define_asset_matches.value_of("token_code");
      let memo = define_asset_matches.value_of("memo")
                                     .unwrap_or("{}")
                                     .to_string();
      let allow_updates = define_asset_matches.is_present("allow_updates");
      let confidential = define_asset_matches.is_present("confidential");
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
        if let Ok(_res) = txn_builder.add_operation_create_asset(&IssuerPublicKey { key: pub_key },
                                                                 &priv_key,
                                                                 Some(asset_token),
                                                                 allow_updates,
                                                                 traceable,
                                                                 &memo,
                                                                 confidential)
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

        if let Ok(_res) = txn_builder.add_basic_issue_asset(&IssuerPublicKey { key: pub_key },
                                                            &priv_key,
                                                            &asset_token,
                                                            seq_num,
                                                            amount)
        {
          store_txn_builder_to_file(&transaction_file_name, &txn_builder);
        } else {
          println!("Failed to add operation to transaction.");
        }
      }
    }
    ("transfer_asset", Some(transfer_asset_matches)) => {
      let index;
      if let Some(index_arg) = transfer_asset_matches.value_of("index") {
        if let Ok(index_num_parsed) = index_arg.parse::<u64>() {
          index = index_num_parsed;
        } else {
          println!("Improperly formatted index.");
          return;
        }
      } else {
        println!("TxoSID index is required to transfer asset.");
        return;
      }
      let amount;
      if let Some(amount_arg) = transfer_asset_matches.value_of("transfer_amount") {
        if let Ok(amount_arg_parsed) = amount_arg.parse::<u64>() {
          amount = amount_arg_parsed;
        } else {
          println!("Improperly formatted amount.");
          return;
        }
      } else {
        println!("Amount is required to transfer asset.");
        return;
      }
      let blind_asset_record: BlindAssetRecord;
      if let Some(blind_asset_record_arg) = transfer_asset_matches.value_of("blind_asset_record") {
        if let Ok(blind_asset_record_parsed) = serde_json::from_str(&blind_asset_record_arg) {
          blind_asset_record = blind_asset_record_parsed;
        } else {
          println!("Improperly formatted blind asset record JSON.");
          return;
        }
      } else {
        println!("Blind asset record JSON is required to transfer asset.");
        return;
      }
      let address: XfrPublicKey;
      if let Some(address_arg) = transfer_asset_matches.value_of("address") {
        address = XfrPublicKey::zei_from_bytes(address_arg.as_bytes());
      } else {
        println!("Address required for transfer.");
        return;
      }
      if let Ok(mut txn_builder) = load_txn_builder_from_file(&transaction_file_name) {
        if let Ok(_res) =
          txn_builder.add_basic_transfer_asset(&[(&TxoRef::Absolute(TxoSID(index)),
                                                  &blind_asset_record,
                                                  amount,
                                                  &priv_key)],
                                               &[(amount, &AccountAddress { key: address })])
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
}
