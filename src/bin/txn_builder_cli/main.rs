extern crate clap;
extern crate core;
extern crate dirs;
extern crate serde;
extern crate serde_json;
extern crate txn_builder;
extern crate zei;

use clap::{App, Arg, SubCommand};
use core::data_model::errors::PlatformError;
use core::data_model::{AccountAddress, AssetTokenCode, IssuerPublicKey, TxoSID};
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use std::env;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::Path;
use txn_builder::{BuildsTransactions, TransactionBuilder};
use zei::basic_crypto::signatures::{XfrKeyPair, XfrPublicKey, XfrSecretKey};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::structs::BlindAssetRecord;

fn load_txn_builder_from_file(file_name: &str) -> Result<TransactionBuilder, PlatformError> {
  let mut file = File::open(file_name).or_else(|_e| {
                                        println!("Transaction file {} does not exist", file_name);
                                        Err(PlatformError::DeserializationError)
                                      })?;
  let mut contents = String::new();
  file.read_to_string(&mut contents).or_else(|_e| {
                                       println!("Failed to read transaction file {}", file_name);
                                       Err(PlatformError::DeserializationError)
                                     })?;
  println!("Parsing builder from file contents: \"{}\"", &contents);
  let builder = serde_json::from_str(&contents)?;
  Ok(builder)
}

fn store_txn_builder_to_file(file_name: &str, txn: &TransactionBuilder) {
  if let Ok(as_json) = serde_json::to_string(txn) {
    let _skip = fs::write(file_name, &as_json).or_else(|_e| {
                  println!("Transaction file {} could not be created", file_name);
                  Err(PlatformError::SerializationError)
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
      return Err(PlatformError::DeserializationError);
    }
  }

  let pub_file_name = if priv_file_name.ends_with(".private") {
    let (begin, _) = priv_file_name.split_at(priv_file_name.len() - "private".len());
    begin.to_string() + "pub"
  } else {
    priv_file_name.to_string() + "pub"
  };

  let mut pub_file = File::open(&pub_file_name).or_else(|_e| {
                                                 println!("Failed to open public key file {}",
                                                          &pub_file_name);
                                                 Err(PlatformError::DeserializationError)
                                               })?;

  let pk: XfrPublicKey;
  let mut pk_byte_buffer = Vec::new();
  match pub_file.read_to_end(&mut pk_byte_buffer) {
    Ok(_len) => {
      pk = XfrPublicKey::zei_from_bytes(&pk_byte_buffer);
    }
    Err(_e) => {
      println!("Failed to read public key file {}", pub_file_name);
      return Err(PlatformError::DeserializationError);
    }
  }

  Ok((pk, sk))
}

fn create_key_files(priv_file_name: &str) {
  let mut prng: ChaChaRng;
  prng = ChaChaRng::from_seed([0u8; 32]);
  let keypair = XfrKeyPair::generate(&mut prng);
  let pub_file_name = if priv_file_name.ends_with(".private") {
    let (begin, _) = priv_file_name.split_at(priv_file_name.len() - "private".len());
    begin.to_string() + "pub"
  } else {
    priv_file_name.to_string() + "pub"
  };
  rename_existing_file(&priv_file_name);
  rename_existing_file(&pub_file_name);
  let _skip_priv = fs::write(priv_file_name, keypair.get_sk_ref().zei_to_bytes()).or_else(|_e| {
                     println!("Private key file {} could not be created", priv_file_name);
                     Err(PlatformError::SerializationError)
                   });
  let _skip_pub = fs::write(&pub_file_name, keypair.get_pk_ref().zei_to_bytes()).or_else(|_e| {
                    println!("Public key file {} could not be created", &pub_file_name);
                    Err(PlatformError::SerializationError)
                  });
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

fn rename_existing_file(_path_to_existing_file: &str) {
  // TODO: if path_to_existing_file ends in .<number>, find the next unused .<number+N> and rename;
  // otherwise, start from .1 and do the same...
}

fn main() {
  let inputs = App::new("Transaction Builder")
    .version("0.0.1")
    .about("©2019 eian.io")
    .arg(Arg::with_name("config")
      .short("c")
      .long("config")
      .value_name("PATH/TO/FILE")
      .help("Specify a custom config file (default: \"$EIAN_DIR/config.toml\")")
      .takes_value(true))
    .arg(Arg::with_name("eian_dir")
      .short("d")
      .long("dir")
      .value_name("PATH")
      .help("Directory for configuaration, security, and temporary files; must be writable")
      .takes_value(true)
      .env("EIAN_DIR"))
    .arg(Arg::with_name("keys_path")
      .short("k")
      .long("keys")
      .value_name("PATH/TO/FILE")
      .help("Path to private key (will extrapolate public key)")
      .takes_value(true))
    .arg(Arg::with_name("txn")
      .long("txn")
      .value_name("FILE")
      .help("Use a named transaction file (will always be under eian_dir)")
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
          .short("tc")
          .long("token_code")
          .help("Specify an explicit 16 character token code for the new asset; must be a unique name. If specified code is already in use, transaction will fail. If not specified, will display automatically generated token code.")
          .takes_value(true))
        .arg(Arg::with_name("allow_updates")
          .short("u")
          .long("allow_updates")
          .help("If specified, updates may be made to asset memo"))
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
    .subcommand(SubCommand::with_name("serialize")
      .arg(Arg::with_name("")))
    .subcommand(SubCommand::with_name("drop")
      .arg(Arg::with_name("")))
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
  let eian_dir = if let Some(dir) = inputs.value_of("eian_dir") {
    dir.to_string()
  } else if let Ok(dir) = env::var("EIAN_DIR") {
    dir
  } else {
    let home_dir = dirs::home_dir().unwrap_or_else(|| Path::new(".").to_path_buf());
    format!("{}/.eian", home_dir.to_str().unwrap_or("./.eian"))
  };

  if let Some(cfg) = inputs.value_of("config") {
    _config_file_path = cfg.to_string();
  } else {
    _config_file_path = format!("{}/config.toml", eian_dir);
  }

  if let Some(priv_key) = inputs.value_of("keys_path") {
    keys_file_path = priv_key.to_string();
  } else {
    keys_file_path = format!("{}/keys/default.private", eian_dir);
  }

  if let Some(txn_store) = inputs.value_of("txn") {
    transaction_file_name = txn_store.to_string();
  } else {
    transaction_file_name = format!("{}/current.txn", eian_dir);
  }

  match inputs.subcommand() {
    ("create", Some(create_matches)) => {
      process_create_cmd(create_matches,
                         &keys_file_path,
                         &transaction_file_name,
                         &eian_dir);
    }
    ("add", Some(add_matches)) => {
      process_add_cmd(add_matches,
                      &keys_file_path,
                      &transaction_file_name,
                      &eian_dir);
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
      let new_keys_path_in = keygen_matches.value_of("create_keys_path");
      let new_keys_path: String;
      if let Some(new_keys_path_in) = new_keys_path_in {
        new_keys_path = new_keys_path_in.to_string();
      } else {
        new_keys_path = format!("{}/keys/default.private", &eian_dir);
      }
      create_directory_if_missing(&new_keys_path);
      create_key_files(&new_keys_path);
    }
    _ => {}
  }
}

fn process_create_cmd(create_matches: &clap::ArgMatches,
                      _keys_file_path: &str,
                      transaction_file_name: &str,
                      _eian_dir: &str) {
  let named = create_matches.value_of("named");
  let overwrite = create_matches.is_present("overwrite");
  let file_path: String;
  if let Some(named) = named {
    file_path = named.to_string();
  } else {
    file_path = transaction_file_name.to_string();
  }
  create_directory_if_missing(&file_path);
  if !overwrite {
    rename_existing_file(&file_path);
  }
  let txn_builder = TransactionBuilder::default();
  store_txn_builder_to_file(&file_path, &txn_builder);
}
fn process_add_cmd(add_matches: &clap::ArgMatches,
                   keys_file_path: &str,
                   transaction_file_name: &str,
                   _eian_dir: &str) {
  let pub_key: XfrPublicKey;
  let priv_key: XfrSecretKey;
  if let Ok((pub_key_out, priv_key_out)) = load_key_pair_from_files(&keys_file_path) {
    pub_key = pub_key_out;
    priv_key = priv_key_out;
  } else {
    println!("Valid keyfile required for this command; if no keyfile currently exists, try running \"eian_txn_builder keygen\"");
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
      if let Err(e) = load_txn_builder_from_file(&transaction_file_name) {
        println!("{:?}", e);
      }
      if let Ok(mut txn_builder) = load_txn_builder_from_file(&transaction_file_name) {
        let asset_token: AssetTokenCode;
        if let Some(token_code) = token_code {
          asset_token = AssetTokenCode::new_from_str(token_code);
        } else {
          asset_token = AssetTokenCode::gen_random();
          println!("Creating asset with token code {:?}", asset_token.val);
        }
        if let Ok(_res) = txn_builder.add_operation_create_asset(&IssuerPublicKey { key: pub_key },
                                                                 &priv_key,
                                                                 Some(asset_token),
                                                                 allow_updates,
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
        let asset_token: AssetTokenCode;
        if let Some(token_code) = token_code {
          asset_token = AssetTokenCode::new_from_str(token_code);
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
          txn_builder.add_basic_transfer_asset(&[(&TxoSID { index },
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
