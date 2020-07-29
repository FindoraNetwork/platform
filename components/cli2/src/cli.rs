#![deny(warnings)]
#![allow(clippy::type_complexity)]
use ledger::data_model::*;
use serde::{Deserialize, Serialize};
use snafu::{OptionExt, ResultExt, Snafu};
use std::collections::HashMap;
use std::fs;
use structopt::StructOpt;
use submission_server::{TxnHandle, TxnStatus};
use txn_builder::TransactionBuilder;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{OpenAssetRecord, OwnerMemo};
// use std::rc::Rc;
use ledger_api_service::LedgerAccessRoutes;
use promptly::{prompt, prompt_default};
use std::process::exit;
use utils::{HashOf, NetworkRoute, SignatureOf};
// use utils::Serialized;
// use txn_builder::{BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder};

pub mod kv;

use kv::{HasTable, KVError, KVStore};

pub struct FreshNamer {
  base: String,
  i: u64,
  delim: String,
}

impl FreshNamer {
  pub fn new(base: String, delim: String) -> Self {
    Self { base, i: 0, delim }
  }
}

impl Iterator for FreshNamer {
  type Item = String;
  fn next(&mut self) -> Option<String> {
    let ret = if self.i == 0 {
      self.base.clone()
    } else {
      format!("{}{}{}", self.base, self.delim, self.i - 1)
    };
    self.i += 1;
    Some(ret)
  }
}

fn default_sub_server() -> String {
  "https://testnet.findora.org/submit_server".to_string()
}

fn default_ledger_server() -> String {
  "https://testnet.findora.org/query_server".to_string()
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
struct CliConfig {
  #[serde(default = "default_sub_server")]
  pub submission_server: String,
  #[serde(default = "default_ledger_server")]
  pub ledger_server: String,
  pub open_count: u64,
  #[serde(default)]
  pub ledger_sig_key: Option<XfrPublicKey>,
  #[serde(default)]
  pub ledger_state: Option<(HashOf<Option<StateCommitmentData>>,
                            u64,
                            SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>)>,
}

impl HasTable for CliConfig {
  const TABLE_NAME: &'static str = "config";
  type Key = String;
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
pub struct AssetTypeName(pub String);

impl HasTable for AssetTypeEntry {
  const TABLE_NAME: &'static str = "asset_types";
  type Key = AssetTypeName;
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
pub struct KeypairName(pub String);

impl HasTable for XfrKeyPair {
  const TABLE_NAME: &'static str = "key_pairs";
  type Key = KeypairName;
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
pub struct PubkeyName(pub String);

impl HasTable for XfrPublicKey {
  const TABLE_NAME: &'static str = "public_keys";
  type Key = PubkeyName;
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
pub struct TxnName(pub String);

impl HasTable for (Transaction, TxnMetadata) {
  const TABLE_NAME: &'static str = "transactions";
  type Key = TxnName;
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
pub struct TxnBuilderName(pub String);

impl HasTable for TxnBuilderEntry {
  const TABLE_NAME: &'static str = "transaction_builders";
  type Key = TxnBuilderName;
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
pub struct TxoName(pub String);

impl HasTable for TxoCacheEntry {
  const TABLE_NAME: &'static str = "txo_cache";
  type Key = TxoName;
}

#[derive(Snafu, Debug)]
enum CliError {
  #[snafu(context(false))]
  KV { source: KVError },
  #[snafu(context(false))]
  #[snafu(display("Error reading user input: {}", source))]
  RustyLine {
    source: rustyline::error::ReadlineError,
  },
  #[snafu(display("Error creating user directory or file at {}: {}", file.display(), source))]
  UserFile {
    source: std::io::Error,
    file: std::path::PathBuf,
  },
  #[snafu(display("Failed to locate user's home directory"))]
  HomeDir,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Default)]
struct TxnMetadata {
  handle: Option<TxnHandle>,
  status: Option<TxnStatus>,
  new_asset_types: HashMap<String, AssetTypeEntry>,
  // new_txos: HashMap<String, TxoCacheEntry>,
  // spent_txos: HashMap<String>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
struct TxoCacheEntry {
  sid: Option<TxoSID>,
  record: TxOutput,
  owner_memo: Option<OwnerMemo>,
  opened_record: Option<OpenAssetRecord>,
  unspent: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
struct AssetTypeEntry {
  asset: AssetType,
  issuer_nick: Option<String>,
}

fn indent_of(indent_level: u64) -> String {
  let mut ret: String = Default::default();
  for _ in 0..indent_level {
    ret = format!("{}{}", ret, " ");
  }
  ret
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum OpMetadata {
  DefineAsset {
    issuer_nick: String,
    asset_nick: String,
  },
}

fn display_op_metadata(indent_level: u64, ent: &OpMetadata) {
  let ind = indent_of(indent_level);
  match ent {
    OpMetadata::DefineAsset { asset_nick,
                              issuer_nick, } => {
      println!("{}DefineAsset `{}`", ind, asset_nick);
      println!("{} issued by `{}`", ind, issuer_nick);
    }
  }
}

fn display_txn_builder(indent_level: u64, ent: &TxnBuilderEntry) {
  let ind = indent_of(indent_level);
  println!("{}Operations:", ind);

  for op in ent.operations.iter() {
    display_op_metadata(indent_level + 1, op);
  }

  println!("{}New asset types defined:", ind);
  for (nick, asset_ent) in ent.new_asset_types.iter() {
    println!("{} {}:", ind, nick);
    display_asset_type(indent_level + 2, asset_ent);
  }
}

fn display_asset_type(indent_level: u64, ent: &AssetTypeEntry) {
  let ind = indent_of(indent_level);
  println!("{}issuer nickname: {}",
           ind,
           ent.issuer_nick
              .clone()
              .unwrap_or_else(|| "<UNKNOWN>".to_string()));
  println!("{}issuer public key: {}",
           ind,
           serde_json::to_string(&ent.asset.properties.issuer.key).unwrap());
  println!("{}code: {}", ind, ent.asset.properties.code.to_base64());
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct TxnBuilderEntry {
  builder: TransactionBuilder,
  #[serde(default)]
  operations: Vec<OpMetadata>,
  #[serde(default)]
  new_asset_types: HashMap<String, AssetTypeEntry>,
}

trait CliDataStore {
  fn get_config(&self) -> Result<CliConfig, CliError>;
  fn update_config<F: FnOnce(&mut CliConfig)>(&mut self, f: F) -> Result<(), CliError>;

  fn get_keypairs(&self) -> Result<HashMap<KeypairName, XfrKeyPair>, CliError>;
  fn get_keypair(&self, k: &KeypairName) -> Result<Option<XfrKeyPair>, CliError>;
  fn delete_keypair(&mut self, k: &KeypairName) -> Result<Option<XfrKeyPair>, CliError>;
  fn get_pubkeys(&self) -> Result<HashMap<PubkeyName, XfrPublicKey>, CliError>;
  fn get_pubkey(&self, k: &PubkeyName) -> Result<Option<XfrPublicKey>, CliError>;
  fn delete_pubkey(&mut self, k: &PubkeyName) -> Result<Option<XfrPublicKey>, CliError>;
  fn add_key_pair(&mut self, k: &KeypairName, kp: XfrKeyPair) -> Result<(), CliError>;
  fn add_public_key(&mut self, k: &PubkeyName, pk: XfrPublicKey) -> Result<(), CliError>;

  fn get_built_transactions(&self)
                            -> Result<HashMap<TxnName, (Transaction, TxnMetadata)>, CliError>;
  fn get_built_transaction(&self,
                           k: &TxnName)
                           -> Result<Option<(Transaction, TxnMetadata)>, CliError>;
  fn build_transaction(&mut self,
                       k_orig: &TxnBuilderName,
                       k_new: &TxnName)
                       -> Result<(Transaction, TxnMetadata), CliError>;
  fn update_txn_metadata<F: FnOnce(&mut TxnMetadata)>(&mut self,
                                                      k: &TxnName,
                                                      f: F)
                                                      -> Result<(), CliError>;

  fn prepare_transaction(&mut self, k: &TxnBuilderName, seq_id: u64) -> Result<(), CliError>;
  fn get_txn_builder(&self, k: &TxnBuilderName) -> Result<Option<TxnBuilderEntry>, CliError>;
  fn get_txn_builders(&self) -> Result<HashMap<TxnBuilderName, TxnBuilderEntry>, CliError>;
  fn with_txn_builder<F: FnOnce(&mut TxnBuilderEntry)>(&mut self,
                                                       k: &TxnBuilderName,
                                                       f: F)
                                                       -> Result<(), CliError>;

  fn get_cached_txos(&self) -> Result<HashMap<TxoName, TxoCacheEntry>, CliError>;
  fn get_cached_txo(&self, k: &TxoName) -> Result<Option<TxoCacheEntry>, CliError>;
  fn delete_cached_txo(&mut self, k: &TxoName) -> Result<(), CliError>;
  fn cache_txo(&mut self, k: &TxoName, ent: TxoCacheEntry) -> Result<(), CliError>;

  fn get_asset_types(&self) -> Result<HashMap<AssetTypeName, AssetTypeEntry>, CliError>;
  fn get_asset_type(&self, k: &AssetTypeName) -> Result<Option<AssetTypeEntry>, CliError>;
  fn update_asset_type<F: FnOnce(&mut AssetTypeEntry)>(&mut self,
                                                       k: &AssetTypeName,
                                                       f: F)
                                                       -> Result<(), CliError>;
  fn delete_asset_type(&self, k: &AssetTypeName) -> Result<Option<AssetTypeEntry>, CliError>;
  fn add_asset_type(&self, k: &AssetTypeName, ent: AssetTypeEntry) -> Result<(), CliError>;
}

fn prompt_for_config(prev_conf: Option<CliConfig>) -> Result<CliConfig, CliError> {
  let default_sub_server = prev_conf.as_ref()
                                    .map(|x| x.submission_server.clone())
                                    .unwrap_or_else(default_sub_server);
  let default_ledger_server = prev_conf.as_ref()
                                       .map(|x| x.ledger_server.clone())
                                       .unwrap_or_else(default_ledger_server);
  Ok(CliConfig { submission_server: prompt_default("Submission Server?", default_sub_server)?,
                 ledger_server: prompt_default("Ledger Access Server?", default_ledger_server)?,
                 open_count: 0,
                 ledger_sig_key: prev_conf.as_ref().and_then(|x| x.ledger_sig_key),
                 ledger_state: prev_conf.as_ref().and_then(|x| x.ledger_state.clone()) })
}

#[derive(StructOpt, Debug)]
#[structopt(about = "Build and manage transactions and assets on a findora ledger",
            rename_all = "kebab-case")]
enum Actions {
  /// Initialize or change your local database configuration
  Setup {},

  /// Display the current configuration and ledger state
  ListConfig {},

  /// Run integrity checks of the local database
  CheckDb {},

  /// Get the latest state commitment data from the ledger
  QueryLedgerState {
    /// Whether to forget the old ledger public key
    #[structopt(short, long)]
    forget_old_key: bool,
  },

  /// Generate a new key pair for <nick>
  KeyGen {
    /// Identity nickname
    nick: String,
  },

  /// Load an existing key pair for <nick>
  LoadKeypair {
    /// Identity nickname
    nick: String,
  },

  /// Load a public key for <nick>
  LoadPublicKey {
    /// Identity nickname
    nick: String,
  },

  ListKeys {},

  /// Display information about the public key for <nick>
  ListPublicKey {
    /// Identity nickname
    nick: String,
  },

  /// Display information about the key pair for <nick>
  ListKeypair {
    /// Identity nickname
    nick: String,
  },

  /// Permanently delete the key pair for <nick>
  DeleteKeypair {
    /// Identity nickname
    nick: String,
  },

  /// Permanently delete the public key for <nick>
  DeletePublicKey {
    /// Identity nickname
    nick: String,
  },

  ListAssetTypes {},
  ListAssetType {
    /// Asset type nickname
    nick: String,
  },
  QueryAssetType {
    /// Asset type nickname
    nick: String,
    /// Asset type code (b64)
    code: String,
  },

  /// Initialize a transaction builder
  PrepareTransaction {
    /// Optional transaction name
    #[structopt(default_value = "txn")]
    nick: String,
    /// Force the transaction's name to be <nick>, instead of the first free <nick>.<n>
    #[structopt(short, long)]
    exact: bool,
  },

  /// List the transaction builders which are in progress
  ListTxnBuilders {},

  /// List the details of a transaction builder
  ListTxnBuilder {
    /// Which transaction?
    nick: String,
  },

  DefineAsset {
    #[structopt(short, long)]
    /// Which txn?
    txn: Option<String>,
    /// Issuer key
    key_nick: String,
    /// Name for the asset type
    asset_nick: String,
  },
  IssueAsset {
    #[structopt(short, long)]
    /// Which txn?
    txn: Option<String>,
    /// Issuer key
    key_nick: String,
    /// Name for the asset type
    asset_name: String,
    /// Amount to issue
    amount: u64,
  },
  TransferAsset {
    #[structopt(short, long)]
    /// Which txn?
    txn: Option<String>,
  },
  ListBuiltTransaction {
    /// txn id
    txn: Option<String>,
  },
  ListBuiltTransactions {
    // TODO: options?
  },
  Submit {
    #[structopt(short, long, default_value = "http://localhost:8669")]
    /// Base URL for the submission server
    server: String,
    /// Which txn?
    txn: String,
  },
  Status {
    #[structopt(short, long, default_value = "http://localhost:8669")]
    /// Base URL for the submission server
    server: String,
    // TODO: how are we indexing in-flight transactions?
    /// Which txn?
    txn: String,
  },

  ListUtxos {
    #[structopt(short, long, default_value = "http://localhost:8669")]
    /// Base URL for the submission server
    server: String,
    /// Whose UTXOs?
    id: Option<String>,
  },
}

fn print_conf(conf: &CliConfig) {
  println!("Submission server: {}", conf.submission_server);
  println!("Ledger access server: {}", conf.ledger_server);
  println!("Ledger public signing key: {}",
           conf.ledger_sig_key
               .map(|x| serde_json::to_string(&x).unwrap())
               .unwrap_or_else(|| "<UNKNOWN>".to_string()));
  println!("Ledger state commitment: {}",
           conf.ledger_state
               .as_ref()
               .map(|x| b64enc(&(x.0).0.hash))
               .unwrap_or_else(|| "<UNKNOWN>".to_string()));
  println!("Ledger block idx: {}",
           conf.ledger_state
               .as_ref()
               .map(|x| format!("{}", x.1))
               .unwrap_or_else(|| "<UNKNOWN>".to_string()));
}

fn run_action<S: CliDataStore>(action: Actions, store: &mut S) {
  // println!("{:?}", action);

  use Actions::*;
  match action {
    Setup {} => {
      store.update_config(|conf| {
        *conf = prompt_for_config(Some(conf.clone())).unwrap();
      }).unwrap();
    }

    ListConfig {} => {
      let conf = store.get_config().unwrap();
      print_conf(&conf);
    }

    QueryLedgerState { forget_old_key } => {
      store.update_config(|conf| {
        let mut new_key = forget_old_key;
        if !new_key && conf.ledger_sig_key.is_none() {
          println!("No signature key found for `{}`.", conf.ledger_server);
          new_key = new_key || prompt_default(" Retrieve a new one?", false).unwrap();
          if !new_key {
            eprintln!("Cannot check ledger state validity without a signature key.");
            exit(-1);
          }
        }

        if new_key {
          let query = format!("{}{}",conf.ledger_server,LedgerAccessRoutes::PublicKey.route());
          let resp: XfrPublicKey;
          match reqwest::blocking::get(&query) {
              Err(e) => {
                  eprintln!("Request `{}` failed: {}",query,e);
                  exit(-1);
              }
              Ok(v) => match v.json::<XfrPublicKey>() {
                  Err(e) => {
                      eprintln!("Failed to parse response: {}",e);
                      exit(-1);
                  }
                  Ok(v) => { resp = v; }
              }
          }

          println!("Saving ledger signing key `{}`",serde_json::to_string(&resp).unwrap());
          conf.ledger_sig_key = Some(resp);
        }

        assert!(conf.ledger_sig_key.is_some());

        let query = format!("{}{}",conf.ledger_server,LedgerAccessRoutes::GlobalState.route());
        let resp: (HashOf<Option<StateCommitmentData>>,
                u64,
                SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>);
        match reqwest::blocking::get(&query) {
            Err(e) => {
                eprintln!("Request `{}` failed: {}",query,e);
                exit(-1);
            }
            Ok(v) => match v.json::<_>() {
                Err(e) => {
                    eprintln!("Failed to parse response: {}",e);
                    exit(-1);
                }
                Ok(v) => { resp = v; }
            }
        }

        if let Err(e) = resp.2.verify(&conf.ledger_sig_key.unwrap(), &(resp.0.clone(),resp.1)) {
            eprintln!("Ledger responded with invalid signature: {}",e);
            exit(-1);
        }

        conf.ledger_state = Some(resp);

        assert!(conf.ledger_state.is_some());

        println!("New state retrieved.");

        print_conf(&conf);
      }).unwrap();
    }

    KeyGen { nick } => {
      let kp = XfrKeyPair::generate(&mut rand::thread_rng());
      store.add_public_key(&PubkeyName(nick.to_string()), *kp.get_pk_ref())
           .unwrap();
      store.add_key_pair(&KeypairName(nick.to_string()), kp)
           .unwrap();
      println!("New key pair added for `{}`", nick);
    }

    ListKeypair { nick } => {
      let kp = store.get_keypair(&KeypairName(nick.to_string())).unwrap();
      let kp = kp.map(|x| serde_json::to_string(&x).unwrap())
                 .unwrap_or(format!("No keypair with name `{}` found", nick));
      println!("{}", kp);
    }
    ListPublicKey { nick } => {
      let pk = store.get_pubkey(&PubkeyName(nick.to_string())).unwrap();
      let pk = pk.map(|x| serde_json::to_string(&x).unwrap())
                 .unwrap_or(format!("No public key with name {} found", nick));
      println!("{}", pk);
    }

    LoadKeypair { nick } => {
      match serde_json::from_str::<XfrKeyPair>(&prompt::<String,_>(format!("Please paste in the key pair for `{}`",nick)).unwrap()) {
        Err(e) => {
          eprintln!("Could not parse key pair: {}",e);
          exit(-1);
        }
        Ok(kp) => {
          store.add_public_key(&PubkeyName(nick.to_string()), *kp.get_pk_ref())
            .unwrap();
          store.add_key_pair(&KeypairName(nick.to_string()), kp)
              .unwrap();
          println!("New key pair added for `{}`", nick);
        }
      }
    }
    LoadPublicKey { nick } => {
      match serde_json::from_str(&prompt::<String,_>(format!("Please paste in the public key for `{}`",nick)).unwrap()) {
        Err(e) => {
          eprintln!("Could not parse key pair: {}",e);
          exit(-1);
        }
        Ok(pk) => {
          store.add_public_key(&PubkeyName(nick.to_string()), pk)
            .unwrap();
          println!("New public key added for `{}`", nick);
        }
      }
    }

    DeleteKeypair { nick } => {
      let kp = store.get_keypair(&KeypairName(nick.to_string())).unwrap();
      match kp {
        None => {
          eprintln!("No keypair with name `{}` found", nick);
          exit(-1);
        }
        Some(_) => {
          if prompt_default(format!("Are you sure you want to delete keypair `{}`?", nick),
                            false).unwrap()
          {
            // TODO: do this atomically?
            store.delete_keypair(&KeypairName(nick.to_string()))
                 .unwrap();
            store.delete_pubkey(&PubkeyName(nick.to_string())).unwrap();
            println!("Keypair `{}` deleted", nick);
          }
        }
      }
    }

    DeletePublicKey { nick } => {
      let pk = store.get_pubkey(&PubkeyName(nick.to_string())).unwrap();
      let kp = store.get_keypair(&KeypairName(nick.to_string())).unwrap();
      match (pk, kp) {
        (None, _) => {
          eprintln!("No public key with name `{}` found", nick);
          exit(-1);
        }
        (Some(_), Some(_)) => {
          eprintln!("`{}` is a keypair. Please use delete-keypair instead.",
                    nick);
          exit(-1);
        }
        (Some(_), None) => {
          if prompt_default(format!("Are you sure you want to delete public key `{}`?", nick),
                            false).unwrap()
          {
            store.delete_pubkey(&PubkeyName(nick.to_string())).unwrap();
            println!("Public key `{}` deleted", nick);
          }
        }
      }
    }

    ListAssetTypes {} => {
        for (nick,a) in store.get_asset_types().unwrap().into_iter() {
            println!("Asset `{}`",nick.0);
            display_asset_type(1,&a);
        }
    }

    ListAssetType { nick } => {
        let a = store.get_asset_type(&AssetTypeName(nick.clone())).unwrap();
        match a {
            None => {
                eprintln!("`{}` does not refer to any known asset type",
                            nick);
                exit(-1);
            }
            Some(a) => {
                display_asset_type(0,&a);
            }
        }
    }

    QueryAssetType { nick, code } => {
        let conf = store.get_config().unwrap();
        let code_b64 = code.clone();
        let _ = AssetTypeCode::new_from_base64(&code).unwrap();
        let query = format!("{}{}/{}",conf.ledger_server,LedgerAccessRoutes::AssetToken.route(),code_b64);
        let resp: AssetType;
        match reqwest::blocking::get(&query) {
            Err(e) => {
                eprintln!("Request `{}` failed: {}",query,e);
                exit(-1);
            }
            Ok(v) => match v.json::<AssetType>() {
                Err(e) => {
                    eprintln!("Failed to parse response: {}",e);
                    exit(-1);
                }
                Ok(v) => { resp = v; }
            }
        }
        let ret = AssetTypeEntry { asset: resp, issuer_nick: None };
        store.add_asset_type(&AssetTypeName(nick.clone()),ret).unwrap();
        println!("Asset type `{}` saved as `{}`", code_b64, nick);
    }

    PrepareTransaction { nick, exact } => {
        let seq_id = match store.get_config().unwrap().ledger_state {
            None => {
                eprintln!(concat!("I don't know what block ID the ledger is on!\n",
                                  "Please run query-ledger-state first."));
                exit(-1);
            }
            Some(s) => s.1
        };

        let mut nick = nick;
        if store.get_txn_builder(&TxnBuilderName(nick.clone())).unwrap().is_some() {
            if exact {
                eprintln!("Transaction builder with the name `{}` already exists.",nick);
                exit(-1);
            }

            for n in FreshNamer::new(nick.clone(),".".to_string()) {
                if store.get_txn_builder(&TxnBuilderName(n.clone())).unwrap().is_none() {
                    nick = n;
                    break;
                }
            }
        }

        println!("Preparing transaction `{}` for block id `{}`...",nick,seq_id);
        store.prepare_transaction(&TxnBuilderName(nick),seq_id).unwrap();
        println!("Done.");
    }

    ListTxnBuilders {} => {
      for (nick,builder) in store.get_txn_builders().unwrap() {
        println!("{}:",nick.0);
        display_txn_builder(1,&builder);
      }
    }

    ListTxnBuilder { nick } => {
      let builder = match store.get_txn_builder(&TxnBuilderName(nick.clone())).unwrap() {
        None => {
            eprintln!("No txn builder `{}` found.",nick);
            exit(-1);
        }
        Some(s) => s
      };

      display_txn_builder(0,&builder);
    }

    _ => {
      unimplemented!();
    }
  }
  store.update_config(|conf| {
         // println!("Opened {} times before", conf.open_count);
         conf.open_count += 1;
       })
       .unwrap();
}

fn main() -> Result<(), CliError> {
  let action = Actions::from_args();

  // use Actions::*;

  let mut home = dirs::home_dir().context(HomeDir)?;
  home.push(".findora");
  fs::create_dir_all(&home).with_context(|| UserFile { file: home.clone() })?;
  home.push("cli2_data.sqlite");
  let first_time = !std::path::Path::exists(&home);
  let mut db = KVStore::open(home.clone())?;
  if first_time {
    println!("No config found at {:?} -- triggering first-time setup",
             &home);
    db.update_config(|conf| {
        *conf = prompt_for_config(None).unwrap();
      })
      .unwrap();

    if let Actions::Setup { .. } = action {
      return Ok(());
    }
  }

  run_action(action, &mut db);
  Ok(())
}
