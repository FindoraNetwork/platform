#![deny(warnings)]
use ledger::data_model::*;
use std::collections::HashMap;
use std::path::PathBuf;
use structopt::StructOpt;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
// use txn_builder::{BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder};
use ledger::error_location;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path; //PathBuf;
use submission_server::{TxnHandle, TxnStatus};
use txn_builder::{BuildsTransactions, TransactionBuilder};
use zei::xfr::structs::OpenAssetRecord;
// use std::rc::Rc;
use promptly::{prompt, prompt_default};
use utils::Serialized;

pub mod kv;

use kv::HasTable;

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Default)]
struct CliConfig {
  pub ledger_server: String,
  pub open_count: u64,
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

impl HasTable for TransactionBuilder {
  const TABLE_NAME: &'static str = "transaction_builders";
  type Key = TxnBuilderName;
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
pub struct TxoName(pub String);

impl HasTable for TxoCacheEntry {
  const TABLE_NAME: &'static str = "txo_cache";
  type Key = TxoName;
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum CliError {
  OtherError(String),
  AlreadyExists(String),
  KeyNotFound(String),
}

impl std::convert::From<std::io::Error> for CliError {
  fn from(error: std::io::Error) -> Self {
    CliError::OtherError(format!("{:?}", &error))
  }
}

impl std::convert::From<serde_json::error::Error> for CliError {
  fn from(error: serde_json::error::Error) -> Self {
    CliError::OtherError(format!("{:?}", &error))
  }
}

// impl std::convert::From<std::option::NoneError> for CliError {
//     fn from(error: std::option::NoneError) -> Self {
//         CliError::OtherError(format!("{:?}", &error))
//     }
// }
// impl<T> From<T: std::error::Error> for CliError {
//   fn from(error: T) -> Self {
//     CliError::OtherError(format!("{:?}", &error))
//   }
// }

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Default)]
struct TxnMetadata {
  handle: Option<TxnHandle>,
  status: Option<TxnStatus>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
struct TxoCacheEntry {
  sid: TxoSID,
  record: TxOutput,
  opened_record: Option<OpenAssetRecord>,
  unspent: bool,
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
                       -> Result<Transaction, CliError>;
  fn update_txn_metadata<F: FnOnce(&mut TxnMetadata)>(&mut self,
                                                      k: &TxnName,
                                                      f: F)
                                                      -> Result<(), CliError>;

  fn prepare_transaction(&mut self, k: &TxnBuilderName, seq_id: u64) -> Result<(), CliError>;
  fn get_txn_builder(&self, k: &TxnBuilderName) -> Result<Option<TransactionBuilder>, CliError>;
  fn with_txn_builder<F: FnOnce(&mut TransactionBuilder)>(&mut self,
                                                          k: &TxnBuilderName,
                                                          f: F)
                                                          -> Result<(), CliError>;

  fn get_cached_txos(&self) -> Result<HashMap<TxoName, TxoCacheEntry>, CliError>;
  fn get_cached_txo(&self, k: &TxoName) -> Result<Option<TxoCacheEntry>, CliError>;
  fn delete_cached_txo(&mut self, k: &TxoName) -> Result<(), CliError>;
  fn cache_txo(&mut self, k: &TxoName, ent: TxoCacheEntry) -> Result<(), CliError>;
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
struct SimpleCliData {
  pub config: CliConfig,
  pub keypairs: HashMap<KeypairName, Serialized<XfrKeyPair>>,
  pub pubkeys: HashMap<PubkeyName, XfrPublicKey>,
  pub built_txns: HashMap<TxnName, (Transaction, TxnMetadata)>,
  pub builders: HashMap<TxnBuilderName, TransactionBuilder>,
  pub txos: HashMap<TxoName, TxoCacheEntry>,
}

struct SimpleCliDataStore {
  pub filepath: PathBuf,
}

impl SimpleCliDataStore {
  fn new() -> Result<Self, CliError> {
    let mut home = dirs::home_dir().ok_or_else(|| CliError::OtherError(error_location!()))?;
    home.push(".findora");
    fs::create_dir_all(&home)?;
    home.push("cli2_data.json");
    let ret = Self { filepath: home };
    let _ = ret.read_data()?;
    Ok(ret)
  }

  fn read_data(&self) -> Result<SimpleCliData, CliError> {
    if !Path::exists(&self.filepath) {
      Ok(Default::default())
    } else {
      let f = fs::OpenOptions::new().read(true).open(&self.filepath)?;
      Ok(serde_json::from_reader(f)?)
    }
  }

  fn write_data(&self, dat: SimpleCliData) -> Result<(), CliError> {
    let file = fs::OpenOptions::new().create(true)
                                     .truncate(true)
                                     .write(true)
                                     .open(&self.filepath)?;
    Ok(serde_json::to_writer(file, &dat)?)
  }
}

impl CliDataStore for SimpleCliDataStore {
  fn get_config(&self) -> Result<CliConfig, CliError> {
    Ok(self.read_data()?.config)
  }
  fn update_config<F: FnOnce(&mut CliConfig)>(&mut self, f: F) -> Result<(), CliError> {
    let mut dat = self.read_data()?;
    f(&mut dat.config);
    self.write_data(dat)
  }

  fn get_keypairs(&self) -> Result<HashMap<KeypairName, XfrKeyPair>, CliError> {
    Ok(self.read_data()?
           .keypairs
           .into_iter()
           .map(|(k, v)| (k, v.deserialize()))
           .collect())
  }

  fn get_keypair(&self, k: &KeypairName) -> Result<Option<XfrKeyPair>, CliError> {
    Ok(self.read_data()?.keypairs.get(k).map(|x| x.deserialize()))
  }

  fn get_pubkeys(&self) -> Result<HashMap<PubkeyName, XfrPublicKey>, CliError> {
    Ok(self.read_data()?.pubkeys)
  }

  fn get_pubkey(&self, k: &PubkeyName) -> Result<Option<XfrPublicKey>, CliError> {
    Ok(self.read_data()?.pubkeys.get(k).cloned())
  }

  fn delete_keypair(&mut self, k: &KeypairName) -> Result<Option<XfrKeyPair>, CliError> {
    let mut dat = self.read_data()?;
    let ret = dat.keypairs.remove(k).map(|x| x.deserialize());
    self.write_data(dat)?;
    Ok(ret)
  }

  fn delete_pubkey(&mut self, k: &PubkeyName) -> Result<Option<XfrPublicKey>, CliError> {
    let mut dat = self.read_data()?;
    let ret = dat.pubkeys.remove(k);
    self.write_data(dat)?;
    Ok(ret)
  }

  fn add_key_pair(&mut self, k: &KeypairName, kp: XfrKeyPair) -> Result<(), CliError> {
    use CliError::*;
    let mut dat = self.read_data()?;
    match dat.keypairs.entry(k.clone()) {
      e @ std::collections::hash_map::Entry::Vacant(_) => {
        e.or_insert_with(|| Serialized::new(&kp));
      }
      _ => {
        return Err(AlreadyExists(error_location!()));
      }
    }
    self.write_data(dat)
  }

  fn add_public_key(&mut self, k: &PubkeyName, pk: XfrPublicKey) -> Result<(), CliError> {
    use CliError::*;
    let mut dat = self.read_data()?;
    match dat.pubkeys.entry(k.clone()) {
      e @ std::collections::hash_map::Entry::Vacant(_) => {
        e.or_insert(pk);
      }
      _ => {
        return Err(AlreadyExists(error_location!()));
      }
    }
    self.write_data(dat)
  }

  fn get_built_transactions(&self)
                            -> Result<HashMap<TxnName, (Transaction, TxnMetadata)>, CliError> {
    Ok(self.read_data()?.built_txns)
  }

  fn get_built_transaction(&self,
                           k: &TxnName)
                           -> Result<Option<(Transaction, TxnMetadata)>, CliError> {
    Ok(self.read_data()?.built_txns.get(k).cloned())
  }

  fn build_transaction(&mut self,
                       k_orig: &TxnBuilderName,
                       k_new: &TxnName)
                       -> Result<Transaction, CliError> {
    use CliError::*;
    let mut dat = self.read_data()?;
    let builder = dat.builders
                     .remove(k_orig)
                     .ok_or_else(|| KeyNotFound(error_location!()))?;
    let ret = builder.transaction().clone();
    dat.built_txns
       .insert(k_new.clone(), (ret.clone(), Default::default()))
       .map(|x| Err(AlreadyExists(format!("[{}] {:?}", error_location!(), x))))
       .unwrap_or(Ok(()))?;
    self.write_data(dat)?;
    Ok(ret)
  }

  fn update_txn_metadata<F: FnOnce(&mut TxnMetadata)>(&mut self,
                                                      k: &TxnName,
                                                      f: F)
                                                      -> Result<(), CliError> {
    use CliError::*;
    let mut dat = self.read_data()?;
    f(&mut dat.built_txns
              .get_mut(k)
              .ok_or_else(|| KeyNotFound(error_location!()))?
              .1);
    self.write_data(dat)
  }

  fn prepare_transaction(&mut self, k: &TxnBuilderName, seq_id: u64) -> Result<(), CliError> {
    use CliError::*;
    let mut dat = self.read_data()?;
    dat.builders
       .insert(k.clone(), TransactionBuilder::from_seq_id(seq_id))
       .map(|x| Err(AlreadyExists(format!("[{}] {:?}", error_location!(), x))))
       .unwrap_or(Ok(()))?;
    self.write_data(dat)
  }

  fn get_txn_builder(&self, k: &TxnBuilderName) -> Result<Option<TransactionBuilder>, CliError> {
    Ok(self.read_data()?.builders.get(k).cloned())
  }

  fn with_txn_builder<F: FnOnce(&mut TransactionBuilder)>(&mut self,
                                                          k: &TxnBuilderName,
                                                          f: F)
                                                          -> Result<(), CliError> {
    use CliError::*;
    let mut dat = self.read_data()?;
    f(dat.builders
         .get_mut(k)
         .ok_or_else(|| KeyNotFound(error_location!()))?);
    self.write_data(dat)
  }

  fn get_cached_txos(&self) -> Result<HashMap<TxoName, TxoCacheEntry>, CliError> {
    Ok(self.read_data()?.txos)
  }
  fn get_cached_txo(&self, k: &TxoName) -> Result<Option<TxoCacheEntry>, CliError> {
    Ok(self.read_data()?.txos.get(k).cloned())
  }

  fn delete_cached_txo(&mut self, k: &TxoName) -> Result<(), CliError> {
    use CliError::*;
    let mut dat = self.read_data()?;
    let _ = dat.txos
               .remove(k)
               .ok_or_else(|| KeyNotFound(error_location!()))?;
    self.write_data(dat)
  }

  fn cache_txo(&mut self, k: &TxoName, ent: TxoCacheEntry) -> Result<(), CliError> {
    use CliError::*;
    let mut dat = self.read_data()?;
    dat.txos
       .insert(k.clone(), ent)
       .map(|x| Err(AlreadyExists(format!("[{}] {:?}", error_location!(), x))))
       .unwrap_or(Ok(()))?;
    self.write_data(dat)
  }
}

#[derive(StructOpt, Debug)]
#[structopt(about = "Build and manage transactions and assets on a findora ledger",
            rename_all = "kebab-case")]
enum Actions {
  /// Run integrity checks of the local database
  CheckDb {},

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

  PrepareTransaction {
    /// Optional transaction name
    nick: Option<String>,
  },
  DefineAsset {
    #[structopt(short, long)]
    /// Which txn?
    txn: Option<String>,
    /// Issuer key
    key_nick: String,
    /// Name for the asset type
    asset_name: String,
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
  ListTransaction {
    /// txn id
    txn: Option<String>,
  },
  ListTransactions {
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

fn run_action<S: CliDataStore>(action: Actions, store: &mut S) {
  // println!("{:?}", action);

  store.update_config(|conf| {
         // println!("Opened {} times before", conf.open_count);
         conf.open_count += 1;
       })
       .unwrap();

  use Actions::*;
  match action {
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
          std::process::exit(-1);
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
          std::process::exit(-1);
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
          std::process::exit(-1);
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
          std::process::exit(-1);
        }
        (Some(_), Some(_)) => {
          eprintln!("`{}` is a keypair. Please use delete-keypair instead.",
                    nick);
          std::process::exit(-1);
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
    _ => {
      unimplemented!();
    }
  }
}

fn main() {
  let action = Actions::from_args();

  // use Actions::*;

  run_action(action, &mut SimpleCliDataStore::new().unwrap());
}
