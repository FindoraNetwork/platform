use rusqlite::{params, Connection};
use serde::{de::DeserializeOwned, Serialize};
use snafu::{Backtrace, GenerateBacktrace, ResultExt, Snafu};
use std::collections::HashMap;
use std::hash::Hash;
use std::path::{Path, PathBuf};
use txn_builder::{BuildsTransactions, TransactionBuilder};

use crate::{CliDataStore, CliError, TxnBuilderEntry};

/// Possible errors encountered when dealing with a KVStore
#[derive(Debug, Snafu)]
pub enum KVError {
  #[snafu(display("Could not open KVStore at {}: {}", path.display(), source))]
  Open {
    source: rusqlite::Error,
    path: PathBuf,
    backtrace: Backtrace,
  },
  #[snafu(display("Failed preparing SQL statement \"{}\": {}", statement, source))]
  Prepare {
    source: rusqlite::Error,
    statement: String,
    backtrace: Backtrace,
  },
  #[snafu(display("An internal SQL error occurred: {}", source))]
  InternalSQL {
    source: rusqlite::Error,
    backtrace: Backtrace,
  },
  #[snafu(display("Failed to deserialize JSON for {}: \n{}", table, json))]
  Deserialization {
    source: serde_json::Error,
    table: String,
    json: String,
    backtrace: Backtrace,
  },
  #[snafu(display("Attempted to call KVStore::with on a key that doesn't exist: {}", key))]
  WithInvailidKey { backtrace: Backtrace, key: String },
}

type Result<T, E = KVError> = std::result::Result<T, E>;

/// Internal trait for mapping types to their tables
pub trait HasTable: Serialize + DeserializeOwned {
  const TABLE_NAME: &'static str;
  type Key: Serialize + DeserializeOwned + Hash + Eq;
}

/// Implements a view over a sqlite database as a KV store, where each type has its
/// own table, containing an ID column, and a single data column into which the
/// values of that type are serialized as JSON blobs
pub struct KVStore {
  db: Connection,
}

impl KVStore {
  /// Opens the store at the provided path, creating it if it does not exist
  pub fn open(db_path: impl AsRef<Path>) -> Result<KVStore> {
    let db_path = db_path.as_ref();
    let conn = Connection::open(db_path).with_context(|| Open { path: db_path.to_owned() })?;
    Ok(KVStore { db: conn })
  }
  /// Opens up an in-memory store. Primarily intended for testing
  pub fn open_in_memory() -> Result<KVStore> {
    let conn =
      Connection::open_in_memory().with_context(|| Open { path: "In Memory".to_owned() })?;
    Ok(KVStore { db: conn })
  }

  /// Checks to see if the table for a type exists
  fn table_exists<T: HasTable>(&self) -> Result<bool> {
    let table = T::TABLE_NAME.to_string();
    let name_query = format!("select name from sqlite_master WHERE type='table' AND name='{}';",
                             table);
    let mut stmt = self.db
                       .prepare(&name_query)
                       .with_context(|| Prepare { statement: name_query.to_string() })?;
    let mut rows = stmt.query(params![]).context(InternalSQL)?;
    // Attempt to get the first row, if it is none, our table does not exist
    Ok(rows.next().context(InternalSQL)?.is_some())
  }

  /// Creates a table for a type, if it does not exist
  pub fn create_table<T: HasTable>(&self) -> Result<()> {
    let create_query = format!("create table if not exists {} ( \
                                    key text NOT NULL, \
                                    value text NOT NULL \
                                    );",
                               T::TABLE_NAME);
    self.db
        .execute(&create_query, rusqlite::NO_PARAMS)
        .context(InternalSQL)?;
    Ok(())
  }

  /// Attempts to get a value from the key store
  pub fn get<T: HasTable>(&self, id: &T::Key) -> Result<Option<T>> {
    // Check if the table exists
    let table = T::TABLE_NAME.to_string();
    println!("{}", table);
    if !self.table_exists::<T>()? {
      return Ok(None);
    }
    // Stringify the key
    // TODO(Nathan M): Should we handle the case where serialization fails? That should
    // only really be possible in cases where the type being serialized contains a
    // Mutex that is poisoned or the like.
    let key = serde_json::to_string(id).expect("JSON serialization failed");
    // Look up our key
    let get_query = format!("select * from {} where key = (?);", table);
    let mut stmt = self.db
                       .prepare(&get_query)
                       .context(Prepare { statement: get_query })?;
    let rows = stmt.query_map(&[&key], |row| row.get::<_, String>(1))
                   .context(InternalSQL)?;
    // If there are multiple values for the key, use the last/most up to date one

    let mut values = rows.map(|x| x.context(InternalSQL))
                         .collect::<Result<Vec<_>>>()?;
    let data_json = if let Some(x) = values.pop() {
      x
    } else {
      return Ok(None);
    };

    let data = serde_json::from_str(&data_json).context(Deserialization { table,
                                                                          json: data_json })?;
    Ok(Some(data))
  }

  /// Attempts to set a key to a value, returning the previous value if there was one
  ///
  /// Will create the required table if it does not exist
  pub fn set<T: HasTable>(&self, key: &T::Key, value: T) -> Result<Option<T>> {
    // First, create the table if it does not exist
    self.create_table::<T>()?;
    // Look up the old value, if any
    let old_value = self.get::<T>(&key)?;
    // Prepare the new key and value
    let key_string = serde_json::to_string(&key).expect("JSON Serialization failed");
    let value_string = serde_json::to_string(&value).expect("JSON Serialization failed");
    // TODO(Nathan M): Use some conditional logic here to use an update when practical
    let set_query = format!("insert into {} (key, value) values (?, ?)", T::TABLE_NAME);
    let mut stmt = self.db
                       .prepare(&set_query)
                       .context(Prepare { statement: set_query })?;
    stmt.execute(&[&key_string, &value_string])
        .context(InternalSQL)?;
    Ok(old_value)
  }

  /// Returns all the Key/Value pairs for a type
  pub fn get_all<T: HasTable>(&self) -> Result<HashMap<T::Key, T>> {
    // Check if the table exists, and exit early with an empty map if it doesn't
    if !self.table_exists::<T>()? {
      return Ok(HashMap::new());
    }
    // Get ourself a fresh hashmap to put our K/Vs in
    let mut ret = HashMap::new();
    // Grab our rows from the db
    let get_all_query = format!("select * from {};", T::TABLE_NAME);
    let mut stmt = self.db
                       .prepare(&get_all_query)
                       .context(Prepare { statement: get_all_query })?;
    let rows = stmt.query_map(params![], |row| {
                     let x = row.get(0);
                     let y = row.get(1);
                     if let Ok(x_value) = x {
                       if let Ok(y_value) = y {
                         Ok((x_value, y_value))
                       } else {
                         Err(y.unwrap_err())
                       }
                     } else {
                       Err(y.unwrap_err())
                     }
                   })
                   .context(InternalSQL)?
                   .map(|x| x.context(InternalSQL))
                   .collect::<Result<Vec<(String, String)>>>()?;
    for (key, value) in rows {
      let key =
        serde_json::from_str(&key).with_context(|| Deserialization { table:
                                                                       T::TABLE_NAME.to_string(),
                                                                     json: key })?;
      let value =
        serde_json::from_str(&value).with_context(|| Deserialization { table:
                                                                         T::TABLE_NAME.to_string(),
                                                                       json: value })?;
      ret.insert(key, value);
    }
    Ok(ret)
  }

  /// Modifies a value "in place"
  pub fn with<T: HasTable, F: FnOnce(&mut T)>(&self, key: &T::Key, f: F) -> Result<()> {
    // Attempt to get the value
    let value: Option<T> = self.get(key)?;
    if let Some(mut value) = value {
      // Do the callers thing to the value
      f(&mut value);
      // Shove it back into the store
      self.set(key, value)?;
      Ok(())
    } else {
      let key_string = serde_json::to_string(&key).expect("JSON serialization failed");
      Err(KVError::WithInvailidKey { backtrace: Backtrace::generate(),
                                     key: key_string })
    }
  }

  /// Deletes all occurrences of a key
  pub fn delete<T: HasTable>(&self, key: &T::Key) -> Result<Option<T>> {
    let current = self.get(key)?;
    let delete_query = format!("delete from {} where key = (?)", T::TABLE_NAME);
    let mut stmt = self.db
                       .prepare(&delete_query)
                       .context(Prepare { statement: delete_query })?;

    let key_string = serde_json::to_string(key).expect("JSON Serialization failed");

    stmt.execute(params![&key_string]).context(InternalSQL)?;

    Ok(current)
  }
}

impl CliDataStore for KVStore {
  fn get_config(&self) -> Result<crate::CliConfig, CliError> {
    let config = self.get(&())?;
    if let Some(config) = config {
      Ok(config)
    } else {
      Ok(crate::CliConfig::default())
    }
  }
  fn update_config<F: FnOnce(&mut crate::CliConfig)>(&mut self, f: F) -> Result<(), CliError> {
    Ok(self.with(&(), f)?)
  }
  fn get_keypairs(&self)
                  -> Result<HashMap<crate::KeypairName, zei::xfr::sig::XfrKeyPair>, CliError> {
    Ok(self.get_all()?)
  }
  fn get_keypair(&self,
                 k: &crate::KeypairName)
                 -> Result<Option<zei::xfr::sig::XfrKeyPair>, CliError> {
    Ok(self.get(k)?)
  }
  fn delete_keypair(&mut self,
                    k: &crate::KeypairName)
                    -> Result<Option<zei::xfr::sig::XfrKeyPair>, CliError> {
    Ok(self.delete(k)?)
  }
  fn get_pubkeys(&self)
                 -> Result<HashMap<crate::PubkeyName, zei::xfr::sig::XfrPublicKey>, CliError> {
    Ok(self.get_all()?)
  }
  fn get_pubkey(&self,
                k: &crate::PubkeyName)
                -> Result<Option<zei::xfr::sig::XfrPublicKey>, CliError> {
    Ok(self.get(k)?)
  }
  fn delete_pubkey(&mut self,
                   k: &crate::PubkeyName)
                   -> Result<Option<zei::xfr::sig::XfrPublicKey>, CliError> {
    Ok(self.delete(k)?)
  }
  fn add_key_pair(&mut self,
                  k: &crate::KeypairName,
                  kp: zei::xfr::sig::XfrKeyPair)
                  -> Result<(), CliError> {
    Ok(self.set(k, kp).map(|_| ())?)
  }
  fn add_public_key(&mut self,
                    k: &crate::PubkeyName,
                    pk: zei::xfr::sig::XfrPublicKey)
                    -> Result<(), CliError> {
    Ok(self.set(k, pk).map(|_| ())?)
  }
  fn get_built_transactions(
    &self)
    -> Result<HashMap<crate::TxnName, (ledger::data_model::Transaction, crate::TxnMetadata)>,
              CliError> {
    Ok(self.get_all()?)
  }
  fn get_built_transaction(
    &self,
    k: &crate::TxnName)
    -> Result<Option<(ledger::data_model::Transaction, crate::TxnMetadata)>, CliError> {
    Ok(self.get(k)?)
  }
  fn build_transaction(
    &mut self,
    k_orig: &crate::TxnBuilderName,
    k_new: &crate::TxnName)
    -> Result<(ledger::data_model::Transaction, crate::TxnMetadata), CliError> {
    let builder = self.delete::<TxnBuilderEntry>(k_orig)?.ok_or_else(|| {
                                                            KVError::WithInvailidKey{
              backtrace: Backtrace::generate(),
              key: serde_json::to_string(k_orig).expect("JSON serialization failed")}
                                                          })?;
    let ret = (builder.builder.transaction().clone(), Default::default());
    self.set(k_new, ret.clone())?;
    Ok(ret)
  }
  fn update_txn_metadata<F: FnOnce(&mut crate::TxnMetadata)>(&mut self,
                                                             k: &crate::TxnName,
                                                             f: F)
                                                             -> Result<(), CliError> {
    Ok(self.with(k, |x: &mut (crate::Transaction, crate::TxnMetadata)| {
             f(&mut x.1)
           })?)
  }
  fn prepare_transaction(&mut self,
                         k: &crate::TxnBuilderName,
                         seq_id: u64)
                         -> Result<(), CliError> {
    Ok(self.set(k,
                TxnBuilderEntry { builder: TransactionBuilder::from_seq_id(seq_id) })
           .map(|_| ())?)
  }
  fn get_txn_builder(&self,
                     k: &crate::TxnBuilderName)
                     -> Result<Option<TxnBuilderEntry>, CliError> {
    Ok(self.get(k)?)
  }
  fn with_txn_builder<F: FnOnce(&mut TxnBuilderEntry)>(&mut self,
                                                       k: &crate::TxnBuilderName,
                                                       f: F)
                                                       -> Result<(), CliError> {
    Ok(self.with(k, f)?)
  }
  fn get_cached_txos(&self) -> Result<HashMap<crate::TxoName, crate::TxoCacheEntry>, CliError> {
    Ok(self.get_all()?)
  }
  fn get_cached_txo(&self, k: &crate::TxoName) -> Result<Option<crate::TxoCacheEntry>, CliError> {
    Ok(self.get(k)?)
  }
  fn delete_cached_txo(&mut self, k: &crate::TxoName) -> Result<(), CliError> {
    Ok(self.delete::<crate::TxoCacheEntry>(k).map(|_| ())?)
  }
  fn cache_txo(&mut self, k: &crate::TxoName, ent: crate::TxoCacheEntry) -> Result<(), CliError> {
    Ok(self.set(k, ent).map(|_| ())?)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde::Deserialize;
  // Define a few test types
  #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
  struct TypeA(String);
  #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
  struct TypeAKey(String);
  impl HasTable for TypeA {
    const TABLE_NAME: &'static str = "type_a";
    type Key = TypeAKey;
  }

  #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
  struct TypeB(String);
  #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
  struct TypeBKey(String);
  impl HasTable for TypeB {
    const TABLE_NAME: &'static str = "type_b";
    type Key = TypeBKey;
  }

  #[test]
  fn smoke_test() -> Result<()> {
    let kv = KVStore::open_in_memory()?;
    // Try to create the table for TypeA
    kv.create_table::<TypeA>()?;
    // Try to set a KV pair for TypeA
    let key1 = TypeAKey("test_keg".to_string());
    let value1 = TypeA("test_value".to_string());
    assert!(kv.set(&key1, value1.clone())?.is_none());
    // Verify the results
    assert!(kv.get(&key1)? == Some(value1.clone()));
    // Update the value
    let value2 = TypeA("Changed Value!".to_string());
    assert!(kv.set(&key1, value2.clone())? == Some(value1));
    // Verify results
    assert!(kv.get(&key1)? == Some(value2));

    // Attempt to get an invalid key
    let invalid_key = TypeAKey("invalid key!".to_string());
    assert!(kv.get::<TypeA>(&invalid_key)? == None);

    // Attempt the initial set/get test, but with TypeB
    // This tests implicit table creation
    let key1 = TypeBKey("test_key_b".to_string());
    let value1 = TypeB("test_value_b".to_string());
    assert!(kv.set(&key1, value1.clone())?.is_none());
    assert!(kv.get(&key1)? == Some(value1.clone()));
    Ok(())
  }

  #[test]
  fn get_all() -> Result<()> {
    // Generate some K/V Pairs
    let mut pairs = HashMap::new();
    for i in 0..10 {
      let k = TypeAKey(format!("key-{}", i));
      let v = TypeA(format!("value-{}", i));
      pairs.insert(k, v);
    }
    // Open our db
    let kv = KVStore::open_in_memory()?;
    for (k, v) in &pairs {
      // Insert an invalid value first, so we can test for any negative interaction with updates
      kv.set(k, TypeA("INVALID".to_string()))?;
      // Insert the correct value
      kv.set(k, v.clone())?;
    }
    // Make sure things match up
    assert!(kv.get_all::<TypeA>()? == pairs);
    Ok(())
  }
  #[test]
  fn with() -> Result<()> {
    let kv = KVStore::open_in_memory()?;
    let key1 = TypeAKey("key-1".to_string());
    let value1 = TypeA("value-1".to_string());
    kv.set(&key1, value1.clone())?;
    // Mutate value1 inside the store
    kv.with::<TypeA, _>(&key1, |x| x.0 = "value-2".to_string())?;
    assert!(kv.get(&key1)? == Some(TypeA("value-2".to_string())));
    Ok(())
  }

  #[test]
  fn delete() -> Result<()> {
    let kv = KVStore::open_in_memory()?;
    // Add the same key a bunch of times
    let key1 = TypeAKey("key-1".to_string());
    for i in 0..10 {
      kv.set(&key1, TypeA(format!("{}", i)))?;
    }
    // Delete the key
    kv.delete::<TypeA>(&key1)?;
    // Make sure its gone
    assert_eq!(kv.get::<TypeA>(&key1)?, None);

    Ok(())
  }
}
