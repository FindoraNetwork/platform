use itertools::Itertools;
use ruc::{err::*, *};
use rusqlite::{params, Connection};
use serde::{de::DeserializeOwned, Serialize};
use std::collections::BTreeMap;
use std::fmt;
use std::hash::Hash;
use std::path::{Path, PathBuf};
use std::result::Result as StdResult;
use std::str::FromStr;
use txn_builder::{BuildsTransactions, TransactionBuilder};

use crate::{
    AssetTypeEntry, AssetTypeName, CliDataStore, CliError, PubkeyName, TxnBuilderEntry,
};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};

pub mod crypto;
pub use crypto::MixedPair;

pub const NICK_FEE: &str = "fee";

/// Possible errors encountered when dealing with a KVStore
#[derive(Debug)]
pub enum KVError {
    Open(PathBuf),
    Prepare(String),
    InternalSQL,
    Deserialization(String, String),
    WithInvalidKey(String),
    ClosureError,
    KeyDecryptionError(String),
    PubKeyDeserialization(String),
}

impl fmt::Display for KVError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KVError::Open(n) => {
                write!(f, "{}: {}", stringify!(KVError::Open), n.to_string_lossy())
            }
            KVError::Prepare(n) => write!(f, "{}: {}", stringify!(KVError::Prepare), n),
            KVError::InternalSQL => write!(f, "{}", stringify!(KVError::InternalSQL)),
            KVError::Deserialization(n, m) => {
                write!(f, "{}: {}, {}", stringify!(KVError::Deserialization), n, m)
            }
            KVError::WithInvalidKey(n) => {
                write!(f, "{}: {}", stringify!(KVError::WithInvalidKey), n)
            }
            KVError::ClosureError => write!(f, "{}", stringify!(KVError::ClosureError)),
            KVError::KeyDecryptionError(n) => {
                write!(f, "{}: {}", stringify!(KVError::KeyDecryptionError), n)
            }
            KVError::PubKeyDeserialization(n) => {
                write!(f, "{}: {}", stringify!(KVError::PubKeyDeserialization), n)
            }
        }
    }
}

/// Internal trait for mapping types to their tables
pub trait HasTable: Serialize + DeserializeOwned {
    const TABLE_NAME: &'static str;
    type Key: Serialize + DeserializeOwned + Hash + Ord + PartialOrd + Eq;
}

/// Internal trait for mapping types to encrypted tables
pub trait HasEncryptedTable: Serialize + DeserializeOwned {
    const TABLE_NAME: &'static str;
    type Key: Serialize + DeserializeOwned + Hash + Ord + PartialOrd + Eq;
    /// The cleartext component of the internal `MixedPair`
    type Clear: Serialize + DeserializeOwned + 'static;
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
        let conn = Connection::open(db_path).c(d!(KVError::Open(db_path.to_owned())))?;
        Ok(KVStore { db: conn })
    }
    /// Opens up an in-memory store. Primarily intended for testing
    pub fn open_in_memory() -> Result<KVStore> {
        let conn = Connection::open_in_memory()
            .c(d!(KVError::Open(PathBuf::from_str("In Memory").unwrap())))?;
        Ok(KVStore { db: conn })
    }

    /// Checks to see if the table for a type exists
    fn table_exists<T: HasTable>(&self) -> Result<bool> {
        let table = T::TABLE_NAME.to_string();
        let name_query = format!(
            "select name from sqlite_master WHERE type='table' AND name='{}';",
            table
        );
        let mut stmt = self
            .db
            .prepare(&name_query)
            .c(d!(KVError::Prepare(name_query.to_string())))?;
        let mut rows = stmt.query(params![]).c(d!(KVError::InternalSQL))?;
        // Attempt to get the first row, if it is none, our table does not exist
        Ok(rows.next().c(d!(KVError::InternalSQL))?.is_some())
    }

    /// Checks to see if the table for an encrypted type exists
    fn encrypted_table_exists<T: HasEncryptedTable>(&self) -> Result<bool> {
        let table = T::TABLE_NAME.to_string();
        let name_query = format!(
            "select name from sqlite_master WHERE type='table' AND name='{}';",
            table
        );
        let mut stmt = self
            .db
            .prepare(&name_query)
            .c(d!(KVError::Prepare(name_query.to_string())))?;
        let mut rows = stmt.query(params![]).c(d!(KVError::InternalSQL))?;
        // Attempt to get the first row, if it is none, our table does not exist
        Ok(rows.next().c(d!(KVError::InternalSQL))?.is_some())
    }

    /// Creates a table for a type, if it does not exist
    pub fn create_table<T: HasTable>(&self) -> Result<()> {
        let create_query = format!(
            "create table if not exists {} ( \
                                    key text NOT NULL, \
                                    value text NOT NULL \
                                    );",
            T::TABLE_NAME
        );
        self.db
            .execute(&create_query, rusqlite::NO_PARAMS)
            .c(d!(KVError::InternalSQL))?;
        Ok(())
    }

    /// Creates a table for an encrypted type, if it does not exist
    pub fn create_encrypted_table<T: HasEncryptedTable>(&self) -> Result<()> {
        let create_query = format!(
            "create table if not exists {} ( \
                                    key text NOT NULL, \
                                    value text NOT NULL \
                                    );",
            T::TABLE_NAME
        );
        self.db
            .execute(&create_query, rusqlite::NO_PARAMS)
            .c(d!(KVError::InternalSQL))?;
        Ok(())
    }

    /// Attempts to get a value from the key store
    pub fn get<T: HasTable>(&self, id: &T::Key) -> Result<Option<T>> {
        // Check if the table exists
        let table = T::TABLE_NAME.to_string();
        if !self.table_exists::<T>().c(d!())? {
            return Ok(None);
        }
        // Stringify the key
        let key = serde_json::to_string(id).expect("JSON serialization failed");
        // Look up our key
        let get_query = format!("select * from {} where key = (?);", table);
        let mut stmt = self
            .db
            .prepare(&get_query)
            .c(d!(KVError::Prepare(get_query)))?;
        let rows = stmt
            .query_map(&[&key], |row| row.get::<_, String>(1))
            .c(d!(KVError::InternalSQL))?;
        // If there are multiple values for the key, use the last/most up to date one

        let mut values = rows
            .collect::<StdResult<Vec<_>, rusqlite::Error>>()
            .c(d!(KVError::InternalSQL))?;
        let data_json = if let Some(x) = values.pop() {
            x
        } else {
            return Ok(None);
        };

        let data = serde_json::from_str(&data_json)
            .c(d!(KVError::Deserialization(table, data_json.to_string())))?;
        Ok(Some(data))
    }

    /// Attempts to get an encrypted value from the key store
    pub fn get_encrypted_raw<T: HasEncryptedTable>(
        &self,
        id: &T::Key,
    ) -> Result<Option<MixedPair<T::Clear, T>>> {
        // Check if the table exists
        let table = T::TABLE_NAME.to_string();
        if !self.encrypted_table_exists::<T>().c(d!())? {
            return Ok(None);
        }
        // Stringify the key
        let key = serde_json::to_string(id).expect("JSON serialization failed");
        // Look up our key
        let get_query = format!("select * from {} where key = (?);", table);
        let mut stmt = self
            .db
            .prepare(&get_query)
            .c(d!(KVError::Prepare(get_query)))?;
        let rows = stmt
            .query_map(&[&key], |row| row.get::<_, String>(1))
            .c(d!(KVError::InternalSQL))?;
        // If there are multiple values for the key, use the last/most up to date one

        let mut values = rows
            .collect::<StdResult<Vec<_>, rusqlite::Error>>()
            .c(d!(KVError::InternalSQL))?;
        let data_json = if let Some(x) = values.pop() {
            x
        } else {
            return Ok(None);
        };

        let data = serde_json::from_str(&data_json)
            .c(d!(KVError::Deserialization(table, data_json.to_string())))?;
        Ok(Some(data))
    }
    /// Attempts to set a key to a value, returning the previous value if there was one
    ///
    /// Will create the required table if it does not exist
    pub fn set<T: HasTable>(&self, key: &T::Key, value: T) -> Result<Option<T>> {
        // First, create the table if it does not exist
        self.create_table::<T>().c(d!())?;
        // Look up the old value, if any
        let old_value = self.get::<T>(&key).c(d!())?;
        // Prepare the new key and value
        let key_string = serde_json::to_string(&key).expect("JSON Serialization failed");
        let value_string =
            serde_json::to_string(&value).expect("JSON Serialization failed");
        // If the value already exists, go ahead and update instead of insert.
        if old_value.is_some() {
            // Go ahead and apply the update to all the rows with the specified key.
            // This will ensure that any duplicates rows have the same, correct value
            let update_query =
                format!("update {} set value = (?) where key = (?);", T::TABLE_NAME);
            let mut stmt = self
                .db
                .prepare(&update_query)
                .c(d!(KVError::Prepare(update_query)))?;
            stmt.execute(params![&value_string, &key_string])
                .c(d!(KVError::InternalSQL))?;
        } else {
            let set_query =
                format!("insert into {} (key, value) values (?, ?)", T::TABLE_NAME);
            let mut stmt = self
                .db
                .prepare(&set_query)
                .c(d!(KVError::Prepare(set_query)))?;
            stmt.execute(&[&key_string, &value_string])
                .c(d!(KVError::InternalSQL))?;
        }
        Ok(old_value)
    }

    /// Attempts to set a key to a value in an encrypted table, returning the previous
    /// value if there was one
    ///
    /// Will create the required table if it does not exist
    pub fn set_encrypted_raw<T: HasEncryptedTable>(
        &self,
        key: &T::Key,
        value: MixedPair<T::Clear, T>,
    ) -> Result<Option<MixedPair<T::Clear, T>>> {
        // First, create the table if it does not exist
        self.create_encrypted_table::<T>().c(d!())?;
        // Look up the old value, if any
        let old_value = self.get_encrypted_raw::<T>(&key).c(d!())?;
        // Prepare the new key and value
        let key_string = serde_json::to_string(&key).expect("JSON Serialization failed");
        let value_string =
            serde_json::to_string(&value).expect("JSON Serialization failed");
        // If the value already exists, go ahead and update instead of insert.
        if old_value.is_some() {
            // Go ahead and apply the update to all the rows with the specified key.
            // This will ensure that any duplicates rows have the same, correct value
            let update_query =
                format!("update {} set value = (?) where key = (?);", T::TABLE_NAME);
            let mut stmt = self
                .db
                .prepare(&update_query)
                .c(d!(KVError::Prepare(update_query)))?;
            stmt.execute(params![&value_string, &key_string])
                .c(d!(KVError::InternalSQL))?;
        } else {
            let set_query =
                format!("insert into {} (key, value) values (?, ?)", T::TABLE_NAME);
            let mut stmt = self
                .db
                .prepare(&set_query)
                .c(d!(KVError::Prepare(set_query)))?;
            stmt.execute(&[&key_string, &value_string])
                .c(d!(KVError::InternalSQL))?;
        }
        Ok(old_value)
    }

    /// Returns all the Key/Value pairs for a type
    pub fn get_all<T: HasTable>(&self) -> Result<BTreeMap<T::Key, T>> {
        // Check if the table exists, and exit early with an empty map if it doesn't
        if !self.table_exists::<T>().c(d!())? {
            return Ok(BTreeMap::new());
        }
        // Get ourself a fresh hashmap to put our K/Vs in
        let mut ret = BTreeMap::new();
        // Grab our rows from the db
        let get_all_query = format!("select * from {};", T::TABLE_NAME);
        let mut stmt = self
            .db
            .prepare(&get_all_query)
            .c(d!(KVError::Prepare(get_all_query)))?;
        let rows = stmt
            .query_map(params![], |row| {
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
            .c(d!(KVError::InternalSQL))?
            .collect::<StdResult<Vec<(String, String)>, rusqlite::Error>>()
            .c(d!(KVError::InternalSQL))?;
        for (key, value) in rows {
            let key = serde_json::from_str(&key)
                .c(d!(KVError::Deserialization(T::TABLE_NAME.to_string(), key)))?;
            let value = serde_json::from_str(&value).c(d!(KVError::Deserialization(
                T::TABLE_NAME.to_string(),
                value
            )))?;
            ret.insert(key, value);
        }
        Ok(ret)
    }

    /// Returns all the Key/Value pairs for an encrypted type
    pub fn get_all_encrypted_raw<T: HasEncryptedTable>(
        &self,
    ) -> Result<BTreeMap<T::Key, MixedPair<T::Clear, T>>> {
        // Check if the table exists, and exit early with an empty map if it doesn't
        if !self.encrypted_table_exists::<T>().c(d!())? {
            return Ok(BTreeMap::new());
        }
        // Get ourself a fresh hashmap to put our K/Vs in
        let mut ret = BTreeMap::new();
        // Grab our rows from the db
        let get_all_query = format!("select * from {};", T::TABLE_NAME);
        let mut stmt = self
            .db
            .prepare(&get_all_query)
            .c(d!(KVError::Prepare(get_all_query)))?;
        let rows = stmt
            .query_map(params![], |row| {
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
            .c(d!(KVError::InternalSQL))?
            .collect::<StdResult<Vec<(String, String)>, rusqlite::Error>>()
            .c(d!(KVError::InternalSQL))?;
        for (key, value) in rows {
            let key = serde_json::from_str(&key)
                .c(d!(KVError::Deserialization(T::TABLE_NAME.to_string(), key)))?;
            let value = serde_json::from_str(&value).c(d!(KVError::Deserialization(
                T::TABLE_NAME.to_string(),
                value
            )))?;
            ret.insert(key, value);
        }
        Ok(ret)
    }

    // TODO: unify these
    pub fn with_opt<T: HasTable, F: FnOnce(Option<&mut T>) -> Result<()>>(
        &self,
        key: &T::Key,
        f: F,
    ) -> Result<()> {
        // Attempt to get the value
        let mut value: Option<T> = self.get(key).c(d!())?;
        // Do the callers thing to the value
        f(value.as_mut())
            .and_then(|_| {
                if let Some(value) = value {
                    // Shove it back into the store
                    self.set(key, value).c(d!())?;
                }
                Ok(())
            })
            .c(d!(KVError::ClosureError))
    }

    pub fn with<T: HasTable, F: FnOnce(&mut T) -> Result<()>>(
        &self,
        key: &T::Key,
        f: F,
    ) -> Result<()> {
        // Attempt to get the value
        let value: Option<T> = self.get(key).c(d!())?;
        if let Some(mut value) = value {
            // Do the callers thing to the value
            f(&mut value)
                .and_then(|_| {
                    // Shove it back into the store
                    self.set(key, value).c(d!()).map(|_| ())
                })
                .c(d!(KVError::ClosureError))
        } else {
            let key_string =
                serde_json::to_string(&key).expect("JSON serialization failed");
            Err(eg!(KVError::WithInvalidKey(key_string)))
        }
    }

    /// Deletes all occurrences of a key
    pub fn delete<T: HasTable>(&self, key: &T::Key) -> Result<Option<T>> {
        let current = self.get(key).c(d!())?;
        let delete_query = format!("delete from {} where key = (?)", T::TABLE_NAME);
        let mut stmt = self
            .db
            .prepare(&delete_query)
            .c(d!(KVError::Prepare(delete_query)))?;

        let key_string = serde_json::to_string(key).expect("JSON Serialization failed");

        stmt.execute(params![&key_string])
            .c(d!(KVError::InternalSQL))?;

        Ok(current)
    }

    /// Deletes all occurrences of a key in an encrypted table
    pub fn delete_encrypted<T: HasEncryptedTable>(
        &self,
        key: &T::Key,
    ) -> Result<Option<MixedPair<T::Clear, T>>> {
        let current = self.get_encrypted_raw(key).c(d!())?;
        let delete_query = format!("delete from {} where key = (?)", T::TABLE_NAME);
        let mut stmt = self
            .db
            .prepare(&delete_query)
            .c(d!(KVError::Prepare(delete_query)))?;

        let key_string = serde_json::to_string(key).expect("JSON Serialization failed");

        stmt.execute(params![&key_string])
            .c(d!(KVError::InternalSQL))?;

        Ok(current)
    }
    /// Performs general house keeping operations on the database, inducing:
    ///
    /// 1. Find and remove duplicate entries
    /// 2. Vaccum the database
    pub fn run_housekeeping(&self) -> Result<()> {
        // Get the list of tables
        let name_query = "select name from sqlite_master WHERE type='table';";
        let mut stmt = self
            .db
            .prepare(name_query)
            .c(d!(KVError::Prepare(name_query.to_string())))?;
        let mut rows = stmt.query(params![]).c(d!(KVError::InternalSQL))?;
        while let Some(table) = rows.next().c(d!(KVError::InternalSQL))? {
            let name: String = table.get_unwrap(0);
            let query = format!(
                "delete from {0} \
                             where rowid not in \
                             (select max(rowid) \
                              from {0} \
                              group by key)",
                name
            );
            self.db
                .execute(&query, params![])
                .c(d!(KVError::InternalSQL))?;
        }
        self.db
            .execute("VACUUM;", params![])
            .c(d!(KVError::InternalSQL))?;
        Ok(())
    }
}

impl CliDataStore for KVStore {
    fn get_config(&self) -> Result<crate::CliConfig> {
        let config = self.get(&String::from("config")).c(d!())?;
        if let Some(config) = config {
            Ok(config)
        } else {
            self.set(&String::from("config"), crate::CliConfig::default())
                .c(d!())?;
            self.get(&String::from("config"))
                .c(d!())?
                .c(d!(KVError::WithInvalidKey("config".to_string())))
        }
    }
    fn update_config<F: FnOnce(&mut crate::CliConfig) -> Result<()>>(
        &mut self,
        f: F,
    ) -> Result<()> {
        let mut current = self.get_config().c(d!())?;
        f(&mut current).c(d!())?;
        self.set(&String::from("config"), current).c(d!())?;
        Ok(())
    }
    fn get_keypairs(&self) -> Result<Vec<crate::KeypairName>> {
        let keys = self
            .get_all_encrypted_raw::<XfrKeyPair>()
            .c(d!())?
            .into_iter()
            .map(|(x, _)| x)
            .collect();
        Ok(keys)
    }
    fn get_keypair_pubkey(
        &self,
        k: &crate::KeypairName,
    ) -> Result<Option<XfrPublicKey>> {
        let mixed_pair = self.get_encrypted_raw::<XfrKeyPair>(k).c(d!())?;
        if let Some(mixed_pair) = mixed_pair {
            let public = mixed_pair
                .clear_no_verify()
                .c(d!(KVError::PubKeyDeserialization(k.0.clone())))?;
            Ok(Some(public))
        } else {
            Ok(None)
        }
    }
    fn with_keypair<F: FnOnce(Option<&XfrKeyPair>) -> Result<()>>(
        &mut self,
        k: &crate::KeypairName,
        f: F,
    ) -> Result<()> {
        let keypair = crate::helpers::prompt_with_retries(3, Some(&k.0), |password| {
            self.get_encrypted_raw::<XfrKeyPair>(k)
                .c(d!(KVError::WithInvalidKey(k.0.clone())))?
                .c(d!())?
                .encrypted(password.as_bytes())
                .c(d!(KVError::WithInvalidKey(k.0.clone())))
        })
        .c(d!(CliError::Password))?;

        f(Some(&keypair)).c(d!(KVError::ClosureError))
    }
    fn get_encrypted_keypair(
        &self,
        k: &crate::KeypairName,
    ) -> Result<Option<MixedPair<XfrPublicKey, XfrKeyPair>>> {
        let mixed_pair = self.get_encrypted_raw::<XfrKeyPair>(k).c(d!())?;
        Ok(mixed_pair)
    }
    fn delete_keypair(&mut self, k: &crate::KeypairName) -> Result<()> {
        self.delete_encrypted::<XfrKeyPair>(k).map(|_| ()).c(d!())?;
        Ok(())
    }
    fn get_pubkeys(&self) -> Result<BTreeMap<crate::PubkeyName, XfrPublicKey>> {
        self.get_all().c(d!())
    }

    fn get_local_pubkeys(&self) -> Result<BTreeMap<crate::PubkeyName, XfrPublicKey>> {
        let key_pair_names = self.get_keypairs().c(d!())?;
        let public_keys = key_pair_names
            .into_iter()
            .map(|kp| (kp.clone().0, self.get_keypair_pubkey(&kp).unwrap().unwrap()))
            .collect_vec();
        let mut res: BTreeMap<crate::PubkeyName, XfrPublicKey> = BTreeMap::new();
        for (kp_name, pk) in public_keys {
            let pk_name = PubkeyName(kp_name);
            res.insert(pk_name, pk);
        }

        Ok(res)
    }

    fn exists_keypair(&self, nick: &str) -> Result<bool> {
        let key_pairs = self.get_keypairs().c(d!())?;
        let res = key_pairs.iter().any(|i| i.0 == *nick);
        Ok(res)
    }

    fn get_pubkey(&self, k: &crate::PubkeyName) -> Result<Option<XfrPublicKey>> {
        if NICK_FEE == k.0 {
            Ok(Some(*ledger::data_model::BLACK_HOLE_PUBKEY))
        } else {
            self.get(k).c(d!())
        }
    }
    fn delete_pubkey(&mut self, k: &crate::PubkeyName) -> Result<Option<XfrPublicKey>> {
        self.delete(k).c(d!())
    }
    fn add_key_pair(&mut self, k: &crate::KeypairName, kp: XfrKeyPair) -> Result<()> {
        let pubkey = kp.get_pk();
        let password = crate::helpers::prompt_confirming_with_retries(3, Some(&k.0))
            .c(d!(CliError::Password))?;
        let mixed_pair = MixedPair::pack(pubkey, &kp, password.as_bytes());

        self.set_encrypted_raw(k, mixed_pair).map(|_| ()).c(d!())
    }
    fn add_encrypted_keypair(
        &mut self,
        k: &crate::KeypairName,
        kp: MixedPair<XfrPublicKey, XfrKeyPair>,
    ) -> Result<()> {
        self.set_encrypted_raw(k, kp).map(|_| ()).c(d!())
    }
    fn add_public_key(&mut self, k: &crate::PubkeyName, pk: XfrPublicKey) -> Result<()> {
        self.set(k, pk).map(|_| ()).c(d!())
    }
    fn get_built_transactions(
        &self,
    ) -> Result<
        BTreeMap<crate::TxnName, (ledger::data_model::Transaction, crate::TxnMetadata)>,
    > {
        self.get_all().c(d!())
    }
    fn get_built_transaction(
        &self,
        k: &crate::TxnName,
    ) -> Result<Option<(ledger::data_model::Transaction, crate::TxnMetadata)>> {
        self.get(k).c(d!())
    }

    fn build_transaction(
        &mut self,
        k_orig: &crate::TxnBuilderName,
        k_new: &crate::TxnName,
        metadata: crate::TxnMetadata,
    ) -> Result<(ledger::data_model::Transaction, crate::TxnMetadata)> {
        let builder = self
            .delete::<TxnBuilderEntry>(k_orig)
            .c(d!(KVError::WithInvalidKey(
                serde_json::to_string(k_orig).expect("JSON serialization failed")
            )))?
            .c(d!())?;
        let ret = (builder.builder.transaction().clone(), metadata);
        self.set(k_new, ret.clone()).c(d!())?;
        Ok(ret)
    }
    fn update_txn_metadata<F: FnOnce(&mut crate::TxnMetadata) -> Result<()>>(
        &mut self,
        k: &crate::TxnName,
        f: F,
    ) -> Result<()> {
        self.with(k, |x: &mut (crate::Transaction, crate::TxnMetadata)| {
            f(&mut x.1)
        })
        .c(d!())
    }
    fn prepare_transaction(
        &mut self,
        k: &crate::TxnBuilderName,
        seq_id: u64,
    ) -> Result<()> {
        self.set(
            k,
            TxnBuilderEntry {
                builder: TransactionBuilder::from_seq_id(seq_id),
                new_asset_types: Default::default(),
                operations: Default::default(),
                signers: Default::default(),
                new_txos: Default::default(),
                spent_txos: Default::default(),
            },
        )
        .map(|_| ())
        .c(d!())
    }
    fn get_txn_builders(
        &self,
    ) -> Result<BTreeMap<crate::TxnBuilderName, TxnBuilderEntry>> {
        self.get_all().c(d!())
    }
    fn get_txn_builder(
        &self,
        k: &crate::TxnBuilderName,
    ) -> Result<Option<TxnBuilderEntry>> {
        self.get(k).c(d!())
    }
    fn with_txn_builder<F: FnOnce(&mut TxnBuilderEntry) -> Result<()>>(
        &mut self,
        k: &crate::TxnBuilderName,
        f: F,
    ) -> Result<()> {
        self.with(k, f).c(d!())
    }
    fn get_cached_txos(&self) -> Result<BTreeMap<crate::TxoName, crate::TxoCacheEntry>> {
        self.get_all().c(d!())
    }
    fn get_cached_txo(
        &self,
        k: &crate::TxoName,
    ) -> Result<Option<crate::TxoCacheEntry>> {
        self.get(k).c(d!())
    }
    fn delete_cached_txo(&mut self, k: &crate::TxoName) -> Result<()> {
        self.delete::<crate::TxoCacheEntry>(k).map(|_| ()).c(d!())
    }
    fn cache_txo(
        &mut self,
        k: &crate::TxoName,
        ent: crate::TxoCacheEntry,
    ) -> Result<()> {
        self.set(k, ent).map(|_| ()).c(d!())
    }

    fn get_asset_types(&self) -> Result<BTreeMap<AssetTypeName, AssetTypeEntry>> {
        self.get_all().c(d!())
    }
    fn get_asset_type(&self, k: &AssetTypeName) -> Result<Option<AssetTypeEntry>> {
        self.get(k).c(d!())
    }
    fn update_asset_type<F: FnOnce(&mut AssetTypeEntry) -> Result<()>>(
        &mut self,
        k: &AssetTypeName,
        f: F,
    ) -> Result<()> {
        self.with(k, f).c(d!())
    }
    fn delete_asset_type(&self, k: &AssetTypeName) -> Result<Option<AssetTypeEntry>> {
        self.delete::<crate::AssetTypeEntry>(k).c(d!())
    }
    fn add_asset_type(&self, k: &AssetTypeName, ent: AssetTypeEntry) -> Result<()> {
        self.set(k, ent).map(|_| ()).c(d!())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;
    // Define a few test types
    #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
    struct TypeA(String);
    #[derive(
        Ord,
        PartialOrd,
        Clone,
        Debug,
        Eq,
        PartialEq,
        Serialize,
        Deserialize,
        Hash,
        Default,
    )]
    struct TypeAKey(String);
    impl HasTable for TypeA {
        const TABLE_NAME: &'static str = "type_a";
        type Key = TypeAKey;
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
    struct TypeB(String);
    #[derive(
        Ord,
        PartialOrd,
        Clone,
        Debug,
        Eq,
        PartialEq,
        Serialize,
        Deserialize,
        Hash,
        Default,
    )]
    struct TypeBKey(String);
    impl HasTable for TypeB {
        const TABLE_NAME: &'static str = "type_b";
        type Key = TypeBKey;
    }

    #[test]
    fn smoke_test() -> Result<()> {
        let kv = KVStore::open_in_memory().c(d!())?;
        // Try to create the table for TypeA
        kv.create_table::<TypeA>().c(d!())?;
        // Try to set a KV pair for TypeA
        let key1 = TypeAKey("test_keg".to_string());
        let value1 = TypeA("test_value".to_string());
        assert!(kv.set(&key1, value1.clone()).c(d!())?.is_none());
        // Verify the results
        assert!(kv.get(&key1).c(d!())? == Some(value1.clone()));
        // Update the value
        let value2 = TypeA("Changed Value!".to_string());
        assert!(kv.set(&key1, value2.clone()).c(d!())? == Some(value1));
        // Run house keeping to make sure we aren't deleting needed keys
        kv.run_housekeeping().c(d!())?;
        // Verify results
        assert!(kv.get(&key1).c(d!())? == Some(value2));

        // Attempt to get an invalid key
        let invalid_key = TypeAKey("invalid key!".to_string());
        assert!(kv.get::<TypeA>(&invalid_key).c(d!())? == None);

        // Attempt the initial set/get test, but with TypeB
        // This tests implicit table creation
        let key1 = TypeBKey("test_key_b".to_string());
        let value1 = TypeB("test_value_b".to_string());
        // Run house keeping to make sure we aren't deleting needed keys
        kv.run_housekeeping().c(d!())?;
        assert!(kv.set(&key1, value1.clone()).c(d!())?.is_none());
        assert!(kv.get(&key1).c(d!())? == Some(value1));
        Ok(())
    }

    #[test]
    fn get_all() -> Result<()> {
        // Generate some K/V Pairs
        let mut pairs = BTreeMap::new();
        for i in 0..10 {
            let k = TypeAKey(format!("key-{}", i));
            let v = TypeA(format!("value-{}", i));
            pairs.insert(k, v);
        }
        // Open our db
        let kv = KVStore::open_in_memory().c(d!())?;
        for (k, v) in &pairs {
            // Insert an invalid value first, so we can test for any negative interaction with updates
            kv.set(k, TypeA("INVALID".to_string())).c(d!())?;
            // Insert the correct value
            kv.set(k, v.clone()).c(d!())?;
        }
        // Make sure things match up
        assert!(kv.get_all::<TypeA>().c(d!())? == pairs);
        Ok(())
    }
    #[test]
    fn with() -> Result<()> {
        let kv = KVStore::open_in_memory().c(d!())?;
        let key1 = TypeAKey("key-1".to_string());
        let value1 = TypeA("value-1".to_string());
        kv.set(&key1, value1).c(d!())?;
        // Mutate value1 inside the store
        kv.with::<TypeA, _>(&key1, |x| {
            x.0 = "value-2".to_string();
            Ok(())
        })
        .c(d!())?;
        assert!(kv.get(&key1).c(d!())? == Some(TypeA("value-2".to_string())));
        Ok(())
    }

    #[test]
    fn delete() -> Result<()> {
        let kv = KVStore::open_in_memory().c(d!())?;
        // Add the same key a bunch of times
        let key1 = TypeAKey("key-1".to_string());
        for i in 0..10 {
            kv.set(&key1, TypeA(format!("{}", i))).c(d!())?;
        }
        // Delete the key
        kv.delete::<TypeA>(&key1).c(d!())?;
        // Make sure its gone
        assert_eq!(kv.get::<TypeA>(&key1).c(d!())?, None);

        Ok(())
    }
}
