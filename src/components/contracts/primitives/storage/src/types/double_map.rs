use crate::hash::StorageHasher;
use crate::*;
use conf::abci::global_cfg::CFG;
use ruc::*;
use std::str::FromStr;
use storage::db::MerkleDB;
use storage::state::State;
use storage::store::Prefix;

/// A type that allow to store values for `(key1, key2)` couple. Similar to `StorageMap` but allow
/// to iterate and remove value associated to first key.
///
/// Each value is stored at:
/// ```nocompile
/// Sha256(Instance::module_prefix() + Instance::STORAGE_PREFIX)
///    ++ serialize(key1)
///    ++ serialize(key2)
/// ```
///
pub struct StorageDoubleMap<Instance, Hasher, Key1, Key2, Value>(
    core::marker::PhantomData<(Instance, Hasher, Key1, Key2, Value)>,
);

impl<Instance, Hasher, Key1, Key2, Value>
    StorageDoubleMap<Instance, Hasher, Key1, Key2, Value>
where
    Instance: StorageInstance + StatelessStore,
    Hasher: StorageHasher<Output = [u8; 32]>,
    Key1: ToString + FromStr,
    Key2: ToString + FromStr,
    Value: Serialize + DeserializeOwned,
{
    pub fn module_prefix() -> &'static [u8] {
        Instance::module_prefix().as_bytes()
    }

    pub fn storage_prefix() -> &'static [u8] {
        Instance::STORAGE_PREFIX.as_bytes()
    }

    /// Get the storage key used to fetch a value corresponding to a specific key.
    pub fn build_key_for(k1: &Key1, k2: &Key2) -> Vec<u8> {
        let prefix_key: Vec<u8> =
            [Self::module_prefix(), Self::storage_prefix()].concat();
        let data_key1 = k1.to_string();
        let data_key2 = k2.to_string();

        let final_key = Prefix::new(prefix_key.as_slice());
        final_key
            .push_sub(data_key1.as_ref(), data_key2.as_ref())
            .as_ref()
            .to_vec()
    }

    pub fn parse_key_for(key_list: Vec<&str>) -> Result<Key2> {
        let last_key = key_list
            .last()
            .copied()
            .ok_or(eg!("parse key failed with empty list"))?;
        Key2::from_str(last_key).map_err(|_| eg!("key convert to string err"))
    }

    /// Does the value (explicitly) exist in storage?
    pub fn contains_key<D: MerkleDB>(state: &State<D>, k1: &Key1, k2: &Key2) -> bool {
        Instance::exists(state, Self::build_key_for(k1, k2).as_slice()).unwrap()
    }

    /// Load the value associated with the given key from the map.
    pub fn get<D: MerkleDB>(state: &State<D>, k1: &Key1, k2: &Key2) -> Option<Value> {
        Instance::get_obj::<Value, D>(state, Self::build_key_for(k1, k2).as_slice())
            .unwrap()
    }

    /// Load versioned value associated with the given key from the map.
    pub fn get_ver<D: MerkleDB>(
        state: &State<D>,
        k1: &Key1,
        k2: &Key2,
        height: u64,
    ) -> Option<Value> {
        Instance::get_obj_v::<Value, D>(
            state,
            Self::build_key_for(k1, k2).as_slice(),
            height,
        )
        .unwrap()
    }

    /// Store a value to be associated with the given key from the map.
    pub fn insert<D: MerkleDB>(
        state: &mut State<D>,
        k1: &Key1,
        k2: &Key2,
        val: &Value,
    ) -> Result<()> {
        Instance::set_obj::<Value, D>(state, Self::build_key_for(k1, k2).as_slice(), val)
    }

    /// Remove the value under a key.
    pub fn remove<D: MerkleDB>(state: &mut State<D>, k1: &Key1, k2: &Key2) {
        if state.height().unwrap() >= CFG.checkpoint.evm_substate_v2_height as u64 {
            Instance::delete(state, Self::build_key_for(k1, k2).as_slice()).unwrap();
        } else {
            Instance::delete_v0(state, Self::build_key_for(k1, k2).as_slice()).unwrap();
        }
    }

    /// Remove all values under the first key.
    pub fn remove_prefix<D: MerkleDB>(state: &mut State<D>, k1: &Key1) {
        for (k2, _) in Self::iterate_prefix(state, k1).iter() {
            Self::remove(state, k1, k2);
        }
    }

    /// Iter over all value of the storage.
    pub fn iterate_prefix<D: MerkleDB>(
        state: &State<D>,
        k1: &Key1,
    ) -> Vec<(Key2, Value)> {
        let prefix_key: Vec<u8> =
            [Self::module_prefix(), Self::storage_prefix()].concat();
        let data_key1 = k1.to_string();
        let final_key = Prefix::new(prefix_key.as_slice()).push(data_key1.as_ref());

        let kv_map = Instance::iter_cur(state, final_key);

        let mut res = Vec::new();
        for (k, v) in kv_map {
            let key_str = String::from_utf8_lossy(k.as_slice()).to_string();
            let key_list: Vec<_> = key_str.split(DB_SEPARATOR).collect();

            let key = Self::parse_key_for(key_list);
            let raw_value = serde_json::from_slice::<Value>(v.as_slice()).ok();

            if let (Ok(k), Some(v)) = (key, raw_value) {
                res.push((k, v))
            }
        }
        res
    }
}
