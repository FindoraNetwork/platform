use crate::hash::StorageHasher;
use crate::*;
use conf::abci::global_cfg::CFG;
use ruc::*;
use std::str::FromStr;
use storage::db::MerkleDB;
use storage::state::State;
use storage::store::Prefix;

/// A type that allow to store value for given key. Allowing to insert/remove/iterate on values.
///
/// Each value is stored at:
/// ```nocompile
/// Sha256(Instance::module_prefix() + Instance::STORAGE_PREFIX)
///     ++ serialize(key)
/// ```
///
pub struct StorageMap<Instance, Hasher, Key, Value>(
    core::marker::PhantomData<(Instance, Hasher, Key, Value)>,
);

impl<Instance, Hasher, Key, Value> StorageMap<Instance, Hasher, Key, Value>
where
    Instance: StorageInstance + StatelessStore,
    Hasher: StorageHasher<Output = [u8; 32]>,
    Key: ToString + FromStr,
    Value: Serialize + DeserializeOwned,
{
    pub fn module_prefix() -> &'static [u8] {
        Instance::module_prefix().as_bytes()
    }

    pub fn storage_prefix() -> &'static [u8] {
        Instance::STORAGE_PREFIX.as_bytes()
    }

    /// Get the storage key used to fetch a value corresponding to a specific key.
    pub fn build_key_for(key: &Key) -> Vec<u8> {
        let prefix_key: Vec<u8> =
            [Self::module_prefix(), Self::storage_prefix()].concat();
        let data_key = key.to_string();

        let final_key = Prefix::new(prefix_key.as_slice());
        final_key.push(data_key.as_ref()).as_ref().to_vec()
    }

    pub fn parse_key_for(key_list: Vec<&str>) -> Result<Key> {
        let last_key = key_list
            .last()
            .copied()
            .ok_or(eg!("parse key failed with empty list"))?;
        Key::from_str(last_key).map_err(|_| eg!("key convert to string err"))
    }

    /// Does the value (explicitly) exist in storage?
    pub fn contains_key<D: MerkleDB>(state: &State<D>, key: &Key) -> bool {
        Instance::exists(state, Self::build_key_for(key).as_slice()).unwrap()
    }

    /// Read the length of the storage value without decoding the entire value under the
    /// given `key`.
    pub fn decode_len<D: MerkleDB>(state: &State<D>, key: &Key) -> Option<usize> {
        Instance::get::<D>(state, Self::build_key_for(key).as_slice())
            .unwrap()
            .map(|val| val.len())
    }

    /// Load the value associated with the given key from the map.
    pub fn get<D: MerkleDB>(state: &State<D>, key: &Key) -> Option<Value> {
        Instance::get_obj::<Value, D>(state, Self::build_key_for(key).as_slice())
            .unwrap()
    }

    /// Load the value associated with the given key from the map.
    pub fn get_bytes<D: MerkleDB>(state: &State<D>, key: &Key) -> Option<Vec<u8>> {
        Instance::get::<D>(state, Self::build_key_for(key).as_slice()).unwrap()
    }

    /// Load versioned value associated with the given key from the map.
    pub fn get_ver<D: MerkleDB>(
        state: &State<D>,
        key: &Key,
        height: u64,
    ) -> Option<Value> {
        Instance::get_obj_v::<Value, D>(
            state,
            Self::build_key_for(key).as_slice(),
            height,
        )
        .unwrap()
    }

    /// Load versioned value associated with the given key from the map.
    pub fn get_ver_bytes<D: MerkleDB>(
        state: &State<D>,
        key: &Key,
        height: u64,
    ) -> Option<Vec<u8>> {
        Instance::get_v::<D>(state, Self::build_key_for(key).as_slice(), height).unwrap()
    }

    /// Load the unique key value pair with specified prefix.
    pub fn get_unique_prefix<D: MerkleDB>(
        state: &State<D>,
        prefix: &Key,
    ) -> Option<(Key, Value)> {
        let prefix = Prefix::new(Self::build_key_for(prefix).as_slice());

        let kv_map = Instance::iter_cur(state, prefix);
        for (k, v) in kv_map {
            let key_str = String::from_utf8_lossy(k.as_slice()).to_string();
            let key_list: Vec<_> = key_str.split(DB_SEPARATOR).collect();

            let key = Self::parse_key_for(key_list);
            let raw_value = serde_json::from_slice::<Value>(v.as_slice()).ok();

            if let (Ok(k), Some(v)) = (key, raw_value) {
                return Some((k, v));
            }
        }
        None
    }

    /// Store a value to be associated with the given key from the map.
    pub fn insert<D: MerkleDB>(
        state: &mut State<D>,
        key: &Key,
        val: &Value,
    ) -> Result<()> {
        Instance::set_obj::<Value, D>(state, Self::build_key_for(key).as_slice(), val)
    }

    /// Store a serialized value to be associated with the given key from the map.
    pub fn insert_bytes<D: MerkleDB>(
        state: &mut State<D>,
        key: &Key,
        val: Vec<u8>,
    ) -> Result<()> {
        Instance::set::<D>(state, Self::build_key_for(key).as_slice(), val)
    }

    /// Remove the value under a key.
    pub fn remove<D: MerkleDB>(state: &mut State<D>, key: &Key) {
        if state.height().unwrap() >= CFG.checkpoint.evm_substate_v2_height as u64 {
            Instance::delete(state, Self::build_key_for(key).as_slice()).unwrap()
        } else {
            Instance::delete_v0(state, Self::build_key_for(key).as_slice()).unwrap()
        }
    }

    /// Iter over all value of the storage.
    pub fn iterate<D: MerkleDB>(state: &State<D>) -> Vec<(Key, Value)> {
        let prefix_key: Vec<u8> =
            [Self::module_prefix(), Self::storage_prefix()].concat();
        let prefix = Prefix::new(prefix_key.as_ref());

        let kv_map = Instance::iter_cur(state, prefix);

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
