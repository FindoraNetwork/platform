use crate::hash::*;
use crate::*;
use storage::db::MerkleDB;
use storage::state::State;
use storage::store::Prefix;

/// A type that allow to store a value.
///
/// Each value is stored at:
/// ```nocompile
/// Sha256(Instance::module_name() + Instance::STORAGE_PREFIX)
/// ```
///
pub struct StorageValue<Instance, Hasher, Value>(
    core::marker::PhantomData<(Instance, Hasher, Value)>,
);

impl<Instance, Hasher, Value> StorageHashedKey for StorageValue<Instance, Hasher, Value>
where
    Instance: StorageInstance + StatelessStore,
    Hasher: StorageHasher<Output = [u8; 32]>,
    Value: Serialize + DeserializeOwned,
{
    /// Get the storage hashed key.
    fn store_key() -> Vec<u8> {
        let raw_key: Vec<u8> = [Self::module_prefix(), Self::storage_prefix()].concat();
        Hasher::hash(raw_key.as_slice()).to_vec()
    }
}

impl<Instance, Hasher, Value> StoragePrefixKey for StorageValue<Instance, Hasher, Value>
where
    Instance: StorageInstance + StatelessStore,
    Hasher: StorageHasher<Output = [u8; 32]>,
    Value: Serialize + DeserializeOwned,
{
    /// Get the storage key.
    fn store_key() -> Vec<u8> {
        let prefix = Prefix::new(Self::module_prefix());
        prefix.push(Self::storage_prefix()).as_ref().to_vec()
    }
}

impl<Instance, Hasher, Value> StorageValue<Instance, Hasher, Value>
where
    Instance: StorageInstance + StatelessStore,
    Hasher: StorageHasher<Output = [u8; 32]>,
    Value: Serialize + DeserializeOwned,
{
    pub fn module_prefix() -> &'static [u8] {
        Instance::module_prefix().as_bytes()
    }

    pub fn storage_prefix() -> &'static [u8] {
        Instance::STORAGE_PREFIX.as_bytes()
    }

    /// Does the value (explicitly) exist in storage?
    pub fn exists<D: MerkleDB>(state: Arc<RwLock<State<D>>>) -> bool {
        Instance::exists(
            state.read().deref(),
            <Self as StoragePrefixKey>::store_key().as_ref(),
        )
        .unwrap()
    }

    /// Load the value from the provided storage instance.
    pub fn get<D: MerkleDB>(state: Arc<RwLock<State<D>>>) -> Option<Value> {
        Instance::get_obj::<Value, D>(
            state.read().deref(),
            <Self as StoragePrefixKey>::store_key().as_ref(),
        )
        .unwrap()
    }

    /// Store a value under this hashed key into the provided storage instance.
    pub fn put<D: MerkleDB>(state: Arc<RwLock<State<D>>>, val: &Value) -> Result<()> {
        Instance::set_obj::<Value, D>(
            state.write().deref_mut(),
            <Self as StoragePrefixKey>::store_key().as_ref(),
            val,
        )
    }

    /// Take a value from storage, removing it afterwards.
    pub fn delete<D: MerkleDB>(state: Arc<RwLock<State<D>>>) {
        Instance::delete(
            state.write().deref_mut(),
            <Self as StoragePrefixKey>::store_key().as_ref(),
        )
        .unwrap()
    }
}
