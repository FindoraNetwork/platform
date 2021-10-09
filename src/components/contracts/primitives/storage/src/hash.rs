use sha2::Digest;

/// Hasher to use to hash keys to insert to storage.
pub trait StorageHasher: 'static {
    type Output: AsRef<[u8]>;
    fn hash(x: &[u8]) -> Self::Output;
}

pub struct Sha256;

impl StorageHasher for Sha256 {
    type Output = [u8; 32];

    fn hash(x: &[u8]) -> Self::Output {
        let mut dest = [0; 32];
        dest.copy_from_slice(sha2::Sha256::digest(x).as_slice());
        dest
    }
}

/// Get the storage hashed key.
pub trait StorageHashedKey {
    fn store_key() -> Vec<u8>;
}

/// Get the storage prefix key.
pub trait StoragePrefixKey {
    fn store_key() -> Vec<u8>;
}
