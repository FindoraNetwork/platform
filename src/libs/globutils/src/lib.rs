//!
//! # Global common utils
//!

#![deny(warnings)]
#![deny(missing_docs)]

pub mod logging;
pub mod wallet;

use {
    cryptohash::{
        sha256::{self, Digest},
        Proof,
    },
    ruc::*,
    serde::{Deserialize, Deserializer, Serialize, Serializer},
    std::{fs, marker::PhantomData, path::PathBuf, result::Result as StdResult},
    zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSignature},
};

/// Perform a synchronize http get request with attohttpc,
/// and parse the response as a String
#[inline(always)]
#[cfg(not(target_arch = "wasm32"))]
pub fn http_get_request(query: &str) -> StdResult<String, attohttpc::Error> {
    attohttpc::get(query).send()?.error_for_status()?.text()
}

/// Create a new temporary file for a `findora_ledger`
pub fn fresh_tmp_dir() -> PathBuf {
    let basedir = PathBuf::from("/tmp");
    let basedirname = "findora_ledger";
    let mut dirname = None;
    while dirname.is_none() {
        let name = std::format!("{}_{}", basedirname, rand::random::<u64>());
        let path = basedir.join(name);
        let _ = fs::remove_dir_all(&path);
        if fs::create_dir(&path).is_ok() {
            dirname = Some(path);
        }
    }
    dirname.unwrap()
}

// Convert a u64 into a string with commas.
fn commas_u64(input: u64) -> String {
    if input < 10000 {
        return format!("{input}");
    }

    let mut value = input;
    let mut result = "".to_string();

    while value > 1000 {
        result = format!(",{:03.3}", value % 1000) + &result;
        value /= 1000;
    }

    if value == 1000 {
        result = "1,000".to_owned() + &result;
    } else {
        result = format!("{value}") + &result;
    }

    result
}

// Convert an i64 into a string with commas.
fn commas_i64(input: i64) -> String {
    if input == 0 {
        return "0".to_string();
    }

    let sign = input < 0;
    let mut result;

    if input == std::i64::MIN {
        result = commas_u64(1u64 << 63);
    } else if input < 0 {
        result = commas_u64(-input as u64);
    } else {
        result = commas_u64(input as u64);
    }

    if sign {
        result = "-".to_owned() + &result;
    }

    result
}

#[allow(missing_docs)]
pub trait Commas {
    fn commas(self) -> String;
}

macro_rules! impl_commas {
    ($ty_s: ty, $ty_t: ty, $func: tt) => {
        impl Commas for $ty_s {
            #[inline(always)]
            fn commas(self) -> String {
                crate::$func(self as $ty_t)
            }
        }
    };
}

impl_commas!(u64, u64, commas_u64);
impl_commas!(u32, u64, commas_u64);
impl_commas!(u16, u64, commas_u64);
impl_commas!(u8, u64, commas_u64);
impl_commas!(usize, u64, commas_u64);
impl_commas!(i64, i64, commas_i64);
impl_commas!(i32, i64, commas_i64);
impl_commas!(i16, i64, commas_i64);
impl_commas!(i8, i64, commas_i64);

/// Wrapper around a serialized variable that maintains type semantics.
#[derive(Clone, Debug)]
pub struct Serialized<T> {
    val: String,
    phantom: PhantomData<T>,
}

/// A tuple struct to create and verify a signature
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct SignatureOf<T>(pub SignatureOfBytes<Serialized<T>>);

impl<T> SignatureOf<T>
where
    T: Serialize + serde::de::DeserializeOwned,
{
    /// Serialize a data structure and singed it with the `XfrKeyPair`
    #[inline(always)]
    pub fn new(xfr: &XfrKeyPair, to_sign: &T) -> Self {
        Self(SignatureOfBytes::new(xfr, &Serialized::new(to_sign)))
    }

    /// Verify if a value is properly singed with the `XfrKeyPair`
    #[inline(always)]
    pub fn verify(&self, pubkey: &XfrPublicKey, val: &T) -> Result<()> {
        self.0.verify(pubkey, &Serialized::new(val)).c(d!())
    }
}

/// A tuple struct to calculate and display a hash value
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct HashOf<T>(pub HashOfBytes<Serialized<T>>);

impl<T> HashOf<T>
where
    T: Serialize + serde::de::DeserializeOwned,
{
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(to_hash: &T) -> Self {
        Self(HashOfBytes::new(&Serialized::new(to_hash)))
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new_from_str(val: String) -> Self {
        Self(HashOfBytes::new(&Serialized::new_from_str(val)))
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn hex(&self) -> String {
        hex::encode(self)
    }
}

impl<T> AsRef<[u8]> for HashOf<T> {
    #[inline(always)]
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

/// A tuple struct to create an verify a merkle proof
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofOf<T>(pub ProofOfBytes<Serialized<T>>);

impl<T> ProofOf<T>
where
    T: Serialize + serde::de::DeserializeOwned,
{
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(proof: Proof) -> Self {
        Self(ProofOfBytes::new(proof))
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn verify(&self, leaf: &T) -> bool {
        self.0.verify(HashOf::new(leaf).0)
    }
}

/// Wrapper around a Digest variable that maintains type semantics.
#[derive(Copy, Clone, Debug)]
pub struct HashOfBytes<T> {
    #[allow(missing_docs)]
    pub hash: Digest,
    phantom: PhantomData<T>,
}

/// Wrapper around a Proof variable that maintains type semantics.
#[derive(Debug, Clone)]
pub struct ProofOfBytes<T> {
    #[allow(missing_docs)]
    pub proof: Proof,
    phantom: PhantomData<T>,
}

/// Wrapper around a XfrSingature variable that maintains type semantics.
#[derive(Clone, Debug)]
pub struct SignatureOfBytes<T> {
    sig: XfrSignature,
    phantom: PhantomData<T>,
}

impl<T> PartialEq for HashOfBytes<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.hash == rhs.hash
    }
}
impl<T> Eq for HashOfBytes<T> {}

impl<T> PartialEq for SignatureOfBytes<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.sig == rhs.sig
    }
}
impl<T> Eq for SignatureOfBytes<T> {}

impl<T> Serialized<T>
where
    T: serde::Serialize + serde::de::DeserializeOwned,
{
    /// Serializing a type by serde_json
    #[inline(always)]
    pub fn new(to_serialize: &T) -> Self {
        Serialized {
            val: serde_json::to_string(&to_serialize).unwrap(),
            phantom: PhantomData,
        }
    }

    #[allow(missing_docs)]
    #[inline(always)]
    pub fn new_from_str(val: String) -> Self {
        Serialized {
            val,
            phantom: PhantomData,
        }
    }

    /// Deserializing a type by serde_json
    #[inline(always)]
    pub fn deserialize(&self) -> T {
        serde_json::from_str(&self.val).unwrap()
    }
}

impl<T> AsRef<[u8]> for Serialized<T>
where
    T: serde::Serialize + serde::de::DeserializeOwned,
{
    #[inline(always)]
    fn as_ref(&self) -> &[u8] {
        self.val.as_ref()
    }
}

impl<T> Default for Serialized<T>
where
    T: Default + serde::Serialize + serde::de::DeserializeOwned,
{
    #[inline(always)]
    fn default() -> Self {
        Self::new(&T::default())
    }
}

impl<T> Serialize for Serialized<T>
where
    T: serde::Serialize + serde::de::DeserializeOwned,
{
    #[inline(always)]
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        self.deserialize().serialize(serializer)
    }
}

impl<'a, T> Deserialize<'a> for Serialized<T>
where
    T: serde::Serialize + serde::de::DeserializeOwned,
{
    #[inline(always)]
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> StdResult<Self, D::Error> {
        T::deserialize(deserializer).map(|x| Self::new(&x))
    }
}

impl<T> PartialEq for Serialized<T>
where
    T: PartialEq + serde::Serialize + serde::de::DeserializeOwned,
{
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.deserialize() == other.deserialize()
    }
}

impl<T> Eq for Serialized<T> where T: Eq + serde::Serialize + serde::de::DeserializeOwned {}

impl<T> HashOfBytes<T>
where
    T: AsRef<[u8]>,
{
    #[inline(always)]
    /// Hashing a type with sha256
    pub fn new(to_hash: &T) -> Self {
        Self {
            hash: sha256::hash(to_hash.as_ref()),
            phantom: PhantomData,
        }
    }
}

impl<T> AsRef<[u8]> for HashOfBytes<T> {
    #[inline(always)]
    fn as_ref(&self) -> &[u8] {
        self.hash.as_ref()
    }
}

impl<T> Serialize for HashOfBytes<T> {
    #[inline(always)]
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        self.hash.serialize(serializer)
    }
}

impl<'a, T> Deserialize<'a> for HashOfBytes<T> {
    #[inline(always)]
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> StdResult<Self, D::Error> {
        // NOTE: doesn't guarantee that there *is* a T that has this hash
        Digest::deserialize(deserializer).map(|hash| Self {
            hash,
            phantom: PhantomData,
        })
    }
}

impl<T> ProofOfBytes<T>
where
    T: AsRef<[u8]>,
{
    /// Creating a wrapper of Proof
    #[inline(always)]
    pub fn new(proof: Proof) -> Self {
        Self {
            proof,
            phantom: PhantomData,
        }
    }

    /// Verifying if this proof is valid
    #[inline(always)]
    pub fn verify(&self, leaf: HashOfBytes<T>) -> bool {
        self.proof.is_valid_proof(leaf.hash.into())
    }
}

impl<T> Serialize for ProofOfBytes<T> {
    #[inline(always)]
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        self.proof.serialize(serializer)
    }
}

impl<'a, T> Deserialize<'a> for ProofOfBytes<T> {
    #[inline(always)]
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> StdResult<Self, D::Error> {
        // NOTE: doesn't guarantee that there *is* a T that has this proof
        Proof::deserialize(deserializer).map(|proof| Self {
            proof,
            phantom: PhantomData,
        })
    }
}

impl<T> SignatureOfBytes<T>
where
    T: AsRef<[u8]>,
{
    /// Create a signature with specified keypair
    #[inline(always)]
    pub fn new(xfr: &XfrKeyPair, to_sign: &T) -> Self {
        Self {
            sig: xfr.get_sk_ref().sign(to_sign.as_ref(), xfr.get_pk_ref()),
            phantom: PhantomData,
        }
    }

    /// Verify a signature with specified keypair
    #[inline(always)]
    pub fn verify(&self, pubkey: &XfrPublicKey, val: &T) -> Result<()> {
        pubkey.verify(val.as_ref(), &self.sig).c(d!())
    }
}

impl<T> Serialize for SignatureOfBytes<T> {
    #[inline(always)]
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        self.sig.serialize(serializer)
    }
}

impl<'a, T> Deserialize<'a> for SignatureOfBytes<T> {
    #[inline(always)]
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> StdResult<Self, D::Error> {
        // NOTE: doesn't guarantee that there *is* a T that this is a signature for
        XfrSignature::deserialize(deserializer).map(|sig| Self {
            sig,
            phantom: PhantomData,
        })
    }
}

#[cfg(test)]
#[allow(missing_docs)]
mod tests {
    use super::*;

    #[test]
    fn test_commas() {
        // Test u64
        assert_eq!("0", 0u64.commas());
        assert_eq!("100", 100u64.commas());
        assert_eq!("999", 999u64.commas());
        assert_eq!("1000", 1000_u64.commas());
        assert_eq!("9999", 9999u64.commas());
        assert_eq!("10,000", 10000_u64.commas());
        assert_eq!("1,000,000", (1000u64 * 1000u64).commas());
        assert_eq!("1,048,576", (1024 * 1024_u64).commas());
        assert_eq!("999,000", (999 * 1000_u64).commas());
        assert_eq!("2000", (2 * 1000_u64).commas());
        assert_eq!("1,000,000,000", (1000 * 1000 * 1000_u64).commas());
        assert_eq!("18,446,744,073,709,551,615", std::u64::MAX.commas());

        // Test u32
        assert_eq!("0", 0u32.commas());
        assert_eq!("100", 100u32.commas());
        assert_eq!("999", 999u32.commas());
        assert_eq!("1000", 1000_u32.commas());
        assert_eq!("9999", 9999u32.commas());
        assert_eq!("10,000", 10000_u32.commas());
        assert_eq!("1,000,000", (1000u32 * 1000u32).commas());
        assert_eq!("1,048,576", (1024 * 1024_u32).commas());
        assert_eq!("999,000", (999 * 1000_u32).commas());
        assert_eq!("2000", (2 * 1000_u32).commas());
        assert_eq!("1,000,000,000", (1000 * 1000 * 1000_u32).commas());
        assert_eq!("4,294,967,295", std::u32::MAX.commas());

        // Test u16
        assert_eq!("0", 0u16.commas());
        assert_eq!("100", 100u16.commas());
        assert_eq!("999", 999u16.commas());
        assert_eq!("1000", 1000_u16.commas());
        assert_eq!("9999", 9999u16.commas());
        assert_eq!("10,000", 10000_u16.commas());
        assert_eq!("2000", (2 * 1000_u16).commas());
        assert_eq!("65,535", std::u16::MAX.commas());

        // Test u8
        assert_eq!("0", 0u8.commas());
        assert_eq!("1", 1u8.commas());
        assert_eq!("100", 100u8.commas());
        assert_eq!("255", std::u8::MAX.commas());

        // Test i64
        assert_eq!("0", 0i64.commas());
        assert_eq!("100", 100i64.commas());
        assert_eq!("999", 999i64.commas());
        assert_eq!("1000", 1000.commas());
        assert_eq!("9999", 9999i64.commas());
        assert_eq!("10,000", 10000_i64.commas());
        assert_eq!("1,000,000", (1000i64 * 1000i64).commas());
        assert_eq!("999,000", (999i64 * 1000i64).commas());
        assert_eq!("2000", (2 * 1000_i64).commas());
        assert_eq!("1,000,000,000", (1000 * 1000 * 1000_i64).commas());
        assert_eq!("9,223,372,036,854,775,807", std::i64::MAX.commas());
        assert_eq!("-100", (-100_i64).commas());
        assert_eq!("-999", (-999_i64).commas());
        assert_eq!("-1000", (-1000_i64).commas());
        assert_eq!("-1,000,000", (-1000 * 1000_i64).commas());
        assert_eq!("-1,048,576", (-1024 * 1024_i64).commas());
        assert_eq!("-999,000", (-999 * 1000_i64).commas());
        assert_eq!("-2000", (-2 * 1000_i64).commas());
        assert_eq!("-1,000,000,000", (-1000 * 1000 * 1000_i64).commas());
        assert_eq!("-9,223,372,036,854,775,808", (std::i64::MIN).commas());

        // Test i32.
        assert_eq!("0", 0i32.commas());
        assert_eq!("100", 100i32.commas());
        assert_eq!("999", 999i32.commas());
        assert_eq!("1000", 1000.commas());
        assert_eq!("9999", 9999i32.commas());
        assert_eq!("10,000", 10000_i32.commas());
        assert_eq!("1,000,000", (1000i32 * 1000i32).commas());
        assert_eq!("999,000", (999i32 * 1000i32).commas());
        assert_eq!("2000", (2 * 1000_i32).commas());
        assert_eq!("1,000,000,000", (1000 * 1000 * 1000_i32).commas());
        assert_eq!("2,147,483,647", std::i32::MAX.commas());
        assert_eq!("-100", (-100_i32).commas());
        assert_eq!("-999", (-999_i32).commas());
        assert_eq!("-1000", (-1000_i32).commas());
        assert_eq!("-1,000,000", (-1000 * 1000_i32).commas());
        assert_eq!("-1,048,576", (-1024 * 1024_i32).commas());
        assert_eq!("-999,000", (-999 * 1000_i32).commas());
        assert_eq!("-2000", (-2 * 1000_i32).commas());
        assert_eq!("-1,000,000,000", (-1000 * 1000 * 1000_i32).commas());
        assert_eq!("-2,147,483,648", (std::i32::MIN).commas());

        // Test i16
        assert_eq!("0", 0i16.commas());
        assert_eq!("100", 100i16.commas());
        assert_eq!("999", 999i16.commas());
        assert_eq!("1000", 1000.commas());
        assert_eq!("9999", 9999i16.commas());
        assert_eq!("10,000", 10000_i16.commas());
        assert_eq!("2000", (2 * 1000_i16).commas());
        assert_eq!("32,767", std::i16::MAX.commas());
        assert_eq!("-100", (-100_i16).commas());
        assert_eq!("-999", (-999_i16).commas());
        assert_eq!("-1000", (-1000_i16).commas());
        assert_eq!("-2000", (-2 * 1000_i16).commas());
        assert_eq!("-32,768", (std::i16::MIN).commas());

        // Test i8
        assert_eq!("0", 0i8.commas());
        assert_eq!("-1", (-1i8).commas());
        assert_eq!("100", 100i8.commas());
        assert_eq!("127", std::i8::MAX.commas());
        assert_eq!("-100", (-100_i8).commas());
        assert_eq!("-128", (std::i8::MIN).commas());
    }
}
