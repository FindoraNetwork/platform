#![deny(warnings)]
// #![deny(missing_docs)]

use cryptohash::sha256::Digest;
use cryptohash::{sha256, Proof};
use percent_encoding::{percent_decode, utf8_percent_encode, AsciiSet, CONTROLS};
use ruc::*;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fs;
use std::io::{Error, ErrorKind};
use std::marker::PhantomData;
use std::path::PathBuf;
use std::result::Result as StdResult;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSignature};

pub const TRANSACTION_WINDOW_WIDTH: u64 = 100;

pub fn string_of_type<T>(_: &T) -> String {
    std::any::type_name::<T>().to_string()
}

pub fn print_type_of<T>(msg: &str, _: &T) {
    println!("type of {}: {}", msg, std::any::type_name::<T>())
}

/// https://url.spec.whatwg.org/#fragment-percent-encode-set
const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');

pub fn urldecode(s: &str) -> String {
    let iter = percent_decode(s.as_bytes());
    iter.decode_utf8().unwrap().to_string()
}

pub fn urlencode(input: &str) -> String {
    let iter = utf8_percent_encode(input, FRAGMENT);
    iter.collect()
}

const PROTOCOL: &str = "http";
const SERVER_HOST: &str = "localhost";

/// Query server port
pub const QUERY_PORT: usize = 8667;
/// Port for submitting transactions.
pub const SUBMIT_PORT: usize = 8669;
/// Ledger port
pub const LEDGER_PORT: usize = 8668;

/// Sets the protocol and host.
///
/// Environment variables `PROTOCOL` and `SERVER_HOST` set the protocol and host,
///
/// By default, the protocol is `http` and the host is `testnet.findora.org`.
pub fn protocol_host() -> (String, String) {
    (
        std::env::var_os("PROTOCOL")
            .filter(|x| !x.is_empty())
            .and_then(|x| x.into_string().ok())
            .unwrap_or_else(|| PROTOCOL.to_string()),
        std::env::var_os("SERVER_HOST")
            .filter(|x| !x.is_empty())
            .and_then(|x| x.into_string().ok())
            .unwrap_or_else(|| SERVER_HOST.to_string()),
    )
}

#[cfg(not(target_arch = "wasm32"))]
pub fn http_post_request<T: Serialize>(
    query: &str,
    body: Option<T>,
) -> StdResult<String, attohttpc::Error> {
    let req = attohttpc::post(query);

    if let Some(body) = body {
        req.json(&body)?.send()?.error_for_status()?.text()
    } else {
        req.send()?.error_for_status()?.text()
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[inline(always)]
pub fn http_get_request(query: &str) -> StdResult<String, attohttpc::Error> {
    attohttpc::get(query).send()?.error_for_status()?.text()
}

pub fn fresh_tmp_dir() -> PathBuf {
    let base_dir = std::env::temp_dir();
    let base_dirname = "findora_ledger";
    let mut i = 0;
    let mut dirname = None;
    while dirname.is_none() {
        debug_assert!(i < 4); // TODO(joe): fail more gracefully
        let name = std::format!("{}_{}", base_dirname, rand::random::<u64>());
        let path = base_dir.join(name);
        let _ = fs::remove_dir_all(&path);
        match fs::create_dir(&path) {
            Ok(()) => {
                dirname = Some(path);
            }
            Err(_) => {
                i += 1;
            }
        }
    }

    // Safe unwrap -- the loop would never terminate if it stayed None
    dirname.unwrap()
}

pub fn se(s: String) -> Option<Error> {
    Some(Error::new(ErrorKind::Other, s))
}

pub fn er<T>(s: String) -> StdResult<T, Error> {
    Err(Error::new(ErrorKind::Other, s))
}

/// Convert a u64 into a string with commas.
fn commas_u64(input: u64) -> String {
    if input < 10000 {
        return format!("{}", input);
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
        result = format!("{}", value) + &result;
    }

    result
}

/// Convert an i64 into a string with commas.
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

pub trait Commas {
    fn commas(self) -> String;
}

impl Commas for u64 {
    fn commas(self) -> String {
        crate::commas_u64(self)
    }
}

impl Commas for u32 {
    fn commas(self) -> String {
        crate::commas_u64(self as u64)
    }
}

impl Commas for u16 {
    fn commas(self) -> String {
        crate::commas_u64(self as u64)
    }
}

impl Commas for u8 {
    fn commas(self) -> String {
        crate::commas_u64(self as u64)
    }
}

impl Commas for usize {
    fn commas(self) -> String {
        crate::commas_u64(self as u64)
    }
}

impl Commas for i64 {
    fn commas(self) -> String {
        crate::commas_i64(self)
    }
}

impl Commas for i32 {
    fn commas(self) -> String {
        crate::commas_i64(self as i64)
    }
}

impl Commas for i16 {
    fn commas(self) -> String {
        crate::commas_i64(self as i64)
    }
}

impl Commas for i8 {
    fn commas(self) -> String {
        crate::commas_i64(self as i64)
    }
}

// Wrapper around a serialized variable that maintains type semantics.
#[derive(Clone, Debug)]
pub struct Serialized<T> {
    pub val: String,
    phantom: PhantomData<T>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct SignatureOf<T>(pub SignatureOfBytes<Serialized<T>>);

impl<T> SignatureOf<T>
where
    T: Serialize + serde::de::DeserializeOwned,
{
    pub fn new(xfr: &XfrKeyPair, to_sign: &T) -> Self {
        Self(SignatureOfBytes::new(xfr, &Serialized::new(to_sign)))
    }

    pub fn verify(&self, pubkey: &XfrPublicKey, val: &T) -> Result<()> {
        self.0.verify(pubkey, &Serialized::new(val)).c(d!())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct HashOf<T>(pub HashOfBytes<Serialized<T>>);

impl<T> HashOf<T>
where
    T: Serialize + serde::de::DeserializeOwned,
{
    pub fn new(to_hash: &T) -> Self {
        Self(HashOfBytes::new(&Serialized::new(to_hash)))
    }

    pub fn hex(&self) -> String {
        hex::encode(self)
    }
}

pub type GlobalState<T> = (
    HashOf<Option<T>>,
    u64,
    SignatureOf<(HashOf<Option<T>>, u64)>,
);

impl<T> AsRef<[u8]> for HashOf<T> {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofOf<T>(pub ProofOfBytes<Serialized<T>>);

impl<T> ProofOf<T>
where
    T: Serialize + serde::de::DeserializeOwned,
{
    pub fn new(proof: Proof) -> Self {
        Self(ProofOfBytes::new(proof))
    }

    pub fn verify(&self, leaf: &T) -> bool {
        self.0.verify(HashOf::new(leaf).0)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct HashOfBytes<T> {
    pub hash: Digest,
    phantom: PhantomData<T>,
}

#[derive(Debug, Clone)]
pub struct ProofOfBytes<T> {
    pub proof: Proof,
    phantom: PhantomData<T>,
}

#[derive(Clone, Debug)]
pub struct SignatureOfBytes<T> {
    pub sig: XfrSignature,
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
    pub fn new(to_serialize: &T) -> Self {
        Serialized {
            val: serde_json::to_string(&to_serialize).unwrap(),
            phantom: PhantomData,
        }
    }

    pub fn deserialize(&self) -> T {
        serde_json::from_str(&self.val).unwrap()
    }
}

impl<T> AsRef<[u8]> for Serialized<T>
where
    T: serde::Serialize + serde::de::DeserializeOwned,
{
    fn as_ref(&self) -> &[u8] {
        self.val.as_ref()
    }
}

impl<T> Default for Serialized<T>
where
    T: Default + serde::Serialize + serde::de::DeserializeOwned,
{
    fn default() -> Self {
        Self::new(&T::default())
    }
}

impl<T> Serialize for Serialized<T>
where
    T: serde::Serialize + serde::de::DeserializeOwned,
{
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        self.deserialize().serialize(serializer)
    }
}

impl<'a, T> Deserialize<'a> for Serialized<T>
where
    T: serde::Serialize + serde::de::DeserializeOwned,
{
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> StdResult<Self, D::Error> {
        T::deserialize(deserializer).map(|x| Self::new(&x))
    }
}

impl<T> PartialEq for Serialized<T>
where
    T: PartialEq + serde::Serialize + serde::de::DeserializeOwned,
{
    fn eq(&self, other: &Self) -> bool {
        self.deserialize() == other.deserialize()
    }
}

impl<T> Eq for Serialized<T> where T: Eq + serde::Serialize + serde::de::DeserializeOwned {}

impl<T> HashOfBytes<T>
where
    T: AsRef<[u8]>,
{
    pub fn new(to_hash: &T) -> Self {
        Self {
            hash: sha256::hash(to_hash.as_ref()),
            phantom: PhantomData,
        }
    }
}

impl<T> AsRef<[u8]> for HashOfBytes<T> {
    fn as_ref(&self) -> &[u8] {
        self.hash.as_ref()
    }
}

impl<T> Serialize for HashOfBytes<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        self.hash.serialize(serializer)
    }
}

impl<'a, T> Deserialize<'a> for HashOfBytes<T> {
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
    pub fn new(proof: Proof) -> Self {
        Self {
            proof,
            phantom: PhantomData,
        }
    }

    pub fn verify(&self, leaf: HashOfBytes<T>) -> bool {
        self.proof.is_valid_proof(leaf.hash.into())
    }
}

impl<T> Serialize for ProofOfBytes<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        self.proof.serialize(serializer)
    }
}

impl<'a, T> Deserialize<'a> for ProofOfBytes<T> {
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
    pub fn new(xfr: &XfrKeyPair, to_sign: &T) -> Self {
        Self {
            sig: xfr.get_sk_ref().sign(to_sign.as_ref(), xfr.get_pk_ref()),
            phantom: PhantomData,
        }
    }

    pub fn verify(&self, pubkey: &XfrPublicKey, val: &T) -> Result<()> {
        pubkey.verify(val.as_ref(), &self.sig).c(d!())
    }
}

impl<T> Serialize for SignatureOfBytes<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> StdResult<S::Ok, S::Error> {
        self.sig.serialize(serializer)
    }
}

impl<'a, T> Deserialize<'a> for SignatureOfBytes<T> {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> StdResult<Self, D::Error> {
        // NOTE: doesn't guarantee that there *is* a T that this is a signature for
        XfrSignature::deserialize(deserializer).map(|sig| Self {
            sig,
            phantom: PhantomData,
        })
    }
}

pub trait NetworkRoute {
    fn route(&self) -> String;

    fn with_arg(&self, arg: &dyn std::fmt::Display) -> String {
        let mut endpoint = self.route();
        endpoint += &("/".to_owned() + &arg.to_string());
        endpoint
    }

    // e.g. SubmissionRoutes::TxnStatus.with_arg_template("str") = "/submit_transaction/{str}"
    fn with_arg_template(&self, arg: &str) -> String {
        let mut endpoint = self.route();
        endpoint += &("/".to_owned() + &"{".to_owned() + arg + &"}".to_owned());
        endpoint
    }
}

#[cfg(test)]
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

///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////

pub mod wallet {
    //!
    //! Generate mnemonic and restore keypair from it.
    //!
    //! Separating mnemonic to a standalone library is needed by tests.
    //!

    use bech32::{self, FromBase32, ToBase32};
    use bip0039::{Count, Language, Mnemonic};
    use ed25519_dalek_bip32::{DerivationPath, ExtendedSecretKey};
    use ruc::*;
    use zei::{
        serialization::ZeiFromToBytes,
        xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey},
    };

    /// Randomly generate a 12words-length mnemonic.
    #[inline(always)]
    pub fn generate_mnemonic_default() -> String {
        Mnemonic::generate_in(Language::English, Count::Words12).into_phrase()
    }

    /// Generate mnemonic with custom length and language.
    /// - @param `wordslen`: acceptable value are one of [ 12, 15, 18, 21, 24 ]
    /// - @param `lang`: acceptable value are one of [ "en", "zh", "zh_traditional", "fr", "it", "ko", "sp", "jp" ]
    #[inline(always)]
    pub fn generate_mnemonic_custom(wordslen: u8, lang: &str) -> Result<String> {
        let w = match wordslen {
            12 => Count::Words12,
            15 => Count::Words15,
            18 => Count::Words18,
            21 => Count::Words21,
            24 => Count::Words24,
            _ => {
                return Err(eg!(
                    "Invalid words length, only 12/15/18/21/24 can be accepted."
                ));
            }
        };

        let l = check_lang(lang).c(d!())?;

        Ok(Mnemonic::generate_in(l, w).into_phrase())
    }

    // do the real restore operation.
    macro_rules! restore_keypair_from_mnemonic {
        ($phrase: expr, $l: expr, $p: expr, $bip: tt) => {
            check_lang($l)
                .c(d!())
                .and_then(|l| Mnemonic::from_phrase_in(l, $phrase).map_err(|e| eg!(e)))
                .map(|m| m.to_seed(""))
                .and_then(|seed| {
                    DerivationPath::$bip($p.coin, $p.account, $p.change, $p.address)
                        .map_err(|e| eg!(e))
                        .map(|dp| (seed, dp))
                })
                .and_then(|(seed, dp)| {
                    ExtendedSecretKey::from_seed(&seed)
                        .map_err(|e| eg!(e))?
                        .derive(&dp)
                        .map_err(|e| eg!(e))
                })
                .and_then(|kp| {
                    XfrSecretKey::zei_from_bytes(&kp.secret_key.to_bytes()[..])
                        .map_err(|e| eg!(e))
                })
                .map(|sk| sk.into_keypair())
        };
    }

    /// Use this struct to express a Bip44/Bip49 path.
    pub struct BipPath {
        coin: u32,
        account: u32,
        change: u32,
        address: u32,
    }

    impl BipPath {
        #[inline(always)]
        pub fn new(coin: u32, account: u32, change: u32, address: u32) -> Self {
            BipPath {
                coin,
                account,
                change,
                address,
            }
        }
    }

    /// Restore the XfrKeyPair from a mnemonic with a default bip44-path,
    /// that is "m/44'/917'/0'/0/0" ("m/44'/coin'/account'/change/address").
    pub fn restore_keypair_from_mnemonic_default(phrase: &str) -> Result<XfrKeyPair> {
        const FRA: u32 = 917;
        restore_keypair_from_mnemonic!(phrase, "en", BipPath::new(FRA, 0, 0, 0), bip44)
            .c(d!())
    }

    /// Restore the XfrKeyPair from a mnemonic with custom params,
    /// in bip44 form.
    #[inline(always)]
    pub fn restore_keypair_from_mnemonic_bip44(
        phrase: &str,
        lang: &str,
        path: &BipPath,
    ) -> Result<XfrKeyPair> {
        restore_keypair_from_mnemonic_bip44_inner(phrase, lang, path).c(d!())
    }

    #[inline(always)]
    fn restore_keypair_from_mnemonic_bip44_inner(
        phrase: &str,
        lang: &str,
        path: &BipPath,
    ) -> Result<XfrKeyPair> {
        restore_keypair_from_mnemonic!(phrase, lang, path, bip44).c(d!())
    }

    /// Restore the XfrKeyPair from a mnemonic with custom params,
    /// in bip49 form.
    #[inline(always)]
    pub fn restore_keypair_from_mnemonic_bip49(
        phrase: &str,
        lang: &str,
        path: &BipPath,
    ) -> Result<XfrKeyPair> {
        restore_keypair_from_mnemonic_bip49_inner(phrase, lang, path).c(d!())
    }

    #[inline(always)]
    fn restore_keypair_from_mnemonic_bip49_inner(
        phrase: &str,
        lang: &str,
        path: &BipPath,
    ) -> Result<XfrKeyPair> {
        restore_keypair_from_mnemonic!(phrase, lang, path, bip49).c(d!())
    }

    // check and generate a Language object from its string value.
    #[inline(always)]
    fn check_lang(lang: &str) -> Result<Language> {
        match lang {
            "en" => Ok(Language::English),
            "zh" => Ok(Language::SimplifiedChinese),
            "zh_traditional" => Ok(Language::TraditionalChinese),
            "fr" => Ok(Language::French),
            "it" => Ok(Language::Italian),
            "ko" => Ok(Language::Korean),
            "sp" => Ok(Language::Spanish),
            "jp" => Ok(Language::Japanese),
            _ => Err(eg!("Unsupported language")),
        }
    }

    /////////////////////////////////////////////////////////////////

    #[inline(always)]
    pub fn public_key_to_base64(key: &XfrPublicKey) -> String {
        base64::encode_config(&ZeiFromToBytes::zei_to_bytes(key), base64::URL_SAFE)
    }

    #[inline(always)]
    pub fn public_key_from_base64(pk: &str) -> Result<XfrPublicKey> {
        base64::decode_config(pk, base64::URL_SAFE)
            .c(d!())
            .and_then(|bytes| XfrPublicKey::zei_from_bytes(&bytes).c(d!()))
    }

    #[inline(always)]
    pub fn public_key_to_bech32(key: &XfrPublicKey) -> String {
        bech32enc(&XfrPublicKey::zei_to_bytes(key))
    }

    #[inline(always)]
    pub fn public_key_from_bech32(addr: &str) -> Result<XfrPublicKey> {
        bech32dec(addr)
            .c(d!())
            .and_then(|bytes| XfrPublicKey::zei_from_bytes(&bytes).c(d!()))
    }

    #[inline(always)]
    fn bech32enc<T: AsRef<[u8]> + ToBase32>(input: &T) -> String {
        bech32::encode("fra", input.to_base32()).unwrap()
    }

    #[inline(always)]
    fn bech32dec(input: &str) -> Result<Vec<u8>> {
        bech32::decode(input)
            .c(d!())
            .and_then(|(_, data)| Vec::<u8>::from_base32(&data).c(d!()))
    }

    /////////////////////////////////////////////////////////////////

    #[cfg(test)]
    mod test {
        use super::*;
        use rand_core::SeedableRng;

        #[test]
        fn t_generate_mnemonic() {
            ["en", "zh", "zh_traditional", "fr", "it", "ko", "sp", "jp"]
                .iter()
                .for_each(|lang| {
                    [12, 15, 18, 21, 24].iter().for_each(|wordslen| {
                        let phrase = generate_mnemonic_custom(*wordslen, lang).unwrap();
                        let path = BipPath {
                            coin: 917,
                            account: rand::random::<u32>() % 100,
                            change: rand::random::<u32>() % 100,
                            address: rand::random::<u32>() % 100,
                        };
                        assert_eq!(*wordslen as usize, phrase.split(' ').count());

                        pnk!(restore_keypair_from_mnemonic_bip44_inner(
                            &phrase, lang, &path
                        ));
                        pnk!(restore_keypair_from_mnemonic_bip49_inner(
                            &phrase, lang, &path
                        ));
                    })
                });
        }

        #[test]
        fn t_generate_mnemonic_bad() {
            assert!(generate_mnemonic_custom(12, "xx").is_err());
            assert!(generate_mnemonic_custom(11, "zh").is_err());
            assert!(generate_mnemonic_custom(11, "xx").is_err());
        }

        fn new_keypair() -> XfrKeyPair {
            let mut small_rng = rand_chacha::ChaChaRng::from_entropy();
            XfrKeyPair::generate(&mut small_rng)
        }

        #[test]
        fn t_converts() {
            let pk = new_keypair().get_pk();
            assert_eq!(pk, pnk!(public_key_from_base64(&public_key_to_base64(&pk))));
            assert_eq!(pk, pnk!(public_key_from_bech32(&public_key_to_bech32(&pk))));
        }
    }
}
