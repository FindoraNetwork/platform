use crate::hashing::keccak_256;
use bip0039::{Count, Language, Mnemonic};
use bip32::{DerivationPath, XPrv};
use core::cmp::Ordering;
use core::convert::{TryFrom, TryInto};
use core::hash::{Hash, Hasher};
use libsecp256k1::{PublicKey, SecretKey};
use primitive_types::{H160, H256};
use rand::{rngs::OsRng, RngCore};
use ruc::eg;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use sha3::{Digest, Keccak256};
use std::str::FromStr;

/// A secret seed (which is bytewise essentially equivalent to a SecretKey).
///
/// We need it as a different type because `Seed` is expected to be AsRef<[u8]>.
type Seed = [u8; 32];

/// The ECDSA compressed public key.
#[derive(Clone, Copy)]
pub struct Public(pub [u8; 33]);

impl PartialOrd for Public {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Public {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl PartialEq for Public {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for Public {}

impl Default for Public {
    fn default() -> Self {
        Public([0u8; 33])
    }
}

impl std::fmt::Debug for Public {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            base64::encode_config(self.as_ref(), base64::URL_SAFE)
        )
    }
}

impl Serialize for Public {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&base64::encode_config(self.as_ref(), base64::URL_SAFE))
    }
}

impl<'de> Deserialize<'de> for Public {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let pk =
            base64::decode_config(String::deserialize(deserializer)?, base64::URL_SAFE)
                .map_err(|e| de::Error::custom(format!("{e:?}",)))?;
        Public::try_from(pk.as_slice()).map_err(|e| de::Error::custom(format!("{e:?}",)))
    }
}

impl Public {
    /// A new instance from the given 33-byte `data`.
    ///
    /// NOTE: No checking goes on to ensure this is a real public key. Only use it if
    /// you are certain that the array actually is a pubkey. GIGO!
    pub fn from_raw(data: [u8; 33]) -> Self {
        Self(data)
    }

    /// A new instance from the given slice that should be 33 bytes long.
    ///
    /// NOTE: No checking goes on to ensure this is a real public key. Only use it if
    /// you are certain that the array actually is a pubkey. GIGO!
    pub fn from_slice(data: &[u8]) -> Self {
        let mut r = [0u8; 33];
        r.copy_from_slice(data);
        Self(r)
    }

    /// Create a new instance from the given full public key.
    ///
    /// This will convert the full public key into the compressed format.
    pub fn from_full(full: &[u8]) -> ruc::Result<Self> {
        libsecp256k1::PublicKey::parse_slice(full, None)
            .map(|k| k.serialize_compressed())
            .map(Self)
            .map_err(|_| eg!("invalid public key"))
    }
}

impl AsRef<[u8]> for Public {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl AsMut<[u8]> for Public {
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.0[..]
    }
}

impl TryFrom<&[u8]> for Public {
    type Error = ();

    fn try_from(data: &[u8]) -> Result<Self, Self::Error> {
        if data.len() == 33 {
            let mut r = [0u8; 33];
            r.copy_from_slice(data);
            Ok(Self(r))
        } else {
            Err(())
        }
    }
}

impl From<SecpPair> for Public {
    fn from(x: SecpPair) -> Self {
        x.public()
    }
}

/// A signature (a 512-bit value, plus 8 bits for recovery ID).
#[derive(Clone, Copy)]
pub struct Signature(pub [u8; 65]);

impl TryFrom<&[u8]> for Signature {
    type Error = ();

    fn try_from(data: &[u8]) -> Result<Self, Self::Error> {
        if data.len() == 65 {
            let mut inner = [0u8; 65];
            inner.copy_from_slice(data);
            Ok(Signature(inner))
        } else {
            Err(())
        }
    }
}

impl Serialize for Signature {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&hex::encode(self))
    }
}

impl<'de> Deserialize<'de> for Signature {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let signature_hex = hex::decode(String::deserialize(deserializer)?)
            .map_err(|e| de::Error::custom(format!("{e:?}",)))?;
        Signature::try_from(signature_hex.as_ref())
            .map_err(|e| de::Error::custom(format!("{e:?}",)))
    }
}

impl Default for Signature {
    fn default() -> Self {
        Signature([0u8; 65])
    }
}

impl PartialEq for Signature {
    fn eq(&self, b: &Self) -> bool {
        self.0[..] == b.0[..]
    }
}

impl Eq for Signature {}

impl std::fmt::Debug for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", hex::encode(self))
    }
}

impl From<Signature> for [u8; 65] {
    fn from(v: Signature) -> [u8; 65] {
        v.0
    }
}

impl AsRef<[u8; 65]> for Signature {
    fn as_ref(&self) -> &[u8; 65] {
        &self.0
    }
}

impl AsRef<[u8]> for Signature {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl AsMut<[u8]> for Signature {
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.0[..]
    }
}

impl Hash for Signature {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.0[..], state);
    }
}

impl Signature {
    /// A new instance from the given 65-byte `data`.
    ///
    /// NOTE: No checking goes on to ensure this is a real signature. Only use it if
    /// you are certain that the array actually is a signature. GIGO!
    pub fn from_raw(data: [u8; 65]) -> Signature {
        Signature(data)
    }

    /// A new instance from the given slice that should be 65 bytes long.
    ///
    /// NOTE: No checking goes on to ensure this is a real signature. Only use it if
    /// you are certain that the array actually is a signature. GIGO!
    pub fn from_slice(data: &[u8]) -> Self {
        let mut r = [0u8; 65];
        r.copy_from_slice(data);
        Signature(r)
    }

    /// Recover the public key from this signature and a message.
    pub fn recover<M: AsRef<[u8]>>(&self, message: M) -> Option<Public> {
        let message = libsecp256k1::Message::parse(&keccak_256(message.as_ref()));
        let sig: (_, _) = self.try_into().ok()?;
        libsecp256k1::recover(&message, &sig.0, &sig.1)
            .ok()
            .map(|recovered| Public(recovered.serialize_compressed()))
    }
}

impl From<(libsecp256k1::Signature, libsecp256k1::RecoveryId)> for Signature {
    fn from(x: (libsecp256k1::Signature, libsecp256k1::RecoveryId)) -> Signature {
        let mut r = Self::default();
        r.0[0..64].copy_from_slice(&x.0.serialize()[..]);
        r.0[64] = x.1.serialize();
        r
    }
}

impl<'a> TryFrom<&'a Signature> for (libsecp256k1::Signature, libsecp256k1::RecoveryId) {
    type Error = ();
    fn try_from(
        x: &'a Signature,
    ) -> core::result::Result<
        (libsecp256k1::Signature, libsecp256k1::RecoveryId),
        Self::Error,
    > {
        Ok((
            libsecp256k1::Signature::parse_standard_slice(&x.0[0..64])
                .expect("hardcoded to 64 bytes; qed"),
            libsecp256k1::RecoveryId::parse(x.0[64]).map_err(|_| ())?,
        ))
    }
}

/// A secp256k1 key pair.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct SecpPair {
    public: PublicKey,
    secret: SecretKey,
}

impl SecpPair {
    /// Get the seed for this key.
    pub fn seed(&self) -> Seed {
        self.secret.serialize()
    }

    pub fn generate() -> (Self, Seed) {
        let mut seed = Seed::default();
        OsRng.fill_bytes(seed.as_mut());
        (Self::from_seed(&seed), seed)
    }

    /// Generate new secure (random) key pair and provide the recovery phrase.
    ///
    /// You can recover the same key later with `from_phrase`.
    pub fn generate_with_phrase(password: Option<&str>) -> (SecpPair, String, Seed) {
        let mnemonic = Mnemonic::generate_in(Language::English, Count::Words12);
        let phrase = mnemonic.phrase();
        let (pair, seed) = Self::from_phrase(phrase, password)
            .expect("All phrases generated by Mnemonic are valid; qed");
        (pair, phrase.to_owned(), seed)
    }

    /// Generate key pair from given recovery phrase and password.
    pub fn from_phrase(
        phrase: &str,
        password: Option<&str>,
    ) -> ruc::Result<(SecpPair, Seed)> {
        let mnemonic = Mnemonic::from_phrase_in(Language::English, phrase)
            .map_err(|_| eg!("InvalidPhrase"))?;
        let bs = mnemonic.to_seed(password.unwrap_or(""));
        let ext = XPrv::derive_from_path(
            bs,
            &DerivationPath::from_str("m/44'/60'/0'/0/0")
                .map_err(|_| eg!("InvalidDerivationPath"))?,
        )
        .map_err(|_| eg!("Failed to ExtendedPrivateKey"))?;
        let mut seed = Seed::default();
        seed.copy_from_slice(&bs[0..32]);
        Self::from_seed_slice(&ext.to_bytes()).map(|x| (x, seed))
    }

    /// Make a new key pair from secret seed material.
    ///
    /// You should never need to use this; generate(), generate_with_phrase
    pub fn from_seed(seed: &Seed) -> SecpPair {
        Self::from_seed_slice(&seed[..]).expect("seed has valid length; qed")
    }

    /// Make a new key pair from secret seed material. The slice must be 32 bytes long or it
    /// will return `None`.
    ///
    /// You should never need to use this; generate(), generate_with_phrase
    pub fn from_seed_slice(seed_slice: &[u8]) -> ruc::Result<SecpPair> {
        let secret =
            SecretKey::parse_slice(seed_slice).map_err(|_| eg!("InvalidSeedLength"))?;
        let public = PublicKey::from_secret_key(&secret);
        Ok(SecpPair { public, secret })
    }

    /// Get the public key.
    pub fn public(&self) -> Public {
        Public(self.public.serialize_compressed())
    }

    /// Ethereum address format.
    pub fn address(&self) -> H160 {
        let mut res = [0u8; 64];
        res.copy_from_slice(&self.public.serialize()[1..65]);
        H160::from(H256::from_slice(Keccak256::digest(res).as_slice()))
    }

    /// Sign a message.
    pub fn sign(&self, message: &[u8]) -> Signature {
        let message = libsecp256k1::Message::parse(&keccak_256(message));
        libsecp256k1::sign(&message, &self.secret).into()
    }

    /// Sign a pre-hashed message
    pub fn sign_prehashed(&self, message: &[u8; 32]) -> Signature {
        let message = libsecp256k1::Message::parse(message);
        libsecp256k1::sign(&message, &self.secret).into()
    }

    /// Verify a signature on a message. Returns true if the signature is good.
    pub fn verify<M: AsRef<[u8]>>(sig: &Signature, message: M, pubkey: &Public) -> bool {
        let message = libsecp256k1::Message::parse(&keccak_256(message.as_ref()));
        let sig: (_, _) = match sig.try_into() {
            Ok(x) => x,
            _ => return false,
        };
        match libsecp256k1::recover(&message, &sig.0, &sig.1) {
            Ok(actual) => pubkey.0[..] == actual.serialize_compressed()[..],
            _ => false,
        }
    }

    /// Return a vec filled with raw data.
    pub fn to_raw_vec(self) -> Vec<u8> {
        self.seed().to_vec()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use hex_literal::hex;

    #[test]
    fn test_vector_should_work() {
        let pair = SecpPair::from_seed(&hex!(
            "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        ));
        let public = pair.public();
        assert_eq!(
            public,
            Public::from_full(
                &hex!("8db55b05db86c0b1786ca49f095d76344c9e6056b2f02701a7e7f3c20aabfd913ebbe148dd17c56551a52952371071a6c604b3f3abe8f2c8fa742158ea6dd7d4")[..],
            ).unwrap(),
        );
        let message = b"";
        let signature = hex!(
            "4e1fd58a98bbce5fe948c4e5fec7662d253130a300156c037429dca66f9f6a0728e8b5e8bc55f4bcf445af4b75928a876d54949aaee93a62e3eb1cf12aefb60800"
        );
        let signature = Signature::from_raw(signature);
        assert_eq!(pair.sign(&message[..]), signature);
        assert!(SecpPair::verify(&signature, &message[..], &public));
    }

    #[test]
    fn generated_pair_should_work() {
        let (pair, _) = SecpPair::generate();
        let public = pair.public();
        let message = b"Something important";
        let signature = pair.sign(&message[..]);
        assert!(SecpPair::verify(&signature, &message[..], &public));
        assert!(!SecpPair::verify(&signature, b"Something else", &public));
    }

    #[test]
    fn seeded_pair_should_work() {
        let pair = SecpPair::from_seed(b"12345678901234567890123456789012");
        let public = pair.public();
        assert_eq!(
            public,
            Public::from_full(
                &hex!("5676109c54b9a16d271abeb4954316a40a32bcce023ac14c8e26e958aa68fba995840f3de562156558efbfdac3f16af0065e5f66795f4dd8262a228ef8c6d813")[..],
            ).unwrap(),
        );
        let message = hex!(
            "2f8c6129d816cf51c374bc7f08c3e63ed156cf78aefb4a6550d97b87997977ee00000000000000000200d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a4500000000000000"
        );
        let signature = pair.sign(&message[..]);
        println!("Correct signature: {signature:?}",);
        assert!(SecpPair::verify(&signature, &message[..], &public));
        assert!(!SecpPair::verify(&signature, "Other message", &public));
    }

    #[test]
    fn generate_with_phrase_recovery_possible() {
        let (pair1, phrase, _) = SecpPair::generate_with_phrase(None);
        let (pair2, _) = SecpPair::from_phrase(&phrase, None).unwrap();

        assert_eq!(pair1.public(), pair2.public());
    }

    #[test]
    fn generate_with_password_phrase_recovery_possible() {
        let (pair1, phrase, _) = SecpPair::generate_with_phrase(Some("password"));
        let (pair2, _) = SecpPair::from_phrase(&phrase, Some("password")).unwrap();

        assert_eq!(pair1.public(), pair2.public());
    }

    #[test]
    fn password_does_something() {
        let (pair1, phrase, _) = SecpPair::generate_with_phrase(Some("password"));
        let (pair2, _) = SecpPair::from_phrase(&phrase, None).unwrap();

        assert_ne!(pair1.public(), pair2.public());
    }

    #[test]
    fn public_serialization_works() {
        let pair = SecpPair::from_seed(b"12345678901234567890123456789012");
        let pk = Public::from(pair);
        let serialized_public = serde_json::to_string(&pk).unwrap();
        let public = serde_json::from_str::<Public>(&serialized_public).unwrap();
        assert!(public.eq(&pk));
    }

    #[test]
    fn signature_serialization_works() {
        let pair = SecpPair::from_seed(b"12345678901234567890123456789012");
        let message = b"Something important";
        let signature = pair.sign(&message[..]);
        let serialized_signature = serde_json::to_string(&signature).unwrap();
        // Signature is 65 bytes, so 130 chars + 2 quote chars
        assert_eq!(serialized_signature.len(), 132);
        let signature = serde_json::from_str(&serialized_signature).unwrap();
        assert!(SecpPair::verify(&signature, &message[..], &pair.public()));
    }

    #[test]
    fn signature_serialization_doesnt_panic() {
        fn deserialize_signature(
            text: &str,
        ) -> Result<Signature, serde_json::error::Error> {
            serde_json::from_str(text)
        }
        assert!(deserialize_signature("Not valid json.").is_err());
        assert!(deserialize_signature("\"Not an actual signature.\"").is_err());
        // Poorly-sized
        assert!(deserialize_signature("\"abc123\"").is_err());
    }

    #[test]
    fn sign_prehashed_works() {
        let (pair, _, _) = SecpPair::generate_with_phrase(Some("password"));

        // `msg` shouldn't be mangled
        let msg = [0u8; 32];
        let sig1 = pair.sign_prehashed(&msg);
        let sig2: Signature =
            libsecp256k1::sign(&libsecp256k1::Message::parse(&msg), &pair.secret).into();

        assert_eq!(sig1, sig2);

        // signature is actually different
        let sig2 = pair.sign(&msg);

        assert_ne!(sig1, sig2);

        // using pre-hashed `msg` works
        let msg = keccak_256(b"this should be hashed");
        let sig1 = pair.sign_prehashed(&msg);
        let sig2: Signature =
            libsecp256k1::sign(&libsecp256k1::Message::parse(&msg), &pair.secret).into();

        assert_eq!(sig1, sig2);
    }
}
