use bech32::{FromBase32, ToBase32};
use core::convert::TryFrom;
use core::fmt::Formatter;
use core::str::FromStr;
use fp_utils::{ecdsa, hashing::keccak_256};
use globutils::wallet;
use hex::FromHex;
use primitive_types::{H160, H256};
use ruc::{d, eg, RucResult};
use serde::{Deserialize, Serialize};
use sha3::{Digest, Keccak256};
use std::fmt::Display;
use std::ops::{Deref, DerefMut};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::{XfrPublicKey, XfrSignature};

/// An opaque 32-byte cryptographic identifier.
#[derive(
    Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash, Serialize, Deserialize, Debug,
)]
pub struct Address32([u8; 32]);

impl AsRef<[u8]> for Address32 {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl AsMut<[u8]> for Address32 {
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.0[..]
    }
}

impl AsRef<[u8; 32]> for Address32 {
    fn as_ref(&self) -> &[u8; 32] {
        &self.0
    }
}

impl AsMut<[u8; 32]> for Address32 {
    fn as_mut(&mut self) -> &mut [u8; 32] {
        &mut self.0
    }
}

impl From<[u8; 32]> for Address32 {
    fn from(x: [u8; 32]) -> Self {
        Self(x)
    }
}

impl core::fmt::Display for Address32 {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", bech32::encode("fra", self.to_base32()).unwrap())
    }
}

impl FromStr for Address32 {
    type Err = ();

    fn from_str(s: &str) -> Result<Address32, ()> {
        let v = bech32::decode(s)
            .and_then(|d| Vec::<u8>::from_base32(&d.1))
            .map_err(|_e| ())?;
        let mut address_32 = Address32::default();
        address_32.0.copy_from_slice(v.as_slice());
        Ok(address_32)
    }
}

impl<'a> TryFrom<&'a [u8]> for Address32 {
    type Error = ();
    fn try_from(x: &'a [u8]) -> Result<Address32, ()> {
        if x.len() == 32 {
            let mut r = Address32::default();
            r.0.copy_from_slice(x);
            Ok(r)
        } else {
            Err(())
        }
    }
}

impl From<XfrPublicKey> for Address32 {
    fn from(k: XfrPublicKey) -> Self {
        Address32::try_from(k.zei_to_bytes().as_slice()).unwrap()
    }
}

impl From<ecdsa::Public> for Address32 {
    fn from(k: ecdsa::Public) -> Self {
        keccak_256(k.as_ref()).into()
    }
}

impl From<H160> for Address32 {
    fn from(k: H160) -> Self {
        let mut data = [0u8; 32];
        data[0..4].copy_from_slice(b"evm:");
        data[4..24].copy_from_slice(k.as_bytes());
        data.into()
    }
}

/// A wrapper of the Hash type defined inf fixed-hash crate.
#[derive(
    Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash, Serialize, Deserialize, Debug,
)]
pub struct HA160(pub H160);

impl From<H160> for HA160 {
    fn from(h: H160) -> Self {
        Self(h)
    }
}

impl Deref for HA160 {
    type Target = H160;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for HA160 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Display for HA160 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hex::encode_upper(self.0))
    }
}

impl FromStr for HA160 {
    type Err = fixed_hash::rustc_hex::FromHexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let hash = H160::from_str(s)?;
        Ok(HA160(hash))
    }
}
/// A wrapper of the Hash type defined inf fixed-hash crate.
#[derive(
    Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash, Serialize, Deserialize, Debug,
)]
pub struct HA256(H256);

impl From<H256> for HA256 {
    fn from(e: H256) -> Self {
        Self(e)
    }
}

impl HA256 {
    pub fn new(hash: H256) -> HA256 {
        HA256(hash)
    }

    pub fn h256(&self) -> H256 {
        self.0
    }
}

impl Display for HA256 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hex::encode_upper(self.0))
    }
}

impl FromStr for HA256 {
    type Err = fixed_hash::rustc_hex::FromHexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let hash = H256::from_str(s)?;
        Ok(HA256(hash))
    }
}

/// Some type that is able to be collapsed into an account ID. It is not possible to recreate the
/// original value from the account ID.
pub trait IdentifyAccount {
    /// The account ID that this can be transformed into.
    type AccountId;
    /// Transform into an account.
    fn into_account(self) -> Self::AccountId;
}

/// Means of signature verification.
pub trait Verify {
    /// Type of the signer.
    type Signer: IdentifyAccount;
    /// Verify a signature.
    ///
    /// Return `true` if signature is valid for the value.
    fn verify(
        &self,
        msg: &[u8],
        signer: &<Self::Signer as IdentifyAccount>::AccountId,
    ) -> bool;
}

/// Signature verify that can work with any known signature types..
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum MultiSignature {
    /// An zei xfr signature.
    Xfr(XfrSignature),
    /// An ECDSA/SECP256k1 signature.
    Ecdsa(ecdsa::Signature),
}

impl From<XfrSignature> for MultiSignature {
    fn from(x: XfrSignature) -> Self {
        MultiSignature::Xfr(x)
    }
}

impl TryFrom<MultiSignature> for XfrSignature {
    type Error = ();
    fn try_from(m: MultiSignature) -> Result<Self, Self::Error> {
        if let MultiSignature::Xfr(x) = m {
            Ok(x)
        } else {
            Err(())
        }
    }
}

impl From<ecdsa::Signature> for MultiSignature {
    fn from(x: ecdsa::Signature) -> Self {
        MultiSignature::Ecdsa(x)
    }
}

impl TryFrom<MultiSignature> for ecdsa::Signature {
    type Error = ();
    fn try_from(m: MultiSignature) -> Result<Self, Self::Error> {
        if let MultiSignature::Ecdsa(x) = m {
            Ok(x)
        } else {
            Err(())
        }
    }
}

impl Verify for MultiSignature {
    type Signer = MultiSigner;

    fn verify(&self, msg: &[u8], signer: &Address32) -> bool {
        match self {
            Self::Xfr(ref sig) => match XfrPublicKey::zei_from_bytes(signer.as_ref()) {
                Ok(who) => sig.verify(msg, &who),
                _ => false,
            },
            // Self::Ecdsa(ref sig) => match sig.recover(msg) {
            //     Some(pubkey) => {
            //         &keccak_256(pubkey.as_ref())
            //             == <dyn AsRef<[u8; 32]>>::as_ref(signer)
            //     }
            //     _ => false,
            // },
            Self::Ecdsa(ref sig) => {
                // let mut msg_hashed = [0u8; 32];
                // msg_hashed.copy_from_slice(msg);

                let msg_hashed = keccak_256(msg);
                match secp256k1_ecdsa_recover(sig.as_ref(), &msg_hashed) {
                    Ok(pubkey) => {
                        Address32::from(H160::from(H256::from_slice(
                            Keccak256::digest(&pubkey).as_slice(),
                        ))) == signer.clone()
                    }
                    _ => false,
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum MultiSigner {
    /// An zei xfr identity.
    Xfr(XfrPublicKey),
    // /// An SECP256k1/ECDSA identity (actually, the keccak 256 hash of the compressed pub key).
    // Ecdsa(ecdsa::Public),
    /// An Ethereum address identity.
    Ethereum(H160),
}

impl Default for MultiSigner {
    fn default() -> Self {
        Self::Xfr(Default::default())
    }
}

impl FromStr for MultiSigner {
    type Err = Box<dyn ruc::RucError>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 42 && &s[..2] == "0x" {
            // Ethereum address
            let address_hex = &s[2..];
            let inner = <[u8; 20]>::from_hex(address_hex).c(d!())?;
            Ok(MultiSigner::Ethereum(H160(inner)))
        } else if let Ok(address) = wallet::public_key_from_base64(s) {
            Ok(MultiSigner::Xfr(address))
        } else {
            let address = wallet::public_key_from_bech32(s)?;
            Ok(MultiSigner::Xfr(address))
        }
    }
}

impl From<MultiSigner> for Address32 {
    fn from(m: MultiSigner) -> Self {
        match m {
            MultiSigner::Ethereum(a) => a.into(),
            MultiSigner::Xfr(a) => a.into(),
        }
    }
}

impl From<XfrPublicKey> for MultiSigner {
    fn from(x: XfrPublicKey) -> Self {
        Self::Xfr(x)
    }
}

impl TryFrom<MultiSigner> for XfrPublicKey {
    type Error = ();
    fn try_from(m: MultiSigner) -> Result<Self, Self::Error> {
        if let MultiSigner::Xfr(x) = m {
            Ok(x)
        } else {
            Err(())
        }
    }
}

// impl From<ecdsa::Public> for MultiSigner {
//     fn from(x: ecdsa::Public) -> Self {
//         Self::Ecdsa(x)
//     }
// }
//
// impl TryFrom<MultiSigner> for ecdsa::Public {
//     type Error = ();
//     fn try_from(m: MultiSigner) -> Result<Self, Self::Error> {
//         if let MultiSigner::Ecdsa(x) = m {
//             Ok(x)
//         } else {
//             Err(())
//         }
//     }
// }

impl From<H160> for MultiSigner {
    fn from(x: H160) -> Self {
        Self::Ethereum(x)
    }
}

impl TryFrom<MultiSigner> for H160 {
    type Error = ();
    fn try_from(m: MultiSigner) -> Result<Self, Self::Error> {
        if let MultiSigner::Ethereum(x) = m {
            Ok(x)
        } else {
            Err(())
        }
    }
}

impl IdentifyAccount for XfrPublicKey {
    type AccountId = Self;
    fn into_account(self) -> Self {
        self
    }
}

impl IdentifyAccount for MultiSigner {
    type AccountId = Address32;
    fn into_account(self) -> Address32 {
        match self {
            MultiSigner::Xfr(who) => who.into(),
            // MultiSigner::Ecdsa(who) => who.into(),
            MultiSigner::Ethereum(who) => who.into(),
        }
    }
}

impl Verify for XfrSignature {
    type Signer = XfrPublicKey;

    fn verify(&self, msg: &[u8], signer: &XfrPublicKey) -> bool {
        signer.verify(msg, self).is_ok()
    }
}

/// Verify and recover a SECP256k1 ECDSA signature.
///
/// - `sig` is passed in RSV format. V should be either `0/1` or `27/28`.
/// - `msg` is the keccak-256 hash of the message.
///
/// Returns `Err` if the signature is bad, otherwise the 64-byte pubkey
/// (doesn't include the 0x04 prefix).
pub fn secp256k1_ecdsa_recover(sig: &[u8; 65], msg: &[u8; 32]) -> ruc::Result<[u8; 64]> {
    let rs = libsecp256k1::Signature::parse_standard_slice(&sig[0..64])
        .map_err(|_| eg!("Ecdsa signature verify error: bad RS"))?;
    let v = libsecp256k1::RecoveryId::parse(if sig[64] > 26 {
        sig[64] - 27
    } else {
        sig[64]
    })
    .map_err(|_| eg!("Ecdsa signature verify error: bad V"))?;
    let pubkey = libsecp256k1::recover(&libsecp256k1::Message::parse(msg), &rs, &v)
        .map_err(|_| eg!("Ecdsa signature verify error: bad signature"))?;
    let mut res = [0u8; 64];
    res.copy_from_slice(&pubkey.serialize()[1..65]);
    Ok(res)
}

/// Alias to 512-bit hash when used in the context of a transaction signature on the chain.
pub type Signature = MultiSignature;

/// Some way of identifying an account on the chain. We intentionally make it equivalent
/// to the public key of our transaction signing scheme.
pub type Address = <<Signature as Verify>::Signer as IdentifyAccount>::AccountId;

#[cfg(test)]
mod tests {
    use super::*;
    use rand_chacha::rand_core::SeedableRng;
    use rand_chacha::ChaChaRng;
    use zei::xfr::sig::XfrKeyPair;

    #[test]
    fn xfr_sign_verify_work() {
        let mut prng = ChaChaRng::from_entropy();
        let alice = XfrKeyPair::generate(&mut prng);
        let sig = alice.get_sk_ref().sign(b"hello", alice.get_pk_ref());
        let signer = MultiSigner::from(alice.get_pk());
        let sig = MultiSignature::from(sig);
        assert!(
            sig.verify(b"hello", &signer.into_account()),
            "xfr signature verify failed"
        );
    }

    #[test]
    fn ecdsa_sign_verify_work() {
        let (alice, _) = ecdsa::SecpPair::generate();
        let sig = alice.sign(b"hello");
        let signer = MultiSigner::from(alice.address());
        let sig = MultiSignature::from(sig);
        assert!(
            sig.verify(b"hello", &signer.into_account()),
            "ecdsa signature verify failed"
        );
    }

    #[test]
    fn test_address_32() {
        let mut prng = ChaChaRng::from_entropy();
        let keypair = XfrKeyPair::generate(&mut prng);
        let pubkey = keypair.pub_key;

        let address_32 = Address32::from(pubkey);
        let address_str = address_32.to_string();

        let new_address = Address32::from_str(address_str.as_str());
        let new_address_str = new_address.unwrap().to_string();

        assert_eq!(new_address_str, address_str);
    }
}
