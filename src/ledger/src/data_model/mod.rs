//!
//! An implementation of findora ledger data model
//!

#![allow(clippy::field_reassign_with_default)]
#![allow(clippy::assertions_on_constants)]

mod __trash__;
mod effects;
mod test;

pub use effects::{BlockEffect, TxnEffect};

use crate::staking::ops::replace_staker::ReplaceStakerOps;

use {
    crate::converter::ConvertAccount,
    crate::staking::{
        ops::{
            claim::ClaimOps, delegation::DelegationOps,
            fra_distribution::FraDistributionOps, governance::GovernanceOps,
            mint_fra::MintFraOps, undelegation::UnDelegationOps,
            update_staker::UpdateStakerOps, update_validator::UpdateValidatorOps,
        },
        Staking,
    },
    __trash__::{Policy, PolicyGlobals, TxnPolicyData},
    bitmap::SparseMap,
    cryptohash::{sha256::Digest as BitDigest, HashValue},
    fbnc::NumKey,
    globutils::{HashOf, ProofOf, Serialized, SignatureOf},
    lazy_static::lazy_static,
    rand::Rng,
    rand_chacha::{rand_core, ChaChaRng},
    rand_core::{CryptoRng, RngCore, SeedableRng},
    ruc::*,
    serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer},
    std::{
        collections::{HashMap, HashSet},
        convert::TryFrom,
        fmt,
        hash::{Hash, Hasher},
        mem,
        ops::Deref,
        result::Result as StdResult,
    },
    unicode_normalization::UnicodeNormalization,
    zei::{
        serialization::ZeiFromToBytes,
        xfr::{
            lib::{gen_xfr_body, XfrNotePolicies},
            sig::{XfrKeyPair, XfrPublicKey},
            structs::{
                AssetRecord, AssetType as ZeiAssetType, BlindAssetRecord, OwnerMemo,
                TracingPolicies, TracingPolicy, XfrAmount, XfrAssetType, XfrBody,
                ASSET_TYPE_LENGTH,
            },
        },
    },
};

const RANDOM_CODE_LENGTH: usize = 16;
const MAX_DECIMALS_LENGTH: u8 = 19;

#[inline(always)]
fn b64enc<T: ?Sized + AsRef<[u8]>>(input: &T) -> String {
    base64::encode_config(input, base64::URL_SAFE)
}

#[inline(always)]
#[allow(missing_docs)]
pub fn b64dec<T: ?Sized + AsRef<[u8]>>(input: &T) -> Result<Vec<u8>> {
    base64::decode_config(input, base64::URL_SAFE).c(d!())
}

/// Unique Identifier for ledger objects
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct Code {
    /// 16 bytes
    pub val: [u8; 16],
}

#[inline(always)]
fn is_default<T: Default + PartialEq>(x: &T) -> bool {
    x == &T::default()
}

#[derive(
    Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize,
)]
/// Findora asset type code
pub struct AssetTypeCode {
    /// Internal asset type
    pub val: ZeiAssetType,
}

impl NumKey for AssetTypeCode {
    fn to_bytes(&self) -> Vec<u8> {
        self.val.0.to_vec()
    }
    fn from_bytes(b: &[u8]) -> Result<Self> {
        let mut b = b.to_owned();
        b.resize(ASSET_TYPE_LENGTH, 0u8);
        Ok(Self {
            val: ZeiAssetType(
                <[u8; ASSET_TYPE_LENGTH]>::try_from(b.as_slice()).c(d!())?,
            ),
        })
    }
}

// The code of FRA is [0;  ASSET_TYPE_LENGTH],
// exactly equal to the derived `default value`,
// so we implement a custom `Default` for it.
impl Default for AssetTypeCode {
    #[inline(always)]
    fn default() -> Self {
        AssetTypeCode {
            val: ZeiAssetType([255; ASSET_TYPE_LENGTH]),
        }
    }
}

impl AssetTypeCode {
    /// Randomly generates a 16-byte data and encodes it with base64 to a utf8 string.
    ///
    /// The utf8 can then be converted to an asset type code using `new_from_utf8_safe` or `new_from_utf8_truncate`.
    #[inline(always)]
    pub fn gen_utf8_random() -> String {
        let mut rng = ChaChaRng::from_entropy();
        let mut buf: [u8; RANDOM_CODE_LENGTH] = [0u8; RANDOM_CODE_LENGTH];
        rng.fill_bytes(&mut buf);
        b64enc(&buf)
    }

    /// Randomly generates an asset type code
    #[inline(always)]
    pub fn gen_random() -> Self {
        Self::gen_random_with_rng(&mut ChaChaRng::from_entropy())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn gen_random_with_rng<R: RngCore + CryptoRng>(prng: &mut R) -> Self {
        let val: [u8; ASSET_TYPE_LENGTH] = prng.gen();
        Self {
            val: ZeiAssetType(val),
        }
    }

    /// Returns whether the input is longer than 32 bytes, and thus will be truncated to construct an asset type code.
    #[inline(always)]
    pub fn will_truncate(bytes: &[u8]) -> bool {
        bytes.len() > ASSET_TYPE_LENGTH
    }

    /// Converts a vector to an asset type code.
    #[inline(always)]
    pub fn new_from_vec(mut bytes: Vec<u8>) -> Self {
        bytes.resize(ASSET_TYPE_LENGTH, 0u8);
        Self {
            val: ZeiAssetType(
                <[u8; ASSET_TYPE_LENGTH]>::try_from(bytes.as_slice()).unwrap(),
            ),
        }
    }

    /// Converts an utf8 string to an asset type code.
    ///
    /// Returns an error if the length is greater than 32 bytes.
    #[inline(always)]
    pub fn new_from_utf8_safe(s: &str) -> Result<Self> {
        let composed = s.to_string().nfc().collect::<String>().into_bytes();
        if AssetTypeCode::will_truncate(&composed) {
            return Err(eg!());
        }
        Ok(AssetTypeCode::new_from_vec(composed))
    }

    /// Converts an utf8 string to an asset type code.
    /// Truncates the code if the length is greater than 32 bytes.
    ///
    /// Used to customize the asset type code.
    #[inline(always)]
    pub fn new_from_utf8_truncate(s: &str) -> Self {
        let composed = s.to_string().nfc().collect::<String>().into_bytes();
        AssetTypeCode::new_from_vec(composed)
    }

    /// Converts the asset type code to an utf8 string.
    ///
    /// Used to display the asset type code.
    #[inline(always)]
    pub fn to_utf8(self) -> Result<String> {
        let mut code = self.val.0.to_vec();
        let len = code.len();
        // Find the last non-empty index
        for i in 1..len {
            if code[len - i] == 0 {
                continue;
            } else {
                code.truncate(len - i + 1);
                match std::str::from_utf8(&code) {
                    Ok(utf8_str) => {
                        return Ok(utf8_str.to_string());
                    }
                    Err(e) => {
                        return Err(eg!((e)));
                    }
                };
            }
        }
        Ok("".to_string())
    }

    /// Converts a string to an asset type code.
    /// Truncates the code if the length is greater than 32 bytes.
    ///
    /// Used to customize the asset type code.
    #[inline(always)]
    pub fn new_from_str(s: &str) -> Self {
        let mut as_vec = s.to_string().into_bytes();
        as_vec.resize(ASSET_TYPE_LENGTH, 0u8);
        let buf = <[u8; ASSET_TYPE_LENGTH]>::try_from(as_vec.as_slice()).unwrap();
        Self {
            val: ZeiAssetType(buf),
        }
    }

    /// Converts a base64-format string to an asset type code.
    /// Truncates the code if the length is greater than 32 bytes.
    ///
    /// Used to customize the asset type code.
    pub fn new_from_base64(b64: &str) -> Result<Self> {
        match b64dec(b64) {
            Ok(mut bin) => {
                bin.resize(ASSET_TYPE_LENGTH, 0u8);
                let buf = <[u8; ASSET_TYPE_LENGTH]>::try_from(bin.as_slice()).c(d!())?;
                Ok(Self {
                    val: ZeiAssetType(buf),
                })
            }
            Err(e) => Err(eg!((format!(
                "Failed to deserialize base64 '{}': {}",
                b64, e
            )))),
        }
    }

    /// Converts the asset type code to a base64 format string.
    ///
    /// Used to display the asset type code.
    #[inline(always)]
    pub fn to_base64(self) -> String {
        b64enc(&self.val.0)
    }

    // pub(crate) fn to_bytes(&self) -> Vec<u8> {
    //     self.val.0.to_vec()
    // }
}

impl Code {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn gen_random() -> Self {
        let mut small_rng = ChaChaRng::from_entropy();
        let mut buf: [u8; 16] = [0u8; 16];
        small_rng.fill_bytes(&mut buf);
        Self { val: buf }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn gen_random_with_rng<R: Rng>(mut prng: R) -> Self {
        let val: [u8; 16] = prng.gen();
        Self { val }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new_from_str(s: &str) -> Self {
        let mut as_vec = s.to_string().into_bytes();
        as_vec.resize(16, 0u8);
        let buf = <[u8; 16]>::try_from(as_vec.as_slice()).unwrap();
        Self { val: buf }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new_from_base64(b64: &str) -> Result<Self> {
        if let Ok(mut bin) = b64dec(b64) {
            bin.resize(16, 0u8);
            let buf = <[u8; 16]>::try_from(bin.as_slice()).c(d!())?;
            Ok(Self { val: buf })
        } else {
            Err(eg!())
        }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn to_base64(self) -> String {
        b64enc(&self.val)
    }

    // pub(crate) fn to_bytes(self) -> Vec<u8> {
    //     self.val.to_vec()
    // }
}

impl Serialize for Code {
    #[inline(always)]
    fn serialize<S>(&self, serializer: S) -> StdResult<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if serializer.is_human_readable() {
            serializer.serialize_str(&b64enc(&self.val))
        } else {
            serializer.serialize_bytes(&self.val)
        }
    }
}

impl<'de> Deserialize<'de> for Code {
    fn deserialize<D>(deserializer: D) -> StdResult<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct CodeVisitor;

        impl<'de> Visitor<'de> for CodeVisitor {
            type Value = Code;

            #[inline(always)]
            fn expecting(
                &self,
                formatter: &mut ::core::fmt::Formatter,
            ) -> core::fmt::Result {
                formatter.write_str("an array of 16 bytes")
            }

            #[inline(always)]
            fn visit_bytes<E>(self, v: &[u8]) -> StdResult<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if v.len() == 16 {
                    let mut val = [0u8; 16];
                    val.copy_from_slice(v);

                    Ok(Code { val })
                } else {
                    Err(serde::de::Error::invalid_length(v.len(), &self))
                }
            }

            #[inline(always)]
            fn visit_str<E>(self, s: &str) -> StdResult<Self::Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_bytes(&b64dec(s).map_err(serde::de::Error::custom)?)
            }
        }

        if deserializer.is_human_readable() {
            deserializer.deserialize_str(CodeVisitor)
        } else {
            deserializer.deserialize_bytes(CodeVisitor)
        }
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Memo(pub String);

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ConfidentialMemo;

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Commitment([u8; 32]);

#[allow(missing_docs)]
#[derive(
    Clone, Copy, Debug, Default, Deserialize, PartialEq, Eq, PartialOrd, Ord, Serialize,
)]
pub struct XfrAddress {
    pub key: XfrPublicKey,
}

impl XfrAddress {
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn to_base64(self) -> String {
        b64enc(&self.key.as_bytes())
    }

    // pub(crate) fn to_bytes(self) -> Vec<u8> {
    //     self.key.as_bytes().to_vec()
    // }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for XfrAddress {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.as_bytes().hash(state);
    }
}

#[allow(missing_docs)]
#[derive(
    Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Ord, PartialOrd, Serialize,
)]
pub struct IssuerPublicKey {
    pub key: XfrPublicKey,
}

impl IssuerPublicKey {
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn to_base64(self) -> String {
        b64enc(self.key.as_bytes())
    }

    // pub(crate) fn to_bytes(&self) -> Vec<u8> {
    //     self.key.as_bytes().to_vec()
    // }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for IssuerPublicKey {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.as_bytes().hash(state);
    }
}

impl Deref for IssuerPublicKey {
    type Target = XfrPublicKey;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.key
    }
}

#[derive(Debug)]
#[allow(missing_docs)]
pub struct IssuerKeyPair<'a> {
    pub keypair: &'a XfrKeyPair,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
struct AccountAddress {
    pub key: XfrPublicKey,
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct IndexedSignature<T> {
    pub address: XfrAddress,
    pub signature: SignatureOf<(T, Option<usize>)>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub input_idx: Option<usize>, // Some(idx) if a co-signature, None otherwise
}

impl<T> IndexedSignature<T>
where
    T: Clone + Serialize + serde::de::DeserializeOwned,
{
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn verify(&self, message: &T) -> bool {
        self.signature
            .verify(&self.address.key, &(message.clone(), self.input_idx))
            .is_ok()
    }
}

/// Stores threshold and weights for a multisignature requirement.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SignatureRules {
    /// Minimum sum of signature weights that is required for an asset transfer.
    pub threshold: u64,
    /// Stored as a vector so that serialization is deterministic
    pub weights: Vec<(XfrPublicKey, u64)>,
}

impl SignatureRules {
    /// Returns Ok(()) if the sum of weights of the keys in keyset reaches the threshold.
    /// Keyset must store XfrPublicKeys in byte form.
    pub fn check_signature_set(&self, keyset: &HashSet<Vec<u8>>) -> Result<()> {
        let mut sum: u64 = 0;
        let mut weight_map = HashMap::new();
        // Convert to map
        for (key, weight) in self.weights.iter() {
            weight_map.insert(key.as_bytes(), *weight);
        }
        // Calculate weighted sum
        for key in keyset.iter() {
            sum = sum
                .checked_add(*weight_map.get(&key[..]).unwrap_or(&0))
                .c(d!())?;
        }

        if sum < self.threshold {
            return Err(eg!());
        }
        Ok(())
    }
}

/// Simple asset rules
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AssetRules {
    /// Transferable: Non-transferable assets can only be transferred once from the issuer to
    ///   another user.
    pub transferable: bool,
    /// Updatable: Whether the asset memo can be updated.
    pub updatable: bool,
    /// Transfer signature rules: Signature weights and threshold for a valid transfer.
    pub transfer_multisig_rules: Option<SignatureRules>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    /// Asset tracing policies: A bundle of tracing policies specifying the tracing proofs that
    ///  constitute a valid transfer.
    pub tracing_policies: TracingPolicies,
    #[serde(with = "serde_strz::emp", default)]
    /// Max units: Optional limit on total issuance amount.
    pub max_units: Option<u64>,
    /// Decimals: default to FRA_DECIMALS
    pub decimals: u8,
}
impl Default for AssetRules {
    #[inline(always)]
    fn default() -> Self {
        AssetRules {
            tracing_policies: TracingPolicies::new(),
            transferable: true,
            updatable: false,
            max_units: None,
            transfer_multisig_rules: None,
            decimals: FRA_DECIMALS,
        }
    }
}

impl AssetRules {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn add_tracing_policy(&mut self, policy: TracingPolicy) -> &mut Self {
        self.tracing_policies.add(policy);
        self
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_max_units(&mut self, max_units: Option<u64>) -> &mut Self {
        self.max_units = max_units;
        self
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_transferable(&mut self, transferable: bool) -> &mut Self {
        self.transferable = transferable;
        self
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_updatable(&mut self, updatable: bool) -> &mut Self {
        self.updatable = updatable;
        self
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_transfer_multisig_rules(
        &mut self,
        multisig_rules: Option<SignatureRules>,
    ) -> &mut Self {
        self.transfer_multisig_rules = multisig_rules;
        self
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn set_decimals(&mut self, decimals: u8) -> Result<&mut Self> {
        if decimals > MAX_DECIMALS_LENGTH {
            return Err(eg!("asset decimals should be less than 20"));
        }
        self.decimals = decimals;
        Ok(self)
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct Asset {
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub code: AssetTypeCode,
    pub issuer: IssuerPublicKey,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub memo: Memo,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub confidential_memo: ConfidentialMemo,
    #[serde(default)]
    pub asset_rules: AssetRules,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub policy: Option<(Box<Policy>, PolicyGlobals)>,
}

/// Note:
/// if the properties field of this struct is changed,
/// update the comment for AssetType::from_json in wasm_data_model.rs as well.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct AssetType {
    /// major properties of this asset
    pub properties: Asset,
    pub(crate) digest: [u8; 32],
    pub(crate) units: u64,
    pub(crate) confidential_units: Commitment,
}

impl AssetType {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn has_issuance_restrictions(&self) -> bool {
        self.properties.asset_rules.max_units.is_some()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn has_transfer_restrictions(&self) -> bool {
        let simple_asset: Asset = {
            let mut ret: Asset = Default::default();
            let asset = &self.properties;

            ret.code = asset.code;
            ret.issuer = asset.issuer;
            ret.memo = asset.memo.clone();
            ret.confidential_memo = asset.confidential_memo;
            // Only relevant for issue operations
            ret.asset_rules.max_units = asset.asset_rules.max_units;
            ret.asset_rules.decimals = asset.asset_rules.decimals;

            ret
        };
        self.properties != simple_asset
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_tracing_policies_ref(&self) -> &TracingPolicies {
        &self.properties.asset_rules.tracing_policies
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CustomAssetPolicy {
    policy: Vec<u8>, // serialized policy, underlying form TBD.
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CredentialProofKey([u8; 16]);

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CredentialProof {
    pub key: CredentialProofKey,
}

#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    Deserialize,
    Eq,
    Hash,
    PartialEq,
    Serialize,
    Ord,
    PartialOrd,
)]
#[allow(missing_docs)]
pub struct TxoSID(pub u64);

impl NumKey for TxoSID {
    fn to_bytes(&self) -> Vec<u8> {
        self.0.to_ne_bytes().to_vec()
    }
    fn from_bytes(b: &[u8]) -> Result<Self> {
        <[u8; mem::size_of::<u64>()]>::try_from(b)
            .c(d!())
            .map(u64::from_ne_bytes)
            .map(TxoSID)
    }
}

#[allow(missing_docs)]
pub type TxoSIDList = Vec<TxoSID>;

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct OutputPosition(pub usize);

#[allow(missing_docs)]
#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    Deserialize,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
)]
pub struct TxnSID(pub usize);

impl fmt::Display for TxoSID {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl NumKey for TxnSID {
    fn to_bytes(&self) -> Vec<u8> {
        self.0.to_ne_bytes().to_vec()
    }
    fn from_bytes(b: &[u8]) -> Result<Self> {
        <[u8; mem::size_of::<usize>()]>::try_from(b)
            .c(d!())
            .map(usize::from_ne_bytes)
            .map(TxnSID)
    }
}

/// (sid, hash)
pub type TxnIDHash = (TxnSID, String);

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct BlockSID(pub usize);

/// An ephemeral index for a transaction (with a different newtype so that
/// it's harder to mix up)
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct TxnTempSID(pub usize);

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TxOutput {
    pub id: Option<TxoSID>,
    pub record: BlindAssetRecord,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub lien: Option<HashOf<Vec<TxOutput>>>,
}

#[allow(missing_docs)]
#[derive(Eq, Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum UtxoStatus {
    Spent,
    Unspent,
    Nonexistent,
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Utxo(pub TxOutput);

impl Utxo {
    #[cfg(not(target_arch = "wasm32"))]
    #[inline(always)]
    pub(crate) fn get_nonconfidential_balance(&self) -> u64 {
        if let XfrAmount::NonConfidential(n) = self.0.record.amount {
            n
        } else {
            0
        }
    }
}

/// Ledger address of input
#[derive(Copy, Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum TxoRef {
    /// Offset backwards from this operation (within a txn) -- 0 is the most recent, (n-1) (if there
    /// are n outputs so far) is the first output of the transaction
    Relative(u64),
    /// Absolute Txo address to a location outside this txn
    Absolute(TxoSID),
}

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct NoReplayToken([u8; 8], u64);

impl NoReplayToken {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new<R: RngCore>(prng: &mut R, seq_id: u64) -> Self {
        NoReplayToken(prng.next_u64().to_be_bytes(), seq_id)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn unsafe_new(rand: u64, seq_id: u64) -> Self {
        NoReplayToken(rand.to_be_bytes(), seq_id)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_seq_id(&self) -> u64 {
        self.1
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_rand(&self) -> [u8; 8] {
        self.0
    }
}

/// The inner data of Transfer Operation
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TransferAssetBody {
    /// Ledger address of inputs
    pub inputs: Vec<TxoRef>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    /// Transfer policies
    pub policies: XfrNotePolicies,
    /// A array of transaction outputs
    pub outputs: Vec<TxOutput>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    /// (inp_idx,out_idx,hash) triples signifying that the lien `hash` on
    /// the input `inp_idx` gets assigned to the output `out_idx`
    pub lien_assignments: Vec<(usize, usize, HashOf<Vec<TxOutput>>)>,
    /// TODO(joe): we probably don't need the whole XfrNote with input records
    /// once it's on the chain
    /// Encrypted transfer note
    pub transfer: Box<XfrBody>,

    /// Only Standard type supported
    pub transfer_type: TransferType,
}

impl TransferAssetBody {
    #[allow(missing_docs)]
    #[allow(clippy::too_many_arguments)]
    pub fn new<R: CryptoRng + RngCore>(
        prng: &mut R,
        input_refs: Vec<TxoRef>,
        input_records: &[AssetRecord],
        output_records: &[AssetRecord],
        policies: Option<XfrNotePolicies>,
        lien_assignments: Vec<(usize, usize, HashOf<Vec<TxOutput>>)>,
        transfer_type: TransferType,
    ) -> Result<TransferAssetBody> {
        let num_inputs = input_records.len();
        let num_outputs = output_records.len();

        if num_inputs == 0 {
            return Err(eg!());
        }

        // If no policies specified, construct set of empty policies
        let policies = policies.unwrap_or_else(|| {
            let no_policies = TracingPolicies::new();
            XfrNotePolicies::new(
                vec![no_policies.clone(); num_inputs],
                vec![None; num_inputs],
                vec![no_policies; num_outputs],
                vec![None; num_outputs],
            )
        });

        // Verify that for each input and output, there is a corresponding policy and credential commitment
        if num_inputs != policies.inputs_tracing_policies.len()
            || num_inputs != policies.inputs_sig_commitments.len()
            || num_outputs != policies.outputs_tracing_policies.len()
            || num_outputs != policies.outputs_sig_commitments.len()
        {
            return Err(eg!());
        }

        let transfer =
            Box::new(gen_xfr_body(prng, input_records, output_records).c(d!())?);
        let outputs = transfer
            .outputs
            .iter()
            .map(|rec| TxOutput {
                id: None,
                record: rec.clone(),
                lien: None,
            })
            .collect();
        Ok(TransferAssetBody {
            inputs: input_refs,
            outputs,
            policies,
            lien_assignments,
            transfer,
            transfer_type,
        })
    }

    /// Computes a body signature. A body signature represents consent to some part of the asset transfer. If an
    /// input_idx is specified, the signature is a co-signature.
    #[inline(always)]
    pub fn compute_body_signature(
        &self,
        keypair: &XfrKeyPair,
        input_idx: Option<usize>,
    ) -> IndexedSignature<TransferAssetBody> {
        let public_key = keypair.get_pk_ref();
        IndexedSignature {
            signature: SignatureOf::new(keypair, &(self.clone(), input_idx)),
            address: XfrAddress { key: *public_key },
            input_idx,
        }
    }

    /// Verifies a body signature
    #[inline(always)]
    pub fn verify_body_signature(
        &self,
        signature: &IndexedSignature<TransferAssetBody>,
    ) -> bool {
        signature.verify(&self)
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct IssueAssetBody {
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub code: AssetTypeCode,
    pub seq_num: u64,
    pub num_outputs: usize,
    pub records: Vec<(TxOutput, Option<OwnerMemo>)>,
}

impl IssueAssetBody {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        token_code: &AssetTypeCode,
        seq_num: u64,
        records: &[(TxOutput, Option<OwnerMemo>)],
    ) -> Result<IssueAssetBody> {
        Ok(IssueAssetBody {
            code: *token_code,
            seq_num,
            num_outputs: records.len(),
            records: records.to_vec(),
        })
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct DefineAssetBody {
    pub asset: Box<Asset>,
}

impl DefineAssetBody {
    #[allow(missing_docs)]
    pub fn new(
        token_code: &AssetTypeCode,
        issuer_key: &IssuerPublicKey,
        asset_rules: AssetRules,
        memo: Option<Memo>,
        confidential_memo: Option<ConfidentialMemo>,
    ) -> Result<DefineAssetBody> {
        let mut asset_def: Asset = Default::default();
        asset_def.code = *token_code;
        asset_def.issuer = *issuer_key;
        asset_def.asset_rules = asset_rules;
        asset_def.policy = None;

        if let Some(memo) = memo {
            asset_def.memo = Memo(memo.0);
        } else {
            asset_def.memo = Memo(String::from(""));
        }

        if let Some(confidential_memo) = confidential_memo {
            asset_def.confidential_memo = confidential_memo;
        } else {
            asset_def.confidential_memo = ConfidentialMemo {};
        }
        Ok(DefineAssetBody {
            asset: Box::new(asset_def),
        })
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct UpdateMemoBody {
    pub new_memo: Memo,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub asset_type: AssetTypeCode,
    pub no_replay_token: NoReplayToken,
}

/// Enum indicating whether an Transfer is standard type
/// Currently only Standard type is supported
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum TransferType {
    /// Standard transfer
    Standard,
    /// Not supported yet
    DebtSwap,
}

impl Default for TransferType {
    #[inline(always)]
    fn default() -> Self {
        Self::Standard
    }
}

/// Enum indicating whether an output appearning in transaction is internally spent
pub enum OutputSpentStatus {
    #[allow(missing_docs)]
    Spent,
    #[allow(missing_docs)]
    Unspent,
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TransferAsset {
    pub body: TransferAssetBody,
    pub body_signatures: Vec<IndexedSignature<TransferAssetBody>>,
}

impl TransferAsset {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(transfer_body: TransferAssetBody) -> Result<TransferAsset> {
        Ok(TransferAsset {
            body: transfer_body,
            body_signatures: Vec::new(),
        })
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn sign(&mut self, keypair: &XfrKeyPair) {
        let sig = self.create_input_signature(keypair);
        self.attach_signature(sig).unwrap()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn attach_signature(
        &mut self,
        sig: IndexedSignature<TransferAssetBody>,
    ) -> Result<()> {
        if !sig.verify(&self.body) {
            return Err(eg!());
        }
        self.body_signatures.push(sig);
        Ok(())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn create_input_signature(
        &self,
        keypair: &XfrKeyPair,
    ) -> IndexedSignature<TransferAssetBody> {
        self.body.compute_body_signature(keypair, None)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_owner_memos_ref(&self) -> Vec<Option<&OwnerMemo>> {
        self.body
            .transfer
            .owners_memos
            .iter()
            .map(|mem| mem.as_ref())
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_owner_addresses(&self) -> Vec<XfrPublicKey> {
        self.body
            .transfer
            .inputs
            .iter()
            .map(|record| record.public_key)
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_outputs_ref(&self) -> Vec<&TxOutput> {
        self.body.outputs.iter().collect()
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct IssueAsset {
    pub body: IssueAssetBody,
    pub pubkey: IssuerPublicKey,
    pub signature: SignatureOf<IssueAssetBody>,
}

impl IssueAsset {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        issuance_body: IssueAssetBody,
        keypair: &IssuerKeyPair,
    ) -> Result<IssueAsset> {
        let signature = SignatureOf::new(&keypair.keypair, &issuance_body);
        Ok(IssueAsset {
            body: issuance_body,
            pubkey: IssuerPublicKey {
                key: *keypair.keypair.get_pk_ref(),
            },
            signature,
        })
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_owner_memos_ref(&self) -> Vec<Option<&OwnerMemo>> {
        self.body
            .records
            .iter()
            .map(|(_, memo)| memo.as_ref())
            .collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_outputs_ref(&self) -> Vec<&TxOutput> {
        self.body.records.iter().map(|rec| &rec.0).collect()
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct DefineAsset {
    pub body: DefineAssetBody,

    pub pubkey: IssuerPublicKey,
    pub signature: SignatureOf<DefineAssetBody>,
}

impl DefineAsset {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        creation_body: DefineAssetBody,
        keypair: &IssuerKeyPair,
    ) -> Result<DefineAsset> {
        let signature = SignatureOf::new(&keypair.keypair, &creation_body);
        Ok(DefineAsset {
            body: creation_body,
            pubkey: IssuerPublicKey {
                key: *keypair.keypair.get_pk_ref(),
            },
            signature,
        })
    }
}

/// Operation data for a updating findora custom asset memo
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct UpdateMemo {
    /// Inner data to update
    pub body: UpdateMemoBody,
    /// The findora account publickey
    pub pubkey: XfrPublicKey,
    /// the signature
    pub signature: SignatureOf<UpdateMemoBody>,
}

impl UpdateMemo {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        update_memo_body: UpdateMemoBody,
        signing_key: &XfrKeyPair,
    ) -> UpdateMemo {
        let signature = SignatureOf::new(signing_key, &update_memo_body);
        UpdateMemo {
            body: update_memo_body,
            pubkey: *signing_key.get_pk_ref(),
            signature,
        }
    }
}

/// Operation list supported in findora network
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Operation {
    /// Transfer a findora asset, FRA or custom asset
    TransferAsset(TransferAsset),
    /// Issue a custom asset in findora network
    IssueAsset(IssueAsset),
    /// Create a new asset for a findora account
    DefineAsset(DefineAsset),
    /// Update memo for a findora custom asset
    UpdateMemo(UpdateMemo),
    /// Add or remove validator from findora network
    UpdateStaker(UpdateStakerOps),
    /// Delegate FRA token to existed validator or self-delegation
    Delegation(DelegationOps),
    /// Withdraw FRA token from findora network
    UnDelegation(Box<UnDelegationOps>),
    /// Claim rewards
    Claim(ClaimOps),
    /// Update initial validator list
    UpdateValidator(UpdateValidatorOps),
    /// Findora network goverance operation
    Governance(GovernanceOps),
    /// Update FRA distribution
    FraDistribution(FraDistributionOps),
    /// Coinbase operation
    MintFra(MintFraOps),
    /// Convert UTXOs to EVM Account balance
    ConvertAccount(ConvertAccount),
    ///replace staker.
    ReplaceStaker(ReplaceStakerOps),
}

fn set_no_replay_token(op: &mut Operation, no_replay_token: NoReplayToken) {
    match op {
        Operation::UpdateStaker(i) => {
            i.set_nonce(no_replay_token);
        }
        Operation::Delegation(i) => {
            i.set_nonce(no_replay_token);
        }
        Operation::UnDelegation(i) => {
            i.set_nonce(no_replay_token);
        }
        Operation::Claim(i) => {
            i.set_nonce(no_replay_token);
        }
        Operation::FraDistribution(i) => {
            i.set_nonce(no_replay_token);
        }
        Operation::UpdateValidator(i) => {
            i.set_nonce(no_replay_token);
        }
        Operation::Governance(i) => {
            i.set_nonce(no_replay_token);
        }
        Operation::UpdateMemo(i) => i.body.no_replay_token = no_replay_token,
        Operation::ConvertAccount(i) => i.set_nonce(no_replay_token),
        _ => {}
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize, Default)]
pub struct TransactionBody {
    pub no_replay_token: NoReplayToken,
    pub operations: Vec<Operation>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub credentials: Vec<CredentialProof>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub policy_options: Option<TxnPolicyData>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub memos: Vec<Memo>,
}

impl TransactionBody {
    #[inline(always)]
    fn from_token(no_replay_token: NoReplayToken) -> Self {
        let mut result = TransactionBody::default();
        result.no_replay_token = no_replay_token;
        result
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct Transaction {
    pub body: TransactionBody,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub signatures: Vec<SignatureOf<TransactionBody>>,
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FinalizedTransaction {
    pub txn: Transaction,
    pub tx_id: TxnSID,
    pub txo_ids: Vec<TxoSID>,

    pub merkle_id: u64,
}

/// Note: if the utxo field of this struct is changed, update the comment for ClientAssetRecord::from_json in wasm_data_model.rs as well.
#[derive(Clone, Serialize, Deserialize)]
pub struct AuthenticatedUtxo {
    /// Utxo to authenticate
    pub utxo: Utxo,
    /// Merkle proof that transaction containing the utxo exists on the ledger
    pub authenticated_txn: AuthenticatedTransaction,
    /// Bitmap proof that the utxo is unspent
    pub authenticated_spent_status: AuthenticatedUtxoStatus,
    /// which output this utxo locations
    pub utxo_location: OutputPosition,
    /// utxo proof data
    pub state_commitment_data: StateCommitmentData,
}

#[allow(missing_docs)]
#[derive(Clone, Serialize, Deserialize)]
pub struct UnAuthenticatedUtxo {
    pub utxo: Utxo,
    pub txn: FinalizedTransaction,
    pub utxo_location: OutputPosition,
}

impl AuthenticatedUtxo {
    /// An authenticated utxo result is valid iff
    /// 1) The state commitment data used during verification hashes to the provided state commitment
    /// 2) The authenticated transaction proof is valid
    /// 3) The spent status proof is valid and denotes the utxo as unspent
    /// 4) The utxo appears in one of the outputs of the transaction (i.e. the output at
    ///    OutputPosition)
    pub fn is_valid(
        &self,
        state_commitment: HashOf<Option<StateCommitmentData>>,
    ) -> bool {
        //1)
        if state_commitment != self.state_commitment_data.compute_commitment() {
            return false;
        }

        //2)

        if !self.authenticated_txn.is_valid(state_commitment.clone()) {
            return false;
        }

        //3)
        if self.authenticated_spent_status.status != UtxoStatus::Unspent
            || !self.authenticated_spent_status.is_valid(state_commitment)
        {
            return false;
        }

        //4)
        let outputs = self
            .authenticated_txn
            .finalized_txn
            .txn
            .get_outputs_ref(false);
        let output = outputs.get(self.utxo_location.0);

        if output.is_none() {
            return false;
        }

        if *output.unwrap() != self.utxo.0 {
            return false;
        }

        true
    }
}

#[allow(missing_docs)]
#[derive(Serialize, Deserialize, Clone)]
pub struct AuthenticatedTransaction {
    pub finalized_txn: FinalizedTransaction,
    pub txn_inclusion_proof: ProofOf<(TxnSID, Transaction)>,
    pub state_commitment_data: StateCommitmentData,
    pub state_commitment: HashOf<Option<StateCommitmentData>>,
}

impl AuthenticatedTransaction {
    /// An authenticated txn result is valid if
    /// 1) The state commitment used in the proof matches what we pass in and the state commitment
    ///    data hashes to the state commitment
    /// 2) The transaction merkle proof is valid
    /// 3) The transaction merkle root matches the value in root_hash_data
    pub fn is_valid(
        &self,
        state_commitment: HashOf<Option<StateCommitmentData>>,
    ) -> bool {
        //1)
        if self.state_commitment != state_commitment
            || self.state_commitment != self.state_commitment_data.compute_commitment()
        {
            return false;
        }

        //2)
        let hash = self.finalized_txn.hash();

        if !self.txn_inclusion_proof.0.verify(hash.0) {
            return false;
        }

        //3)
        if self.state_commitment_data.transaction_merkle_commitment
            != self.txn_inclusion_proof.0.proof.root_hash
        {
            return false;
        }

        true
    }
}

#[allow(missing_docs)]
pub struct AuthenticatedBlock {
    pub block: FinalizedBlock,
    pub block_inclusion_proof: ProofOf<Vec<Transaction>>,
    pub state_commitment_data: StateCommitmentData,
    pub state_commitment: HashOf<Option<StateCommitmentData>>,
}

impl AuthenticatedBlock {
    /// An authenticated block result is valid if
    /// 1) The block merkle proof is valid
    /// 2) The block merkle root matches the value in root_hash_data
    /// 3) root_hash_data hashes to root_hash
    /// 4) The state commitment of the proof matches the state commitment passed in
    pub fn is_valid(
        &self,
        state_commitment: HashOf<Option<StateCommitmentData>>,
    ) -> bool {
        //1) compute block hash
        let txns: Vec<Transaction> = self
            .block
            .txns
            .iter()
            .map(|auth_tx| auth_tx.txn.clone())
            .collect();

        if !self.block_inclusion_proof.verify(&txns) {
            return false;
        }

        //2)
        if self.state_commitment_data.block_merkle
            != self.block_inclusion_proof.0.proof.root_hash
        {
            return false;
        }

        //3) 4)
        if self.state_commitment != self.state_commitment_data.compute_commitment()
            || state_commitment != self.state_commitment
        {
            return false;
        }

        true
    }
}

#[allow(missing_docs)]
#[derive(Serialize, Clone, Deserialize)]
pub struct AuthenticatedUtxoStatus {
    pub status: UtxoStatus,
    pub utxo_sid: TxoSID,
    pub state_commitment_data: StateCommitmentData,
    pub utxo_map_bytes: Option<Vec<u8>>, // BitMap only needed for proof if the txo_sid exists
    pub state_commitment: HashOf<Option<StateCommitmentData>>,
}

impl AuthenticatedUtxoStatus {
    /// An authenticated utxo status is valid (for txos that exist) if
    /// 1) The state commitment of the proof matches the state commitment passed in
    /// 2) The state commitment data hashes to the state commitment
    /// 3) For txos that don't exist, simply show that the utxo_sid greater than max_sid
    /// 4) The status matches the bit stored in the bitmap
    /// 5) The bitmap checksum matches digest in state commitment data
    pub fn is_valid(
        &self,
        state_commitment: HashOf<Option<StateCommitmentData>>,
    ) -> bool {
        let state_commitment_data = &self.state_commitment_data;
        let utxo_sid = self.utxo_sid.0;

        // 1, 2) First, validate the state commitment
        if state_commitment != self.state_commitment
            || self.state_commitment != state_commitment_data.compute_commitment()
        {
            return false;
        }

        // 3)
        if self.status == UtxoStatus::Nonexistent {
            return utxo_sid >= state_commitment_data.txo_count;
        }

        // If the txo exists, the proof must also contain a bitmap
        let utxo_map = SparseMap::new(&self.utxo_map_bytes.as_ref().unwrap()).unwrap();

        // 4) The status matches the bit stored in the bitmap
        let spent = !utxo_map.query(utxo_sid).unwrap();

        if (self.status == UtxoStatus::Spent && !spent)
            || (self.status == UtxoStatus::Unspent && spent)
        {
            return false;
        }

        // 5)
        if utxo_map.checksum() != self.state_commitment_data.bitmap {
            println!("failed at bitmap checksum");
            return false;
        }

        true
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct FinalizedBlock {
    pub txns: Vec<FinalizedTransaction>,
    pub merkle_id: u64,
    pub state: StateCommitmentData,
}

impl FinalizedTransaction {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn hash(&self) -> HashOf<(TxnSID, Transaction)> {
        self.txn.hash(self.tx_id)
    }

    #[allow(missing_docs)]
    pub fn set_txo_id(&mut self) {
        let ids = mem::take(&mut self.txo_ids);

        self.txn
            .body
            .operations
            .iter_mut()
            .flat_map(|new| match new {
                Operation::TransferAsset(d) => d.body.outputs.iter_mut().collect(),
                Operation::MintFra(d) => {
                    d.entries.iter_mut().map(|et| &mut et.utxo).collect()
                }
                Operation::IssueAsset(d) => {
                    d.body.records.iter_mut().map(|(o, _)| o).collect()
                }
                _ => Vec::new(),
            })
            .zip(ids.iter())
            .for_each(|(o, id)| {
                o.id = Some(*id);
            });

        self.txo_ids = ids;
    }
}

/// Use pure zero bytes(aka [0, 0, ... , 0]) to express FRA.
pub const ASSET_TYPE_FRA: ZeiAssetType = ZeiAssetType([0; ASSET_TYPE_LENGTH]);

/// FRA decimals
pub const FRA_DECIMALS: u8 = 6;

lazy_static! {
    /// The destination of Fee is an black hole,
    /// all token transfered to it will be burned.
    pub static ref BLACK_HOLE_PUBKEY: XfrPublicKey = pnk!(XfrPublicKey::zei_from_bytes(&[0; ed25519_dalek::PUBLIC_KEY_LENGTH][..]));
    /// BlackHole of Staking
    pub static ref BLACK_HOLE_PUBKEY_STAKING: XfrPublicKey = pnk!(XfrPublicKey::zei_from_bytes(&[1; ed25519_dalek::PUBLIC_KEY_LENGTH][..]));
}

/// see [**mainnet-v0.1 defination**](https://www.notion.so/findora/Transaction-Fees-Analysis-d657247b70f44a699d50e1b01b8a2287)
pub const TX_FEE_MIN: u64 = 1_0000;

impl Transaction {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn is_coinbase_tx(&self) -> bool {
        self.body
            .operations
            .iter()
            .any(|o| matches!(o, Operation::MintFra(_)))
    }

    /// All-in-one checker
    #[inline(always)]
    pub fn valid_in_abci(&self) -> bool {
        self.check_fee() && !self.is_coinbase_tx()
    }

    /// A simple fee checker
    ///
    /// The check logic is as follows:
    /// - Only `NonConfidential Operation` can be used as fee
    /// - FRA code == [0; ASSET_TYPE_LENGTH]
    /// - Fee destination == BLACK_HOLE_PUBKEY
    /// - A transaction with an `Operation` of defining/issuing FRA need NOT fee
    /// - A transaction with all addresses of inputs equal to BLACK_HOLE_PUBKEY need NOT fee
    pub fn check_fee(&self) -> bool {
        // This method can not completely solve the DOS risk,
        // we should further limit the number of txo[s] in every operation.
        //
        // But it seems enough when we combine it with limiting
        // the payload size of submission-server's http-requests.
        self.is_coinbase_tx()
            || self.body.operations.iter().any(|ops| {
                if let Operation::TransferAsset(ref x) = ops {
                    return x.body.outputs.iter().any(|o| {
                        if let XfrAssetType::NonConfidential(ty) = o.record.asset_type {
                            if ty == ASSET_TYPE_FRA
                                && *BLACK_HOLE_PUBKEY == o.record.public_key
                            {
                                if let XfrAmount::NonConfidential(am) = o.record.amount {
                                    if am > (TX_FEE_MIN - 1) {
                                        return true;
                                    }
                                }
                            }
                        }
                        false
                    });
                } else if let Operation::DefineAsset(ref x) = ops {
                    if x.body.asset.code.val == ASSET_TYPE_FRA {
                        return true;
                    }
                } else if let Operation::IssueAsset(ref x) = ops {
                    if x.body.code.val == ASSET_TYPE_FRA {
                        return true;
                    }
                } else if matches!(ops, Operation::UpdateValidator(_)) {
                    return true;
                }
                false
            })
    }

    /// findora hash
    #[inline(always)]
    pub fn hash(&self, id: TxnSID) -> HashOf<(TxnSID, Transaction)> {
        HashOf::new(&(id, self.clone()))
    }

    /// tendermint hash
    #[inline(always)]
    pub fn hash_tm(&self) -> HashOf<Transaction> {
        HashOf::new(self)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn hash_tm_rawbytes(&self) -> Vec<u8> {
        self.hash_tm().0.hash.as_ref().to_vec()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn handle(&self) -> String {
        let digest = self.hash(TxnSID(0));
        hex::encode(digest)
    }

    /// Create a transaction from seq id
    #[inline(always)]
    pub fn from_seq_id(seq_id: u64) -> Self {
        let mut prng = ChaChaRng::from_entropy();
        let no_replay_token = NoReplayToken::new(&mut prng, seq_id);
        Transaction {
            body: TransactionBody::from_token(no_replay_token),
            signatures: Vec::new(),
        }
    }

    /// Create a transaction from a operation
    #[inline(always)]
    pub fn from_operation(op: Operation, seq_id: u64) -> Self {
        let mut tx = Transaction::from_seq_id(seq_id);
        tx.add_operation(op);
        tx
    }

    /// Create a transaction from coinbase operation
    #[inline(always)]
    pub fn from_operation_coinbase_mint(op: Operation, seq_id: u64) -> Self {
        let mut tx = Transaction {
            body: TransactionBody::from_token(NoReplayToken::unsafe_new(
                seq_id.saturating_add(1357).saturating_mul(89),
                seq_id,
            )),
            signatures: Vec::new(),
        };
        tx.add_operation(op);
        tx
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn add_operation(&mut self, mut op: Operation) {
        set_no_replay_token(&mut op, self.body.no_replay_token);
        self.body.operations.push(op);
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn sign(&mut self, keypair: &XfrKeyPair) {
        self.signatures.push(SignatureOf::new(keypair, &self.body));
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn check_signature(
        &self,
        public_key: &XfrPublicKey,
        sig: &SignatureOf<TransactionBody>,
    ) -> Result<()> {
        sig.verify(public_key, &self.body).c(d!())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_owner_memos_ref(&self) -> Vec<Option<&OwnerMemo>> {
        let mut memos = Vec::new();
        for op in self.body.operations.iter() {
            match op {
                Operation::TransferAsset(xfr_asset) => {
                    memos.append(&mut xfr_asset.get_owner_memos_ref());
                }
                Operation::MintFra(mint_asset) => {
                    memos.append(&mut mint_asset.get_owner_memos_ref());
                }
                Operation::IssueAsset(issue_asset) => {
                    memos.append(&mut issue_asset.get_owner_memos_ref());
                }
                _ => {}
            }
        }
        memos
    }

    /// Returns the outputs of a transaction. Internally spent outputs can be optionally included.
    /// This will never panic on a well formed transaction, but may panic on a malformed one.
    #[inline(always)]
    pub fn get_outputs_ref(&self, include_spent: bool) -> Vec<TxOutput> {
        let eff = TxnEffect::compute_effect(self.clone()).unwrap();
        if !include_spent {
            eff.txos.into_iter().flatten().collect()
        } else {
            let mut spent = eff.internally_spent_txos.into_iter();
            let mut ret = Vec::new();
            for txo in eff.txos.into_iter() {
                if let Some(txo) = txo {
                    ret.push(txo);
                } else {
                    ret.push(spent.next().unwrap());
                }
            }
            ret
        }
    }

    /// NOTE: this does *not* guarantee that a private key affiliated with
    /// `public_key` has signed this transaction! If `public_key` is derived
    /// from `self` somehow, then it is infeasible for someone to forge a
    /// passing signature, but it is plausible for someone to generate an
    /// unrelated `public_key` which can pass this signature check!
    #[inline(always)]
    pub fn check_has_signature(&self, public_key: &XfrPublicKey) -> Result<()> {
        let serialized = Serialized::new(&self.body);
        for sig in self.signatures.iter() {
            match sig.0.verify(public_key, &serialized) {
                Err(_) => {}
                Ok(_) => {
                    return Ok(());
                }
            }
        }
        Err(eg!())
    }

    /// NOTE: This method is used to verify the signature in the transaction,
    /// when the user constructs the transaction not only needs to sign each `operation`,
    /// but also needs to sign the whole transaction, otherwise it will not be passed here
    #[inline(always)]
    pub fn check_tx(&self) -> Result<()> {
        for operation in self.body.operations.iter() {
            match operation {
                Operation::TransferAsset(o) => {
                    for pk in o.get_owner_addresses().iter() {
                        self.check_has_signature(pk)?;
                    }
                }
                Operation::IssueAsset(o) => {
                    self.check_has_signature(&o.pubkey.key)?;
                }
                Operation::DefineAsset(o) => {
                    self.check_has_signature(&o.pubkey.key)?;
                }
                Operation::UpdateMemo(o) => {
                    self.check_has_signature(&o.pubkey)?;
                }
                Operation::UpdateStaker(o) => {
                    self.check_has_signature(&o.pubkey)?;
                }
                Operation::Delegation(o) => {
                    self.check_has_signature(&o.pubkey)?;
                }
                Operation::UnDelegation(o) => {
                    self.check_has_signature(&o.pubkey)?;
                }
                Operation::Claim(o) => {
                    self.check_has_signature(&o.pubkey)?;
                }
                Operation::UpdateValidator(_) => {}
                Operation::Governance(_) => {}
                Operation::FraDistribution(_) => {}
                Operation::MintFra(_) => {}
                Operation::ConvertAccount(o) => {
                    self.check_has_signature(&o.signer)?;
                }
                Operation::ReplaceStaker(o) => {
                    if !o.get_related_pubkeys().is_empty() {
                        for get_related_pubkey in o.get_related_pubkeys() {
                            self.check_has_signature(&get_related_pubkey)?;
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

/// Current ledger state commitment data
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct StateCommitmentData {
    /// The checksum of the utxo_map
    pub bitmap: BitDigest,
    /// The root hash of the block Merkle tree
    pub block_merkle: HashValue,
    /// The hash of the transactions in the block
    pub txns_in_block_hash: HashOf<Vec<Transaction>>,
    /// The prior global block hash
    pub previous_state_commitment: HashOf<Option<StateCommitmentData>>,
    /// The root hash of the transaction Merkle tree
    pub transaction_merkle_commitment: HashValue,
    /// for compatible with old data of mainnet
    pub air_commitment: BitDigest,
    /// Number of transaction outputs. Used to provide proof that a utxo does not exist
    pub txo_count: u64,
    /// a consensus-specific counter; should be 0 unless consensus needs it.
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub pulse_count: u64,
    /// hash(non-empty Staking)
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub staking: Option<HashOf<Staking>>,
}

impl StateCommitmentData {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn compute_commitment(&self) -> HashOf<Option<Self>> {
        HashOf::new(&Some(self).cloned())
    }
}

/// Used in `Staking` logic to create consensus-tmp XfrPublicKey
#[derive(Clone, Copy, Debug, Deserialize, Serialize, Eq, PartialEq, Default)]
pub struct ConsensusRng(u32);

impl CryptoRng for ConsensusRng {}

impl RngCore for ConsensusRng {
    fn next_u32(&mut self) -> u32 {
        self.0 = self.0.overflowing_add(1).0;
        self.0
    }
    fn next_u64(&mut self) -> u64 {
        self.next_u32() as u64
    }
    fn fill_bytes(&mut self, _dest: &mut [u8]) {}
    fn try_fill_bytes(&mut self, _dest: &mut [u8]) -> StdResult<(), rand_core::Error> {
        Ok(())
    }
}

#[inline(always)]
#[allow(missing_docs)]
pub fn gen_random_keypair() -> XfrKeyPair {
    XfrKeyPair::generate(&mut ChaChaRng::from_entropy())
}
