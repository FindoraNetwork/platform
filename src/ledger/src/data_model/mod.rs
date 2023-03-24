//!
//! An implementation of findora ledger data model
//!

#![allow(clippy::field_reassign_with_default)]
#![allow(clippy::assertions_on_constants)]

mod __trash__;
mod effects;
mod test;

pub use effects::{BlockEffect, TxnEffect};

use config::abci::CheckPointConfig;
use {
    crate::{
        converter::ConvertAccount,
        staking::{
            ops::{
                claim::ClaimOps, delegation::DelegationOps,
                fra_distribution::FraDistributionOps, governance::GovernanceOps,
                mint_fra::MintFraOps, replace_staker::ReplaceStakerOps,
                undelegation::UnDelegationOps, update_staker::UpdateStakerOps,
                update_validator::UpdateValidatorOps,
            },
            Staking,
        },
    },
    __trash__::{Policy, PolicyGlobals, TxnPolicyData},
    bitmap::SparseMap,
    cryptohash::{sha256::Digest as BitDigest, HashValue},
    digest::{consts::U64, Digest},
    fbnc::NumKey,
    globutils::wallet::public_key_to_base64,
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
    zei::noah_algebra::{
        bls12_381::BLSScalar, bn254::BN254Scalar, serialization::NoahFromToBytes,
        traits::Scalar,
    },
    zei::noah_api::{
        anon_xfr::{
            abar_to_abar::AXfrNote,
            abar_to_ar::{verify_abar_to_ar_note, AbarToArNote},
            abar_to_bar::{verify_abar_to_bar_note, AbarToBarNote},
            ar_to_abar::{verify_ar_to_abar_note, ArToAbarNote},
            bar_to_abar::{verify_bar_to_abar_note, BarToAbarNote},
            commit,
            structs::{AnonAssetRecord, AxfrOwnerMemo, Nullifier, OpenAnonAssetRecord},
            AXfrAddressFoldingInstance,
        },
        keys::PublicKey as NoahXfrPublicKey,
        parameters::{AddressFormat, VerifierParams},
        xfr::{
            gen_xfr_body,
            structs::{
                AssetRecord, AssetType as NoahAssetType, TracingPolicies, TracingPolicy,
                XfrAmount, XfrAssetType, ASSET_TYPE_LENGTH,
            },
            XfrNotePolicies,
        },
    },
    zei::noah_crypto::anemoi_jive::{AnemoiJive, AnemoiJive254},
    zei::{BlindAssetRecord, OwnerMemo, XfrBody, XfrKeyPair, XfrPublicKey},
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
    pub val: NoahAssetType,
}

impl NumKey for AssetTypeCode {
    fn to_bytes(&self) -> Vec<u8> {
        self.val.0.to_vec()
    }
    fn from_bytes(b: &[u8]) -> Result<Self> {
        let mut b = b.to_owned();
        b.resize(ASSET_TYPE_LENGTH, 0u8);
        Ok(Self {
            val: NoahAssetType(
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
            val: NoahAssetType([255; ASSET_TYPE_LENGTH]),
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
            val: NoahAssetType(val),
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
            val: NoahAssetType(
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
            val: NoahAssetType(buf),
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
                    val: NoahAssetType(buf),
                })
            }
            Err(e) => Err(eg!((format!("Failed to deserialize base64 '{b64}': {e}",)))),
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

    /// Generates the asset type code from the prefix and the Anemoi hash function
    #[inline(always)]
    pub fn from_prefix_and_raw_asset_type_code_2nd_update(
        prefix: AssetTypePrefix,
        raw_asset_type_code: &AssetTypeCode,
    ) -> Self {
        let mut f = Vec::with_capacity(3);
        f.push(prefix.to_field_element());

        let mut bytes = vec![0u8; 32];
        bytes[..31].copy_from_slice(&raw_asset_type_code.val.0[..31]);
        f.push(BN254Scalar::from_bytes(&bytes).unwrap());

        let mut bytes = vec![0u8; 32];
        bytes[0] = raw_asset_type_code.val.0[31];
        f.push(BN254Scalar::from_bytes(&bytes).unwrap());

        let res = AnemoiJive254::eval_variable_length_hash(&f);
        Self::new_from_vec(res.to_bytes())
    }

    /// Former version, now deprecated way to derive the asset code.
    /// This version uses BLS12-381.
    #[inline(always)]
    #[deprecated]
    pub fn from_prefix_and_raw_asset_type_code_1st_update(
        prefix: AssetTypePrefix,
        raw_asset_type_code: &AssetTypeCode,
    ) -> Self {
        let mut f = Vec::with_capacity(3);

        #[allow(deprecated)]
        f.push(prefix.to_field_element_old());

        let mut bytes = vec![0u8; 32];
        bytes[..31].copy_from_slice(&raw_asset_type_code.val.0[..31]);
        f.push(BLSScalar::from_bytes(&bytes).unwrap());

        let mut bytes = vec![0u8; 32];
        bytes[0] = raw_asset_type_code.val.0[31];
        f.push(BLSScalar::from_bytes(&bytes).unwrap());

        #[allow(deprecated)]
        {
            use zei::noah_crypto::anemoi_jive::bls12_381_deprecated::AnemoiJive381Deprecated;
            let res = AnemoiJive381Deprecated::eval_variable_length_hash(&f);
            Self::new_from_vec(res.to_bytes())
        }
    }

    /// Select the right asset code based on the global setting.
    pub fn from_prefix_and_raw_asset_type_code(
        prefix: AssetTypePrefix,
        raw_asset_type_code: &AssetTypeCode,
        checkpoint: &CheckPointConfig,
        cur_height: u64,
    ) -> Self {
        if raw_asset_type_code.val == ASSET_TYPE_FRA
            || core::cmp::min(
                checkpoint.utxo_asset_prefix_height,
                checkpoint.utxo_asset_prefix_height_2nd_update,
            ) > cur_height
        {
            *raw_asset_type_code
        } else if checkpoint.utxo_asset_prefix_height_2nd_update > cur_height
            && checkpoint.utxo_asset_prefix_height <= cur_height
        {
            #[allow(deprecated)]
            AssetTypeCode::from_prefix_and_raw_asset_type_code_1st_update(
                prefix,
                &raw_asset_type_code,
            )
        } else {
            AssetTypeCode::from_prefix_and_raw_asset_type_code_2nd_update(
                prefix,
                &raw_asset_type_code,
            )
        }
    }
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
    #[cfg(all(not(target_arch = "wasm32"), feature = "fin_storage"))]
    pub(crate) fn to_base64(self) -> String {
        b64enc(&self.key.to_bytes().as_slice())
    }

    // pub(crate) fn to_bytes(self) -> Vec<u8> {
    //     self.key.as_bytes().to_vec()
    // }
}

impl Hash for XfrAddress {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.to_bytes().as_slice().hash(state);
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
    #[cfg(all(not(target_arch = "wasm32"), feature = "fin_storage"))]
    pub(crate) fn to_base64(self) -> String {
        b64enc(&self.key.noah_to_bytes().as_slice())
    }

    // pub(crate) fn to_bytes(&self) -> Vec<u8> {
    //     self.key.as_bytes().to_vec()
    // }
}

impl Hash for IssuerPublicKey {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.noah_to_bytes().as_slice().hash(state);
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
        let mut weight_map: HashMap<Vec<u8>, u64> = HashMap::new();
        // Convert to map
        for (key, weight) in self.weights.iter() {
            let b = key.to_bytes();
            weight_map.insert(b, *weight);
        }
        // Calculate weighted sum
        for key in keyset.iter() {
            sum = sum
                .checked_add(*weight_map.get::<[u8]>(&key.as_slice()).unwrap_or(&0))
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
pub struct ATxoSID(pub u64);

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
    #[cfg(all(not(target_arch = "wasm32"), feature = "fin_storage"))]
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

        let transfer = Box::new(
            gen_xfr_body(prng, input_records, output_records)
                .and_then(|xb| XfrBody::from_noah(&xb))
                .c(d!())?,
        );
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
        let public_key = keypair.get_pk();
        IndexedSignature {
            signature: SignatureOf::new(&keypair, &(self.clone(), input_idx)),
            address: XfrAddress { key: public_key },
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

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize)]
pub enum AssetTypePrefix {
    UserDefined,
    ERC20,
    ERC721,
    ERC1155,
}

impl AssetTypePrefix {
    #[allow(missing_docs)]
    pub fn bytes(&self) -> Vec<u8> {
        match self {
            // println!("{:?}",Keccak256::digest(b"Findora User-defined Asset Type",).as_slice());
            AssetTypePrefix::UserDefined => vec![
                21, 32, 222, 134, 151, 157, 34, 235, 197, 37, 167, 187, 177, 22, 151,
                207, 188, 106, 180, 176, 48, 199, 185, 128, 200, 9, 142, 225, 131, 159,
                227, 159,
            ],
            // println!("{:?}",Keccak256::digest(b"Findora ERC20 Asset Type",).as_slice());
            AssetTypePrefix::ERC20 => vec![
                65, 125, 161, 85, 119, 221, 149, 146, 115, 34, 149, 216, 194, 125, 118,
                193, 48, 66, 123, 79, 108, 204, 246, 48, 201, 158, 89, 51, 76, 223, 150,
                61,
            ],
            // println!("{:?}",Keccak256::digest(b"Findora ERC721 Asset Type",).as_slice());
            AssetTypePrefix::ERC721 => vec![
                170, 49, 235, 37, 197, 120, 157, 2, 29, 201, 171, 189, 37, 86, 83, 193,
                122, 235, 202, 55, 40, 170, 141, 141, 78, 235, 98, 142, 128, 114, 97,
                59,
            ],
            // println!("{:?}",Keccak256::digest(b"Findora ERC1155 Asset Type",).as_slice());
            AssetTypePrefix::ERC1155 => vec![
                150, 43, 52, 224, 244, 30, 0, 120, 126, 90, 220, 123, 15, 96, 104, 17,
                211, 55, 68, 229, 107, 134, 178, 207, 138, 35, 88, 150, 3, 48, 36, 154,
            ],
        }
    }

    #[allow(missing_docs)]
    pub fn to_field_element(&self) -> BN254Scalar {
        BN254Scalar::from_bytes(&self.bytes()).unwrap()
    }

    #[allow(missing_docs)]
    #[deprecated]
    pub fn to_field_element_old(&self) -> BLSScalar {
        BLSScalar::from_bytes(&self.bytes()).unwrap()
    }
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
    pub fn get_owner_memos_ref(&self) -> Vec<Option<OwnerMemo>> {
        self.body.transfer.owners_memos.to_vec()
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
    pub fn get_owner_memos_ref(&self) -> Vec<Option<OwnerMemo>> {
        self.body
            .records
            .iter()
            .map(|(_, memo)| memo.clone())
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
        let signature = SignatureOf::new(&signing_key, &update_memo_body);
        UpdateMemo {
            body: update_memo_body,
            pubkey: *signing_key.get_pk_ref(),
            signature,
        }
    }
}

/// A note which enumerates the transparent and confidential BAR to
/// Anon Asset record conversion.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum BarAnonConvNote {
    /// A transfer note with ZKP for a confidential asset record
    BarNote(Box<BarToAbarNote>),
    /// A transfer note with ZKP for a non-confidential asset record
    ArNote(Box<ArToAbarNote>),
}

/// Operation for converting a Blind Asset Record to a Anonymous record
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BarToAbarOps {
    /// the note which contains the inp/op and ZKP
    pub note: BarAnonConvNote,
    /// The TxoSID of the the input BAR
    pub txo_sid: TxoSID,
    nonce: NoReplayToken,
}

impl BarToAbarOps {
    /// Generates a new BarToAbarOps object
    /// # Arguments
    /// * bar_to_abar_note - The BarToAbarNote of the conversion
    /// * txo_sid          - the TxoSID of the converting BAR
    /// * nonce
    pub fn new(
        note: BarAnonConvNote,
        txo_sid: TxoSID,
        nonce: NoReplayToken,
    ) -> Result<BarToAbarOps> {
        Ok(BarToAbarOps {
            note,
            txo_sid,
            nonce,
        })
    }

    /// verifies the signatures and proof of the note
    pub fn verify(&self) -> Result<()> {
        match &self.note {
            BarAnonConvNote::BarNote(note) => {
                // fetch the verifier Node Params for PlonkProof
                let node_params = VerifierParams::get_bar_to_abar().c(d!())?;
                // verify the Plonk proof and signature
                verify_bar_to_abar_note(&node_params, &note, &note.body.input.public_key)
                    .c(d!())
            }
            BarAnonConvNote::ArNote(note) => {
                // fetch the verifier Node Params for PlonkProof
                let node_params = VerifierParams::get_ar_to_abar().c(d!())?;
                // verify the Plonk proof and signature
                verify_ar_to_abar_note(&node_params, note).c(d!())
            }
        }
    }

    /// provides a copy of the input record in the note
    pub fn input_record(&self) -> BlindAssetRecord {
        match &self.note {
            BarAnonConvNote::BarNote(n) => {
                BlindAssetRecord::from_noah(&n.body.input).unwrap()
            }
            BarAnonConvNote::ArNote(n) => {
                BlindAssetRecord::from_noah(&n.body.input).unwrap()
            }
        }
    }

    /// provides a copy of the output record of the note.
    pub fn output_record(&self) -> AnonAssetRecord {
        match &self.note {
            BarAnonConvNote::BarNote(n) => n.body.output.clone(),
            BarAnonConvNote::ArNote(n) => n.body.output.clone(),
        }
    }

    /// provides a copy of the AxfrOwnerMemo in the note
    pub fn axfr_memo(&self) -> AxfrOwnerMemo {
        match &self.note {
            BarAnonConvNote::BarNote(n) => n.body.memo.clone(),
            BarAnonConvNote::ArNote(n) => n.body.memo.clone(),
        }
    }

    #[inline(always)]
    /// Sets the nonce for the operation
    pub fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.nonce = nonce;
    }

    #[inline(always)]
    /// Fetches the nonce of the operation
    pub fn get_nonce(&self) -> NoReplayToken {
        self.nonce
    }
}

/// AbarConvNote
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum AbarConvNote {
    /// Conversion to a amount or type confidential BAR
    AbarToBar(Box<AbarToBarNote>),
    /// Conversion to a transparent BAR
    AbarToAr(Box<AbarToArNote>),
}

impl AbarConvNote {
    /// Verifies the ZKP based on the type of conversion
    pub fn verify<D: Digest<OutputSize = U64> + Default>(
        &self,
        merkle_root: BN254Scalar,
        hasher: D,
    ) -> ruc::Result<()> {
        match self {
            AbarConvNote::AbarToBar(note) => {
                let af = match note.folding_instance {
                    AXfrAddressFoldingInstance::Secp256k1(_) => AddressFormat::SECP256K1,
                    AXfrAddressFoldingInstance::Ed25519(_) => AddressFormat::ED25519,
                };
                let abar_to_bar_verifier_params =
                    VerifierParams::get_abar_to_bar(af).c(d!())?;
                // An axfr_abar_conv requires versioned merkle root hash for verification.
                // verify zk proof with merkle root
                verify_abar_to_bar_note(
                    &abar_to_bar_verifier_params,
                    &note,
                    &merkle_root,
                    hasher,
                )
                .c(d!("Abar to Bar conversion proof verification failed"))
            }
            AbarConvNote::AbarToAr(note) => {
                let af = match note.folding_instance {
                    AXfrAddressFoldingInstance::Secp256k1(_) => AddressFormat::SECP256K1,
                    AXfrAddressFoldingInstance::Ed25519(_) => AddressFormat::ED25519,
                };
                let abar_to_ar_verifier_params =
                    VerifierParams::get_abar_to_ar(af).c(d!())?;
                // An axfr_abar_conv requires versioned merkle root hash for verification.
                // verify zk proof with merkle root
                verify_abar_to_ar_note(
                    &abar_to_ar_verifier_params,
                    &note,
                    &merkle_root,
                    hasher,
                )
                .c(d!("Abar to AR conversion proof verification failed"))
            }
        }
    }

    /// input nullifier in the note body
    pub fn get_input(&self) -> Nullifier {
        match self {
            AbarConvNote::AbarToBar(note) => note.body.input,
            AbarConvNote::AbarToAr(note) => note.body.input,
        }
    }

    /// merkle root version of the proof
    pub fn get_merkle_root_version(&self) -> u64 {
        match self {
            AbarConvNote::AbarToBar(note) => note.body.merkle_root_version,
            AbarConvNote::AbarToAr(note) => note.body.merkle_root_version,
        }
    }

    /// public key of the note body
    pub fn get_public_key(&self) -> XfrPublicKey {
        match self {
            AbarConvNote::AbarToBar(note) => {
                XfrPublicKey::from_noah(&note.body.output.public_key).unwrap()
            }
            AbarConvNote::AbarToAr(note) => {
                XfrPublicKey::from_noah(&note.body.output.public_key).unwrap()
            }
        }
    }

    /// output BAR of the note body
    pub fn get_output(&self) -> BlindAssetRecord {
        match self {
            AbarConvNote::AbarToBar(note) => {
                BlindAssetRecord::from_noah(&note.body.output).unwrap()
            }
            AbarConvNote::AbarToAr(note) => {
                BlindAssetRecord::from_noah(&note.body.output).unwrap()
            }
        }
    }

    /// gets address of owner memo in the note body
    pub fn get_owner_memos_ref(&self) -> Vec<Option<OwnerMemo>> {
        match self {
            AbarConvNote::AbarToBar(note) => {
                vec![note
                    .body
                    .memo
                    .as_ref()
                    .map(|om| OwnerMemo::from_noah(om).unwrap())]
            }
            AbarConvNote::AbarToAr(note) => {
                vec![note
                    .body
                    .memo
                    .as_ref()
                    .map(|om| OwnerMemo::from_noah(om).unwrap())]
            }
        }
    }

    /// get serialized bytes for signature and prove (only ABAR body).
    pub fn digest(&self) -> Vec<u8> {
        match self {
            AbarConvNote::AbarToBar(note) => {
                Serialized::new(&note.body).as_ref().to_vec()
            }
            AbarConvNote::AbarToAr(note) => {
                Serialized::new(&note.body).as_ref().to_vec()
            }
        }
    }
}

/// Operation for converting a Blind Asset Record to a Anonymous record
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AbarToBarOps {
    /// the note which contains the inp/op and ZKP
    pub note: AbarConvNote,
    nonce: NoReplayToken,
}

impl AbarToBarOps {
    /// Generates a new BarToAbarOps object
    pub fn new(note: AbarConvNote, nonce: NoReplayToken) -> Result<AbarToBarOps> {
        Ok(AbarToBarOps { note, nonce })
    }

    #[inline(always)]
    /// Sets the nonce for the operation
    pub fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.nonce = nonce;
    }

    #[inline(always)]
    /// Fetches the nonce of the operation
    pub fn get_nonce(&self) -> NoReplayToken {
        self.nonce
    }
}

/// A struct to hold the transfer ops
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AnonTransferOps {
    /// The note which holds the signatures, the ZKF and memo
    pub note: AXfrNote,
    nonce: NoReplayToken,
}
impl AnonTransferOps {
    /// Generates the anon transfer note
    pub fn new(note: AXfrNote, nonce: NoReplayToken) -> Result<AnonTransferOps> {
        Ok(AnonTransferOps { note, nonce })
    }

    /// Sets the nonce for the operation
    #[inline(always)]
    #[allow(dead_code)]
    fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.nonce = nonce;
    }

    /// Fetches the nonce of the operation
    #[inline(always)]
    fn get_nonce(&self) -> NoReplayToken {
        self.nonce
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
    /// Anonymous conversion operation
    BarToAbar(Box<BarToAbarOps>),
    /// De-anonymize ABAR operation
    AbarToBar(Box<AbarToBarOps>),
    /// Anonymous transfer operation
    TransferAnonAsset(Box<AnonTransferOps>),
    ///replace staker.
    ReplaceStaker(ReplaceStakerOps),
}

impl Operation {
    /// get serialized bytes for signature and prove.
    pub fn digest(&self) -> Vec<u8> {
        match self {
            Operation::UpdateStaker(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::Delegation(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::UnDelegation(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::Claim(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::FraDistribution(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::UpdateValidator(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::Governance(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::UpdateMemo(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::ConvertAccount(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::BarToAbar(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::ReplaceStaker(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::TransferAsset(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::IssueAsset(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::DefineAsset(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::MintFra(i) => Serialized::new(i).as_ref().to_vec(),
            Operation::AbarToBar(i) => i.note.digest(),
            Operation::TransferAnonAsset(i) => {
                Serialized::new(&i.note.body).as_ref().to_vec()
            }
        }
    }
}

fn set_no_replay_token(op: &mut Operation, no_replay_token: NoReplayToken) {
    match op {
        Operation::UpdateStaker(i) => i.set_nonce(no_replay_token),
        Operation::Delegation(i) => i.set_nonce(no_replay_token),
        Operation::UnDelegation(i) => i.set_nonce(no_replay_token),
        Operation::Claim(i) => i.set_nonce(no_replay_token),
        Operation::FraDistribution(i) => i.set_nonce(no_replay_token),
        Operation::UpdateValidator(i) => i.set_nonce(no_replay_token),
        Operation::Governance(i) => i.set_nonce(no_replay_token),
        Operation::UpdateMemo(i) => i.body.no_replay_token = no_replay_token,
        Operation::ConvertAccount(i) => i.set_nonce(no_replay_token),
        Operation::BarToAbar(i) => i.set_nonce(no_replay_token),
        Operation::AbarToBar(i) => i.set_nonce(no_replay_token),
        Operation::TransferAnonAsset(i) => i.set_nonce(no_replay_token),
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

    /// get serialized bytes for signature and prove.
    pub fn digest(&self) -> Vec<u8> {
        let mut bytes = vec![];
        bytes.extend_from_slice(Serialized::new(&self.no_replay_token).as_ref());
        bytes.extend_from_slice(Serialized::new(&self.credentials).as_ref());
        bytes.extend_from_slice(Serialized::new(&self.policy_options).as_ref());
        bytes.extend_from_slice(Serialized::new(&self.memos).as_ref());
        for o in &self.operations {
            bytes.extend_from_slice(&o.digest());
        }
        bytes
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct Transaction {
    pub body: TransactionBody,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub signatures: Vec<SignatureOf<TransactionBody>>,
    #[serde(default)]
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub pubkey_sign_map: HashMap<XfrPublicKey, SignatureOf<TransactionBody>>,
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FinalizedTransaction {
    pub txn: Transaction,
    pub tx_id: TxnSID,
    pub txo_ids: Vec<TxoSID>,
    #[serde(default)]
    pub atxo_ids: Vec<ATxoSID>,

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
pub const ASSET_TYPE_FRA: NoahAssetType = NoahAssetType([0; ASSET_TYPE_LENGTH]);

/// FRA decimals
pub const FRA_DECIMALS: u8 = 6;

lazy_static! {
    /// The destination of Fee is an black hole,
    /// all token transfered to it will be burned.
    pub static ref BLACK_HOLE_PUBKEY: NoahXfrPublicKey = pnk!(NoahXfrPublicKey::noah_from_bytes(&[0; ed25519_dalek::PUBLIC_KEY_LENGTH][..]));
    /// BlackHole of Staking
    pub static ref BLACK_HOLE_PUBKEY_STAKING: NoahXfrPublicKey = pnk!(NoahXfrPublicKey::noah_from_bytes(&[1; ed25519_dalek::PUBLIC_KEY_LENGTH][..]));
}

/// see [**mainnet-v0.1 defination**](https://www.notion.so/findora/Transaction-Fees-Analysis-d657247b70f44a699d50e1b01b8a2287)
pub const TX_FEE_MIN: u64 = 10_000; // 0.01 FRA
/// Double the
pub const BAR_TO_ABAR_TX_FEE_MIN: u64 = 20_000; // 0.02 FRA (2*TX_FEE_MIN)

/// Calculate the FEE with inputs and outputs number.
pub const FEE_CALCULATING_FUNC: fn(u32, u32) -> u32 = |x: u32, y: u32| {
    let extra_outputs = y.saturating_sub(x);
    50_0000 + 10_0000 * x + 20_0000 * y + (10_000 * extra_outputs)
};

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

    #[allow(clippy::if_same_then_else)]
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

        let mut min_fee = TX_FEE_MIN;
        // Charge double the min fee if the transaction is BarToAbar
        for op in self.body.operations.iter() {
            if let Operation::BarToAbar(_a) = op {
                min_fee = BAR_TO_ABAR_TX_FEE_MIN;
            }
        }

        self.is_coinbase_tx()
            || self.body.operations.iter().any(|ops| {
                if let Operation::TransferAsset(ref x) = ops {
                    return x.body.outputs.iter().any(|o| {
                        if let XfrAssetType::NonConfidential(ty) = o.record.asset_type {
                            if ty == ASSET_TYPE_FRA
                                && XfrPublicKey::from_noah(&BLACK_HOLE_PUBKEY).unwrap()
                                    == o.record.public_key
                            {
                                if let XfrAmount::NonConfidential(am) = o.record.amount {
                                    if am > (min_fee - 1) {
                                        return true;
                                    }
                                }
                            }
                        }
                        tracing::error!("Txn failed in check_fee {:?}", self);
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
                } else if let Operation::TransferAnonAsset(_) = ops {
                    return true;
                } else if let Operation::BarToAbar(_) = ops {
                    return true;
                } else if let Operation::AbarToBar(_) = ops {
                    return true;
                } else if matches!(ops, Operation::UpdateValidator(_)) {
                    return true;
                }
                tracing::error!("Txn failed in check_fee {:?}", self);
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
            pubkey_sign_map: Default::default(),
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
            pubkey_sign_map: Default::default(),
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
    pub fn sign_to_map(&mut self, keypair: &XfrKeyPair) {
        self.pubkey_sign_map
            .insert(keypair.pub_key, SignatureOf::new(keypair, &self.body));
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
    pub fn get_owner_memos_ref(&self) -> Vec<Option<OwnerMemo>> {
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
                Operation::AbarToBar(abar_to_bar) => {
                    memos.append(&mut abar_to_bar.note.get_owner_memos_ref());
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
            match sig.0.verify(&public_key, &serialized) {
                Err(_) => {}
                Ok(_) => {
                    return Ok(());
                }
            }
        }
        Err(eg!())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn check_has_signature_from_map(&self, public_key: &XfrPublicKey) -> Result<()> {
        if let Some(sign) = self.pubkey_sign_map.get(public_key) {
            sign.0.verify(public_key, &Serialized::new(&self.body))
        } else {
            Err(eg!(
                "the pubkey not match: {}",
                public_key_to_base64(public_key)
            ))
        }
    }

    /// NOTE: This method is used to verify the signature in the transaction,
    /// when the user constructs the transaction not only needs to sign each `operation`,
    /// but also needs to sign the whole transaction, otherwise it will not be passed here
    #[allow(missing_docs)]
    #[inline(always)]
    pub fn check_tx(&self) -> Result<()> {
        let select_check = |tx: &Transaction, pk: &XfrPublicKey| -> Result<()> {
            if tx.signatures.is_empty() {
                tx.check_has_signature_from_map(pk)
            } else {
                tx.check_has_signature(pk)
            }
        };

        for operation in self.body.operations.iter() {
            match operation {
                Operation::TransferAsset(o) => {
                    for pk in o.get_owner_addresses().iter() {
                        select_check(self, pk).c(d!())?;
                    }
                }
                Operation::IssueAsset(o) => {
                    select_check(self, &o.pubkey.key).c(d!())?;
                }
                Operation::DefineAsset(o) => {
                    select_check(self, &o.pubkey.key).c(d!())?;
                }
                Operation::UpdateMemo(o) => {
                    select_check(self, &o.pubkey).c(d!())?;
                }
                Operation::UpdateStaker(o) => {
                    select_check(self, &o.pubkey).c(d!())?;
                }
                Operation::Delegation(o) => {
                    select_check(self, &o.pubkey).c(d!())?;
                }
                Operation::UnDelegation(o) => {
                    select_check(self, &o.pubkey).c(d!())?;
                }
                Operation::Claim(o) => {
                    select_check(self, &o.pubkey).c(d!())?;
                }
                Operation::UpdateValidator(_) => {}
                Operation::Governance(_) => {}
                Operation::FraDistribution(_) => {}
                Operation::MintFra(_) => {}
                Operation::ConvertAccount(o) => {
                    select_check(self, &o.signer).c(d!())?;
                }
                Operation::ReplaceStaker(o) => {
                    if !o.get_related_pubkeys().is_empty() {
                        for pk in o.get_related_pubkeys() {
                            select_check(self, &pk).c(d!())?;
                        }
                    }
                }
                Operation::BarToAbar(_) => {}
                Operation::AbarToBar(_) => {}
                Operation::TransferAnonAsset(_) => {}
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

/// Commitment data for Anon merkle trees
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AnonStateCommitmentData {
    /// Root hash of the latest committed version of abar merkle tree
    pub abar_root_hash: BN254Scalar,
    /// Root hash of the nullifier set merkle tree
    pub nullifier_root_hash: BitDigest,
}

impl AnonStateCommitmentData {
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

#[inline(always)]
#[allow(missing_docs)]
pub fn get_abar_commitment(oabar: OpenAnonAssetRecord) -> BN254Scalar {
    let c = commit(
        oabar.pub_key_ref(),
        oabar.get_blind(),
        oabar.get_amount(),
        oabar.get_asset_type().as_scalar(),
    )
    .unwrap();
    c.0
}

#[derive(Serialize, Deserialize)]
#[allow(missing_docs)]
pub struct ABARData {
    pub commitment: String,
}
