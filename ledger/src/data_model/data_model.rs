#![allow(clippy::field_reassign_with_default)]
#![allow(clippy::assertions_on_constants)]
extern crate unicode_normalization;

use super::errors;
use crate::policy_script::{Policy, PolicyGlobals, TxnPolicyData};
use crate::{des_fail, error_location, ser_fail, zei_fail};
use air::{check_merkle_proof as air_check_merkle_proof, AIRResult};
use bitmap::SparseMap;
use chrono::prelude::*;
use credentials::{CredCommitment, CredIssuerPublicKey, CredPoK, CredUserPublicKey};
use cryptohash::sha256::Digest as BitDigest;
use cryptohash::sha256::DIGESTBYTES;
use cryptohash::HashValue;
use errors::PlatformError;
use lazy_static::lazy_static;
use rand::Rng;
use rand_chacha::rand_core;
use rand_chacha::ChaChaRng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};
use sparse_merkle_tree::{check_merkle_proof, Key, MerkleProof};
use std::boxed::Box;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
// use std::convert::TryInto;
use std::hash::{Hash, Hasher};
use unicode_normalization::UnicodeNormalization;
use utils::{HashOf, ProofOf, Serialized, SignatureOf};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::lib::{gen_xfr_body, XfrNotePolicies};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{
    AssetRecord, AssetRecordTemplate, AssetTracingPolicies, AssetTracingPolicy,
    AssetType as ZeiAssetType, BlindAssetRecord, OwnerMemo, XfrAmount, XfrAssetType,
    XfrBody, ASSET_TYPE_LENGTH,
};

use super::effects::*;
use std::error::Error;
use std::ops::Deref;

pub const RANDOM_CODE_LENGTH: usize = 16;
pub const TRANSACTION_WINDOW_WIDTH: usize = 128;
pub const MAX_DECIMALS_LENGTH: u8 = 19;

pub fn b64enc<T: ?Sized + AsRef<[u8]>>(input: &T) -> String {
    base64::encode_config(input, base64::URL_SAFE)
}
pub fn b64dec<T: ?Sized + AsRef<[u8]>>(
    input: &T,
) -> Result<Vec<u8>, base64::DecodeError> {
    base64::decode_config(input, base64::URL_SAFE)
}

// Unique Identifier for ledger objects
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct Code {
    pub val: [u8; 16],
}

const ZERO_256: [u8; DIGESTBYTES] = [0; DIGESTBYTES];
const ZERO_DIGEST: BitDigest = BitDigest { 0: ZERO_256 };

fn default_digest() -> BitDigest {
    ZERO_DIGEST
}

fn is_default_digest(x: &BitDigest) -> bool {
    x == &ZERO_DIGEST
}

fn is_default<T: Default + PartialEq>(x: &T) -> bool {
    x == &T::default()
}

const UTF8_ASSET_TYPES_WORK: bool = false;

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct AssetTypeCode {
    pub val: ZeiAssetType,
}

impl AssetTypeCode {
    /// Randomly generates a 16-byte data and encodes it with base64 to a utf8 string.
    ///
    /// The utf8 can then be converted to an asset type code using `new_from_utf8_safe` or `new_from_utf8_truncate`.
    pub fn gen_utf8_random() -> String {
        let mut rng = ChaChaRng::from_entropy();
        let mut buf: [u8; RANDOM_CODE_LENGTH] = [0u8; RANDOM_CODE_LENGTH];
        rng.fill_bytes(&mut buf);
        b64enc(&buf)
    }

    /// Randomly generates an asset type code
    pub fn gen_random() -> Self {
        Self::gen_random_with_rng(&mut ChaChaRng::from_entropy())
    }

    pub fn gen_random_with_rng<R: RngCore + CryptoRng>(prng: &mut R) -> Self {
        let val: [u8; ASSET_TYPE_LENGTH] = prng.gen();
        Self {
            val: ZeiAssetType(val),
        }
    }

    /// Returns whether the input is longer than 32 bytes, and thus will be truncated to construct an asset type code.
    pub fn will_truncate(bytes: &[u8]) -> bool {
        bytes.len() > ASSET_TYPE_LENGTH
    }

    /// Converts a vector to an asset type code.
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
    pub fn new_from_utf8_safe(s: &str) -> Result<Self, PlatformError> {
        assert!(UTF8_ASSET_TYPES_WORK);
        let composed = s.to_string().nfc().collect::<String>().into_bytes();
        if AssetTypeCode::will_truncate(&composed) {
            return Err(PlatformError::InputsError(error_location!()));
        }
        Ok(AssetTypeCode::new_from_vec(composed))
    }

    /// Converts an utf8 string to an asset type code.
    /// Truncates the code if the length is greater than 32 bytes.
    ///
    /// Used to customize the asset type code.
    pub fn new_from_utf8_truncate(s: &str) -> Self {
        assert!(UTF8_ASSET_TYPES_WORK);
        let composed = s.to_string().nfc().collect::<String>().into_bytes();
        AssetTypeCode::new_from_vec(composed)
    }

    /// Converts the asset type code to an utf8 string.
    ///
    /// Used to display the asset type code.
    pub fn to_utf8(&self) -> Result<String, PlatformError> {
        assert!(UTF8_ASSET_TYPES_WORK);
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
                        return Err(ser_fail!(e));
                    }
                };
            }
        }
        Ok("".to_string())
    }

    pub fn new_from_str(s: &str) -> Self {
        let mut as_vec = s.to_string().into_bytes();
        as_vec.resize(ASSET_TYPE_LENGTH, 0u8);
        let buf = <[u8; ASSET_TYPE_LENGTH]>::try_from(as_vec.as_slice()).unwrap();
        Self {
            val: ZeiAssetType(buf),
        }
    }

    pub fn new_from_base64(b64: &str) -> Result<Self, PlatformError> {
        match b64dec(b64) {
            Ok(mut bin) => {
                bin.resize(ASSET_TYPE_LENGTH, 0u8);
                let buf = <[u8; ASSET_TYPE_LENGTH]>::try_from(bin.as_slice()).unwrap();
                Ok(Self {
                    val: ZeiAssetType(buf),
                })
            }
            Err(e) => Err(des_fail!(format!(
                "Failed to deserialize base64 '{}': {}",
                b64, e
            ))),
        }
    }
    pub fn to_base64(&self) -> String {
        b64enc(&self.val.0)
    }
}

pub type AssetPolicyKey = Code;
pub type SmartContractKey = Code;

impl Code {
    pub fn gen_random() -> Self {
        let mut small_rng = ChaChaRng::from_entropy();
        let mut buf: [u8; 16] = [0u8; 16];
        small_rng.fill_bytes(&mut buf);
        Self { val: buf }
    }
    pub fn gen_random_with_rng<R: Rng>(mut prng: R) -> Self {
        let val: [u8; 16] = prng.gen();
        Self { val }
    }
    pub fn new_from_str(s: &str) -> Self {
        let mut as_vec = s.to_string().into_bytes();
        as_vec.resize(16, 0u8);
        let buf = <[u8; 16]>::try_from(as_vec.as_slice()).unwrap();
        Self { val: buf }
    }
    pub fn new_from_base64(b64: &str) -> Result<Self, PlatformError> {
        if let Ok(mut bin) = b64dec(b64) {
            bin.resize(16, 0u8);
            let buf = <[u8; 16]>::try_from(bin.as_slice()).unwrap();
            Ok(Self { val: buf })
        } else {
            Err(des_fail!())
        }
    }
    pub fn to_base64(&self) -> String {
        b64enc(&self.val)
    }
}

impl Serialize for Code {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
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
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct CodeVisitor;

        impl<'de> Visitor<'de> for CodeVisitor {
            type Value = Code;

            fn expecting(
                &self,
                formatter: &mut ::core::fmt::Formatter,
            ) -> ::core::fmt::Result {
                formatter.write_str("an array of 16 bytes")
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
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
            fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
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

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct AssetDigest {
    // Generated from the asset definition, also unique
    pub val: [u8; 32],
}

#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Memo(pub String);
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ConfidentialMemo;
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Commitment([u8; 32]);

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct XfrAddress {
    pub key: XfrPublicKey,
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for XfrAddress {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.as_bytes().hash(state);
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct IssuerPublicKey {
    pub key: XfrPublicKey,
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for IssuerPublicKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.as_bytes().hash(state);
    }
}

impl Deref for IssuerPublicKey {
    type Target = XfrPublicKey;
    fn deref(&self) -> &Self::Target {
        &self.key
    }
}

#[derive(Debug)]
pub struct IssuerKeyPair<'a> {
    pub keypair: &'a XfrKeyPair,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct AccountAddress {
    pub key: XfrPublicKey,
}

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
    pub fn verify(&self, message: &T) -> bool {
        self.signature
            .verify(&self.address.key, &(message.clone(), self.input_idx))
            .is_ok()
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Stores threshold and weights for a multisignature requirement.
pub struct SignatureRules {
    pub threshold: u64,
    pub weights: Vec<(XfrPublicKey, u64)>, // Stored as a vector so that serialization is deterministic
}

impl SignatureRules {
    /// Returns Ok(()) if the sum of weights of the keys in keyset reaches the threshold.
    /// Keyset must store XfrPublicKeys in byte form.
    pub fn check_signature_set(
        &self,
        keyset: &HashSet<Vec<u8>>,
    ) -> Result<(), PlatformError> {
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
                .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        }

        if sum < self.threshold {
            return Err(PlatformError::InputsError(error_location!()));
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Simple asset rules:
/// 1) Transferable: Non-transferable assets can only be transferred once from the issuer to
///    another user.
/// 2) Updatable: Whether the asset memo can be updated.
/// 3) Transfer signature rules: Signature weights and threshold for a valid transfer.
/// 4) Asset tracing policies: A bundle of tracing policies specifying the tracing proofs that
///    constitute a valid transfer.
/// 5) Max units: Optional limit on total issuance amount.
pub struct AssetRules {
    pub transferable: bool,
    pub updatable: bool,
    pub transfer_multisig_rules: Option<SignatureRules>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub tracing_policies: AssetTracingPolicies,
    pub max_units: Option<u64>,
    pub decimals: u8,
}
impl Default for AssetRules {
    fn default() -> Self {
        AssetRules {
            tracing_policies: AssetTracingPolicies::new(),
            transferable: true,
            updatable: false,
            max_units: None,
            transfer_multisig_rules: None,
            decimals: 0,
        }
    }
}

impl AssetRules {
    pub fn add_tracing_policy(&mut self, policy: AssetTracingPolicy) -> &mut Self {
        self.tracing_policies.add(policy);
        self
    }

    pub fn set_max_units(&mut self, max_units: Option<u64>) -> &mut Self {
        self.max_units = max_units;
        self
    }

    pub fn set_transferable(&mut self, transferable: bool) -> &mut Self {
        self.transferable = transferable;
        self
    }

    pub fn set_updatable(&mut self, updatable: bool) -> &mut Self {
        self.updatable = updatable;
        self
    }

    pub fn set_transfer_multisig_rules(
        &mut self,
        multisig_rules: Option<SignatureRules>,
    ) -> &mut Self {
        self.transfer_multisig_rules = multisig_rules;
        self
    }

    #[inline(always)]
    pub fn set_decimals(&mut self, decimals: u8) -> Result<&mut Self, Box<dyn Error>> {
        if decimals > MAX_DECIMALS_LENGTH {
            return Err("asset decimals should be less than 20".into());
        }
        self.decimals = decimals;
        Ok(self)
    }
}

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
    #[serde(skip_serializing_if = "is_default")]
    pub asset_rules: AssetRules,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub policy: Option<(Box<Policy>, PolicyGlobals)>,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
// Note: if the properties field of this struct is changed, update the comment for AssetType::from_json in wasm_data_model.rs as well.
pub struct AssetType {
    pub properties: Asset,
    pub digest: [u8; 32],
    pub units: u64,
    pub confidential_units: Commitment,
}

impl AssetType {
    pub fn has_issuance_restrictions(&self) -> bool {
        self.properties.asset_rules.max_units.is_some()
    }

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

            ret
        };
        self.properties != simple_asset
    }

    pub fn get_tracing_policies_ref(&self) -> &AssetTracingPolicies {
        &self.properties.asset_rules.tracing_policies
    }
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CustomAssetPolicy {
    policy: Vec<u8>, // serialized policy, underlying form TBD.
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CredentialProofKey([u8; 16]);

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CredentialProof {
    pub key: CredentialProofKey,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct SmartContract;

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct TxoSID(pub u64);

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct OutputPosition(pub usize);

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct TxnSID(pub usize);

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct BlockSID(pub usize);

// An ephemeral index for a transaction (with a different newtype so that
// it's harder to mix up)
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct TxnTempSID(pub usize);

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TxOutput {
    pub record: BlindAssetRecord,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub lien: Option<HashOf<Vec<TxOutput>>>,
}

#[derive(Eq, Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum UtxoStatus {
    Spent,
    Unspent,
    Nonexistent,
}

pub enum LienEntry {
    SimpleTxo(TxOutput),
    Lien(TxOutput, Vec<LienEntry>),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Utxo(pub TxOutput);

#[derive(Copy, Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum TxoRef {
    // Offset backwards from this operation (within a txn) -- 0 is the most recent, (n-1) (if there
    // are n outputs so far) is the first output of the transaction
    Relative(u64),
    // Absolute Txo address to a location outside this txn
    Absolute(TxoSID),
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct NoReplayToken([u8; 8], u64);

impl NoReplayToken {
    pub fn new<R: RngCore>(prng: &mut R, seq_id: u64) -> Self {
        NoReplayToken(prng.next_u64().to_be_bytes(), seq_id)
    }

    pub fn testonly_new(rand: u64, seq_id: u64) -> Self {
        NoReplayToken(rand.to_be_bytes(), seq_id)
    }

    pub fn get_seq_id(&self) -> u64 {
        self.1
    }

    pub fn get_rand(&self) -> [u8; 8] {
        self.0
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TransferAssetBody {
    pub inputs: Vec<TxoRef>, // Ledger address of inputs
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub policies: XfrNotePolicies,
    pub outputs: Vec<TxOutput>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    // (inp_idx,out_idx,hash) triples signifying that the lien `hash` on
    // the input `inp_idx` gets assigned to the output `out_idx`
    pub lien_assignments: Vec<(usize, usize, HashOf<Vec<TxOutput>>)>,
    // TODO(joe): we probably don't need the whole XfrNote with input records
    // once it's on the chain
    pub transfer: Box<XfrBody>, // Encrypted transfer note

    pub transfer_type: TransferType,
}

impl TransferAssetBody {
    #[allow(clippy::too_many_arguments)]
    pub fn new<R: CryptoRng + RngCore>(
        prng: &mut R,
        input_refs: Vec<TxoRef>,
        input_records: &[AssetRecord],
        output_records: &[AssetRecord],
        policies: Option<XfrNotePolicies>,
        lien_assignments: Vec<(usize, usize, HashOf<Vec<TxOutput>>)>,
        transfer_type: TransferType,
    ) -> Result<TransferAssetBody, errors::PlatformError> {
        let num_inputs = input_records.len();
        let num_outputs = output_records.len();

        if num_inputs == 0 {
            return Err(PlatformError::InputsError(error_location!()));
        }

        // If no policies specified, construct set of empty policies
        let policies = policies.unwrap_or_else(|| {
            let no_policies = AssetTracingPolicies::new();
            XfrNotePolicies::new(
                vec![no_policies.clone(); num_inputs],
                vec![None; num_inputs],
                vec![no_policies; num_outputs],
                vec![None; num_outputs],
            )
        });

        // Verify that for each input and output, there is a corresponding policy and credential commitment
        if num_inputs != policies.inputs_tracking_policies.len()
            || num_inputs != policies.inputs_sig_commitments.len()
            || num_outputs != policies.outputs_tracking_policies.len()
            || num_outputs != policies.outputs_sig_commitments.len()
        {
            return Err(PlatformError::InputsError(error_location!()));
        }

        let transfer = Box::new(
            gen_xfr_body(prng, input_records, output_records)
                .map_err(|e| PlatformError::ZeiError(error_location!(), e))?,
        );
        let outputs = transfer
            .outputs
            .iter()
            .map(|rec| TxOutput {
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
    pub fn verify_body_signature(
        &self,
        signature: &IndexedSignature<TransferAssetBody>,
    ) -> bool {
        signature.verify(&self)
    }
}

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
    pub fn new(
        token_code: &AssetTypeCode,
        seq_num: u64,
        records: &[(TxOutput, Option<OwnerMemo>)],
    ) -> Result<IssueAssetBody, PlatformError> {
        Ok(IssueAssetBody {
            code: *token_code,
            seq_num,
            num_outputs: records.len(),
            records: records.to_vec(),
        })
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct DefineAssetBody {
    pub asset: Box<Asset>,
}

impl DefineAssetBody {
    pub fn new(
        token_code: &AssetTypeCode,
        issuer_key: &IssuerPublicKey,
        asset_rules: AssetRules,
        memo: Option<Memo>,
        confidential_memo: Option<ConfidentialMemo>,
        policy: Option<(Box<Policy>, PolicyGlobals)>,
    ) -> Result<DefineAssetBody, PlatformError> {
        let mut asset_def: Asset = Default::default();
        asset_def.code = *token_code;
        asset_def.issuer = *issuer_key;
        asset_def.asset_rules = asset_rules;
        asset_def.policy = policy;

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

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct UpdateMemoBody {
    pub new_memo: Memo,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub asset_type: AssetTypeCode,
    pub no_replay_token: NoReplayToken,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AIRAssignBody {
    pub addr: CredUserPublicKey,
    pub data: CredCommitment,
    pub issuer_pk: CredIssuerPublicKey,
    pub pok: CredPoK,
    pub no_replay_token: NoReplayToken,
}

impl AIRAssignBody {
    pub fn new(
        addr: CredUserPublicKey,
        data: CredCommitment,
        issuer_pk: CredIssuerPublicKey,
        pok: CredPoK,
        no_replay_token: NoReplayToken,
    ) -> Result<AIRAssignBody, errors::PlatformError> {
        Ok(AIRAssignBody {
            addr,
            data,
            issuer_pk,
            pok,
            no_replay_token,
        })
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum TransferType {
    Standard,
    DebtSwap,
}

impl Default for TransferType {
    fn default() -> Self {
        Self::Standard
    }
}

/// Enum indicating whether an output appearning in transaction is internally spent
pub enum OutputSpentStatus {
    Spent,
    Unspent,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TransferAsset {
    pub body: TransferAssetBody,
    pub body_signatures: Vec<IndexedSignature<TransferAssetBody>>,
}

impl TransferAsset {
    pub fn new(
        transfer_body: TransferAssetBody,
    ) -> Result<TransferAsset, PlatformError> {
        Ok(TransferAsset {
            body: transfer_body,
            body_signatures: Vec::new(),
        })
    }

    pub fn sign(&mut self, keypair: &XfrKeyPair) {
        let sig = self.create_input_signature(keypair);
        self.attach_signature(sig).unwrap()
    }

    pub fn sign_cosignature(&mut self, keypair: &XfrKeyPair, input_idx: usize) {
        let sig = self.create_cosignature(keypair, input_idx);
        self.attach_signature(sig).unwrap()
    }

    pub fn attach_signature(
        &mut self,
        sig: IndexedSignature<TransferAssetBody>,
    ) -> Result<(), PlatformError> {
        if !sig.verify(&self.body) {
            return Err(PlatformError::InputsError(error_location!()));
        }
        self.body_signatures.push(sig);
        Ok(())
    }

    pub fn create_input_signature(
        &self,
        keypair: &XfrKeyPair,
    ) -> IndexedSignature<TransferAssetBody> {
        self.body.compute_body_signature(keypair, None)
    }

    pub fn create_cosignature(
        &self,
        keypair: &XfrKeyPair,
        input_idx: usize,
    ) -> IndexedSignature<TransferAssetBody> {
        self.body.compute_body_signature(keypair, Some(input_idx))
    }

    pub fn get_owner_memos_ref(&self) -> Vec<Option<&OwnerMemo>> {
        self.body
            .transfer
            .owners_memos
            .iter()
            .map(|mem| mem.as_ref())
            .collect()
    }

    pub fn get_outputs_ref(&self) -> Vec<&TxOutput> {
        self.body.outputs.iter().collect()
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct IssueAsset {
    pub body: IssueAssetBody,
    pub pubkey: IssuerPublicKey,
    pub signature: SignatureOf<IssueAssetBody>,
}

impl IssueAsset {
    pub fn new(
        issuance_body: IssueAssetBody,
        keypair: &IssuerKeyPair,
    ) -> Result<IssueAsset, PlatformError> {
        let signature = SignatureOf::new(&keypair.keypair, &issuance_body);
        Ok(IssueAsset {
            body: issuance_body,
            pubkey: IssuerPublicKey {
                key: *keypair.keypair.get_pk_ref(),
            },
            signature,
        })
    }

    pub fn get_owner_memos_ref(&self) -> Vec<Option<&OwnerMemo>> {
        self.body
            .records
            .iter()
            .map(|(_, memo)| memo.as_ref())
            .collect()
    }
    pub fn get_outputs_ref(&self) -> Vec<&TxOutput> {
        self.body.records.iter().map(|rec| &rec.0).collect()
    }
}

// ... etc...
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct DefineAsset {
    pub body: DefineAssetBody,

    pub pubkey: IssuerPublicKey,
    pub signature: SignatureOf<DefineAssetBody>,
}

impl DefineAsset {
    pub fn new(
        creation_body: DefineAssetBody,
        keypair: &IssuerKeyPair,
    ) -> Result<DefineAsset, PlatformError> {
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

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct UpdateMemo {
    pub body: UpdateMemoBody,
    pub pubkey: XfrPublicKey,
    pub signature: SignatureOf<UpdateMemoBody>,
}

impl UpdateMemo {
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

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AIRAssign {
    pub body: Box<AIRAssignBody>,
    pub pubkey: XfrPublicKey,
    pub signature: SignatureOf<AIRAssignBody>,
}

impl AIRAssign {
    pub fn new(
        creation_body: AIRAssignBody,
        keypair: &XfrKeyPair,
    ) -> Result<AIRAssign, errors::PlatformError> {
        let signature = SignatureOf::new(keypair, &creation_body);
        Ok(AIRAssign {
            body: Box::new(creation_body),
            pubkey: *keypair.get_pk_ref(),
            signature,
        })
    }
}

// pub const KV_BLOCK_SIZE: usize = 4*(1<<10);

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct KVHash(pub HashOf<(Vec<u8>, Option<KVBlind>)>);

impl KVHash {
    pub fn new(data: &dyn AsRef<[u8]>, blind: Option<&KVBlind>) -> Self {
        KVHash(HashOf::new(&(data.as_ref().to_vec(), blind.cloned())))
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct KVBlind(pub [u8; 16]);

impl KVBlind {
    pub fn gen_random() -> Self {
        let mut small_rng = ChaChaRng::from_entropy();
        let mut buf: [u8; 16] = [0u8; 16];
        small_rng.fill_bytes(&mut buf);
        Self(buf)
    }

    pub fn to_base64(&self) -> String {
        b64enc(&self.0)
    }

    pub fn from_base64(b64: &str) -> Result<Self, PlatformError> {
        if let Ok(mut bin) = b64dec(b64) {
            bin.resize(16, 0u8);
            let buf = <[u8; 16]>::try_from(bin.as_slice()).unwrap();
            Ok(Self(buf))
        } else {
            Err(des_fail!())
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct KVEntry(pub XfrPublicKey, pub KVHash);

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct KVUpdate {
    pub body: (Key, u64, Option<KVEntry>),
    pub signature: SignatureOf<(Key, u64, Option<KVEntry>)>,
}

pub type KVEntrySignature = SignatureOf<(Key, u64, Option<KVEntry>)>;

impl KVUpdate {
    pub fn new(
        creation_body: (Key, Option<KVHash>),
        seq_num: u64,
        keypair: &XfrKeyPair,
    ) -> KVUpdate {
        let creation_body = match creation_body {
            (k, None) => (k, seq_num, None),
            (k, Some(data)) => (k, seq_num, Some(KVEntry(*keypair.get_pk_ref(), data))),
        };
        let signature = SignatureOf::new(&keypair, &creation_body);
        KVUpdate {
            body: creation_body,
            signature,
        }
    }

    pub fn get_entry(&self) -> &Option<KVEntry> {
        &self.body.2
    }

    pub fn check_signature(
        &self,
        public_key: &XfrPublicKey,
    ) -> Result<(), PlatformError> {
        self.signature
            .verify(public_key, &self.body)
            .map_err(|e| zei_fail!(e))
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BindAssetsBody {
    pub contract: TxoRef,    // UTXO representing the lien contract
    pub inputs: Vec<TxoRef>, // the assets being held
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    // (inp_idx,hash) pairs signifying that there is a lien `hash` on the
    // input `inp_idx`
    // NOTE: `inp_idx` is relative to `inputs`, not `[contract] + inputs`
    pub input_liens: Vec<(usize, HashOf<Vec<TxOutput>>)>,
    // Note transferring the contract record & the inputs -- though with
    // only one output, which must be for the `contract`.
    pub transfer: Box<XfrBody>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BindAssets {
    pub body: BindAssetsBody,
    // Signatures authorizing each of the inputs
    pub body_signatures: Vec<IndexedSignature<BindAssetsBody>>,
}

impl BindAssetsBody {
    pub fn new<R: CryptoRng + RngCore>(
        prng: &mut R,
        contract_ref: TxoRef,
        input_refs: Vec<(TxoRef, Option<HashOf<Vec<TxOutput>>>)>,
        input_records: &[AssetRecord],
        output_record: &AssetRecord,
    ) -> Result<BindAssetsBody, errors::PlatformError> {
        if input_records.is_empty() {
            return Err(PlatformError::InputsError(error_location!()));
        }

        if 1 + input_refs.len() != input_records.len() {
            return Err(PlatformError::InputsError(error_location!()));
        }

        let out_pubkey = &output_record
            .open_asset_record
            .blind_asset_record
            .public_key;
        let mut out_records = vec![output_record.clone()];
        for i in input_records[1..].iter() {
            let open_rec = i.open_asset_record.clone();

            let amt = *open_rec.get_amount();
            let unit_code = *open_rec.get_asset_type();

            let art = open_rec.get_record_type();
            let ar = AssetRecordTemplate::with_no_asset_tracking(
                amt,
                unit_code,
                art,
                *out_pubkey,
            );
            let ar = AssetRecord::from_template_no_identity_tracking(prng, &ar).unwrap();
            out_records.push(ar);
        }

        let transfer = Box::new(
            gen_xfr_body(prng, input_records, &out_records)
                .map_err(|e| PlatformError::ZeiError(error_location!(), e))?,
        );
        let input_liens = input_refs
            .iter()
            .map(|(_l, r)| r)
            .enumerate()
            .filter_map(|(l, x)| x.clone().map(|x| (l, x)))
            .collect::<Vec<_>>();
        let input_refs = input_refs.iter().map(|(l, _r)| l).cloned().collect();
        Ok(BindAssetsBody {
            contract: contract_ref,
            inputs: input_refs,
            input_liens,
            transfer,
        })
    }

    /// Computes a body signature. A body signature represents consent to some part of the asset transfer. If an
    /// input_idx is specified, the signature is a co-signature.
    pub fn compute_body_signature(
        &self,
        keypair: &XfrKeyPair,
        input_idx: Option<usize>,
    ) -> IndexedSignature<BindAssetsBody> {
        let public_key = keypair.get_pk_ref();
        IndexedSignature {
            signature: SignatureOf::new(keypair, &(self.clone(), input_idx)),
            address: XfrAddress { key: *public_key },
            input_idx,
        }
    }

    /// Verifies a body signature
    pub fn verify_body_signature(
        &self,
        signature: &IndexedSignature<BindAssetsBody>,
    ) -> bool {
        signature.verify(&self)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ReleaseAssetsBody {
    pub contract: TxoRef, // UTXO representing the lien contract
    pub lien: HashOf<Vec<TxOutput>>, // which lien?
    pub num_outputs: usize, // how many UTXOs?
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    // (inp_idx,out_idx,hash) triples signifying that the lien `hash` on
    // the input `inp_idx` gets assigned to the output `out_idx`
    // (an inp_idx of 0 is invalid)
    pub lien_assignments: Vec<(usize, usize, HashOf<Vec<TxOutput>>)>,
    pub transfer: Box<XfrBody>, // Note transferrring the contract & the held assets
}

impl ReleaseAssetsBody {
    pub fn new<R: CryptoRng + RngCore>(
        prng: &mut R,
        input_ref: TxoRef,
        lien: HashOf<Vec<TxOutput>>,
        lien_assignments: Vec<(usize, usize, HashOf<Vec<TxOutput>>)>,
        input_records: &[AssetRecord],
        output_records: &[AssetRecord],
    ) -> Result<ReleaseAssetsBody, errors::PlatformError> {
        if input_records.is_empty() {
            return Err(PlatformError::InputsError(error_location!()));
        }
        let transfer = Box::new(
            gen_xfr_body(prng, input_records, output_records)
                .map_err(|e| PlatformError::ZeiError(error_location!(), e))?,
        );
        Ok(ReleaseAssetsBody {
            contract: input_ref,
            lien,
            num_outputs: output_records.len(),
            lien_assignments,
            transfer,
        })
    }

    /// Computes a body signature. A body signature represents consent to some part of the asset transfer. If an
    /// input_idx is specified, the signature is a co-signature.
    pub fn compute_body_signature(
        &self,
        keypair: &XfrKeyPair,
        input_idx: Option<usize>,
    ) -> IndexedSignature<ReleaseAssetsBody> {
        let public_key = keypair.get_pk_ref();
        IndexedSignature {
            signature: SignatureOf::new(keypair, &(self.clone(), input_idx)),
            address: XfrAddress { key: *public_key },
            input_idx,
        }
    }

    /// Verifies a body signature
    pub fn verify_body_signature(
        &self,
        signature: &IndexedSignature<ReleaseAssetsBody>,
    ) -> bool {
        signature.verify(&self)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ReleaseAssets {
    pub body: ReleaseAssetsBody,
    pub body_signatures: Vec<IndexedSignature<ReleaseAssetsBody>>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Operation {
    TransferAsset(TransferAsset),
    IssueAsset(IssueAsset),
    DefineAsset(DefineAsset),
    UpdateMemo(UpdateMemo),
    AIRAssign(AIRAssign),
    KVStoreUpdate(KVUpdate),
    BindAssets(BindAssets),
    ReleaseAssets(ReleaseAssets),
    // ... etc...
}

fn set_no_replay_token(op: &mut Operation, no_replay_token: NoReplayToken) {
    match op {
        Operation::UpdateMemo(um) => um.body.no_replay_token = no_replay_token,
        Operation::AIRAssign(aa) => aa.body.no_replay_token = no_replay_token,
        _ => (),
    }
}

#[derive(Clone, Debug)]
pub struct TimeBounds {
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
}

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
    fn from_token(no_replay_token: NoReplayToken) -> Self {
        let mut result = TransactionBody::default();
        result.no_replay_token = no_replay_token;
        result
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct Transaction {
    pub body: TransactionBody,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub signatures: Vec<SignatureOf<TransactionBody>>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FinalizedTransaction {
    pub txn: Transaction,
    pub tx_id: TxnSID,

    pub merkle_id: u64,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AuthenticatedAIRResult {
    pub state_commitment_data: Option<StateCommitmentData>,
    pub state_commitment: HashOf<Option<StateCommitmentData>>,
    pub air_result: AIRResult,
}

impl AuthenticatedAIRResult {
    // An authenticated air result is valid if
    // 1) State commitment data hashes to the provided state commitment
    // 2) The air root matches the root in state commitment data.
    // 3) The air proof is valid.
    pub fn is_valid(
        &self,
        state_commitment: HashOf<Option<StateCommitmentData>>,
    ) -> bool {
        let root = self.air_result.merkle_root;
        match &self.state_commitment_data {
            None => {
                if self.air_result.value.is_some() {
                    return false;
                }
                if state_commitment != HashOf::new(&None) {
                    return false;
                }
                if root != ZERO_DIGEST {
                    return false;
                }
            }
            Some(comm_data) => {
                if self.state_commitment != comm_data.compute_commitment() {
                    return false;
                }

                if comm_data.air_commitment != root {
                    return false;
                }
            }
        }
        if state_commitment != self.state_commitment {
            return false;
        }
        let key = &self.air_result.key;
        let value = self.air_result.value.as_ref();
        let proof = &self.air_result.merkle_proof;

        air_check_merkle_proof(&root, key, value, proof)
    }

    // Extract the credential commitment stored in this AIRResult
    pub fn get_credential_commitment(&self) -> Option<CredCommitment> {
        // This unwrap is safe because by design, AIR values can only be credential commitments
        self.air_result
            .value
            .as_ref()
            .map(|cred_str| serde_json::from_str(&cred_str).unwrap())
    }
}

#[derive(Clone, Serialize, Deserialize)]
// Note: if the utxo field of this struct is changed, update the comment for ClientAssetRecord::from_json in wasm_data_model.rs as well.
pub struct AuthenticatedUtxo {
    pub utxo: Utxo,                                  // Utxo to authenticate
    pub authenticated_txn: AuthenticatedTransaction, // Merkle proof that transaction containing the utxo exists on the ledger
    pub authenticated_spent_status: AuthenticatedUtxoStatus, // Bitmap proof that the utxo is unspent
    pub utxo_location: OutputPosition,
    pub state_commitment_data: StateCommitmentData,
}

impl AuthenticatedUtxo {
    // An authenticated utxo result is valid iff
    // 1) The state commitment data used during verification hashes to the provided state commitment
    // 2) The authenticated transaction proof is valid
    // 3) The spent status proof is valid and denotes the utxo as unspent
    // 4) The utxo appears in one of the outputs of the transaction (i.e. the output at
    //    OutputPosition)
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

#[derive(Serialize, Deserialize, Clone)]
pub struct AuthenticatedTransaction {
    pub finalized_txn: FinalizedTransaction,
    pub txn_inclusion_proof: ProofOf<(TxnSID, Transaction)>,
    pub state_commitment_data: StateCommitmentData,
    pub state_commitment: HashOf<Option<StateCommitmentData>>,
}

impl AuthenticatedTransaction {
    // An authenticated txn result is valid if
    // 1) The state commitment used in the proof matches what we pass in and the state commitment
    //    data hashes to the state commitment
    // 2) The transaction merkle proof is valid
    // 3) The transaction merkle root matches the value in root_hash_data
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

pub struct AuthenticatedBlock {
    pub block: FinalizedBlock,
    pub block_inclusion_proof: ProofOf<Vec<Transaction>>,
    pub state_commitment_data: StateCommitmentData,
    pub state_commitment: HashOf<Option<StateCommitmentData>>,
}

impl AuthenticatedBlock {
    // An authenticated block result is valid if
    // 1) The block merkle proof is valid
    // 2) The block merkle root matches the value in root_hash_data
    // 3) root_hash_data hashes to root_hash
    // 4) The state commitment of the proof matches the state commitment passed in
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

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct AuthenticatedKVLookup {
    pub key: Key,
    pub result: Option<Serialized<(u64, Option<KVEntry>)>>,
    pub state_commitment_data: Option<StateCommitmentData>,
    pub merkle_root: BitDigest,
    pub merkle_proof: MerkleProof,
    pub state_commitment: HashOf<Option<StateCommitmentData>>,
}

impl AuthenticatedKVLookup {
    pub fn is_valid(
        &self,
        state_commitment: HashOf<Option<StateCommitmentData>>,
    ) -> bool {
        match &self.state_commitment_data {
            None => {
                if self.result.is_some() {
                    return false;
                }
                if state_commitment != HashOf::new(&None) {
                    return false;
                }
                if self.merkle_root
                    != (BitDigest {
                        0: [0_u8; DIGESTBYTES],
                    })
                {
                    return false;
                }
            }
            Some(comm_data) => {
                if self.state_commitment != comm_data.compute_commitment() {
                    return false;
                }

                if comm_data.kv_store != self.merkle_root {
                    return false;
                }
            }
        }
        if state_commitment != self.state_commitment {
            return false;
        }
        check_merkle_proof(
            &self.merkle_root,
            &self.key,
            self.result.as_ref(),
            &self.merkle_proof,
        )
    }
}

pub type SparseMapBytes = Vec<u8>;

#[derive(Serialize, Clone, Deserialize)]
pub struct AuthenticatedUtxoStatus {
    pub status: UtxoStatus,
    pub utxo_sid: TxoSID,
    pub state_commitment_data: StateCommitmentData,
    pub utxo_map_bytes: Option<SparseMapBytes>, // BitMap only needed for proof if the txo_sid exists
    pub state_commitment: HashOf<Option<StateCommitmentData>>,
}

impl AuthenticatedUtxoStatus {
    // An authenticated utxo status is valid (for txos that exist) if
    // 1) The state commitment of the proof matches the state commitment passed in
    // 2) The state commitment data hashes to the state commitment
    // 3) For txos that don't exist, simply show that the utxo_sid greater than max_sid
    // 4) The status matches the bit stored in the bitmap
    // 5) The bitmap checksum matches digest in state commitment data
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

        if self.status == UtxoStatus::Nonexistent {
            // 3)
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

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FinalizedBlock {
    pub txns: Vec<FinalizedTransaction>,
    pub merkle_id: u64,
}

impl FinalizedTransaction {
    pub fn hash(&self) -> HashOf<(TxnSID, Transaction)> {
        self.txn.hash(self.tx_id)
    }
}

/// Will be used by `cli2`.
pub const ASSET_TYPE_FRA_BYTES: [u8; ASSET_TYPE_LENGTH] = [0; ASSET_TYPE_LENGTH];
/// Use pure zero bytes(aka [0, 0, ... , 0]) to express FRA.
pub const ASSET_TYPE_FRA: ZeiAssetType = ZeiAssetType(ASSET_TYPE_FRA_BYTES);

lazy_static! {
    /// The destination of Fee is an black hole,
    /// all token transfered to it will be burned.
    pub static ref BLACK_HOLE_PUBKEY: XfrPublicKey =
        XfrPublicKey::zei_from_bytes(&[0; ed25519_dalek::PUBLIC_KEY_LENGTH][..])
            .unwrap();
}

/// TODO: a better value ?
pub const TX_FEE_MIN: u64 = 1;

impl Transaction {
    /// A simple fee checker for mainnet v1.0.
    ///
    /// The check logic is as follows:
    /// - Only `NonConfidential Operation` can be used as fee
    /// - FRA code == [0; ASSET_TYPE_LENGTH]
    /// - Fee destination == [0; ed25519_dalek::PUBLIC_KEY_LENGTH]
    /// - A transaction with an `Operation` of defining FRA need NOT any fee
    ///
    /// > Is this function compatible with the process of
    /// > defining and issuing FRA in the genesis block ?
    /// >
    /// > Yes, I think so. But please note:
    /// >
    /// > - Your should put all Operations related to
    /// > the defination and issuing of FRA into a same transaction.
    /// > Because the basic unit of `check_fee` is a whole transaction, and
    /// > there can be many Operations inside this transaction, such as:
    /// > defining assets, issuing assets, etc.
    /// > as long as the order of these operations is correct.
    /// > - `TransferAsset` operations of FRA can NOT be placed
    /// > in the same transaction with its defination and issuing,
    /// > or the transaction can NOT pass the check of `apply_transaction(...)`
    pub fn check_fee(&self) -> bool {
        self.body.operations.iter().any(|o| {
            if let Operation::TransferAsset(ref x) = o {
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
            } else if let Operation::DefineAsset(ref x) = o {
                if x.body.asset.code.val == ASSET_TYPE_FRA {
                    return true;
                }
            }

            false
        })
    }

    /// Issuing FRA is denied except in the genesis block.
    pub fn check_fra_no_illegal_issuance(&self, tendermint_block_height: i64) -> bool {
        // **mainnet v1.0**
        //
        // FRA is defined and issued in genesis block.
        if 2 > tendermint_block_height {
            return true;
        }

        !self.body.operations.iter().any(|o| {
            if let Operation::IssueAsset(ref x) = o {
                if ASSET_TYPE_FRA == x.body.code.val {
                    return true;
                }
            }
            false
        })
    }

    pub fn hash(&self, id: TxnSID) -> HashOf<(TxnSID, Transaction)> {
        HashOf::new(&(id, self.clone()))
    }

    pub fn from_seq_id(seq_id: u64) -> Self {
        let mut prng = ChaChaRng::from_entropy();
        let no_replay_token = NoReplayToken::new(&mut prng, seq_id);
        Transaction {
            body: TransactionBody::from_token(no_replay_token),
            signatures: Vec::new(),
        }
    }

    pub fn from_operation(op: Operation, seq_id: u64) -> Self {
        let mut tx = Transaction::from_seq_id(seq_id);
        tx.add_operation(op);
        tx
    }

    pub fn add_operation(&mut self, op: Operation) {
        let mut mutable_op = op;
        set_no_replay_token(&mut mutable_op, self.body.no_replay_token);
        self.body.operations.push(mutable_op);
    }

    pub fn testonly_add_operation(&mut self, op: Operation) {
        self.body.operations.push(op);
    }

    pub fn sign(&mut self, keypair: &XfrKeyPair) {
        self.signatures.push(SignatureOf::new(keypair, &self.body));
    }

    pub fn check_signature(
        &self,
        public_key: &XfrPublicKey,
        sig: &SignatureOf<TransactionBody>,
    ) -> Result<(), PlatformError> {
        sig.verify(public_key, &self.body)
            .map_err(|e| PlatformError::ZeiError(error_location!(), e))?;
        Ok(())
    }

    pub fn get_owner_memos_ref(&self) -> Vec<Option<&OwnerMemo>> {
        let mut memos = vec![];
        for op in self.body.operations.iter() {
            match op {
                Operation::TransferAsset(xfr_asset) => {
                    memos.append(&mut xfr_asset.get_owner_memos_ref());
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
    pub fn get_outputs_ref(&self, include_spent: bool) -> Vec<TxOutput> {
        let eff = TxnEffect::compute_effect(self.clone()).unwrap();
        if !include_spent {
            eff.txos.into_iter().filter_map(|x| x).collect()
        } else {
            let mut spent = eff.internally_spent_txos.into_iter();
            let mut ret = vec![];
            for txo in eff.txos.into_iter() {
                if let Some(txo) = txo {
                    ret.push(txo);
                } else {
                    ret.push(spent.next().unwrap());
                }
            }
            ret
        }

        // let mut outputs = vec![];
        // let mut spent_indices = vec![];
        // for op in self.body.operations.iter() {
        //   match op {
        //     Operation::TransferAsset(xfr_asset) => {
        //       for txo_ref in &xfr_asset.body.inputs {
        //         match txo_ref {
        //           TxoRef::Relative(offset) => {
        //             let idx = (outputs.len() as u64) - *offset - 1;
        //             spent_indices.push(idx);
        //           }
        //           TxoRef::Absolute(_) => {}
        //         };
        //       }
        //       outputs.append(&mut xfr_asset.get_outputs_ref());
        //     }
        //     Operation::IssueAsset(issue_asset) => {
        //       outputs.append(&mut issue_asset.get_outputs_ref());
        //     }
        //     _ => {}
        //   }
        // }
        // if !include_spent {
        //   for idx in spent_indices {
        //     outputs.remove(idx.try_into().unwrap());
        //   }
        // }
        // outputs
    }

    /// NOTE: this does *not* guarantee that a private key affiliated with
    /// `public_key` has signed this transaction! If `public_key` is derived
    /// from `self` somehow, then it is infeasible for someone to forge a
    /// passing signature, but it is plausible for someone to generate an
    /// unrelated `public_key` which can pass this signature check!
    pub fn check_has_signature(
        &self,
        public_key: &XfrPublicKey,
    ) -> Result<(), PlatformError> {
        let serialized = Serialized::new(&self.body);
        for sig in self.signatures.iter() {
            match sig.0.verify(public_key, &serialized) {
                Err(_) => {}
                Ok(_) => {
                    return Ok(());
                }
            }
        }
        Err(PlatformError::InputsError(error_location!()))
    }
}

#[repr(C)]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct StateCommitmentData {
    pub bitmap: BitDigest,       // The checksum of the utxo_map
    pub block_merkle: HashValue, // The root hash of the block Merkle tree
    pub txns_in_block_hash: HashOf<Vec<Transaction>>, // The hash of the transactions in the block
    pub previous_state_commitment: HashOf<Option<StateCommitmentData>>, // The prior global block hash
    pub transaction_merkle_commitment: HashValue, // The root hash of the transaction Merkle tree
    pub air_commitment: BitDigest, // The root hash of the AIR sparse Merkle tree
    pub txo_count: u64, // Number of transaction outputs. Used to provide proof that a utxo does not exist
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub pulse_count: u64, // a consensus-specific counter; should be 0 unless consensus needs it.
    #[serde(default = "default_digest")]
    #[serde(skip_serializing_if = "is_default_digest")]
    pub kv_store: BitDigest, // The root hash of the KV Store SMT
}

impl StateCommitmentData {
    pub fn compute_commitment(&self) -> HashOf<Option<Self>> {
        HashOf::new(&Some(self).cloned())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct AccountID {
    pub val: String,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct Account {
    pub id: AccountID,
    pub access_control_list: Vec<AccountAddress>,
    pub key_value: HashMap<String, String>, //key value storage...
}

#[cfg(test)]
mod tests {
    use super::*;
    use curve25519_dalek::ristretto::CompressedRistretto;
    use rand_core::SeedableRng;
    use std::cmp::min;
    use zei::xfr::structs::{AssetTypeAndAmountProof, XfrBody, XfrProofs};

    // This test may fail as it is a statistical test that sometimes fails (but very rarely)
    // It uses the central limit theorem, but essentially testing the rand crate
    #[test]
    fn test_gen_random_with_rng() {
        let mut sum: u64 = 0;
        let mut sample_size = 0;

        let mut rng = rand::thread_rng();
        for _ in 0..1000 {
            let code = AssetTypeCode::gen_random_with_rng(&mut rng);
            let mut failed = true;

            for byte in code.val.0.iter() {
                if *byte != 0 {
                    failed = false;
                }

                sum += *byte as u64;
                sample_size += 1;
            }

            assert!(!failed);
        }

        // Use the central limit theorem. The standard deviation of the
        // sample mean should be normal(127.5, uniform variance). Work
        // from the standard deviation of uniform(0, 1), sqrt(1/12). The
        // expected average (mu) is 127.5 if the random number generator
        // is unbiased.
        let uniform_stddev = 1.0 / (12.0f64).sqrt();
        let average = sum as f64 / sample_size as f64;
        let stddev = (uniform_stddev * 255.0) / (sample_size as f64).sqrt();
        println!("Average {}, stddev {}", average, stddev);
        assert!(average > 127.5 - 5.0 * stddev);
        assert!(average < 127.5 + 5.0 * stddev);
    }

    #[test]
    // Test that an error is returned if the asset code is greater than 32 byts and a safe conversion is chosen
    fn test_base64_from_to_utf8_safe() {
        if UTF8_ASSET_TYPES_WORK {
            let code = "My 资产 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$";
            let result = AssetTypeCode::new_from_utf8_safe(code);
            match result {
                Err(PlatformError::InputsError(_)) => {}
                _ => panic!("InputsError expected."),
            }
        }
    }

    #[test]
    // Test that a customized asset code can be converted to and from base 64 correctly
    fn test_base64_from_to_utf8_truncate() {
        if UTF8_ASSET_TYPES_WORK {
            let customized_code = "❤️💰 My 资产 $";
            let code = AssetTypeCode::new_from_utf8_truncate(customized_code);
            let utf8 = AssetTypeCode::to_utf8(&code).unwrap();
            assert_eq!(utf8, customized_code);
        }
    }

    #[test]
    // Test that a customized asset code is truncated correctly if the lenght is greater than 32
    fn test_utf8_truncate() {
        if UTF8_ASSET_TYPES_WORK {
            let customized_code_short = "My 资产 $";
            let customized_code_32_bytes = "My 资产 $$$$$$$$$$$$$$$$$$$$$$";
            let customized_code_to_truncate =
                "My 资产 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$";

            let code_short =
                AssetTypeCode::new_from_utf8_truncate(customized_code_short);
            let code_32_bytes =
                AssetTypeCode::new_from_utf8_truncate(customized_code_32_bytes);
            let code_to_truncate =
                AssetTypeCode::new_from_utf8_truncate(customized_code_to_truncate);
            assert_ne!(code_short, code_32_bytes);
            assert_eq!(code_32_bytes, code_to_truncate);

            let utf8 = AssetTypeCode::to_utf8(&code_32_bytes).unwrap();
            assert_eq!(utf8, customized_code_32_bytes);
        }
    }

    #[test]
    fn test_new_from_str() {
        let value = "1";
        let mut input = "".to_string();

        for i in 0..64 {
            let code = AssetTypeCode::new_from_str(&input);
            let mut checked = 0;

            for j in 0..min(i, code.val.0.len()) {
                assert!(code.val.0[j] == value.as_bytes()[0]);
                checked += 1;
            }

            for j in i..code.val.0.len() {
                assert!(code.val.0[j] == 0);
                checked += 1;
            }

            assert!(checked == code.val.0.len());
            input += value;
        }
    }

    #[quickcheck]
    #[ignore]
    #[test]
    fn test_to_from_base64(bytes: Vec<u8>) {
        let code = AssetTypeCode::new_from_vec(bytes);
        assert_eq!(Ok(code), AssetTypeCode::new_from_base64(&code.to_base64()));
    }

    #[quickcheck]
    #[ignore]
    #[test]
    fn test_to_from_utf8(bytes: Vec<u8>) {
        if UTF8_ASSET_TYPES_WORK {
            let code = AssetTypeCode::new_from_vec(bytes);
            println!("{:?}", code.to_utf8());
            assert_eq!(
                Ok(code),
                AssetTypeCode::new_from_utf8_safe(&code.to_utf8().unwrap())
            );
        }
    }

    #[test]
    fn test_new_from_base64() {
        let base64 = "ZGVmZ2hpamtsbW5vcHFycw==";
        let result = Code::new_from_base64(base64);

        assert_eq!(
            result.ok(),
            Some(Code {
                val: [
                    100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
                    113, 114, 115
                ]
            })
        );
    }

    #[test]
    fn test_code_to_base64() {
        let code = Code {
            val: [
                100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
                114, 115,
            ],
        };
        assert_eq!(code.to_base64(), "ZGVmZ2hpamtsbW5vcHFycw==");
    }

    // Test Transaction::add_operation
    // Below are not directly tested but called:
    //   TransferAssetBody::new
    //   IssueAssetBody::new
    //   DefineAssetBody::new
    //   TransferAsset::new
    //   IssueAsset::new
    //   DefineAsset::new
    fn gen_sample_tx() -> Transaction {
        // Create values to be used to instantiate operations. Just make up a seq_id, since
        // it will never be sent to a real ledger
        let mut transaction: Transaction = Transaction::from_seq_id(666);

        let mut prng = rand_chacha::ChaChaRng::from_entropy();

        let keypair = XfrKeyPair::generate(&mut prng);

        let xfr_note = XfrBody {
            inputs: Vec::new(),
            outputs: Vec::new(),
            proofs: XfrProofs {
                asset_type_and_amount_proof: AssetTypeAndAmountProof::NoProof,
                asset_tracking_proof: Default::default(),
            },
            asset_tracing_memos: vec![],
            owners_memos: vec![],
        };

        let no_policies = AssetTracingPolicies::new();

        let policies = XfrNotePolicies::new(
            vec![no_policies.clone()],
            vec![None],
            vec![no_policies],
            vec![None],
        );

        let asset_transfer_body = TransferAssetBody {
            inputs: Vec::new(),
            outputs: Vec::new(),
            policies,
            transfer: Box::new(xfr_note),
            lien_assignments: vec![],
            transfer_type: TransferType::Standard,
        };

        let asset_transfer = {
            let mut ret = TransferAsset::new(asset_transfer_body).unwrap();
            ret.sign(&keypair);
            ret
        };

        let transfer_operation = Operation::TransferAsset(asset_transfer.clone());

        // Instantiate an IssueAsset operation
        let asset_issuance_body = IssueAssetBody {
            code: AssetTypeCode::gen_random(),
            seq_num: 0,
            num_outputs: 0,
            records: Vec::new(),
        };

        let asset_issuance =
            IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &keypair })
                .unwrap();

        let issuance_operation = Operation::IssueAsset(asset_issuance.clone());

        // Instantiate an DefineAsset operation
        let mut asset = Box::new(Asset::default());
        asset.code = AssetTypeCode::gen_random();

        let asset_creation = DefineAsset::new(
            DefineAssetBody { asset },
            &IssuerKeyPair { keypair: &keypair },
        )
        .unwrap();

        let creation_operation = Operation::DefineAsset(asset_creation.clone());

        // Add operations to the transaction
        transaction.add_operation(transfer_operation);
        transaction.add_operation(issuance_operation);
        transaction.add_operation(creation_operation);

        // Verify operatoins
        assert_eq!(transaction.body.operations.len(), 3);

        assert_eq!(
            transaction.body.operations.get(0),
            Some(&Operation::TransferAsset(asset_transfer))
        );
        assert_eq!(
            transaction.body.operations.get(1),
            Some(&Operation::IssueAsset(asset_issuance))
        );
        assert_eq!(
            transaction.body.operations.get(2),
            Some(&Operation::DefineAsset(asset_creation))
        );

        transaction
    }

    #[test]
    fn test_add_operation() {
        gen_sample_tx();
    }

    fn gen_fee_operation(
        amount: Option<u64>,
        asset_type: Option<ZeiAssetType>,
        dest_pubkey: XfrPublicKey,
    ) -> Operation {
        Operation::TransferAsset(TransferAsset {
            body: TransferAssetBody {
                inputs: Vec::new(),
                policies: XfrNotePolicies::default(),
                outputs: vec![TxOutput {
                    record: BlindAssetRecord {
                        amount: amount
                            .map(|am| XfrAmount::NonConfidential(am))
                            .unwrap_or(XfrAmount::Confidential((
                                CompressedRistretto([0; 32]),
                                CompressedRistretto([0; 32]),
                            ))),
                        asset_type: asset_type
                            .map(|at| XfrAssetType::NonConfidential(at))
                            .unwrap_or(XfrAssetType::Confidential(CompressedRistretto(
                                [0; 32],
                            ))),
                        public_key: dest_pubkey,
                    },
                    lien: None,
                }],
                lien_assignments: Vec::new(),
                transfer: Box::new(XfrBody {
                    inputs: Vec::new(),
                    outputs: Vec::new(),
                    proofs: XfrProofs {
                        asset_type_and_amount_proof: AssetTypeAndAmountProof::NoProof,
                        asset_tracking_proof: Default::default(),
                    },
                    asset_tracing_memos: Vec::new(),
                    owners_memos: Vec::new(),
                }),
                transfer_type: TransferType::Standard,
            },
            body_signatures: Vec::new(),
        })
    }

    #[test]
    fn test_check_fee() {
        let mut tx = gen_sample_tx();
        assert!(!tx.check_fee());

        let invalid_confidential_type =
            gen_fee_operation(Some(TX_FEE_MIN), None, *BLACK_HOLE_PUBKEY);
        let invalid_confidential_amount = gen_fee_operation(
            None,
            Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
            *BLACK_HOLE_PUBKEY,
        );
        let invalid_nonconfidential_not_fra_code = gen_fee_operation(
            Some(TX_FEE_MIN),
            Some(ZeiAssetType([9; ASSET_TYPE_LENGTH])),
            *BLACK_HOLE_PUBKEY,
        );
        let invalid_nonconfidential_fee_too_little = gen_fee_operation(
            Some(TX_FEE_MIN - 1),
            Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
            *BLACK_HOLE_PUBKEY,
        );
        let invalid_destination_not_black_hole = gen_fee_operation(
            Some(TX_FEE_MIN),
            Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
            XfrPublicKey::zei_from_bytes(&[9; ed25519_dalek::PUBLIC_KEY_LENGTH][..])
                .unwrap(),
        );
        let valid = gen_fee_operation(
            Some(TX_FEE_MIN),
            Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
            *BLACK_HOLE_PUBKEY,
        );
        let valid2 = gen_fee_operation(
            Some(TX_FEE_MIN + 999),
            Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
            *BLACK_HOLE_PUBKEY,
        );

        tx.add_operation(invalid_confidential_type.clone());
        assert!(!tx.check_fee());

        tx.add_operation(invalid_confidential_amount.clone());
        assert!(!tx.check_fee());

        tx.add_operation(invalid_nonconfidential_not_fra_code.clone());
        assert!(!tx.check_fee());

        tx.add_operation(invalid_nonconfidential_fee_too_little.clone());
        assert!(!tx.check_fee());

        tx.add_operation(invalid_destination_not_black_hole.clone());
        assert!(!tx.check_fee());

        tx.add_operation(valid);
        assert!(tx.check_fee());

        tx.add_operation(invalid_confidential_type);
        assert!(tx.check_fee());

        tx.add_operation(invalid_confidential_amount);
        assert!(tx.check_fee());

        tx.add_operation(valid2);
        assert!(tx.check_fee());

        tx.add_operation(invalid_nonconfidential_not_fra_code);
        assert!(tx.check_fee());

        tx.add_operation(invalid_nonconfidential_fee_too_little);
        assert!(tx.check_fee());

        tx.add_operation(invalid_destination_not_black_hole);
        assert!(tx.check_fee());
    }

    // Verify that the hash values of two transactions:
    //   are the same if the transactions differ only in merkle_id
    //   are different if the transactions differ in other fields
    // TODO(joe): determine a good test to replace this
    // #[test]
    // fn test_compute_merkle_hash() {
    //   let transaction_default: Transaction = Default::default();

    //   let transaction_different_merkle_id =
    //     Transaction { operations: Vec::new(),
    //                   credentials: Vec::new(),
    //                   memos: Vec::new() };

    //   let transaction_other_differences = Transaction { operations: Vec::new(),
    //                                                     credentials: Vec::new(),
    //                                                     memos: Vec::new(),
    //                                                     };

    //   let hash_value_default = transaction_default.compute_merkle_hash();
    //   let hash_value_different_merkle_id = transaction_different_merkle_id.compute_merkle_hash();
    //   let hash_value_other_differences = transaction_other_differences.compute_merkle_hash();

    //   assert_eq!(hash_value_different_merkle_id, hash_value_default);
    //   assert_ne!(hash_value_other_differences, hash_value_default);
    // }
}
