#![deny(warnings)]
use crate::util::error_to_jsvalue;
use credentials::{
  CredCommitment, CredIssuerPublicKey, CredIssuerSecretKey, CredPoK, CredRevealSig, CredSignature,
  CredUserPublicKey, CredUserSecretKey, Credential as PlatformCredential,
};
use cryptohash::sha256::{Digest, DIGESTBYTES};
use ledger::data_model::{
  AssetRules as PlatformAssetRules, AssetType as PlatformAssetType,
  AuthenticatedAIRResult as PlatformAuthenticatedAIRResult, AuthenticatedUtxo,
  KVBlind as PlatformKVBlind, KVHash as PlatformKVHash, SignatureRules as PlatformSignatureRules,
  TxOutput, TxoRef as PlatformTxoRef, TxoSID,
};
use rand_chacha::ChaChaRng;
use rand_core::{RngCore, SeedableRng};
use serde::{Deserialize, Serialize};
use utils::HashOf;
use wasm_bindgen::prelude::*;
use zei::setup::PublicParams as ZeiPublicParams;
use zei::xfr::asset_tracer::gen_asset_tracer_keypair;
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::{
  AssetTracerDecKeys, AssetTracerEncKeys, AssetTracerKeyPair as ZeiAssetTracerKeyPair,
  AssetTracingPolicies, AssetTracingPolicy, BlindAssetRecord, IdentityRevealPolicy,
  OwnerMemo as ZeiOwnerMemo,
};

#[wasm_bindgen]
/// Public parameters necessary for generating asset records. Generating this is expensive and
/// should be done as infrequently as possible.
/// @see {@link TransactionBuilder#add_basic_issue_asset}
pub struct PublicParams {
  pub(crate) params: ZeiPublicParams,
}

#[allow(clippy::new_without_default)]
#[wasm_bindgen]
impl PublicParams {
  /// Generates a new set of parameters.
  pub fn new() -> PublicParams {
    PublicParams { params: ZeiPublicParams::new() }
  }
}

impl PublicParams {
  pub fn get_ref(&self) -> &ZeiPublicParams {
    &self.params
  }
}

#[wasm_bindgen]
/// Indicates whether the TXO ref is an absolute or relative value.
pub struct TxoRef {
  pub(crate) txo_ref: PlatformTxoRef,
}

#[wasm_bindgen]
impl TxoRef {
  /// Creates a relative txo reference as a JSON string. Relative txo references are offset
  /// backwards from the operation they appear in -- 0 is the most recent, (n-1) is the first output
  /// of the transaction.
  ///
  /// Use relative txo indexing when referring to outputs of intermediate operations (e.g. a
  /// transaction containing both an issuance and a transfer).
  ///
  /// # Arguments
  /// @param {BigInt} idx -  Relative TXO (transaction output) SID.
  pub fn relative(idx: u64) -> Self {
    TxoRef { txo_ref: PlatformTxoRef::Relative(idx) }
  }

  /// Creates an absolute transaction reference as a JSON string.
  ///
  /// Use absolute txo indexing when referring to an output that has been assigned a utxo index (i.e.
  /// when the utxo has been committed to the ledger in an earlier transaction).
  ///
  /// # Arguments
  /// @param {BigInt} idx -  Txo (transaction output) SID.
  pub fn absolute(idx: u64) -> Self {
    TxoRef { txo_ref: PlatformTxoRef::Absolute(TxoSID(idx)) }
  }
}

impl TxoRef {
  pub fn get_txo(&self) -> &PlatformTxoRef {
    &self.txo_ref
  }
}

/// Object representing an authenticable asset record. Clients can validate authentication proofs
/// against a ledger state commitment.
#[wasm_bindgen]
pub struct AuthenticatedAssetRecord {
  pub(crate) authenticated_record: AuthenticatedUtxo,
}

impl AuthenticatedAssetRecord {
  pub fn get_auth_record_ref(&self) -> &AuthenticatedUtxo {
    &self.authenticated_record
  }
}

#[wasm_bindgen]
impl AuthenticatedAssetRecord {
  /// Given a serialized state commitment, returns true if the
  /// authenticated UTXO proofs validate correctly and false otherwise. If the proofs validate, the
  /// asset record contained in this structure exists on the ledger and is unspent.
  /// @param {string} state_commitment - String representing the state commitment.
  /// @see {@link Network#getStateCommitment|Network.getStateCommitment} for instructions on fetching a ledger state commitment.
  /// @throws Will throw an error if the state commitment fails to deserialize.
  pub fn is_valid(&self, state_commitment: String) -> Result<bool, JsValue> {
    let state_commitment = serde_json::from_str::<HashOf<_>>(&state_commitment).map_err(|_e| {
                             JsValue::from_str("Could not deserialize state commitment")
                           })?;
    Ok(self.authenticated_record.is_valid(state_commitment))
  }

  /// Builds an AuthenticatedAssetRecord from a JSON-encoded asset record returned from the ledger
  /// server.
  /// @param {JsValue} val - JSON-encoded asset record fetched from ledger server.
  /// @see {@link Network#getUtxo|Network.getUtxo} for information about how to
  /// fetch an asset record from the ledger server.
  pub fn from_json_record(record: &JsValue) -> Result<AuthenticatedAssetRecord, JsValue> {
    Ok(AuthenticatedAssetRecord { authenticated_record: record.into_serde()
                                                              .map_err(error_to_jsvalue)? })
  }
}

#[wasm_bindgen]
/// This object represents an asset record owned by a ledger key pair.
/// @see {@link open_client_asset_record} for information about how to decrypt an encrypted asset
/// record.
pub struct ClientAssetRecord {
  pub(crate) txo: TxOutput,
}

impl ClientAssetRecord {
  pub fn get_bar_ref(&self) -> &BlindAssetRecord {
    &self.txo.0
  }
}

#[wasm_bindgen]
impl ClientAssetRecord {
  /// Builds a client record from a JSON-encodedJavaScript value.
  /// @param {JsValue} val - JSON-encoded authenticated asset record fetched from ledger server.
  /// @see {@link Network#getUtxo|Network.getUtxo} for information about how to
  /// fetch an asset record from the ledger server.
  pub fn from_json(val: &JsValue) -> Self {
    ClientAssetRecord { txo: val.into_serde().unwrap() }
  }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// Key pair used by asset tracers to decrypt asset amounts, types, and identity
/// commitments associated with traceable asset transfers.
/// @see {@link TracingPolicy} for information about tracing policies.
/// @see {@link AssetRules#add_tracing_policy} for information about how to add a tracing policy to
/// an asset definition.
pub struct AssetTracerKeyPair {
  pub(crate) keypair: ZeiAssetTracerKeyPair,
}

#[wasm_bindgen]
impl AssetTracerKeyPair {
  /// Creates a new tracer key pair.
  pub fn new() -> Self {
    let mut small_rng = ChaChaRng::from_entropy();
    AssetTracerKeyPair { keypair: gen_asset_tracer_keypair(&mut small_rng) }
  }
}
impl Default for AssetTracerKeyPair {
  fn default() -> Self {
    Self::new()
  }
}

impl AssetTracerKeyPair {
  pub fn get_enc_key(&self) -> &AssetTracerEncKeys {
    &self.keypair.enc_key
  }

  pub fn get_dec_key(&self) -> &AssetTracerDecKeys {
    &self.keypair.dec_key
  }

  pub fn get_keys(&self) -> &ZeiAssetTracerKeyPair {
    &self.keypair
  }
}

#[wasm_bindgen]
#[derive(Deserialize)]
/// Asset owner memo. Contains information needed to decrypt an asset record.
/// @see {@link ClientAssetRecord} for more details about asset records.
pub struct OwnerMemo {
  pub(crate) memo: ZeiOwnerMemo,
}

#[wasm_bindgen]
impl OwnerMemo {
  /// Builds an owner memo from a JSON-serialized JavaScript value.
  /// @param {JsValue} val - JSON owner memo fetched from query server with the `get_owner_memo/{sid}` route,
  /// where `sid` can be fetched from the query server with the `get_owned_utxos/{address}` route.
  /// * E.g.: `{"blind_share":[91,251,44,28,7,221,67,155,175,213,25,183,70,90,119,232,212,238,226,142,159,200,54,19,60,115,38,221,248,202,74,248],
  /// "lock":{"ciphertext":[119,54,117,136,125,133,112,193],"encoded_rand":"8KDql2JphPB5WLd7-aYE1bxTQAcweFSmrqymLvPDntM="}}`.
  pub fn from_json(val: &JsValue) -> Self {
    let zei_owner_memo: ZeiOwnerMemo = val.into_serde().unwrap();
    OwnerMemo { memo: ZeiOwnerMemo { blind_share: zei_owner_memo.blind_share,
                                     lock: zei_owner_memo.lock } }
  }
}

impl OwnerMemo {
  pub fn get_memo_ref(&self) -> &ZeiOwnerMemo {
    &self.memo
  }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct AttributeDefinition {
  pub name: String,
  pub size: usize,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct AttributeAssignment {
  pub name: String,
  pub val: String,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// Key pair of a credential user.
pub struct CredentialUserKeyPair {
  pub(crate) pk: CredUserPublicKey,
  pub(crate) sk: CredUserSecretKey,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// Key pair of a credential issuer.
pub struct CredentialIssuerKeyPair {
  pub(crate) pk: CredIssuerPublicKey,
  pub(crate) sk: CredIssuerSecretKey,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// Signature of a credential record.
pub struct CredentialSignature {
  pub(crate) sig: CredSignature,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// Reveal signature of a credential record.
pub struct CredentialRevealSig {
  pub(crate) sig: CredRevealSig,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// Commitment to a credential record and proof that the commitment is a valid re-randomization of a
/// commitment signed by a certain credential issuer.
pub struct CredentialCommitmentAndPoK {
  pub(crate) commitment: CredentialCommitment,
  pub(crate) pok: CredentialPoK,
}

#[wasm_bindgen]
impl CredentialCommitmentAndPoK {
  /// Returns the underlying credential commitment.
  /// @see {@link wasm_credential_verify_commitment} for information about how to verify a
  /// credential commitment.
  pub fn get_commitment(&self) -> CredentialCommitment {
    self.commitment.clone()
  }
  /// Returns the underlying proof of knowledge that the credential is a valid re-randomization.
  /// @see {@link wasm_credential_verify_commitment} for information about how to verify a
  /// credential commitment.
  pub fn get_pok(&self) -> CredentialPoK {
    self.pok.clone()
  }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
/// Commitment to a credential record.
/// @see {@link wasm_credential_verify_commitment} for information about how to verify a
/// credential commitment.
pub struct CredentialCommitment {
  pub(crate) commitment: CredCommitment,
}

impl CredentialCommitment {
  pub fn get_ref(&self) -> &CredCommitment {
    &self.commitment
  }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
/// Proof that a credential is a valid re-randomization of a credential signed by a certain asset
/// issuer.
/// @see {@link wasm_credential_verify_commitment} for information about how to verify a
/// credential commitment.
pub struct CredentialPoK {
  pub(crate) pok: CredPoK,
}

impl CredentialPoK {
  pub fn get_ref(&self) -> &CredPoK {
    &self.pok
  }
}

#[wasm_bindgen]
/// Authenticated address identity registry value. Contains a proof that the AIR result is stored
/// on the ledger.
pub struct AuthenticatedAIRResult {
  pub(crate) result: PlatformAuthenticatedAIRResult,
}

impl AuthenticatedAIRResult {
  pub fn get_ref(&self) -> &PlatformAuthenticatedAIRResult {
    &self.result
  }
}

#[wasm_bindgen]
/// Object representing an asset definition. Used to fetch tracing policies and any other
/// information that may be required to construct a valid transfer or issuance.
pub struct AssetType {
  pub(crate) asset_type: PlatformAssetType,
}

#[wasm_bindgen]
impl AssetType {
  /// Builds an asset type from a JSON-encoded JavaScript value.
  /// @param {JsValue} val - JSON-encoded asset type fetched from ledger server.
  /// @see {@link Network#getAssetProperties|Network.getAsset} for information about how to
  /// fetch an asset type from the ledger server.
  pub fn from_json(json: &JsValue) -> Result<AssetType, JsValue> {
    let asset_type: PlatformAssetType = json.into_serde().map_err(error_to_jsvalue)?;
    Ok(AssetType { asset_type })
  }

  /// Fetch the tracing policies associated with this asset type.
  pub fn get_tracing_policies(&self) -> TracingPolicies {
    TracingPolicies { policies: self.asset_type
                                    .properties
                                    .asset_rules
                                    .tracing_policies
                                    .clone() }
  }
}

#[wasm_bindgen]
impl AuthenticatedAIRResult {
  /// Construct an AIRResult from the JSON-encoded value returned by the ledger.
  /// @see {@link Network#getAIRResult|Network.getAIRResult} for information about how to fetch a
  /// value from the address identity registry.
  pub fn from_json(json: &JsValue) -> Result<AuthenticatedAIRResult, JsValue> {
    let result: PlatformAuthenticatedAIRResult = json.into_serde().map_err(error_to_jsvalue)?;
    Ok(AuthenticatedAIRResult { result })
  }

  /// Returns true if the authenticated AIR result proofs verify succesfully. If the proofs are
  /// valid, the identity commitment contained in the AIR result is a valid part of the ledger.
  /// @param {string} state_commitment - String representing the ledger state commitment.
  pub fn is_valid(&self, state_commitment: String) -> Result<bool, JsValue> {
    let state_commitment = serde_json::from_str::<HashOf<_>>(&state_commitment).map_err(|_e| {
                             JsValue::from_str("Could not deserialize state commitment")
                           })?;
    Ok(self.get_ref().is_valid(state_commitment))
  }

  /// Returns the underlying credential commitment of the AIR result.
  pub fn get_commitment(&self) -> Option<CredentialCommitment> {
    let commitment = self.get_ref().get_credential_commitment();
    commitment.map(|comm| CredentialCommitment { commitment: comm })
  }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// A user credential that can be used to selectively reveal credential attributes.
/// @see {@link wasm_credential_commit} for information about how to commit to a credential.
/// @see {@link wasm_credential_reveal} for information about how to selectively reveal credential
/// attributes.
pub struct Credential {
  pub(crate) credential: PlatformCredential,
}

impl CredentialSignature {
  pub fn get_sig_ref(&self) -> &CredSignature {
    &self.sig
  }
}

impl Credential {
  pub fn get_cred_ref(&self) -> &PlatformCredential {
    &self.credential
  }
}

impl CredentialRevealSig {
  pub fn get_sig_ref(&self) -> &CredRevealSig {
    &self.sig
  }
}

#[wasm_bindgen]
/// Key pair of a credential issuer
impl CredentialIssuerKeyPair {
  /// Returns the credential issuer's public key.
  pub fn get_pk(&self) -> CredIssuerPublicKey {
    self.pk.clone()
  }
  /// Returns the credential issuer's secret key.
  pub fn get_sk(&self) -> CredIssuerSecretKey {
    self.sk.clone()
  }
  /// Convert the key pair to a serialized value that can be used in the browser.
  pub fn to_json(&self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }
  /// Generate a key pair from a JSON-serialized JavaScript value.
  pub fn from_json(val: &JsValue) -> Self {
    val.into_serde().unwrap()
  }
}

#[wasm_bindgen]
impl CredentialUserKeyPair {
  /// Returns the credential issuer's public key.
  pub fn get_pk(&self) -> CredUserPublicKey {
    self.pk.clone()
  }
  /// Returns the credential issuer's secret key.
  pub fn get_sk(&self) -> CredUserSecretKey {
    self.sk.clone()
  }
  /// Convert the key pair to a serialized value that can be used in the browser.
  pub fn to_json(&self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }
  /// Generate a key pair from a JSON-serialized JavaScript value.
  pub fn from_json(val: &JsValue) -> Self {
    val.into_serde().unwrap()
  }
}

#[wasm_bindgen]
/// Stores threshold and weights for a multisignature requirement.
pub struct SignatureRules {
  pub(crate) sig_rules: PlatformSignatureRules,
}

#[wasm_bindgen]
/// Creates a new set of co-signature rules.
///
/// @param {BigInt} threshold - Minimum sum of signature weights that is required for an asset
/// transfer.
/// @param {JsValue} weights - Array of public key weights of the form `[["kAb...", BigInt(5)]]', where the
/// first element of each tuple is a base64 encoded public key and the second is the key's
/// associated weight.
impl SignatureRules {
  pub fn new(threshold: u64, weights: JsValue) -> Result<SignatureRules, JsValue> {
    let weights: Vec<(String, u64)> = weights.into_serde().map_err(error_to_jsvalue)?;
    let weights: Vec<(XfrPublicKey, u64)> =
      weights.iter()
             .map(|(b64_key, weight)| {
               let parsed = crate::util::public_key_from_base64(b64_key.clone());
               match parsed {
                 Err(err) => Err(err),
                 Ok(pk) => Ok((pk, *weight)),
               }
             })
             .collect::<Result<Vec<(XfrPublicKey, u64)>, JsValue>>()?;
    let sig_rules = PlatformSignatureRules { threshold, weights };
    Ok(SignatureRules { sig_rules })
  }
}

#[wasm_bindgen]
/// A collection of tracing policies. Use this object when constructing asset transfers to generate
/// the correct tracing proofs for traceable assets.
pub struct TracingPolicies {
  pub(crate) policies: AssetTracingPolicies,
}

impl TracingPolicies {
  pub fn get_policies_ref(&self) -> &AssetTracingPolicies {
    &self.policies
  }
}

#[wasm_bindgen]
/// Tracing policy for asset transfers. Can be configured to track credentials, the asset type and
/// amount, or both.
pub struct TracingPolicy {
  pub(crate) policy: AssetTracingPolicy,
}

#[wasm_bindgen]
impl TracingPolicy {
  pub fn new_with_tracking(tracing_key: &AssetTracerKeyPair) -> Self {
    let policy = AssetTracingPolicy { enc_keys: tracing_key.get_enc_key().clone(),
                                      asset_tracking: true,
                                      identity_tracking: None };
    TracingPolicy { policy }
  }

  pub fn new_with_identity_tracking(tracing_key: &AssetTracerKeyPair,
                                    cred_issuer_key: &CredIssuerPublicKey,
                                    reveal_map: JsValue,
                                    tracking: bool)
                                    -> Result<TracingPolicy, JsValue> {
    let reveal_map: Vec<bool> = reveal_map.into_serde().map_err(error_to_jsvalue)?;
    let identity_policy = IdentityRevealPolicy { cred_issuer_pub_key: cred_issuer_key.get_ref()
                                                                                     .clone(),
                                                 reveal_map };
    let policy = AssetTracingPolicy { enc_keys: tracing_key.get_enc_key().clone(),
                                      asset_tracking: tracking,
                                      identity_tracking: Some(identity_policy) };
    Ok(TracingPolicy { policy })
  }
}

impl TracingPolicy {
  pub fn get_ref(&self) -> &AssetTracingPolicy {
    &self.policy
  }
}

#[wasm_bindgen]
#[derive(Default)]
/// When an asset is defined, several options governing the assets must be
/// specified:
/// 1. **Traceable**: Records and identities of traceable assets can be decrypted by a provided tracking key. By defaults, assets do not have
/// any tracing policies.
/// 2. **Transferable**: Non-transferable assets can only be transferred once from the issuer to another user. By default, assets are transferable.
/// 3. **Updatable**: Whether the asset memo can be updated. By default, assets are not updatable.
/// 4. **Transfer signature rules**: Signature weights and threshold for a valid transfer. By
///    default, there are no special signature requirements.
/// 5. **Max units**: Optional limit on the total number of units of this asset that can be issued.
///    By default, assets do not have issuance caps.
/// @see {@link TracingPolicies} for more information about tracing policies.
/// @see {@link TransactionBuilder#add_operation_update_memo|add_operation_update_memo} for more information about how to add
/// a memo update operation to a transaction.
/// @see {@link SignatureRules} for more information about co-signatures.
/// @see {@link TransactionBuilder#add_operation_create_asset|add_operation_create_asset} for information about how to add asset rules to an asset definition.
pub struct AssetRules {
  pub(crate) rules: PlatformAssetRules,
}

#[wasm_bindgen]
impl AssetRules {
  /// Create a default set of asset rules. See class description for defaults.
  pub fn new() -> AssetRules {
    AssetRules::default()
  }

  /// Adds an asset tracing policy.
  /// @param {TracingPolicy} policy - Tracing policy for the new asset.
  pub fn add_tracing_policy(mut self, policy: &TracingPolicy) -> AssetRules {
    self.rules.tracing_policies.add(policy.get_ref().clone());
    self
  }

  /// Set a cap on the number of units of this asset that can be issued.
  /// @param {BigInt} max_units - Maximum number of units that can be issued.
  pub fn set_max_units(mut self, max_units: u64) -> AssetRules {
    self.rules.max_units = Some(max_units);
    self
  }

  /// Transferability toggle. Assets that are not transferable can only be transferred by the asset
  /// issuer.
  /// @param {boolean} transferable - Boolean indicating whether asset can be transferred.
  pub fn set_transferable(mut self, transferable: bool) -> AssetRules {
    self.rules.transferable = transferable;
    self
  }

  /// The updatable flag determines whether the asset memo can be updated after issuance.
  /// @param {boolean} updatable - Boolean indicating whether asset memo can be updated.
  /// @see {@link TransactionBuilder#add_operation_update_memo} for more information about how to add
  /// a memo update operation to a transaction.
  pub fn set_updatable(mut self, updatable: bool) -> AssetRules {
    self.rules.updatable = updatable;
    self
  }

  /// Co-signature rules. Assets with co-signatue rules require additional weighted signatures to
  /// be transferred.
  /// @param {SignatureRules} multisig_rules - Co-signature restrictions.
  pub fn set_transfer_multisig_rules(mut self, multisig_rules: SignatureRules) -> AssetRules {
    self.rules.transfer_multisig_rules = Some(multisig_rules.sig_rules);
    self
  }
}

#[wasm_bindgen]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Blinding factor for a custom data operation. A blinding factor adds a random value to the
/// custom data being hashed to make the hash hiding.
pub struct KVBlind {
  pub(crate) blind: PlatformKVBlind,
}

#[wasm_bindgen]
impl KVBlind {
  /// Generate a random blinding factor.
  pub fn gen_random() -> Self {
    let mut small_rng = ChaChaRng::from_entropy();
    let mut buf: [u8; 16] = [0u8; 16];
    small_rng.fill_bytes(&mut buf);
    KVBlind { blind: PlatformKVBlind(buf) }
  }
}

impl KVBlind {
  pub fn get_blind_ref(&self) -> &PlatformKVBlind {
    &self.blind
  }
}

#[wasm_bindgen]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Key for hashes in the ledger's custom data store.
pub struct Key(Digest);

#[wasm_bindgen]
impl Key {
  /// Generate a random key.
  /// Figure out how to store prng ref in browser: https://bugtracker.findora.org/issues/63
  pub fn gen_random() -> Self {
    let mut small_rng = ChaChaRng::from_entropy();
    let mut buf: [u8; DIGESTBYTES] = [0u8; DIGESTBYTES];
    small_rng.fill_bytes(&mut buf);
    Key(Digest::from_slice(&buf).unwrap())
  }
}

impl Key {
  pub fn get_ref(&self) -> &Digest {
    &self.0
  }
}

#[wasm_bindgen]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Hash that can be stored in the ledger's custom data store.
pub struct KVHash {
  pub(crate) hash: PlatformKVHash,
}

#[wasm_bindgen]
impl KVHash {
  /// Generate a new custom data hash without a blinding factor.
  pub fn new_no_blind(data: &str) -> Self {
    KVHash { hash: PlatformKVHash(HashOf::new(&(data.as_bytes().to_vec(), None))) }
  }

  /// Generate a new custom data hash with a blinding factor.
  pub fn new_with_blind(data: &str, kv_blind: &KVBlind) -> Self {
    KVHash { hash: PlatformKVHash(HashOf::new(&(data.as_bytes().to_vec(),
                                                Some(kv_blind.get_blind_ref().clone())))) }
  }
}

impl KVHash {
  pub fn get_hash(&self) -> &PlatformKVHash {
    &self.hash
  }
}
