use {
    core::fmt::Display,
    credentials::{
        CredCommitment, CredCommitmentKey, CredIssuerPublicKey, CredIssuerSecretKey,
        CredPoK, CredRevealSig, CredSignature, CredUserPublicKey, CredUserSecretKey,
        Credential as PlatformCredential,
    },
    globutils::{wallet, HashOf},
    ledger::data_model::{
        AssetRules as PlatformAssetRules, AssetType as PlatformAssetType,
        AuthenticatedUtxo, SignatureRules as PlatformSignatureRules, TxOutput,
        TxoRef as PlatformTxoRef, TxoSID,
    },
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::{d, err::RucResult},
    serde::{Deserialize, Serialize},
    wasm_bindgen::prelude::*,
    zei::anon_xfr::structs::{AnonBlindAssetRecord, MTLeafInfo as ZeiMTLeafInfo},
    zei::xfr::{
        sig::XfrPublicKey,
        structs::{
            AssetTracerDecKeys, AssetTracerEncKeys,
            AssetTracerKeyPair as ZeiAssetTracerKeyPair, BlindAssetRecord,
            IdentityRevealPolicy, OwnerMemo as ZeiOwnerMemo,
            TracingPolicies as ZeiTracingPolicies, TracingPolicy as ZeiTracingPolicy,
        },
    },
};

#[wasm_bindgen]
/// Indicates whether the TXO ref is an absolute or relative value.
#[derive(Copy, Clone)]
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
        TxoRef {
            txo_ref: PlatformTxoRef::Relative(idx),
        }
    }

    /// Creates an absolute transaction reference as a JSON string.
    ///
    /// Use absolute txo indexing when referring to an output that has been assigned a utxo index (i.e.
    /// when the utxo has been committed to the ledger in an earlier transaction).
    ///
    /// # Arguments
    /// @param {BigInt} idx -  Txo (transaction output) SID.
    pub fn absolute(idx: u64) -> Self {
        TxoRef {
            txo_ref: PlatformTxoRef::Absolute(TxoSID(idx)),
        }
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
    /// @see {@link module:Findora-Network~Network#getStateCommitment|getStateCommitment} for instructions on fetching a ledger state commitment.
    /// @throws Will throw an error if the state commitment fails to deserialize.
    pub fn is_valid(&self, state_commitment: String) -> Result<bool, JsValue> {
        let state_commitment = serde_json::from_str::<HashOf<_>>(&state_commitment)
            .map_err(|_e| JsValue::from_str("Could not deserialize state commitment"))?;
        Ok(self.authenticated_record.is_valid(state_commitment))
    }

    /// Builds an AuthenticatedAssetRecord from a JSON-encoded asset record returned from the ledger
    /// server.
    /// @param {JsValue} val - JSON-encoded asset record fetched from ledger server.
    /// @see {@link module:Findora-Network~Network#getUtxo|Network.getUtxo} for information about how to
    /// fetch an asset record from the ledger server.
    pub fn from_json_record(
        record: &JsValue,
    ) -> Result<AuthenticatedAssetRecord, JsValue> {
        Ok(AuthenticatedAssetRecord {
            authenticated_record: record
                .into_serde()
                .c(d!())
                .map_err(error_to_jsvalue)?,
        })
    }
}

#[wasm_bindgen]
/// This object represents an asset record owned by a ledger key pair.
/// @see {@link module:Findora-Wasm.open_client_asset_record|open_client_asset_record} for information about how to decrypt an encrypted asset
/// record.
#[derive(Clone)]
pub struct ClientAssetRecord {
    pub(crate) txo: TxOutput,
}

impl ClientAssetRecord {
    pub fn get_bar_ref(&self) -> &BlindAssetRecord {
        &self.txo.record
    }
}

#[wasm_bindgen]
impl ClientAssetRecord {
    /// Builds a client record from a JSON-encoded JavaScript value.
    ///
    /// @param {JsValue} val - JSON-encoded autehtnicated asset record fetched from ledger server with the `utxo_sid/{sid}` route,
    /// where `sid` can be fetched from the query server with the `get_owned_utxos/{address}` route.
    /// Note: The first field of an asset record is `utxo`. See the example below.
    ///
    /// @example
    /// "utxo":{
    ///   "amount":{
    ///     "NonConfidential":5
    ///   },
    ///  "asset_type":{
    ///     "NonConfidential":[113,168,158,149,55,64,18,189,88,156,133,204,156,46,106,46,232,62,69,233,157,112,240,132,164,120,4,110,14,247,109,127]
    ///   },
    ///   "public_key":"Glf8dKF6jAPYHzR_PYYYfzaWqpYcMvnrIcazxsilmlA="
    /// }
    ///
    /// @see {@link module:Findora-Network~Network#getUtxo|Network.getUtxo} for information about how to
    /// fetch an asset record from the ledger server.
    pub fn from_json(val: &JsValue) -> Result<ClientAssetRecord, JsValue> {
        Ok(ClientAssetRecord {
            txo: val.into_serde().c(d!()).map_err(error_to_jsvalue)?,
        })
    }

    /// ClientAssetRecord ==> JsValue
    pub fn to_json(&self) -> Result<JsValue, JsValue> {
        serde_json::to_string(&self.txo)
            .map(|s| JsValue::from_str(&s))
            .c(d!())
            .map_err(error_to_jsvalue)
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// Key pair used by asset tracers to decrypt asset amounts, types, and identity
/// commitments associated with traceable asset transfers.
/// @see {@link module:Findora-Wasm.TracingPolicy|TracingPolicy} for information about tracing policies.
/// @see {@link module:Findora-Wasm~AssetRules#add_tracing_policy|add_tracing_policy} for information about how to add a tracing policy to
/// an asset definition.
pub struct AssetTracerKeyPair {
    pub(crate) keypair: ZeiAssetTracerKeyPair,
}

#[wasm_bindgen]
impl AssetTracerKeyPair {
    /// Creates a new tracer key pair.
    pub fn new() -> Self {
        let mut small_rng = ChaChaRng::from_entropy();
        AssetTracerKeyPair {
            keypair: ZeiAssetTracerKeyPair::generate(&mut small_rng),
        }
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
#[derive(Deserialize, Clone)]
/// Asset owner memo. Contains information needed to decrypt an asset record.
/// @see {@link module:Findora-Wasm.ClientAssetRecord|ClientAssetRecord} for more details about asset records.
pub struct OwnerMemo {
    pub(crate) memo: ZeiOwnerMemo,
}

#[wasm_bindgen]
impl OwnerMemo {
    /// Builds an owner memo from a JSON-serialized JavaScript value.
    /// @param {JsValue} val - JSON owner memo fetched from query server with the `get_owner_memo/{sid}` route,
    /// where `sid` can be fetched from the query server with the `get_owned_utxos/{address}` route. See the example below.
    ///
    /// @example
    /// {
    ///   "blind_share":[91,251,44,28,7,221,67,155,175,213,25,183,70,90,119,232,212,238,226,142,159,200,54,19,60,115,38,221,248,202,74,248],
    ///   "lock":{"ciphertext":[119,54,117,136,125,133,112,193],"encoded_rand":"8KDql2JphPB5WLd7-aYE1bxTQAcweFSmrqymLvPDntM="}
    /// }
    pub fn from_json(val: &JsValue) -> Result<OwnerMemo, JsValue> {
        let zei_owner_memo: ZeiOwnerMemo =
            val.into_serde().c(d!()).map_err(error_to_jsvalue)?;
        Ok(OwnerMemo {
            memo: ZeiOwnerMemo {
                blind_share: zei_owner_memo.blind_share,
                lock: zei_owner_memo.lock,
            },
        })
    }

    /// Creates a clone of the owner memo.
    pub fn clone(&self) -> Self {
        OwnerMemo {
            memo: self.memo.clone(),
        }
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
impl CredentialRevealSig {
    /// Returns the underlying credential commitment.
    /// @see {@link module:Findora-Wasm.wasm_credential_verify_commitment|wasm_credential_verify_commitment} for information about how to verify a
    /// credential commitment.
    pub fn get_commitment(&self) -> CredentialCommitment {
        CredentialCommitment {
            commitment: self.sig.sig_commitment.clone(),
        }
    }
    /// Returns the underlying proof of knowledge that the credential is valid.
    /// @see {@link module:Findora-Wasm.wasm_credential_verify_commitment|wasm_credential_verify_commitment} for information about how to verify a
    /// credential commitment.
    pub fn get_pok(&self) -> CredentialPoK {
        CredentialPoK {
            pok: self.sig.pok.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// Commitment to a credential record, proof that the commitment is valid, and credential key that can be used
/// to open a commitment.
pub struct CredentialCommitmentData {
    pub(crate) commitment: CredentialCommitment,
    pub(crate) pok: CredentialPoK,
    pub(crate) commitment_key: CredentialCommitmentKey,
}

#[wasm_bindgen]
impl CredentialCommitmentData {
    /// Returns the underlying credential commitment.
    /// @see {@link module:Findora-Wasm.wasm_credential_verify_commitment|wasm_credential_verify_commitment} for information about how to verify a
    /// credential commitment.
    pub fn get_commitment(&self) -> CredentialCommitment {
        self.commitment.clone()
    }
    /// Returns the underlying proof of knowledge that the credential is valid.
    /// @see {@link module:Findora-Wasm.wasm_credential_verify_commitment|wasm_credential_verify_commitment} for information about how to verify a
    /// credential commitment.
    pub fn get_pok(&self) -> CredentialPoK {
        self.pok.clone()
    }

    /// Returns the key used to generate the commitment.
    /// @see {@link module:Findora-Wasm.wasm_credential_open_commitment|wasm_credential_open_commitment} for information about how to open a
    /// credential commitment.
    pub fn get_commit_key(&self) -> CredentialCommitmentKey {
        self.commitment_key.clone()
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
/// Commitment to a credential record.
/// @see {@link module:Findora-Wasm.wasm_credential_verify_commitment|wasm_credential_verify_commitment} for information about how to verify a
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
/// @see {@link module:Findora-Wasm.wasm_credential_verify_commitment|wasm_credential_verify_commitment} for information about how to verify a
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
#[derive(Serialize, Deserialize, Clone)]
/// Key used to generate a credential commitment.
/// @see {@link module:Findora-Wasm.wasm_credential_open_commitment|wasm_credential_open_commitment} for information about how to
/// open a credential commitment.
pub struct CredentialCommitmentKey {
    pub(crate) key: CredCommitmentKey,
}

impl CredentialCommitmentKey {
    pub fn get_ref(&self) -> &CredCommitmentKey {
        &self.key
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
    /// @param {JsValue} val - JSON-encoded asset type fetched from ledger server with the `asset_token/{code}` route.
    /// Note: The first field of an asset type is `properties`. See the example below.
    ///
    /// @example
    /// "properties":{
    ///   "code":{
    ///     "val":[151,8,106,38,126,101,250,236,134,77,83,180,43,152,47,57,83,30,60,8,132,218,48,52,167,167,190,244,34,45,78,80]
    ///   },
    ///   "issuer":{"key":“iFW4jY_DQVSGED05kTseBBn0BllPB9Q9escOJUpf4DY=”},
    ///   "memo":“test memo”,
    ///   "asset_rules":{
    ///     "transferable":true,
    ///     "updatable":false,
    ///     "transfer_multisig_rules":null,
    ///     "max_units":5000
    ///   }
    /// }
    ///
    /// @see {@link module:Findora-Network~Network#getAssetProperties|Network.getAsset} for information about how to
    /// fetch an asset type from the ledger server.
    pub fn from_json(json: &JsValue) -> Result<AssetType, JsValue> {
        let asset_type: PlatformAssetType =
            json.into_serde().c(d!()).map_err(error_to_jsvalue)?;
        Ok(AssetType { asset_type })
    }

    /// Fetch the tracing policies associated with this asset type.
    pub fn get_tracing_policies(&self) -> TracingPolicies {
        TracingPolicies {
            policies: self
                .asset_type
                .properties
                .asset_rules
                .tracing_policies
                .clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
/// A user credential that can be used to selectively reveal credential attributes.
/// @see {@link module:Findora-Wasm.wasm_credential_commit|wasm_credential_commit} for information about how to commit to a credential.
/// @see {@link module:Findora-Wasm.wasm_credential_reveal|wasm_credential_reveal} for information about how to selectively reveal credential
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
    pub fn from_json(val: &JsValue) -> Result<CredentialIssuerKeyPair, JsValue> {
        val.into_serde().c(d!()).map_err(error_to_jsvalue)
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
    pub fn from_json(val: &JsValue) -> Result<CredentialUserKeyPair, JsValue> {
        val.into_serde().c(d!()).map_err(error_to_jsvalue)
    }
}

#[wasm_bindgen]
/// Stores threshold and weights for a multisignature requirement.
pub struct SignatureRules {
    pub(crate) sig_rules: PlatformSignatureRules,
}

#[wasm_bindgen]
impl SignatureRules {
    /// Creates a new set of co-signature rules.
    ///
    /// @param {BigInt} threshold - Minimum sum of signature weights that is required for an asset
    /// transfer.
    /// @param {JsValue} weights - Array of public key weights of the form `[["kAb...", BigInt(5)]]', where the
    /// first element of each tuple is a base64 encoded public key and the second is the key's
    /// associated weight.
    pub fn new(threshold: u64, weights: JsValue) -> Result<SignatureRules, JsValue> {
        let weights: Vec<(String, u64)> =
            weights.into_serde().c(d!()).map_err(error_to_jsvalue)?;
        let weights: Vec<(XfrPublicKey, u64)> = weights
            .iter()
            .map(|(b64_key, weight)| {
                wallet::public_key_from_base64(&b64_key)
                    .map(|pk| (pk, *weight))
                    .c(d!())
                    .map_err(error_to_jsvalue)
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
    pub(crate) policies: ZeiTracingPolicies,
}

impl TracingPolicies {
    pub fn get_policies_ref(&self) -> &ZeiTracingPolicies {
        &self.policies
    }
}

#[wasm_bindgen]
/// Tracing policy for asset transfers. Can be configured to track credentials, the asset type and
/// amount, or both.
pub struct TracingPolicy {
    pub(crate) policy: ZeiTracingPolicy,
}

#[wasm_bindgen]
impl TracingPolicy {
    pub fn new_with_tracing(tracing_key: &AssetTracerKeyPair) -> Self {
        let policy = ZeiTracingPolicy {
            enc_keys: tracing_key.get_enc_key().clone(),
            asset_tracing: true,
            identity_tracing: None,
        };
        TracingPolicy { policy }
    }

    pub fn new_with_identity_tracing(
        tracing_key: &AssetTracerKeyPair,
        cred_issuer_key: &CredIssuerPublicKey,
        reveal_map: JsValue,
        tracing: bool,
    ) -> Result<TracingPolicy, JsValue> {
        let reveal_map: Vec<bool> =
            reveal_map.into_serde().c(d!()).map_err(error_to_jsvalue)?;
        let identity_policy = IdentityRevealPolicy {
            cred_issuer_pub_key: cred_issuer_key.get_ref().clone(),
            reveal_map,
        };
        let policy = ZeiTracingPolicy {
            enc_keys: tracing_key.get_enc_key().clone(),
            asset_tracing: tracing,
            identity_tracing: Some(identity_policy),
        };
        Ok(TracingPolicy { policy })
    }
}

impl TracingPolicy {
    pub fn get_ref(&self) -> &ZeiTracingPolicy {
        &self.policy
    }
}

#[wasm_bindgen]
#[derive(Default)]
/// When an asset is defined, several options governing the assets must be
/// specified:
/// 1. **Traceable**: Records and identities of traceable assets can be decrypted by a provided tracing key. By defaults, assets do not have
/// any tracing policies.
/// 2. **Transferable**: Non-transferable assets can only be transferred once from the issuer to another user. By default, assets are transferable.
/// 3. **Updatable**: Whether the asset memo can be updated. By default, assets are not updatable.
/// 4. **Transfer signature rules**: Signature weights and threshold for a valid transfer. By
///    default, there are no special signature requirements.
/// 5. **Max units**: Optional limit on the total number of units of this asset that can be issued.
///    By default, assets do not have issuance caps.
/// @see {@link module:Findora-Wasm~TracingPolicies|TracingPolicies} for more information about tracing policies.
/// @see {@link module:Findora-Wasm~TransactionBuilder#add_operation_update_memo|add_operation_update_memo} for more information about how to add
/// a memo update operation to a transaction.
/// @see {@link module:Findora-Wasm~SignatureRules|SignatureRules} for more information about co-signatures.
/// @see {@link
/// module:Findora-Wasm~TransactionBuilder#add_operation_create_asset|add_operation_create_asset}
/// for information about how to add asset rules to an asset definition.
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
        self.rules.add_tracing_policy(policy.get_ref().clone());
        self
    }

    /// Set a cap on the number of units of this asset that can be issued.
    /// @param {BigInt} max_units - Maximum number of units that can be issued.
    pub fn set_max_units(mut self, max_units: u64) -> AssetRules {
        self.rules.set_max_units(Some(max_units));
        self
    }

    /// Transferability toggle. Assets that are not transferable can only be transferred by the asset
    /// issuer.
    /// @param {boolean} transferable - Boolean indicating whether asset can be transferred.
    pub fn set_transferable(mut self, transferable: bool) -> AssetRules {
        self.rules.set_transferable(transferable);
        self
    }

    /// The updatable flag determines whether the asset memo can be updated after issuance.
    /// @param {boolean} updatable - Boolean indicating whether asset memo can be updated.
    /// @see {@link module:Findora-Wasm~TransactionBuilder#add_operation_update_memo|add_operation_update_memo} for more information about how to add
    /// a memo update operation to a transaction.
    pub fn set_updatable(mut self, updatable: bool) -> AssetRules {
        self.rules.set_updatable(updatable);
        self
    }

    /// Co-signature rules. Assets with co-signatue rules require additional weighted signatures to
    /// be transferred.
    /// @param {SignatureRules} multisig_rules - Co-signature restrictions.
    pub fn set_transfer_multisig_rules(
        mut self,
        multisig_rules: SignatureRules,
    ) -> AssetRules {
        self.rules
            .set_transfer_multisig_rules(Some(multisig_rules.sig_rules));
        self
    }

    /// Set the decimal number of asset. Return error string if failed, otherwise return changed asset.
    /// #param {Number} decimals - The number of decimals used to set its user representation.
    /// Decimals should be 0 ~ 255.
    pub fn set_decimals(mut self, decimals: u8) -> Result<AssetRules, JsValue> {
        self.rules
            .set_decimals(decimals)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }
}

#[inline(always)]
pub(crate) fn error_to_jsvalue<T: Display>(e: T) -> JsValue {
    JsValue::from_str(&e.to_string())
}

#[wasm_bindgen]
#[derive(Default, Clone)]
pub struct MTLeafInfo {
    object: ZeiMTLeafInfo,
}

impl MTLeafInfo {
    pub fn get_zei_mt_leaf_info(&self) -> &ZeiMTLeafInfo {
        &self.object
    }
}

#[wasm_bindgen]
impl MTLeafInfo {
    pub fn from_json(json: &JsValue) -> Result<MTLeafInfo, JsValue> {
        let mt_leaf_info: ZeiMTLeafInfo =
            json.into_serde().c(d!()).map_err(error_to_jsvalue)?;
        Ok(MTLeafInfo {
            object: mt_leaf_info,
        })
    }

    pub fn to_json(&self) -> Result<JsValue, JsValue> {
        serde_json::to_string(&self.object)
            .map(|s| JsValue::from_str(&s))
            .c(d!())
            .map_err(error_to_jsvalue)
    }
}

/// AnonKeys is used to store keys for Anon proofs
#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct AnonKeys {
    pub(crate) axfr_secret_key: String,
    pub(crate) axfr_public_key: String,
    pub(crate) enc_key: String,
    pub(crate) dec_key: String,
}

/// AnonKeys is a struct to store keys required for anon transfer
#[wasm_bindgen]
#[allow(missing_docs)]
impl AnonKeys {
    pub fn from_json(json: &JsValue) -> Result<AnonKeys, JsValue> {
        let anon_keys: AnonKeys = json.into_serde().c(d!()).map_err(error_to_jsvalue)?;
        Ok(anon_keys)
    }

    pub fn to_json(&self) -> Result<JsValue, JsValue> {
        serde_json::to_string(&self)
            .map(|s| JsValue::from_str(&s))
            .c(d!())
            .map_err(error_to_jsvalue)
    }

    #[wasm_bindgen(getter)]
    pub fn axfr_secret_key(&self) -> String {
        self.axfr_secret_key.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_axfr_secret_key(&mut self, axfr_secret_key: String) {
        self.axfr_secret_key = axfr_secret_key;
    }

    #[wasm_bindgen(getter)]
    pub fn axfr_public_key(&self) -> String {
        self.axfr_public_key.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_axfr_public_key(&mut self, axfr_public_key: String) {
        self.axfr_public_key = axfr_public_key;
    }

    #[wasm_bindgen(getter)]
    pub fn enc_key(&self) -> String {
        self.enc_key.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_enc_key(&mut self, enc_key: String) {
        self.enc_key = enc_key;
    }

    #[wasm_bindgen(getter)]
    pub fn dec_key(&self) -> String {
        self.dec_key.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_dec_key(&mut self, dec_key: String) {
        self.dec_key = dec_key;
    }
}
