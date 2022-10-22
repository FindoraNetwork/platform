use crate::rust::TransactionBuilder as TxBuilder;
use crate::rust::TransferOperationBuilder as TxOpBuilder;
use crate::rust::*;
use credentials::{
    CredIssuerPublicKey, CredIssuerSecretKey, CredUserPublicKey, CredUserSecretKey,
};
use noah::xfr::sig::{XfrKeyPair, XfrPublicKey};
use noah::xfr::structs::ASSET_TYPE_LENGTH;
use ruc::{d, err::RucResult};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
/// Generates asset type as a Base64 string from a JSON-serialized JavaScript value.
pub fn asset_type_from_jsvalue(val: &JsValue) -> Result<String, JsValue> {
    let code: [u8; ASSET_TYPE_LENGTH] =
        val.into_serde().c(d!()).map_err(error_to_jsvalue)?;
    Ok(rs_asset_type_from_value(code))
}

#[wasm_bindgen]
/// Given a serialized state commitment and transaction, returns true if the transaction correctly
/// hashes up to the state commitment and false otherwise.
/// @param {string} state_commitment - String representing the state commitment.
/// @param {string} authenticated_txn - String representing the transaction.
/// @see {@link module:Network~Network#getTxn|Network.getTxn} for instructions on fetching a transaction from the ledger.
/// @see {@link module:Network~Network#getStateCommitment|Network.getStateCommitment}
/// for instructions on fetching a ledger state commitment.
/// @throws Will throw an error if the state commitment or the transaction fails to deserialize.
pub fn verify_authenticated_txn(
    state_commitment: String,
    authenticated_txn: String,
) -> Result<bool, JsValue> {
    rs_verify_authenticated_txn(state_commitment, authenticated_txn)
        .c(d!())
        .map_err(error_to_jsvalue)
}

#[wasm_bindgen]
/// Structure that allows users to construct arbitrary transactions.
pub struct TransactionBuilder(TxBuilder);

#[wasm_bindgen]
impl TransactionBuilder {
    /// @param am: amount to pay
    /// @param kp: owner's XfrKeyPair
    pub fn add_fee_relative_auto(
        self,
        kp: XfrKeyPair,
    ) -> Result<TransactionBuilder, JsValue> {
        let builder = self
            .0
            .add_fee_relative_auto(kp)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(TransactionBuilder(builder))
    }

    /// Use this func to get the necessary infomations for generating `Relative Inputs`
    ///
    /// - TxoRef::Relative("Element index of the result")
    /// - ClientAssetRecord::from_json("Element of the result")
    pub fn get_relative_outputs(&self) -> Vec<JsValue> {
        self.0
            .get_relative_outputs()
            .into_iter()
            .fold(vec![], |mut base, new| {
                base.push(new.to_json().unwrap());
                base
            })
    }

    /// As the last operation of any transaction,
    /// add a static fee to the transaction.
    pub fn add_fee(self, inputs: FeeInputs) -> Result<TransactionBuilder, JsValue> {
        let builder = self
            .0
            .add_fee(inputs.into())
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(TransactionBuilder(builder))
    }

    /// A simple fee checker for mainnet v1.0.
    ///
    /// SEE [check_fee](ledger::data_model::Transaction::check_fee)
    pub fn check_fee(&self) -> bool {
        self.0.check_fee()
    }

    /// Create a new transaction builder.
    /// @param {BigInt} seq_id - Unique sequence ID to prevent replay attacks.
    pub fn new(seq_id: u64) -> Self {
        TransactionBuilder(TxBuilder::new(seq_id))
    }

    /// Wraps around TransactionBuilder to add an asset definition operation to a transaction builder instance.
    /// @example <caption> Error handling </caption>
    /// try {
    ///     await wasm.add_operation_create_asset(wasm.new_keypair(), "test_memo", wasm.random_asset_type(), wasm.AssetRules.default());
    /// } catch (err) {
    ///     console.log(err)
    /// }
    ///
    /// @param {XfrKeyPair} key_pair -  Issuer XfrKeyPair.
    /// @param {string} memo - Text field for asset definition.
    /// @param {string} token_code - Optional Base64 string representing the token code of the asset to be issued.
    /// If empty, a token code will be chosen at random.
    /// @param {AssetRules} asset_rules - Asset rules object specifying which simple policies apply
    /// to the asset.
    pub fn add_operation_create_asset(
        self,
        key_pair: &XfrKeyPair,
        memo: String,
        token_code: String,
        asset_rules: AssetRules,
    ) -> Result<TransactionBuilder, JsValue> {
        let builder = self
            .0
            .add_operation_create_asset_with_policy(
                key_pair,
                memo,
                token_code,
                String::new(),
                asset_rules,
            )
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(TransactionBuilder(builder))
    }

    /// @ignore
    // Testnet will not support Discret policies.
    pub fn add_operation_create_asset_with_policy(
        self,
        key_pair: &XfrKeyPair,
        memo: String,
        token_code: String,
        policy_choice: String,
        asset_rules: AssetRules,
    ) -> Result<TransactionBuilder, JsValue> {
        let builder = self
            .0
            .add_operation_create_asset_with_policy(
                key_pair,
                memo,
                token_code,
                policy_choice,
                asset_rules,
            )
            .c(d!())
            .map_err(error_to_jsvalue)?;

        Ok(TransactionBuilder(builder))
    }

    /// Wraps around TransactionBuilder to add an asset issuance to a transaction builder instance.
    ///
    /// Use this function for simple one-shot issuances.
    ///
    /// @param {XfrKeyPair} key_pair  - Issuer XfrKeyPair.
    /// and types of traced assets.
    /// @param {string} code - base64 string representing the token code of the asset to be issued.
    /// @param {BigInt} seq_num - Issuance sequence number. Every subsequent issuance of a given asset type must have a higher sequence number than before.
    /// @param {BigInt} amount - Amount to be issued.
    /// @param {boolean} conf_amount - `true` means the asset amount is confidential, and `false` means it's nonconfidential.
    pub fn add_basic_issue_asset(
        self,
        key_pair: &XfrKeyPair,
        code: String,
        seq_num: u64,
        amount: u64,
        conf_amount: bool,
    ) -> Result<TransactionBuilder, JsValue> {
        let builder = self
            .0
            .add_basic_issue_asset(key_pair, code, seq_num, amount, conf_amount)
            .c(d!())
            .map_err(error_to_jsvalue)?;

        Ok(TransactionBuilder(builder))
    }

    /// Adds an operation to the transaction builder that adds a hash to the ledger's custom data
    /// store.
    /// @param {XfrKeyPair} auth_key_pair - Asset creator key pair.
    /// @param {String} code - base64 string representing token code of the asset whose memo will be updated.
    /// transaction validates.
    /// @param {String} new_memo - The new asset memo.
    /// @see {@link module:Findora-Wasm~AssetRules#set_updatable|AssetRules.set_updatable} for more information about how
    /// to define an updatable asset.
    pub fn add_operation_update_memo(
        self,
        auth_key_pair: &XfrKeyPair,
        code: String,
        new_memo: String,
    ) -> Result<TransactionBuilder, JsValue> {
        let builder = self
            .0
            .add_operation_update_memo(auth_key_pair, code, new_memo)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(TransactionBuilder(builder))
    }

    /// Adds a serialized transfer asset operation to a transaction builder instance.
    /// @param {string} op - a JSON-serialized transfer operation.
    /// @see {@link module:Findora-Wasm~TransferOperationBuilder} for details on constructing a transfer operation.
    /// @throws Will throw an error if `op` fails to deserialize.
    pub fn add_transfer_operation(
        self,
        op: String,
    ) -> Result<TransactionBuilder, JsValue> {
        let builder = self
            .0
            .add_transfer_operation(op)
            .c(d!())
            .map_err(error_to_jsvalue)?;

        Ok(TransactionBuilder(builder))
    }

    pub fn sign(self, kp: &XfrKeyPair) -> Result<TransactionBuilder, JsValue> {
        let builder = self.0.sign(kp).c(d!()).map_err(error_to_jsvalue)?;
        Ok(TransactionBuilder(builder))
    }

    /// Extracts the serialized form of a transaction.
    pub fn transaction(&self) -> String {
        self.0.transaction()
    }

    /// Calculates transaction handle.
    pub fn transaction_handle(&self) -> String {
        self.0.transaction_handle()
    }

    /// Fetches a client record from a transaction.
    /// @param {number} idx - Record to fetch. Records are added to the transaction builder sequentially.
    pub fn get_owner_record(&self, idx: usize) -> ClientAssetRecord {
        self.0.get_owner_record(idx)
    }

    /// Fetches an owner memo from a transaction
    /// @param {number} idx - Owner memo to fetch. Owner memos are added to the transaction builder sequentially.
    pub fn get_owner_memo(&self, idx: usize) -> Option<OwnerMemo> {
        self.0.get_owner_memo(idx)
    }
}

#[wasm_bindgen]
#[derive(Default)]
/// Structure that enables clients to construct complex transfers.
pub struct TransferOperationBuilder(TxOpBuilder);

#[wasm_bindgen]
impl TransferOperationBuilder {
    /// Create a new transfer operation builder.
    pub fn new() -> Self {
        Self::default()
    }

    // Debug function that does not need to go into the docs.
    /// @ignore
    pub fn debug(&self) -> String {
        self.0.debug()
    }

    /// Wraps around TransferOperationBuilder to add an input to a transfer operation builder.
    /// @param {TxoRef} txo_ref - Absolute or relative utxo reference
    /// @param {string} asset_record - Serialized client asset record to serve as transfer input. This record must exist on the
    /// ledger for the transfer to be valid.
    /// @param {OwnerMemo} owner_memo - Opening parameters.
    /// @param tracing_key {AssetTracerKeyPair} - Tracing key, must be added to traceable
    /// assets.
    /// @param {XfrKeyPair} key - Key pair associated with the input.
    /// @param {BigInt} amount - Amount of input record to transfer.
    /// @see {@link module:Findora-Wasm~TxoRef#create_absolute_txo_ref|TxoRef.create_absolute_txo_ref}
    /// or {@link module:Findora-Wasm~TxoRef#create_relative_txo_ref|TxoRef.create_relative_txo_ref} for details on txo
    /// references.
    /// @see {@link module:Findora-Network~Network#getUtxo|Network.getUtxo} for details on fetching blind asset records.
    /// @throws Will throw an error if `oar` or `txo_ref` fail to deserialize.
    pub fn add_input_with_tracing(
        self,
        txo_ref: TxoRef,
        asset_record: ClientAssetRecord,
        owner_memo: Option<OwnerMemo>,
        tracing_policies: &TracingPolicies,
        key: &XfrKeyPair,
        amount: u64,
    ) -> Result<TransferOperationBuilder, JsValue> {
        let builder = self
            .0
            .add_input_with_tracing(
                txo_ref,
                asset_record,
                owner_memo,
                tracing_policies,
                key,
                amount,
            )
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(TransferOperationBuilder(builder))
    }
    /// Wraps around TransferOperationBuilder to add an input to a transfer operation builder.
    /// @param {TxoRef} txo_ref - Absolute or relative utxo reference
    /// @param {string} asset_record - Serialized client asset record to serve as transfer input. This record must exist on the
    /// ledger for the transfer to be valid
    /// @param {OwnerMemo} owner_memo - Opening parameters.
    /// @param {XfrKeyPair} key - Key pair associated with the input.
    /// @param {BigInt} amount - Amount of input record to transfer
    /// or {@link module:Findora-Wasm~TxoRef#create_relative_txo_ref|TxoRef.create_relative_txo_ref} for details on txo
    /// references.
    /// @see {@link module:Findora-Network~Network#getUtxo|Network.getUtxo} for details on fetching blind asset records.
    /// @throws Will throw an error if `oar` or `txo_ref` fail to deserialize.
    // Note: these two functions are necessary because Wasm cannot handle optional references and I
    // don't want any of the functions to take ownership of the tracing key.
    pub fn add_input_no_tracing(
        self,
        txo_ref: TxoRef,
        asset_record: &ClientAssetRecord,
        owner_memo: Option<OwnerMemo>,
        key: &XfrKeyPair,
        amount: u64,
    ) -> Result<TransferOperationBuilder, JsValue> {
        let builder = self
            .0
            .add_input_no_tracing(txo_ref, asset_record, owner_memo, key, amount)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(TransferOperationBuilder(builder))
    }

    /// Wraps around TransferOperationBuilder to add an output to a transfer operation builder.
    ///
    /// @param {BigInt} amount - amount to transfer to the recipient.
    /// @param {XfrPublicKey} recipient - public key of the recipient.
    /// @param tracing_key {AssetTracerKeyPair} - Optional tracing key, must be added to traced
    /// assets.
    /// @param code {string} - String representation of the asset token code.
    /// @param conf_amount {boolean} - `true` means the output's asset amount is confidential, and `false` means it's nonconfidential.
    /// @param conf_type {boolean} - `true` means the output's asset type is confidential, and `false` means it's nonconfidential.
    /// @throws Will throw an error if `code` fails to deserialize.
    pub fn add_output_with_tracing(
        self,
        amount: u64,
        recipient: &XfrPublicKey,
        tracing_policies: &TracingPolicies,
        code: String,
        conf_amount: bool,
        conf_type: bool,
    ) -> Result<TransferOperationBuilder, JsValue> {
        let builder = self
            .0
            .add_output_with_tracing(
                amount,
                recipient,
                tracing_policies,
                code,
                conf_amount,
                conf_type,
            )
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(TransferOperationBuilder(builder))
    }

    /// Wraps around TransferOperationBuilder to add an output to a transfer operation builder.
    ///
    /// @param {BigInt} amount - amount to transfer to the recipient
    /// @param {XfrPublicKey} recipient - public key of the recipient
    /// @param code {string} - String representaiton of the asset token code
    /// @param conf_amount {boolean} - `true` means the output's asset amount is confidential, and `false` means it's nonconfidential.
    /// @param conf_type {boolean} - `true` means the output's asset type is confidential, and `false` means it's nonconfidential.
    /// @throws Will throw an error if `code` fails to deserialize.
    pub fn add_output_no_tracing(
        self,
        amount: u64,
        recipient: &XfrPublicKey,
        code: String,
        conf_amount: bool,
        conf_type: bool,
    ) -> Result<TransferOperationBuilder, JsValue> {
        let builder = self
            .0
            .add_output_no_tracing(amount, recipient, code, conf_amount, conf_type)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(TransferOperationBuilder(builder))
    }

    /// Wraps around TransferOperationBuilder to ensure the transfer inputs and outputs are balanced.
    /// This function will add change outputs for all unspent portions of input records.
    /// @throws Will throw an error if the transaction cannot be balanced.
    pub fn balance(self) -> Result<TransferOperationBuilder, JsValue> {
        let builder =
            self.0.balance().c(d!()).map_err(|e| {
                JsValue::from_str(&format!("Error balancing txn: {}", e))
            })?;
        Ok(TransferOperationBuilder(builder))
    }

    /// Wraps around TransferOperationBuilder to finalize the transaction.
    ///
    /// @throws Will throw an error if input and output amounts do not add up.
    /// @throws Will throw an error if not all record owners have signed the transaction.
    pub fn create(self) -> Result<TransferOperationBuilder, JsValue> {
        let builder = self.0.create().c(d!()).map_err(error_to_jsvalue)?;
        Ok(TransferOperationBuilder(builder))
    }

    /// Wraps around TransferOperationBuilder to add a signature to the operation.
    ///
    /// All input owners must sign.
    ///
    /// @param {XfrKeyPair} kp - key pair of one of the input owners.
    pub fn sign(self, kp: &XfrKeyPair) -> Result<TransferOperationBuilder, JsValue> {
        let builder = self.0.sign(kp).c(d!()).map_err(error_to_jsvalue)?;
        Ok(TransferOperationBuilder(builder))
    }

    pub fn builder(&self) -> String {
        self.0.builder()
    }

    /// Wraps around TransferOperationBuilder to extract an operation expression as JSON.
    pub fn transaction(&self) -> Result<String, JsValue> {
        self.0.transaction().c(d!()).map_err(error_to_jsvalue)
    }
}

#[wasm_bindgen]
/// Returns a JavaScript object containing decrypted owner record information,
/// where `amount` is the decrypted asset amount, and `asset_type` is the decrypted asset type code.
///
/// @param {ClientAssetRecord} record - Owner record.
/// @param {OwnerMemo} owner_memo - Owner memo of the associated record.
/// @param {XfrKeyPair} keypair - Keypair of asset owner.
/// @see {@link module:Findora-Wasm~ClientAssetRecord#from_json_record|ClientAssetRecord.from_json_record} for information about how to construct an asset record object
/// from a JSON result returned from the ledger server.
pub fn open_client_asset_record(
    record: &ClientAssetRecord,
    owner_memo: Option<OwnerMemo>,
    keypair: &XfrKeyPair,
) -> Result<JsValue, JsValue> {
    rs_open_client_asset_record(record, owner_memo, keypair)
        .c(d!())
        .map_err(|e| JsValue::from_str(&format!("Could not open asset record: {}", e)))
        .and_then(|oa| JsValue::from_serde(&oa).c(d!()).map_err(error_to_jsvalue))
}

#[wasm_bindgen]
/// Converts a base64 encoded public key string to a public key.
pub fn public_key_from_base64(pk: &str) -> Result<XfrPublicKey, JsValue> {
    rs_public_key_from_base64(pk)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Generates a new credential issuer key.
/// @param {JsValue} attributes - Array of attribute types of the form `[{name: "credit_score",
/// size: 3}]`. The size refers to byte-size of the credential. In this case, the "credit_score"
/// attribute is represented as a 3 byte string "760". `attributes` is the list of attribute types
/// that the issuer can sign off on.
#[wasm_bindgen]
pub fn wasm_credential_issuer_key_gen(attributes: JsValue) -> CredentialIssuerKeyPair {
    let attributes: Vec<AttributeDefinition> = attributes.into_serde().unwrap();
    rs_wasm_credential_issuer_key_gen(attributes)
}

/// Verifies a credential commitment. Used to confirm that a credential is tied to a ledger
/// address.
/// @param {CredIssuerPublicKey} issuer_pub_key - The credential issuer that has attested to the
/// credentials that have been committed to.
/// @param {CredentialCommitment} Credential commitment
/// @param {CredPoK} Proof of knowledge of the underlying commitment
/// @param {XfrPublicKey} Ledger address linked to this credential commitment.
/// @throws Will throw an error during verification failure (i.e. the supplied ledger address is
/// incorrect, the commitment is tied to a different credential issuer, or the proof of knowledge is
/// invalid, etc.)
#[wasm_bindgen]
pub fn wasm_credential_verify_commitment(
    issuer_pub_key: &CredIssuerPublicKey,
    commitment: &CredentialCommitment,
    pok: &CredentialPoK,
    xfr_pk: &XfrPublicKey,
) -> Result<(), JsValue> {
    rs_wasm_credential_verify_commitment(issuer_pub_key, commitment, pok, xfr_pk)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Generates a new reveal proof from a credential commitment key.
/// @param {CredUserSecretKey} user_secret_key - Secret key of the credential user who owns
/// the credentials.
/// @param {Credential} credential - Credential whose attributes will be revealed.
/// @param {JsValue} reveal_fields - Array of strings representing attribute fields to reveal.
/// @throws Will throw an error if a reveal proof cannot be generated from the credential
/// or ```reveal_fields``` fails to deserialize.
#[wasm_bindgen]
pub fn wasm_credential_open_commitment(
    user_secret_key: &CredUserSecretKey,
    credential: &Credential,
    key: &CredentialCommitmentKey,
    reveal_fields: JsValue,
) -> Result<CredentialPoK, JsValue> {
    let reveal_fields: Vec<String> = reveal_fields.into_serde().c(d!()).map_err(|e| JsValue::from(&format!("Could not deserialize reveal fields. Please ensure that reveal fields are of the form [String]: {}", e)))?;
    rs_wasm_credential_open_commitment(user_secret_key, credential, key, reveal_fields)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Generates a signature on user attributes that can be used to create a credential.
/// @param {CredIssuerSecretKey} issuer_secret_key - Secret key of credential issuer.
/// @param {CredUserPublicKey} user_public_key - Public key of credential user.
/// @param {JsValue} attributes - Array of attribute assignments of the form `[{name: "credit_score",
/// val: "760"}]`.
/// @throws Will throw an error if the signature cannot be generated.
#[wasm_bindgen]
pub fn wasm_credential_sign(
    issuer_secret_key: &CredIssuerSecretKey,
    user_public_key: &CredUserPublicKey,
    attributes: JsValue,
) -> Result<CredentialSignature, JsValue> {
    let attributes: Vec<AttributeAssignment> = attributes.into_serde().c(d!()).map_err(|e| JsValue::from(&format!("Could not deserialize attributes. Please ensure that attribute definition is of the form [{{name: string, val: string}}]: {}", e)))?;
    rs_wasm_credential_sign(issuer_secret_key, user_public_key, attributes)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Generates a signature on user attributes that can be used to create a credential.
/// @param {CredIssuerPublicKey} issuer_public_key - Public key of credential issuer.
/// @param {CredentialSignature} signature - Credential issuer signature on attributes.
/// @param {JsValue} attributes - Array of attribute assignments of the form `[{name: "credit_score",
/// val: "760"}]'.
#[wasm_bindgen]
pub fn create_credential(
    issuer_public_key: &CredIssuerPublicKey,
    signature: &CredentialSignature,
    attributes: &JsValue,
) -> Credential {
    let attributes: Vec<AttributeAssignment> = attributes.into_serde().unwrap();
    rs_create_credential(issuer_public_key, signature, attributes)
}

/// Generates a credential commitment. A credential commitment can be used to selectively reveal
/// attribute assignments.
/// @param {CredUserSecretKey} user_secret_key - Secret key of credential user.
/// @param {XfrPublicKey} user_public_key - Ledger signing key to link this credential to.
/// @param {Credential} credential - Credential object.
#[wasm_bindgen]
pub fn wasm_credential_commit(
    user_secret_key: &CredUserSecretKey,
    user_public_key: &XfrPublicKey,
    credential: &Credential,
) -> Result<CredentialCommitmentData, JsValue> {
    rs_wasm_credential_commit(user_secret_key, user_public_key, credential)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Selectively reveals attributes committed to in a credential commitment
/// @param {CredUserSecretKey} user_sk - Secret key of credential user.
/// @param {Credential} credential - Credential object.
/// @param {JsValue} reveal_fields - Array of string names representing credentials to reveal (i.e.
/// `["credit_score"]`).
#[wasm_bindgen]
pub fn wasm_credential_reveal(
    user_sk: &CredUserSecretKey,
    credential: &Credential,
    reveal_fields: JsValue,
) -> Result<CredentialRevealSig, JsValue> {
    let reveal_fields: Vec<String> = reveal_fields.into_serde().unwrap();
    rs_wasm_credential_reveal(user_sk, credential, reveal_fields)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Verifies revealed attributes from a commitment.
/// @param {CredIssuerPublicKey} issuer_pub_key - Public key of credential issuer.
/// @param {JsValue} attributes - Array of attribute assignments to check of the form `[{name: "credit_score",
/// val: "760"}]`.
/// @param {CredentialCommitment} commitment - Commitment to the credential.
/// @param {CredentialPoK} pok - Proof that the credential commitment is valid and commits
/// to the attribute values being revealed.
#[wasm_bindgen]
pub fn wasm_credential_verify(
    issuer_pub_key: &CredIssuerPublicKey,
    attributes: JsValue,
    commitment: &CredentialCommitment,
    pok: &CredentialPoK,
) -> Result<(), JsValue> {
    let attributes: Vec<AttributeAssignment> = attributes.into_serde().unwrap();
    rs_wasm_credential_verify(issuer_pub_key, attributes, commitment, pok)
        .c(d!())
        .map_err(error_to_jsvalue)
}

#[wasm_bindgen]
/// Returns information about traceable assets for a given transfer.
/// @param {JsValue} xfr_body - JSON of a transfer note from a transfer operation.
/// @param {AssetTracerKeyPair} tracer_keypair - Asset tracer keypair.
/// @param {JsValue} candidate_assets - List of asset types traced by the tracer keypair.
pub fn trace_assets(
    xfr_body: JsValue,
    tracer_keypair: &AssetTracerKeyPair,
    _candidate_assets: JsValue,
) -> Result<JsValue, JsValue> {
    let xfr_body = xfr_body.into_serde().c(d!()).map_err(error_to_jsvalue)?;
    let record_data = rs_trace_assets(xfr_body, tracer_keypair)
        .c(d!())
        .map_err(error_to_jsvalue)?;

    JsValue::from_serde(&record_data)
        .c(d!())
        .map_err(error_to_jsvalue)
}

#[wasm_bindgen]
/// Converts a bech32 encoded public key string to a public key.
pub fn public_key_from_bech32(addr: &str) -> Result<XfrPublicKey, JsValue> {
    rs_public_key_from_bech32(addr)
        .c(d!())
        .map_err(error_to_jsvalue)
}

#[wasm_bindgen]
pub fn bech32_to_base64(pk: &str) -> Result<String, JsValue> {
    rs_bech32_to_base64(pk).c(d!()).map_err(error_to_jsvalue)
}

#[wasm_bindgen]
pub fn base64_to_bech32(pk: &str) -> Result<String, JsValue> {
    rs_base64_to_bech32(pk).c(d!()).map_err(error_to_jsvalue)
}

/// Generate mnemonic with custom length and language.
/// - @param `wordslen`: acceptable value are one of [ 12, 15, 18, 21, 24 ]
/// - @param `lang`: acceptable value are one of [ "en", "zh", "zh_traditional", "fr", "it", "ko", "sp", "jp" ]
#[wasm_bindgen]
pub fn generate_mnemonic_custom(wordslen: u8, lang: &str) -> Result<String, JsValue> {
    rs_generate_mnemonic_custom(wordslen, lang)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Restore the XfrKeyPair from a mnemonic with a default bip44-path,
/// that is "m/44'/917'/0'/0/0" ("m/44'/coin'/account'/change/address").
#[wasm_bindgen]
pub fn restore_keypair_from_mnemonic_default(
    phrase: &str,
) -> Result<XfrKeyPair, JsValue> {
    rs_restore_keypair_from_mnemonic_default(phrase)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Restore the XfrKeyPair from a mnemonic with custom params,
/// in bip44 form.
#[wasm_bindgen]
pub fn restore_keypair_from_mnemonic_bip44(
    phrase: &str,
    lang: &str,
    path: &BipPath,
) -> Result<XfrKeyPair, JsValue> {
    rs_restore_keypair_from_mnemonic_bip44(phrase, lang, path)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Restore the XfrKeyPair from a mnemonic with custom params,
/// in bip49 form.
#[wasm_bindgen]
pub fn restore_keypair_from_mnemonic_bip49(
    phrase: &str,
    lang: &str,
    path: &BipPath,
) -> Result<XfrKeyPair, JsValue> {
    rs_restore_keypair_from_mnemonic_bip49(phrase, lang, path)
        .c(d!())
        .map_err(error_to_jsvalue)
}
