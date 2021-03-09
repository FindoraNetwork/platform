// Interface for issuing transactions that can be compiled to Wasm.
// Allows web clients to issue transactions from a browser contexts.
// For now, forwards transactions to a ledger hosted locally.
// To compile wasm package, run wasm-pack build in the wasm directory;
use crate::wasm_data_model::*;
use credentials::{
    credential_commit, credential_issuer_key_gen, credential_open_commitment,
    credential_reveal, credential_sign, credential_user_key_gen, credential_verify,
    credential_verify_commitment, CredIssuerPublicKey, CredIssuerSecretKey,
    CredUserPublicKey, CredUserSecretKey, Credential as PlatformCredential,
};
use cryptohash::sha256;
use ledger::data_model::{
    AssetTypeCode, AuthenticatedKVLookup, AuthenticatedTransaction, Operation,
    TransferType, TxOutput, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY, TX_FEE_MIN,
};
use ledger::policies::{DebtMemo, Fraction};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ruc::{d, err::RucResult};
use txn_builder::{
    BuildsTransactions, FeeInput as PlatformFeeInput, FeeInputs as PlatformFeeInputs,
    PolicyChoice, TransactionBuilder as PlatformTransactionBuilder,
    TransferOperationBuilder as PlatformTransferOperationBuilder,
};
use util::error_to_jsvalue;
use utils::HashOf;
use wasm_bindgen::prelude::*;

use zei::serialization::ZeiFromToBytes;
use zei::xfr::asset_record::{open_blind_asset_record as open_bar, AssetRecordType};
use zei::xfr::lib::trace_assets as zei_trace_assets;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey};
use zei::xfr::structs::{
    AssetRecordTemplate, AssetType as ZeiAssetType, XfrBody, ASSET_TYPE_LENGTH,
};

mod util;
mod wasm_data_model;

/// Constant defining the git commit hash and commit date of the commit this library was built
/// against.
const BUILD_ID: &str = concat!(env!("VERGEN_SHA_SHORT"), " ", env!("VERGEN_BUILD_DATE"));

/// Returns the git commit hash and commit date of the commit this library was built against.
#[wasm_bindgen]
pub fn build_id() -> String {
    BUILD_ID.to_string()
}

/////////// TRANSACTION BUILDING ////////////////

//Random Helpers

#[wasm_bindgen]
/// Generates random Base64 encoded asset type as a Base64 string. Used in asset definitions.
/// @see {@link
/// module:Findora-Wasm~TransactionBuilder#add_operation_create_asset|add_operation_create_asset}
/// for instructions on how to define an asset with a new
/// asset type
pub fn random_asset_type() -> String {
    AssetTypeCode::gen_random().to_base64()
}

#[wasm_bindgen]
/// Generates asset type as a Base64 string from a JSON-serialized JavaScript value.
pub fn asset_type_from_jsvalue(val: &JsValue) -> Result<String, JsValue> {
    let code: [u8; ASSET_TYPE_LENGTH] =
        val.into_serde().c(d!()).map_err(error_to_jsvalue)?;
    Ok(AssetTypeCode {
        val: ZeiAssetType(code),
    }
    .to_base64())
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
    let authenticated_txn =
        serde_json::from_str::<AuthenticatedTransaction>(&authenticated_txn)
            .c(d!())
            .map_err(|e| {
                JsValue::from_str(&format!("Could not deserialize transaction: {}", e))
            })?;
    let state_commitment = serde_json::from_str::<HashOf<_>>(&state_commitment)
        .c(d!())
        .map_err(|e| {
            JsValue::from_str(&format!("Could not deserialize state commitment: {}", e))
        })?;
    Ok(authenticated_txn.is_valid(state_commitment))
}

#[wasm_bindgen]
/// Given a serialized state commitment and an authenticated custom data result, returns true if the custom data result correctly
/// hashes up to the state commitment and false otherwise.
/// @param {string} state_commitment - String representing the state commitment.
/// @param {JsValue} authenticated_txn - JSON-encoded value representing the authenticated custom
/// data result.
/// @throws Will throw an error if the state commitment or the authenticated result fail to deserialize.
pub fn verify_authenticated_custom_data_result(
    state_commitment: String,
    authenticated_res: JsValue,
) -> Result<bool, JsValue> {
    let authenticated_res: AuthenticatedKVLookup =
        authenticated_res.into_serde().c(d!()).map_err(|e| {
            JsValue::from_str(&format!(
                "couldn't deserialize the authenticated custom data lookup: {}",
                e
            ))
        })?;
    let state_commitment = serde_json::from_str::<HashOf<_>>(&state_commitment)
        .c(d!())
        .map_err(|e| {
            JsValue::from_str(&format!("Could not deserialize state commitment: {}", e))
        })?;
    Ok(authenticated_res.is_valid(state_commitment))
}

#[wasm_bindgen]
/// Performs a simple loan repayment fee calculation.
///
/// The returned fee is a fraction of the `outstanding_balance`
/// where the interest rate is expressed as a fraction `ir_numerator` / `ir_denominator`.
///
/// This function is specific to the  Lending Demo.
/// @param {BigInt} ir_numerator - Interest rate numerator.
/// @param {BigInt} ir_denominator - Interest rate denominator.
/// @param {BigInt} outstanding_balance - Amount of outstanding debt.
/// @ignore
pub fn calculate_fee(
    ir_numerator: u64,
    ir_denominator: u64,
    outstanding_balance: u64,
) -> u64 {
    ledger::policies::calculate_fee(
        outstanding_balance,
        Fraction::new(ir_numerator, ir_denominator),
    )
}

#[wasm_bindgen]
// Testnet will not support direct API access to hardcoded debt policy.
/// Returns an address to use for cancelling debt tokens in a debt swap.
/// @ignore
pub fn get_null_pk() -> XfrPublicKey {
    XfrPublicKey::zei_from_bytes(&[0; 32]).unwrap()
}

#[wasm_bindgen]
// Testnet will not support Discret policies.
/// @ignore
pub fn create_default_policy_info() -> String {
    serde_json::to_string(&PolicyChoice::Fungible()).unwrap() // should never fail
}

#[wasm_bindgen]
/// Create policy information needed for debt token asset types.
/// This data will be parsed by the policy evalautor to ensure
/// that all payment and fee amounts are correct.
/// # Arguments
///
/// * `ir_numerator` - interest rate numerator
/// * `ir_denominator`- interest rate denominator
/// * `fiat_code` - Base64 string representing asset type used to pay off the loan
/// * `amount` - loan amount
/// @ignore
// Testnet will not support Discret policies.
pub fn create_debt_policy_info(
    ir_numerator: u64,
    ir_denominator: u64,
    fiat_code: String,
    loan_amount: u64,
) -> Result<String, JsValue> {
    let fiat_code = AssetTypeCode::new_from_base64(&fiat_code)
        .c(d!())
        .map_err(error_to_jsvalue)?;

    serde_json::to_string(&PolicyChoice::LoanToken(
        Fraction::new(ir_numerator, ir_denominator),
        fiat_code,
        loan_amount,
    ))
    .c(d!())
    .map_err(|e| JsValue::from_str(&format!("Could not serialize PolicyChoice: {}", e)))
}

#[wasm_bindgen]
/// Creates the memo needed for debt token asset types. The memo will be parsed by the policy evaluator to ensure
/// that all payment and fee amounts are correct.
/// @param {BigInt} ir_numerator  - Interest rate numerator.
/// @param {BigInt} ir_denominator - Interest rate denominator.
/// @param {string} fiat_code - Base64 string representing asset type used to pay off the loan.
/// @param {BigInt} loan_amount - Loan amount.
/// @throws Will throw an error if `fiat_code` fails to deserialize.
/// @ignore
// Testnet will not support Discret policies.
pub fn create_debt_memo(
    ir_numerator: u64,
    ir_denominator: u64,
    fiat_code: String,
    loan_amount: u64,
) -> Result<String, JsValue> {
    let fiat_code = AssetTypeCode::new_from_base64(&fiat_code)
        .c(d!())
        .map_err(error_to_jsvalue)?;
    let memo = DebtMemo {
        interest_rate: Fraction::new(ir_numerator, ir_denominator),
        fiat_code,
        loan_amount,
    };
    Ok(serde_json::to_string(&memo).unwrap())
}

#[wasm_bindgen]
/// Structure that allows users to construct arbitrary transactions.
pub struct TransactionBuilder {
    transaction_builder: PlatformTransactionBuilder,
}

impl TransactionBuilder {
    pub fn get_builder(&self) -> &PlatformTransactionBuilder {
        &self.transaction_builder
    }

    pub fn get_builder_mut(&mut self) -> &mut PlatformTransactionBuilder {
        &mut self.transaction_builder
    }
}

struct FeeInput {
    // Amount
    am: u64,
    // Index of txo
    tr: TxoRef,
    // Input body
    ar: ClientAssetRecord,
    // the owner_memo of `ar` for `Confidential` asset
    om: Option<OwnerMemo>,
    // Owner of this txo
    kp: XfrKeyPair,
}

impl From<FeeInput> for PlatformFeeInput {
    fn from(fi: FeeInput) -> Self {
        PlatformFeeInput {
            am: fi.am,
            tr: fi.tr.txo_ref,
            ar: fi.ar.txo,
            om: fi.om.map(|om| om.memo),
            kp: fi.kp,
        }
    }
}

#[wasm_bindgen]
#[derive(Default)]
pub struct FeeInputs {
    inner: Vec<FeeInput>,
}

impl From<FeeInputs> for PlatformFeeInputs {
    fn from(fi: FeeInputs) -> Self {
        PlatformFeeInputs {
            inner: fi.inner.into_iter().map(|i| i.into()).collect(),
        }
    }
}

#[wasm_bindgen]
impl FeeInputs {
    pub fn new() -> Self {
        FeeInputs {
            inner: Vec::with_capacity(1),
        }
    }

    pub fn append(
        &mut self,
        am: u64,
        tr: TxoRef,
        ar: ClientAssetRecord,
        om: Option<OwnerMemo>,
        kp: XfrKeyPair,
    ) {
        self.inner.push(FeeInput { am, tr, ar, om, kp })
    }

    pub fn append2(
        mut self,
        am: u64,
        tr: TxoRef,
        ar: ClientAssetRecord,
        om: Option<OwnerMemo>,
        kp: XfrKeyPair,
    ) -> Self {
        self.inner.push(FeeInput { am, tr, ar, om, kp });
        self
    }
}

#[wasm_bindgen]
impl TransactionBuilder {
    /// @param am: amount to pay
    /// @param kp: owner's XfrKeyPair
    pub fn add_fee_relative_auto(
        mut self,
        am: u64,
        kp: XfrKeyPair,
    ) -> Result<TransactionBuilder, JsValue> {
        self.transaction_builder
            .add_fee_relative_auto(am, &kp)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    /// Use this func to get the necessary infomations for generating `Relative Inputs`
    ///
    /// - TxoRef::Relative("Element index of the result")
    /// - ClientAssetRecord::from_json("Element of the result")
    pub fn get_relative_outputs(&self) -> Vec<JsValue> {
        self.transaction_builder
            .get_relative_outputs()
            .into_iter()
            .fold(vec![], |mut base, new| {
                base.push(
                    ClientAssetRecord {
                        txo: TxOutput {
                            record: new.0,
                            lien: None,
                        },
                    }
                    .to_json()
                    .unwrap(),
                );
                base
            })
    }

    /// As the last operation of any transaction,
    /// add a static fee to the transaction.
    pub fn add_fee(mut self, inputs: FeeInputs) -> Result<TransactionBuilder, JsValue> {
        self.transaction_builder
            .add_fee(inputs.into())
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    /// A simple fee checker for mainnet v1.0.
    ///
    /// SEE [check_fee](ledger::data_model::Transaction::check_fee)
    pub fn check_fee(&self) -> bool {
        self.transaction_builder.check_fee()
    }

    /// Create a new transaction builder.
    /// @param {BigInt} seq_id - Unique sequence ID to prevent replay attacks.
    pub fn new(seq_id: u64) -> Self {
        TransactionBuilder {
            transaction_builder: PlatformTransactionBuilder::from_seq_id(seq_id),
        }
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
        self.add_operation_create_asset_with_policy(
            key_pair,
            memo,
            token_code,
            create_default_policy_info(),
            asset_rules,
        )
    }

    /// @ignore
    // Testnet will not support Discret policies.
    pub fn add_operation_create_asset_with_policy(
        mut self,
        key_pair: &XfrKeyPair,
        memo: String,
        token_code: String,
        policy_choice: String,
        asset_rules: AssetRules,
    ) -> Result<TransactionBuilder, JsValue> {
        let asset_token = if token_code.is_empty() {
            AssetTypeCode::gen_random()
        } else {
            AssetTypeCode::new_from_base64(&token_code)
                .c(d!())
                .map_err(error_to_jsvalue)?
        };

        let policy_choice = serde_json::from_str::<PolicyChoice>(&policy_choice)
            .c(d!())
            .map_err(|e| {
                JsValue::from_str(&format!("Could not deserialize PolicyChoice: {}", e))
            })?;
        self.get_builder_mut()
            .add_operation_create_asset(
                &key_pair,
                Some(asset_token),
                asset_rules.rules,
                &memo,
                policy_choice,
            )
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    /// @ignore
    // Testnet will not support Discret policies.
    pub fn add_policy_option(
        mut self,
        token_code: String,
        which_check: String,
    ) -> Result<TransactionBuilder, JsValue> {
        let token_code = AssetTypeCode::new_from_base64(&token_code)
            .c(d!())
            .map_err(error_to_jsvalue)?;

        self.get_builder_mut()
            .add_policy_option(token_code, which_check);
        Ok(self)
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
    /// @param {PublicParams} zei_params - Public parameters necessary to generate asset records.
    pub fn add_basic_issue_asset(
        mut self,
        key_pair: &XfrKeyPair,
        code: String,
        seq_num: u64,
        amount: u64,
        conf_amount: bool,
        zei_params: &PublicParams,
    ) -> Result<TransactionBuilder, JsValue> {
        let asset_token = AssetTypeCode::new_from_base64(&code)
            .c(d!())
            .map_err(error_to_jsvalue)?;

        // TODO: (keyao/noah) enable client support for identity
        // tracing?
        // Redmine issue: #44
        let confidentiality_flags = AssetRecordType::from_flags(conf_amount, false);
        self.get_builder_mut()
            .add_basic_issue_asset(
                &key_pair,
                &asset_token,
                seq_num,
                amount,
                confidentiality_flags,
                zei_params.get_ref(),
            )
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    /// Adds an operation to the transaction builder that appends a credential commitment to the address
    /// identity registry.
    /// @param {XfrKeyPair} key_pair - Ledger key that is tied to the credential.
    /// @param {CredUserPublicKey} user_public_key - Public key of the credential user.
    /// @param {CredIssuerPublicKey} issuer_public_key - Public key of the credential issuer.
    /// @param {CredentialCommitment} commitment - Credential commitment to add to the address identity registry.
    /// @param {CredPoK} pok- Proof that the credential commitment is valid.
    /// @see {@link module:Findora-Wasm.wasm_credential_commit|wasm_credential_commit} for information about how to generate a credential
    /// commitment.
    pub fn add_operation_air_assign(
        mut self,
        key_pair: &XfrKeyPair,
        user_public_key: &CredUserPublicKey,
        issuer_public_key: &CredIssuerPublicKey,
        commitment: &CredentialCommitment,
        pok: &CredentialPoK,
    ) -> Result<TransactionBuilder, JsValue> {
        self.get_builder_mut()
            .add_operation_air_assign(
                key_pair,
                user_public_key.clone(),
                commitment.get_ref().clone(),
                issuer_public_key.clone(),
                pok.get_ref().clone(),
            )
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    /// Adds an operation to the transaction builder that removes a hash from ledger's custom data
    /// store.
    /// @param {XfrKeyPair} auth_key_pair - Key pair that is authorized to delete the hash at the
    /// provided key.
    /// @param {Key} key - The key of the custom data store whose value will be cleared if the
    /// transaction validates.
    /// @param {BigInt} seq_num - Nonce to prevent replays.
    pub fn add_operation_kv_update_no_hash(
        mut self,
        auth_key_pair: &XfrKeyPair,
        key: &Key,
        seq_num: u64,
    ) -> Result<TransactionBuilder, JsValue> {
        self.get_builder_mut()
            .add_operation_kv_update(auth_key_pair, key.get_ref(), seq_num, None)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    /// Adds an operation to the transaction builder that adds a hash to the ledger's custom data
    /// store.
    /// @param {XfrKeyPair} auth_key_pair - Key pair that is authorized to add the hash at the
    /// provided key.
    /// @param {Key} key - The key of the custom data store the value will be added to if the
    /// transaction validates.
    /// @param {KVHash} hash - The hash to add to the custom data store.
    /// @param {BigInt} seq_num - Nonce to prevent replays.
    pub fn add_operation_kv_update_with_hash(
        mut self,
        auth_key_pair: &XfrKeyPair,
        key: &Key,
        seq_num: u64,
        kv_hash: &KVHash,
    ) -> Result<TransactionBuilder, JsValue> {
        let hash = kv_hash.get_hash().clone();
        self.get_builder_mut()
            .add_operation_kv_update(auth_key_pair, key.get_ref(), seq_num, Some(&hash))
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
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
        mut self,
        auth_key_pair: &XfrKeyPair,
        code: String,
        new_memo: String,
    ) -> Result<TransactionBuilder, JsValue> {
        // First, decode the asset code
        let code = AssetTypeCode::new_from_base64(&code)
            .c(d!())
            .map_err(error_to_jsvalue)?;

        self.get_builder_mut()
            .add_operation_update_memo(auth_key_pair, code, &new_memo);
        Ok(self)
    }

    /// Adds a serialized transfer asset operation to a transaction builder instance.
    /// @param {string} op - a JSON-serialized transfer operation.
    /// @see {@link module:Findora-Wasm~TransferOperationBuilder} for details on constructing a transfer operation.
    /// @throws Will throw an error if `op` fails to deserialize.
    pub fn add_transfer_operation(
        mut self,
        op: String,
    ) -> Result<TransactionBuilder, JsValue> {
        let op = serde_json::from_str::<Operation>(&op)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        self.get_builder_mut().add_operation(op);
        Ok(self)
    }

    pub fn sign(mut self, kp: &XfrKeyPair) -> Result<TransactionBuilder, JsValue> {
        self.get_builder_mut().sign(kp);
        Ok(self)
    }

    /// Extracts the serialized form of a transaction.
    pub fn transaction(&self) -> String {
        self.get_builder().serialize_str()
    }

    /// Calculates transaction handle.
    pub fn transaction_handle(&self) -> String {
        self.get_builder().transaction().handle()
    }

    /// Fetches a client record from a transaction.
    /// @param {number} idx - Record to fetch. Records are added to the transaction builder sequentially.
    pub fn get_owner_record(&self, idx: usize) -> ClientAssetRecord {
        ClientAssetRecord {
            txo: self.get_builder().get_output_ref(idx),
        }
    }

    /// Fetches an owner memo from a transaction
    /// @param {number} idx - Owner memo to fetch. Owner memos are added to the transaction builder sequentially.
    pub fn get_owner_memo(&self, idx: usize) -> Option<OwnerMemo> {
        self.get_builder()
            .get_owner_memo_ref(idx)
            .map(|memo| OwnerMemo { memo: memo.clone() })
    }
}
#[wasm_bindgen]
#[derive(Default)]
/// Structure that enables clients to construct complex transfers.
pub struct TransferOperationBuilder {
    op_builder: PlatformTransferOperationBuilder,
}

impl TransferOperationBuilder {
    pub fn get_builder(&self) -> &PlatformTransferOperationBuilder {
        &self.op_builder
    }

    pub fn get_builder_mut(&mut self) -> &mut PlatformTransferOperationBuilder {
        &mut self.op_builder
    }
}

impl TransferOperationBuilder {
    pub fn add_input(
        mut self,
        txo_ref: TxoRef,
        asset_record: &ClientAssetRecord,
        owner_memo: Option<OwnerMemo>,
        tracing_policies: Option<&TracingPolicies>,
        key: &XfrKeyPair,
        amount: u64,
    ) -> Result<TransferOperationBuilder, JsValue> {
        let oar = open_bar(
            asset_record.get_bar_ref(),
            &owner_memo.map(|memo| memo.get_memo_ref().clone()),
            &key,
        )
        .c(d!())
        .map_err(|e| {
            JsValue::from_str(&format!("Could not open asset record: {}", e))
        })?;
        self.get_builder_mut()
            .add_input(
                *txo_ref.get_txo(),
                oar,
                tracing_policies.map(|policies| policies.get_policies_ref().clone()),
                None,
                amount,
            )
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    pub fn add_output(
        mut self,
        amount: u64,
        recipient: &XfrPublicKey,
        tracing_policies: Option<&TracingPolicies>,
        code: String,
        conf_amount: bool,
        conf_type: bool,
    ) -> Result<TransferOperationBuilder, JsValue> {
        let code = AssetTypeCode::new_from_base64(&code)
            .c(d!())
            .map_err(error_to_jsvalue)?;

        let asset_record_type = AssetRecordType::from_flags(conf_amount, conf_type);
        // TODO (noah/keyao) support identity tracing (issue #298)
        let template = if let Some(policies) = tracing_policies {
            AssetRecordTemplate::with_asset_tracing(
                amount,
                code.val,
                asset_record_type,
                *recipient,
                policies.get_policies_ref().clone(),
            )
        } else {
            AssetRecordTemplate::with_no_asset_tracing(
                amount,
                code.val,
                asset_record_type,
                *recipient,
            )
        };
        self.get_builder_mut()
            .add_output(
                &template,
                tracing_policies.map(|policies| policies.get_policies_ref().clone()),
                None,
                None,
            )
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }
}

#[wasm_bindgen]
impl TransferOperationBuilder {
    /// Create a new transfer operation builder.
    pub fn new() -> Self {
        Self::default()
    }

    // Debug function that does not need to go into the docs.
    /// @ignore
    pub fn debug(&self) -> String {
        serde_json::to_string(&self.op_builder).unwrap()
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
        self.add_input(
            txo_ref,
            &asset_record,
            owner_memo,
            Some(tracing_policies),
            key,
            amount,
        )
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
        self.add_input(txo_ref, asset_record, owner_memo, None, key, amount)
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
        self.add_output(
            amount,
            recipient,
            Some(tracing_policies),
            code,
            conf_amount,
            conf_type,
        )
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
        self.add_output(amount, recipient, None, code, conf_amount, conf_type)
    }

    /// Wraps around TransferOperationBuilder to ensure the transfer inputs and outputs are balanced.
    /// This function will add change outputs for all unspent portions of input records.
    /// @throws Will throw an error if the transaction cannot be balanced.
    pub fn balance(mut self) -> Result<TransferOperationBuilder, JsValue> {
        self.get_builder_mut()
            .balance()
            .c(d!())
            .map_err(|e| JsValue::from_str(&format!("Error balancing txn: {}", e)))?;
        Ok(self)
    }

    /// Wraps around TransferOperationBuilder to finalize the transaction.
    ///
    /// @throws Will throw an error if input and output amounts do not add up.
    /// @throws Will throw an error if not all record owners have signed the transaction.
    pub fn create(mut self) -> Result<TransferOperationBuilder, JsValue> {
        self.get_builder_mut()
            .create(TransferType::Standard)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    /// Wraps around TransferOperationBuilder to add a signature to the operation.
    ///
    /// All input owners must sign.
    ///
    /// @param {XfrKeyPair} kp - key pair of one of the input owners.
    pub fn sign(mut self, kp: &XfrKeyPair) -> Result<TransferOperationBuilder, JsValue> {
        self.get_builder_mut()
            .sign(&kp)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    /// Co-sign an input index
    /// @param {XfrKeyPair} kp - Co-signature key.
    /// @params {Number} input_idx - Input index to apply co-signature to.
    pub fn add_cosignature(
        mut self,
        kp: &XfrKeyPair,
        input_idx: usize,
    ) -> Result<TransferOperationBuilder, JsValue> {
        self.get_builder_mut()
            .sign_cosignature(kp, input_idx)
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(self)
    }

    pub fn builder(&self) -> String {
        serde_json::to_string(self.get_builder()).unwrap()
    }

    /// Wraps around TransferOperationBuilder to extract an operation expression as JSON.
    pub fn transaction(&self) -> Result<String, JsValue> {
        let op = self
            .get_builder()
            .transaction()
            .c(d!())
            .map_err(error_to_jsvalue)?;
        Ok(serde_json::to_string(&op).unwrap())
    }
}

///////////// CRYPTO //////////////////////
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
    open_bar(
        record.get_bar_ref(),
        &owner_memo.map(|memo| memo.get_memo_ref().clone()),
        &keypair,
    )
    .c(d!())
    .map_err(|e| JsValue::from_str(&format!("Could not open asset record: {}", e)))
    .and_then(|oa| JsValue::from_serde(&oa).c(d!()).map_err(error_to_jsvalue))
}

#[wasm_bindgen]
/// Extracts the public key as a string from a transfer key pair.
pub fn get_pub_key_str(key_pair: &XfrKeyPair) -> String {
    serde_json::to_string(key_pair.get_pk_ref()).unwrap()
}

#[wasm_bindgen]
/// Extracts the private key as a string from a transfer key pair.
pub fn get_priv_key_str(key_pair: &XfrKeyPair) -> String {
    serde_json::to_string(key_pair.get_sk_ref()).unwrap()
}

#[wasm_bindgen]
/// Creates a new transfer key pair.
pub fn new_keypair() -> XfrKeyPair {
    let mut small_rng = rand::thread_rng();
    XfrKeyPair::generate(&mut small_rng)
}

#[wasm_bindgen]
/// Generates a new keypair deterministically from a seed string and an optional name.
pub fn new_keypair_from_seed(seed_str: String, name: Option<String>) -> XfrKeyPair {
    let seed_str = seed_str + &name.unwrap_or_default();
    let hash = sha256::hash(&seed_str.as_bytes());
    let mut prng = ChaChaRng::from_seed(hash.0);
    XfrKeyPair::generate(&mut prng)
}

#[wasm_bindgen]
/// Returns base64 encoded representation of an XfrPublicKey.
pub fn public_key_to_base64(key: &XfrPublicKey) -> String {
    wallet::public_key_to_base64(key)
}

#[wasm_bindgen]
/// Converts a base64 encoded public key string to a public key.
pub fn public_key_from_base64(pk: &str) -> Result<XfrPublicKey, JsValue> {
    wallet::public_key_from_base64(pk)
        .c(d!())
        .map_err(error_to_jsvalue)
}

#[wasm_bindgen]
/// Expresses a transfer key pair as a hex-encoded string.
/// To decode the string, use `keypair_from_str` function.
pub fn keypair_to_str(key_pair: &XfrKeyPair) -> String {
    hex::encode(key_pair.zei_to_bytes())
}

#[wasm_bindgen]
/// Constructs a transfer key pair from a hex-encoded string.
/// The encode a key pair, use `keypair_to_str` function.
pub fn keypair_from_str(str: String) -> XfrKeyPair {
    XfrKeyPair::zei_from_bytes(&hex::decode(str).unwrap()).unwrap()
}

/// Generates a new credential issuer key.
/// @param {JsValue} attributes - Array of attribute types of the form `[{name: "credit_score",
/// size: 3}]`. The size refers to byte-size of the credential. In this case, the "credit_score"
/// attribute is represented as a 3 byte string "760". `attributes` is the list of attribute types
/// that the issuer can sign off on.
#[wasm_bindgen]
pub fn wasm_credential_issuer_key_gen(attributes: JsValue) -> CredentialIssuerKeyPair {
    let mut prng = ChaChaRng::from_entropy();
    let mut attributes: Vec<AttributeDefinition> = attributes.into_serde().unwrap();
    let attributes: Vec<(String, usize)> = attributes
        .drain(..)
        .map(|attr| (attr.name, attr.size))
        .collect();

    let (pk, sk) = credential_issuer_key_gen(&mut prng, &attributes[..]);
    CredentialIssuerKeyPair { pk, sk }
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
    credential_verify_commitment(
        issuer_pub_key,
        commitment.get_ref(),
        pok.get_ref(),
        xfr_pk.as_bytes(),
    )
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
    let mut prng = ChaChaRng::from_entropy();
    let reveal_fields: Vec<String> = reveal_fields.into_serde().c(d!()).map_err(|e| JsValue::from(&format!("Could not deserialize reveal fields. Please ensure that reveal fields are of the form [String]: {}", e)))?;
    let pok = credential_open_commitment(
        &mut prng,
        user_secret_key,
        credential.get_cred_ref(),
        key.get_ref(),
        &reveal_fields,
    )
    .c(d!())
    .map_err(error_to_jsvalue)?;
    Ok(CredentialPoK { pok })
}

/// Generates a new credential user key.
/// @param {CredIssuerPublicKey} issuer_pub_key - The credential issuer that can sign off on this
/// user's attributes.
#[wasm_bindgen]
pub fn wasm_credential_user_key_gen(
    issuer_pub_key: &CredIssuerPublicKey,
) -> CredentialUserKeyPair {
    let mut prng = ChaChaRng::from_entropy();
    let (pk, sk) = credential_user_key_gen(&mut prng, issuer_pub_key);
    CredentialUserKeyPair { pk, sk }
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
    let mut prng = ChaChaRng::from_entropy();
    let attributes: Vec<AttributeAssignment> = attributes.into_serde().c(d!()).map_err(|e| JsValue::from(&format!("Could not deserialize attributes. Please ensure that attribute definition is of the form [{{name: string, val: string}}]: {}", e)))?;
    let attributes: Vec<(String, &[u8])> = attributes
        .iter()
        .map(|attr| (attr.name.clone(), attr.val.as_bytes()))
        .collect();
    let sig =
        credential_sign(&mut prng, &issuer_secret_key, &user_public_key, &attributes)
            .c(d!())
            .map_err(error_to_jsvalue)?;
    Ok(CredentialSignature { sig })
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
    let attributes: Vec<(String, Vec<u8>)> = attributes
        .iter()
        .map(|attr| (attr.name.clone(), attr.val.as_bytes().to_vec()))
        .collect();
    Credential {
        credential: PlatformCredential {
            attributes,
            issuer_pub_key: issuer_public_key.clone(),
            signature: signature.get_sig_ref().clone(),
        },
    }
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
    let mut prng = ChaChaRng::from_entropy();
    let (commitment, pok, key) = credential_commit(
        &mut prng,
        &user_secret_key,
        credential.get_cred_ref(),
        &user_public_key.as_bytes(),
    )
    .c(d!())
    .map_err(error_to_jsvalue)?;
    Ok(CredentialCommitmentData {
        commitment: CredentialCommitment { commitment },
        pok: CredentialPoK { pok },
        commitment_key: CredentialCommitmentKey { key },
    })
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
    let mut prng = ChaChaRng::from_entropy();
    let reveal_fields: Vec<String> = reveal_fields.into_serde().unwrap();
    Ok(CredentialRevealSig {
        sig: credential_reveal(
            &mut prng,
            &user_sk,
            credential.get_cred_ref(),
            &reveal_fields[..],
        )
        .c(d!())
        .map_err(error_to_jsvalue)?,
    })
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
    let attributes: Vec<(String, &[u8])> = attributes
        .iter()
        .map(|attr| (attr.name.clone(), attr.val.as_bytes()))
        .collect();
    credential_verify(
        issuer_pub_key,
        &attributes,
        commitment.get_ref(),
        pok.get_ref(),
    )
    .c(d!())
    .map_err(error_to_jsvalue)?;
    Ok(())
}

// Asset Tracing

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
    // let candidate_assets: Vec<String> =
    //     candidate_assets.into_serde().c(d!()).map_err(error_to_jsvalue)?;
    let xfr_body: XfrBody = xfr_body.into_serde().c(d!()).map_err(error_to_jsvalue)?;
    // let candidate_assets: Vec<ZeiAssetType> = candidate_assets
    //     .iter()
    //     .map(|asset_type_str| {
    //         AssetTypeCode::new_from_str(&asset_type_str.to_string()).val
    //     })
    //     .collect();
    let record_data = zei_trace_assets(&xfr_body, tracer_keypair.get_keys())
        .c(d!())
        .map_err(error_to_jsvalue)?;
    let record_data: Vec<(u64, String)> = record_data
        .iter()
        .map(|(amt, asset_type, _, _)| {
            let asset_type_code = AssetTypeCode { val: *asset_type };
            (*amt, asset_type_code.to_base64())
        })
        .collect();

    JsValue::from_serde(&record_data)
        .c(d!())
        .map_err(|e| JsValue::from_str(&e.to_string()))
}

#[test]
pub fn test() {
    let kp = new_keypair();
    let b64 = public_key_to_base64(kp.get_pk_ref());
    let pk = public_key_from_base64(&b64).unwrap();
    dbg!(pk);
}

//////////////////////////////////////////
// Author: Chao Ma, github.com/chaosma. //
//////////////////////////////////////////

use aes_gcm::aead::{generic_array::GenericArray, Aead, NewAead};
use aes_gcm::Aes256Gcm;
use rand::{thread_rng, Rng};
use ring::pbkdf2;
use std::num::NonZeroU32;
use std::str;

#[wasm_bindgen]
/// Returns bech32 encoded representation of an XfrPublicKey.
pub fn public_key_to_bech32(key: &XfrPublicKey) -> String {
    wallet::public_key_to_bech32(key)
}

#[wasm_bindgen]
/// Converts a bech32 encoded public key string to a public key.
pub fn public_key_from_bech32(addr: &str) -> Result<XfrPublicKey, JsValue> {
    wallet::public_key_from_bech32(addr)
        .c(d!())
        .map_err(error_to_jsvalue)
}

#[wasm_bindgen]
pub fn bech32_to_base64(pk: &str) -> Result<String, JsValue> {
    let pub_key = public_key_from_bech32(pk)?;
    Ok(public_key_to_base64(&pub_key))
}

#[wasm_bindgen]
pub fn base64_to_bech32(pk: &str) -> Result<String, JsValue> {
    let pub_key = public_key_from_base64(pk)?;
    Ok(public_key_to_bech32(&pub_key))
}

#[wasm_bindgen]
pub fn encryption_pbkdf2_aes256gcm(key_pair: String, password: String) -> Vec<u8> {
    const CREDENTIAL_LEN: usize = 32;
    const IV_LEN: usize = 12;
    let n_iter = NonZeroU32::new(32).unwrap();
    let mut rng = thread_rng();

    let mut salt = [0u8; CREDENTIAL_LEN];
    rng.fill(&mut salt);
    let mut derived_key = [0u8; CREDENTIAL_LEN];
    pbkdf2::derive(
        pbkdf2::PBKDF2_HMAC_SHA512,
        n_iter,
        &salt,
        password.as_bytes(),
        &mut derived_key,
    );

    let mut iv = [0u8; IV_LEN];
    rng.fill(&mut iv);

    let cipher = Aes256Gcm::new(GenericArray::from_slice(&derived_key));
    let ciphertext = cipher
        .encrypt(GenericArray::from_slice(&iv), key_pair.as_ref())
        .unwrap_or_default();

    // this is a hack, wasm-bindgen not support tuple of vectors
    let mut res: Vec<u8> = Vec::new();
    res.append(&mut salt.to_vec());
    res.append(&mut iv.to_vec());
    res.append(&mut ciphertext.to_vec());
    res
}

#[wasm_bindgen]
pub fn decryption_pbkdf2_aes256gcm(enc_key_pair: Vec<u8>, password: String) -> String {
    const CREDENTIAL_LEN: usize = 32;
    const IV_LEN: usize = 12;
    let n_iter = NonZeroU32::new(32).unwrap();

    if enc_key_pair.len() <= CREDENTIAL_LEN + IV_LEN {
        return "".to_string();
    }

    let salt = &enc_key_pair[0..CREDENTIAL_LEN];
    let iv = &enc_key_pair[CREDENTIAL_LEN..(CREDENTIAL_LEN + IV_LEN)];
    let ciphertext = &enc_key_pair[(CREDENTIAL_LEN + IV_LEN)..];

    let mut derived_key = [0u8; CREDENTIAL_LEN];
    pbkdf2::derive(
        pbkdf2::PBKDF2_HMAC_SHA512,
        n_iter,
        salt,
        password.as_bytes(),
        &mut derived_key,
    );
    let cipher = Aes256Gcm::new(GenericArray::from_slice(&derived_key));
    let plaintext = cipher
        .decrypt(GenericArray::from_slice(iv), ciphertext.as_ref())
        .unwrap_or_default();

    String::from_utf8(plaintext).unwrap_or_else(|_| "".to_string())
}

#[wasm_bindgen]
pub fn create_keypair_from_secret(sk_str: String) -> Option<XfrKeyPair> {
    serde_json::from_str::<XfrSecretKey>(&sk_str)
        .map(|sk| sk.into_keypair())
        .ok()
}

#[wasm_bindgen]
pub fn get_pk_from_keypair(kp: &XfrKeyPair) -> XfrPublicKey {
    kp.get_pk()
}

///////////////////////////////////////////
// Author: FanHui(FH), github.com/ktmlm. //
///////////////////////////////////////////

/// Randomly generate a 12words-length mnemonic.
#[wasm_bindgen]
pub fn generate_mnemonic_default() -> String {
    wallet::generate_mnemonic_default()
}

/// Generate mnemonic with custom length and language.
/// - @param `wordslen`: acceptable value are one of [ 12, 15, 18, 21, 24 ]
/// - @param `lang`: acceptable value are one of [ "en", "zh", "zh_traditional", "fr", "it", "ko", "sp", "jp" ]
#[wasm_bindgen]
pub fn generate_mnemonic_custom(wordslen: u8, lang: &str) -> Result<String, JsValue> {
    wallet::generate_mnemonic_custom(wordslen, lang)
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// Use this struct to express a Bip44/Bip49 path.
#[wasm_bindgen]
pub struct BipPath {
    coin: u32,
    account: u32,
    change: u32,
    address: u32,
}

#[wasm_bindgen]
impl BipPath {
    pub fn new(coin: u32, account: u32, change: u32, address: u32) -> Self {
        BipPath {
            coin,
            account,
            change,
            address,
        }
    }
}

impl From<&BipPath> for wallet::BipPath {
    fn from(p: &BipPath) -> Self {
        wallet::BipPath::new(p.coin, p.account, p.change, p.address)
    }
}

/// Restore the XfrKeyPair from a mnemonic with a default bip44-path,
/// that is "m/44'/917'/0'/0/0" ("m/44'/coin'/account'/change/address").
#[wasm_bindgen]
pub fn restore_keypair_from_mnemonic_default(
    phrase: &str,
) -> Result<XfrKeyPair, JsValue> {
    wallet::restore_keypair_from_mnemonic_default(phrase)
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
    wallet::restore_keypair_from_mnemonic_bip44(phrase, lang, &path.into())
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
    wallet::restore_keypair_from_mnemonic_bip49(phrase, lang, &path.into())
        .c(d!())
        .map_err(error_to_jsvalue)
}

/// ID of FRA, in `String` format.
#[wasm_bindgen]
pub fn fra_get_asset_code() -> String {
    AssetTypeCode {
        val: ASSET_TYPE_FRA,
    }
    .to_base64()
}

/// Fee smaller than this value will be denied.
#[wasm_bindgen]
pub fn fra_get_minimal_fee() -> u64 {
    TX_FEE_MIN
}

/// The destination for fee to be transfered to.
#[wasm_bindgen]
pub fn fra_get_dest_pubkey() -> XfrPublicKey {
    *BLACK_HOLE_PUBKEY
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn t_keypair_conversion() {
        let kp = new_keypair();
        let b64 = public_key_to_base64(kp.get_pk_ref());
        let be32 = public_key_to_bech32(kp.get_pk_ref());
        public_key_from_base64(&b64).unwrap();
        public_key_from_bech32(&be32).unwrap();
    }

    #[test]
    fn t_keypair_encryption() {
        let key_pair = "hello world".to_string();
        let password = "12345".to_string();
        let enc = encryption_pbkdf2_aes256gcm(key_pair.clone(), password.clone());
        let dec_key_pair = decryption_pbkdf2_aes256gcm(enc, password);
        assert_eq!(key_pair, dec_key_pair);
    }

    #[test]
    fn t_create_keypair_from_secret() {
        let kp = new_keypair();
        let sk_str = serde_json::to_string(&kp.get_sk()).unwrap();
        let kp1 = create_keypair_from_secret(sk_str).unwrap();
        let kp_str = serde_json::to_string(&kp).unwrap();
        let kp1_str = serde_json::to_string(&kp1).unwrap();
        assert_eq!(kp_str, kp1_str);
    }
}
