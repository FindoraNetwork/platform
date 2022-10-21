#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

use super::data_model::*;
use finutils::txn_builder::{
    FeeInput as PlatformFeeInput, FeeInputs as PlatformFeeInputs,
    TransactionBuilder as PlatformTransactionBuilder,
    TransferOperationBuilder as PlatformTransferOperationBuilder,
};
use globutils::HashOf;
use ledger::{
    data_model::{
        gen_random_keypair, AssetTypeCode, AuthenticatedTransaction, Operation,
        TransferType, TxOutput,
    },
    staking::{td_addr_to_bytes, PartialUnDelegation, TendermintAddr},
};
use ruc::{eg, Result as RucResult};
use serde_json::Result;
use zei::xfr::asset_record::{open_blind_asset_record as open_bar, AssetRecordType};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::AssetRecordTemplate;

/// Given a serialized state commitment and transaction, returns true if the transaction correctly
/// hashes up to the state commitment and false otherwise.
pub fn rs_verify_authenticated_txn(
    state_commitment: String,
    authenticated_txn: String,
) -> Result<bool> {
    let authenticated_txn =
        serde_json::from_str::<AuthenticatedTransaction>(&authenticated_txn)?;
    let state_commitment = serde_json::from_str::<HashOf<_>>(&state_commitment)?;
    Ok(authenticated_txn.is_valid(state_commitment))
}

#[derive(Clone)]
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

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
#[derive(Clone, Default)]
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

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
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

#[derive(Clone)]
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

impl TransactionBuilder {
    /// @param am: amount to pay
    /// @param kp: owner's XfrKeyPair
    pub fn add_fee_relative_auto(
        mut self,
        kp: XfrKeyPair,
    ) -> RucResult<TransactionBuilder> {
        self.transaction_builder.add_fee_relative_auto(&kp)?;
        Ok(self)
    }

    /// Use this func to get the necessary infomations for generating `Relative Inputs`
    ///
    /// - TxoRef::Relative("Element index of the result")
    /// - ClientAssetRecord::from_json("Element of the result")
    pub fn get_relative_outputs(&self) -> Vec<ClientAssetRecord> {
        self.transaction_builder
            .get_relative_outputs()
            .into_iter()
            .fold(vec![], |mut base, new| {
                base.push(ClientAssetRecord {
                    txo: TxOutput {
                        id: None,
                        record: new.0,
                        lien: None,
                    },
                });
                base
            })
    }

    /// As the last operation of any transaction,
    /// add a static fee to the transaction.
    pub fn add_fee(mut self, inputs: FeeInputs) -> RucResult<TransactionBuilder> {
        self.transaction_builder.add_fee(inputs.into())?;
        Ok(self)
    }

    /// A simple fee checker for mainnet v1.0.
    ///
    /// SEE [check_fee](ledger::data_model::Transaction::check_fee)
    pub fn check_fee(&self) -> bool {
        self.transaction_builder.check_fee()
    }

    /// Create a new transaction builder.
    pub fn new(seq_id: u64) -> Self {
        TransactionBuilder {
            transaction_builder: PlatformTransactionBuilder::from_seq_id(seq_id),
        }
    }

    /// Wraps around TransactionBuilder to add an asset definition operation to a transaction builder instance.
    pub fn add_operation_create_asset(
        self,
        key_pair: &XfrKeyPair,
        memo: String,
        token_code: String,
        asset_rules: AssetRules,
    ) -> RucResult<TransactionBuilder> {
        self.add_operation_create_asset_with_policy(
            key_pair,
            memo,
            token_code,
            String::new(),
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
        _policy_choice: String,
        asset_rules: AssetRules,
    ) -> RucResult<TransactionBuilder> {
        let asset_token = if token_code.is_empty() {
            AssetTypeCode::gen_random()
        } else {
            AssetTypeCode::new_from_base64(&token_code)?
        };

        self.get_builder_mut().add_operation_create_asset(
            key_pair,
            Some(asset_token),
            asset_rules.rules,
            &memo,
        )?;
        Ok(self)
    }

    /// Wraps around TransactionBuilder to add an asset issuance to a transaction builder instance.
    ///
    /// Use this function for simple one-shot issuances.
    pub fn add_basic_issue_asset(
        mut self,
        key_pair: &XfrKeyPair,
        code: String,
        seq_num: u64,
        amount: u64,
        conf_amount: bool,
    ) -> RucResult<TransactionBuilder> {
        let asset_token = AssetTypeCode::new_from_base64(&code)?;

        // TODO: (keyao/noah) enable client support for identity
        // tracing?
        // Redmine issue: #44
        let confidentiality_flags = AssetRecordType::from_flags(conf_amount, false);
        self.get_builder_mut().add_basic_issue_asset(
            key_pair,
            &asset_token,
            seq_num,
            amount,
            confidentiality_flags,
        )?;
        Ok(self)
    }

    /// Adds an operation to the transaction builder that adds a hash to the ledger's custom data
    /// store.
    pub fn add_operation_update_memo(
        mut self,
        auth_key_pair: &XfrKeyPair,
        code: String,
        new_memo: String,
    ) -> RucResult<TransactionBuilder> {
        // First, decode the asset code
        let code = AssetTypeCode::new_from_base64(&code)?;

        self.get_builder_mut()
            .add_operation_update_memo(auth_key_pair, code, &new_memo);
        Ok(self)
    }

    #[allow(missing_docs)]
    pub fn add_operation_delegate(
        mut self,
        keypair: &XfrKeyPair,
        amount: u64,
        validator: TendermintAddr,
    ) -> RucResult<TransactionBuilder> {
        self.get_builder_mut()
            .add_operation_delegation(keypair, amount, validator);
        Ok(self)
    }

    #[allow(missing_docs)]
    pub fn add_operation_undelegate(
        mut self,
        keypair: &XfrKeyPair,
    ) -> RucResult<TransactionBuilder> {
        self.get_builder_mut()
            .add_operation_undelegation(keypair, None);
        Ok(self)
    }

    #[allow(missing_docs)]
    pub fn add_operation_undelegate_partially(
        mut self,
        keypair: &XfrKeyPair,
        am: u64,
        target_validator: TendermintAddr,
    ) -> RucResult<TransactionBuilder> {
        let middle_pk = gen_random_keypair().get_pk();
        self.get_builder_mut().add_operation_undelegation(
            keypair,
            Some(PartialUnDelegation::new(
                am,
                middle_pk,
                td_addr_to_bytes(&target_validator)?,
            )),
        );
        Ok(self)
    }

    #[allow(missing_docs)]
    pub fn add_operation_claim(
        mut self,
        keypair: &XfrKeyPair,
    ) -> RucResult<TransactionBuilder> {
        self.get_builder_mut().add_operation_claim(keypair, None);
        Ok(self)
    }

    #[allow(missing_docs)]
    pub fn add_operation_claim_custom(
        mut self,
        keypair: &XfrKeyPair,
        am: u64,
    ) -> RucResult<TransactionBuilder> {
        if 0 == am {
            return Err(eg!("Amount can not be zero"));
        }
        self.get_builder_mut()
            .add_operation_claim(keypair, Some(am));
        Ok(self)
    }

    /// Adds a serialized transfer asset operation to a transaction builder instance.
    pub fn add_transfer_operation(mut self, op: String) -> Result<TransactionBuilder> {
        let op = serde_json::from_str::<Operation>(&op)?;
        self.get_builder_mut().add_operation(op);
        Ok(self)
    }

    pub fn sign(mut self, kp: &XfrKeyPair) -> Result<TransactionBuilder> {
        self.get_builder_mut().sign_to_map(kp);
        Ok(self)
    }

    pub fn sign_origin(mut self, kp: &XfrKeyPair) -> Result<TransactionBuilder> {
        self.get_builder_mut().sign(kp);
        Ok(self)
    }

    /// Extracts the serialized form of a transaction.
    pub fn transaction(&mut self) -> String {
        self.get_builder_mut().build().unwrap();
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

#[derive(Clone, Default)]
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
    ) -> RucResult<TransferOperationBuilder> {
        let oar = open_bar(
            asset_record.get_bar_ref(),
            &owner_memo.map(|memo| memo.get_memo_ref().clone()),
            key,
        )?;
        self.get_builder_mut().add_input(
            *txo_ref.get_txo(),
            oar,
            tracing_policies.map(|policies| policies.get_policies_ref().clone()),
            None,
            amount,
        )?;
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
    ) -> RucResult<TransferOperationBuilder> {
        let code = AssetTypeCode::new_from_base64(&code)?;

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
        self.get_builder_mut().add_output(
            &template,
            tracing_policies.map(|policies| policies.get_policies_ref().clone()),
            None,
            None,
        )?;
        Ok(self)
    }
}

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
    pub fn add_input_with_tracing(
        self,
        txo_ref: TxoRef,
        asset_record: ClientAssetRecord,
        owner_memo: Option<OwnerMemo>,
        tracing_policies: &TracingPolicies,
        key: &XfrKeyPair,
        amount: u64,
    ) -> RucResult<TransferOperationBuilder> {
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
    pub fn add_input_no_tracing(
        self,
        txo_ref: TxoRef,
        asset_record: &ClientAssetRecord,
        owner_memo: Option<OwnerMemo>,
        key: &XfrKeyPair,
        amount: u64,
    ) -> RucResult<TransferOperationBuilder> {
        self.add_input(txo_ref, asset_record, owner_memo, None, key, amount)
    }

    /// Wraps around TransferOperationBuilder to add an output to a transfer operation builder.
    pub fn add_output_with_tracing(
        self,
        amount: u64,
        recipient: &XfrPublicKey,
        tracing_policies: &TracingPolicies,
        code: String,
        conf_amount: bool,
        conf_type: bool,
    ) -> RucResult<TransferOperationBuilder> {
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
    pub fn add_output_no_tracing(
        self,
        amount: u64,
        recipient: &XfrPublicKey,
        code: String,
        conf_amount: bool,
        conf_type: bool,
    ) -> RucResult<TransferOperationBuilder> {
        self.add_output(amount, recipient, None, code, conf_amount, conf_type)
    }

    /// Wraps around TransferOperationBuilder to ensure the transfer inputs and outputs are balanced.
    /// This function will add change outputs for all unspent portions of input records.
    /// @throws Will throw an error if the transaction cannot be balanced.
    pub fn balance(
        mut self,
        asset: Option<AssetRecordType>,
    ) -> RucResult<TransferOperationBuilder> {
        self.get_builder_mut().balance(asset)?;
        Ok(self)
    }

    /// Wraps around TransferOperationBuilder to finalize the transaction.
    pub fn create(mut self) -> RucResult<TransferOperationBuilder> {
        self.get_builder_mut().create(TransferType::Standard)?;
        Ok(self)
    }

    /// Wraps around TransferOperationBuilder to add a signature to the operation.
    ///
    /// All input owners must sign.
    pub fn sign(mut self, kp: &XfrKeyPair) -> RucResult<TransferOperationBuilder> {
        self.get_builder_mut().sign(kp)?;
        Ok(self)
    }

    pub fn builder(&self) -> String {
        serde_json::to_string(self.get_builder()).unwrap()
    }

    /// Wraps around TransferOperationBuilder to extract an operation expression as JSON.
    pub fn transaction(&self) -> RucResult<String> {
        let op = self.get_builder().transaction()?;
        Ok(serde_json::to_string(&op).unwrap())
    }
}
