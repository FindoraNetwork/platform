//!
//! utils for findora transaction
//!

#![deny(warnings)]
#![allow(clippy::needless_borrow)]

use {
    credentials::CredUserSecretKey,
    curve25519_dalek::scalar::Scalar,
    fp_types::{crypto::MultiSigner, H160},
    globutils::SignatureOf,
    ledger::{
        converter::ConvertAccount,
        data_model::{
            AssetRules, AssetTypeCode, ConfidentialMemo, DefineAsset, DefineAssetBody,
            IndexedSignature, IssueAsset, IssueAssetBody, IssuerKeyPair,
            IssuerPublicKey, Memo, NoReplayToken, Operation, Transaction,
            TransactionBody, TransferAsset, TransferAssetBody, TransferType, TxOutput,
            TxoRef, UpdateMemo, UpdateMemoBody, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY,
            TX_FEE_MIN,
        },
        staking::{
            is_valid_tendermint_addr,
            ops::{
                claim::ClaimOps,
                delegation::DelegationOps,
                fra_distribution::FraDistributionOps,
                governance::{ByzantineKind, GovernanceOps},
                replace_staker::ReplaceStakerOps,
                undelegation::UnDelegationOps,
                update_staker::UpdateStakerOps,
                update_validator::UpdateValidatorOps,
            },
            td_addr_to_string, BlockHeight, PartialUnDelegation, StakerMemo,
            TendermintAddr, Validator,
        },
    },
    rand_chacha::ChaChaRng,
    rand_core::{CryptoRng, RngCore, SeedableRng},
    ruc::*,
    serde::{Deserialize, Serialize},
    std::{
        cmp::Ordering,
        collections::{BTreeMap, HashSet},
    },
    tendermint::PrivateKey,
    zei::{
        api::anon_creds::{
            ac_confidential_open_commitment, ACCommitment, ACCommitmentKey,
            ConfidentialAC, Credential,
        },
        serialization::ZeiFromToBytes,
        setup::PublicParams,
        xfr::{
            asset_record::{
                build_blind_asset_record, build_open_asset_record,
                open_blind_asset_record, AssetRecordType,
            },
            lib::XfrNotePolicies,
            sig::{XfrKeyPair, XfrPublicKey},
            structs::{
                AssetRecord, AssetRecordTemplate, BlindAssetRecord, OpenAssetRecord,
                OwnerMemo, TracingPolicies, TracingPolicy,
            },
        },
    },
};

macro_rules! no_transfer_err {
    () => {
        ("Transaction has not yet been finalized".to_string())
    };
}

/// Definition of a fee operation, as a inner data structure of FeeInputs
pub struct FeeInput {
    /// Amount
    pub am: u64,
    /// Index of txo
    pub tr: TxoRef,
    /// Input body
    pub ar: TxOutput,
    /// responce to `ar`
    pub om: Option<OwnerMemo>,
    /// Owner of this txo
    pub kp: XfrKeyPair,
}

#[derive(Default)]
#[allow(missing_docs)]
pub struct FeeInputs {
    pub inner: Vec<FeeInput>,
}

impl FeeInputs {
    #[allow(missing_docs)]
    pub fn new() -> Self {
        FeeInputs::default()
    }

    #[allow(missing_docs)]
    pub fn append(
        &mut self,
        am: u64,
        tr: TxoRef,
        ar: TxOutput,
        om: Option<OwnerMemo>,
        kp: XfrKeyPair,
    ) {
        self.inner.push(FeeInput { am, tr, ar, om, kp })
    }
}

/// An simple builder for findora transaction
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransactionBuilder {
    txn: Transaction,
    outputs: u64,
    #[allow(missing_docs)]
    pub no_replay_token: NoReplayToken,
}

impl TransactionBuilder {
    /// Convert builder to it's inner transaction
    pub fn into_transaction(self) -> Transaction {
        self.txn
    }

    /// Get reference of it's inner transaction
    pub fn get_transaction(&self) -> &Transaction {
        &self.txn
    }

    /// Get the outputs of asset `transfer` and `issue` operation in transaction
    pub fn get_relative_outputs(&self) -> Vec<(BlindAssetRecord, Option<OwnerMemo>)> {
        // lien outputs can NOT be used as fee
        macro_rules! seek {
            ($d: expr) => {
                $d.body
                    .transfer
                    .outputs
                    .iter()
                    .zip($d.body.transfer.owners_memos.iter())
                    .map(|(r, om)| (r.clone(), om.clone()))
                    .collect()
            };
        }

        self.get_transaction()
            .body
            .operations
            .iter()
            .flat_map(|new| match new {
                Operation::TransferAsset(d) => {
                    seek!(d)
                }
                Operation::IssueAsset(d) => d
                    .body
                    .records
                    .iter()
                    .map(|(o, om)| (o.record.clone(), om.clone()))
                    .collect(),
                _ => Vec::new(),
            })
            .rev()
            .collect()
    }

    /// @param am: amount to pay
    /// @param kp: owner's XfrKeyPair
    pub fn add_fee_relative_auto(
        &mut self,
        kp: &XfrKeyPair,
    ) -> Result<&mut TransactionBuilder> {
        let mut opb = TransferOperationBuilder::default();
        let outputs = self.get_relative_outputs();

        let mut am = TX_FEE_MIN;
        for (idx, (o, om)) in outputs.into_iter().enumerate() {
            if 0 < am {
                if let Ok(oar) = open_blind_asset_record(&o, &om, &kp) {
                    if ASSET_TYPE_FRA == oar.asset_type
                        && kp.get_pk_ref().as_bytes() == o.public_key.as_bytes()
                    {
                        let n = alt!(oar.amount > am, am, oar.amount);
                        am = am.saturating_sub(oar.amount);
                        opb.add_input(TxoRef::Relative(idx as u64), oar, None, None, n)
                            .c(d!())?;
                    }
                }
            }
        }

        opb.add_output(
            &AssetRecordTemplate::with_no_asset_tracing(
                TX_FEE_MIN,
                ASSET_TYPE_FRA,
                AssetRecordType::from_flags(false, false),
                *BLACK_HOLE_PUBKEY,
            ),
            None,
            None,
            None,
        )
        .c(d!())
        .and_then(|o| o.balance(None).c(d!()))
        .and_then(|o| o.create(TransferType::Standard).c(d!()))
        .and_then(|o| o.sign(&kp).c(d!()))
        .and_then(|o| o.transaction().c(d!()))
        .map(move |op| self.add_operation(op))
    }

    /// As the last operation of any transaction,
    /// add a static fee to the transaction.
    pub fn add_fee(&mut self, inputs: FeeInputs) -> Result<&mut TransactionBuilder> {
        let mut kps = vec![];
        let mut opb = TransferOperationBuilder::default();

        for i in inputs.inner.into_iter() {
            open_blind_asset_record(&i.ar.record, &i.om, &i.kp)
                .c(d!())
                .and_then(|oar| {
                    opb.add_input(i.tr, oar, None, None, i.am)
                        .map(|_| {
                            kps.push(i.kp);
                        })
                        .c(d!())
                })?;
        }

        opb.add_output(
            &AssetRecordTemplate::with_no_asset_tracing(
                TX_FEE_MIN,
                ASSET_TYPE_FRA,
                AssetRecordType::from_flags(false, false),
                *BLACK_HOLE_PUBKEY,
            ),
            None,
            None,
            None,
        )
        .c(d!())
        .and_then(|o| o.balance(None).c(d!()))
        .and_then(|o| o.create(TransferType::Standard).c(d!()))
        .and_then(|o| {
            let cmp = |a: &XfrKeyPair, b: &XfrKeyPair| {
                a.get_pk().as_bytes().cmp(b.get_pk().as_bytes())
            };
            kps.sort_by(cmp);
            kps.dedup_by(|a, b| matches!(cmp(a, b), Ordering::Equal));
            for i in kps.iter() {
                o.sign(i).c(d!())?;
            }
            Ok(o)
        })
        .and_then(|o| o.transaction().c(d!()))
        .map(move |op| self.add_operation(op))
    }

    /// SEE [check_fee](ledger::data_model::Transaction::check_fee)
    #[inline(always)]
    pub fn check_fee(&self) -> bool {
        self.txn.check_fee()
    }

    #[allow(missing_docs)]
    pub fn get_owner_memo_ref(&self, idx: usize) -> Option<&OwnerMemo> {
        self.txn.get_owner_memos_ref()[idx]
    }

    #[allow(missing_docs)]
    pub fn get_output_ref(&self, idx: usize) -> TxOutput {
        self.txn.get_outputs_ref(true)[idx].clone()
    }

    /// Create a instance from seq_id
    pub fn from_seq_id(seq_id: u64) -> Self {
        let mut prng = ChaChaRng::from_entropy();
        let no_replay_token = NoReplayToken::new(&mut prng, seq_id);
        TransactionBuilder {
            txn: Transaction::from_seq_id(seq_id),
            outputs: 0,
            no_replay_token,
        }
    }

    #[allow(missing_docs)]
    pub fn get_seq_id(&self) -> u64 {
        self.no_replay_token.get_seq_id()
    }
}

impl TransactionBuilder {
    /// Add a basic asset issuing operation to builder and return modified builder
    pub fn add_basic_issue_asset(
        &mut self,
        key_pair: &XfrKeyPair,
        token_code: &AssetTypeCode,
        seq_num: u64,
        amount: u64,
        confidentiality_flags: AssetRecordType,
        zei_params: &PublicParams,
    ) -> Result<&mut Self> {
        let mut prng = ChaChaRng::from_entropy();
        let ar = AssetRecordTemplate::with_no_asset_tracing(
            amount,
            token_code.val,
            confidentiality_flags,
            key_pair.get_pk(),
        );

        let (ba, _, owner_memo) =
            build_blind_asset_record(&mut prng, &zei_params.pc_gens, &ar, vec![]);
        self.add_operation_issue_asset(
            key_pair,
            token_code,
            seq_num,
            &[(
                TxOutput {
                    id: None,
                    record: ba,
                    lien: None,
                },
                owner_memo,
            )],
        )
        .c(d!())
    }

    #[allow(missing_docs)]
    pub fn transaction(&self) -> &Transaction {
        &self.txn
    }

    #[allow(missing_docs)]
    pub fn take_transaction(self) -> Transaction {
        self.txn
    }

    /// Append a transaction memo
    pub fn add_memo(&mut self, memo: Memo) -> &mut Self {
        self.txn.body.memos.push(memo);
        self
    }

    /// Add asset creating operation to builder an return modified builder
    pub fn add_operation_create_asset(
        &mut self,
        key_pair: &XfrKeyPair,
        token_code: Option<AssetTypeCode>,
        asset_rules: AssetRules,
        memo: &str,
    ) -> Result<&mut Self> {
        let token_code = match token_code {
            Some(code) => code,
            None => AssetTypeCode::gen_random(),
        };
        let iss_keypair = IssuerKeyPair { keypair: &key_pair };
        self.txn.add_operation(Operation::DefineAsset(
            DefineAsset::new(
                DefineAssetBody::new(
                    &token_code,
                    &IssuerPublicKey {
                        key: *key_pair.get_pk_ref(),
                    },
                    asset_rules,
                    Some(Memo(memo.into())),
                    Some(ConfidentialMemo {}),
                )
                .c(d!())?,
                &iss_keypair,
            )
            .c(d!())?,
        ));

        Ok(self)
    }

    /// Add asset issuing operation to builder and return modified builder
    pub fn add_operation_issue_asset(
        &mut self,
        key_pair: &XfrKeyPair,
        token_code: &AssetTypeCode,
        seq_num: u64,
        records_and_memos: &[(TxOutput, Option<OwnerMemo>)],
    ) -> Result<&mut Self> {
        let iss_keypair = IssuerKeyPair { keypair: &key_pair };

        self.txn.add_operation(Operation::IssueAsset(
            IssueAsset::new(
                IssueAssetBody::new(token_code, seq_num, &records_and_memos).c(d!())?,
                &iss_keypair,
            )
            .c(d!())?,
        ));
        Ok(self)
    }

    /// Add asset transfer operation to builder and return modified builder
    #[allow(clippy::too_many_arguments)]
    pub fn add_operation_transfer_asset(
        &mut self,
        keys: &XfrKeyPair,
        input_sids: Vec<TxoRef>,
        input_records: &[OpenAssetRecord],
        input_tracing_policies: Vec<Option<TracingPolicy>>,
        _input_identity_commitments: Vec<Option<ACCommitment>>,
        output_records: &[AssetRecord],
        _output_identity_commitments: Vec<Option<ACCommitment>>,
    ) -> Result<&mut Self> {
        let mut prng = ChaChaRng::from_entropy();
        let mut input_asset_records = vec![];
        for (oar, tracing_policy) in
            input_records.iter().zip(input_tracing_policies.iter())
        {
            let mut policies = TracingPolicies::new();
            if let Some(policy) = tracing_policy {
                policies.add(policy.clone());
            }
            input_asset_records.push(
                AssetRecord::from_open_asset_record_with_asset_tracing_but_no_identity(
                    &mut prng,
                    oar.clone(),
                    policies,
                )
                .c(d!())?,
            );
        }

        let mut xfr = TransferAsset::new(
            TransferAssetBody::new(
                &mut prng,
                input_sids,
                &input_asset_records[..],
                output_records,
                None,
                vec![],
                TransferType::Standard,
            )
            .c(d!())?,
        )
        .c(d!())?;
        xfr.sign(&keys);

        self.txn.add_operation(Operation::TransferAsset(xfr));
        Ok(self)
    }

    /// Add a operation to updating asset memo
    pub fn add_operation_update_memo(
        &mut self,
        auth_key_pair: &XfrKeyPair,
        asset_code: AssetTypeCode,
        new_memo: &str,
    ) -> &mut Self {
        let new_memo = Memo(new_memo.into());
        let mut memo_update = UpdateMemo::new(
            UpdateMemoBody {
                new_memo,
                asset_type: asset_code,
                no_replay_token: self.txn.body.no_replay_token,
            },
            auth_key_pair,
        );
        memo_update.pubkey = auth_key_pair.get_pk();
        let op = Operation::UpdateMemo(memo_update);
        self.txn.add_operation(op);
        self
    }

    /// Add a operation to delegating findora account to a tendermint validator.
    /// The transfer operation to BLACK_HOLE_PUBKEY_STAKING should be sent along with.
    pub fn add_operation_delegation(
        &mut self,
        keypair: &XfrKeyPair,
        amount: u64,
        validator: TendermintAddr,
    ) -> &mut Self {
        let op = DelegationOps::new(
            keypair,
            None,
            amount,
            validator,
            None,
            self.txn.body.no_replay_token,
        );
        self.add_operation(Operation::Delegation(op))
    }

    /// Add a operation to updating staker memo and commission_rate
    pub fn add_operation_update_staker(
        &mut self,
        keypair: &XfrKeyPair,
        vltor_key: &PrivateKey,
        td_pubkey: Vec<u8>,
        commission_rate: [u64; 2],
        memo: StakerMemo,
    ) -> Result<&mut Self> {
        let v_id = keypair.get_pk();

        let v = Validator::new_staker(td_pubkey, v_id, commission_rate, memo).c(d!())?;
        let vaddr = td_addr_to_string(&v.td_addr);

        if !is_valid_tendermint_addr(&vaddr) {
            return Err(eg!("invalid pubkey, invalid address"));
        }

        let op = UpdateStakerOps::new(
            keypair,
            vltor_key,
            vaddr,
            v,
            self.txn.body.no_replay_token,
        );

        Ok(self.add_operation(Operation::UpdateStaker(op)))
    }

    /// Add a staking operation to add a tendermint node as a validator
    pub fn add_operation_staking(
        &mut self,
        keypair: &XfrKeyPair,
        amount: u64,
        vltor_key: &PrivateKey,
        td_pubkey: Vec<u8>,
        commission_rate: [u64; 2],
        memo: Option<String>,
    ) -> Result<&mut Self> {
        let v_id = keypair.get_pk();

        let memo = if memo.is_some() {
            serde_json::from_str(memo.unwrap().as_str()).c(d!())?
        } else {
            Default::default()
        };

        let v = Validator::new_staker(td_pubkey, v_id, commission_rate, memo).c(d!())?;
        let vaddr = td_addr_to_string(&v.td_addr);

        if !is_valid_tendermint_addr(&vaddr) {
            return Err(eg!("invalid pubkey, invalid address"));
        }

        let op = DelegationOps::new(
            keypair,
            Some(vltor_key),
            amount,
            vaddr,
            Some(v),
            self.txn.body.no_replay_token,
        );

        Ok(self.add_operation(Operation::Delegation(op)))
    }

    /// Add a operation to reduce delegation amount of a findora account.
    /// If no validator address and FRA amount provided, it will be a full un-delegation
    /// Otherwise, it will withdraw some FRA from the validator
    pub fn add_operation_undelegation(
        &mut self,
        keypair: &XfrKeyPair,
        pu: Option<PartialUnDelegation>,
    ) -> &mut Self {
        let op = UnDelegationOps::new(keypair, self.txn.body.no_replay_token, pu);
        self.add_operation(Operation::UnDelegation(Box::new(op)))
    }

    /// Add a operation to claim all the rewards
    pub fn add_operation_claim(
        &mut self,
        td_addr: Option<Vec<u8>>,
        keypair: &XfrKeyPair,
        am: Option<u64>,
    ) -> &mut Self {
        let op = ClaimOps::new(td_addr, keypair, am, self.txn.body.no_replay_token);
        self.add_operation(Operation::Claim(op))
    }

    #[allow(missing_docs)]
    pub fn add_operation_fra_distribution(
        &mut self,
        kps: &[&XfrKeyPair],
        alloc_table: BTreeMap<XfrPublicKey, u64>,
    ) -> Result<&mut Self> {
        FraDistributionOps::new(kps, alloc_table, self.txn.body.no_replay_token)
            .c(d!())
            .map(move |op| self.add_operation(Operation::FraDistribution(op)))
    }

    #[allow(missing_docs)]
    pub fn add_operation_governance(
        &mut self,
        kps: &[&XfrKeyPair],
        byzantine_id: XfrPublicKey,
        kind: ByzantineKind,
        custom_amount: Option<[u64; 2]>,
    ) -> Result<&mut Self> {
        GovernanceOps::new(
            kps,
            byzantine_id,
            kind,
            custom_amount,
            self.txn.body.no_replay_token,
        )
        .c(d!())
        .map(move |op| self.add_operation(Operation::Governance(op)))
    }

    /// Add a operation update the validator set at specified block height.
    pub fn add_operation_update_validator(
        &mut self,
        kps: &[&XfrKeyPair],
        h: BlockHeight,
        v_set: Vec<Validator>,
    ) -> Result<&mut Self> {
        UpdateValidatorOps::new(kps, h, v_set, self.txn.body.no_replay_token)
            .c(d!())
            .map(move |op| self.add_operation(Operation::UpdateValidator(op)))
    }

    /// Add an operation to replace the staker of validator.
    pub fn add_operation_replace_staker(
        &mut self,
        keypair: &XfrKeyPair,
        new_delegator: H160,
        new_delegator_pk: Option<Vec<u8>>,
        td_addr: Vec<u8>,
    ) -> Result<&mut Self> {
        let ops = ReplaceStakerOps::new(
            keypair,
            new_delegator,
            new_delegator_pk,
            td_addr,
            self.txn.body.no_replay_token,
        );
        self.add_operation(Operation::ReplaceStaker(ops));
        Ok(self)
    }

    /// Add a operation convert utxo asset to account balance.
    pub fn add_operation_convert_account(
        &mut self,
        kp: &XfrKeyPair,
        addr: MultiSigner,
        amount: u64,
        asset: Option<AssetTypeCode>,
        lowlevel_data: Option<Vec<u8>>,
    ) -> Result<&mut Self> {
        self.add_operation(Operation::ConvertAccount(ConvertAccount {
            signer: kp.get_pk(),
            nonce: self.txn.body.no_replay_token,
            receiver: addr,
            value: amount,
            asset_type: asset.map(|a| a.val),
            lowlevel_data,
        }));
        Ok(self)
    }

    #[allow(missing_docs)]
    pub fn add_operation(&mut self, op: Operation) -> &mut Self {
        self.txn.add_operation(op);
        self
    }

    /// Signing this transaction with XfrKeyPair, but insert `Transaction.signatures`
    pub fn sign(&mut self, kp: &XfrKeyPair) -> &mut Self {
        self.txn.sign(kp);
        self
    }

    /// Check and append signature to transaction
    pub fn add_signature(
        &mut self,
        pk: &XfrPublicKey,
        sig: SignatureOf<TransactionBody>,
    ) -> Result<&mut Self> {
        self.txn.check_signature(pk, &sig).c(d!())?;
        self.txn.signatures.push(sig);
        Ok(self)
    }

    /// Signing this transaction with XfrKeyPair, but insert to `Transaction.pubkey_sign_map`
    pub fn sign_to_map(&mut self, kp: &XfrKeyPair) -> &mut Self {
        self.txn.sign_to_map(kp);
        self
    }

    #[allow(missing_docs)]
    pub fn serialize(&self) -> Vec<u8> {
        // Unwrap is safe beacuse the underlying transaction is guaranteed to be serializable.
        let j = serde_json::to_string(&self.txn).unwrap();
        j.as_bytes().to_vec()
    }

    #[allow(missing_docs)]
    pub fn serialize_str(&self) -> String {
        // Unwrap is safe because the underlying transaction is guaranteed to be serializable.
        serde_json::to_string(&self.txn).unwrap()
    }
}

/// Generates an asset record from an asset record template using optional identity proof.
/// Returns the asset record, amount blinds, and type blind.
pub(crate) fn build_record_and_get_blinds<R: CryptoRng + RngCore>(
    prng: &mut R,
    template: &AssetRecordTemplate,
    identity_proof: Option<ConfidentialAC>,
) -> Result<(AssetRecord, (Scalar, Scalar), Scalar)> {
    // Check input consistency:
    // - if no policy, then no identity proof needed
    // - if policy and identity tracing, then identity proof is needed
    // - if policy but no identity tracing, then no identity proof is needed
    let asset_tracing = !template.asset_tracing_policies.is_empty();
    if !asset_tracing && identity_proof.is_some()
        || asset_tracing
            && (template
                .asset_tracing_policies
                .get_policy(0)
                .as_ref()
                .c(d!())?
                .identity_tracing
                .is_some()
                && identity_proof.is_none()
                || template
                    .asset_tracing_policies
                    .get_policy(0)
                    .as_ref()
                    .unwrap()
                    .identity_tracing
                    .is_none()
                    && identity_proof.is_some())
    {
        return Err(eg!());
    }
    // 1. get ciphertext and proofs from identity proof structure
    let (attr_ctext, reveal_proof) = match identity_proof {
        None => (None, None),
        Some(conf_ac) => {
            let (c, p) = conf_ac.get_fields();
            (Some(c.into_iter().map(|i| (0u32, i)).collect()), Some(p))
        }
    };
    // 2. Use record template and ciphertexts to build open asset record
    let params = PublicParams::default();
    let (open_asset_record, asset_tracing_memos, owner_memo) = build_open_asset_record(
        prng,
        &params.pc_gens,
        template,
        vec![attr_ctext.unwrap_or_default()],
    );
    // 3. Return record input containing open asset record, tracing policy, identity reveal proof,
    //    asset_tracer_memo, and owner_memo

    let mut identity_proofs = vec![];
    if reveal_proof.is_some() {
        identity_proofs.push(reveal_proof);
    }

    Ok((
        AssetRecord {
            open_asset_record: open_asset_record.clone(),
            tracing_policies: template.asset_tracing_policies.clone(),
            identity_proofs,
            owner_memo,
            asset_tracers_memos: asset_tracing_memos,
        },
        (
            open_asset_record.amount_blinds.0 .0,
            open_asset_record.amount_blinds.1 .0,
        ),
        open_asset_record.type_blind.0,
    ))
}

/// TransferOperationBuilder constructs transfer operations using the factory pattern
/// Inputs and outputs are added iteratively before being signed by all input record owners
#[derive(Clone, Serialize, Deserialize, Default)]
pub struct TransferOperationBuilder {
    input_sids: Vec<TxoRef>,
    spend_amounts: Vec<u64>, // Amount of each input record to spend, the rest will be refunded if user calls balance
    input_records: Vec<AssetRecord>,
    inputs_tracing_policies: Vec<TracingPolicies>,
    input_identity_commitments: Vec<Option<ACCommitment>>,
    output_records: Vec<AssetRecord>,
    outputs_tracing_policies: Vec<TracingPolicies>,
    output_identity_commitments: Vec<Option<ACCommitment>>,
    transfer: Option<TransferAsset>,
    transfer_type: TransferType,
    auto_refund: bool,
}

impl TransferOperationBuilder {
    #[allow(missing_docs)]
    pub fn new() -> Self {
        Self {
            auto_refund: true,
            ..Default::default()
        }
    }

    /// set auto_refund, will be checked when calling `create`
    pub fn auto_refund(&mut self, auto_refund: bool) -> &mut Self {
        self.auto_refund = auto_refund;
        self
    }

    /// TxoRef is the location of the input on the ledger and the amount is how much of the record
    /// should be spent in the transfer. See tests for example usage.
    pub fn add_input(
        &mut self,
        txo_sid: TxoRef,
        open_ar: OpenAssetRecord,
        tracing_policies: Option<TracingPolicies>,
        identity_commitment: Option<ACCommitment>,
        amount: u64,
    ) -> Result<&mut Self> {
        if self.transfer.is_some() {
            return Err(eg!(
                ("Cannot mutate a transfer that has been signed".to_string())
            ));
        }
        let policies = tracing_policies.unwrap_or_default();

        let asset_record =
            AssetRecord::from_open_asset_record_with_asset_tracing_but_no_identity(
                &mut ChaChaRng::from_entropy(),
                open_ar,
                policies.clone(),
            )
            .c(d!())?;
        self.input_sids.push(txo_sid);
        self.input_records.push(asset_record);
        self.inputs_tracing_policies.push(policies);
        self.input_identity_commitments.push(identity_commitment);
        self.spend_amounts.push(amount);
        Ok(self)
    }

    #[allow(missing_docs)]
    pub fn add_output(
        &mut self,
        asset_record_template: &AssetRecordTemplate,
        tracing_policies: Option<TracingPolicies>,
        identity_commitment: Option<ACCommitment>,
        credential_record: Option<(&CredUserSecretKey, &Credential, &ACCommitmentKey)>,
    ) -> Result<&mut Self> {
        let prng = &mut ChaChaRng::from_entropy();
        if self.transfer.is_some() {
            return Err(eg!(
                ("Cannot mutate a transfer that has been signed".to_string())
            ));
        }
        let policies = tracing_policies.unwrap_or_default();
        let ar = if let Some((user_secret_key, credential, commitment_key)) =
            credential_record
        {
            AssetRecord::from_template_with_identity_tracing(
                prng,
                asset_record_template,
                user_secret_key.get_ref(),
                credential,
                commitment_key,
            )
            .c(d!())?
        } else {
            AssetRecord::from_template_no_identity_tracing(prng, asset_record_template)
                .c(d!())?
        };
        self.output_records.push(ar);
        self.outputs_tracing_policies.push(policies);
        self.output_identity_commitments.push(identity_commitment);
        Ok(self)
    }

    /// Adds output to the records, and stores the asset amount blinds and type blind in the blinds parameter passed in.
    pub fn add_output_and_store_blinds<R: CryptoRng + RngCore>(
        &mut self,
        asset_record_template: &AssetRecordTemplate,
        credential_record: Option<(&CredUserSecretKey, &Credential, &ACCommitmentKey)>,
        prng: &mut R,
        blinds: &mut ((Scalar, Scalar), Scalar),
    ) -> Result<&mut Self> {
        if self.transfer.is_some() {
            return Err(eg!(
                ("Cannot mutate a transfer that has been signed".to_string())
            ));
        }
        let (ar, amount_blinds, type_blind) =
            if let Some((user_secret_key, credential, commitment_key)) =
                credential_record
            {
                match asset_record_template.asset_tracing_policies.get_policy(0) {
                    None => {
                        // identity tracing must have asset_tracing policy
                        return Err(eg!());
                    }
                    Some(policy) => {
                        match &policy.identity_tracing {
                            // policy must have a identity tracing policy
                            None => {
                                return Err(eg!());
                            }
                            Some(reveal_policy) => {
                                let conf_ac = ac_confidential_open_commitment(
                                    prng,
                                    user_secret_key.get_ref(),
                                    credential,
                                    commitment_key,
                                    &policy.enc_keys.attrs_enc_key,
                                    &reveal_policy.reveal_map,
                                    &[],
                                )
                                .c(d!())?;
                                build_record_and_get_blinds(
                                    prng,
                                    &asset_record_template,
                                    Some(conf_ac),
                                )
                                .c(d!())?
                            }
                        }
                    }
                }
            } else {
                if let Some(policy) =
                    asset_record_template.asset_tracing_policies.get_policy(0)
                {
                    if policy.identity_tracing.is_some() {
                        return Err(eg!());
                    }
                }
                build_record_and_get_blinds(prng, &asset_record_template, None)?
            };
        blinds.0 = amount_blinds;
        blinds.1 = type_blind;
        self.output_records.push(ar);
        self.outputs_tracing_policies
            .push(asset_record_template.asset_tracing_policies.clone());
        self.output_identity_commitments.push(None);
        Ok(self)
    }

    // Check if outputs and inputs are balanced

    fn check_balance(&self) -> Result<()> {
        let input_total: u64 = self
            .input_records
            .iter()
            .fold(0, |acc, ar| acc + ar.open_asset_record.amount);
        let output_total = self
            .output_records
            .iter()
            .fold(0, |acc, ar| acc + ar.open_asset_record.amount);
        if input_total != output_total {
            return Err(eg!(format!("{input_total} != {output_total}")));
        }

        Ok(())
    }

    /// Ensures that outputs and inputs are balanced by adding remainder outputs for leftover asset
    /// amounts
    pub fn balance(&mut self, rt: Option<AssetRecordType>) -> Result<&mut Self> {
        let mut prng = ChaChaRng::from_entropy();
        if self.transfer.is_some() {
            return Err(eg!(
                ("Cannot mutate a transfer that has been signed".to_string())
            ));
        }

        // for: repeated/idempotent balance
        let mut amt_cache = vec![];

        let spend_total: u64 = self.spend_amounts.iter().sum();
        let mut partially_consumed_inputs = Vec::new();

        for (idx, ((spend_amount, ar), policies)) in self
            .spend_amounts
            .iter()
            .zip(self.input_records.iter())
            .zip(self.inputs_tracing_policies.iter())
            .enumerate()
        {
            let amt = ar.open_asset_record.get_amount();
            match spend_amount.cmp(amt) {
                Ordering::Greater => {
                    return Err(eg!());
                }
                Ordering::Less => {
                    let asset_type = *ar.open_asset_record.get_asset_type();
                    let record_type =
                        rt.unwrap_or_else(|| ar.open_asset_record.get_record_type());
                    let recipient = *ar.open_asset_record.get_pub_key();
                    let ar_template = AssetRecordTemplate::with_asset_tracing(
                        amt - spend_amount,
                        asset_type,
                        record_type,
                        recipient,
                        policies.clone(),
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        &mut prng,
                        &ar_template,
                    )
                    .c(d!())?;
                    partially_consumed_inputs.push(ar);
                    self.outputs_tracing_policies.push(policies.clone());
                    self.output_identity_commitments.push(None);

                    // for: repeated/idempotent balance
                    amt_cache.push((idx, *amt));
                }
                _ => {}
            }
        }

        let output_total = self
            .output_records
            .iter()
            .fold(0, |acc, ar| acc + ar.open_asset_record.amount);
        if spend_total != output_total {
            return Err(eg!(format!("{spend_total} != {output_total}")));
        }
        self.output_records.append(&mut partially_consumed_inputs);

        // for: repeated/idempotent balance
        amt_cache.into_iter().for_each(|(idx, am)| {
            self.spend_amounts[idx] = am;
        });

        Ok(self)
    }

    /// Finalize the transaction and prepare for signing. Once called, the transaction cannot be
    /// modified.
    pub fn create(&mut self, transfer_type: TransferType) -> Result<&mut Self> {
        if self.auto_refund {
            self.balance(None).c(d!())?;
        } else {
            self.check_balance().c(d!())?;
        }

        let mut prng = ChaChaRng::from_entropy();
        let num_inputs = self.input_records.len();
        let num_outputs = self.output_records.len();
        let xfr_policies = XfrNotePolicies::new(
            self.inputs_tracing_policies.clone(),
            vec![None; num_inputs],
            self.outputs_tracing_policies.clone(),
            vec![None; num_outputs],
        );
        let body = TransferAssetBody::new(
            &mut prng,
            self.input_sids.clone(),
            &self.input_records,
            &self.output_records,
            Some(xfr_policies),
            vec![],
            transfer_type,
        )
        .c(d!())?;
        self.transfer = Some(TransferAsset::new(body).c(d!())?);
        Ok(self)
    }

    #[allow(missing_docs)]
    pub fn get_output_record(&self, idx: usize) -> Option<BlindAssetRecord> {
        self.transfer
            .as_ref()?
            .body
            .transfer
            .outputs
            .get(idx)
            .cloned()
    }

    /// All input owners must sign eventually for the transaction to be valid.
    pub fn sign(&mut self, kp: &XfrKeyPair) -> Result<&mut Self> {
        if self.transfer.is_none() {
            return Err(eg!(no_transfer_err!()));
        }
        self.transfer.as_mut().c(d!())?.sign(&kp);
        Ok(self)
    }

    #[allow(missing_docs)]
    pub fn create_input_signature(
        &self,
        keypair: &XfrKeyPair,
    ) -> Result<IndexedSignature<TransferAssetBody>> {
        let sig = self
            .transfer
            .as_ref()
            .c(d!(no_transfer_err!()))?
            .create_input_signature(keypair);
        Ok(sig)
    }

    #[allow(missing_docs)]
    pub fn attach_signature(
        &mut self,
        sig: IndexedSignature<TransferAssetBody>,
    ) -> Result<&mut Self> {
        self.transfer
            .as_mut()
            .c(d!(no_transfer_err!()))?
            .attach_signature(sig)
            .c(d!())?;
        Ok(self)
    }

    /// Return the transaction operation
    pub fn transaction(&self) -> Result<Operation> {
        if self.transfer.is_none() {
            return Err(eg!(no_transfer_err!()));
        }
        Ok(Operation::TransferAsset(self.transfer.clone().c(d!())?))
    }

    /// Checks to see whether all necessary signatures are present and valid
    pub fn validate_signatures(&mut self) -> Result<&mut Self> {
        if self.transfer.is_none() {
            return Err(eg!(no_transfer_err!()));
        }

        let trn = self.transfer.as_ref().c(d!())?;
        let mut sig_keys = HashSet::new();
        for sig in &trn.body_signatures {
            if !sig.verify(&trn.body) {
                return Err(eg!(("Invalid signature")));
            }
            sig_keys.insert(sig.address.key.zei_to_bytes());
        }

        for record in &trn.body.transfer.inputs {
            if !sig_keys.contains(&record.public_key.zei_to_bytes()) {
                return Err(eg!(("Not all signatures present")));
            }
        }
        Ok(self)
    }
}

#[cfg(test)]
#[allow(missing_docs)]
mod tests {
    use {
        super::*,
        ledger::data_model::{TxnEffect, TxoRef},
        ledger::store::{utils::fra_gen_initial_tx, LedgerState},
        rand_chacha::ChaChaRng,
        rand_core::SeedableRng,
        zei::setup::PublicParams,
        zei::xfr::asset_record::AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record},
        zei::xfr::sig::XfrKeyPair,
    };

    // Defines an asset type
    #[derive(Clone, Debug, Eq, PartialEq)]
    struct AssetType(pub u8);

    #[derive(Clone, Debug, Eq, PartialEq)]
    struct KeyPair(pub u8);

    #[derive(Clone, Debug, Eq, PartialEq)]
    struct TxoReference(pub u64);

    // Defines an input record
    // (type, amount, conf_type, conf_amount, traceable)
    #[derive(Clone, Debug, Eq, PartialEq)]
    struct InputRecord(pub u64, pub AssetType, pub bool, pub bool, pub bool);

    // Defines an output record
    // (amount, asset type, keypair)
    #[derive(Clone, Debug, Eq, PartialEq)]
    struct OutputRecord(pub u64, pub AssetType, pub KeyPair);

    #[test]
    fn test_transfer_op_builder() {
        pnk!(test_transfer_op_builder_inner());
    }

    fn test_transfer_op_builder_inner() -> Result<()> {
        let mut prng = ChaChaRng::from_entropy();
        let params = PublicParams::default();
        let code_1 = AssetTypeCode::gen_random();
        let code_2 = AssetTypeCode::gen_random();
        let alice = XfrKeyPair::generate(&mut prng);
        let bob = XfrKeyPair::generate(&mut prng);
        let charlie = XfrKeyPair::generate(&mut prng);
        let ben = XfrKeyPair::generate(&mut prng);

        let ar_1 = AssetRecordTemplate::with_no_asset_tracing(
            1000,
            code_1.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            alice.get_pk(),
        );
        let ar_2 = AssetRecordTemplate::with_no_asset_tracing(
            1000,
            code_2.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            bob.get_pk(),
        );
        let (ba_1, _, memo1) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &ar_1, vec![]);
        let (ba_2, _, memo2) =
            build_blind_asset_record(&mut prng, &params.pc_gens, &ar_2, vec![]);

        // Attempt to spend too much
        let mut invalid_outputs_transfer_op = TransferOperationBuilder::new();
        let output_template = AssetRecordTemplate::with_no_asset_tracing(
            25,
            code_1.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            bob.get_pk(),
        );
        let res = invalid_outputs_transfer_op
            .add_input(
                TxoRef::Relative(1),
                open_blind_asset_record(&ba_1, &memo1, &alice).c(d!())?,
                None,
                None,
                20,
            )
            .c(d!())?
            .add_output(&output_template, None, None, None)
            .c(d!())?
            .balance(None);

        assert!(res.is_err());

        // Change transaction after signing
        let mut invalid_sig_op = TransferOperationBuilder::new();
        let output_template = AssetRecordTemplate::with_no_asset_tracing(
            20,
            code_1.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            bob.get_pk(),
        );
        let res = invalid_sig_op
            .add_input(
                TxoRef::Relative(1),
                open_blind_asset_record(&ba_1, &memo1, &alice).c(d!())?,
                None,
                None,
                20,
            )
            .c(d!())?
            .add_output(&output_template, None, None, None)
            .c(d!())?
            .balance(None)
            .c(d!())?
            .create(TransferType::Standard)
            .c(d!())?
            .sign(&alice)
            .c(d!())?
            .add_output(&output_template, None, None, None);
        assert!(res.is_err());

        // Not all signatures present
        let mut missing_sig_op = TransferOperationBuilder::new();
        let output_template = AssetRecordTemplate::with_no_asset_tracing(
            20,
            code_1.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            bob.get_pk(),
        );
        let res = missing_sig_op
            .add_input(
                TxoRef::Relative(1),
                open_blind_asset_record(&ba_1, &memo1, &alice).c(d!())?,
                None,
                None,
                20,
            )
            .c(d!())?
            .add_output(&output_template, None, None, None)
            .c(d!())?
            .balance(None)
            .c(d!())?
            .create(TransferType::Standard)
            .c(d!())?
            .validate_signatures();

        assert!(&res.is_err());

        // Finally, test a valid transfer
        let output_bob5_code1_template = AssetRecordTemplate::with_no_asset_tracing(
            5,
            code_1.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            bob.get_pk(),
        );
        let output_charlie13_code1_template = AssetRecordTemplate::with_no_asset_tracing(
            13,
            code_1.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            charlie.get_pk(),
        );
        let output_ben2_code1_template = AssetRecordTemplate::with_no_asset_tracing(
            2,
            code_1.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            ben.get_pk(),
        );
        let output_bob5_code2_template = AssetRecordTemplate::with_no_asset_tracing(
            5,
            code_2.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            bob.get_pk(),
        );
        let output_charlie13_code2_template = AssetRecordTemplate::with_no_asset_tracing(
            13,
            code_2.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            charlie.get_pk(),
        );
        let output_ben2_code2_template = AssetRecordTemplate::with_no_asset_tracing(
            2,
            code_2.val,
            NonConfidentialAmount_NonConfidentialAssetType,
            ben.get_pk(),
        );
        let _valid_transfer_op = TransferOperationBuilder::new()
            .add_input(
                TxoRef::Relative(1),
                open_blind_asset_record(&ba_1, &memo1, &alice).c(d!())?,
                None,
                None,
                20,
            )
            .c(d!())?
            .add_input(
                TxoRef::Relative(2),
                open_blind_asset_record(&ba_2, &memo2, &bob).c(d!())?,
                None,
                None,
                20,
            )
            .c(d!())?
            .add_output(&output_bob5_code1_template, None, None, None)
            .c(d!())?
            .add_output(&output_charlie13_code1_template, None, None, None)
            .c(d!())?
            .add_output(&output_ben2_code1_template, None, None, None)
            .c(d!())?
            .add_output(&output_bob5_code2_template, None, None, None)
            .c(d!())?
            .add_output(&output_charlie13_code2_template, None, None, None)
            .c(d!())?
            .add_output(&output_ben2_code2_template, None, None, None)
            .c(d!())?
            .balance(None)
            .c(d!())?
            .create(TransferType::Standard)
            .c(d!())?
            .sign(&alice)
            .c(d!())?
            .sign(&bob)
            .c(d!())?
            .transaction()
            .c(d!())?;
        Ok(())
    }

    #[test]
    fn test_check_fee_with_ledger() {
        let mut ledger = LedgerState::tmp_ledger();
        let fra_owner_kp = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
        let bob_kp = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
        assert_eq!(
            bob_kp.get_sk().into_keypair().zei_to_bytes(),
            bob_kp.zei_to_bytes()
        );

        let mut tx = fra_gen_initial_tx(&fra_owner_kp);
        assert!(tx.check_fee());

        let effect = TxnEffect::compute_effect(tx.clone()).unwrap();
        let mut block = ledger.start_block().unwrap();
        let tmp_sid = ledger.apply_transaction(&mut block, effect).unwrap();
        let txo_sid = ledger
            .finish_block(block)
            .unwrap()
            .remove(&tmp_sid)
            .unwrap()
            .1[0];

        macro_rules! transfer_to_bob {
            ($txo_sid: expr, $bob_pk: expr) => {{
                let output_bob_fra_template = AssetRecordTemplate::with_no_asset_tracing(
                    100 * TX_FEE_MIN,
                    ASSET_TYPE_FRA,
                    NonConfidentialAmount_NonConfidentialAssetType,
                    $bob_pk,
                );
                TransferOperationBuilder::new()
                    .add_input(
                        TxoRef::Absolute($txo_sid),
                        open_blind_asset_record(
                            &ledger.get_utxo_light($txo_sid).unwrap().utxo.0.record,
                            &None,
                            &fra_owner_kp,
                        )
                        .unwrap(),
                        None,
                        None,
                        100 * TX_FEE_MIN,
                    )
                    .unwrap()
                    .add_output(&output_bob_fra_template, None, None, None)
                    .unwrap()
                    .balance(None)
                    .unwrap()
                    .create(TransferType::Standard)
                    .unwrap()
                    .sign(&fra_owner_kp)
                    .unwrap()
                    .transaction()
                    .unwrap()
            }};
        }

        let mut tx2 = TransactionBuilder::from_seq_id(1);
        tx2.add_operation(transfer_to_bob!(txo_sid, bob_kp.get_pk()))
            .add_fee_relative_auto(&fra_owner_kp)
            .unwrap();
        assert!(tx2.check_fee());

        let effect = TxnEffect::compute_effect(tx2.into_transaction()).unwrap();
        let mut block = ledger.start_block().unwrap();
        let tmp_sid = ledger.apply_transaction(&mut block, effect).unwrap();
        // txo_sid[0]: fra_owner to bob
        // txo_sid[1]: fra_owner to fee
        // txo_sid[2]: balance to fra_owner
        let txo_sid = ledger
            .finish_block(block)
            .unwrap()
            .remove(&tmp_sid)
            .unwrap()
            .1;

        // (0) transfer first time
        let mut fi = FeeInputs::new();
        let utxo = ledger.get_utxo_light(txo_sid[0]).unwrap();
        fi.append(
            TX_FEE_MIN,
            TxoRef::Absolute(txo_sid[0]),
            utxo.utxo.0,
            utxo.txn.txn.get_owner_memos_ref()[utxo.utxo_location.0].cloned(),
            bob_kp.get_sk().into_keypair(),
        );
        let mut tx3 = TransactionBuilder::from_seq_id(2);
        pnk!(tx3
            .add_operation(transfer_to_bob!(txo_sid[2], bob_kp.get_pk()))
            .add_fee(fi));
        assert!(tx3.check_fee());

        let effect = TxnEffect::compute_effect(tx3.into_transaction()).unwrap();
        let mut block = ledger.start_block().unwrap();
        let tmp_sid = ledger.apply_transaction(&mut block, effect).unwrap();
        // txo_sid[0]: fra_owner to bob
        // txo_sid[1]: balance to fra_owner
        // txo_sid[2]: bob to fee
        // txo_sid[3]: balance to bob
        let txo_sid = ledger
            .finish_block(block)
            .unwrap()
            .remove(&tmp_sid)
            .unwrap()
            .1;

        // (2) transfer second time
        let mut fi = FeeInputs::new();
        let utxo = ledger.get_utxo_light(txo_sid[0]).unwrap();
        fi.append(
            TX_FEE_MIN,
            TxoRef::Absolute(txo_sid[0]),
            utxo.utxo.0,
            utxo.txn.txn.get_owner_memos_ref()[utxo.utxo_location.0].cloned(),
            bob_kp.get_sk().into_keypair(),
        );
        let mut tx4 = TransactionBuilder::from_seq_id(3);
        tx4.add_operation(transfer_to_bob!(txo_sid[1], bob_kp.get_pk()))
            .add_fee(fi)
            .unwrap();
        assert!(tx4.check_fee());

        let effect = TxnEffect::compute_effect(tx4.into_transaction()).unwrap();
        let mut block = ledger.start_block().unwrap();
        ledger.apply_transaction(&mut block, effect).unwrap();
        ledger.finish_block(block).unwrap();

        // Ensure that FRA can only be defined only once.
        tx.body.no_replay_token =
            NoReplayToken::new(&mut ChaChaRng::from_entropy(), 100);
        let effect = TxnEffect::compute_effect(tx).unwrap();
        let mut block = ledger.start_block().unwrap();
        assert!(ledger.apply_transaction(&mut block, effect).is_err());
    }
}
