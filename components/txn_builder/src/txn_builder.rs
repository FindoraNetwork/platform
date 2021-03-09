extern crate ledger;
extern crate serde;
extern crate zei;
#[macro_use]
extern crate serde_derive;

use credentials::{
    CredCommitment, CredIssuerPublicKey, CredPoK, CredUserPublicKey, CredUserSecretKey,
};
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::*;
use ledger::inv_fail;
use ledger::policies::Fraction;
use ledger::policy_script::{Policy, PolicyGlobals, TxnCheckInputs, TxnPolicyData};
use rand_chacha::ChaChaRng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use ruc::*;
use sparse_merkle_tree::Key;
use std::cmp::Ordering;
use std::collections::HashSet;
use utils::SignatureOf;
use zei::api::anon_creds::{
    ac_confidential_open_commitment, ACCommitment, ACCommitmentKey, ConfidentialAC,
    Credential,
};
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{
    build_blind_asset_record, build_open_asset_record, open_blind_asset_record,
    AssetRecordType,
};
use zei::xfr::lib::XfrNotePolicies;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{
    AssetRecord, AssetRecordTemplate, BlindAssetRecord, OpenAssetRecord, OwnerMemo,
    TracingPolicies, TracingPolicy,
};

macro_rules! no_transfer_err {
    () => {
        inv_fail!("Transaction has not yet been finalized".to_string())
    };
}

#[derive(Deserialize, Serialize, PartialEq)]
pub enum PolicyChoice {
    Fungible(),
    LoanToken(Fraction, AssetTypeCode, u64),
}

impl Default for PolicyChoice {
    fn default() -> Self {
        Self::Fungible()
    }
}

fn debt_policy() -> Policy {
    use ledger::policy_script::*;
    Policy {
        num_id_globals: 1,
        num_rt_globals: 2,
        num_amt_globals: 1,
        num_frac_globals: 1,
        init_check: TxnCheck {
            name: "init_txn".to_string(),
            in_params: vec![],
            out_params: vec![],
            id_ops: vec![],
            rt_ops: vec![],
            fraction_ops: vec![
                FractionOp::Const(Fraction::new(0, 1)),
                FractionOp::Var(FractionVar(0)),
                FractionOp::Const(Fraction::new(1, 1)),
            ],
            amount_ops: vec![AmountOp::Const(0), AmountOp::Var(AmountVar(0))],
            bool_ops: vec![
                BoolOp::FracGe(FractionVar(1), FractionVar(1)),
                BoolOp::FracGe(FractionVar(2), FractionVar(1)),
                BoolOp::FracEq(FractionVar(2), FractionVar(1)),
                BoolOp::Not(BoolVar(2)),
                BoolOp::FracGe(FractionVar(2), FractionVar(1)),
                BoolOp::And(BoolVar(3), BoolVar(4)),
                BoolOp::FracGe(FractionVar(3), FractionVar(1)),
                BoolOp::FracEq(FractionVar(3), FractionVar(2)),
                BoolOp::Not(BoolVar(7)),
                BoolOp::FracGe(FractionVar(3), FractionVar(2)),
                BoolOp::And(BoolVar(8), BoolVar(9)),
                BoolOp::AmtGe(AmountVar(1), AmountVar(1)),
                BoolOp::AmtGe(AmountVar(2), AmountVar(1)),
            ],
            assertions: vec![
                BoolVar(0),
                BoolVar(1),
                BoolVar(5),
                BoolVar(6),
                BoolVar(10),
                BoolVar(11),
                BoolVar(12),
            ],
            required_signatures: vec![],
            txn_template: vec![],
        },
        txn_choices: vec![
            TxnCheck {
                name: "setup_loan".to_string(),
                in_params: vec![],
                out_params: vec![ResourceTypeVar(0)],
                id_ops: vec![IdOp::OwnerOf(ResourceVar(0)), IdOp::Var(IdVar(0))],
                rt_ops: vec![],
                fraction_ops: vec![],
                amount_ops: vec![AmountOp::Var(AmountVar(0))],
                bool_ops: vec![
                    BoolOp::IdEq(IdVar(1), IdVar(2)),
                    BoolOp::AmtEq(AmountVar(1), AmountVar(1)),
                ],
                assertions: vec![BoolVar(0), BoolVar(1)],
                required_signatures: vec![IdVar(0)],
                txn_template: vec![TxnOp::Issue(
                    AmountVar(1),
                    ResourceTypeVar(0),
                    ResourceVar(0),
                )],
            },
            TxnCheck {
                name: "start_loan".to_string(),
                in_params: vec![ResourceTypeVar(0), ResourceTypeVar(1)],
                out_params: vec![ResourceTypeVar(0), ResourceTypeVar(1)],
                id_ops: vec![
                    IdOp::OwnerOf(ResourceVar(0)),
                    IdOp::Var(IdVar(0)),
                    IdOp::OwnerOf(ResourceVar(1)),
                    IdOp::OwnerOf(ResourceVar(2)),
                ],
                rt_ops: vec![],
                fraction_ops: vec![],
                amount_ops: vec![
                    AmountOp::AmountOf(ResourceVar(1)),
                    AmountOp::AmountOf(ResourceVar(0)),
                    AmountOp::Const(0),
                    AmountOp::Minus(AmountVar(1), AmountVar(2)),
                    AmountOp::Minus(AmountVar(2), AmountVar(2)),
                ],
                bool_ops: vec![
                    BoolOp::AmtGe(AmountVar(1), AmountVar(2)),
                    BoolOp::IdEq(IdVar(1), IdVar(2)),
                    BoolOp::IdEq(IdVar(4), IdVar(3)),
                    BoolOp::AmtEq(AmountVar(2), AmountVar(2)),
                    BoolOp::AmtGe(AmountVar(3), AmountVar(3)),
                    BoolOp::AmtEq(AmountVar(4), AmountVar(3)),
                    BoolOp::AmtGe(AmountVar(2), AmountVar(2)),
                    BoolOp::AmtEq(AmountVar(5), AmountVar(3)),
                ],
                assertions: vec![
                    BoolVar(0),
                    BoolVar(1),
                    BoolVar(2),
                    BoolVar(3),
                    BoolVar(4),
                    BoolVar(5),
                    BoolVar(6),
                    BoolVar(7),
                ],
                required_signatures: vec![IdVar(0), IdVar(3)],
                txn_template: vec![
                    TxnOp::Transfer(AmountVar(2), ResourceVar(1), Some(ResourceVar(3))),
                    TxnOp::Transfer(AmountVar(2), ResourceVar(0), Some(ResourceVar(2))),
                ],
            },
            TxnCheck {
                name: "repay_loan".to_string(),
                in_params: vec![ResourceTypeVar(0), ResourceTypeVar(1)],
                out_params: vec![ResourceTypeVar(0), ResourceTypeVar(1)],
                id_ops: vec![
                    IdOp::OwnerOf(ResourceVar(3)),
                    IdOp::OwnerOf(ResourceVar(0)),
                ],
                rt_ops: vec![],
                fraction_ops: vec![
                    FractionOp::Var(FractionVar(0)),
                    FractionOp::AmtTimes(AmountVar(1), FractionVar(1)),
                ],
                amount_ops: vec![
                    AmountOp::AmountOf(ResourceVar(0)),
                    AmountOp::AmountOf(ResourceVar(1)),
                    AmountOp::Round(FractionVar(2)),
                    AmountOp::Minus(AmountVar(2), AmountVar(3)),
                    AmountOp::Minus(AmountVar(1), AmountVar(4)),
                    AmountOp::Const(0),
                    AmountOp::Minus(AmountVar(2), AmountVar(2)),
                    AmountOp::Minus(AmountVar(5), AmountVar(5)),
                ],
                bool_ops: vec![
                    BoolOp::IdEq(IdVar(1), IdVar(2)),
                    BoolOp::AmtGe(AmountVar(2), AmountVar(3)),
                    BoolOp::AmtGe(AmountVar(1), AmountVar(4)),
                    BoolOp::AmtGe(AmountVar(1), AmountVar(5)),
                    BoolOp::AmtGe(AmountVar(6), AmountVar(6)),
                    BoolOp::AmtGe(AmountVar(2), AmountVar(2)),
                    BoolOp::AmtEq(AmountVar(7), AmountVar(6)),
                    BoolOp::AmtGe(AmountVar(5), AmountVar(5)),
                    BoolOp::AmtEq(AmountVar(8), AmountVar(6)),
                ],
                assertions: vec![
                    BoolVar(0),
                    BoolVar(1),
                    BoolVar(2),
                    BoolVar(3),
                    BoolVar(4),
                    BoolVar(5),
                    BoolVar(6),
                    BoolVar(7),
                    BoolVar(8),
                ],
                required_signatures: vec![],
                txn_template: vec![
                    TxnOp::Transfer(AmountVar(4), ResourceVar(0), None),
                    TxnOp::Transfer(AmountVar(5), ResourceVar(0), Some(ResourceVar(2))),
                    TxnOp::Transfer(AmountVar(2), ResourceVar(1), Some(ResourceVar(3))),
                ],
            },
        ],
    }
}

fn debt_globals(
    code: &AssetTypeCode,
    borrower: &XfrPublicKey,
    interest_rate: Fraction,
    fiat_type: AssetTypeCode,
    amount: u64,
) -> PolicyGlobals {
    PolicyGlobals {
        id_vars: vec![*borrower],
        rt_vars: vec![(*code).val, fiat_type.val],
        amt_vars: vec![amount],
        frac_vars: vec![interest_rate],
    }
}

fn policy_from_choice(
    code: &AssetTypeCode,
    borrower: &XfrPublicKey,
    c: PolicyChoice,
) -> Option<(Box<Policy>, PolicyGlobals)> {
    match c {
        PolicyChoice::Fungible() => None,
        PolicyChoice::LoanToken(interest_rate, fiat_type, amount) => Some((
            Box::new(debt_policy()),
            debt_globals(code, borrower, interest_rate, fiat_type, amount),
        )),
    }
}

pub trait BuildsTransactions {
    fn transaction(&self) -> &Transaction;
    fn sign(&mut self, kp: &XfrKeyPair) -> &mut Self;
    fn add_signature(
        &mut self,
        pk: &XfrPublicKey,
        sig: SignatureOf<TransactionBody>,
    ) -> Result<&mut Self>;
    fn add_memo(&mut self, memo: Memo) -> &mut Self;
    fn add_policy_option(
        &mut self,
        token_code: AssetTypeCode,
        which_check: String,
    ) -> &mut Self;
    #[allow(clippy::too_many_arguments)]
    fn add_operation_create_asset(
        &mut self,
        key_pair: &XfrKeyPair,
        token_code: Option<AssetTypeCode>,
        asset_rules: AssetRules,
        memo: &str,
        policy_choice: PolicyChoice,
    ) -> Result<&mut Self>;
    fn add_operation_issue_asset(
        &mut self,
        key_pair: &XfrKeyPair,
        token_code: &AssetTypeCode,
        seq_num: u64,
        records: &[(TxOutput, Option<OwnerMemo>)],
    ) -> Result<&mut Self>;
    #[allow(clippy::too_many_arguments)]
    fn add_operation_transfer_asset(
        &mut self,
        keys: &XfrKeyPair,
        input_sids: Vec<TxoRef>,
        input_records: &[OpenAssetRecord],
        input_tracing_policies: Vec<Option<TracingPolicy>>,
        input_identity_commitments: Vec<Option<ACCommitment>>,
        output_records: &[AssetRecord],
        output_identity_commitments: Vec<Option<ACCommitment>>,
    ) -> Result<&mut Self>;
    fn add_operation_air_assign(
        &mut self,
        key_pair: &XfrKeyPair,
        addr: CredUserPublicKey,
        data: CredCommitment,
        issuer_pk: CredIssuerPublicKey,
        pok: CredPoK,
    ) -> Result<&mut Self>;
    fn add_operation_kv_update(
        &mut self,
        auth_key_pair: &XfrKeyPair,
        index: &Key,
        seq_num: u64,
        data: Option<&KVHash>,
    ) -> Result<&mut Self>;
    fn add_operation_update_memo(
        &mut self,
        auth_key_pair: &XfrKeyPair,
        asset_code: AssetTypeCode,
        new_memo: &str,
    ) -> &mut Self;

    fn serialize(&self) -> Vec<u8>;
    fn serialize_str(&self) -> String;

    fn add_operation(&mut self, op: Operation) -> &mut Self;

    fn add_basic_issue_asset(
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
                    record: ba,
                    lien: None,
                },
                owner_memo,
            )],
        )
        .c(d!())
    }

    #[allow(clippy::comparison_chain)]
    #[allow(clippy::too_many_arguments)]
    fn add_basic_transfer_asset(
        &mut self,
        key_pair: &XfrKeyPair,
        transfer_from: &[(&TxoRef, &BlindAssetRecord, u64, &Option<OwnerMemo>)],
        input_tracing_policies: Vec<Option<TracingPolicy>>,
        input_identity_commitments: Vec<Option<ACCommitment>>,
        transfer_to: &[(u64, &AccountAddress)],
        output_tracing_policies: Vec<Option<TracingPolicy>>,
        output_identity_commitments: Vec<Option<ACCommitment>>,
    ) -> Result<&mut Self> {
        // TODO(fernando): where to get prng
        let mut prng: ChaChaRng;
        prng = ChaChaRng::from_entropy();

        let input_sids: Vec<TxoRef> = transfer_from
            .iter()
            .map(|(ref txo_sid, _, _, _)| *(*txo_sid))
            .collect();
        let input_amounts: Vec<u64> = transfer_from
            .iter()
            .map(|(_, _, amount, _)| *amount)
            .collect();
        let input_oars: Result<Vec<OpenAssetRecord>> = transfer_from
            .iter()
            .map(|(_, ref ba, _, owner_memo)| {
                open_blind_asset_record(&ba, owner_memo, &key_pair)
            })
            .collect();
        let input_oars = input_oars.c(d!(PlatformError::ZeiError(None)))?;
        let input_total: u64 = input_amounts.iter().sum();
        let mut partially_consumed_inputs = Vec::new();
        for ((input_amount, oar), input_tracing_policy) in input_amounts
            .iter()
            .zip(input_oars.iter())
            .zip(input_tracing_policies.iter())
        {
            if input_amount > oar.get_amount() {
                return Err(eg!(PlatformError::InputsError(None)));
            } else if input_amount < oar.get_amount() {
                let mut policies = TracingPolicies::new();
                if let Some(policy) = &input_tracing_policy {
                    policies.add(policy.clone());
                }
                let ar = AssetRecordTemplate::with_asset_tracing(
                    oar.get_amount() - input_amount,
                    *oar.get_asset_type(),
                    oar.get_record_type(),
                    *oar.get_pub_key(),
                    policies,
                );
                partially_consumed_inputs.push(ar);
            }
        }
        let output_total = transfer_to.iter().fold(0, |acc, (amount, _)| acc + amount);
        if input_total != output_total {
            return Err(eg!(PlatformError::InputsError(None)));
        }
        let asset_type = input_oars[0].get_asset_type();
        let asset_record_type = input_oars[0].get_record_type();
        let mut output_ars_templates = Vec::new();
        for ((amount, ref addr), output_tracing_policy) in
            transfer_to.iter().zip(output_tracing_policies.iter())
        {
            let mut policies = TracingPolicies::new();
            if let Some(policy) = output_tracing_policy {
                policies.add(policy.clone())
            }
            let template = AssetRecordTemplate::with_asset_tracing(
                *amount,
                *asset_type,
                asset_record_type,
                addr.key,
                policies,
            );
            output_ars_templates.push(template);
        }
        output_ars_templates.append(&mut partially_consumed_inputs);
        let output_ars: Result<Vec<AssetRecord>> = output_ars_templates
            .iter()
            .map(|x| AssetRecord::from_template_no_identity_tracing(&mut prng, x))
            .collect();
        let output_ars = output_ars.c(d!(PlatformError::ZeiError(None)))?;
        self.add_operation_transfer_asset(
            &key_pair,
            input_sids,
            &input_oars,
            input_tracing_policies,
            input_identity_commitments,
            &output_ars,
            output_identity_commitments,
        )
        .c(d!())?;

        Ok(self)
    }
}

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
pub struct FeeInputs {
    pub inner: Vec<FeeInput>,
}

impl FeeInputs {
    pub fn new() -> Self {
        FeeInputs::default()
    }

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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransactionBuilder {
    txn: Transaction,
    outputs: u64,
    no_replay_token: NoReplayToken,
}

impl TransactionBuilder {
    pub fn into_transaction(self) -> Transaction {
        self.txn
    }

    pub fn get_transaction(&self) -> &Transaction {
        &self.txn
    }

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
            .map(|new| match new {
                Operation::TransferAsset(d) => {
                    seek!(d)
                }
                Operation::IssueAsset(d) => d
                    .body
                    .records
                    .iter()
                    .map(|(o, om)| (o.record.clone(), om.clone()))
                    .collect(),
                Operation::BindAssets(d) => {
                    seek!(d)
                }
                Operation::ReleaseAssets(d) => {
                    seek!(d)
                }
                _ => Vec::new(),
            })
            .flatten()
            .rev()
            .collect()
    }

    /// @param am: amount to pay
    /// @param kp: owner's XfrKeyPair
    pub fn add_fee_relative_auto(
        &mut self,
        mut am: u64,
        kp: &XfrKeyPair,
    ) -> Result<&mut TransactionBuilder> {
        let mut opb = TransferOperationBuilder::default();
        let outputs = self.get_relative_outputs();

        for (idx, (o, om)) in outputs.into_iter().enumerate() {
            if 0 < am {
                if let Ok(oar) = open_blind_asset_record(&o, &om, &kp) {
                    if ASSET_TYPE_FRA == oar.asset_type
                        && kp.get_pk_ref().as_bytes() == o.public_key.as_bytes()
                    {
                        let n = if oar.amount > am {
                            let n = am;
                            am = 0;
                            n
                        } else {
                            am -= oar.amount;
                            oar.amount
                        };

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
        .and_then(|o| o.balance().c(d!()))
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
        .and_then(|o| o.balance().c(d!()))
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

    /// SEE [check_fee](leder::data_model::Transaction::check_fee)
    #[inline(always)]
    pub fn check_fee(&self) -> bool {
        self.txn.check_fee()
    }

    pub fn get_owner_memo_ref(&self, idx: usize) -> Option<&OwnerMemo> {
        self.txn.get_owner_memos_ref()[idx]
    }

    pub fn get_output_ref(&self, idx: usize) -> TxOutput {
        self.txn.get_outputs_ref(true)[idx].clone()
    }

    pub fn from_seq_id(seq_id: u64) -> Self {
        let mut prng = ChaChaRng::from_entropy();
        let no_replay_token = NoReplayToken::new(&mut prng, seq_id);
        TransactionBuilder {
            txn: Transaction::from_seq_id(seq_id),
            outputs: 0,
            no_replay_token,
        }
    }
}

impl BuildsTransactions for TransactionBuilder {
    fn transaction(&self) -> &Transaction {
        &self.txn
    }
    fn add_memo(&mut self, memo: Memo) -> &mut Self {
        self.txn.body.memos.push(memo);
        self
    }

    fn add_policy_option(
        &mut self,
        token_code: AssetTypeCode,
        which_check: String,
    ) -> &mut Self {
        if self.txn.body.policy_options.is_none() {
            self.txn.body.policy_options = Some(TxnPolicyData(vec![]));
        }
        self.txn
            .body
            .policy_options
            .as_mut()
            .unwrap()
            .0
            .push((token_code, TxnCheckInputs { which_check }));
        self
    }

    fn add_operation_create_asset(
        &mut self,
        key_pair: &XfrKeyPair,
        token_code: Option<AssetTypeCode>,
        asset_rules: AssetRules,
        memo: &str,
        policy_choice: PolicyChoice,
    ) -> Result<&mut Self> {
        let token_code = match token_code {
            Some(code) => code,
            None => AssetTypeCode::gen_random(),
        };
        let pol = policy_from_choice(&token_code, key_pair.get_pk_ref(), policy_choice);
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
                    pol,
                )
                .c(d!())?,
                &iss_keypair,
            )
            .c(d!())?,
        ));

        Ok(self)
    }
    fn add_operation_issue_asset(
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

    fn add_operation_transfer_asset(
        &mut self,
        keys: &XfrKeyPair,
        input_sids: Vec<TxoRef>,
        input_records: &[OpenAssetRecord],
        input_tracing_policies: Vec<Option<TracingPolicy>>,
        _input_identity_commitments: Vec<Option<ACCommitment>>,
        output_records: &[AssetRecord],
        _output_identity_commitments: Vec<Option<ACCommitment>>,
    ) -> Result<&mut Self> {
        // TODO(joe/noah): keep a prng around somewhere?
        let mut prng: ChaChaRng;
        prng = ChaChaRng::from_entropy();
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
                .c(d!(PlatformError::ZeiError(None)))?,
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
    fn add_operation_kv_update(
        &mut self,
        auth_key_pair: &XfrKeyPair,
        index: &Key,
        seq_num: u64,
        hash: Option<&KVHash>,
    ) -> Result<&mut Self> {
        let update = KVUpdate::new((*index, hash.cloned()), seq_num, auth_key_pair);
        self.txn.add_operation(Operation::KVStoreUpdate(update));
        Ok(self)
    }
    fn add_operation_air_assign(
        &mut self,
        key_pair: &XfrKeyPair,
        addr: CredUserPublicKey,
        data: CredCommitment,
        issuer_pk: CredIssuerPublicKey,
        pok: CredPoK,
    ) -> Result<&mut Self> {
        let xfr = AIRAssign::new(
            AIRAssignBody::new(
                addr,
                data,
                issuer_pk,
                pok,
                self.txn.body.no_replay_token,
            )
            .c(d!())?,
            key_pair,
        )
        .c(d!())?;
        self.txn.add_operation(Operation::AIRAssign(xfr));
        Ok(self)
    }

    fn add_operation_update_memo(
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

    fn add_operation(&mut self, op: Operation) -> &mut Self {
        self.txn.add_operation(op);
        self
    }

    fn sign(&mut self, kp: &XfrKeyPair) -> &mut Self {
        self.txn.sign(kp);
        self
    }

    fn add_signature(
        &mut self,
        pk: &XfrPublicKey,
        sig: SignatureOf<TransactionBody>,
    ) -> Result<&mut Self> {
        self.txn.check_signature(pk, &sig).c(d!())?;
        self.txn.signatures.push(sig);
        Ok(self)
    }

    fn serialize(&self) -> Vec<u8> {
        // Unwrap is safe beacuse the underlying transaction is guaranteed to be serializable.
        let j = serde_json::to_string(&self.txn).unwrap();
        j.as_bytes().to_vec()
    }

    fn serialize_str(&self) -> String {
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
    // TODO (fernando) this code does not handle more than one policy, hence the following assert
    // REDMINE #104
    debug_assert!(template.asset_tracing_policies.len() <= 1);
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
        return Err(eg!(PlatformError::InputsError(None)));
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
            open_asset_record.amount_blinds.0.0,
            open_asset_record.amount_blinds.1.0,
        ),
        open_asset_record.type_blind.0,
    ))
}

// TransferOperationBuilder constructs transfer operations using the factory pattern
// Inputs and outputs are added iteratively before being signed by all input record owners
//
// Example usage:
//
//    let alice = XfrKeyPair::generate(&mut prng);
//    let bob = XfrKeyPair::generate(&mut prng);
//
//    let ar = AssetRecord::new(1000, code_1.val, *alice.get_pk_ref()).c(d!())?;
//    let ba = build_blind_asset_record(&mut prng, &params.pc_gens, &ar_1, false, false, &None);
//
//    let builder = TransferOperationBuilder::new()..add_input(TxoRef::Relative(1),
//                                       open_blind_asset_record(&ba, &alice).c(d!())?,
//                                       None,
//                                       20)?
//                            .add_output(20, bob.get_pk_ref(), code_1)?
//                            .balance()?
//                            .create(TransferType::Standard)?
//                            .sign(&alice)?;
//
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
}

impl TransferOperationBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    // TxoRef is the location of the input on the ledger and the amount is how much of the record
    // should be spent in the transfer. See tests for example usage.
    pub fn add_input(
        &mut self,
        txo_sid: TxoRef,
        open_ar: OpenAssetRecord,
        tracing_policies: Option<TracingPolicies>,
        identity_commitment: Option<ACCommitment>,
        amount: u64,
    ) -> Result<&mut Self> {
        if self.transfer.is_some() {
            return Err(eg!(inv_fail!(
                "Cannot mutate a transfer that has been signed".to_string()
            )));
        }
        let policies = tracing_policies.unwrap_or_default();

        let asset_record =
            AssetRecord::from_open_asset_record_with_asset_tracing_but_no_identity(
                &mut ChaChaRng::from_entropy(),
                open_ar,
                policies.clone(),
            )
            .c(d!(PlatformError::ZeiError(None)))?;
        self.input_sids.push(txo_sid);
        self.input_records.push(asset_record);
        self.inputs_tracing_policies.push(policies);
        self.input_identity_commitments.push(identity_commitment);
        self.spend_amounts.push(amount);
        Ok(self)
    }

    pub fn add_output(
        &mut self,
        asset_record_template: &AssetRecordTemplate,
        tracing_policies: Option<TracingPolicies>,
        identity_commitment: Option<ACCommitment>,
        credential_record: Option<(&CredUserSecretKey, &Credential, &ACCommitmentKey)>,
    ) -> Result<&mut Self> {
        let prng = &mut ChaChaRng::from_entropy();
        if self.transfer.is_some() {
            return Err(eg!(inv_fail!(
                "Cannot mutate a transfer that has been signed".to_string()
            )));
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
            return Err(eg!(inv_fail!(
                "Cannot mutate a transfer that has been signed".to_string()
            )));
        }
        let (ar, amount_blinds, type_blind) =
            if let Some((user_secret_key, credential, commitment_key)) =
                credential_record
            {
                match asset_record_template.asset_tracing_policies.get_policy(0) {
                    None => {
                        // identity tracing must have asset_tracing policy
                        return Err(eg!(PlatformError::InputsError(None)));
                    }
                    Some(policy) => {
                        match &policy.identity_tracing {
                            // policy must have a identity tracing policy
                            None => {
                                return Err(eg!(PlatformError::InputsError(None)));
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
                                .c(d!(PlatformError::ZeiError(None)))?;
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
                        return Err(eg!(PlatformError::InputsError(None)));
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

    // Ensures that outputs and inputs are balanced by adding remainder outputs for leftover asset
    // amounts
    pub fn balance(&mut self) -> Result<&mut Self> {
        let mut prng = ChaChaRng::from_entropy();
        if self.transfer.is_some() {
            return Err(eg!(inv_fail!(
                "Cannot mutate a transfer that has been signed".to_string()
            )));
        }
        let spend_total: u64 = self.spend_amounts.iter().sum();
        let mut partially_consumed_inputs = Vec::new();
        for ((spend_amount, ar), policies) in self
            .spend_amounts
            .iter()
            .zip(self.input_records.iter())
            .zip(self.inputs_tracing_policies.iter())
        {
            let amt = ar.open_asset_record.get_amount();
            match spend_amount.cmp(&amt) {
                Ordering::Greater => {
                    return Err(eg!(PlatformError::InputsError(None)));
                }
                Ordering::Less => {
                    let asset_type = *ar.open_asset_record.get_asset_type();
                    let record_type = ar.open_asset_record.get_record_type();
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
                }
                _ => {}
            }
        }
        let output_total = self
            .output_records
            .iter()
            .fold(0, |acc, ar| acc + ar.open_asset_record.amount);
        if spend_total != output_total {
            return Err(eg!(PlatformError::InputsError(None)));
        }
        self.output_records.append(&mut partially_consumed_inputs);
        Ok(self)
    }

    // Finalize the transaction and prepare for signing. Once called, the transaction cannot be
    // modified.
    pub fn create(&mut self, transfer_type: TransferType) -> Result<&mut Self> {
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

    pub fn get_output_record(&self, idx: usize) -> Option<BlindAssetRecord> {
        self.transfer
            .as_ref()?
            .body
            .transfer
            .outputs
            .get(idx)
            .cloned()
    }

    // All input owners must sign eventually for the transaction to be valid.
    pub fn sign(&mut self, kp: &XfrKeyPair) -> Result<&mut Self> {
        if self.transfer.is_none() {
            return Err(eg!(no_transfer_err!()));
        }
        self.transfer.as_mut().c(d!())?.sign(&kp);
        Ok(self)
    }

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

    pub fn create_cosignature(
        &self,
        keypair: &XfrKeyPair,
        input_idx: usize,
    ) -> Result<IndexedSignature<TransferAssetBody>> {
        let sig = self
            .transfer
            .as_ref()
            .c(d!(no_transfer_err!()))?
            .create_cosignature(keypair, input_idx);
        Ok(sig)
    }

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

    // Add a co-signature for an input.
    pub fn sign_cosignature(
        &mut self,
        kp: &XfrKeyPair,
        input_idx: usize,
    ) -> Result<&mut Self> {
        let mut new_transfer = self.transfer.as_mut().c(d!(no_transfer_err!()))?.clone();
        new_transfer.sign_cosignature(&kp, input_idx);
        Ok(self)
    }

    // Return the transaction operation
    pub fn transaction(&self) -> Result<Operation> {
        if self.transfer.is_none() {
            return Err(eg!(no_transfer_err!()));
        }
        Ok(Operation::TransferAsset(self.transfer.clone().c(d!())?))
    }

    // Checks to see whether all necessary signatures are present and valid
    pub fn validate_signatures(&mut self) -> Result<&mut Self> {
        if self.transfer.is_none() {
            return Err(eg!(no_transfer_err!()));
        }

        let trn = self.transfer.as_ref().c(d!())?;
        let mut sig_keys = HashSet::new();
        for sig in &trn.body_signatures {
            if !sig.verify(&trn.body) {
                return Err(eg!(inv_fail!("Invalid signature".to_string())));
            }
            sig_keys.insert(sig.address.key.zei_to_bytes());
        }

        for record in &trn.body.transfer.inputs {
            if !sig_keys.contains(&record.public_key.zei_to_bytes()) {
                return Err(eg!(inv_fail!("Not all signatures present".to_string())));
            }
        }
        Ok(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ledger::data_model::TxoRef;
    use ledger::store::{fra_gen_initial_tx, LedgerAccess, LedgerState, LedgerUpdate};
    use rand_chacha::ChaChaRng;
    use rand_core::SeedableRng;
    use zei::setup::PublicParams;
    use zei::xfr::asset_record::AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
    use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record};
    use zei::xfr::sig::XfrKeyPair;

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
            .balance();

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
            .balance()
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
            .balance()
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
            .balance()
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
        let mut ledger = LedgerState::test_ledger();
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
                            &ledger.get_utxo($txo_sid).unwrap().utxo.0.record,
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
                    .balance()
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
            .add_fee_relative_auto(TX_FEE_MIN, &fra_owner_kp)
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
        let utxo = ledger.get_utxo(txo_sid[0]).unwrap();
        fi.append(
            TX_FEE_MIN,
            TxoRef::Absolute(txo_sid[0]),
            utxo.utxo.0,
            utxo.authenticated_txn
                .finalized_txn
                .txn
                .get_owner_memos_ref()[utxo.utxo_location.0]
                .map(|om| om.clone()),
            bob_kp.get_sk().into_keypair(),
        );
        let mut tx3 = TransactionBuilder::from_seq_id(2);
        pnk!(
            tx3.add_operation(transfer_to_bob!(txo_sid[2], bob_kp.get_pk()))
                .add_fee(fi)
        );
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
        let utxo = ledger.get_utxo(txo_sid[0]).unwrap();
        fi.append(
            TX_FEE_MIN,
            TxoRef::Absolute(txo_sid[0]),
            utxo.utxo.0,
            utxo.authenticated_txn
                .finalized_txn
                .txn
                .get_owner_memos_ref()[utxo.utxo_location.0]
                .map(|om| om.clone()),
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
        ledger.abort_block(block);
    }
}
