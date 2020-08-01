#![deny(warnings)]
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
use ledger::policies::Fraction;
use ledger::policy_script::{Policy, PolicyGlobals, TxnCheckInputs, TxnPolicyData};
use ledger::{error_location, inv_fail};
use rand_chacha::ChaChaRng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use sparse_merkle_tree::Key;
use std::cmp::Ordering;
use std::collections::HashSet;
use utils::SignatureOf;
use zei::api::anon_creds::{
  ac_confidential_open_commitment, ACCommitment, ACCommitmentKey, ConfidentialAC, Credential,
};
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{
  build_blind_asset_record, build_open_asset_record, open_blind_asset_record, AssetRecordType,
};
use zei::xfr::lib::XfrNotePolicies;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{
  AssetRecord, AssetRecordTemplate, AssetTracingPolicies, AssetTracingPolicy, BlindAssetRecord,
  OpenAssetRecord, OwnerMemo,
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
  Policy { num_id_globals: 1,
           num_rt_globals: 2,
           num_amt_globals: 1,
           num_frac_globals: 1,
           init_check: TxnCheck { name: "init_txn".to_string(),
                                  in_params: vec![],
                                  out_params: vec![],
                                  id_ops: vec![],
                                  rt_ops: vec![],
                                  fraction_ops: vec![FractionOp::Const(Fraction::new(0, 1)),
                                                     FractionOp::Var(FractionVar(0)),
                                                     FractionOp::Const(Fraction::new(1, 1)),],
                                  amount_ops: vec![AmountOp::Const(0),
                                                   AmountOp::Var(AmountVar(0)),],
                                  bool_ops: vec![BoolOp::FracGe(FractionVar(1),
                                                                FractionVar(1)),
                                                 BoolOp::FracGe(FractionVar(2),
                                                                FractionVar(1)),
                                                 BoolOp::FracEq(FractionVar(2),
                                                                FractionVar(1)),
                                                 BoolOp::Not(BoolVar(2)),
                                                 BoolOp::FracGe(FractionVar(2),
                                                                FractionVar(1)),
                                                 BoolOp::And(BoolVar(3), BoolVar(4)),
                                                 BoolOp::FracGe(FractionVar(3),
                                                                FractionVar(1)),
                                                 BoolOp::FracEq(FractionVar(3),
                                                                FractionVar(2)),
                                                 BoolOp::Not(BoolVar(7)),
                                                 BoolOp::FracGe(FractionVar(3),
                                                                FractionVar(2)),
                                                 BoolOp::And(BoolVar(8), BoolVar(9)),
                                                 BoolOp::AmtGe(AmountVar(1), AmountVar(1)),
                                                 BoolOp::AmtGe(AmountVar(2), AmountVar(1)),],
                                  assertions: vec![BoolVar(0),
                                                   BoolVar(1),
                                                   BoolVar(5),
                                                   BoolVar(6),
                                                   BoolVar(10),
                                                   BoolVar(11),
                                                   BoolVar(12),],
                                  required_signatures: vec![],
                                  txn_template: vec![] },
           txn_choices: vec![
                             TxnCheck { name: "setup_loan".to_string(),
                                        in_params: vec![],
                                        out_params: vec![ResourceTypeVar(0)],
                                        id_ops: vec![
    IdOp::OwnerOf(ResourceVar(0)),
    IdOp::Var(IdVar(0)),
  ],
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
  )], },
                             TxnCheck { name: "start_loan".to_string(),
                                        in_params: vec![
    ResourceTypeVar(0),
    ResourceTypeVar(1),
  ],
                                        out_params: vec![
    ResourceTypeVar(0),
    ResourceTypeVar(1),
  ],
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
  ], },
                             TxnCheck { name: "repay_loan".to_string(),
                                        in_params: vec![
    ResourceTypeVar(0),
    ResourceTypeVar(1),
  ],
                                        out_params: vec![
    ResourceTypeVar(0),
    ResourceTypeVar(1),
  ],
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
  ], },
  ] }
}

fn debt_globals(code: &AssetTypeCode,
                borrower: &XfrPublicKey,
                interest_rate: Fraction,
                fiat_type: AssetTypeCode,
                amount: u64)
                -> PolicyGlobals {
  PolicyGlobals { id_vars: vec![*borrower],
                  rt_vars: vec![(*code).val, fiat_type.val],
                  amt_vars: vec![amount],
                  frac_vars: vec![interest_rate] }
}

fn policy_from_choice(code: &AssetTypeCode,
                      borrower: &XfrPublicKey,
                      c: PolicyChoice)
                      -> Option<(Box<Policy>, PolicyGlobals)> {
  match c {
    PolicyChoice::Fungible() => None,
    PolicyChoice::LoanToken(interest_rate, fiat_type, amount) => {
      Some((Box::new(debt_policy()),
            debt_globals(code, borrower, interest_rate, fiat_type, amount)))
    }
  }
}

pub trait BuildsTransactions {
  fn transaction(&self) -> &Transaction;
  fn sign(&mut self, kp: &XfrKeyPair) -> &mut Self;
  fn add_signature(&mut self,
                   pk: &XfrPublicKey,
                   sig: SignatureOf<TransactionBody>)
                   -> Result<&mut Self, PlatformError>;
  fn add_memo(&mut self, memo: Memo) -> &mut Self;
  fn add_policy_option(&mut self, token_code: AssetTypeCode, which_check: String) -> &mut Self;
  #[allow(clippy::too_many_arguments)]
  fn add_operation_create_asset(&mut self,
                                key_pair: &XfrKeyPair,
                                token_code: Option<AssetTypeCode>,
                                asset_rules: AssetRules,
                                memo: &str,
                                policy_choice: PolicyChoice)
                                -> Result<&mut Self, PlatformError>;
  fn add_operation_issue_asset(&mut self,
                               key_pair: &XfrKeyPair,
                               token_code: &AssetTypeCode,
                               seq_num: u64,
                               records: &[(TxOutput, Option<OwnerMemo>)])
                               -> Result<&mut Self, PlatformError>;
  #[allow(clippy::too_many_arguments)]
  fn add_operation_transfer_asset(&mut self,
                                  keys: &XfrKeyPair,
                                  input_sids: Vec<TxoRef>,
                                  input_records: &[OpenAssetRecord],
                                  input_tracing_policies: Vec<Option<AssetTracingPolicy>>,
                                  input_identity_commitments: Vec<Option<ACCommitment>>,
                                  output_records: &[AssetRecord],
                                  output_identity_commitments: Vec<Option<ACCommitment>>)
                                  -> Result<&mut Self, PlatformError>;
  fn add_operation_air_assign(&mut self,
                              key_pair: &XfrKeyPair,
                              addr: CredUserPublicKey,
                              data: CredCommitment,
                              issuer_pk: CredIssuerPublicKey,
                              pok: CredPoK)
                              -> Result<&mut Self, PlatformError>;
  fn add_operation_kv_update(&mut self,
                             auth_key_pair: &XfrKeyPair,
                             index: &Key,
                             seq_num: u64,
                             data: Option<&KVHash>)
                             -> Result<&mut Self, PlatformError>;
  fn add_operation_update_memo(&mut self,
                               auth_key_pair: &XfrKeyPair,
                               asset_code: AssetTypeCode,
                               new_memo: &str)
                               -> &mut Self;

  fn serialize(&self) -> Vec<u8>;
  fn serialize_str(&self) -> String;

  fn add_operation(&mut self, op: Operation) -> &mut Self;

  fn add_basic_issue_asset(&mut self,
                           key_pair: &XfrKeyPair,
                           token_code: &AssetTypeCode,
                           seq_num: u64,
                           amount: u64,
                           confidentiality_flags: AssetRecordType,
                           zei_params: &PublicParams)
                           -> Result<&mut Self, PlatformError> {
    let mut prng = ChaChaRng::from_entropy();
    let ar = AssetRecordTemplate::with_no_asset_tracking(amount,
                                                         token_code.val,
                                                         confidentiality_flags,
                                                         key_pair.get_pk());

    let (ba, _, owner_memo) = build_blind_asset_record(&mut prng, &zei_params.pc_gens, &ar, vec![]);
    self.add_operation_issue_asset(key_pair, token_code, seq_num, &[(TxOutput(ba), owner_memo)])
  }

  #[allow(clippy::comparison_chain)]
  #[allow(clippy::too_many_arguments)]
  fn add_basic_transfer_asset(&mut self,
                              key_pair: &XfrKeyPair,
                              transfer_from: &[(&TxoRef,
                                 &BlindAssetRecord,
                                 u64,
                                 &Option<OwnerMemo>)],
                              input_tracing_policies: Vec<Option<AssetTracingPolicy>>,
                              input_identity_commitments: Vec<Option<ACCommitment>>,
                              transfer_to: &[(u64, &AccountAddress)],
                              output_tracing_policies: Vec<Option<AssetTracingPolicy>>,
                              output_identity_commitments: Vec<Option<ACCommitment>>)
                              -> Result<&mut Self, PlatformError> {
    // TODO(fernando): where to get prng
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_entropy();

    let input_sids: Vec<TxoRef> = transfer_from.iter()
                                               .map(|(ref txo_sid, _, _, _)| *(*txo_sid))
                                               .collect();
    let input_amounts: Vec<u64> = transfer_from.iter()
                                               .map(|(_, _, amount, _)| *amount)
                                               .collect();
    let input_oars: Result<Vec<OpenAssetRecord>, _> =
      transfer_from.iter()
                   .map(|(_, ref ba, _, owner_memo)| {
                     open_blind_asset_record(&ba, owner_memo, &key_pair.get_sk_ref())
                   })
                   .collect();
    let input_oars = input_oars.map_err(|e| PlatformError::ZeiError(error_location!(), e))?;
    let input_total: u64 = input_amounts.iter().sum();
    let mut partially_consumed_inputs = Vec::new();
    for ((input_amount, oar), input_tracing_policy) in
      input_amounts.iter()
                   .zip(input_oars.iter())
                   .zip(input_tracing_policies.iter())
    {
      if input_amount > oar.get_amount() {
        return Err(PlatformError::InputsError(error_location!()));
      } else if input_amount < oar.get_amount() {
        let mut policies = AssetTracingPolicies::new();
        if let Some(policy) = &input_tracing_policy {
          policies.add(policy.clone());
        }
        let ar = AssetRecordTemplate::with_asset_tracking(oar.get_amount() - input_amount,
                                                          *oar.get_asset_type(),
                                                          oar.get_record_type(),
                                                          *oar.get_pub_key(),
                                                          policies);
        partially_consumed_inputs.push(ar);
      }
    }
    let output_total = transfer_to.iter().fold(0, |acc, (amount, _)| acc + amount);
    if input_total != output_total {
      return Err(PlatformError::InputsError(error_location!()));
    }
    let asset_type = input_oars[0].get_asset_type();
    let asset_record_type = input_oars[0].get_record_type();
    let mut output_ars_templates = Vec::new();
    for ((amount, ref addr), output_tracing_policy) in
      transfer_to.iter().zip(output_tracing_policies.iter())
    {
      let mut policies = AssetTracingPolicies::new();
      if let Some(policy) = output_tracing_policy {
        policies.add(policy.clone())
      }
      let template = AssetRecordTemplate::with_asset_tracking(*amount,
                                                              *asset_type,
                                                              asset_record_type,
                                                              addr.key,
                                                              policies);
      output_ars_templates.push(template);
    }
    output_ars_templates.append(&mut partially_consumed_inputs);
    let output_ars: Result<Vec<AssetRecord>, _> =
      output_ars_templates.iter()
                          .map(|x| AssetRecord::from_template_no_identity_tracking(&mut prng, x))
                          .collect();
    let output_ars = output_ars.map_err(|e| PlatformError::ZeiError(error_location!(), e))?;
    self.add_operation_transfer_asset(&key_pair,
                                      input_sids,
                                      &input_oars,
                                      input_tracing_policies,
                                      input_identity_commitments,
                                      &output_ars,
                                      output_identity_commitments)?;
    Ok(self)
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransactionBuilder {
  txn: Transaction,
  outputs: u64,
}

impl TransactionBuilder {
  pub fn get_owner_memo_ref(&self, idx: usize) -> Option<&OwnerMemo> {
    self.txn.get_owner_memos_ref()[idx]
  }

  pub fn get_output_ref(&self, idx: usize) -> &TxOutput {
    self.txn.get_outputs_ref(true)[idx]
  }

  pub fn from_seq_id(seq_id: u64) -> Self {
    TransactionBuilder { txn: Transaction::from_seq_id(seq_id),
                         outputs: 0 }
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

  fn add_policy_option(&mut self, token_code: AssetTypeCode, which_check: String) -> &mut Self {
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

  fn add_operation_create_asset(&mut self,
                                key_pair: &XfrKeyPair,
                                token_code: Option<AssetTypeCode>,
                                asset_rules: AssetRules,
                                memo: &str,
                                policy_choice: PolicyChoice)
                                -> Result<&mut Self, PlatformError> {
    let token_code = match token_code {
      Some(code) => code,
      None => AssetTypeCode::gen_random(),
    };
    let pol = policy_from_choice(&token_code, key_pair.get_pk_ref(), policy_choice);
    let iss_keypair = IssuerKeyPair { keypair: &key_pair };
    self.txn.add_operation(Operation::DefineAsset(DefineAsset::new(DefineAssetBody::new(&token_code, &IssuerPublicKey { key: *key_pair.get_pk_ref() }, asset_rules, Some(Memo(memo.into())), Some(ConfidentialMemo {}), pol)?, &iss_keypair)?));

    Ok(self)
  }
  fn add_operation_issue_asset(&mut self,
                               key_pair: &XfrKeyPair,
                               token_code: &AssetTypeCode,
                               seq_num: u64,
                               records_and_memos: &[(TxOutput, Option<OwnerMemo>)])
                               -> Result<&mut Self, PlatformError> {
    let iss_keypair = IssuerKeyPair { keypair: &key_pair };

    self.txn
        .add_operation(Operation::IssueAsset(IssueAsset::new(IssueAssetBody::new(token_code,
                                                                                 seq_num,
                                                                                 &records_and_memos)?,
                                                             &iss_keypair)?));
    Ok(self)
  }

  fn add_operation_transfer_asset(&mut self,
                                  keys: &XfrKeyPair,
                                  input_sids: Vec<TxoRef>,
                                  input_records: &[OpenAssetRecord],
                                  input_tracing_policies: Vec<Option<AssetTracingPolicy>>,
                                  _input_identity_commitments: Vec<Option<ACCommitment>>,
                                  output_records: &[AssetRecord],
                                  _output_identity_commitments: Vec<Option<ACCommitment>>)
                                  -> Result<&mut Self, PlatformError> {
    // TODO(joe/noah): keep a prng around somewhere?
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_entropy();
    let mut input_asset_records = vec![];
    for (oar, tracing_policy) in input_records.iter().zip(input_tracing_policies.iter()) {
      let mut policies = AssetTracingPolicies::new();
      if let Some(policy) = tracing_policy {
        policies.add(policy.clone());
      }
      input_asset_records.push(AssetRecord::from_open_asset_record_with_asset_tracking_but_no_identity(
        oar.clone(),
        policies,

      ).map_err(|e| PlatformError::ZeiError(error_location!(), e))?);
    }

    let mut xfr = TransferAsset::new(TransferAssetBody::new(&mut prng,
                                                            input_sids,
                                                            &input_asset_records[..],
                                                            output_records,
                                                            None,
                                                            TransferType::Standard)?)?;
    xfr.sign(&keys);

    self.txn.add_operation(Operation::TransferAsset(xfr));
    Ok(self)
  }
  fn add_operation_kv_update(&mut self,
                             auth_key_pair: &XfrKeyPair,
                             index: &Key,
                             seq_num: u64,
                             hash: Option<&KVHash>)
                             -> Result<&mut Self, PlatformError> {
    let update = KVUpdate::new((*index, hash.cloned()), seq_num, auth_key_pair);
    self.txn.add_operation(Operation::KVStoreUpdate(update));
    Ok(self)
  }
  fn add_operation_air_assign(&mut self,
                              key_pair: &XfrKeyPair,
                              addr: CredUserPublicKey,
                              data: CredCommitment,
                              issuer_pk: CredIssuerPublicKey,
                              pok: CredPoK)
                              -> Result<&mut Self, PlatformError> {
    let xfr = AIRAssign::new(AIRAssignBody::new(addr, data, issuer_pk, pok)?, key_pair)?;
    self.txn.add_operation(Operation::AIRAssign(xfr));
    Ok(self)
  }

  fn add_operation_update_memo(&mut self,
                               auth_key_pair: &XfrKeyPair,
                               asset_code: AssetTypeCode,
                               new_memo: &str)
                               -> &mut Self {
    let new_memo = Memo(new_memo.into());
    let memo_update = UpdateMemo::new(UpdateMemoBody { new_memo,
                                                       asset_type: asset_code },
                                      auth_key_pair);
    let op = Operation::UpdateMemo(memo_update);
    self.add_operation(op);
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

  fn add_signature(&mut self,
                   pk: &XfrPublicKey,
                   sig: SignatureOf<TransactionBody>)
                   -> Result<&mut Self, PlatformError> {
    self.txn.check_signature(pk, &sig)?;
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
  identity_proof: Option<ConfidentialAC>)
  -> Result<(AssetRecord, (Scalar, Scalar), Scalar), PlatformError> {
  // Check input consistency:
  // - if no policy, then no identity proof needed
  // - if policy and identity tracking, then identity proof is needed
  // - if policy but no identity tracking, then no identity proof is needed
  // TODO (fernando) this code does not handle more than one policy, hence the following assert
  // REDMINE #104
  assert!(template.asset_tracing_policies.len() <= 1);
  let asset_tracing = !template.asset_tracing_policies.is_empty();
  if !asset_tracing && identity_proof.is_some()
     || asset_tracing
        && (template.asset_tracing_policies
                    .get_policy(0)
                    .as_ref()
                    .unwrap()
                    .identity_tracking
                    .is_some()
            && identity_proof.is_none()
            || template.asset_tracing_policies
                       .get_policy(0)
                       .as_ref()
                       .unwrap()
                       .identity_tracking
                       .is_none()
               && identity_proof.is_some())
  {
    return Err(PlatformError::InputsError(error_location!()));
  }
  // 1. get ciphertext and proofs from identity proof structure
  let (attr_ctext, reveal_proof) = match identity_proof {
    None => (None, None),
    Some(conf_ac) => {
      let (c, p) = conf_ac.get_fields();
      (Some(c), Some(p))
    }
  };
  // 2. Use record template and ciphertexts to build open asset record
  let params = PublicParams::new();
  let (open_asset_record, asset_tracing_memos, owner_memo) =
    build_open_asset_record(prng, &params.pc_gens, template, vec![attr_ctext]);
  // 3. Return record input containing open asset record, tracking policy, identity reveal proof,
  //    asset_tracer_memo, and owner_memo

  let mut identity_proofs = vec![];
  if reveal_proof.is_some() {
    identity_proofs.push(reveal_proof);
  }

  Ok((AssetRecord { open_asset_record: open_asset_record.clone(),
                    tracking_policies: template.asset_tracing_policies.clone(),
                    identity_proofs,
                    owner_memo,
                    asset_tracers_memos: asset_tracing_memos },
      open_asset_record.amount_blinds,
      open_asset_record.type_blind))
}

// TransferOperationBuilder constructs transfer operations using the factory pattern
// Inputs and outputs are added iteratively before being signed by all input record owners
//
// Example usage:
//
//    let alice = XfrKeyPair::generate(&mut prng);
//    let bob = XfrKeyPair::generate(&mut prng);
//
//    let ar = AssetRecord::new(1000, code_1.val, *alice.get_pk_ref()).unwrap();
//    let ba = build_blind_asset_record(&mut prng, &params.pc_gens, &ar_1, false, false, &None);
//
//    let builder = TransferOperationBuilder::new()..add_input(TxoRef::Relative(1),
//                                       open_blind_asset_record(&ba, alice.get_sk_ref()).unwrap(),
//                                       None,
//                                       20)?
//                            .add_output(20, bob.get_pk_ref(), code_1)?
//                            .balance()?
//                            .create(TransferType::Standard)?
//                            .sign(&alice)?;
//
#[derive(Serialize, Deserialize, Default)]
pub struct TransferOperationBuilder {
  input_sids: Vec<TxoRef>,
  spend_amounts: Vec<u64>, // Amount of each input record to spend, the rest will be refunded if user calls balance
  input_records: Vec<AssetRecord>,
  inputs_tracing_policies: Vec<AssetTracingPolicies>,
  input_identity_commitments: Vec<Option<ACCommitment>>,
  output_records: Vec<AssetRecord>,
  outputs_tracing_policies: Vec<AssetTracingPolicies>,
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
  pub fn add_input(&mut self,
                   txo_sid: TxoRef,
                   open_ar: OpenAssetRecord,
                   tracing_policies: Option<AssetTracingPolicies>,
                   identity_commitment: Option<ACCommitment>,
                   amount: u64)
                   -> Result<&mut Self, PlatformError> {
    if self.transfer.is_some() {
      return Err(inv_fail!("Cannot mutate a transfer that has been signed".to_string()));
    }
    let policies = tracing_policies.unwrap_or_default();

    let asset_record =
      AssetRecord::from_open_asset_record_with_asset_tracking_but_no_identity(
        open_ar,
        policies.clone())
        .map_err(|e| PlatformError::ZeiError(error_location!(), e))?;
    self.input_sids.push(txo_sid);
    self.input_records.push(asset_record);
    self.inputs_tracing_policies.push(policies);
    self.input_identity_commitments.push(identity_commitment);
    self.spend_amounts.push(amount);
    Ok(self)
  }

  pub fn add_output(&mut self,
                    asset_record_template: &AssetRecordTemplate,
                    tracing_policies: Option<AssetTracingPolicies>,
                    identity_commitment: Option<ACCommitment>,
                    credential_record: Option<(&CredUserSecretKey,
                            &Credential,
                            &ACCommitmentKey)>)
                    -> Result<&mut Self, PlatformError> {
    let prng = &mut ChaChaRng::from_entropy();
    if self.transfer.is_some() {
      return Err(inv_fail!("Cannot mutate a transfer that has been signed".to_string()));
    }
    let policies = tracing_policies.unwrap_or_default();
    let ar = if let Some((user_secret_key, credential, commitment_key)) = credential_record {
      AssetRecord::from_template_with_identity_tracking(prng,
                                                        asset_record_template,
                                                        user_secret_key.get_ref(),
                                                        credential,
                                                        commitment_key).unwrap()
    } else {
      AssetRecord::from_template_no_identity_tracking(prng, asset_record_template).unwrap()
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
    blinds: &mut ((Scalar, Scalar), Scalar))
    -> Result<&mut Self, PlatformError> {
    if self.transfer.is_some() {
      return Err(inv_fail!("Cannot mutate a transfer that has been signed".to_string()));
    }
    let (ar, amount_blinds, type_blind) =
      if let Some((user_secret_key, credential, commitment_key)) = credential_record {
        match asset_record_template.asset_tracing_policies.get_policy(0) {
          None => {
            // identity tracking must have asset_tracking policy
            return Err(PlatformError::InputsError(error_location!()));
          }
          Some(policy) => {
            match &policy.identity_tracking {
              // policy must have a identity tracking policy
              None => {
                return Err(PlatformError::InputsError(error_location!()));
              }
              Some(reveal_policy) => {
                let conf_ac =
                  ac_confidential_open_commitment(prng,
                                                  user_secret_key.get_ref(),
                                                  credential,
                                                  commitment_key,
                                                  &policy.enc_keys.attrs_enc_key,
                                                  &reveal_policy.reveal_map,
                                                  &[]).map_err(|e| {
                                                        PlatformError::ZeiError(error_location!(),
                                                                                e)
                                                      })?;
                build_record_and_get_blinds(prng, &asset_record_template, Some(conf_ac))?
              }
            }
          }
        }
      } else {
        if let Some(policy) = asset_record_template.asset_tracing_policies.get_policy(0) {
          if policy.identity_tracking.is_some() {
            return Err(PlatformError::InputsError(error_location!()));
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
  pub fn balance(&mut self) -> Result<&mut Self, PlatformError> {
    let mut prng = ChaChaRng::from_entropy();
    if self.transfer.is_some() {
      return Err(inv_fail!("Cannot mutate a transfer that has been signed".to_string()));
    }
    let spend_total: u64 = self.spend_amounts.iter().sum();
    let mut partially_consumed_inputs = Vec::new();
    for ((spend_amount, ar), policies) in self.spend_amounts
                                              .iter()
                                              .zip(self.input_records.iter())
                                              .zip(self.inputs_tracing_policies.iter())
    {
      let amt = ar.open_asset_record.get_amount();
      match spend_amount.cmp(&amt) {
        Ordering::Greater => {
          return Err(PlatformError::InputsError(error_location!()));
        }
        Ordering::Less => {
          let asset_type = *ar.open_asset_record.get_asset_type();
          let record_type = ar.open_asset_record.get_record_type();
          let recipient = *ar.open_asset_record.get_pub_key();
          let ar_template = AssetRecordTemplate::with_asset_tracking(amt - spend_amount,
                                                                     asset_type,
                                                                     record_type,
                                                                     recipient,
                                                                     policies.clone());
          let ar =
            AssetRecord::from_template_no_identity_tracking(&mut prng, &ar_template).unwrap();
          partially_consumed_inputs.push(ar);
          self.outputs_tracing_policies.push(policies.clone());
          self.output_identity_commitments.push(None);
        }
        _ => {}
      }
    }
    let output_total = self.output_records
                           .iter()
                           .fold(0, |acc, ar| acc + ar.open_asset_record.amount);
    if spend_total != output_total {
      return Err(PlatformError::InputsError(error_location!()));
    }
    self.output_records.append(&mut partially_consumed_inputs);
    Ok(self)
  }

  // Finalize the transaction and prepare for signing. Once called, the transaction cannot be
  // modified.
  pub fn create(&mut self, transfer_type: TransferType) -> Result<&mut Self, PlatformError> {
    let mut prng = ChaChaRng::from_entropy();
    let num_inputs = self.input_records.len();
    let num_outputs = self.output_records.len();
    let xfr_policies = XfrNotePolicies::new(self.inputs_tracing_policies.clone(),
                                            vec![None; num_inputs],
                                            self.outputs_tracing_policies.clone(),
                                            vec![None; num_outputs]);
    let body = TransferAssetBody::new(&mut prng,
                                      self.input_sids.clone(),
                                      &self.input_records,
                                      &self.output_records,
                                      Some(xfr_policies),
                                      transfer_type)?;
    self.transfer = Some(TransferAsset::new(body)?);
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
  pub fn sign(&mut self, kp: &XfrKeyPair) -> Result<&mut Self, PlatformError> {
    if self.transfer.is_none() {
      return Err(no_transfer_err!());
    }
    self.transfer.as_mut().unwrap().sign(&kp);
    Ok(self)
  }

  pub fn create_input_signature(&self,
                                keypair: &XfrKeyPair)
                                -> Result<TransferBodySignature, PlatformError> {
    let sig = self.transfer
                  .as_ref()
                  .ok_or_else(|| no_transfer_err!())?
                  .create_input_signature(keypair);
    Ok(sig)
  }

  pub fn create_cosignature(&self,
                            keypair: &XfrKeyPair,
                            input_idx: usize)
                            -> Result<TransferBodySignature, PlatformError> {
    let sig = self.transfer
                  .as_ref()
                  .ok_or_else(|| no_transfer_err!())?
                  .create_cosignature(keypair, input_idx);
    Ok(sig)
  }

  pub fn attach_signature(&mut self,
                          sig: TransferBodySignature)
                          -> Result<&mut Self, PlatformError> {
    self.transfer
        .as_mut()
        .ok_or_else(|| no_transfer_err!())?
        .attach_signature(sig)?;
    Ok(self)
  }

  // Add a co-signature for an input.
  pub fn sign_cosignature(&mut self,
                          kp: &XfrKeyPair,
                          input_idx: usize)
                          -> Result<&mut Self, PlatformError> {
    let mut new_transfer = self.transfer
                               .as_mut()
                               .ok_or_else(|| no_transfer_err!())?
                               .clone();
    new_transfer.sign_cosignature(&kp, input_idx);
    Ok(self)
  }

  // Return the transaction operation
  pub fn transaction(&self) -> Result<Operation, PlatformError> {
    if self.transfer.is_none() {
      return Err(no_transfer_err!());
    }
    Ok(Operation::TransferAsset(self.transfer.clone().unwrap()))
  }

  // Checks to see whether all necessary signatures are present and valid
  pub fn validate_signatures(&mut self) -> Result<&mut Self, PlatformError> {
    if self.transfer.is_none() {
      return Err(no_transfer_err!());
    }

    let trn = self.transfer.as_ref().unwrap();
    let mut sig_keys = HashSet::new();
    for sig in &trn.body_signatures {
      if !sig.verify(&trn.body) {
        return Err(inv_fail!("Invalid signature".to_string()));
      }
      sig_keys.insert(sig.address.key.zei_to_bytes());
    }

    for record in &trn.body.transfer.inputs {
      if !sig_keys.contains(&record.public_key.zei_to_bytes()) {
        return Err(inv_fail!("Not all signatures present".to_string()));
      }
    }
    Ok(self)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::TxoRef;
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
  fn test_transfer_op_builder() -> Result<(), PlatformError> {
    let mut prng = ChaChaRng::from_entropy();
    let params = PublicParams::new();
    let code_1 = AssetTypeCode::gen_random();
    let code_2 = AssetTypeCode::gen_random();
    let alice = XfrKeyPair::generate(&mut prng);
    let bob = XfrKeyPair::generate(&mut prng);
    let charlie = XfrKeyPair::generate(&mut prng);
    let ben = XfrKeyPair::generate(&mut prng);

    let ar_1 =
      AssetRecordTemplate::with_no_asset_tracking(1000,
                                                  code_1.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  alice.get_pk());
    let ar_2 =
      AssetRecordTemplate::with_no_asset_tracking(1000,
                                                  code_2.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  bob.get_pk());
    let (ba_1, _, memo1) = build_blind_asset_record(&mut prng, &params.pc_gens, &ar_1, vec![]);
    let (ba_2, _, memo2) = build_blind_asset_record(&mut prng, &params.pc_gens, &ar_2, vec![]);

    // Attempt to spend too much
    let mut invalid_outputs_transfer_op = TransferOperationBuilder::new();
    let output_template =
      AssetRecordTemplate::with_no_asset_tracking(25,
                                                  code_1.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  bob.get_pk());
    let res =
      invalid_outputs_transfer_op.add_input(TxoRef::Relative(1),
                                            open_blind_asset_record(&ba_1,
                                                                    &memo1,
                                                                    alice.get_sk_ref()).unwrap(),
                                            None,
                                            None,
                                            20)?
                                 .add_output(&output_template, None, None, None)?
                                 .balance();

    assert!(res.is_err());

    // Change transaction after signing
    let mut invalid_sig_op = TransferOperationBuilder::new();
    let output_template =
      AssetRecordTemplate::with_no_asset_tracking(20,
                                                  code_1.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  bob.get_pk());
    let res = invalid_sig_op.add_input(TxoRef::Relative(1),
                                       open_blind_asset_record(&ba_1, &memo1,alice.get_sk_ref()).unwrap(),
                                       None,
                                       None,
                                       20)?
                            .add_output(&output_template, None, None, None)?
                            .balance()?
                            .create(TransferType::Standard)?
                            .sign(&alice)?
                            .add_output(&output_template, None, None, None);
    assert!(res.is_err());

    // Not all signatures present
    let mut missing_sig_op = TransferOperationBuilder::new();
    let output_template =
      AssetRecordTemplate::with_no_asset_tracking(20,
                                                  code_1.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  bob.get_pk());
    let res = missing_sig_op.add_input(TxoRef::Relative(1),
                                       open_blind_asset_record(&ba_1, &memo1,alice.get_sk_ref()).unwrap(),
                                       None,
                                       None,
                                       20)?
                            .add_output(&output_template, None, None, None)?
                            .balance()?
                            .create(TransferType::Standard)?
                            .validate_signatures();

    assert!(&res.is_err());

    // Finally, test a valid transfer
    let output_bob5_code1_template =
      AssetRecordTemplate::with_no_asset_tracking(5,
                                                  code_1.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  bob.get_pk());
    let output_charlie13_code1_template =
      AssetRecordTemplate::with_no_asset_tracking(13,
                                                  code_1.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  charlie.get_pk());
    let output_ben2_code1_template =
      AssetRecordTemplate::with_no_asset_tracking(2,
                                                  code_1.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  ben.get_pk());
    let output_bob5_code2_template =
      AssetRecordTemplate::with_no_asset_tracking(5,
                                                  code_2.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  bob.get_pk());
    let output_charlie13_code2_template =
      AssetRecordTemplate::with_no_asset_tracking(13,
                                                  code_2.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  charlie.get_pk());
    let output_ben2_code2_template =
      AssetRecordTemplate::with_no_asset_tracking(2,
                                                  code_2.val,
                                                  NonConfidentialAmount_NonConfidentialAssetType,
                                                  ben.get_pk());
    let _valid_transfer_op =
      TransferOperationBuilder::new()
      .add_input(TxoRef::Relative(1), open_blind_asset_record(&ba_1, &memo1, alice.get_sk_ref()).unwrap(), None, None, 20)?
      .add_input(TxoRef::Relative(2), open_blind_asset_record(&ba_2, &memo2, bob.get_sk_ref()).unwrap(), None, None, 20)?
      .add_output(&output_bob5_code1_template, None, None, None)?
      .add_output(&output_charlie13_code1_template, None, None, None)?
      .add_output(&output_ben2_code1_template, None, None, None)?
      .add_output(&output_bob5_code2_template, None, None, None)?
      .add_output(&output_charlie13_code2_template, None, None, None)?
      .add_output(&output_ben2_code2_template, None, None, None)?
      .balance()?
      .create(TransferType::Standard)?
      .sign(&alice)?
      .sign(&bob)?
      .transaction()?;
    Ok(())
  }
}
