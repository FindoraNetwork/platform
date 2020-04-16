#![deny(warnings)]
extern crate ledger;
extern crate serde;
extern crate zei;
#[macro_use]
extern crate serde_derive;

use credentials::{CredCommitment, CredIssuerPublicKey, CredPoK, CredUserSecretKey};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::*;
use ledger::error_location;
use ledger::policies::Fraction;
use ledger::policy_script::{Policy, PolicyGlobals, TxnCheckInputs, TxnPolicyData};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::cmp::Ordering;
use std::collections::HashSet;
use zei::api::anon_creds::{ACCommitmentKey, Credential};
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{
  AssetRecord, AssetRecordTemplate, AssetTracingPolicy, BlindAssetRecord, OpenAssetRecord,
  OwnerMemo,
};

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
                               records: &[(TxOutput, Option<OwnerMemo>)],
                               tracing_policy: Option<AssetTracingPolicy>)
                               -> Result<&mut Self, PlatformError>;
  fn add_operation_transfer_asset(&mut self,
                                  keys: &XfrKeyPair,
                                  input_sids: Vec<TxoRef>,
                                  input_records: &[OpenAssetRecord],
                                  output_records: &[AssetRecord])
                                  -> Result<&mut Self, PlatformError>;
  fn add_operation_air_assign(&mut self,
                              key_pair: &XfrKeyPair,
                              addr: CredIssuerPublicKey,
                              data: CredCommitment,
                              pok: CredPoK)
                              -> Result<&mut Self, PlatformError>;
  fn serialize(&self) -> Result<Vec<u8>, PlatformError>;
  fn serialize_str(&self) -> Result<String, PlatformError>;

  fn add_operation(&mut self, op: Operation) -> &mut Self;

  fn add_basic_issue_asset(&mut self,
                           key_pair: &XfrKeyPair,
                           tracing_policy: Option<AssetTracingPolicy>,
                           token_code: &AssetTypeCode,
                           seq_num: u64,
                           amount: u64,
                           confidentiality_flags: AssetRecordType)
                           -> Result<&mut Self, PlatformError> {
    let mut prng = ChaChaRng::from_entropy();
    let params = PublicParams::new();
    let ar = match tracing_policy.clone() {
      Some(policy) => AssetRecordTemplate::with_asset_tracking(amount,
                                                               token_code.val,
                                                               confidentiality_flags,
                                                               key_pair.get_pk(),
                                                               policy),
      None => AssetRecordTemplate::with_no_asset_tracking(amount,
                                                          token_code.val,
                                                          confidentiality_flags,
                                                          key_pair.get_pk()),
    };
    let (ba, _, owner_memo) = build_blind_asset_record(&mut prng, &params.pc_gens, &ar, None);
    self.add_operation_issue_asset(key_pair,
                                   token_code,
                                   seq_num,
                                   &[(TxOutput(ba), owner_memo)],
                                   tracing_policy)
  }

  #[allow(clippy::comparison_chain)]
  fn add_basic_transfer_asset(&mut self,
                              key_pair: &XfrKeyPair,
                              tracing_policy: &Option<AssetTracingPolicy>,
                              transfer_from: &[(&TxoRef,
                                 &BlindAssetRecord,
                                 u64,
                                 &Option<OwnerMemo>)],
                              transfer_to: &[(u64, &AccountAddress)])
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
    let input_oars = input_oars?;
    let input_total: u64 = input_amounts.iter().sum();
    let mut partially_consumed_inputs = Vec::new();
    for (input_amount, oar) in input_amounts.iter().zip(input_oars.iter()) {
      if input_amount > oar.get_amount() {
        return Err(PlatformError::InputsError(error_location!()));
      } else if input_amount < oar.get_amount() {
        let ar = match tracing_policy {
          Some(policy) => AssetRecordTemplate::with_asset_tracking(oar.get_amount() - input_amount,
                                                                   *oar.get_asset_type(),
                                                                   oar.get_record_type(),
                                                                   *oar.get_pub_key(),
                                                                   policy.clone()),
          _ => AssetRecordTemplate::with_no_asset_tracking(oar.get_amount() - input_amount,
                                                           *oar.get_asset_type(),
                                                           oar.get_record_type(),
                                                           *oar.get_pub_key()),
        };
        partially_consumed_inputs.push(ar);
      }
    }
    let output_total = transfer_to.iter().fold(0, |acc, (amount, _)| acc + amount);
    if input_total != output_total {
      return Err(PlatformError::InputsError(error_location!()));
    }
    let asset_type = input_oars[0].get_asset_type();
    let asset_record_type = input_oars[0].get_record_type();
    let mut output_ars_templates: Vec<AssetRecordTemplate> =
      transfer_to.iter()
                 .map(|(amount, ref addr)| match tracing_policy {
                   Some(policy) => AssetRecordTemplate::with_asset_tracking(*amount,
                                                                            *asset_type,
                                                                            asset_record_type,
                                                                            addr.key,
                                                                            policy.clone()),
                   _ => AssetRecordTemplate::with_no_asset_tracking(*amount,
                                                                    *asset_type,
                                                                    asset_record_type,
                                                                    addr.key),
                 })
                 .collect();
    output_ars_templates.append(&mut partially_consumed_inputs);
    let output_ars: Result<Vec<AssetRecord>, _> =
      output_ars_templates.iter()
                          .map(|x| AssetRecord::from_template_no_identity_tracking(&mut prng, x))
                          .collect();
    let output_ars = output_ars?;
    self.add_operation_transfer_asset(&key_pair, input_sids, &input_oars, &output_ars)?;
    Ok(self)
  }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct TransactionBuilder {
  txn: Transaction,
  owner_records: Vec<(TxOutput, Option<OwnerMemo>)>,
  outputs: u64,
}

impl TransactionBuilder {
  pub fn get_owner_record_and_memo(&self, idx: usize) -> Option<&(TxOutput, Option<OwnerMemo>)> {
    self.owner_records.get(idx)
  }
}

impl BuildsTransactions for TransactionBuilder {
  fn transaction(&self) -> &Transaction {
    &self.txn
  }
  fn add_memo(&mut self, memo: Memo) -> &mut Self {
    self.txn.memos.push(memo);
    self
  }

  fn add_policy_option(&mut self, token_code: AssetTypeCode, which_check: String) -> &mut Self {
    if self.txn.policy_options.is_none() {
      self.txn.policy_options = Some(TxnPolicyData(vec![]));
    }
    self.txn
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
    let pub_key = &IssuerPublicKey { key: key_pair.get_pk() };
    let priv_key = &key_pair.get_sk();
    let token_code = token_code.unwrap_or_else(AssetTypeCode::gen_random);
    self.txn.add_operation(Operation::DefineAsset(DefineAsset::new(DefineAssetBody::new(&token_code, pub_key, asset_rules, Some(Memo(memo.into())), Some(ConfidentialMemo {}), policy_from_choice(&token_code,&pub_key.key,policy_choice))?, pub_key, priv_key)?));
    Ok(self)
  }
  fn add_operation_issue_asset(&mut self,
                               key_pair: &XfrKeyPair,
                               token_code: &AssetTypeCode,
                               seq_num: u64,
                               records_and_memos: &[(TxOutput, Option<OwnerMemo>)],
                               tracing_policy: Option<AssetTracingPolicy>)
                               -> Result<&mut Self, PlatformError> {
    let pub_key = &IssuerPublicKey { key: key_pair.get_pk() };
    let priv_key = &key_pair.get_sk();
    let mut records = vec![];
    for (output, memo) in records_and_memos {
      records.push(output.clone());
      self.owner_records.push((output.clone(), memo.clone()));
    }
    self.txn
        .add_operation(Operation::IssueAsset(IssueAsset::new(IssueAssetBody::new(token_code,
                                                                                 seq_num,
                                                                                 &records,
                                                                                 tracing_policy)?,
                                                             pub_key,
                                                             priv_key)?));
    Ok(self)
  }
  fn add_operation_transfer_asset(&mut self,
                                  keys: &XfrKeyPair,
                                  input_sids: Vec<TxoRef>,
                                  input_records: &[OpenAssetRecord],
                                  output_records: &[AssetRecord])
                                  -> Result<&mut Self, PlatformError> {
    // TODO(joe/noah): keep a prng around somewhere?
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_entropy();
    let mut xfr = TransferAsset::new(TransferAssetBody::new(&mut prng,
                                                            input_sids,
                                                            input_records,
                                                            output_records)?,
                                     TransferType::Standard)?;
    for (ix, _) in input_records.iter().enumerate() {
      xfr.sign(&keys, ix);
    }

    for (output, memo) in xfr.body
                             .transfer
                             .outputs
                             .iter()
                             .zip(xfr.body.transfer.owners_memos.iter())
    {
      self.owner_records
          .push((TxOutput(output.clone()), memo.clone()));
    }
    self.txn.add_operation(Operation::TransferAsset(xfr));
    Ok(self)
  }
  fn add_operation_air_assign(&mut self,
                              key_pair: &XfrKeyPair,
                              addr: CredIssuerPublicKey,
                              data: CredCommitment,
                              pok: CredPoK)
                              -> Result<&mut Self, PlatformError> {
    let xfr = AIRAssign::new(AIRAssignBody::new(addr, data, pok)?, key_pair)?;
    self.txn.add_operation(Operation::AIRAssign(xfr));
    Ok(self)
  }

  fn add_operation(&mut self, op: Operation) -> &mut Self {
    self.txn.add_operation(op);
    self
  }

  fn sign(&mut self, kp: &XfrKeyPair) -> &mut Self {
    self.txn.sign(kp.get_sk_ref(), kp.get_pk_ref());
    self
  }

  fn serialize(&self) -> Result<Vec<u8>, PlatformError> {
    let j = serde_json::to_string(&self.txn)?;
    Ok(j.as_bytes().to_vec())
  }

  fn serialize_str(&self) -> Result<String, PlatformError> {
    if let Ok(serialized) = serde_json::to_string(&self.txn) {
      Ok(serialized)
    } else {
      Err(PlatformError::SerializationError)
    }
  }
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
//                                       20)?
//                            .add_output(20, bob.get_pk_ref(), code_1)?
//                            .balance()?
//                            .create(TransferType::Standard)?
//                            .sign(&alice)?;
//
#[derive(Serialize, Deserialize, Default)]
pub struct TransferOperationBuilder {
  input_sids: Vec<TxoRef>,
  input_records: Vec<OpenAssetRecord>,
  spend_amounts: Vec<u64>, //Amount of each input record to spend, the rest will be refunded
  output_records: Vec<AssetRecord>,
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
                   amount: u64)
                   -> Result<&mut Self, PlatformError> {
    if self.transfer.is_some() {
      return Err(PlatformError::InvariantError(Some("Cannot mutate a transfer that has been signed".to_string())));
    }
    self.input_sids.push(txo_sid);
    self.input_records.push(open_ar);
    self.spend_amounts.push(amount);
    Ok(self)
  }

  pub fn add_output(&mut self,
                    asset_record_template: &AssetRecordTemplate,
                    credential_record: Option<(&CredUserSecretKey,
                            &Credential,
                            &ACCommitmentKey)>)
                    -> Result<&mut Self, PlatformError> {
    let mut prng = ChaChaRng::from_entropy();
    if self.transfer.is_some() {
      return Err(PlatformError::InvariantError(Some("Cannot mutate a transfer that has been signed".to_string())));
    }
    let ar = if let Some((user_secret_key, credential, commitment_key)) = credential_record {
      AssetRecord::from_template_with_identity_tracking(&mut prng,
                                                        asset_record_template,
                                                        user_secret_key.get_ref(),
                                                        credential,
                                                        commitment_key).unwrap()
    } else {
      AssetRecord::from_template_no_identity_tracking(&mut prng, asset_record_template).unwrap()
    };
    self.output_records.push(ar);
    Ok(self)
  }

  // Ensures that outputs and inputs are balanced by adding remainder outputs for leftover asset
  // amounts
  pub fn balance(&mut self) -> Result<&mut Self, PlatformError> {
    let mut prng = ChaChaRng::from_entropy();
    if self.transfer.is_some() {
      return Err(PlatformError::InvariantError(Some("Cannot mutate a transfer that has been signed".to_string())));
    }
    let spend_total: u64 = self.spend_amounts.iter().sum();
    let mut partially_consumed_inputs = Vec::new();
    for (spend_amount, oar) in self.spend_amounts.iter().zip(self.input_records.iter()) {
      match spend_amount.cmp(oar.get_amount()) {
        Ordering::Greater => {
          return Err(PlatformError::InputsError(error_location!()));
        }
        Ordering::Less => {
          let ar_template = AssetRecordTemplate::with_no_asset_tracking(oar.get_amount()
                                                                        - spend_amount,
                                                                        *oar.get_asset_type(),
                                                                        oar.get_record_type(),
                                                                        *oar.get_pub_key());
          let ar =
            AssetRecord::from_template_no_identity_tracking(&mut prng, &ar_template).unwrap();
          partially_consumed_inputs.push(ar);
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
    let body = TransferAssetBody::new(&mut prng,
                                      self.input_sids.clone(),
                                      &self.input_records,
                                      &self.output_records)?;
    self.transfer = Some(TransferAsset::new(body, transfer_type)?);
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
  pub fn sign(&mut self, kp: &XfrKeyPair, input_idx: usize) -> Result<&mut Self, PlatformError> {
    if self.transfer.is_none() {
      return Err(PlatformError::InvariantError(Some("Transaction has not yet been finalized".to_string())));
    }
    let mut new_transfer = self.transfer.as_ref().unwrap().clone();
    new_transfer.sign(&kp, input_idx);
    self.transfer = Some(new_transfer);
    Ok(self)
  }

  // Return the transaction operation
  pub fn transaction(&self) -> Result<Operation, PlatformError> {
    if self.transfer.is_none() {
      return Err(PlatformError::InvariantError(Some("Must create transfer".to_string())));
    }
    Ok(Operation::TransferAsset(self.transfer.clone().unwrap()))
  }

  // Checks to see whether all necessary signatures are present and valid
  pub fn validate_signatures(&mut self) -> Result<&mut Self, PlatformError> {
    if self.transfer.is_none() {
      return Err(PlatformError::InvariantError(Some("Transaction has not yet been finalized".to_string())));
    }

    let trn = self.transfer.as_ref().unwrap();
    let mut sig_keys = HashSet::new();
    for sig in &trn.body_signatures {
      if !sig.verify(&serde_json::to_vec(&trn.body).unwrap()) {
        return Err(PlatformError::InvariantError(Some("Invalid signature".to_string())));
      }
      sig_keys.insert(sig.address.key.zei_to_bytes());
    }

    for record in &trn.body.transfer.inputs {
      if !sig_keys.contains(&record.public_key.zei_to_bytes()) {
        return Err(PlatformError::InvariantError(Some("Not all signatures present".to_string())));
      }
    }
    Ok(self)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::TxoRef;
  use quickcheck::{Arbitrary, Gen};
  use quickcheck_macros::quickcheck;
  use rand::Rng;
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use zei::serialization::ZeiFromToBytes;
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
  use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record};
  use zei::xfr::lib::{gen_xfr_note, verify_xfr_note_no_policies};
  use zei::xfr::sig::XfrKeyPair;
  use zei::xfr::structs::AssetRecord;

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

  impl Arbitrary for OutputRecord {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      OutputRecord(u64::arbitrary(g),
                   AssetType::arbitrary(g),
                   KeyPair::arbitrary(g))
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0
                   .shrink()
                   .zip(self.1.shrink())
                   .zip(self.2.shrink())
                   .map(|((amount, asset_type), key_pair)| {
                     OutputRecord(amount, asset_type, key_pair)
                   }))
    }
  }

  impl Arbitrary for InputRecord {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      InputRecord(u64::arbitrary(g),
                  AssetType::arbitrary(g),
                  bool::arbitrary(g),
                  bool::arbitrary(g),
                  bool::arbitrary(g))
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0
                   .shrink()
                   .zip(self.1.shrink())
                   .zip(self.2.shrink())
                   .zip(self.3.shrink())
                   .zip(self.4.shrink())
                   .map(|((((amount, asset_type), conf_type), conf_amount), traceable)| {
                          InputRecord(amount, asset_type, conf_type, conf_amount, traceable)
                        }))
    }
  }

  impl Arbitrary for AssetType {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      AssetType(u8::arbitrary(g))
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0.shrink().map(AssetType))
    }
  }

  impl Arbitrary for TxoReference {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      TxoReference(g.gen::<u64>() % 10)
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0.shrink().map(TxoReference))
    }
  }

  impl Arbitrary for KeyPair {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      // We can generate 10 possible key pairs
      KeyPair(g.gen::<u8>() % 10)
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0.shrink().map(KeyPair))
    }
  }

  #[quickcheck]
  #[ignore]
  fn test_compose_transfer_txn(inputs: Vec<InputRecord>,
                               outputs: Vec<OutputRecord>,
                               key_pair: KeyPair,
                               input_sids: Vec<TxoReference>) {
    let mut prng = ChaChaRng::from_entropy();

    //TODO: noah asset records should be buildable by reference
    let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_seed([key_pair.0; 32]));
    let key_pair_copy = XfrKeyPair::zei_from_bytes(&key_pair.zei_to_bytes());

    // Compose input records
    let input_records: Result<Vec<AssetRecord>, _> =
      inputs.iter()
            .map(|InputRecord(amount, asset_type, _conf_type, _conf_amount, _)| {
                   let template = AssetRecordTemplate::with_no_asset_tracking(*amount,
                                             [asset_type.0; 16],
                                             NonConfidentialAmount_NonConfidentialAssetType,
                                             key_pair_copy.get_pk());
                   AssetRecord::from_template_no_identity_tracking(&mut prng, &template)
                 })
            .collect();

    // Compose output records
    let output_records: Result<Vec<AssetRecord>, _> =
      outputs.iter()
             .map(|OutputRecord(amount, asset_type, key_pair)| {
               let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_seed([key_pair.0; 32]));
               let template = AssetRecordTemplate::with_no_asset_tracking(*amount, [asset_type.0; 16], NonConfidentialAmount_NonConfidentialAssetType,  key_pair.get_pk());
               AssetRecord::from_template_no_identity_tracking(&mut prng, &template)
             })
             .collect();

    let _input_sids: Vec<TxoRef> = input_sids.iter()
                                             .map(|TxoReference(sid)| TxoRef::Relative(*sid))
                                             .collect();
    let note = gen_xfr_note(&mut prng,
                            &input_records.unwrap(),
                            &output_records.unwrap(),
                            &[&key_pair]);
    if let Ok(xfr_note) = note {
      assert!(verify_xfr_note_no_policies(&mut prng, &xfr_note).is_ok())
    }
  }

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
    let (ba_1, _, memo1) = build_blind_asset_record(&mut prng, &params.pc_gens, &ar_1, None);
    let (ba_2, _, memo2) = build_blind_asset_record(&mut prng, &params.pc_gens, &ar_2, None);

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
                                            20)?
                                 .add_output(&output_template, None)?
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
                                       20)?
                            .add_output(&output_template, None)?
                            .balance()?
                            .create(TransferType::Standard)?
                            .sign(&alice, 0)?
                            .add_output(&output_template, None);
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
                                       20)?
                            .add_output(&output_template, None)?
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
      .add_input(TxoRef::Relative(1), open_blind_asset_record(&ba_1, &memo1, alice.get_sk_ref()).unwrap(), 20)?
      .add_input(TxoRef::Relative(2), open_blind_asset_record(&ba_2, &memo2, bob.get_sk_ref()).unwrap(), 20)?
      .add_output(&output_bob5_code1_template, None)?
      .add_output(&output_charlie13_code1_template, None)?
      .add_output(&output_ben2_code1_template, None)?
      .add_output(&output_bob5_code2_template, None)?
      .add_output(&output_charlie13_code2_template, None)?
      .add_output(&output_ben2_code2_template, None)?
      .balance()?
      .create(TransferType::Standard)?
      .sign(&alice, 0)?
      .sign(&bob, 0)?
      .transaction()?;
    Ok(())
  }
}
