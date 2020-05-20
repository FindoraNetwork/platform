#![deny(warnings)]
use crate::data_model::errors::PlatformError;
use crate::data_model::{Asset, AssetTypeCode, Operation, Transaction, TxOutput};
use crate::error_location;
use crate::policies::Fraction;
use fixed::types::I20F12;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::AssetType;

macro_rules! fail {
  () => {
    PlatformError::PolicyFailureError(error_location!())
  };
  ($s:expr) => {
    PlatformError::PolicyFailureError(format!("[{}] {}", &error_location!(), &$s))
  };
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct IdVar(pub u64);
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct AmountVar(pub u64);
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct FractionVar(pub u64);
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ResourceTypeVar(pub u64);
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ResourceVar(pub u64);
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct BoolVar(pub u64);

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum IdOp {
  OwnerOf(ResourceVar),
  Var(IdVar),
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum AmountOp {
  Var(AmountVar),
  Const(u64),
  AmountOf(ResourceVar),
  Plus(AmountVar, AmountVar),
  Minus(AmountVar, AmountVar),
  Times(AmountVar, AmountVar),
  Round(FractionVar),
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum FractionOp {
  Var(FractionVar),
  Const(Fraction),
  Plus(FractionVar, FractionVar),
  Times(FractionVar, FractionVar),
  AmtTimes(AmountVar, FractionVar),
  TimesAmt(FractionVar, AmountVar),
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum ResourceTypeOp {
  Var(ResourceTypeVar),
  TypeOfResource(ResourceVar),
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum BoolOp {
  Const(bool),
  IdEq(IdVar, IdVar),
  AmtEq(AmountVar, AmountVar),
  FracEq(FractionVar, FractionVar),
  ResourceTypeEq(ResourceTypeVar, ResourceTypeVar),

  Not(BoolVar),
  And(BoolVar, BoolVar),
  Or(BoolVar, BoolVar),

  AmtGe(AmountVar, AmountVar),
  FracGe(FractionVar, FractionVar),
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum TxnOp {
  Issue(AmountVar, ResourceTypeVar, ResourceVar),
  Transfer(AmountVar, ResourceVar, Option<ResourceVar>), // None is a burn address
}

// TxnOps, but corresponding correctly to how `Op`s work on the ledger
// ie, multi-input multi-output
#[derive(Clone, Debug)]
pub enum RealTxnOp {
  Issue(ResourceTypeVar, Vec<(AmountVar, ResourceVar)>),
  Transfer(Vec<(AmountVar, ResourceVar, Option<ResourceVar>)>), // None is a burn address
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct TxnCheck {
  pub name: String,
  pub in_params: Vec<ResourceTypeVar>,
  pub out_params: Vec<ResourceTypeVar>,
  pub id_ops: Vec<IdOp>,
  pub rt_ops: Vec<ResourceTypeOp>,

  // These actually need to execute back & forth, which feels like maybe
  // a problem
  pub fraction_ops: Vec<FractionOp>,
  pub amount_ops: Vec<AmountOp>,

  pub bool_ops: Vec<BoolOp>,
  pub assertions: Vec<BoolVar>,
  pub required_signatures: Vec<IdVar>,

  pub txn_template: Vec<TxnOp>,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct Policy {
  // Implicitly: bound asset type and issuer as AssetType & Id globals
  // respectively
  pub num_id_globals: usize,
  pub num_rt_globals: usize,
  pub num_amt_globals: usize,
  pub num_frac_globals: usize,

  pub init_check: TxnCheck,

  pub txn_choices: Vec<TxnCheck>,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct TxnCheckInputs {
  pub which_check: String,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct PolicyGlobals {
  pub id_vars: Vec<XfrPublicKey>,
  pub rt_vars: Vec<AssetType>,
  pub amt_vars: Vec<u64>,
  pub frac_vars: Vec<Fraction>,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct TxnPolicyData(pub Vec<(AssetTypeCode, TxnCheckInputs)>);

pub fn policy_get_globals(asset: &Asset) -> Result<PolicyGlobals, PlatformError> {
  let (pol, mem) = asset.policy
                        .as_ref()
                        .ok_or_else(|| PlatformError::InputsError(error_location!()))?;

  let ret = mem.clone();

  // let ret = serde_json::from_str::<PolicyGlobals>(&mem.0).map_err(|_| PlatformError::InputsError)?;

  if ret.id_vars.len() != pol.num_id_globals
     || ret.rt_vars.len() != pol.num_rt_globals
     || ret.amt_vars.len() != pol.num_amt_globals
     || ret.frac_vars.len() != pol.num_frac_globals
     || ret.id_vars
           .first()
           .ok_or_else(|| PlatformError::InputsError(error_location!()))?
        != &asset.issuer.key
     || ret.rt_vars
           .first()
           .ok_or_else(|| PlatformError::InputsError(error_location!()))?
        != &asset.code.val
  {
    Err(fail!("Incorrect number of variables for policy".to_string()))
  } else {
    Ok(ret)
  }
}

pub fn policy_check_txn(type_code: &AssetTypeCode,
                        globals: PolicyGlobals,
                        pol: &Policy,
                        txn: &Transaction)
                        -> Result<(), PlatformError> {
  let pol_data =
    // serde_json::from_str::<TxnPolicyData>(&txn.memos.get(0).ok_or(PlatformError::InputsError)?.0)?.0.drain(..).collect::<HashMap<_,_>>();
    txn.policy_options.as_ref().ok_or_else(|| PlatformError::InputsError(error_location!()))?.0.iter().cloned().collect::<HashMap<_,_>>();
  // serde_json::from_str::<TxnPolicyData>(&txn.memos.get(0).ok_or(PlatformError::InputsError)?.0)?.0.drain(..).collect::<HashMap<_,_>>();

  let inputs = pol_data.get(type_code)
                       .ok_or_else(|| PlatformError::InputsError(error_location!()))?;

  let the_check = {
    let mut check = Err(PlatformError::InputsError(error_location!()));
    for c in pol.txn_choices.iter() {
      if c.name == inputs.which_check {
        check = Ok(c);
        break;
      }
    }
    check
  }?;

  run_txn_check(the_check,
                globals.id_vars,
                globals.rt_vars,
                globals.amt_vars,
                globals.frac_vars,
                txn)
}

/*
 * The *_vars variables are the initial global variables.
 */
#[allow(clippy::cognitive_complexity)]
pub fn run_txn_check(check: &TxnCheck,
                     mut id_vars: Vec<XfrPublicKey>,
                     mut rt_vars: Vec<AssetType>,
                     mut amt_vars: Vec<u64>,
                     mut frac_vars: Vec<Fraction>,
                     txn: &Transaction)
                     -> Result<(), PlatformError> {
  dbg!(check);
  dbg!(&id_vars);
  dbg!(&rt_vars);
  dbg!(&amt_vars);
  dbg!(&frac_vars);
  dbg!(txn);

  /*
   * To convert the TxnOps into RealTxnOps, we need to:
   *  - gather any operations that share an input resource (necessarily
   *    Transfers) into the same op
   *  - gather any operations that share a non-burn output resource into
   *    the same op
   *  - Ensure that no resource is assigned to more than one "position"
   *    (ie input or output) or more than one op.
   *  - Ensure that (at the end), resource indices completely cover
   *    0..(num_resources-1) inclusive
   *  - Ensure that all the resources have matching asset types
   *    - Specifically, issued resources must have a matching type, and
   *      transferred resources must have equal types
   *
   * If all those things hold, then every "resource" in the script
   * corresponds one-to-one with an AssetRecord in the transaction.
   */

  let resvar_types = check.in_params
                          .iter()
                          .chain(check.out_params.iter())
                          .enumerate()
                          .map(|(ix, tp)| (ResourceVar(ix as u64), tp))
                          .collect::<HashMap<_, _>>();

  // Record which types are used in issuances, and pairs of resource vars
  // used in transfers.
  // TODO(joe): evaluate using a union-find for transfer_types
  // let mut issue_types = HashMap<ResourceVar, ResourceTypeVar>::new();
  // let mut transfer_types = Vec<(ResourceVar, ResourceVar)>::new();

  let mut pending_inputs = HashSet::<ResourceVar>::new();
  let mut pending_outputs = HashSet::<ResourceVar>::new();
  let mut pending_op: Option<RealTxnOp> = None;

  let mut num_outputs: u64 = 0;
  let mut num_inputs: u64 = 0;
  let mut used_resources = HashSet::<ResourceVar>::new();
  let mut real_ops = Vec::<RealTxnOp>::new();
  for op in check.txn_template.iter() {
    dbg!(&pending_op);
    debug_assert!(pending_inputs.is_disjoint(&used_resources));
    debug_assert!(pending_inputs.is_disjoint(&pending_outputs));
    debug_assert!(pending_outputs.is_disjoint(&used_resources));
    debug_assert!(num_inputs + num_outputs == used_resources.len() as u64);

    match op {
      TxnOp::Issue(amt, rt, res) => {
        // If it's already accounted for, error.
        if used_resources.contains(res) || pending_outputs.contains(res) {
          return Err(fail!());
        }

        // // record its type as a requirement
        // issue_types.insert(res.clone(), rt.clone());

        if let Some(ref mut pending) = pending_op {
          match pending {
            RealTxnOp::Transfer(_) => {
              real_ops.push(pending.clone());
              num_inputs += pending_inputs.len() as u64;
              num_outputs += pending_outputs.len() as u64;
              used_resources.extend(pending_inputs.drain());
              used_resources.extend(pending_outputs.drain());
              pending_op = Some(RealTxnOp::Issue(*rt, vec![(*amt, *res)]));
            }
            RealTxnOp::Issue(pending_rt, ref mut pending_dsts) => {
              debug_assert!(pending_inputs.is_empty());
              if *pending_rt != *rt {
                real_ops.push(pending.clone());
                num_outputs += pending_outputs.len() as u64;
                used_resources.extend(pending_outputs.drain());
                pending_op = Some(RealTxnOp::Issue(*rt, vec![(*amt, *res)]));
              } else {
                pending_dsts.push((*amt, *res));
              }
            }
          }
        } else {
          pending_op = Some(RealTxnOp::Issue(*rt, vec![(*amt, *res)]));
        }
        pending_outputs.insert(*res);
      }
      TxnOp::Transfer(amt, inp, out) => {
        // If inp or out are already accounted for, error.
        if used_resources.contains(inp) {
          return Err(fail!());
        }
        if let Some(out_res) = out {
          if used_resources.contains(out_res) {
            return Err(fail!());
          }

          // // record type equality as a requirement
          // transfer_types.push((inp.clone(), out.clone()));
        }

        if let Some(ref mut pending) = pending_op {
          match pending {
            RealTxnOp::Issue(_, _) => {
              if pending_outputs.contains(inp) {
                return Err(fail!());
              }
              if let Some(out_res) = out {
                if pending_outputs.contains(out_res) {
                  return Err(fail!());
                }
              }
              real_ops.push(pending.clone());
              debug_assert!(pending_inputs.is_empty());
              num_outputs += pending_outputs.len() as u64;
              used_resources.extend(pending_outputs.drain());
              pending_op = Some(RealTxnOp::Transfer(vec![(*amt, *inp, *out)]));
            }
            RealTxnOp::Transfer(transfers) => {
              // if an input gets used as an output or
              // vice-versa, error.
              if pending_outputs.contains(inp) {
                return Err(fail!());
              }
              if let Some(out_res) = out {
                if pending_inputs.contains(out_res) {
                  return Err(fail!());
                }
              }

              /*
               * Require that each Transfer adds either a new
               * input or a new output. As with the
               * no-double-output restriction in Issue, this
               * is not strictly necessary and should be
               * safely removable.
               */
              if pending_inputs.contains(inp) {
                if let Some(out_res) = out {
                  if pending_outputs.contains(out_res) {
                    return Err(fail!());
                  }
                }
              }
              transfers.push((*amt, *inp, *out));
            }
          }
        } else {
          pending_op = Some(RealTxnOp::Transfer(vec![(*amt, *inp, *out)]));
        }

        pending_inputs.insert(*inp);
        if let Some(out_res) = out {
          pending_outputs.insert(*out_res);
        }
      }
    }
  }
  dbg!(&pending_op);

  debug_assert!(pending_inputs.is_disjoint(&used_resources));
  debug_assert!(pending_inputs.is_disjoint(&pending_outputs));
  debug_assert!(pending_outputs.is_disjoint(&used_resources));

  if let Some(final_op) = pending_op {
    real_ops.push(final_op);
    num_inputs += pending_inputs.len() as u64;
    num_outputs += pending_outputs.len() as u64;
    used_resources.extend(pending_inputs.drain());
    used_resources.extend(pending_outputs.drain());
    pending_op = None;
  }

  dbg!("Built txn template");

  debug_assert!(pending_op.is_none());
  debug_assert!(num_inputs + num_outputs == used_resources.len() as u64);
  if num_inputs != check.in_params.len() as u64 || num_outputs != check.out_params.len() as u64 {
    return Err(fail!());
  }

  dbg!("Resource check");
  // check that all resources in the index range are used
  for ix in 0..used_resources.len().try_into().map_err(|_| fail!())? {
    if !used_resources.contains(&ResourceVar(ix)) {
      return Err(fail!());
    }
  }

  /*
   * -------- Match the RealTxnOps to the AssetRecords -----
   */

  if real_ops.len() != txn.operations.len() {
    return Err(fail!());
  }

  // freeze real_ops
  let real_ops = real_ops;

  /*
   * So the thing we need to do here is:
   *  - Step through the template and add new resources to res_vars
   *  - For any non-new resources, accumulate the AmountVars.
   *
   * This should maybe be done through a memo?
   */

  let mut res_vars = HashMap::<ResourceVar, TxOutput>::new();
  let mut res_var_inputs = HashSet::<ResourceVar>::new();
  res_vars.reserve(used_resources.len());
  let mut res_totals = HashMap::<ResourceVar, Vec<AmountVar>>::new();

  dbg!(&real_ops);

  for (target, real) in real_ops.iter().zip(txn.operations.iter()) {
    dbg!("Txn match step");
    match (target, real) {
      (RealTxnOp::Issue(_, outs), Operation::IssueAsset(iss)) => {
        debug_assert!(!outs.is_empty());
        // NOTE: This relies on the no-repeated-outputs invariant
        // from before
        if outs.len() != iss.body.records.len() {
          return Err(fail!());
        }

        res_vars.extend(outs.iter()
                            .map(|(_, rv)| *rv)
                            .zip(iss.body.records.iter().cloned()));
        res_totals.extend(outs.iter().map(|(amt, rv)| (*rv, vec![*amt])));
      }
      (RealTxnOp::Transfer(transfers), Operation::TransferAsset(trn)) => {
        debug_assert!(!transfers.is_empty());
        let mut inp_ix = 0;
        let mut out_ix = 0;

        for (amt, inp, out) in transfers.iter() {
          let asset_type: AssetType;
          if let Some(inp_txo) = res_vars.get(inp) {
            debug_assert!(if let Some(out_k) = out {
                            !res_vars.contains_key(out_k)
                          } else {
                            true
                          });
            res_totals.get_mut(inp).as_mut().unwrap().push(*amt);

            debug_assert!(!inp_txo.0.asset_type.is_confidential());

            asset_type = inp_txo.0.asset_type.get_asset_type().unwrap();
          } else {
            debug_assert!(!res_totals.contains_key(inp));

            let inp_txo = trn.body
                             .transfer
                             .inputs
                             .get(inp_ix)
                             .ok_or_else(|| fail!())?;
            asset_type = inp_txo.asset_type.get_asset_type().ok_or_else(|| fail!())?;

            res_vars.insert(*inp, TxOutput(inp_txo.clone()));
            res_totals.insert(*inp, vec![*amt]);
            res_var_inputs.insert(*inp);

            inp_ix += 1;
          }

          if let Some(out_res) = out {
            if let Some(out_txo) = res_vars.get(out_res) {
              debug_assert!(!res_vars.contains_key(inp));
              res_totals.get_mut(out_res).as_mut().unwrap().push(*amt);

              debug_assert!(!out_txo.0.asset_type.is_confidential());

              if asset_type != out_txo.0.asset_type.get_asset_type().unwrap() {
                return Err(fail!());
              }
            } else {
              debug_assert!(!res_totals.contains_key(out_res));

              let out_txo = trn.body
                               .transfer
                               .outputs
                               .get(out_ix)
                               .ok_or_else(|| fail!())?;

              if asset_type != out_txo.asset_type.get_asset_type().ok_or_else(|| fail!())? {
                return Err(fail!());
              }

              res_vars.insert(*out_res, TxOutput(out_txo.clone()));
              res_totals.insert(*out_res, vec![*amt]);

              out_ix += 1;
            }
          } else {
            let null_public_key = XfrPublicKey::zei_from_bytes(&[0; 32]);
            let out_txo = trn.body
                             .transfer
                             .outputs
                             .get(out_ix)
                             .ok_or_else(|| fail!())?;

            // TODO(joe): maybe move this later?
            if out_txo.asset_type.get_asset_type().ok_or_else(|| fail!())? != asset_type {
              return Err(fail!());
            }

            if out_txo.public_key != null_public_key {
              return Err(fail!());
            }

            out_ix += 1;
          }
        }

        if inp_ix < trn.body.transfer.inputs.len() {
          return Err(fail!());
        }
        if out_ix < trn.body.transfer.outputs.len() {
          return Err(fail!());
        }
      }
      _ => {
        return Err(fail!());
      }
    }
  }

  // freeze res_vars & res_totals
  let res_vars = res_vars;
  let res_totals = res_totals;

  dbg!(&res_vars);

  /*
   * AT THIS POINT we should know:
   *  - All the *transfers* are of consistent asset types.
   *      - Issuance requires actually knowing *what* the asset types
   *        are, which may not be available before policy-running.
   *  - All the input and output TXOs are consistently selected by
   *    AssetVars (indicated by res_vars)
   */

  /*
   * -------- Now we can actually run the policy! (jeez) -------
   */

  /* Step 1: calculate resource type vars */
  for rt_op in check.rt_ops.iter() {
    dbg!("RT op");
    rt_vars.push(match rt_op {
                   ResourceTypeOp::Var(rv) => {
                     let ix: usize = rv.0.try_into().map_err(|_| fail!())?;
                     let asset_type: &AssetType = rt_vars.get(ix).ok_or_else(|| fail!())?;
                     *asset_type
                   }
                   ResourceTypeOp::TypeOfResource(res_var) => res_vars.get(res_var)
                                                                      .ok_or_else(|| fail!())?
                                                                      .0
                                                                      .asset_type
                                                                      .get_asset_type()
                                                                      .ok_or_else(|| fail!())?,
                 });
    dbg!(rt_vars.len());
    dbg!(rt_vars.last());
  }

  /* Step 2: Calculate identity ops */
  for id_op in check.id_ops.iter() {
    dbg!("ID op");
    id_vars.push(match id_op {
                   IdOp::Var(iv) => {
                     let ix: usize = iv.0.try_into().map_err(|_| fail!())?;
                     let id: &XfrPublicKey = id_vars.get(ix).ok_or_else(|| fail!())?;
                     *id
                   }
                   IdOp::OwnerOf(res_var) => {
                     res_vars.get(res_var).ok_or_else(|| fail!())?.0.public_key
                   }
                 })
  }

  /* Step 3: Calculate fraction & amount ops */
  {
    let mut frac_ix = 0;
    let mut amt_ix = 0;
    let mut needed_frac_ix = 0;
    let mut needed_amt_ix = 0;
    #[derive(Debug)]
    enum FracAmtPhase {
      Frac,
      Amt,
    };
    let mut phase = FracAmtPhase::Amt;

    while frac_ix < check.fraction_ops.len() || amt_ix < check.amount_ops.len() {
      debug_assert!(frac_ix <= check.fraction_ops.len());
      debug_assert!(amt_ix <= check.amount_ops.len());

      // needed_*_ix should only ever get updated to an index that
      // *could* be computed
      debug_assert!(needed_frac_ix == 0
                    || needed_frac_ix < frac_vars.len() + (check.fraction_ops.len() - frac_ix));
      debug_assert!(needed_amt_ix == 0
                    || needed_amt_ix < amt_vars.len() + (check.amount_ops.len() - amt_ix));

      dbg!(&phase);
      dbg!(&needed_frac_ix);
      dbg!(&needed_amt_ix);
      dbg!(&amt_vars.len());
      dbg!(&frac_vars.len());

      match phase {
        FracAmtPhase::Amt => {
          dbg!("Amount op");
          dbg!(check.amount_ops.get(amt_ix));
          match check.amount_ops.get(amt_ix) {
            None => {
              phase = FracAmtPhase::Frac;
              continue;
            }
            Some(AmountOp::Var(ix)) => {
              let ix: usize = ix.0.try_into().map_err(|_| fail!())?;
              amt_vars.push(*(amt_vars.get(ix).ok_or_else(|| fail!())?));
              amt_ix += 1;
            }
            Some(AmountOp::Const(n)) => {
              amt_vars.push(*n);
              amt_ix += 1;
            }
            Some(AmountOp::AmountOf(res_var)) => {
              if !res_var_inputs.contains(res_var) {
                return Err(fail!());
              }

              let n = res_vars.get(res_var)
                              .ok_or_else(|| fail!())?
                              .0
                              .amount
                              .get_amount()
                              .ok_or_else(|| fail!())?;
              amt_vars.push(n);
              amt_ix += 1;
            }
            Some(AmountOp::Plus(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail!())?;
              let r: usize = r.0.try_into().map_err(|_| fail!())?;
              let lv: u64 = *(amt_vars.get(l).ok_or_else(|| fail!())?);
              let rv: u64 = *(amt_vars.get(r).ok_or_else(|| fail!())?);
              amt_vars.push(lv.checked_add(rv).ok_or_else(|| fail!())?);
              amt_ix += 1;
            }
            Some(AmountOp::Minus(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail!())?;
              let r: usize = r.0.try_into().map_err(|_| fail!())?;
              let lv: u64 = *(amt_vars.get(l).ok_or_else(|| fail!())?);
              let rv: u64 = *(amt_vars.get(r).ok_or_else(|| fail!())?);
              amt_vars.push(lv.checked_sub(rv).ok_or_else(|| fail!())?);
              amt_ix += 1;
            }
            Some(AmountOp::Times(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail!())?;
              let r: usize = r.0.try_into().map_err(|_| fail!())?;
              let lv: u64 = *(amt_vars.get(l).ok_or_else(|| fail!())?);
              let rv: u64 = *(amt_vars.get(r).ok_or_else(|| fail!())?);
              amt_vars.push(lv.checked_mul(rv).ok_or_else(|| fail!())?);
              amt_ix += 1;
            }
            Some(AmountOp::Round(round_ix)) => {
              let round_ix = round_ix.0.try_into().map_err(|_| fail!())?;
              dbg!(&round_ix);
              dbg!(frac_vars.get(round_ix));
              match frac_vars.get(round_ix) {
                Some(fv) => {
                  let fv: &Fraction = fv;
                  let fv: I20F12 = fv.0;
                  amt_vars.push(fv.round().checked_to_num().ok_or_else(|| fail!())?);
                  amt_ix += 1;
                }
                None => {
                  // if the frac ops are waiting on a
                  // result we haven't computed yet,
                  // there's a circular dependency.
                  // Error.
                  if amt_vars.get(needed_amt_ix).is_none() {
                    return Err(fail!());
                  }

                  // If the needed fraction index can't
                  // possibly be available, error.
                  if round_ix >= frac_vars.len() + (check.fraction_ops.len() - frac_ix) {
                    return Err(fail!());
                  }

                  needed_frac_ix = round_ix;
                  phase = FracAmtPhase::Frac;
                }
              }
            }
          }
          dbg!(amt_vars.len());
          dbg!(amt_vars.last());
        }
        FracAmtPhase::Frac => {
          dbg!("Frac op");
          dbg!(check.fraction_ops.get(frac_ix));
          match check.fraction_ops.get(frac_ix) {
            None => {
              phase = FracAmtPhase::Amt;
              continue;
            }
            Some(FractionOp::Var(ix)) => {
              let ix: usize = ix.0.try_into().map_err(|_| fail!())?;
              frac_vars.push(*(frac_vars.get(ix).ok_or_else(|| fail!())?));
              frac_ix += 1;
            }
            Some(FractionOp::Const(v)) => {
              frac_vars.push(*v);
              frac_ix += 1;
            }
            Some(FractionOp::Plus(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail!())?;
              let r: usize = r.0.try_into().map_err(|_| fail!())?;
              let lv: Fraction = *(frac_vars.get(l).ok_or_else(|| fail!())?);
              let rv: Fraction = *(frac_vars.get(r).ok_or_else(|| fail!())?);
              frac_vars.push(Fraction(lv.0.checked_add(rv.0).ok_or_else(|| fail!())?));
              frac_ix += 1;
            }
            Some(FractionOp::Times(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail!())?;
              let r: usize = r.0.try_into().map_err(|_| fail!())?;
              let lv: Fraction = *(frac_vars.get(l).ok_or_else(|| fail!())?);
              let rv: Fraction = *(frac_vars.get(r).ok_or_else(|| fail!())?);
              frac_vars.push(Fraction(lv.0.checked_mul(rv.0).ok_or_else(|| fail!())?));
              frac_ix += 1;
            }
            Some(FractionOp::AmtTimes(a, f)) => {
              let a: usize = a.0.try_into().map_err(|_| fail!())?;
              let f: usize = f.0.try_into().map_err(|_| fail!())?;
              match amt_vars.get(a) {
                None => {
                  // if the amount ops are waiting on a
                  // result we haven't computed yet,
                  // there's a circular dependency.
                  // Error.
                  if frac_vars.get(needed_frac_ix).is_none() {
                    return Err(fail!());
                  }
                  // If the needed amount index can't
                  // possibly be available, error.
                  if amt_ix >= amt_vars.len() + (check.amount_ops.len() - amt_ix) {
                    return Err(fail!());
                  }

                  needed_amt_ix = a;
                  phase = FracAmtPhase::Amt;
                }
                Some(amt_val) => {
                  let fv: Fraction = *(frac_vars.get(f).ok_or_else(|| fail!())?);
                  let fv: I20F12 = fv.0;
                  let av: I20F12 = Fraction::checked_new(*amt_val, 1).ok_or_else(|| fail!())?.0;
                  frac_vars.push(Fraction(av.checked_mul(fv).ok_or_else(|| fail!())?));
                  frac_ix += 1;
                }
              }
            }
            Some(FractionOp::TimesAmt(f, a)) => {
              let a: usize = a.0.try_into().map_err(|_| fail!())?;
              let f: usize = f.0.try_into().map_err(|_| fail!())?;
              match amt_vars.get(a) {
                None => {
                  // if the amount ops are waiting on a
                  // result we haven't computed yet,
                  // there's a circular dependency.
                  // Error.
                  if frac_vars.get(needed_frac_ix).is_none() {
                    return Err(fail!());
                  }
                  // If the needed amount index can't
                  // possibly be available, error.
                  if amt_ix >= amt_vars.len() + (check.amount_ops.len() - amt_ix) {
                    return Err(fail!());
                  }

                  needed_amt_ix = a;
                  phase = FracAmtPhase::Amt;
                }
                Some(amt_val) => {
                  let fv: Fraction = *(frac_vars.get(f).ok_or_else(|| fail!())?);
                  let fv: I20F12 = fv.0;
                  let av: I20F12 = Fraction::checked_new(*amt_val, 1).ok_or_else(|| fail!())?.0;
                  frac_vars.push(Fraction(fv.checked_mul(av).ok_or_else(|| fail!())?));
                  frac_ix += 1;
                }
              }
            }
          }
          dbg!(frac_vars.len());
          dbg!(frac_vars.last());
        }
      }
    }
  }

  /* Step 4: Compute all the bool expressions. */
  /* NOTE: in the future we will have conditional expressions, and
   * everything will end up being the kind of back-and-forth mess that
   * the frac/amt stuff is right now, since all kinds of things can feed
   * back and forth through boolean expressionss and conditionals. We'll
   * probably actually want to build some sort of dependency graph +
   * topological sort so that the interpretation logic is simpler.
   */

  let mut bool_vars = Vec::<bool>::new();

  for op in check.bool_ops.iter() {
    dbg!("Bool op");
    match op {
      BoolOp::Const(v) => {
        bool_vars.push(*v);
      }
      BoolOp::IdEq(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail!())?;
        let r: usize = r.0.try_into().map_err(|_| fail!())?;
        bool_vars.push(id_vars.get(l).ok_or_else(|| fail!())?
                       == id_vars.get(r).ok_or_else(|| fail!())?);
      }
      BoolOp::AmtEq(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail!())?;
        let r: usize = r.0.try_into().map_err(|_| fail!())?;
        bool_vars.push(amt_vars.get(l).ok_or_else(|| fail!())?
                       == amt_vars.get(r).ok_or_else(|| fail!())?);
      }
      BoolOp::FracEq(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail!())?;
        let r: usize = r.0.try_into().map_err(|_| fail!())?;
        bool_vars.push(frac_vars.get(l).ok_or_else(|| fail!())?
                       == frac_vars.get(r).ok_or_else(|| fail!())?);
      }
      BoolOp::ResourceTypeEq(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail!())?;
        let r: usize = r.0.try_into().map_err(|_| fail!())?;
        bool_vars.push(rt_vars.get(l).ok_or_else(|| fail!())?
                       == rt_vars.get(r).ok_or_else(|| fail!())?);
      }
      BoolOp::Not(bv) => {
        let bv: usize = bv.0.try_into().map_err(|_| fail!())?;
        let v: bool = *(bool_vars.get(bv).ok_or_else(|| fail!())?);
        bool_vars.push(!v);
      }
      BoolOp::And(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail!())?;
        let r: usize = r.0.try_into().map_err(|_| fail!())?;
        let lv = *(bool_vars.get(l).ok_or_else(|| fail!())?);
        let rv = *(bool_vars.get(r).ok_or_else(|| fail!())?);
        bool_vars.push(lv && rv);
      }
      BoolOp::Or(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail!())?;
        let r: usize = r.0.try_into().map_err(|_| fail!())?;
        let lv = *(bool_vars.get(l).ok_or_else(|| fail!())?);
        let rv = *(bool_vars.get(r).ok_or_else(|| fail!())?);
        bool_vars.push(lv || rv);
      }
      BoolOp::AmtGe(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail!())?;
        let r: usize = r.0.try_into().map_err(|_| fail!())?;
        let lv = *(amt_vars.get(l).ok_or_else(|| fail!())?);
        let rv = *(amt_vars.get(r).ok_or_else(|| fail!())?);
        bool_vars.push(lv >= rv);
      }
      BoolOp::FracGe(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail!())?;
        let r: usize = r.0.try_into().map_err(|_| fail!())?;
        let lv = *(frac_vars.get(l).ok_or_else(|| fail!())?);
        let rv = *(frac_vars.get(r).ok_or_else(|| fail!())?);
        bool_vars.push(lv.0 >= rv.0);
      }
    }
  }

  /* Step 5: check assertions */
  for bv in check.assertions.iter() {
    dbg!("assertion");
    let bv: usize = bv.0.try_into().map_err(|_| fail!())?;
    let v: &bool = bool_vars.get(bv).ok_or_else(|| fail!())?;
    if !*v {
      return Err(fail!());
    }
  }

  /* Step 6: check for signatures */
  for iv in check.required_signatures.iter() {
    dbg!("signature");
    let iv: usize = iv.0.try_into().map_err(|_| fail!())?;
    let v: &XfrPublicKey = id_vars.get(iv).ok_or_else(|| fail!())?;
    dbg!("has signature?");
    dbg!(v);
    txn.check_has_signature(v).map_err(|_| fail!())?;
  }

  /* Step 7: consistency checks with asset records.
   *  (a) Check that asset type vars of issuances match
   *  (b) Check that asset types of transfers match
   *  (c) Check that amount sums of transfers match
   */

  // (a), (b)
  for op in real_ops.iter() {
    dbg!("op check");
    match op {
      // (a)
      RealTxnOp::Issue(rt_ix, outs) => {
        // should never happen
        if outs.is_empty() {
          return Err(fail!());
        }

        let rt_ix: usize = rt_ix.0.try_into().map_err(|_| fail!())?;
        let asset_type = rt_vars.get(rt_ix).ok_or_else(|| fail!())?;

        for (_, rv) in outs.iter() {
          let txo = res_vars.get(rv).ok_or_else(|| fail!())?;
          // txo.0.asset_type should be established as non-None by the
          // first two checking loops.
          if *asset_type != txo.0.asset_type.get_asset_type().unwrap() {
            return Err(fail!());
          }
        }
      }
      // (b)
      RealTxnOp::Transfer(transfers) => {
        for (_, inp, out) in transfers {
          let inp_txo = res_vars.get(inp).ok_or_else(|| fail!())?;
          let inp_asset_type = resvar_types.get(inp).ok_or_else(|| fail!())?;
          let inp_asset_type = rt_vars.get(inp_asset_type.0 as usize)
                                      .ok_or_else(|| fail!())?;
          // txo.0.asset_type should be established as non-None by the
          // first two checking loops.
          if *inp_asset_type != inp_txo.0.asset_type.get_asset_type().unwrap() {
            return Err(fail!());
          }

          if let Some(real_out) = out {
            let out_txo = res_vars.get(real_out).ok_or_else(|| fail!())?;
            let out_asset_type = resvar_types.get(real_out).ok_or_else(|| fail!())?;
            let out_asset_type = rt_vars.get(out_asset_type.0 as usize)
                                        .ok_or_else(|| fail!())?;
            // txo.0.asset_type should be established as non-None by the
            // first two checking loops.
            if *out_asset_type != out_txo.0.asset_type.get_asset_type().unwrap() {
              return Err(fail!());
            }
          }
        }
      }
    }
  }

  // (c)
  for (rv, tot_vars) in res_totals.iter() {
    dbg!("total check");
    if tot_vars.is_empty() {
      return Err(fail!());
    }
    let mut total: u64 = 0;

    for av in tot_vars.iter() {
      let av: usize = av.0.try_into().map_err(|_| fail!())?;
      let val: u64 = *(amt_vars.get(av).ok_or_else(|| fail!())?);
      total = total.checked_add(val).ok_or_else(|| fail!())?;
    }

    if total
       != res_vars.get(rv)
                  .ok_or_else(|| fail!())?
                  .0
                  .amount
                  .get_amount()
                  .ok_or_else(|| fail!())?
    {
      return Err(fail!());
    }
  }

  /*
   * At this point we have:
   *  - Validated the ResourceVar <-> TXO correspondence
   *  - Checked that the transactions match the given template
   *  - Checked that all stated transfers match in asset type
   *  - Computed all the various values within the policy, erroring out
   *    if anything has an ill-defined effect (eg overflow)
   *  - Checked that all required signatures are present.
   *  - Checked that all asserted boolean values came out as true.
   *  - Checked that resource-type expressions agree with the asset types
   *    used for issuance
   *  - Checked that amount values used for operations total correctly
   *
   *  Thus, the check has passed!
   */

  Ok(())
}

/*
 * How should this work?
 * Each op generates a var, the only possible dependency cycle is fraction
 * <-> amount so some "cycle detection" is necessary.
 *  - alg sketch:
 *      min_frac_index, min_amt_index
 *      which = frac | amt
 *      iterate forward on ops[which], do computation *unless* it depends
 *        on an index in the other list which is past the current computed
 *        list. If min_[which]_index is after our current position,
 *        fail.clone().
 *        Otherwise, set min_[other]_index to the thing we depend on, set
 *        which to other, continue.
 *
 *
 * We need to be able to decode the global variables from a DefineAsset
 * memo
 *
 * From a transaction, we need to be able to decode
 *  (a) which txn choice to use
 *  (b) which of the AssetRecords involved in the transaction correspond to
 *      the `ResourceVar`s the TxnCheck will use
 *  (c) Some info about combining multiple Transfer/Issue `TxnOp`s?
 *
 *      Followup:
 *          - Probably just require that all these things from the same txo
 *            are in sequence and do the naive thing.
 *
 * Important checks:
 *  - overflow -- always use checked_* methods
 *  - making sure everything's accounted for at the end
 *  -
 *
 * The other important thing here is that a policy should be able to
 * generate a transaction passing the policy.
 *
 * This should yield a test -- if you have a policy, and you sample
 * transactions generated from the policy, it should pass the validation.
 *
 */
