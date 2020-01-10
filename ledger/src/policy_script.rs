#![deny(warnings)]
use crate::data_model::errors::PlatformError;
use crate::data_model::{Operation, Transaction, TxOutput};
use crate::policies::Fraction;
use fixed::types::I20F12;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::AssetType;

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

pub enum IdOp {
  OwnerOf(ResourceVar),
  Var(IdVar),
}

pub enum AmountOp {
  Var(AmountVar),
  Const(u64),
  Plus(AmountVar, AmountVar),
  Minus(AmountVar, AmountVar),
  Times(AmountVar, AmountVar),
  Round(FractionVar),
}

pub enum FractionOp {
  Var(FractionVar),
  Const(Fraction),
  Plus(FractionVar, FractionVar),
  Times(FractionVar, FractionVar),
  AmtTimes(AmountVar, FractionVar),
  TimesAmt(FractionVar, AmountVar),
}

pub enum ResourceTypeOp {
  Var(ResourceTypeVar),
  TypeOfResource(ResourceVar),
}

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

pub struct TxnCheck {
  pub num_in_params: u64,
  pub num_out_params: u64,
  pub id_ops: Vec<IdOp>,
  pub rt_ops: Vec<ResourceTypeOp>,

  // These actually need to execute back & forth, which feels like maybe
  // a problem
  pub fraction_ops: Vec<FractionOp>,
  pub amount_ops: Vec<AmountOp>,

  pub bool_ops: Vec<BoolOp>,
  pub assertions: Vec<BoolVar>,

  pub txn_template: Vec<TxnOp>,
}

pub struct Policy {
  // Implicitly: bound asset type and issuer as AssetType & Id globals
  // respectively
  pub num_id_globals: u64,
  pub num_rt_globals: u64,
  pub num_amt_globals: u64,
  pub num_frac_globals: u64,

  pub init_check: TxnCheck,

  pub txn_choices: Vec<TxnCheck>,
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
  let fail = &PlatformError::PolicyFailureError;

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
   *
   * If all those things hold, then every "resource" in the script
   * corresponds one-to-one with an AssetRecord in the transaction.
   */

  let mut pending_inputs = HashSet::<ResourceVar>::new();
  let mut pending_outputs = HashSet::<ResourceVar>::new();
  let mut pending_op: Option<RealTxnOp> = None;

  let mut used_resources = HashSet::<ResourceVar>::new();
  let mut real_ops = Vec::<RealTxnOp>::new();
  for op in check.txn_template.iter() {
    debug_assert!(pending_inputs.is_disjoint(&used_resources));
    debug_assert!(pending_inputs.is_disjoint(&pending_outputs));
    debug_assert!(pending_outputs.is_disjoint(&used_resources));

    match op {
      TxnOp::Issue(amt, rt, res) => {
        // If it's already accounted for, error.
        if used_resources.contains(res) || pending_outputs.contains(res) {
          return Err(fail.clone());
        }
        if let Some(ref mut pending) = pending_op {
          match pending {
            RealTxnOp::Transfer(_) => {
              real_ops.push(pending.clone());
              used_resources.extend(pending_inputs.drain());
              used_resources.extend(pending_outputs.drain());
              pending_op = Some(RealTxnOp::Issue(*rt, vec![(*amt, *res)]));
            }
            RealTxnOp::Issue(pending_rt, ref mut pending_dsts) => {
              debug_assert!(pending_inputs.is_empty());
              if *pending_rt != *rt {
                real_ops.push(pending.clone());
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
          return Err(fail.clone());
        }
        if let Some(out_res) = out {
          if used_resources.contains(out_res) {
            return Err(fail.clone());
          }
        }

        if let Some(ref mut pending) = pending_op {
          match pending {
            RealTxnOp::Issue(_, _) => {
              if pending_outputs.contains(inp) {
                return Err(fail.clone());
              }
              if let Some(out_res) = out {
                if pending_outputs.contains(out_res) {
                  return Err(fail.clone());
                }
              }
              real_ops.push(pending.clone());
              debug_assert!(pending_inputs.is_empty());
              used_resources.extend(pending_outputs.drain());
              pending_op = Some(RealTxnOp::Transfer(vec![(*amt, *inp, *out)]));
            }
            RealTxnOp::Transfer(transfers) => {
              // if an input gets used as an output or
              // vice-versa, error.
              if pending_outputs.contains(inp) {
                return Err(fail.clone());
              }
              if let Some(out_res) = out {
                if pending_inputs.contains(out_res) {
                  return Err(fail.clone());
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
                    return Err(fail.clone());
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

  debug_assert!(pending_inputs.is_disjoint(&used_resources));
  debug_assert!(pending_inputs.is_disjoint(&pending_outputs));
  debug_assert!(pending_outputs.is_disjoint(&used_resources));

  if let Some(final_op) = pending_op {
    real_ops.push(final_op);
    used_resources.extend(pending_inputs.drain());
    used_resources.extend(pending_outputs.drain());
    pending_op = None;
  }

  debug_assert!(pending_op.is_none());

  // check that all resources in the index range are used
  for ix in 0..used_resources.len().try_into().map_err(|_| fail.clone())? {
    if !used_resources.contains(&ResourceVar(ix)) {
      return Err(fail.clone());
    }
  }

  /*
   * -------- Match the RealTxnOps to the AssetRecords -----
   */

  if real_ops.len() != txn.operations.len() {
    return Err(fail.clone());
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
  res_vars.reserve(used_resources.len());
  let mut res_totals = HashMap::<ResourceVar, Vec<AmountVar>>::new();

  for (target, real) in real_ops.iter().zip(txn.operations.iter()) {
    match (target, real) {
      (RealTxnOp::Issue(_, outs), Operation::IssueAsset(iss)) => {
        debug_assert!(!outs.is_empty());
        // NOTE: This relies on the no-repeated-outputs invariant
        // from before
        if outs.len() != iss.body.records.len() {
          return Err(fail.clone());
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

            debug_assert!(inp_txo.0.asset_type.is_some());

            asset_type = inp_txo.0.asset_type.unwrap();
          } else {
            debug_assert!(!res_totals.contains_key(inp));

            let inp_txo = trn.body
                             .transfer
                             .inputs
                             .get(inp_ix)
                             .ok_or_else(|| fail.clone())?;
            asset_type = inp_txo.asset_type.ok_or_else(|| fail.clone())?;

            res_vars.insert(*inp, TxOutput(inp_txo.clone()));
            res_totals.insert(*inp, vec![*amt]);

            inp_ix += 1;
          }

          if let Some(out_res) = out {
            if let Some(out_txo) = res_vars.get(out_res) {
              debug_assert!(!res_vars.contains_key(inp));
              res_totals.get_mut(out_res).as_mut().unwrap().push(*amt);

              debug_assert!(out_txo.0.asset_type.is_some());

              if asset_type != out_txo.0.asset_type.unwrap() {
                return Err(fail.clone());
              }
            } else {
              debug_assert!(!res_totals.contains_key(out_res));

              let out_txo = trn.body
                               .transfer
                               .outputs
                               .get(out_ix)
                               .ok_or_else(|| fail.clone())?;

              if asset_type != out_txo.asset_type.ok_or_else(|| fail.clone())? {
                return Err(fail.clone());
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
                             .ok_or_else(|| fail.clone())?;

            // TODO(joe): maybe move this later?
            if out_txo.asset_type.ok_or_else(|| fail.clone())? != asset_type {
              return Err(fail.clone());
            }

            if out_txo.public_key != null_public_key {
              return Err(fail.clone());
            }

            out_ix += 1;
          }
        }

        if inp_ix < trn.body.transfer.inputs.len() {
          return Err(fail.clone());
        }
        if out_ix < trn.body.transfer.outputs.len() {
          return Err(fail.clone());
        }
      }
      _ => {
        return Err(fail.clone());
      }
    }
  }

  // freeze res_vars & res_totals
  let res_vars = res_vars;
  let res_totals = res_totals;

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
    rt_vars.push(match rt_op {
                   ResourceTypeOp::Var(rv) => {
                     let ix: usize = rv.0.try_into().map_err(|_| fail.clone())?;
                     let asset_type: &AssetType = rt_vars.get(ix).ok_or_else(|| fail.clone())?;
                     *asset_type
                   }
                   ResourceTypeOp::TypeOfResource(res_var) => res_vars.get(res_var)
                                                                      .ok_or_else(|| fail.clone())?
                                                                      .0
                                                                      .asset_type
                                                                      .ok_or_else(|| fail.clone())?,
                 })
  }

  /* Step 2: Calculate identity ops */
  for id_op in check.id_ops.iter() {
    id_vars.push(match id_op {
                   IdOp::Var(iv) => {
                     let ix: usize = iv.0.try_into().map_err(|_| fail.clone())?;
                     let id: &XfrPublicKey = id_vars.get(ix).ok_or_else(|| fail.clone())?;
                     *id
                   }
                   IdOp::OwnerOf(res_var) => {
                     res_vars.get(res_var)
                             .ok_or_else(|| fail.clone())?
                             .0
                             .public_key
                   }
                 })
  }

  /* Step 3: Calculate fraction & amount ops */
  {
    let mut frac_ix = 0;
    let mut amt_ix = 0;
    let mut needed_frac_ix = 0;
    let mut needed_amt_ix = 0;
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

      match phase {
        FracAmtPhase::Amt => {
          match check.amount_ops.get(amt_ix) {
            None => {
              phase = FracAmtPhase::Frac;
              continue;
            }
            Some(AmountOp::Var(ix)) => {
              let ix: usize = ix.0.try_into().map_err(|_| fail.clone())?;
              amt_vars.push(*(amt_vars.get(ix).ok_or_else(|| fail.clone())?));
              amt_ix += 1;
            }
            Some(AmountOp::Const(n)) => {
              amt_vars.push(*n);
              amt_ix += 1;
            }
            Some(AmountOp::Plus(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
              let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
              let lv: u64 = *(amt_vars.get(l).ok_or_else(|| fail.clone())?);
              let rv: u64 = *(amt_vars.get(r).ok_or_else(|| fail.clone())?);
              amt_vars.push(lv.checked_add(rv).ok_or_else(|| fail.clone())?);
              amt_ix += 1;
            }
            Some(AmountOp::Minus(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
              let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
              let lv: u64 = *(amt_vars.get(l).ok_or_else(|| fail.clone())?);
              let rv: u64 = *(amt_vars.get(r).ok_or_else(|| fail.clone())?);
              amt_vars.push(lv.checked_sub(rv).ok_or_else(|| fail.clone())?);
              amt_ix += 1;
            }
            Some(AmountOp::Times(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
              let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
              let lv: u64 = *(amt_vars.get(l).ok_or_else(|| fail.clone())?);
              let rv: u64 = *(amt_vars.get(r).ok_or_else(|| fail.clone())?);
              amt_vars.push(lv.checked_mul(rv).ok_or_else(|| fail.clone())?);
              amt_ix += 1;
            }
            Some(AmountOp::Round(frac_ix)) => {
              let frac_ix = frac_ix.0.try_into().map_err(|_| fail.clone())?;
              match frac_vars.get(frac_ix) {
                Some(fv) => {
                  let fv: &Fraction = fv;
                  let fv: I20F12 = fv.0;
                  amt_vars.push(fv.round().checked_to_num().ok_or_else(|| fail.clone())?);
                  amt_ix += 1;
                }
                None => {
                  // if the frac ops are waiting on a
                  // result we haven't computed yet,
                  // there's a circular dependency.
                  // Error.
                  if amt_vars.get(needed_amt_ix).is_none() {
                    return Err(fail.clone());
                  }

                  // If the needed fraction index can't
                  // possibly be available, error.
                  if frac_ix >= frac_vars.len() + (check.fraction_ops.len() - frac_ix) {
                    return Err(fail.clone());
                  }

                  needed_frac_ix = frac_ix;
                  phase = FracAmtPhase::Frac;
                }
              }
            }
          }
        }
        FracAmtPhase::Frac => {
          match check.fraction_ops.get(frac_ix) {
            None => {
              phase = FracAmtPhase::Amt;
              continue;
            }
            Some(FractionOp::Var(ix)) => {
              let ix: usize = ix.0.try_into().map_err(|_| fail.clone())?;
              frac_vars.push(*(frac_vars.get(ix).ok_or_else(|| fail.clone())?));
              frac_ix += 1;
            }
            Some(FractionOp::Const(v)) => {
              frac_vars.push(*v);
              frac_ix += 1;
            }
            Some(FractionOp::Plus(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
              let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
              let lv: Fraction = *(frac_vars.get(l).ok_or_else(|| fail.clone())?);
              let rv: Fraction = *(frac_vars.get(r).ok_or_else(|| fail.clone())?);
              frac_vars.push(Fraction(lv.0.checked_add(rv.0).ok_or_else(|| fail.clone())?));
              frac_ix += 1;
            }
            Some(FractionOp::Times(l, r)) => {
              let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
              let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
              let lv: Fraction = *(frac_vars.get(l).ok_or_else(|| fail.clone())?);
              let rv: Fraction = *(frac_vars.get(r).ok_or_else(|| fail.clone())?);
              frac_vars.push(Fraction(lv.0.checked_mul(rv.0).ok_or_else(|| fail.clone())?));
              frac_ix += 1;
            }
            Some(FractionOp::AmtTimes(a, f)) => {
              let a: usize = a.0.try_into().map_err(|_| fail.clone())?;
              let f: usize = f.0.try_into().map_err(|_| fail.clone())?;
              match amt_vars.get(a) {
                None => {
                  // if the amount ops are waiting on a
                  // result we haven't computed yet,
                  // there's a circular dependency.
                  // Error.
                  if frac_vars.get(needed_frac_ix).is_none() {
                    return Err(fail.clone());
                  }
                  // If the needed amount index can't
                  // possibly be available, error.
                  if amt_ix >= amt_vars.len() + (check.amount_ops.len() - amt_ix) {
                    return Err(fail.clone());
                  }

                  needed_amt_ix = a;
                  phase = FracAmtPhase::Amt;
                }
                Some(amt_val) => {
                  let fv: Fraction = *(frac_vars.get(f).ok_or_else(|| fail.clone())?);
                  let fv: I20F12 = fv.0;
                  let av: I20F12 = Fraction::checked_new(*amt_val, 1).ok_or_else(|| fail.clone())?
                                                                     .0;
                  frac_vars.push(Fraction(av.checked_mul(fv).ok_or_else(|| fail.clone())?));
                  frac_ix += 1;
                }
              }
            }
            Some(FractionOp::TimesAmt(f, a)) => {
              let a: usize = a.0.try_into().map_err(|_| fail.clone())?;
              let f: usize = f.0.try_into().map_err(|_| fail.clone())?;
              match amt_vars.get(a) {
                None => {
                  // if the amount ops are waiting on a
                  // result we haven't computed yet,
                  // there's a circular dependency.
                  // Error.
                  if frac_vars.get(needed_frac_ix).is_none() {
                    return Err(fail.clone());
                  }
                  // If the needed amount index can't
                  // possibly be available, error.
                  if amt_ix >= amt_vars.len() + (check.amount_ops.len() - amt_ix) {
                    return Err(fail.clone());
                  }

                  needed_amt_ix = a;
                  phase = FracAmtPhase::Amt;
                }
                Some(amt_val) => {
                  let fv: Fraction = *(frac_vars.get(f).ok_or_else(|| fail.clone())?);
                  let fv: I20F12 = fv.0;
                  let av: I20F12 = Fraction::checked_new(*amt_val, 1).ok_or_else(|| fail.clone())?
                                                                     .0;
                  frac_vars.push(Fraction(fv.checked_mul(av).ok_or_else(|| fail.clone())?));
                  frac_ix += 1;
                }
              }
            }
          }
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
    match op {
      BoolOp::Const(v) => {
        bool_vars.push(*v);
      }
      BoolOp::IdEq(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
        let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
        bool_vars.push(id_vars.get(l).ok_or_else(|| fail.clone())?
                       == id_vars.get(r).ok_or_else(|| fail.clone())?);
      }
      BoolOp::AmtEq(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
        let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
        bool_vars.push(amt_vars.get(l).ok_or_else(|| fail.clone())?
                       == amt_vars.get(r).ok_or_else(|| fail.clone())?);
      }
      BoolOp::FracEq(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
        let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
        bool_vars.push(frac_vars.get(l).ok_or_else(|| fail.clone())?
                       == frac_vars.get(r).ok_or_else(|| fail.clone())?);
      }
      BoolOp::ResourceTypeEq(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
        let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
        bool_vars.push(rt_vars.get(l).ok_or_else(|| fail.clone())?
                       == rt_vars.get(r).ok_or_else(|| fail.clone())?);
      }
      BoolOp::Not(bv) => {
        let bv: usize = bv.0.try_into().map_err(|_| fail.clone())?;
        let v: bool = *(bool_vars.get(bv).ok_or_else(|| fail.clone())?);
        bool_vars.push(!v);
      }
      BoolOp::And(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
        let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
        let lv = *(bool_vars.get(l).ok_or_else(|| fail.clone())?);
        let rv = *(bool_vars.get(r).ok_or_else(|| fail.clone())?);
        bool_vars.push(lv && rv);
      }
      BoolOp::Or(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
        let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
        let lv = *(bool_vars.get(l).ok_or_else(|| fail.clone())?);
        let rv = *(bool_vars.get(r).ok_or_else(|| fail.clone())?);
        bool_vars.push(lv || rv);
      }
      BoolOp::AmtGe(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
        let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
        let lv = *(amt_vars.get(l).ok_or_else(|| fail.clone())?);
        let rv = *(amt_vars.get(r).ok_or_else(|| fail.clone())?);
        bool_vars.push(lv >= rv);
      }
      BoolOp::FracGe(l, r) => {
        let l: usize = l.0.try_into().map_err(|_| fail.clone())?;
        let r: usize = r.0.try_into().map_err(|_| fail.clone())?;
        let lv = *(frac_vars.get(l).ok_or_else(|| fail.clone())?);
        let rv = *(frac_vars.get(r).ok_or_else(|| fail.clone())?);
        bool_vars.push(lv.0 >= rv.0);
      }
    }
  }

  /* Step 5: check assertions */
  for bv in check.assertions.iter() {
    let bv: usize = bv.0.try_into().map_err(|_| fail.clone())?;
    let v: &bool = bool_vars.get(bv).ok_or_else(|| fail.clone())?;
    if !*v {
      return Err(fail.clone());
    }
  }

  /* Step 6: consistency checks with asset records.
   *  (a) Check that asset type vars of issuances match
   *  (b) Check that amount sums of transfers match
   */

  // (a)
  for op in real_ops.iter() {
    if let RealTxnOp::Issue(rt_ix, outs) = op {
      // should never happen
      if outs.is_empty() {
        return Err(fail.clone());
      }

      let rt_ix: usize = rt_ix.0.try_into().map_err(|_| fail.clone())?;
      let asset_type = rt_vars.get(rt_ix).ok_or_else(|| fail.clone())?;

      for (_, rv) in outs.iter() {
        let txo = res_vars.get(rv).ok_or_else(|| fail.clone())?;
        // txo.0.asset_type should be established as non-None by the
        // first two checking loops.
        if *asset_type != txo.0.asset_type.unwrap() {
          return Err(fail.clone());
        }
      }
    }
  }

  // (b)
  for (rv, tot_vars) in res_totals.iter() {
    if tot_vars.is_empty() {
      return Err(fail.clone());
    }
    let mut total: u64 = 0;

    for av in tot_vars.iter() {
      let av: usize = av.0.try_into().map_err(|_| fail.clone())?;
      let val: u64 = *(amt_vars.get(av).ok_or_else(|| fail.clone())?);
      total = total.checked_add(val).ok_or_else(|| fail.clone())?;
    }

    if total
       != res_vars.get(rv)
                  .ok_or_else(|| fail.clone())?
                  .0
                  .amount
                  .ok_or_else(|| fail.clone())?
    {
      return Err(fail.clone());
    }
  }

  /*
   * At this point we have:
   *  - Validated the ResourceVar <-> TXO correspondence
   *  - Checked that the transactions match the given template
   *  - Checked that all stated transfers match in asset type
   *  - Computed all the various values within the policy, erroring out
   *    if anything has an ill-defined effect (eg overflow)
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
