
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

enum IdOp {
    OwnerOf(ResourceVar),
    GlobalId(IdVar),
}

enum AmountOp {
    Global(AmountVar),
    Const(u64),
    Plus(AmountVar,AmountVar),
    Minus(AmountVar,AmountVar),
    Times(AmountVar,AmountVar),
    Round(FractionVar),
}

enum FractionOp {
    Global(FractionVar),
    Const(Fraction),
    Plus(FractionVar,FractionVar),
    Times(FractionVar,FractionVar),
    AmtTimes(AmountVar,FractionVar),
    TimesAmt(FractionVar,AmountVar),
}

enum ResourceTypeOp {
    GlobalResType(ResourceTypeVar),
    TypeOfResource(ResourceVar),
}

enum BoolOp {
    Const(bool),
    IdEq(IdVar,IdVar),
    AmtEq(AmountVar,AmountVar),
    FracEq(FractionVar,FractionVar),
    ResourceTypeEq(ResourceTypeVar,ResourceTypeVar),

    Not(BoolVar),
    And(BoolVar,BoolVar),
    Or(BoolVar,BoolVar),

    AmtGe(AmountVar,AmountVar),
    FracGe(FractionVar,FractionVar),
}

enum TxnOp {
    Issue(AmountVar,ResourceTypeVar,ResourceVar),
    Transfer(AmountVar,ResourceVar,Option<ResourceVar>), // None is a burn address
}

// TxnOps, but corresponding correctly to how `Op`s work on the ledger
// ie, multi-input multi-output
enum RealTxnOp {
    Issue(ResourceTypeVar,Vec<(AmountVar,ResourceVar)>),
    Transfer(Vec<(AmountVar,ResourceVar,Option<ResourceVar>)>), // None is a burn address
}

struct TxnCheck {
    num_in_params: u64,
    num_out_params: u64,
    id_ops: Vec<IdOp>,
    rt_ops: Vec<ResourceTypeOp>,

    // These actually need to execute back & forth, which feels like maybe
    // a problem
    fraction_ops: Vec<FractionOp>,
    amount_ops: Vec<AmountOp>,

    txn_template: Vec<TxnOp>,
}

struct Policy {
    // Implicitly: bound asset type and issuer as AssetType & Id globals
    // respectively
    num_id_globals: u64,
    num_rt_globals: u64,
    num_amt_globals: u64,
    num_frac_globals: u64,

    init_check: TxnCheck,

    txn_choices: Vec<TxnCheck>,
}

fn run_txn_check(check: &TxnCheck, mut id_vars: Vec<XfrPublicKey>,
                 mut rt_vars: Vec<AssetTypeCode>, mut amt_vars: Vec<u64>,
                 mut frac_vars: Vec<Fraction>,
                 txn: &Transaction) -> Result<(), PlatformError> {
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

    let mut pending_inputs = HashSet<ResourceVar>::new();
    let mut pending_outputs = HashSet<ResourceVar>::new();
    let mut pending_op: Option<RealTxnOp> = None;

    let mut used_resources = HashSet<ResourceVar>::new();
    let mut real_ops = Vec<RealTxnOp>::new();
    for op in check.txn_template.iter() {
        debug_assert!(pending_inputs.is_disjoint(used_resources));
        debug_assert!(pending_inputs.is_disjoint(pending_outputs));
        debug_assert!(pending_outputs.is_disjoint(used_resources));

        match op {
            TxnOp::Issue(amt,rt,res) => {
                // If it's already accounted for, error.
                if used_resources.contains(res) || pending_outputs.contains(res) {
                    return Err(PlatformError::InputsError);
                }
                if let Some(pending) = pending_op {
                    match pending {
                        RealTxnOp::Transfer(_,_) => {
                            real_ops.push(pending);
                            used_resources.extend(pending_inputs.drain());
                            used_resources.extend(pending_outputs.drain());
                            pending_op = Some(RealTxnOp::Issue(rt,vec![(amt,res)]));
                        },
                        RealTxnOp::Issue(pending_rt,pending_dsts) => {
                            debug_assert!(pending_inputs.is_empty());
                            if pending_rt != rt {
                                real_ops.push(pending);
                                used_resources.extend(pending_outputs.drain());
                                pending_op = Some(RealTxnOp::Issue(rt,vec![(amt,res)]));
                            } else {
                                pending_dsts.push((amt,res));
                            }
                        },
                    }
                } else {
                    pending_op = Some(RealTxnOp::Issue(rt,vec![(amt,res)]));
                }
                pending_outputs.insert(res);
            },
            TxnOp::Transfer(amt,inp,out) => {
                // If inp or out are already accounted for, error.
                if used_resources.contains(inp) {
                    return Err(PlatformError::InputsError);
                }
                if let Some(out_res) {
                    if used_resources.contains(out_res) {
                        return Err(PlatformError::InputsError);
                    }
                }

                if let Some(pending) = pending_op {
                    match pending {
                        RealTxnOp::Issue(_,_) => {
                            if pending_outputs.contains(inp) {
                                return Err(PlatformError::InputsError);
                            }
                            if let Some(out_res) {
                                if pending_outputs.contains(out_res) {
                                    return Err(PlatformError::InputsError);
                                }
                            }
                            real_ops.push(pending);
                            debug_assert!(pending_inputs.is_empty());
                            used_resources.extend(pending_outputs.drain());
                            pending = Some(RealTxnOp::Transfer(vec![(amt,inp,out)]));
                        },
                        RealTxnOp::Transfer(transfers) => {
                            // if an input gets used as an output or
                            // vice-versa, error.
                            if pending_outputs.contains(inp) {
                                return Err(PlatformError::InputsError);
                            }
                            if let Some(out_res) {
                                if pending_inputs.contains(out_res) {
                                    return Err(PlatformError::InputsError);
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
                                        return Err(PlatformError::InputsError);
                                    }
                                }
                            }
                            transfers.push((amt,inp,out));
                        },
                    }
                } else {
                    pending = Some(RealTxnOp::Transfer(vec![(amt,inp,out)]));
                }

                pending_inputs.insert(inp);
                if let Some(out_res) = out {
                    pending_outputs.insert(out_res);
                }
            },
        }
    }

    debug_assert!(pending_inputs.is_disjoint(used_resources));
    debug_assert!(pending_inputs.is_disjoint(pending_outputs));
    debug_assert!(pending_outputs.is_disjoint(used_resources));

    if let Some(final_op) = pending_op {
        real_ops.push(pending);
        used_resources.extend(pending_inputs.drain());
        used_resources.extend(pending_outputs.drain());
        final_op = None;
    }

    debug_assert!(final_op == None);

    // check that all resources in the index range are used
    for ix in 0..used_resources.len() {
        if !used_resources.contains(ix) {
            return Err(PlatformError::InputsError);
        }
    }

    /*
     * -------- Match the RealTxnOps to the AssetRecords -----
     */

    if real_ops.len() != txn.ops.len() {
        return Err(PlatformError::InputsError);
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

    let mut res_vars = HashMap<ResourceVar,TxOutput>::new();
    res_vars.reserve(used_resources.len());
    let mut res_totals = HashMap<ResourceVar,Vec<AmountVar>>::new();

    for (target,real) in real_ops.iter().zip(txn.ops.iter()) {
        match (target,real) {
            (RealTxnOp::Issue(rt,outs), Operation::IssueAsset(iss)) => {
                debug_assert!(!outs.is_empty());
                // NOTE: This relies on the no-repeated-outputs invariant
                // from before
                if outs.len() != iss.body.records.len() {
                    return Err(PlatformError::InputsError);
                }

                res_vars.extend(outs.iter().map(|(_,rv)| rv)
                                .zip(iss.body.records.iter().cloned()));
                res_totals.extend(outs.iter().map(|(amt,rv)| (rv,vec![amt])));
            },
            (RealTxnOp::Transfer(transfers), Operation::TransferAsset(trn)) => {
                debug_assert!(!transfers.is_empty());
                let mut inp_ix = 0;
                let mut out_ix = 0;

                for (amt,inp,out) in transfers.iter() {
                    let mut asset_type: AssetType;
                    if let Some(inp_txo) = res_vars.get(inp) {
                        debug_assert!(!res_vars.contains(out));
                        res_totals.get_mut(inp).as_ref().unwrap().push(amt);

                        debug_assert!(inp_txo.asset_type.is_some());

                        asset_type = inp_txo.asset_type.unwrap();
                    } else {
                        debug_assert!(!res_totals.contains_key(inp));

                        let inp_txo = trn.body.transfer.body.inputs.get(inp_ix)?;
                        asset_type = inp_txo.asset_type?;

                        res_vars.insert((inp,inp_txo.clone()));
                        res_totals.insert((inp,vec![amt]));

                        inp_ix += 1;
                    }

                    if let Some(out_res) = out {
                        if let Some(out_txo) = res_vars.get(out) {
                            debug_assert!(!res_vars.contains(inp));
                            res_totals.get_mut(out).as_ref().unwrap().push(amt);

                            debug_assert!(out_txo.asset_type.is_some());

                            if asset_type != out_txo.asset_type.unwrap() {
                                return Err(PlatformError::InputsError);
                            }
                        } else {
                            debug_assert!(!res_totals.contains_key(out));

                            let out_txo = trn.body.transfer.body.outputs.get(out_ix)?;

                            if out_txo.asset_type? != asset_type {
                                return Err(PlatformError::InputsError);
                            }

                            res_vars.insert((out,out_txo.clone()));
                            res_totals.insert((out,vec![amt]));

                            out_ix += 1;
                        }
                    } else {
                        let null_public_key = XfrPublicKey::zei_from_bytes(&[0; 32]);
                        let out_txo = trn.body.transfer.body.outputs.get(out_ix)?;

                        // TODO(joe): maybe move this later?
                        if out_txo.asset_type? != asset_type {
                            return Err(PlatformError::InputsError);
                        }

                        if out_txo.public_key != null_public_key {
                            return Err(PlatformError::InputsError);
                        }

                        out_ix += 1;
                    }
                }

                if inp_ix < trn.body.transfer.body.inputs.len() {
                    return Err(PlatformError::InputsError);
                }
                if out_ix < trn.body.transfer.body.outputs.len() {
                    return Err(PlatformError::InputsError);
                }
            },
            _ => {
                return Err(PlatformError::InputsError);
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
     *    AssetVars
     */

    /*
     * -------- Now we can actually run the policy! (jeez) -------
     */

    unimplemented!();
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
 *        list. If min_[which]_index is after our current position, fail.
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

