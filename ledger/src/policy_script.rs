
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

fn run_txn_check(check: &TxnCheck, ) -> Result<()> {

}

/*
 * How should this work?
 * Each op generates a var, the only possible dependency cycle is fraction
 * <-> amount so some "cycle detection" is necessary.
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
 * Important checks:
 *  - overflow?
 *  - making sure everything's accounted for?
 *
 * The other important thing here is that a policy should be able to
 * generate a transaction passing the policy.
 *
 * This should yield a test -- if you have a policy, and you sample
 * transactions generated from the policy, it should pass the validation.
 *
 */

