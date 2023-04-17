//!
//! # Old Policy Logics
//!
//! Keep this moduler for compatible reasons,
//!
//! **DO NOT USE any code of this moduler** !!!
//!

#![allow(missing_docs)]

use {
    crate::data_model::AssetTypeCode,
    fixed::types::I20F12,
    serde::{Deserialize, Serialize},
    zei::noah_api::xfr::structs::AssetType,
    zei::XfrPublicKey,
};

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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub struct Fraction(pub I20F12);

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
