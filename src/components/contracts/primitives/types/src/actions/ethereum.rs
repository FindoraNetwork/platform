use ethereum::TransactionV0 as LegcayTransaction;
use ethereum::TransactionV2 as Transaction;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Transact(LegcayTransaction),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action2 {
    Transact(Transaction),
}
