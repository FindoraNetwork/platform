use ethereum::TransactionV0 as LegacyTransaction;
use ethereum::TransactionV2 as Transaction;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Transact(Transaction),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum LegacyAction {
    Transact(LegacyTransaction),
}
