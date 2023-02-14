use ethereum::TransactionV0 as LegcayTransaction;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Transact(LegcayTransaction),
}
