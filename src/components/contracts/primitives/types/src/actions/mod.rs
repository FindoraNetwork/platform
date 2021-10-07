pub mod account;
pub mod ethereum;
pub mod evm;
pub mod template;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Ethereum(ethereum::Action),
    Evm(evm::Action),
    Account(account::Action),
    Template(template::Action),
}
