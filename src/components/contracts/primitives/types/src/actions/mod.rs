pub mod ethereum;
pub mod evm;
pub mod template;
pub mod xhub;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Ethereum(ethereum::Action),
    Evm(evm::Action),
    XHub(xhub::Action),
    Template(template::Action),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum LegacyAction {
    Ethereum(ethereum::LegacyAction),
    Evm(evm::Action),
    XHub(xhub::Action),
    Template(template::Action),
}
