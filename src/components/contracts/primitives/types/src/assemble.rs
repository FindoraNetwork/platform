use crate::actions::ethereum::Action as EtherAction;
use crate::actions::Action;
use crate::crypto::{Address, Signature};
use crate::transaction;
use ethereum::TransactionV2 as Transaction;
use primitive_types::U256;
use ruc::*;
use serde::{Deserialize, Serialize};

// Same as baseapp/src/extensions/SignedExtra used by wasm
pub type SignedExtra = (CheckNonce, CheckFee);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CheckNonce(U256);

impl CheckNonce {
    pub fn new(nonce: U256) -> Self {
        CheckNonce(nonce)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CheckFee(Option<U256>);

impl CheckFee {
    pub fn new(fee: Option<U256>) -> Self {
        CheckFee(fee)
    }
}

/// Unchecked transaction type as expected by this application.
pub type UncheckedTransaction<Extra> =
    transaction::UncheckedTransaction<Address, Action, Signature, Extra>;

/// Transaction type that has already been checked.
pub type CheckedTransaction<Extra> =
    transaction::CheckedTransaction<Address, Action, Extra>;

/// Convert base action to sub module action within CheckedTransaction
/// if tx is unsigned transaction.
pub fn convert_unsigned_transaction<Action, Extra>(
    action: Action,
    tx: CheckedTransaction<Extra>,
) -> transaction::CheckedTransaction<Address, Action, Extra> {
    transaction::CheckedTransaction {
        signed: tx.signed,
        function: action,
    }
}

/// Convert raw transaction to unchecked transaction.
pub fn convert_unchecked_transaction<'a, Extra: Deserialize<'a>>(
    transaction: &'a [u8],
) -> Result<UncheckedTransaction<Extra>> {
    serde_json::from_slice::<UncheckedTransaction<Extra>>(transaction)
        .map_err(|e| eg!(e))
}

/// Convert raw ethereum transaction to unified format unchecked transaction.
pub fn convert_ethereum_transaction<Extra>(
    transaction: &[u8],
) -> Result<UncheckedTransaction<Extra>> {
    let tx = serde_json::from_slice::<Transaction>(transaction).map_err(|e| eg!(e))?;
    Ok(UncheckedTransaction::<Extra>::new_unsigned(
        Action::Ethereum(EtherAction::Transact(tx)),
    ))
}
