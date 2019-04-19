extern crate core;

use core::data_model::errors::PlatformError;
use core::data_model::Transaction;
use core::store::{ArchiveAccess, LedgerAccess};

pub trait TransactionBuilder {
  fn add_operation_create_asset(&mut self);
  fn add_operation_issue_asset(&mut self);
  fn add_operation_transfer_asset(&mut self);
  fn serialize(&self) -> Result<[u8], PlatformError>;
}

pub struct LedgerTransactionBuilder {
  txn: Transaction,
  ledger: &LedgerAccess,
  archive: &ArchiveAccess,
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
