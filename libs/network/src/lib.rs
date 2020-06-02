//#![deny(warnings)]
use actix_service::Service;
use actix_web::test::TestRequest;
use actix_web::{test, web, App};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  AssetType, AssetTypeCode, AuthenticatedKVLookup, BlockSID, FinalizedTransaction, KVBlind,
  StateCommitmentData, Transaction, TxoSID, Utxo,
};
use ledger::store::LedgerState;
use ledger_api_service::{MockLedgerClient, RestfulArchiveAccess, RestfulLedgerAccess};
use query_api::RestfulQueryServerAccess;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::Serialize;
use sparse_merkle_tree::Key;
use std::sync::{Arc, RwLock};
use submission_api::{MockLUClient, RestfulLedgerUpdate};
use submission_server::{TxnHandle, TxnStatus};
use utils::{HashOf, NetworkRoute, SignatureOf};
use zei::xfr::sig::XfrPublicKey;

/// Mock rest client that simulates http requests for all servers, no ports required
pub struct MockLedgerStandalone {
  mock_submission_server: MockLUClient,
  mock_ledger: MockLedgerClient,
}

impl MockLedgerStandalone {
  pub fn new(block_capacity: usize) -> Self {
    let ledger_state = LedgerState::test_ledger();
    let state_lock = Arc::new(RwLock::new(ledger_state));
    let mock_ledger = MockLedgerClient::new(&state_lock);
    let mock_submission_server = MockLUClient::new(&state_lock, 1);

    MockLedgerStandalone { mock_submission_server,
                           mock_ledger }
  }
}

impl RestfulArchiveAccess for MockLedgerStandalone {
  fn get_blocks_since(&self,
                      addr: BlockSID)
                      -> Result<Vec<(usize, Vec<FinalizedTransaction>)>, PlatformError> {
    self.mock_ledger.get_blocks_since(addr)
  }
}

impl RestfulQueryServerAccess for MockLedgerStandalone {
  fn store_custom_data(&mut self,
                       data: &dyn AsRef<[u8]>,
                       key: &Key,
                       blind: Option<KVBlind>)
                       -> Result<(), PlatformError> {
    unimplemented!();
  }

  fn fetch_custom_data(&self, key: &Key) -> Result<Vec<u8>, PlatformError> {
    unimplemented!();
  }
}

impl RestfulLedgerAccess for MockLedgerStandalone {
  fn get_utxo(&self, addr: TxoSID) -> Result<Utxo, PlatformError> {
    self.mock_ledger.get_utxo(addr)
  }

  fn get_issuance_num(&self, code: &AssetTypeCode) -> Result<u64, PlatformError> {
    self.mock_ledger.get_issuance_num(code)
  }

  fn get_asset_type(&self, code: &AssetTypeCode) -> Result<AssetType, PlatformError> {
    self.mock_ledger.get_asset_type(code)
  }

  fn get_state_commitment(&self)
                          -> Result<(HashOf<Option<StateCommitmentData>>, u64), PlatformError> {
    self.mock_ledger.get_state_commitment()
  }

  fn get_kv_entry(&self, addr: Key) -> Result<AuthenticatedKVLookup, PlatformError> {
    self.mock_ledger.get_kv_entry(addr)
  }

  fn public_key(&self) -> Result<XfrPublicKey, PlatformError> {
    self.mock_ledger.public_key()
  }

  fn sign_message<T: Serialize + serde::de::DeserializeOwned>(
    &self,
    msg: &T)
    -> Result<SignatureOf<T>, PlatformError> {
    self.mock_ledger.sign_message(msg)
  }
}

impl RestfulLedgerUpdate for MockLedgerStandalone {
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError> {
    self.mock_submission_server.submit_transaction(txn)
  }
  fn force_end_block(&mut self) -> Result<(), PlatformError> {
    self.mock_submission_server.force_end_block()
  }
  fn txn_status(&self, handle: &TxnHandle) -> Result<TxnStatus, PlatformError> {
    self.mock_submission_server.txn_status(handle)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_mock_client() {
    let tx = Transaction::default();
    let mut mock_rest_client = MockRestClient::new(2);
    let handle = mock_rest_client.submit_transaction(&tx).unwrap();
    mock_rest_client.force_end_block().unwrap();
    let status = mock_rest_client.txn_status(&handle).unwrap();
    if let TxnStatus::Committed(_comm) = status {
      assert!(true);
    } else {
      assert!(false);
    }
  }
}
