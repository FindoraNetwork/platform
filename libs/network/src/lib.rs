#![deny(warnings)]
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  AssetType, AssetTypeCode, AuthenticatedKVLookup, BlockSID, FinalizedTransaction, KVBlind,
  StateCommitmentData, Transaction, TxoSID, Utxo,
};
use ledger::store::LedgerState;
use ledger_api_service::{
  ActixLedgerClient, MockLedgerClient, RestfulArchiveAccess, RestfulLedgerAccess,
};
use query_api::{ActixQueryServerClient, MockQueryServerClient, RestfulQueryServerAccess};
use serde::Serialize;
use sparse_merkle_tree::Key;
use std::sync::{Arc, RwLock};
use submission_api::{ActixLUClient, MockLUClient, RestfulLedgerUpdate};
use submission_server::{TxnHandle, TxnStatus};
use utils::{HashOf, SignatureOf, LEDGER_PORT, QUERY_PORT, SUBMIT_PORT};
use zei::xfr::sig::XfrPublicKey;

pub type MockLedgerStandalone =
  LedgerStandalone<MockLUClient, MockLedgerClient, MockQueryServerClient>;

pub struct LedgerStandalone<LU: RestfulLedgerUpdate,
 LA: RestfulLedgerAccess + RestfulArchiveAccess,
 Q: RestfulQueryServerAccess> {
  submission_server_client: LU,
  ledger_client: LA,
  query_server_client: Q,
}

pub struct HttpStandaloneConfig {
  ledger_port: usize,
  submit_port: usize,
  query_port: usize,
  host: String,
  protocol: String,
}

impl HttpStandaloneConfig {
  pub fn testnet() -> Self {
    HttpStandaloneConfig { ledger_port: LEDGER_PORT,
                           submit_port: SUBMIT_PORT,
                           query_port: QUERY_PORT,
                           host: String::from("testnet.findora.org"),
                           protocol: String::from("https") }
  }

  pub fn local() -> Self {
    HttpStandaloneConfig { ledger_port: LEDGER_PORT,
                           submit_port: SUBMIT_PORT,
                           query_port: QUERY_PORT,
                           host: String::from("localhost"),
                           protocol: String::from("http") }
  }
}

impl LedgerStandalone<MockLUClient, MockLedgerClient, MockQueryServerClient> {
  /// Creates a ledger standalone client that talks to fake servers, no ports required
  pub fn new_mock(block_capacity: usize) -> Self {
    let ledger_state = LedgerState::test_ledger();
    let state_lock = Arc::new(RwLock::new(ledger_state));
    let ledger_client = MockLedgerClient::new(&state_lock);
    let submission_server_client = MockLUClient::new(&state_lock, block_capacity);
    let query_server_client = MockQueryServerClient();

    LedgerStandalone { submission_server_client,
                       query_server_client,
                       ledger_client }
  }
}

impl LedgerStandalone<ActixLUClient, ActixLedgerClient, ActixQueryServerClient> {
  /// Creates a ledger standalone client that communicates with live Findora servers
  pub fn new_http(config: &HttpStandaloneConfig) -> Self {
    let ledger_client = ActixLedgerClient::new(config.ledger_port, &config.host, &config.protocol);
    let submission_server_client =
      ActixLUClient::new(config.submit_port, &config.host, &config.protocol);
    let query_server_client =
      ActixQueryServerClient::new(config.query_port, &config.host, &config.protocol);

    LedgerStandalone { submission_server_client,
                       query_server_client,
                       ledger_client }
  }
}

impl<LU: RestfulLedgerUpdate,
      LA: RestfulLedgerAccess + RestfulArchiveAccess,
      Q: RestfulQueryServerAccess> RestfulArchiveAccess for LedgerStandalone<LU, LA, Q>
{
  fn get_blocks_since(&self,
                      addr: BlockSID)
                      -> Result<Vec<(usize, Vec<FinalizedTransaction>)>, PlatformError> {
    self.ledger_client.get_blocks_since(addr)
  }

  fn get_source(&self) -> String {
    self.ledger_client.get_source()
  }
}

impl<LU: RestfulLedgerUpdate,
      LA: RestfulLedgerAccess + RestfulArchiveAccess,
      Q: RestfulQueryServerAccess> RestfulQueryServerAccess for LedgerStandalone<LU, LA, Q>
{
  fn store_custom_data(&mut self,
                       data: &dyn AsRef<[u8]>,
                       key: &Key,
                       blind: Option<KVBlind>)
                       -> Result<(), PlatformError> {
    self.query_server_client.store_custom_data(data, key, blind)
  }

  fn fetch_custom_data(&self, key: &Key) -> Result<Vec<u8>, PlatformError> {
    self.query_server_client.fetch_custom_data(key)
  }
}

impl<LU: RestfulLedgerUpdate,
      LA: RestfulLedgerAccess + RestfulArchiveAccess,
      Q: RestfulQueryServerAccess> RestfulLedgerAccess for LedgerStandalone<LU, LA, Q>
{
  fn get_utxo(&self, addr: TxoSID) -> Result<Utxo, PlatformError> {
    self.ledger_client.get_utxo(addr)
  }

  fn get_issuance_num(&self, code: &AssetTypeCode) -> Result<u64, PlatformError> {
    self.ledger_client.get_issuance_num(code)
  }

  fn get_asset_type(&self, code: &AssetTypeCode) -> Result<AssetType, PlatformError> {
    self.ledger_client.get_asset_type(code)
  }

  #[allow(clippy::type_complexity)]
  fn get_state_commitment(
    &self)
    -> Result<(HashOf<Option<StateCommitmentData>>,
               u64,
               SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>),
              PlatformError> {
    self.ledger_client.get_state_commitment()
  }

  fn get_kv_entry(&self, addr: Key) -> Result<AuthenticatedKVLookup, PlatformError> {
    self.ledger_client.get_kv_entry(addr)
  }

  fn public_key(&self) -> Result<XfrPublicKey, PlatformError> {
    self.ledger_client.public_key()
  }

  fn sign_message<T: Serialize + serde::de::DeserializeOwned>(
    &self,
    msg: &T)
    -> Result<SignatureOf<T>, PlatformError> {
    self.ledger_client.sign_message(msg)
  }
}

impl<LU: RestfulLedgerUpdate,
      LA: RestfulLedgerAccess + RestfulArchiveAccess,
      Q: RestfulQueryServerAccess> RestfulLedgerUpdate for LedgerStandalone<LU, LA, Q>
{
  fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle, PlatformError> {
    self.submission_server_client.submit_transaction(txn)
  }
  fn force_end_block(&mut self) -> Result<(), PlatformError> {
    self.submission_server_client.force_end_block()
  }
  fn txn_status(&self, handle: &TxnHandle) -> Result<TxnStatus, PlatformError> {
    self.submission_server_client.txn_status(handle)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::{Memo, Operation, UpdateMemo, UpdateMemoBody};
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use zei::xfr::sig::XfrKeyPair;

  #[test]
  fn test_mock_client() {
    let tx = Transaction::from_seq_id(0);
    let mut mock_rest_client = LedgerStandalone::new_mock(2);
    let handle = mock_rest_client.submit_transaction(&tx).unwrap();
    mock_rest_client.force_end_block().unwrap();
    let status = mock_rest_client.txn_status(&handle).unwrap();
    if let TxnStatus::Committed(_comm) = status {
      assert!(true);
    } else {
      assert!(false);
    }
  }

  #[test]
  fn test_mock_client_failure() {
    let code = AssetTypeCode { val: [1; 16] };
    let new_memo = Memo("new_memo".to_string());
    let mut prng = ChaChaRng::from_entropy();
    let creator = XfrKeyPair::generate(&mut prng);
    let adversary = XfrKeyPair::generate(&mut prng);
    let mut memo_update = UpdateMemo::new(UpdateMemoBody { new_memo: new_memo.clone(),
                                                           asset_type: code },
                                          &creator);
    let tx = Transaction::from_operation(Operation::UpdateMemo(memo_update.clone()), 0);
    let mut mock_rest_client = LedgerStandalone::new_mock(2);
    assert!(mock_rest_client.submit_transaction(&tx).is_err());
  }
}
