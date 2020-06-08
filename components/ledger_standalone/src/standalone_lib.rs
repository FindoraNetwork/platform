#![deny(warnings)]
use cryptohash::sha256::Digest as BitDigest;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{Transaction, TxoSID};
use ledger::error_location;
use std::ffi::OsString;
use std::time::{Duration, SystemTime};
use submission_server::{TxnHandle, TxnStatus};
use subprocess::{Popen, PopenConfig};
use zei::xfr::sig::XfrSignature;
use zei::xfr::structs::BlindAssetRecord;

macro_rules! fail {
  () => {
    PlatformError::SubmissionServerError(error_location!())
  };
  ($s:expr) => {
    PlatformError::SubmissionServerError(format!("[{}] {}", &error_location!(), &$s))
  };
}

const POLL_TIME: u64 = 40000;
// Struct that spins a standalone server up and down. Useful for testing.
// When instantiated, the struct will spin up a standalone ledger and submission server.
// While the struct is in scope, helper methods can be called to make queries to the standalone
// and submission server.
//
//
pub struct LedgerStandalone {
  ledger: Popen,
  submit_port: usize,
  ledger_port: usize,
  client: reqwest::Client,
  poll_time: Duration,
}

impl Drop for LedgerStandalone {
  fn drop(&mut self) {
    self.ledger.terminate().unwrap();
    if self.ledger.wait_timeout(Duration::from_millis(10)).is_err() {
      self.ledger.kill().unwrap();
      self.ledger.wait().unwrap();
    }
  }
}

impl Default for LedgerStandalone {
  fn default() -> Self {
    Self::new()
  }
}

impl LedgerStandalone {
  pub fn new() -> Self {
    LedgerStandalone{
        ledger: Popen::create(&["/usr/bin/env", "bash", "-c", "cargo run"],
                  PopenConfig {
                    cwd: Some(OsString::from("../ledger_standalone/")),
                    ..Default::default()
                  }).unwrap(),
        submit_port: 8669,
        ledger_port: 8668,
        poll_time: Duration::from_millis(POLL_TIME),
        client: reqwest::Client::new() }
  }

  // Attempt to ping the ledger server until poll_time elapses. Returns an error if
  // ledger server and submission server cannot be reached within poll_durations milliseconds.
  pub fn poll_until_ready(&self) -> Result<(), PlatformError> {
    let mut poll_duration = Duration::new(0, 0);
    while poll_duration < self.poll_time {
      let now = SystemTime::now();
      // do polling here
      let query1 = format!("http://localhost:{}/ping/", &self.submit_port);
      let query2 = format!("http://localhost:{}/ping/", &self.ledger_port);
      let ledger_ping_res = &self.client.get(&query1).send();
      let submission_server_ping_res = &self.client.get(&query2).send();
      if ledger_ping_res.is_ok() && submission_server_ping_res.is_ok() {
        return Ok(());
      }
      poll_duration += Duration::from_secs(now.elapsed().unwrap().as_secs());
    }
    Err(fail!())
  }

  // Submits a transaction to the standalone server
  pub fn submit_transaction(&self, tx: &Transaction) {
    let host = "localhost";
    let port = format!("{}", self.submit_port);
    let query1 = format!("http://{}:{}/submit_transaction", host, port);
    let query2 = format!("http://{}:{}/force_end_block", host, port);
    self.client
        .post(&query1)
        .json(&tx)
        .send()
        .unwrap()
        .error_for_status()
        .unwrap()
        .text()
        .unwrap();
    self.client
        .post(&query2)
        .send()
        .unwrap()
        .error_for_status()
        .unwrap()
        .text()
        .unwrap();
  }

  // Submits a transaction to the standalone server and fetch the UTXO SIDs
  pub fn submit_transaction_and_fetch_utxos(&self, tx: &Transaction) -> Vec<TxoSID> {
    // Submit the transaction
    let host = "localhost";
    let port = format!("{}", self.submit_port);
    let query1 = format!("http://{}:{}/submit_transaction", host, port);
    let query2 = format!("http://{}:{}/force_end_block", host, port);
    let text = self.client
                   .post(&query1)
                   .json(&tx)
                   .send()
                   .unwrap()
                   .error_for_status()
                   .unwrap()
                   .text()
                   .unwrap();
    self.client
        .post(&query2)
        .send()
        .unwrap()
        .error_for_status()
        .unwrap()
        .text()
        .unwrap();

    // Fetch the UTXO SIDs
    let handle = serde_json::from_str::<TxnHandle>(&text).unwrap_or_else(|e| {
                                                           panic!("<Invalid JSON> ({}): \"{}\"",
                                                                  &e, &text)
                                                         });
    let query3 = format!("http://{}:{}/txn_status/{}", host, port, &handle.0);
    match serde_json::from_str::<TxnStatus>(&self.client
                                                 .get(&query3)
                                                 .send()
                                                 .unwrap()
                                                 .error_for_status()
                                                 .unwrap()
                                                 .text()
                                                 .unwrap()).unwrap()
    {
      TxnStatus::Committed((_sid, txos)) => txos,
      _ => panic!("Failed to fetch UTXO SIDs"),
    }
  }

  // Fetch a blind asset record at a given index. Useful for building transfer operations.
  pub fn fetch_blind_asset_record(&self, utxo_sid: TxoSID) -> BlindAssetRecord {
    let host = "localhost";
    let query = format!("http://{}:{}/utxo_sid/{}",
                        host, self.ledger_port, utxo_sid.0);
    serde_json::from_str::<BlindAssetRecord>(&self.client
                                                  .get(&query)
                                                  .send()
                                                  .unwrap()
                                                  .error_for_status()
                                                  .unwrap()
                                                  .text()
                                                  .unwrap()).unwrap()
  }

  // Fetch a blind asset record at a given index. Useful for building transfer operations.
  pub fn fetch_global_state(&self) -> (BitDigest, u64, XfrSignature) {
    let host = "localhost";
    let query = format!("http://{}:{}/global_state", host, self.ledger_port);
    serde_json::from_str::<(BitDigest, u64, XfrSignature)>(&self.client
                                                                .get(&query)
                                                                .send()
                                                                .unwrap()
                                                                .error_for_status()
                                                                .unwrap()
                                                                .text()
                                                                .unwrap()).unwrap()
  }
}
