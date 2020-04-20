#![deny(warnings)]
use cryptohash::sha256::Digest as BitDigest;
use ledger::store::{LedgerAccess, LedgerState};
use std::path::Path;

fn log_test(logfile: &Path, outfile: Option<&str>, expected_file: Option<&str>) {
  let tmp_dir = findora::fresh_tmp_dir();

  let mut target_file = tmp_dir.clone();
  target_file.push("txn_log");
  std::fs::copy(logfile, target_file.into_boxed_path()).unwrap();

  // from `load_or_init`
  let block_merkle = tmp_dir.join("block_merkle");
  let block_merkle = block_merkle.to_str().unwrap();
  let air = tmp_dir.join("air");
  let air = air.to_str().unwrap();
  let txn_merkle = tmp_dir.join("txn_merkle");
  let txn_merkle = txn_merkle.to_str().unwrap();
  let txn_log = tmp_dir.join("txn_log");
  let txn_log = txn_log.to_str().unwrap();
  let utxo_map = tmp_dir.join("utxo_map");
  let utxo_map = utxo_map.to_str().unwrap();
  let sig_key_file_buf = tmp_dir.join("sig_key");
  let sig_key_file = sig_key_file_buf.to_str().unwrap();

  {
    let st = LedgerState::load_from_log(&block_merkle,
                                        &air,
                                        &txn_merkle,
                                        &txn_log,
                                        &utxo_map,
                                        Some(sig_key_file),
                                        None).unwrap();
    let comm = st.get_state_commitment();

    if let Some(outfile) = outfile {
      let comm_output = std::fs::File::create(&outfile).unwrap();
      bincode::serialize_into(&comm_output, &comm).unwrap();
      comm_output.sync_all().unwrap();
    }

    println!("{:?}", comm);

    if let Some(expected_file) = expected_file {
      let comm_expected = bincode::deserialize_from::<_,(BitDigest,u64)>(std::fs::File::open(&expected_file).unwrap()).unwrap();
      assert!(comm == comm_expected);
    }
  }

  std::fs::remove_dir_all(tmp_dir).unwrap();
}

#[test]
fn test_example_log() {
  log_test(Path::new("example_log"), None, Some("expected"));
}

fn main() {
  env_logger::init();

  let args = std::env::args().collect::<Vec<_>>();
  assert!(args.len() == 4);

  let logfile = Path::new(&args[1]);

  let outfile = if &args[2] != "-" {
    Some(args[2].as_str())
  } else {
    None
  };

  let expected_file = if &args[3] != "-" {
    Some(args[3].as_str())
  } else {
    None
  };

  log_test(logfile, outfile, expected_file);
}
