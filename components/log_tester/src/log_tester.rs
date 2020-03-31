#![deny(warnings)]
use cryptohash::sha256::Digest as BitDigest;
use ledger::store::{ArchiveAccess, LedgerState};
use std::path::Path;

fn main() {
  let args = std::env::args().collect::<Vec<_>>();
  assert!(args.len() == 4);

  let tmp_dir = findora::fresh_tmp_dir();

  let logfile = Path::new(&args[1]);
  let mut target_file = tmp_dir.clone();
  target_file.push("txn_log");
  std::fs::copy(logfile, target_file.into_boxed_path()).unwrap();

  env_logger::init();

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

    if &args[2] != "-" {
      let comm_output = std::fs::File::create(&args[2]).unwrap();
      bincode::serialize_into(&comm_output, &comm).unwrap();
      comm_output.sync_all().unwrap();
    }

    println!("{:?}", comm);

    if &args[3] != "-" {
      let comm_expected = bincode::deserialize_from::<_,(BitDigest,u64)>(std::fs::File::open(&args[3]).unwrap()).unwrap();
      assert!(comm == comm_expected);
    }
  }

  std::fs::remove_dir_all(tmp_dir).unwrap();
}
