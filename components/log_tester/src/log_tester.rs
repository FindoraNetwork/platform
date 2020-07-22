#![deny(warnings)]
use ledger::data_model::errors::PlatformError;
use ledger::error_location;
use ledger::store::LoggedBlock;
use ledger::store::{LedgerAccess, LedgerState};
use ledger_api_service::{ActixLedgerClient, RestfulLedgerAccess};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use submission_api::{ActixLUClient, RestfulLedgerUpdate};
use utils::HashOf;

fn run_log_against<LU, LA>(submit: &mut LU,
                           access: &LA,
                           logfile: &Path,
                           outfile: Option<&str>,
                           expected_file: Option<&str>)
  where LU: RestfulLedgerUpdate,
        LA: RestfulLedgerAccess
{
  // Check that we're starting from an empty ledger
  let init_comm = access.get_state_commitment().unwrap();
  println!("{:?}", init_comm);
  assert!(init_comm.1 == 0);

  let access_key = access.public_key().unwrap();

  // effectively copied from load_transaction_log
  let blocks = (|| {
    let file = File::open(logfile).unwrap();
    let reader = BufReader::new(file);
    let mut v = Vec::new();
    for l in reader.lines() {
      let l = l.unwrap();
      match serde_json::from_str::<LoggedBlock>(&l) {
        Ok(next_block) => {
          v.push(next_block);
        }
        Err(e) => {
          if l != "" {
            return Err(PlatformError::DeserializationError(format!("[{}]: {:?}",
                                                                   &error_location!(),
                                                                   e)));
          }
        }
      }
    }
    Ok(v)
  })();
  let blocks = blocks.unwrap();

  for (ix, logged_block) in blocks.into_iter().enumerate() {
    println!("{}: {}", ix, serde_json::to_string(&logged_block).unwrap());
    let (comm, block) = (logged_block.state, logged_block.block);

    let (prev_comm, prev_count, prev_sig) = access.get_state_commitment().unwrap();
    prev_sig.verify(&access_key, &(prev_comm.clone(), prev_count))
            .unwrap();

    if prev_count != ix as u64 {
      panic!("{:?}",
             PlatformError::CheckedReplayError(format!("{}:{}:{}, {} != {}",
                                                       std::file!(),
                                                       std::line!(),
                                                       std::column!(),
                                                       ix,
                                                       prev_count)));
    }

    if prev_comm != comm.previous_state_commitment {
      panic!("{:?}",
             PlatformError::CheckedReplayError(format!("{}:{}:{}",
                                                       std::file!(),
                                                       std::line!(),
                                                       std::column!())));
    }

    for txn in block {
      submit.submit_transaction(&txn).unwrap();
    }

    submit.force_end_block().unwrap();
  }

  let (final_comm, final_count, final_sig) = access.get_state_commitment().unwrap();
  final_sig.verify(&access_key, &(final_comm.clone(), final_count))
           .unwrap();

  let final_comm = (final_comm, final_count);

  if let Some(outfile) = outfile {
    let comm_output = std::fs::File::create(&outfile).unwrap();
    serde_json::to_writer(&comm_output, &final_comm).unwrap();
    comm_output.sync_all().unwrap();
  }

  println!("{:?}", final_comm);

  if let Some(expected_file) = expected_file {
    let comm_expected = serde_json::from_reader::<_,(HashOf<_>,u64)>(std::fs::File::open(&expected_file).unwrap()).unwrap();
    assert!(final_comm == comm_expected);
  }
}

fn log_test(logfile: &Path, outfile: Option<&str>, expected_file: Option<&str>) {
  let tmp_dir = utils::fresh_tmp_dir();

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
      serde_json::to_writer(&comm_output, &comm).unwrap();
      comm_output.sync_all().unwrap();
    }

    println!("{:?}", comm);

    if let Some(expected_file) = expected_file {
      let comm_expected = serde_json::from_reader::<_,(HashOf<_>,u64)>(std::fs::File::open(&expected_file).unwrap()).unwrap();
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
  flexi_logger::Logger::with_env().start().unwrap();

  let args = std::env::args().collect::<Vec<_>>();
  match args.len() {
    4 => {
      // <exec> logfile outfile expected_file
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
    9 => {
      // <exec> logfile outfile expected_file protocol suburl subport accurl accport
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

      let protocol = args[4].clone();
      let (suburl, subport) = (args[5].clone(), args[6].parse::<usize>().unwrap());
      let (accurl, accport) = (args[7].clone(), args[8].parse::<usize>().unwrap());

      let mut submit = ActixLUClient::new(subport, &suburl, &protocol);
      let access = ActixLedgerClient::new(accport, &accurl, &protocol);

      run_log_against(&mut submit, &access, logfile, outfile, expected_file);
    }
    x => panic!("expected 4 or 9 arguments, got {}", x),
  }
}
