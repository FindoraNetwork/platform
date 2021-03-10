#![deny(warnings)]

use ledger::data_model::errors::PlatformError;
use ledger::store::LoggedBlock;
use ledger::store::{LedgerAccess, LedgerState};
use ledger_api_service::{ActixLedgerClient, RestfulLedgerAccess};
use log::info;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::thread;
use std::time;
use submission_api::{ActixLUClient, RestfulLedgerUpdate};
use submission_server::TxnStatus;
use utils::HashOf;

fn run_log_against<LU, LA>(
    submit: &mut LU,
    access1: &LA,
    access2: Option<&LA>,
    logfile: &Path,
    outfile: Option<&str>,
    expected_file: Option<&str>,
) where
    LU: RestfulLedgerUpdate,
    LA: RestfulLedgerAccess,
{
    let wait_time = time::Duration::from_millis(100);

    // Check that we're starting from an empty ledger
    let init_comm = access1.get_state_commitment().unwrap();
    println!("{:?}", init_comm);
    assert!(init_comm.1 == 0);

    if let Some(acc2) = access2 {
        assert!(init_comm.0 == acc2.get_state_commitment().unwrap().0);
    }

    let access_key = access1.public_key().unwrap();

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
                    if !l.is_empty() {
                        return Err(PlatformError::DeserializationError(Some(format!(
                            "[{}:{}:{}]: {:?}",
                            std::file!(),
                            std::line!(),
                            std::column!(),
                            e
                        ))));
                    }
                }
            }
        }
        Ok(v)
    })();
    let blocks = blocks.unwrap();

    for (ix, logged_block) in blocks.into_iter().enumerate() {
        info!("{}: {}", ix, serde_json::to_string(&logged_block).unwrap());
        let (comm, block) = (logged_block.state, logged_block.block);

        let (prev_comm, prev_count, prev_sig) = access1.get_state_commitment().unwrap();
        info!("comm:       {:?}", comm);
        info!("prev_count: {:?}", prev_count);
        info!("prev_comm:  {:?}", prev_comm);
        prev_sig
            .verify(&access_key, &(prev_comm.clone(), prev_count))
            .unwrap();

        if let Some(acc2) = access2 {
            let comm2 = acc2.get_state_commitment().unwrap();
            assert!(prev_comm == comm2.0);
            assert!(prev_count == comm2.1);
        }

        if prev_count != ix as u64 {
            panic!(
                "{:?}",
                PlatformError::CheckedReplayError(Some(format!(
                    "{}:{}:{}, {} != {}",
                    std::file!(),
                    std::line!(),
                    std::column!(),
                    ix,
                    prev_count
                )))
            );
        }

        if prev_comm != comm.previous_state_commitment {
            info!(
                "{:?}\n{:?}\n!=\n{:?}",
                PlatformError::CheckedReplayError(Some(format!(
                    "{}:{}:{}",
                    std::file!(),
                    std::line!(),
                    std::column!()
                ))),
                prev_comm,
                comm.previous_state_commitment
            );
        }

        let mut handles = vec![];
        for txn in block {
            let handle = submit.submit_transaction(&txn).unwrap();
            let mut i = 0;
            while let Err(e) = submit.txn_status(&handle) {
                if i % 10 == 0 {
                    info!("Waiting for {}: {}", handle, e);
                }
                thread::sleep(wait_time);
                i += 1;
            }
            handles.push(handle);
        }

        let mut new_comm;
        while {
            new_comm = access1.get_state_commitment().unwrap();
            new_comm.1 == prev_count
        } {
            info!("Waiting for block end: {:?}", new_comm);
            // submit.force_end_block().c(d!())?;
            thread::sleep(wait_time);
        }

        for h in handles {
            match submit.txn_status(&h) {
                Ok(TxnStatus::Committed((_txnsid, txo_sids))) => {
                    for txo in txo_sids {
                        assert!(
                            access1
                                .get_utxo(txo)
                                .or_else(|e| {
                                    eprintln!(
                                        "Got error: {:?}, waiting then retrying...",
                                        e
                                    );
                                    thread::sleep(wait_time);
                                    access1.get_utxo(txo)
                                })
                                .unwrap()
                                .is_valid(new_comm.0.clone())
                        );
                        if let Some(access2) = access2 {
                            assert!(
                                access2
                                    .get_utxo(txo)
                                    .or_else(|e| {
                                        eprintln!(
                                            "Got error: {:?}, waiting then retrying...",
                                            e
                                        );
                                        thread::sleep(wait_time);
                                        access2.get_utxo(txo)
                                    })
                                    .unwrap()
                                    .is_valid(new_comm.0.clone())
                            );
                        }
                    }
                }
                err => panic!("uncommitted handle {}: {:?}", h, err),
            }
        }
    }

    let (final_comm, final_count, final_sig) = access1.get_state_commitment().unwrap();
    final_sig
        .verify(&access_key, &(final_comm.clone(), final_count))
        .unwrap();

    if let Some(acc2) = access2 {
        let comm2 = acc2.get_state_commitment().unwrap();
        assert!(final_comm == comm2.0);
        assert!(final_count == comm2.1);
    }

    let final_comm = (final_comm, final_count);

    if let Some(outfile) = outfile {
        let comm_output = std::fs::File::create(&outfile).unwrap();
        serde_json::to_writer(&comm_output, &final_comm).unwrap();
        comm_output.sync_all().unwrap();
    }

    println!("{:?}", final_comm);

    if let Some(expected_file) = expected_file {
        let comm_expected = serde_json::from_reader::<_, (HashOf<_>, u64)>(
            std::fs::File::open(&expected_file).unwrap(),
        )
        .unwrap();
        if final_comm != comm_expected {
            info!("{:?} != {:?}", final_comm, comm_expected);
        }
    }
}

fn log_test(logfile: &Path, outfile: Option<&str>, expected_file: Option<&str>) {
    dbg!(logfile, &outfile, &expected_file);
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
        let st = LedgerState::load_from_log(
            &block_merkle,
            &air,
            &txn_merkle,
            &txn_log,
            &utxo_map,
            Some(sig_key_file),
            None,
        );
        let st = st.unwrap();
        let comm = st.get_state_commitment();

        if let Some(outfile) = outfile {
            let comm_output = std::fs::File::create(&outfile).unwrap();
            serde_json::to_writer(&comm_output, &comm).unwrap();
            comm_output.sync_all().unwrap();
        }

        println!("{:?}", comm);

        if let Some(expected_file) = expected_file {
            let comm_expected = serde_json::from_reader::<_, (HashOf<_>, u64)>(
                std::fs::File::open(&expected_file).unwrap(),
            )
            .unwrap();
            assert!(comm == comm_expected);
        }
    }

    std::fs::remove_dir_all(tmp_dir).unwrap();
}

// #[cfg(test)]
// mod tests {
//     use utils::TESTING_get_project_root;
//     #[test]
//     fn test_example_log() {
//         // TODO: Need to regenerate example_log so that public key can be verified
//         // Since adding proper deserialization to max_units in asset policies
//         // this test fails because the prior transaction body assumed a 'null' value
//         // would just be a missing field which is incorrect behavior for an Option type.
//         // Therefore the prior tx body that was signed was malformed so this test fails.
//         // It needs to be regenerated with a new keypair and signed and the
//         // expected and expected_log file must be updated for this test to pass.
//         // The billion dollar mistake strikes again!
//         let root = TESTING_get_project_root();
//         super::log_test(
//             &root
//                 .clone()
//                 .into_boxed_path()
//                 .join("components/trash/log_tester/example_log"),
//             None,
//             Some(
//                 &root
//                     .into_boxed_path()
//                     .join("components/trash/log_tester/expected")
//                     .into_os_string()
//                     .into_string()
//                     .unwrap(),
//             ),
//         );
//     }
// }

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
        11 => {
            // <exec> logfile outfile expected_file protocol suburl subport accurl1 accport1 accurl2
            // accport2
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
            let (accurl1, accport1) =
                (args[7].clone(), args[8].parse::<usize>().unwrap());
            let (accurl2, accport2) =
                (args[9].clone(), args[10].parse::<usize>().unwrap());

            let mut submit = ActixLUClient::new(subport, &suburl, &protocol);
            let access1 = ActixLedgerClient::new(accport1, &accurl1, &protocol);
            let access2 = ActixLedgerClient::new(accport2, &accurl2, &protocol);

            run_log_against(
                &mut submit,
                &access1,
                Some(&access2),
                logfile,
                outfile,
                expected_file,
            );
        }
        x => panic!("expected 4 or 9 arguments, got {}", x),
    }
}
