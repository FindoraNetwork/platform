//
//  Runs a long test on the AppendOnlyMerkle tree implementation.
//  It just inserts hashes and invokes consistency tests.
//

extern crate rand;
use cryptohash::HashValue;
use merkle_tree::append_only_merkle::AppendOnlyMerkle;
use rand::prelude::thread_rng;
use rand::Rng;
use ruc::*;
use std::env;
use utils::Commas;

fn usage(arguments: Vec<String>, no_checks: &str) {
    println!("Usage:  {} [ {} ]", arguments[0], no_checks);
}

fn main() {
    let arguments: Vec<String> = env::args().collect();
    let skip_checks_arg = "--no-checks";

    if arguments.len() > 2 {
        usage(arguments, skip_checks_arg);
        return;
    }

    let mut skip_checks = false;

    if arguments.len() == 2 && arguments[1] == skip_checks_arg {
        skip_checks = true;
    } else if arguments.len() == 2 {
        usage(arguments, skip_checks_arg);
        return;
    }

    println!("Running the long test.");

    // Create a tree for testing.
    let path = "long_test";

    let mut tree = pnk!(AppendOnlyMerkle::create(&path));

    let mut hash = HashValue {
        hash: Default::default(),
    };
    let mut countdown = 256;
    let mut range = 256 * 1024u64;

    if skip_checks {
        countdown = std::u64::MAX;
    }

    // Append some hash values to the tree.
    for tid in 0..400 * 1024 * 1024 {
        // Create a unique, non-zero hash by treating the hash array as a
        // base-256 numeral. Start at 1 and add 1 every iteration.
        let mut carry = 1;

        for i in 0..hash.hash.len() {
            if carry > 0 && hash.hash[i] == 255 {
                hash.hash[i] = 0;
                carry = 1;
            } else if carry > 0 {
                hash.hash[i] += 1;
                carry = 0;
            }

            if carry == 0 {
                break;
            }
        }

        // Try the insertion.
        assert_eq!(tid, pnk!(tree.append_hash(&hash)));

        countdown -= 1;

        // Do consistency checks on the tree from time to time.
        if countdown <= 1 {
            println!("Checking the tree and disk image at tid {}", tid.commas());
            check_tree(&tree);
            check_disk_tree(&mut tree, false);

            if tree.total_size() != tid + 1 {
                panic!(
                    "Got {} nodes, but expected {}.",
                    tree.total_size(),
                    tid.commas()
                );
            }

            // Sync the tree to disk.
            println!("    Syncing the tree.");
            write_tree(&mut tree);

            test_proof(&mut tree);

            // Test the reset function now and then, as well as double checks and
            // checks with a synchronized disk image.
            if thread_rng().gen::<u32>() % 4 == 0 {
                println!("    Rechecking the in-memory tree.");
                check_tree(&tree);
                println!("    Rechecking the disk.");
                check_disk_tree(&mut tree, true);

                pnk!(AppendOnlyMerkle::open(&path));

                reset_tree(&mut tree);
                check_disk_tree(&mut tree, false);
            }

            println!("    Done with checking.");

            // Restart the countdown.
            countdown = thread_rng().gen::<u64>() % range;
            countdown += 1;

            if tree.total_size() as u64 > 4 * range {
                range *= 4;
                println!("    Range extended to {}", range.commas());
            }
        }
    }

    pnk!(tree.write());
    println!("Done with {} entries.", tree.total_size().commas());
    println!("The test passed.");
}

fn check_tree(tree: &AppendOnlyMerkle) {
    if let Err(e) = tree.check() {
        panic!("Got check error:  {}", e);
    }
}

fn check_disk_tree(tree: &mut AppendOnlyMerkle, flushed: bool) {
    if let Err(e) = tree.check_disk(flushed) {
        panic!("Got disk check error:  {}", e);
    }
}

fn write_tree(tree: &mut AppendOnlyMerkle) {
    if let Err(e) = tree.write() {
        panic!("tree.write failed:  {}", e);
    }
}

fn reset_tree(tree: &mut AppendOnlyMerkle) {
    if let Err(e) = tree.reset_disk() {
        panic!("tree.reset_disk failed:  {}", e);
    }
}

fn test_proof(tree: &mut AppendOnlyMerkle) {
    let state = tree.total_size();
    let rand = thread_rng().gen::<u64>();

    let id = if state > 2 { rand % (state + 1) } else { state };

    println!("    Testing a proof for tid {}", id.commas());

    match tree.generate_proof(id, state) {
        Err(x) => {
            panic!("Error on generating a proof for tid {}:  {}", id, x);
        }
        Ok(proof) => {
            assert!(proof.tx_id == id);
            assert!(proof.state == tree.total_size());
        }
    }
}
