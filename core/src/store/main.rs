extern crate AppendOnlyMT;
extern crate rand;

use rand::prelude::thread_rng;
use rand::Rng;
use crate::core::store::append_only_merkle::AppendOnlyMerkle;
use crate::core::store::append_only_merkle::HashValue;

fn main() -> Result<(), std::io::Error> {
  println!("Running the long test.");

  // Create a tree for testing.
  let path = "long_test";

  let mut tree = match AppendOnlyMerkle::create(&path) {
    Ok(tree) => tree,
    Err(x) => {
      panic!("create failed:  {}", x);
    }
  };

  let mut hash = HashValue { hash: Default::default() };
  let mut countdown = 256;

  // Append some hash values to the tree.
  for tid in 0..512 * 1024 * 1024 {
    // Create a unique, non-zero hash by treating the hash array as a
    // base-256 numeral.  Start at 1 and add 1 every iteration.
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
    match tree.append_hash(&hash) {
      Ok(id) => {
        assert!(id == tid);
      }
      Err(x) => {
        panic!("append_hash failed:  {}", x);
      }
    }

    countdown -= 1;

    // Do consistency checks on the tree from time to time.
    if countdown <= 0 {
      println!("Checking the tree and disk image at tid {}", tid);
      check_tree(&tree);
      check_disk_tree(&mut tree, false);

      if tree.total_size() != tid + 1 {
        panic!("Got {} nodes, but expected {}.", tree.total_size(), tid);
      }

      // Sync the tree to disk.
      println!("Syncing the tree.");
      write_tree(&mut tree);

      // Test the reset function now and then, as well as double checks and
      // checks with a synchronized disk image.
      if thread_rng().gen::<u32>() % 4 == 0 {
        println!("Rechecking the in-memory tree.");
        check_tree(&tree);
        println!("Rechecking the disk.");
        check_disk_tree(&mut tree, true);

        if let Err(x) = AppendOnlyMerkle::open(&path) {
          panic!("open failed:  {}", x);
        }

        reset_tree(&mut tree);
        check_disk_tree(&mut tree, false);
      }

      println!("Done with checking.");

      // Restart the countdown.
      countdown = thread_rng().gen::<u32>() % (256 * 1024);
      countdown += 1;
    }
  }

  println!("The test passed.");
  Ok(())
}

fn check_tree(tree: &AppendOnlyMerkle) {
  if let Some(x) = tree.check() {
    panic!("Got check error:  {}", x);
  }
}

fn check_disk_tree(tree: &mut AppendOnlyMerkle, flushed: bool) {
  if let Some(x) = tree.check_disk(flushed) {
    panic!("Got disk check error:  {}", x);
  }
}

fn write_tree(tree: &mut AppendOnlyMerkle) {
  if let Some(x) = tree.write() {
    panic!("tree.write failed:  {}", x);
  }
}

fn reset_tree(tree: &mut AppendOnlyMerkle) {
  if let Some(x) = tree.reset_disk() {
    panic!("tree.reset_disk failed:  {}", x);
  }
}
