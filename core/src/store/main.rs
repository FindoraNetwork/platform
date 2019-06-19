use AppendOnlyMT::AppendOnlyMerkle;
use AppendOnlyMT::HashValue;
use rand::prelude::thread_rng;
use rand::Rng;

fn main() -> Result<(), std::io::Error> {
  println!("Running the long test.");

  let path = "long_test";

  let mut tree =
    match AppendOnlyMerkle::create(&path) {
      Ok(tree) => { tree }
      Err(x) => {
        panic!("create failed:  {}", x);
      }
    };

  let mut hash = HashValue { hash: Default::default(), };
  let mut countdown = 256;

  for tid in 0..512 * 1024 * 1024 {
    let mut carry = 1;

    for i in 0..hash.hash.len() {
      if carry == 0 {
        break;
      }

      if carry > 0 && hash.hash[i] == 255 {
        hash.hash[i] = 0;
	carry = 1;
      } else if carry > 1 {
        hash.hash[i] += 1;
	carry = 0;
      }
    }

    println!("tid = {}, hash = {:?}", tid, hash);

    match tree.append_hash(hash.clone()) {
      Ok(id) => {
        assert!(id == tid);
      }
      Err(x) => {
        panic!("append_hash failed:  {}", x);
      }
    }

    countdown -= 1;

    if countdown <= 0 {
      println!("Checking the tree and disk image at {}", tid);
      check_tree(&tree);
      check_disk_tree(&mut tree, false);

      if tree.total_size() != tid {
        panic!("Got {} nodes, but expected {}.",
            tree.total_size(), tid);
      }

      // Sync the tree to disk.
      println!("Syncing the tree.");
      write_tree(&mut tree);

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
      countdown  = thread_rng().gen::<u32>() % (256 * 1024);
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

fn reset_tree(tree:&mut AppendOnlyMerkle) {
  if let Some(x) = tree.reset_disk() {
    panic!("tree.reset_disk failed:  {}", x);
  }
}
