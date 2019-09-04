//! # The Append-Only Merkle Tree Check and Repair Program
//!
//!  This program provides Merkle tree check and repair
//!  operations.  Usage:
//!
//!      check_merkle [ -c | -r ] path
//!
//!  where
//!
//!      -c  specifies check only
//!      -r  requests a rebuild attempt if the tree is corrupt
//!
//!  If neither -c nor -r is specified, the program assumes
//!  that only a check is requested.
//!
//!  The program first attempts to open the tree.  If that is
//!  successful, the tree is flushed to disk and a complete
//!  check is done.  If the check fails, the disk image is
//!  reset and the program attempts to rewrite the files with
//!  the tree.write() method.  Any error during the rewrite is
//!  fatal and reports an error.
//!
//!  If the file cannot be opened, the program invokes the tree
//!  rebuild method, and returns the resul of that procedure.
//!
extern crate ledger;
#[macro_use]
extern crate findora;

use ledger::store::append_only_merkle::AppendOnlyMerkle;
use findora::timestamp;
use findora::EnableMap;
use std::env;
use std::path::Path;
use std::process::exit;

#[allow(non_upper_case_globals)]
static check: EnableMap = EnableMap {
  info_enabled:  true,
  warning_enabled: true,
  error_enabled: true,
  log_enabled: true,
  debug_enabled: true,
};

#[allow(non_upper_case_globals)]
static rebuild: EnableMap = EnableMap {
  info_enabled:  true,
  warning_enabled: true,
  error_enabled: true,
  log_enabled: true,
  debug_enabled: true,
};

fn main() {
  let (path, do_repairs) = parse_arguments();
  log!(check, "Opening the Merkle tree at \"{}\".", path);

  let mut tree = match AppendOnlyMerkle::open(&path) {
    Ok(tree) => tree,
    Err(e) => {
      log!(check, "check_merkle failed to open \"{}\":  {}", path, e);

      // The open failed, so try a rebuild if asked.
      if do_repairs {
        try_rebuild(&path);
      }

      exit(1);
    }
  };

  // The open succeeded.  Flush any reconstructed blocks to the
  // disk.
  if let Some(e) = tree.write() {
    log!(check, "The Merkle tree write returned an error:  {}", e);
    log!(check, "Continuing.");
  }

  log!(check, "Performing the initial check.");

  // Perform the full check.
  if let Some(e) = tree.check_disk(true) {
    log!(check, "The Merkle tree check returned an error:  {}", e);
  } else {
    log!(check,
         "The Merkle tree at \"{}\" is valid with {} entries.",
         path,
         tree.total_size());
    return;
  }

  if !do_repairs {
    exit(1);
  }

  // The check didn't pass, so try rebuilding the interior blocks
  // (level 1 and up).
  log!(check, "Rewriting the Merkle tree.");

  // Try to save the level 0 data file.
  let save = path.to_owned() + "-check_merkle";
  let _ = std::fs::remove_file(&save);
  let _ = std::fs::rename(&path, &save);

  // Tell the tree to assume that the disk image is
  // invalid.
  if let Some(e) = tree.reset_disk() {
    log!(check, "The disk reset failed:  {}", e);
    log!(check, "Continuing");
  }

  // Rewrite the entire image, if possible.
  if let Some(e) = tree.write() {
    log!(check, "The rewrite failed:  {}", e);
    exit(1);
  }

  // Try the full check again.
  if let Some(e) = tree.check_disk(true) {
    log!(check, "The final check failed:  {}", e);
    exit(1);
  }

  log!(check,
       "The Merkle tree at \"{}\" is now valid with {} entries.",
       path,
       tree.total_size());
}

fn parse_arguments() -> (String, bool) {
  let arguments: Vec<String> = env::args().collect();

  if arguments.len() > 3 || arguments.len() < 2 {
    print_usage();
    exit(1);
  }

  if arguments.len() == 2 {
    return (arguments[1].clone(), false);
  }

  let do_repairs = match arguments[1].as_ref() {
    "-c" => false,
    "-r" => true,
    _ => {
      println!("That option ({}) is invalid.", arguments[1]);
      println!();
      print_usage();
      exit(1);
    }
  };

  (arguments[2].clone(), do_repairs)
}

fn try_rebuild(path: &str) {
  if !Path::new(path).exists() {
    exit(1);
  }

  log!(rebuild, "Trying to rebuild the Merkle tree.");

  let tree = match AppendOnlyMerkle::rebuild(path) {
    Ok(tree) => tree,
    Err(e) => {
      log!(rebuild, "The rebuild failed:  {}", e);
      exit(1);
    }
  };

  log!(rebuild,
       "The Merkle tree at \"{}\" has been rebuilt with {} entries.",
       path,
       tree.total_size());
  exit(0);
}

fn print_usage() {
  println!("Usage:  check_merkle [ -c | -r ] <path>");
  println!("   -c  Check the Merkle tree.");
  println!("   -r  Check and repair as needed.");
  println!();
  println!("The default is to check but not repair the tree.");
}
