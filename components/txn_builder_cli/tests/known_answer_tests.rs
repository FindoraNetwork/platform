use std::io::{self, Write};
use std::process::Command;

// TODO: helper function/macro to replace copypasta
#[test]
fn call_no_args() {
  // TODO: derive path and command name from cwd
  let output =
    Command::new("../../target/debug/txn_builder_cli").output()
                                                      .expect("failed to execute process");

  // TODO: figure out how to colorize stdout and stderr
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  // TODO: fix this!
  assert!(output.status.success())
}

#[test]
fn call_with_help() {
  // TODO: derive path and command name from cwd
  let output =
    Command::new("../../target/debug/txn_builder_cli").arg("help")
                                                      .output()
                                                      .expect("failed to execute process");

  // TODO: figure out how to colorize stdout and stderr
  io::stdout().write_all(&output.stdout).unwrap();
  io::stdout().write_all(&output.stderr).unwrap();

  assert!(output.status.success())
}

// TODO: setup/teardown: create tempdir, populate with test-specific files, run each test in dir, destroy on success, preserve on error
