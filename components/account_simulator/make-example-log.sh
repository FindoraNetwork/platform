#!/usr/bin/env bash
set -e
cargo run --release -- replay <(gunzip -c example_log_src.gz) ../log_tester/example_log
pushd ../log_tester
cargo run -- example_log expected -
cargo run -- example_log - expected
