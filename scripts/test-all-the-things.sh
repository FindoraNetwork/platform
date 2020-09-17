#!/usr/bin/env bash

set -e

./scripts/incur fmt -- --check
cargo clippy --workspace
cargo build --workspace
cargo build --release --workspace

# Build under wasm as well, to catch any compilation differences
pushd ./components/wasm >/dev/null
wasm-pack build --target nodejs
popd >/dev/null


cargo test --no-run
cargo test --release --no-run

function get_test_packages {
  cargo test -- --list | sed -n 's/^\(.*\): test$/\1/p' | sed 's/::.*$/::/g' | sort | uniq
}
get_test_packages

# cargo test --no-fail-fast
get_test_packages | parallel cargo test --release --no-fail-fast {} -- --report-time
pushd ./components/cli2
bash ./run_tests_local.sh
popd
# cargo test --release --no-fail-fast -- --ignored
get_test_packages | parallel cargo test --release --no-fail-fast {} -- --ignored --report-time


