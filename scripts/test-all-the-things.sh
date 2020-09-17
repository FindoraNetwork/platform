#!/usr/bin/env bash

set -e

./scripts/incur fmt -- --check
cargo clippy --workspace

# Build under wasm as well, to catch any compilation differences
pushd ./components/wasm >/dev/null
wasm-pack build --target nodejs
popd >/dev/null


{ echo 'cargo build --workspace'; echo 'cargo build --release --workspace'; echo 'cargo build --release'; echo 'cargo test --release --no-run'; } | parallel -j2 -u {}
# cargo test --no-run
# cargo test --release --no-run

# cargo test --no-fail-fast

{ echo 'bash components/cli2/run_tests_local.sh'; cargo test --release -- --list 2>&1 >/dev/null  | sed -n 's/^\s*Running \(\S*\)\s*$/\1\n\1 --ignored/p'; } | FINDORA_TXN_CLI_DATA_SEARCH_PATH=`pwd`/components/txn_cli FINDORA___TEST___PROJECT___ROOT=`pwd` parallel {}


