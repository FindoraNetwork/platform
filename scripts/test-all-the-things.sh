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

# cargo test --no-fail-fast
cargo test --release -- --list 2>&1 >/dev/null  | sed -n 's/^\s*Running \(\S*\)\s*$/\1/p' | FINDORA_TXN_CLI_DATA_SEARCH_PATH=`pwd`/components/txn_cli FINDORA___TEST___PROJECT___ROOT=`pwd` parallel {}
pushd ./components/cli2
bash ./run_tests_local.sh
popd
# cargo test --release --no-fail-fast -- --ignored
# cargo test --release -- --list 2>&1 >/dev/null  | sed -n 's/^\s*Running \(\S*\)\s*$/\1/p' | parallel {} --ignored
cargo test --release -- --list 2>&1 >/dev/null  | sed -n 's/^\s*Running \(\S*\)\s*$/\1/p' | FINDORA_TXN_CLI_DATA_SEARCH_PATH=`pwd`/components/txn_cli FINDORA___TEST___PROJECT___ROOT=`pwd` parallel {} --ignored


