#!/usr/bin/env bash

set -e

./scripts/incur fmt
cargo clippy --workspace
cargo build --workspace
cargo build --release --workspace

# Build under wasm as well, to catch any compilation differences
pushd ./components/wasm >/dev/null
wasm-pack build --target nodejs
popd >/dev/null

cargo test --no-run
cargo test --release --no-run
cargo test --no-fail-fast
pushd ./components/cli2
bash ./run_tests_local.sh
popd
cargo test --release --no-fail-fast -- --ignored

