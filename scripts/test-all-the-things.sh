#!/usr/bin/env bash

set -e

./scripts/incur fmt -- --check
cargo clippy --workspace
cargo build --workspace
cargo build --release --workspace

cargo test --no-run
cargo test --release --no-run
cargo test --no-fail-fast
cargo test --release --no-fail-fast -- --ignored

