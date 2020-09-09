#!/usr/bin/env bash
set -euo pipefail
cargo build
exec ../../target/debug/findora $*
