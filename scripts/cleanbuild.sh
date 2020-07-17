#!/usr/bin/env bash

# "strict mode" -- see
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail

# move to the root of the git repository we're in
GIT_ROOT="$(git rev-parse --show-toplevel)"
pushd $GIT_ROOT >/dev/null

cargo clippy
cargo clean
cargo build
cargo test
# return to original working directory
popd >/dev/null
