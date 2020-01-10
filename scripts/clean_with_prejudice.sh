#!/usr/bin/env bash

# "strict mode" -- see
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail

GIT_ROOT="$(git rev-parse --show-toplevel)"
pushd $GIT_ROOT >/dev/null
find -type f -name 'Cargo.lock' -print0 | xargs -0 rm -v
find -type d -name 'target' -print0 | xargs -0 rm -rv
popd >/dev/null

