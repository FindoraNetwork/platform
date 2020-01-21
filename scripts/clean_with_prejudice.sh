#!/usr/bin/env bash

# "strict mode" -- see
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail

# move to the root of the git repository we're in
GIT_ROOT="$(git rev-parse --show-toplevel)"
pushd $GIT_ROOT >/dev/null

# Delete all files named 'Cargo.lock' in the tree below
find . -type f -name 'Cargo.lock' -print0 | xargs -r -n 1 -0 rm -v
# Delete all directories named 'target' in the tree below
# Sorting in reverse order suppresses error messages by deleting
#   .../target/.../target before .../target
find . -type d -name 'target' -print0 | sort -z -r | xargs -r -n 1 -0 rm -rv

# return to original working directory
popd >/dev/null

