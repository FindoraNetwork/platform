#!/usr/bin/env bash

set -e

use_nix=$(hash nix && echo "--nix" || echo "")

cd $(dirname $0)

# `stack build` can take a while to realize that nothing needs to be
# done, so this hackily-but-quickly checks all the build inputs for
# changes -- specifically, this file, any .hs source files, and
# any .cabal or .yaml build configuration files.
hash_expected=$(cat .stack_hash 2>/dev/null || echo "")
curr_hash=$( { echo $(basename $0); find . -name .stack-work -prune -o -type f '(' -name '*.hs' -o -name '*.cabal' -o -name '*.yaml' ')' -print; } | sort | xargs sha256sum | sha256sum)
if [[ "$curr_hash" != "$hash_expected" ]]; then
  echo "change detected, rebuilding..."
  stack $use_nix build
  echo "$curr_hash" >.stack_hash
fi

exec stack $use_nix exec policy

