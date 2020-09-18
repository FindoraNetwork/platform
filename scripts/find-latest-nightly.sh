#!/usr/bin/env bash

i=0
set -e

while true; do
  d=$(date -d "$i days ago" +'%Y-%m-%d')
  sed -i 's/^\(\s*NIGHTLY_DATE="\)[^"]*\(";\)$/\1'$d'\2/' shell.nix

  ./scripts/clean_with_prejudice.sh
  nix-shell --command 'cargo update'
  if nix-shell --command 'RUSTC_WRAPPER= exec bash ./scripts/test-all-the-things.sh'; then
    # 8 extra characters on the banner bc of a 10-char date format in
    # $d
    echo "================================"
    echo "== FOUND! nightly: $d =="
    echo "================================"
    break
  fi

  i=$(( $i + 1 ))
done

