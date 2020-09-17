#! /usr/bin/env bash

set -euo pipefail

pushd $(dirname $0)/../..
if [[ -z "$CLI2" ]]; then
  export CLI2=`pwd`/target/release/findora
fi
export CLI2

./target/release/ledger_standalone &
ledger_pid=$!
popd

cd $(dirname $0)

FINDORA_SUBMIT_URL='http://localhost:8669' FINDORA_ACCESS_URL='http://localhost:8668' bash ./run_tests.sh
kill $!
wait -n

