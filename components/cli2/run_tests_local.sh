#! /usr/bin/env bash

set -euo pipefail

pushd $(dirname $0)/../..
export CLI2=`pwd`/target/release/findora

./target/release/ledger_standalone &
ledger_pid=$!
popd

cd $(dirname $0)

FINDORA_SUBMIT_URL='http://localhost:8669' FINDORA_ACCESS_URL='http://localhost:8668' bash ./run_tests.sh
kill $!
wait

