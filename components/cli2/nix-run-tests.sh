#!/usr/bin/env bash
exec nix-shell -p bats bc --command 'BATS=`which bats` CLI2=`pwd`/../../target/debug/findora bash ./run_tests.sh'
