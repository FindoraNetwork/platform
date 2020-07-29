#!/usr/bin/env bats
FINDORA_STORE_FILE=~/.findora/cli2_data.sqlite

# Start from a fresh state
rm $FINDORA_STORE_FILE

@test "key generation" {
  run $CLI2 key-gen alice
  echo ${lines[0]}
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = 'New key pair added for `alice`' ]
}

