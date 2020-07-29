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

@test "add bob's public key" {
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'
  echo "${lines[0]}"
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = 'New public key added for `bob`' ]
}
