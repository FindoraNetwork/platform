#!/usr/bin/env bats
FINDORA_STORE_FILE=~/.findora/cli2_data.sqlite

# Start from a fresh state
rm -f $FINDORA_STORE_FILE

@test "key generation" {
  run $CLI2 key-gen alice
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = 'New key pair added for `alice`' ]
}

@test "add bob's public key" {
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = 'New public key added for `bob`' ]
}

@test "issue an asset" {
   skip "Not implemented yet"
  run $CLI2 issue-asset alice AliceCoin 20
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = '' ]
}
