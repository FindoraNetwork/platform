#!/usr/bin/env bats

# Start from a fresh state
rm -f ~/.findora/cli2_data.json

@test "key generation" {
  run $CLI2 key-gen alice
  echo ${lines[0]}
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = 'KeyGen { nick: "alice" }' ]
  [ "${lines[1]}" = 'Opened 0 times before' ]
  [ "${lines[2]}" = 'New key added for `alice`' ]
}

