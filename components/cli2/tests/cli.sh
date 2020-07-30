#!/usr/bin/env bats
FINDORA_STORE_FILE=${FINDORA_HOME:-${HOME}/.findora}/cli2_data.sqlite
echo "FINDORA_HOME: ${FINDORA_HOME}"
echo "FINDORA_STORE_FILE: ${FINDORA_STORE_FILE}"

setup() {
  # Start from a fresh state
  rm -f $FINDORA_STORE_FILE
  bash -c '{ echo; echo; } | $CLI2 setup'
}

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

@test "list the key pairs" {
  run bash -c '$CLI2 key-gen bob; $CLI2 list-keypair bob'
  [ "$status" -eq 0 ]
  [ "${lines[1]:0:10}" = '{"pub_key"' ]
  [ "${lines[1]:58:9}" = '"sec_key"' ]
}

@test "prepare transaction" {
  run bash -c '$CLI2 key-gen alice; echo y | $CLI2 query-ledger-state; $CLI2 prepare-transaction 0;$CLI2 list-txn-builders;'
  [ "$status" -eq 0 ]
  [ "${lines[9]}" = 'Done.' ] # TODO better capture of output
}

@test "define asset" {
  run bash -c '$CLI2 key-gen alice; echo y | $CLI2 query-ledger-state; $CLI2 prepare-transaction 0;echo memo0 | $CLI2 define-asset alice AliceCoin --txn 0;'
  [ "$status" -eq 0 ]
  $CLI2 list-txn-builders;
  [ "$status" -eq 0 ]
  [ "${lines[9]}" = 'Done.' ] #TODO better capture of output
}
