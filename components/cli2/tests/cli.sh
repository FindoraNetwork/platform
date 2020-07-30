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

@test "list public keys" {
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'
  run bash -c 'echo "\"CaOPNpTSFitNXoyxpsfL-amF_lHanegLIAUTkNsA2yw==\"" | $CLI2 load-public-key greg'
  run $CLI2 list-public-key bob
  [ "$status" -eq 0 ]
  run $CLI2 list-public-key greg
  [ "$status" -eq 0 ]
  run $CLI2 list-public-key plato
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = 'No public key with name plato found' ]
}

@test "delete public key" {
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'
  run bash -c 'echo y | $CLI2 delete-public-key bob'
  [ "$status" -eq 0 ]
}

@test "list the key pairs" {
  run bash -c '$CLI2 key-gen bob; $CLI2 list-keypair bob'
  [ "$status" -eq 0 ]
  [ "${lines[1]:0:10}" = '{"pub_key"' ]
  [ "${lines[1]:58:9}" = '"sec_key"' ]
}

@test "delete key pairs" {
  run bash -c '$CLI2 key-gen bob;echo y | $CLI2 delete-keypair bob'
  [ "$status" -eq 0 ]
}

@test "prepare transaction" {
  run bash -c '$CLI2 key-gen alice; echo y | $CLI2 query-ledger-state; $CLI2 prepare-transaction 0;$CLI2 list-txn-builders;'
  [ "$status" -eq 0 ]
  # TODO better capture of output
  #[ "${lines[9]}" = 'Done.' ]
}

@test "define asset" {
  run bash -c '$CLI2 key-gen alice; echo y | $CLI2 query-ledger-state; $CLI2 prepare-transaction 0;echo memo0 | $CLI2 define-asset alice AliceCoin --txn 0;'
  [ "$status" -eq 0 ]
  $CLI2 list-txn-builders;
  [ "$status" -eq 0 ]
  [ "${lines[9]}" = 'Done.' ] #TODO better capture of output
  run bash -c
}


@test "issue asset" {
  skip "Not implemented"
  run bash -c '$CLI2 key-gen alice; echo y | $CLI2 query-ledger-state; $CLI2 prepare-transaction 0;echo memo0 | $CLI2 define-asset alice AliceCoin --txn 0;'
  [ "$status" -eq 0 ]
  $CLI2 issue-asset --txn=0 alice AliceCoin 1000
  [ "$status" -eq 0 ]
}
