#!/usr/bin/env bats

FINDORA_STORE_FILE=${FINDORA_HOME:-${HOME}/.findora}/cli2_data.sqlite

setup() {
  # Start from a fresh state
  echo "Deleting $FINDORA_STORE_FILE..."
  rm  -f $FINDORA_STORE_FILE || true
  bash -c '{ echo; echo; } | $CLI2 setup'
}

debug_array() {

  echo "Debugging array..."
  arr=("$@")
  COUNTER=0
  for i in "${arr[@]}";
    do
        echo "[$COUNTER]$i"
        COUNTER=$((COUNTER+1))
    done
}

@test "list config" {
  run $CLI2 list-config
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = 'Submission server: https://testnet.findora.org/submit_server' ]
  [ "${lines[1]}" = 'Ledger access server: https://testnet.findora.org/query_server' ]
  [ "${lines[2]:0:26}" = 'Ledger public signing key:' ]
  [ "${lines[3]:0:24}" = 'Ledger state commitment:' ]
  [ "${lines[4]:0:17}" = 'Ledger block idx:' ]
  [ "${lines[5]}" = 'Current target transaction: <NONE>' ]
}

@test "query ledger state" {

  # TODO using true or false does not change the result. Is that OK?

  run $CLI2 query-ledger-state --forget-old-key=true
  [ "$status" -eq 0 ]
  [ "${lines[0]:0:25}" = "Saving ledger signing key" ]
  [ "${lines[1]}" = 'New state retrieved.' ]
  [ "${lines[2]}" = 'Submission server: https://testnet.findora.org/submit_server' ]
  [ "${lines[3]}" = 'Ledger access server: https://testnet.findora.org/query_server' ]
  [ "${lines[4]:0:26}" = 'Ledger public signing key:' ]
  [ "${lines[5]:0:24}" = 'Ledger state commitment:' ]
  [ "${lines[6]:0:17}" = 'Ledger block idx:' ]
  [ "${lines[7]}" = 'Current target transaction: <NONE>' ]

  run $CLI2 query-ledger-state --forget-old-key=false
  [ "$status" -eq 0 ]
  [ "${lines[0]:0:25}" = "Saving ledger signing key" ]
  [ "${lines[1]}" = 'New state retrieved.' ]
  [ "${lines[2]}" = 'Submission server: https://testnet.findora.org/submit_server' ]
  [ "${lines[3]}" = 'Ledger access server: https://testnet.findora.org/query_server' ]
  [ "${lines[4]:0:26}" = 'Ledger public signing key:' ]
  [ "${lines[5]:0:24}" = 'Ledger state commitment:' ]
  [ "${lines[6]:0:17}" = 'Ledger block idx:' ]
  [ "${lines[7]}" = 'Current target transaction: <NONE>' ]
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

@test "list public key" {
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

@test "list keys" {
  run bash -c '$CLI2 key-gen alice; $CLI2 key-gen bob; $CLI2 list-keys'
  [ "$status" -eq 0 ]
  [ "${lines[2]:0:14}" = 'keypair alice:' ]
  [ "${lines[3]:0:12}" = 'keypair bob:' ]
}

@test "delete public key" {
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'
  run bash -c 'echo y | $CLI2 delete-public-key bob'
  [ "$status" -eq 0 ]
}

@test "list the key pair" {
  run bash -c '$CLI2 key-gen bob; $CLI2 list-keypair -s bob'
  [ "$status" -eq 0 ]
  [ "${lines[1]:0:10}" = '{"pub_key"' ]
  [ "${lines[1]:58:9}" = '"sec_key"' ]
}

@test "load key pair" {
  run bash -c 'echo "{\"pub_key\":\"iAnNs_n9HLzdpOYM1cxCOVapua-jS59j1j92lRPe64E=\",\"sec_key\":\"Au3s9u8TdPWX36X-j_9xvMud0DOKrYK1x39imArYI9g=\"}" | $CLI2 load-keypair bob'
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = 'New key pair added for `bob`' ]
}

@test "delete key pair" {
  run bash -c '$CLI2 key-gen bob;echo y | $CLI2 delete-keypair bob'
  [ "$status" -eq 0 ]
}

@test "prepare transaction" {
  run bash -c " $CLI2 key-gen alice; \
                echo y | $CLI2 query-ledger-state; \
                $CLI2 prepare-transaction -e 0;
                $CLI2 list-txn-builders
                "
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = 'Done.' ]

  debug_array "${lines[@]}"

  [ "${lines[12]}" = '0:' ]
  [ "${lines[13]}" = ' Operations:' ]
  [ "${lines[14]}" = ' New asset types defined:' ]
  [ "${lines[15]}" = ' Signers:' ]
  [ "${lines[16]}" = 'Done.' ]
}

@test "define, publish and list asset(s)" {
  run  bash -c "  $CLI2 key-gen alice; \
                  echo y | $CLI2 query-ledger-state; \
                  $CLI2 prepare-transaction -e 0; \
                  echo memo_alice | $CLI2 define-asset alice AliceCoin --txn 0; \
                  $CLI2 build-transaction 0; \
                  echo Y | $CLI2 submit 0"
  [ "$status" -eq 0 ]
  run $CLI2 list-asset-types
  [ "$status" -eq 0 ]
  echo "${lines[0]}"
  [ "${lines[0]}" = 'Asset `AliceCoin`' ]
  run $CLI2 list-asset-type AliceCoin
  [ "$status" -eq 0 ]
  [ "${lines[0]}" = 'issuer nickname: alice' ]
}

@test "issue asset" {
  skip "Not implemented"
  run bash -c "$CLI2 key-gen alice; \
              echo y | $CLI2 query-ledger-state; \
              $CLI2 prepare-transaction 0; \
              echo memo0 | $CLI2 define-asset alice AliceCoin --txn 0;"
  [ "$status" -eq 0 ]
  $CLI2 issue-asset --txn=0 alice AliceCoin 1000
  [ "$status" -eq 0 ]
}
