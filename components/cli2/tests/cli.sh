#!/usr/bin/env bats

FINDORA_STORE_FILE=${FINDORA_HOME:-${HOME}/.findora}/cli2_data.sqlite

############################## Helper functions ########################################################################

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

debug_lines(){
  debug_array "${lines[@]}"
}

check_line() {
  line_number="$1"
  command_str="$2"
  command_str_length=`expr length "$command_str"`
  [ "${lines[$line_number]:0:$command_str_length}" = "$command_str" ]
}

################################ Tests #################################################################################

@test "list config" {
  run $CLI2 list-config
  [ "$status" -eq 0 ]
  check_line 0 'Submission server: https://testnet.findora.org/submit_server'
  check_line 1 'Ledger access server: https://testnet.findora.org/query_server'
  check_line 2 'Ledger public signing key:'
  check_line 3 'Ledger state commitment:'
  check_line 4 'Ledger block idx:'
  check_line 5 'Current focused transaction builder: <NONE>'
}

@test "query ledger state" {

  # TODO using true or false does not change the result. Is that OK?

  run $CLI2 query-ledger-state --forget-old-key=true
  [ "$status" -eq 0 ]
  check_line 0  "Saving ledger signing key"
  check_line 1  'New state retrieved.'
  check_line 2 'Submission server: https://testnet.findora.org/submit_server'
  check_line 3 'Ledger access server: https://testnet.findora.org/query_server'
  check_line 4 'Ledger public signing key:'
  check_line 5 'Ledger state commitment:'
  check_line 6 'Ledger block idx:'
  check_line 7 'Current focused transaction builder: <NONE>'

  run $CLI2 query-ledger-state --forget-old-key=false
  [ "$status" -eq 0 ]
  check_line 0  "Saving ledger signing key"
  check_line 1  'New state retrieved.'
  check_line 2 'Submission server: https://testnet.findora.org/submit_server'
  check_line 3 'Ledger access server: https://testnet.findora.org/query_server'
  check_line 4 'Ledger public signing key:'
  check_line 5 'Ledger state commitment:'
  check_line 6 'Ledger block idx:'
  check_line 7 'Current focused transaction builder: <NONE>'
}

@test "key generation" {
  run $CLI2 key-gen alice
  [ "$status" -eq 0 ]
  check_line 0 'New key pair added for `alice`'
}

@test "add bob's public key" {
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'
  [ "$status" -eq 0 ]
  check_line 0 'New public key added for `bob`' ]
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
  check_line 0 'No public key with name plato found'
}

@test "list keys" {
  run bash -c '$CLI2 key-gen alice; $CLI2 key-gen bob; $CLI2 list-keys'
  [ "$status" -eq 0 ]
  check_line 2 'keypair alice:'
  check_line 3 'keypair bob:'
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
  check_line 0 'New key pair added for `bob`' ]
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
  check_line -1 'Done.'

  debug_lines

  check_line 12 '0:'
  check_line 13 ' Operations:'
  check_line 14 ' New asset types defined:'
  check_line 15 ' New asset records:'
  check_line 16 ' Signers:'
  check_line 17 'Done.'
}

DEFINE_ASSET_TYPE_COMMANDS="  $CLI2 key-gen alice; \
                              echo y | $CLI2 query-ledger-state; \
                              $CLI2 prepare-transaction -e 0; \
                              echo memo_alice | $CLI2 define-asset alice AliceCoin --builder 0; \
                              $CLI2 build-transaction 0; \
                              { echo; echo Y; } | $CLI2 submit 0;"

@test "define, publish and list asset type(s)" {
  run  bash -c "$DEFINE_ASSET_TYPE_COMMANDS"
  [ "$status" -eq 0 ]
  run $CLI2 list-asset-types
  [ "$status" -eq 0 ]
  check_line 0 'Asset `AliceCoin`'
  run $CLI2 list-asset-type AliceCoin
  [ "$status" -eq 0 ]
  check_line 0 'issuer nickname: alice'
}

@test "query asset type" {
  run  bash -c "  $DEFINE_ASSET_TYPE_COMMANDS \
                  $CLI2 query-asset-type --replace=true AliceCoin2 kt2_x12-CiMz802pkydMrNsSqLEAplDUgKTgzLtprnk=
                  $CLI2 list-asset-types
                  "
  debug_lines
  [ "$status" -eq 0 ]
  check_line 43 'Asset `AliceCoin2`'
}

@test "issue asset" {

  run bash -c "$CLI2 key-gen alice; \
               echo y | $CLI2 query-ledger-state; \
               $CLI2 prepare-transaction -e 0; \
               echo memo_alice | $CLI2 define-asset alice TheBestAliceCoinsOnEarthV2 --builder 0; \
               $CLI2 issue-asset TheBestAliceCoinsOnEarthV2 0 10000; \
               $CLI2 build-transaction 0; \
               { echo; echo Y; } | $CLI2 submit 0;"

  [ "$status" -eq 0 ]
  debug_lines
  check_line 21 'Submitting to `https://testnet.findora.org/submit_server/submit_transaction`'
  check_line 22 " seq_id:"
  check_line 26 '  DefineAsset `TheBestAliceCoinsOnEarthV2`'
  check_line 27 '   issued by `alice`'
  check_line 28 '  IssueAsset 10000 of `TheBestAliceCoinsOnEarthV2`'
  check_line 39 '   Owned by: "'
  check_line 40 '   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"'
  check_line 41 '   Amount: 10000'
  check_line 42 "   Type:"
  check_line 43 "   Decrypted Amount: 10000"
  check_line 47 " Signers:"
  check_line 48 '  - `alice`'
  check_line 49 "Submitted"
  check_line 50 'Got status: {"Committed":'
}

