#!/usr/bin/env bats

source "tests/common.sh"

@test "list config" {
  run $CLI2 list-config
  [ "$status" -eq 0 ]
  check_line 0 'Submission server: https://testnet.findora.org:8669'
  check_line 1 'Ledger access server: https://testnet.findora.org:8668'
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
  check_line 2 'Submission server: https://testnet.findora.org:8669'
  check_line 3 'Ledger access server: https://testnet.findora.org:8668'
  check_line 4 'Ledger public signing key:'
  check_line 5 'Ledger state commitment:'
  check_line 6 'Ledger block idx:'
  check_line 7 'Current focused transaction builder: <NONE>'

  run $CLI2 query-ledger-state --forget-old-key=false
  [ "$status" -eq 0 ]
  check_line 0  "Saving ledger signing key"
  check_line 1  'New state retrieved.'
  check_line 2 'Submission server: https://testnet.findora.org:8669'
  check_line 3 'Ledger access server: https://testnet.findora.org:8668'
  check_line 4 'Ledger public signing key:'
  check_line 5 'Ledger state commitment:'
  check_line 6 'Ledger block idx:'
  check_line 7 'Current focused transaction builder: <NONE>'
}


@test "prepare transaction" {
  run bash -c " $CLI2 key-gen alice; \
                echo y | $CLI2 query-ledger-state; \
                $CLI2 prepare-transaction -e 0;
                $CLI2 list-txn-builders
                "
  [ "$status" -eq 0 ]
  check_line -1 'Done.'

  check_line 12 '0:'
  check_line 13 ' Operations:'
  check_line 14 ' New asset types defined:'
  check_line 15 ' New asset records:'
  check_line 16 ' Signers:'
  check_line 17 ' Consuming TXOs:'
  check_line 18 'Done.'
}

DEFINE_ASSET_TYPE_AND_BUILD_COMMANDS="$CLI2 key-gen alice; \
                                     echo y | $CLI2 query-ledger-state; \
                                     $CLI2 prepare-transaction -e 0; \
                                     echo memo_alice | $CLI2 define-asset alice TheBestAliceCoinsOnEarthV2 --builder 0; \
                                     $CLI2 issue-asset TheBestAliceCoinsOnEarthV2 0 10000; \
                                     $CLI2 build-transaction 0;"

@test "list-built-transactions" {
  run bash -c "$DEFINE_ASSET_TYPE_AND_BUILD_COMMANDS"

  run $CLI2 list-built-transactions
  debug_lines
  [ "$status" -eq 0 ]

  check_line 1 " seq_id:"
  check_line 5 '  DefineAsset `TheBestAliceCoinsOnEarthV2`'
  check_line 6 '   issued by `alice`'
  check_line 17 "  utxo0 (Not finalized):"
}


@test "list-built-transaction" {

  run bash -c "$DEFINE_ASSET_TYPE_AND_BUILD_COMMANDS"
  run $CLI2 list-built-transaction 0

  [ "$status" -eq 0 ]

  check_line 0 "seq_id:"
  check_line 1 "Handle: <UNKNOWN>"
  check_line 2 "Status: <UNKNOWN>"
  check_line 15 "New asset records:"
  check_line 16 " utxo0 (Not finalized):"
  check_line 17 "  sid: <UNKNOWN>"
  check_line 18 "  Owned by: "
  check_line 19 "  Record Type: \"NonConfidentialAmount_NonConfidentialAssetType\""
  check_line 20 "  Amount: 10000"
  check_line 21 "  Type:"
  check_line 22 "  Decrypted Amount: 10000"
  check_line 24 "  Spent? Unspent"
  check_line 25 "  Have owner memo? No"
  check_line 26 "Signers:"
  check_line 27  ' - `alice`'
}


DEFINE_ASSET_TYPE_AND_SUBMIT_COMMANDS="  set -x; $CLI2 key-gen alice; \
                              echo y | $CLI2 query-ledger-state; \
                              $CLI2 prepare-transaction -e 0; \
                              echo memo_alice | $CLI2 define-asset alice AliceCoin --builder 0; \
                              $CLI2 build-transaction 0; \
                              { echo; echo Y; } | $CLI2 submit 0; \
                  while ! $CLI2 status 0 |  grep Committed; do \
                    $CLI2 status-check 0 || true; sleep 1s; done"

@test "define, publish and list asset type(s)" {
  run  bash -c "$DEFINE_ASSET_TYPE_AND_SUBMIT_COMMANDS"
  debug_lines
  [ "$status" -eq 0 ]
  run $CLI2 list-asset-types
  [ "$status" -eq 0 ]
  check_line 0 'Asset `AliceCoin`'
  run $CLI2 list-asset-type AliceCoin
  [ "$status" -eq 0 ]
  check_line 0 'issuer nickname: alice'
}

@test "query asset type" {
  run  bash -c "  $DEFINE_ASSET_TYPE_AND_SUBMIT_COMMANDS"
  debug_lines
  run $CLI2 query-asset-type --replace=false AliceCoin kt2_x12-CiMz802pkydMrNsSqLEAplDUgKTgzLtprnk=
  debug_lines
  check_line 0 "issue_seq_number: 0"
  [ "$status" -eq 0 ]
  run $CLI2 list-asset-types
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 "Asset `AliceCoin`"
  check_line 1 " issuer nickname: <UNKNOWN>"
  check_line 2 " issuer public key:"
  check_line 3 " code: kt2_x12-CiMz802pkydMrNsSqLEAplDUgKTgzLtprnk="
  check_line 4 " memo: `memo_alice`"
  check_line 5 " issue_seq_number: 0"
}

@test "issue asset" {

  run bash -c "$CLI2 key-gen alice; \
               echo y | $CLI2 query-ledger-state; \
               $CLI2 prepare-transaction -e 0; \
               echo memo_alice | $CLI2 define-asset alice TheBestAliceCoinsOnEarthV2 --builder 0; \
               $CLI2 issue-asset TheBestAliceCoinsOnEarthV2 0 10000; \
               $CLI2 build-transaction 0; \
               { echo; echo Y; } | $CLI2 submit 0; \
                  $CLI2 list-built-transaction 0; \
                  $CLI2 status 0; \
                  while ! $CLI2 status 0 | grep Committed; do \
                    $CLI2 status-check 0 || true; sleep 1s; done"

  debug_lines
  [ "$status" -eq 0 ]
  check_line 22 'Submitting to `https://testnet.findora.org:8669/submit_transaction`'
  check_line 23 " seq_id:"
  check_line 27 '  DefineAsset `TheBestAliceCoinsOnEarthV2`'
  check_line 28 '   issued by `alice`'
  check_line 29 '  IssueAsset 10000 of `TheBestAliceCoinsOnEarthV2`'
  check_line 37 '   issue_seq_number: 0'
  check_line 41 '   Owned by: "'
  check_line 42 '   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"'
  check_line 43 '   Amount: 10000'
  check_line 44 "   Type:"
  check_line 45 "   Decrypted Amount: 10000"
  check_line 49 " Signers:"
  check_line 50 '  - `alice`'
  check_line 51 "Submitted"
  check_line 52 'Got status: {"Committed":'

  # We query the asset type to check the issue_seq_number has been incremented
  run $CLI2 query-asset-type --replace=false TheBestAliceCoinsOnEarthV2 5lS9ivI6WXJSOpDvzhYmM-iKIxF2dQjoCRcsEMnN9s4=
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 "issue_seq_number: 1"

}

