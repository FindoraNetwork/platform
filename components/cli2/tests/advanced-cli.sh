#!/usr/bin/env bats

source "tests/common.sh"

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
  check_line 21 "Submitting to `https://testnet.findora.org/submit_server/submit_transaction`"
  check_line 22 " seq_id:"
  check_line 26 "  DefineAsset `TheBestAliceCoinsOnEarthV2`"
  check_line 27 "   issued by `alice`"
  check_line 28 "  IssueAsset 10000 of `TheBestAliceCoinsOnEarthV2`"
  check_line 39 '   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"'
  check_line 40 '   Amount: 10000'
  check_line 41 "   Type:"
  check_line 42 "   Decrypted Amount: 10000"
  check_line 46 " Signers:"
  check_line 47 "  - `alice`"
  check_line 48 "Submitted"
  check_line 49 'Got status: {"Committed":'
}

