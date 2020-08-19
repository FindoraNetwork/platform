#!/usr/bin/env bats

source "tests/common.sh"

@test "list-config" {
  run $CLI2 list-config
  [ "$status" -eq 0 ]
  check_line 0 'Submission server: https://testnet.findora.org:8669'
  check_line 1 'Ledger access server: https://testnet.findora.org:8668'
  check_line 2 'Ledger public signing key:'
  check_line 3 'Ledger state commitment:'
  check_line 4 'Ledger block idx:'
  check_line 5 'Current focused transaction builder: <NONE>'
}

@test "query-ledger-state" {

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

}

@test "initialize-transaction" {
  run bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice;
                echo y | $CLI2 query-ledger-state; \
                $CLI2 initialize-transaction 0;
                $CLI2 list-txn"
  [ "$status" -eq 0 ]
  check_line 0 "Enter password for alice: Enter password again:New key pair added for `alice`"
  check_line 10 'Preparing transaction `0` for block id'
  check_line 11 "Done."

}


@test "list-built-transactions" {
  run bash -c "$DEFINE_AND_ISSUE_ASSET_TYPE_WITH_BUILD"
  debug_lines
  run $CLI2 list-built-transactions
  debug_lines
  [ "$status" -eq 0 ]

  check_line 1 " seq_id:"
  check_line 5 '  DefineAsset `TheBestAliceCoinsOnEarthV2`'
  check_line 6 '   issued by `alice`'
  check_line 17 "  utxo0 (Not finalized):"
}


@test "list-built-transaction" {

  run bash -c "$DEFINE_AND_ISSUE_ASSET_TYPE_WITH_BUILD"
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

@test "define, publish and list asset type(s)" {
  run  bash -c "$DEFINE_ASSET_TYPE_WITH_SUBMIT_COMMANDS"
  debug_lines
  [ "$status" -eq 0 ]
  run $CLI2 list-asset-types
  [ "$status" -eq 0 ]
  check_line 0 'Asset `AliceCoin`'
  run $CLI2 list-asset-type AliceCoin
  [ "$status" -eq 0 ]
  check_line 0 'issuer nickname: alice'
}


@test "query-asset-type" {
  run  bash -c "  $DEFINE_ASSET_TYPE_WITH_SUBMIT_COMMANDS"
  debug_lines
  [ "$status" -eq 0 ]

  run $CLI2 query-asset-type --replace=false AliceCoin PfUm5iKkKjiEDuBllekTa3bvxtOSKZy1PeUFLzef1JY=
  debug_lines
  [ "$status" -eq 0 ]

  run $CLI2 list-asset-types
  [ "$status" -eq 0 ]
  check_line 0 'Asset `AliceCoin`'
  check_line 1 " issuer nickname: <UNKNOWN>"
  check_line 2 " issuer public key:"
  check_line 3 " code: PfUm5iKkKjiEDuBllekTa3bvxtOSKZy1PeUFLzef1JY="
  check_line 4 ' memo: `memo_alice`'
  check_line 5 " issue_seq_number: 0"
}

@test "issue-asset" {

  run bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
               echo y | $CLI2 query-ledger-state; \
               $CLI2 initialize-transaction 0; \
               $MEMO_ALICE_WITH_PROMPT | $CLI2 define-asset 0 alice TheBestAliceCoinsOnEarthV2;\
               $PASSWORD_PROMPT | $CLI2 issue-asset 0 TheBestAliceCoinsOnEarthV2 0 10000; \
               $PASSWORD_PROMPT | $CLI2 build-transaction; \
               $DOUBLE_CONFIRM_WITH_PROMPT | $CLI2 submit 0;"
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
  check_line_err 54 "Committed!"

  # We query the asset type to check the issue_seq_number has been incremented
  run $CLI2 query-asset-type --replace=false TheBestAliceCoinsOnEarthV2 0nyMhQo3bZZDxEHN_isswne7Q3dbb6TP2Z_dbZOBoTc=
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 "issue_seq_number: 1"

}

@test "list-txos" {

  run  bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
                $MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice AliceCoin;"
  run bash -c "$ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-issue-asset AliceCoin 10000"
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'

  transfer_assets "5000" "5000" "n" "n" "AliceCoin" "alice" "bob"
  [ "$status" -eq 0 ]

  run bash -c "$CLI2 list-txos"
  [ "$status" -eq 0 ]
  check_line 0 "TXO"
  check_line 28 "Done."

  run bash -c "$CLI2 list-txos --unspent=true"
  [ "$status" -eq 0 ]
  check_line 0 "TXO"
  check_line 18 "Done." # There are less unspent transactions

  run bash -c "$CLI2 list-utxos-filter-owner alice"
  debug_lines
  check_line 0 "TXO"
  check_line 1 " sid:"
  check_line 2 " Owned by: "
  check_line 3 " Record Type: \"NonConfidentialAmount_NonConfidentialAssetType\""
  check_line 4 " Amount: 5000"
  check_line 5 " Type:"
  check_line 8 " Spent? Unspent"
  check_line 9 " Have owner memo? No"
  check_line 10 "Done."
}

@test "status" {
  run bash -c "$DEFINE_ASSET_TYPE_WITH_SUBMIT_COMMANDS"
  run $CLI2 status 0
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 "handle"
}
