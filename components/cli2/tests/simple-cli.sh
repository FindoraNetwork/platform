#!/usr/bin/env bats

source "tests/common.sh"

@test "key generation" {
  run bash -c "$PASSWORD_PROMPT |$CLI2 key-gen alice"
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 'Enter password for alice: Enter password again:New key pair added for `alice`'
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
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 'No public key with name plato found'
}

@test "list keys" {
  run bash -c '$PASSWORD_PROMPT | $CLI2 key-gen alice; $PASSWORD_PROMPT | $CLI2 key-gen bob; $CLI2 list-keys'
  [ "$status" -eq 0 ]
  check_line 2 'keypair alice:'
  check_line 3 'keypair bob:'
}

@test "delete public key" {
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'
  run bash -c 'echo y | $CLI2 delete-public-key bob'
  debug_lines
  [ "$status" -eq 0 ]
}

@test "list the key pair" {
  run bash -c '$PASSWORD_PROMPT | $CLI2 key-gen bob; $PASSWORD_PROMPT | $CLI2 list-keypair -s bob'
  debug_lines
  [ "$status" -eq 0 ]
  [ "${lines[1]:24:10}" = '{"pub_key"' ]
  [ "${lines[1]:82:9}" = '"sec_key"' ]
}

@test "load key pair" {
  run bash -c 'echo "{\"pub_key\":\"iAnNs_n9HLzdpOYM1cxCOVapua-jS59j1j92lRPe64E=\",\"sec_key\":\"Au3s9u8TdPWX36X-j_9xvMud0DOKrYK1x39imArYI9g=\"}" | $CLI2 load-keypair bob'
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 'Enter password for bob: Enter password again:New key pair added for `bob`'
}

@test "delete key pair" {
  run bash -c 'echo -e "hi\nhi" | $CLI2 key-gen bob;'
  [ "$status" -eq 0 ]
  check_line 0 'Enter password for bob: Enter password again:New key pair added for `bob`'
  run bash -c 'echo -e "hi\ny" | $CLI2 delete-keypair bob;'
  debug_lines
  [ "$status" -eq 0 ]
}

@test "simple-define-asset" {
  run  bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
                $MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice AliceCoin;"
  debug_lines
  [ "$status" -eq 0 ]

  check_line 19 "Submitting to `https://testnet.findora.org:8669/submit_transaction`"
  check_line 24 "  DefineAsset `AliceCoin`"
  check_line 28 "   issuer nickname: alice"
  check_line 31 "   memo: `memo_alice`"
  check_line 32 "   issue_seq_number: 0"
  check_line_err 39 "Committed!"

  run $CLI2 list-asset-type AliceCoin

  [ "$status" -eq 0 ]

  check_line 0 "issuer nickname: alice"
  check_line 1 "issuer public key:"
  check_line 2 "code:"
  check_line 3 "memo: `memo_alice`"
  check_line 4 "issue_seq_number: 0"

  run $CLI2 list-asset-types

  [ "$status" -eq 0 ]
  check_line 0 "Asset `AliceCoin`"
  check_line 1 " issuer nickname: alice"
  check_line 2 ' issuer public key:'
  check_line 3 " code:"
  check_line 4 " memo: `memo_alice`"
  check_line 5 " issue_seq_number: 0"
}


# shellcheck disable=SC2030
@test "simple-issue-asset" {
  # Define the asset
  run  bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
                $MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice AliceCoin;"
  debug_lines
  [ "$status" -eq 0 ]

  # Issue the asset
  run bash -c "$ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-issue-asset AliceCoin 10000"
  debug_lines

  [ "$status" -eq 0 ]
  check_line 0 "Preparing transaction"
  check_line 1 "Done."
  check_line 2 "Enter password for alice: IssueAsset: 10000"
  check_line 3 "Successfully added to"

}

# shellcheck disable=SC2030
@test "transfer-asset" {
  # Alice key gen and AliceCoin asset definition
  run  bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
              $MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice AliceCoin;"
  # Issue the asset
  run bash -c "$ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-issue-asset AliceCoin 10000"
  [ "$status" -eq 0 ]

  # Load Bob's public key
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'

  # Transfer the asset
  amount="5000"
  change_amount="5000"
  transfer_assets "$amount" "$change_amount" "0" "false" "AliceCoin" "alice" "bob" "false"
  debug_lines
  [ "$status" -eq 0 ]

  check_line 5 "  TransferAssets:"
  check_line 10 "    Record Type: \"NonConfidentialAmount_NonConfidentialAssetType\""
  check_line 11 "    Amount: 10000"
  check_line 22 "    Amount: $amount"
  check_line 29 "    Record Type: \"NonConfidentialAmount_NonConfidentialAssetType\""
  check_line_err 60 "Committed!"

  run bash -c "$CLI2 list-txos --unspent=true"
  [ "$status" -eq 0 ]
  debug_lines
  check_line 2 " Owned by: \"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\" (bob)"
  check_line 4 " Amount: 5000"
}

@test "balances" {

  # First we let Alice define and issue some asset AliceCoin
  # Then Alice transfers some AliceCoins to Bob
  run  bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
                $MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice AliceCoin;"
  run bash -c "$ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-issue-asset AliceCoin 10000"
  run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'
  run bash -c "$PASSWORD_PROMPT | $CLI2 key-gen arturo;"

  transfer_assets "5000" "5000" "0" "false" "AliceCoin" "alice" "bob" "false"
  [ "$status" -eq 0 ]

  transfer_assets "1500" "3500" "0" "false" "AliceCoin" "alice" "bob" "false"
  [ "$status" -eq 0 ]

  # Alice makes a confidential transfer to Bob of some AliceCoins
  transfer_assets "1000" "2500" "0" "true" "AliceCoin" "alice" "bob" "false"
  [ "$status" -eq 0 ]

  # Alice makes a confidential transfer to Arturo of some AliceCoins
  transfer_assets "1300" "1200" "0" "true" "AliceCoin" "alice" "arturo" "true"
  [ "$status" -eq 0 ]

  # Now Alice creates another coin
  run bash -c "$MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice YamCoin;"
  [ "$status" -eq 0 ]

  run bash -c "$ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-issue-asset YamCoin 15000"
  [ "$status" -eq 0 ]

  run bash -c "$CLI2 balances"
  debug_lines
  [ "$status" -eq  0 ]
  check_line 1 "(alice,AliceCoin):1200"
  check_line 2 "(alice,YamCoin):15000"
  check_line 3 "(arturo,AliceCoin):1300"

}


