#!/usr/bin/env bats

source "tests/common.sh"

@test "key generation" {
  run bash -c "$PASSWORD_PROMPT |$CLI2 key-gen alice"
  [ "$status" -eq 0 ]
  check_line 0 'Enter password for alice: Enter password again:New key pair added for `alice`'
  run bash -c "$PASSWORD_PROMPT |$CLI2 key-gen alice"

  # When trying to generate a key pair with the same name the user is asked for confirmation.
  run bash -c "echo 'n' | $CLI2 key-gen alice"
  [ "$status" -eq 0 ]
  check_line 0 'Do you want to overwrite the existing key pair? CAUTION: this operation cannot be reverted. You may loose all your funds.'
  check_line 1 'Operation aborted by the user.'

  run bash -c "$PASSWORD_PROMPT_YES |$CLI2 key-gen alice"
  check_line 0 'Do you want to overwrite the existing key pair? CAUTION: this operation cannot be reverted. You may loose all your funds.'
  check_line 1 'Enter password for alice: Enter password again:New key pair added for `alice`'
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

  run bash -c "echo n | $CLI2 load-keypair bob"
  check_line 0 "Do you want to overwrite the existing key pair? CAUTION: this operation cannot be reverted. You may loose all your funds."
  check_line 1 "Operation aborted by the user."

  run bash -c 'echo -e "y\n{\"pub_key\":\"iAnNs_n9HLzdpOYM1cxCOVapua-jS59j1j92lRPe64E=\",\"sec_key\":\"Au3s9u8TdPWX36X-j_9xvMud0DOKrYK1x39imArYI9g=\"}" | $CLI2 load-keypair bob'
  debug_lines
  check_line 0 "Do you want to overwrite the existing key pair? CAUTION: this operation cannot be reverted. You may loose all your funds."
  check_line 1 'Enter password for bob: Enter password again:New key pair added for `bob`'

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
  [ "$status" -eq 0 ]

  # Issue the asset
  run bash -c "$ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-issue-asset AliceCoin 10000"

  [ "$status" -eq 0 ]
  check_line 0 "Preparing transaction"
  check_line 1 "Done."
  check_line 2 "Enter password for alice: IssueAsset: 10000"
  check_line 3 "Successfully added to"
}



