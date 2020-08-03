#!/usr/bin/env bats

source "tests/common.sh"


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
  check_line 0 'New key pair added for `bob`'
}

@test "delete key pair" {
  run bash -c '$CLI2 key-gen bob;echo y | $CLI2 delete-keypair bob'
  [ "$status" -eq 0 ]
}

@test "simple asset type definition" {
  run  bash -c "$CLI2 key-gen alice; \
                echo -e 'memo_alice \n y \n' | $CLI2 simple-define-asset alice AliceCoin;"
  debug_lines
  [ "$status" -eq 0 ]
  check_line 19 "Submitting to `https://testnet.findora.org/submit_server/submit_transaction`"
  check_line 24 "  DefineAsset `AliceCoin`"
  check_line 28 "   issuer nickname: alice"
  check_line 31 "   memo: `memo_alice`"
  check_line 32 "   issue_seq_number: 0"
  check_line 36 "Submitted"
  check_line 37 "Got status: {\"Committed\""
  check_line 38 "Done caching TXOs."

  run $CLI2 list-asset-type AliceCoin
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 "issuer nickname: alice"
  check_line 1 "issuer public key:"
  check_line 2 "code:"
  check_line 3 "memo: `memo_alice`"
  check_line 4 "issue_seq_number: 0"

  run $CLI2 list-asset-types
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 "Asset `AliceCoin`"
  check_line 1 " issuer nickname: alice"
  check_line 2 ' issuer public key:'
  check_line 3 " code:"
  check_line 4 " memo: `memo_alice`"
  check_line 5 " issue_seq_number: 0"
}

@test "simple asset issuance" {
  # Define the asset
  run  bash -c "$CLI2 key-gen alice; \
                echo -e 'memo_alice \n y \n' | $CLI2 simple-define-asset alice AliceCoin;"
  [ "$status" -eq 0 ]

  # Issue the asset
  run $CLI2 simple-issue-asset AliceCoin 10000
  debug_lines
  [ "$status" -eq 0 ]
  check_line 0 "Preparing transaction"
  check_line 1 "Done."
  check_line 2 "IssueAsset: 10000 of"
  check_line 3 "Successfully added to"

}
