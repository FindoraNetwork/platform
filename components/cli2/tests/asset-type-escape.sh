#!/usr/bin/env bats

source "tests/common.sh"

@test "query-hyphen-asset-type" {
  found=false
  found_i=0
  run bash -c "{ $PASSWORD_PROMPT | $CLI2 key-gen alice; } && \
               { echo y | $CLI2 query-ledger-state; } && \
               { $CLI2 initialize-transaction 0; }"
  [ "$status" -eq 0 ]
  prev_code=""

  while ! $found; do # (1 - 1/64)^300 < 1/100
    asset_type="AliceCoin${found_i}"
    # echo "$found_i" >>hyphen-log.txt

    run  bash -c "{ $MEMO_ALICE_WITH_PROMPT | $CLI2 define-asset 0 alice $asset_type; }"

    alice_coin_code="$(debug_lines | sed -n 's/^.*code:\s*\(\S*\)*$/\1/p')"

    debug_lines
    found_i=$(( ${found_i} + 1 ))

    [ "$status" -eq 0 ]

    # alice_coin_code=$($CLI2 list-txn | grep -C3 "$asset_type" | sed -n 's/^\s*code:\s*\(\S*\)*$/\1/p' | tail -n1)
    echo $alice_coin_code
    echo $prev_code
    [ "$alice_coin_code" != "$prev_code" ]
    prev_code="$alice_coin_code"
    # echo "$found_i ($asset_type): $alice_coin_code" >>hyphen-log.txt
    if [[ "$alice_coin_code" =~ ^- ]]; then
      found=true
    else
      continue
    fi

    run bash -c "{ $PASSWORD_PROMPT | $CLI2 build-transaction; } && \
                 { $DOUBLE_CONFIRM_WITH_PROMPT | $CLI2 submit 0; };"
    debug_lines
    [ "$status" -eq 0 ]

    setup

    echo "code: $alice_coin_code"
    run $CLI2 query-asset-type --replace=false $asset_type -- "$alice_coin_code"

    debug_lines
    echo $status
    [ "$status" -eq 0 ]

    run $CLI2 list-asset-type $asset_type
    debug_lines
    [ "$status" -eq 0 ]
    check_line 0 "issuer nickname: <UNKNOWN>"
    check_line 1 "issuer public key:"
    check_line 2 "code: $alice_coin_code"
    check_line 3 "memo: 'memo_alice'"
    check_line 4 "issue_seq_number: 0"
  done
}

@test "query-asset-type-a-bunch" {
  found=false
  run bash -c "{ $PASSWORD_PROMPT | $CLI2 key-gen alice; } && \
               { echo y | $CLI2 query-ledger-state; } && \
               { $CLI2 initialize-transaction 0; }"
  [ "$status" -eq 0 ]
  prev_code=""
  codes=""

  for found_i in `seq 1 300`; do # (1 - 1/64)^300 < 1/100
    asset_type="AliceCoin${found_i}"
    # echo "$found_i" >>hyphen-log.txt

    run  bash -c "{ $MEMO_ALICE_WITH_PROMPT | $CLI2 define-asset 0 alice $asset_type; }"

    alice_coin_code="$(debug_lines | sed -n 's/^.*code:\s*\(\S*\)*$/\1/p')"
    debug_lines

    [ "$status" -eq 0 ]

    # alice_coin_code=$($CLI2 list-txn | grep -C3 "$asset_type" | sed -n 's/^\s*code:\s*\(\S*\)*$/\1/p' | tail -n1)
    echo $alice_coin_code
    echo $prev_code
    [ "$alice_coin_code" != "$prev_code" ]
    prev_code="$alice_coin_code"
    # echo "$found_i ($asset_type): $alice_coin_code" >>hyphen-log.txt
    codes="$codes $alice_coin_code"
  done

  run bash -c "{ $PASSWORD_PROMPT | $CLI2 build-transaction; } && \
                { $DOUBLE_CONFIRM_WITH_PROMPT | $CLI2 submit 0; };"
  debug_lines
  [ "$status" -eq 0 ]

  setup

  for code in $(echo $codes); do
    asset_type="asset_$code"
    echo "code: $code"
    run $CLI2 query-asset-type --replace=false "$asset_type" -- "$code"

    debug_lines
    echo $status
    [ "$status" -eq 0 ]

    run $CLI2 list-asset-type $asset_type
    debug_lines
    [ "$status" -eq 0 ]
    check_line 0 "issuer nickname: <UNKNOWN>"
    check_line 1 "issuer public key:"
    check_line 2 "code: $code"
    check_line 3 "memo: 'memo_alice'"
    check_line 4 "issue_seq_number: 0"
  done
}


