#!/usr/bin/env bats

source "tests/common.sh"

check_balances()
{
    unlock=$1 # Must be 'Y' or 'n'

    # First we let Alice define and issue some asset AliceCoin
    # Then Alice transfers some AliceCoins to Bob
    run  bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
                    $MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice AliceCoin;"
    run bash -c "$ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-issue-asset AliceCoin 10000"
    run bash -c 'echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob'
    run bash -c "$PASSWORD_PROMPT | $CLI2 key-gen arturo;"

    transfer_assets "5000" "5000" "n" "n" "true" "alice" "bob" "false" "Y"
    [ "$status" -eq 0 ]

    transfer_assets "1500" "3500" "n" "n" "true" "alice" "bob" "false" "Y"
    debug_lines
    [ "$status" -eq 0 ]

    # Alice makes a confidential transfer to Bob of some AliceCoins
    transfer_assets "1000" "2500" "Y" "Y" "true" "alice" "bob" "false" $unlock
    [ "$status" -eq 0 ]

    # Alice makes a confidential transfer to Arturo of some AliceCoins
    transfer_assets "1300" "1200" "Y" "Y" "true" "alice" "arturo" "true" $unlock
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

@test "balances-with-unlock" {
    check_balances "Y"
}

@test "balances-without-unlock" {
    check_balances "n"
}
