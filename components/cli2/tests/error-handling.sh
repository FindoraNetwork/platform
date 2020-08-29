#!/usr/bin/env bats

source "tests/common.sh"

@test "query-asset-type" {
  run $CLI2 query-asset-type does_not_exist does_not_exist
  [ "$status" -eq 1 ]
}

@test "build-transaction" {
    run $CLI2 build-transaction
    [ "$status" -eq 255 ]
    check_line 0 "I don't know which transaction to use!"
}

@test "define-asset" {
    wrong_tx_id=1
    run bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
                echo y | $CLI2 query-ledger-state; \
                $CLI2 initialize-transaction 0; \
                $MEMO_ALICE_WITH_PROMPT | $CLI2 define-asset $wrong_tx_id alice TheBestAliceCoinsOnEarthV2"

    [ "$status" -eq 255 ]
    check_line 13 "Transaction builder '1' not found."

    wrong_keypair_id="arturo"
    run bash -c "$PASSWORD_PROMPT_YES | $CLI2 key-gen alice; \
                echo y | $CLI2 query-ledger-state; \
                $CLI2 initialize-transaction 0; \
                $MEMO_ALICE_WITH_PROMPT | $CLI2 define-asset 0 $wrong_keypair_id TheBestAliceCoinsOnEarthV2"

    check_line 11 "Enter password for arturo: Password was incorrect, please try again."
    check_line 12 "Enter password for arturo: Password was incorrect, please try again."
    check_line 13 "Enter password for arturo: Error: Failed to read password"
    check_line 14 "   Caused by: The provided password was incorrect."
}

@test "delete-keypair" {
    run bash -c "$PASSWORD_PROMPT |$CLI2 key-gen alice"
    not_existing_user="arturo"
    run bash -c "$PASSWORD_PROMPT | $CLI2 delete-keypair $not_existing_user"

    [ "$status" -eq 1 ]
    check_line 0 "Enter password for arturo: Password was incorrect, please try again."
    check_line 1 "Enter password for arturo: Password was incorrect, please try again."
    check_line 2 "Enter password for arturo: Error: Failed to read password"
    check_line 3 "   Caused by: The provided password was incorrect."
}

@test "delete-public-key" {
    run bash -c "$PASSWORD_PROMPT |$CLI2 key-gen alice"

    run bash -c "$PASSWORD_PROMPT | $CLI2 delete-public-key alice"
    [ "$status" -eq 255 ]
    check_line 0 "'alice' is a keypair. Please use delete-keypair instead."

    run bash -c "echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob"
    not_existing_user="arturo"
    run bash -c "$PASSWORD_PROMPT | $CLI2 delete-public-key $not_existing_user"
    debug_lines
    [ "$status" -eq 255 ]
    check_line 0 "No public key with name 'arturo' found"
}

@test "initialize-transaction" {
    run bash -c "$CLI2 initialize-transaction 1"
    [ "$status" -eq 255 ]
    check_line 0 "I don't know what block ID the ledger is on!"
    check_line 1 "Please run query-ledger-state first."

    run bash -c "echo y | $CLI2 query-ledger-state"
    run bash -c "$CLI2 initialize-transaction 1"
    [ "$status" -eq 0 ]

    run bash -c "$CLI2 initialize-transaction 1"

    [ "$status" -eq 255 ]
    check_line 0 "Transaction builder with the name '1' already exists."
}


@test "issue-asset" {
    run bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
               echo y | $CLI2 query-ledger-state; \
               $CLI2 initialize-transaction 0; \
               $MEMO_ALICE_WITH_PROMPT | $CLI2 define-asset 0 alice AliceCoin;\
               $PASSWORD_PROMPT | $CLI2 issue-asset 0 BobCoin 0 10000; "
    debug_lines
    [ "$status" -eq 255 ]
    check_line 19 "No asset type with name 'BobCoin' found"
}

@test "key-gen" {
    echo "Nothing interesting to test here"
}

@test "list-asset-type" {
    run  bash -c "$DEFINE_ASSET_TYPE_WITH_SUBMIT_COMMANDS"
    [ "$status" -eq 0 ]

    run $CLI2 list-asset-type "nonExistingAssetType"
    [ "$status" -eq 255 ]
    check_line 0 "'nonExistingAssetType' does not refer to any known asset type"
}

@test "list-asset-types" {
    echo "Nothing interesting to test here"
}

@test "list-built-transaction" {
    run bash -c "$CLI2 list-built-transaction \"unknown\""
    check_line 0 "No txn 'unknown' found."
}

@test "list-built-transactions" {
    echo "Nothing interesting to test here"
}

@test "list-config" {
    echo "Nothing interesting to test here"
}

@test "list-keypair" {
    run bash -c "$CLI2 list-keypair unknown"

    [ "$status" -eq 255 ]
    check_line 0 "No keypair with name 'unknown' found"
}

@test "list-keys" {
    echo "Nothing interesting to test here"
}

@test "list-public-key" {
    run bash -c "$CLI2 list-public-key doesnotexist"
    check_line 0 "No public key with name doesnotexist found"
}

@test "list-txn" {
     run bash -c "$CLI2 list-txn"
     check_line 0 "Error: No transaction currently in progress"
}

@test "list-txo" {
    run bash -c "$CLI2 list-txo doesnotexist"
    check_line 0 "No txo 'doesnotexist' found"
}

@test "list-txos" {
    echo "Nothing interesting to test here"
}

@test "load-keypair" {
    run bash -c "echo not_a_key_pair | $CLI2 load-keypair alice"

    [ "$status" -eq 255 ]
    check_line 0 "Could not parse key pair: expected ident at line 1 column 2"
}

@test "load-owner-memo" {
    run bash -c "$CLI2 load-owner-memo tx_id_does_not_exists"
    check_line 0 "No txo 'tx_id_does_not_exists' found."
}

@test "load-public-key" {
    run bash -c "echo 'not_a_public_key' | $CLI2 load-public-key bob"
    [ "$status" -eq 255 ]
    check_line 0 "Could not parse public key: expected ident at line 1 column 2"
}

@test "query-ledger-state" {
    run bash -c "echo N | $CLI2 query-ledger-state"
    [ "$status" -eq 255 ]
    check_line 1 "Cannot check ledger state validity without a signature key."
}

@test "query-txo" {
    run bash -c "$CLI2 query-txo bad_txo_id"
    [ "$status" -eq 255 ]
    check_line 0 "No TXO nicknamed 'bad_txo_id' found and no SID given!"
}

@test "query-txos" {
    echo "Nothing interesting to test here"
}

@test "setup" {
    echo "Nothing interesting to test here"
}

@test "show-owner-memo" {
    run bash -c "$CLI2 show-owner-memo bad_txo_id"
    [ "$status" -eq 255 ]
    check_line 0 "No txo 'bad_txo_id' found."
}

@test "simple-define-asset" {
    run  bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
                $MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice AliceCoin;"
    [ "$status" -eq 0 ]

    run bash -c "$MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice AliceCoin;"
    debug_lines
    [ "$status" -eq 255 ]
    check_line 19 "Asset type 'AliceCoin' already exists!"

    # Bob's key has not been created
    run bash -c "$MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset bob BobCoin;"
    
    check_line 11 "Enter password for bob: Password was incorrect, please try again."
    check_line 12 "Enter password for bob: Password was incorrect, please try again."
    check_line 13 "Enter password for bob: Error: Failed to read password"
    check_line 14 "   Caused by: The provided password was incorrect."
}

@test "simple-issue-asset" {
    run  bash -c "$PASSWORD_PROMPT | $CLI2 key-gen alice; \
                $MEMO_ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-define-asset alice AliceCoin;"
    [ "$status" -eq 0 ]

    # Issue the asset
    run bash -c "$ALICE_WITH_SEVERAL_PROMPTS | $CLI2 simple-issue-asset BobCoin 10000"
    
    [ "$status" -eq 255 ]
    check_line 2 "No asset type with name 'BobCoin' found"
}

@test "status" {
    run bash -c "$CLI2 status bad_tx_id"
    [ "$status" -eq 255 ]
    check_line 0 "No txn 'bad_tx_id' found."
}

@test "status-check" {
    run bash -c "$CLI2 status-check bad_tx_id"
    
    [ "$status" -eq 255 ]
    check_line 0 "No txn 'bad_tx_id' found."
}

@test "submit" {
    run bash -c "$CLI2 submit bad_tx_id"
    
    [ "$status" -eq 255 ]
    check_line 0 "No transaction 'bad_tx_id' found."
}

@test "transfer-assets-empty-tx" {

    run bash -c "echo y | $CLI2 query-ledger-state"
    [ "$status" -eq 0 ]

    run bash -c "$CLI2 initialize-transaction 0"
    [ "$status" -eq 0 ]

    run bash -c "$CLI2 transfer-assets --builder=0"
    [ "$status" -eq 0 ]
    check_line 0 "There is no input available for building the transaction. You need to issue some asset or receive some funds to your address."
}

@test "unlock-txo" {
    run bash -c "$CLI2 unlock-txo bad_tx_id"
    [ "$status" -eq 255 ]
    check_line 0 "No txo 'bad_tx_id' found."
}
