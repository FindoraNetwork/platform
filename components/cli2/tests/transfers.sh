#!/usr/bin/env bats

source "tests/common.sh"

get_record_type(){
    is_amount_confidential=$1
    is_asset_confidential=$2

    # Amount
    if [[ $is_amount_confidential == "Y" ]]
    then
        AMOUNT_TYPE="ConfidentialAmount"
    else
        AMOUNT_TYPE="NonConfidentialAmount"
    fi

    # Asset
    if [[ $is_asset_confidential == "Y" ]]
    then
        ASSET_TYPE="ConfidentialAssetType"
    else
        ASSET_TYPE="NonConfidentialAssetType"
    fi

    echo "${AMOUNT_TYPE}_${ASSET_TYPE}"
}

get_amount_displayed(){
    amount=$1
    is_amount_confidential=$2

    if [[ $is_amount_confidential == "Y" ]]
    then
        res="<SECRET>"
    else
        res="$amount"
    fi

    echo "$res"
}

check_transfer()
{
    is_amount_confidential=$1
    is_asset_confidential=$2

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
    transfer_assets "$amount" "$change_amount" "$is_amount_confidential" "$is_asset_confidential" "true" "alice" "bob" "false" "Y"
    [ "$status" -eq 0 ]

    check_line 5 "  TransferAssets:"
    check_line 10 "    Record Type: \"NonConfidentialAmount_NonConfidentialAssetType\""
    check_line 11 "    Amount: 10000"

    amount_displayed=$(get_amount_displayed $amount $is_amount_confidential)
    check_line 22 "    Amount: $amount_displayed"

    record_type=$(get_record_type "$is_amount_confidential" "$is_asset_confidential")
    echo "RECORD_TYPE=$record_type"
    check_line 29 "    Record Type: \"$record_type\""
    check_line_err 60 "Committed!"

    run bash -c "$CLI2 list-txos --unspent=true"
    # debug_lines
    [ "$status" -eq 0 ]
    # debug_lines
    check_line 2 " Owned by: \"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\" (bob)"
    check_line 4 " Amount: $amount_displayed"

}

@test "transfer-amount-non-confidential-asset-non-confidential" {
    check_transfer "n" "n"
}

@test "transfer-amount-non-confidential-asset-confidential" {
    check_transfer "n" "Y"
}

@test "transfer-amount-confidential-asset-non-confidential" {
    check_transfer "Y" "n"
}

@test "transfer-amount-confidential-asset-confidential" {
    check_transfer "Y" "Y"
}

