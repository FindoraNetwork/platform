#!/usr/bin/env bash

set -e

# run env scripts
TRIPLE_MASKING_SCRIPTS_PATH="tools/regression/triple_masking/scripts"
source $TRIPLE_MASKING_SCRIPTS_PATH/env.sh

# declare sleep intervals
let TM_SLEEP=($BLOCK_INTERVAL + 5) # If breaking, increase the sleep time

# generate keys and create test bars
source $TRIPLE_MASKING_SCRIPTS_PATH/gen_keys_test-bars.sh


# Run Tests
echo -e "${YEL}Run test cases and verifying results${NC}"

echo "\n ***** Verifying balances ***** "
python3 "$REGRESSION_PATH"/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 840000000
if [ $? != 0 ];
then
    exit 1
fi
echo

# test eth to fra
echo -e "\n ***** Test transfer from eth to fra prefix address *****"
$BIN/fn transfer --amount 209990000 --asset FRA -T $ED_ADDRESS
sleep $BLOCK_INTERVAL

echo "\n ***** Verifying balances ***** "
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $ED_SEC_KEY --amount 210010000
if [ $? != 0 ];
then
    exit 1
fi
echo
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 630000000
if [ $? != 0 ];
then
    exit 1
fi
echo

# test fra to eth
echo -e "\n ***** Test transfer from fra to eth prefix address *****"
$BIN/fn transfer --amount 210000000 --asset FRA -f ./$FILE_FRA_KEY -T $FRA_ADDRESS
sleep $BLOCK_INTERVAL

echo "\n ***** Verifying balances ***** "
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $ED_SEC_KEY --amount 0
if [ $? != 0 ];
then
    exit 1
fi
echo
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 840000000
if [ $? != 0 ];
then
    exit 1
fi
echo

# test bar to abar
echo -e "\n ***** Bar To Abar Conversion *****"
TXO_SID=$($BIN/fn owned-utxos | head -4 | tail -1 |  awk -F ' ' '{print $1}')
sleep 1
$BIN/fn convert-bar-to-abar --to-address $ANON_PK_1  --txo-sid $TXO_SID
sleep $TM_SLEEP

echo "\n ***** Verifying balances ***** "
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 629980000
if [ $? != 0 ];
then
    exit 1
fi
echo
commitment1=$(tail -n 1 owned_commitments)
printf $ANON_SK_1 > temp
python3 $REGRESSION_PATH/evm.py verify-anon-balance --from-seckey ./temp --commitments $commitment1 --amount 210000000
if [ $? != 0 ];
then
    exit 1
fi

# test anonymous transfers
echo -e "\n ***** Anonymous Transfer from Sender1 to Receiver1 ***** "
printf $ANON_SK_1 > temp
$BIN/fn anon-transfer --amount 189990000 --from-seckey ./temp --to-address $ANON_PK_2 --commitment $commitment1 > /dev/null
sleep $TM_SLEEP

commitment2=$(tail -n 1 sent_commitments)
echo -e "\n ***** Owned Abars for Receiver1 after Anon Transfer 1 ***** "
printf $ANON_SK_2 > temp
$BIN/fn owned-abars --commitments $commitment2 --from-seckey ./temp

echo -e "\n ***** Verifying balances ***** "
printf $ANON_SK_1 > temp
python3 "$REGRESSION_PATH"/evm.py verify-anon-balance --from-seckey ./temp --commitments $commitment1 --amount 0
if [ $? != 0 ];
then
    exit 1
fi
printf $ANON_SK_2 > temp
python3 "$REGRESSION_PATH"/evm.py verify-anon-balance --from-seckey ./temp --commitments $commitment2 --amount 189990000
if [ $? != 0 ];
then
    exit 1
fi


echo -e "\n ***** Anonymous Transfer from Receiver1 (Sender2) to Receiver2 ***** "
printf $ANON_SK_2 > temp
$BIN/fn anon-transfer --amount 169990000 --from-seckey ./temp --to-address $ANON_PK_3 --commitment $commitment2 > /dev/null
sleep $TM_SLEEP

commitment3=$(tail -n 1 sent_commitments)
echo -e "\n ***** Owned Abars for Receiver2 after Anon Transfer 2 ***** "
printf $ANON_SK_3 > temp
$BIN/fn owned-abars --commitments $commitment3 --from-seckey ./temp

echo -e "\n ***** Verifying balances ***** "
printf $ANON_SK_2 > temp
python3 $REGRESSION_PATH/evm.py verify-anon-balance --from-seckey ./temp --commitments $commitment2 --amount 0
if [ $? != 0 ];
then
    exit 1
fi
printf $ANON_SK_3 > temp
python3 $REGRESSION_PATH/evm.py verify-anon-balance --from-seckey ./temp --commitments $commitment3 --amount 169990000
if [ $? != 0 ];
then
    exit 1
fi

sleep 2
echo -e "\n Fetch merkle proof for Anon Transfer 2"
$BIN/fn anon-fetch-merkle-proof -a 2 > /dev/null


# test abar to bar
$BIN/fn owned-utxos

echo -e "\n ***** Bar To Abar Conversion ***** "
TXO_SID=$($BIN/fn owned-utxos | sed \$d | sed \$d | sed \$d |sort -k 1 -n | head -4 | tail -1 |  awk -F ' ' '{print $1}')
sleep 1
printf $ANON_SK_1 > temp
$BIN/fn convert-bar-to-abar --txo-sid "$TXO_SID" --to-address $ANON_PK_1
sleep $TM_SLEEP

commitment4=$(tail -n 1 owned_commitments)
echo -e "\n Owned Abars after Bar to Abar conversion"
printf $ANON_SK_1 > temp
$BIN/fn owned-abars --commitments "$commitment4" --from-seckey ./temp

echo -e "\n ***** Verifying balances ***** "
python3 "$REGRESSION_PATH"/evm.py verify-balance --sec-key "$BAR_SEC_KEY" --amount 419960000
if [ $? != 0 ];
then
    exit 1
fi
printf $ANON_SK_1 > temp
python3 "$REGRESSION_PATH"/evm.py verify-anon-balance --from-seckey ./temp --commitments "$commitment4" --amount 210000000
if [ $? != 0 ];
then
    exit 1
fi


echo -e "\n ***** Abar To Bar Conversion ***** "
printf $ANON_SK_1 > temp
$BIN/fn convert-abar-to-bar --from-seckey ./temp -c "$commitment4" --to-wallet-address $FRA_ADDRESS > /dev/null
sleep $TM_SLEEP

echo -e "\n ***** Verifying balances ***** "
python3 "$REGRESSION_PATH"/evm.py verify-balance --sec-key "$BAR_SEC_KEY" --amount 629960000
if [ $? != 0 ];
then
    exit 1
fi
printf $ANON_SK_1 > temp
python3 "$REGRESSION_PATH"/evm.py verify-anon-balance --from-seckey ./temp --commitments "$commitment4" --amount 0
if [ $? != 0 ];
then
    exit 1
fi

./"$TM_REGRESSION_PATH"/asset_mixing_test.sh