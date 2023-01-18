#!/usr/bin/env bash

set -e

# run env scripts
TRIPLE_MASKING_SCRIPTS_PATH="tools/regression/triple_masking/scripts"
source $TRIPLE_MASKING_SCRIPTS_PATH/env.sh

# declare sleep intervals
let TM_SLEEP=($BLOCK_INTERVAL + 5) # If breaking, increase the sleep time

# generate keys and create test bars
source $TRIPLE_MASKING_SCRIPTS_PATH/gen_keys_test-bars.sh

set +e
rm owned_commitments
rm sent_commitments
set -e

# Run Tests
echo -e "${YEL}Run test cases and verifying results${NC}"

#Verify FRA balance
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 840000000
if [ $? != 0 ];
then
    exit 1
fi
echo

echo "\n\n FRA Bar To Abar ..."
echo "==============================================================================="
TXO_SID=$($BIN/fn owned-utxos | head -4 | tail -1 |  awk -F ' ' '{print $1}')
$BIN/fn convert-bar-to-abar --to-address $ANON_PK_1 --txo-sid "$TXO_SID"
echo "waiting block time..."
sleep $TM_SLEEP

#Verify FRA balance
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 629980000
if [ $? != 0 ];
then
    exit 1
fi
commitment1=$(tail -n 1 owned_commitments)
printf $ANON_SK_1 > temp
python3 $REGRESSION_PATH/evm.py verify-anon-balance --from-seckey ./temp --commitments $commitment1 --amount 210000000
if [ $? != 0 ];
then
    exit 1
fi
echo

echo "\n\n Create Asset 1 ..."
echo "------------------------------------------------------------------------------"
$BIN/fn asset --create --memo "asset1" --transferable 2> /dev/null
echo "waiting block time..."
sleep $TM_SLEEP

echo "\n\n Create Asset 2 ..."
echo "------------------------------------------------------------------------------"
$BIN/fn asset --create --memo "asset2" --transferable 2> /dev/null
echo "waiting block time..."
sleep $TM_SLEEP

echo "\n\n Building assets ..."
echo "------------------------------------------------------------------------------"
$BIN/fn asset --show --addr $FRA_ADDRESS > tmp_file
ASSET1=$(awk 'FNR==1' tmp_file | awk -F ' ' '{print $2}'| sed 's/,*$//g')
ASSET2=$(awk 'FNR==2' tmp_file | awk -F ' ' '{print $2}'| sed 's/,*$//g')
echo $ASSET1
echo $ASSET2

echo "\n\n Issue Asset 1 ..."
echo "------------------------------------------------------------------------------"
$BIN/fn asset --issue --code $ASSET1 --amount 100000000
echo "waiting block time..."
sleep $TM_SLEEP

echo "\n\n\n Issue Asset 2 ..."
echo "------------------------------------------------------------------------------"
$BIN/fn asset --issue --code $ASSET2 --amount 100000000
echo "waiting block time..."
sleep $TM_SLEEP

echo "\n ***** Issue Asset & FRA successfully! ***** "

#Verify balance for custom assets
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 100000000 --asset="$ASSET1"
if [ $? != 0 ];
then
    exit 1
fi
echo
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 100000000 --asset="$ASSET2"
if [ $? != 0 ];
then
    exit 1
fi
echo

$BIN/fn owned-utxos

echo "\n\n Asset 1 Bar To Abar ..."
echo "==============================================================================="
TXO_SID=$($BIN/fn owned-utxos --asset "$ASSET1" | head -4 | tail -1 | awk -F ' ' '{print $1}')
$BIN/fn convert-bar-to-abar --to-address $ANON_PK_1 --txo-sid "$TXO_SID"
echo "waiting block time..."
sleep $TM_SLEEP

echo "\n\n Asset 2 Bar To Abar ..."
echo "==============================================================================="
TXO_SID=$($BIN/fn owned-utxos --asset "$ASSET2" | head -4 | tail -1 | awk -F ' ' '{print $1}')
$BIN/fn convert-bar-to-abar --to-address $ANON_PK_1 --txo-sid "$TXO_SID"
echo "waiting block time..."
sleep $TM_SLEEP

#Verify balance for custom assets
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 0 --asset="$ASSET1"
if [ $? != 0 ];
then
    exit 1
fi
commitmentAsset1=$(tail -n 2 owned_commitments | head -n 1)
printf $ANON_SK_1 > temp
python3 $REGRESSION_PATH/evm.py verify-anon-balance --from-seckey ./temp --commitments $commitmentAsset1 --amount 100000000 --asset="$ASSET1"
if [ $? != 0 ];
then
    exit 1
fi
echo
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 0 --asset="$ASSET2"
if [ $? != 0 ];
then
    exit 1
fi
commitmentAsset2=$(tail -n 1 owned_commitments)
printf $ANON_SK_1 > temp
python3 $REGRESSION_PATH/evm.py verify-anon-balance --from-seckey ./temp --commitments $commitmentAsset2 --amount 100000000 --asset="$ASSET2"
if [ $? != 0 ];
then
    exit 1
fi
echo

#Verify FRA balance
python3 $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 629900000
if [ $? != 0 ];
then
    exit 1
fi
echo

echo "\n\n Anon transfer Asset 1 ..."
echo "==============================================================================="
COMMITMENT=$(awk 'FNR==3' owned_commitments)
FRA_COMMITMENT=$(awk 'FNR==2' owned_commitments)
printf $ANON_SK_1 > temp
$BIN/fn anon-transfer              \
  --amount 50000000                \
  --from-seckey ./temp             \
  --commitment $COMMITMENT         \
  --fra-commitment $FRA_COMMITMENT \
  --to-address $ANON_PK_2
echo "waiting for transaction to complete..."
sleep $TM_SLEEP

echo "\n\n Anon transfer batch: Asset 1 & Asset 2 & FRA ..."
echo "==============================================================================="
echo "input  => key1 * FRA + key1 * asset2 + key2 * asset1"
echo "output => key2 * FRA + key2 * asset2 + key3 * asset1"

ANON_KEY_1_FRA_COMMITMENT=$(awk 'FNR==6' owned_commitments)     # FRA
ANON_KEY_1_ASSET_1_COMMITMENT=$(awk 'FNR==5' owned_commitments) # ASSET 1
ANON_KEY_1_ASSET_2_COMMITMENT=$(awk 'FNR==4' owned_commitments) # ASSET 2

BATCH_C="batch_c.keys"
BATCH_PK="batch_pk.keys"
BATCH_AMOUNT="batch_amount.keys"
BATCH_ASSET="batch_asset.keys"

echo $ANON_KEY_1_FRA_COMMITMENT > $BATCH_C
echo $ANON_KEY_1_ASSET_2_COMMITMENT >> $BATCH_C
echo $ANON_KEY_1_ASSET_1_COMMITMENT >> $BATCH_C

echo $ANON_PK_2 > $BATCH_PK
echo $ANON_PK_2 >> $BATCH_PK
echo $ANON_PK_3 >> $BATCH_PK

echo "" > $BATCH_ASSET
echo $ASSET2 >> $BATCH_ASSET
echo $ASSET1 >> $BATCH_ASSET

echo 10000000 > $BATCH_AMOUNT
echo 10000000 >> $BATCH_AMOUNT
echo 50000000 >> $BATCH_AMOUNT

echo ""
echo "Sending multi-asset transaction..."
printf $ANON_SK_1 > temp
$BIN/fn anon-transfer-batch           \
  --from-seckey ./temp                \
  --commitment-file $BATCH_C          \
  --to-address-file $BATCH_PK \
  --amount-file $BATCH_AMOUNT         \
  --asset-file $BATCH_ASSET         > /dev/null
echo "waiting for transaction to complete..."
sleep $TM_SLEEP

echo "checking..."
printf $ANON_SK_2 > temp
$BIN/fn owned-abars --commitments $(awk 'FNR==3,FNR==4' sent_commitments | awk -v d="," '{s=(NR==1?s:s d)$0}END{print s}') --from-seckey ./temp
printf $ANON_SK_3 > temp
$BIN/fn owned-abars --commitments $(awk 'FNR==5' sent_commitments) --from-seckey ./temp

printf $ANON_SK_2 > temp
python3 "$REGRESSION_PATH"/evm.py verify-anon-balance --from-seckey ./temp --commitments "$(awk 'FNR==3' sent_commitments)" --amount 10000000
if [ $? != 0 ];
then
    exit 1
fi
printf $ANON_SK_2 > temp
python3 "$REGRESSION_PATH"/evm.py verify-anon-balance --from-seckey ./temp --commitments "$(awk 'FNR==4' sent_commitments)" --amount 10000000 --asset "$ASSET2"
if [ $? != 0 ];
then
    exit 1
fi
printf $ANON_SK_3 > temp
python3 "$REGRESSION_PATH"/evm.py verify-anon-balance --from-seckey ./temp --commitments "$(awk 'FNR==5' sent_commitments)" --amount 50000000 --asset "$ASSET1"
if [ $? != 0 ];
then
    exit 1
fi


rm $BATCH_C $BATCH_PK $BATCH_AMOUNT $BATCH_ASSET
echo -e "\n ***** Tested all successfully! ***** "
