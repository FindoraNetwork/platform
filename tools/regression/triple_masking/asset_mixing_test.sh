#!/usr/bin/env bash
set +e

EVM_SCRIPTS_PATH="tools/regression/evm/scripts"
TRIPLE_MASKING_SCRIPTS_PATH="tools/regression/triple_masking/scripts"
source $EVM_SCRIPTS_PATH/env.sh
source $TRIPLE_MASKING_SCRIPTS_PATH/env.sh
SLEEP_INTERVAL=($"BLOCK_INTERVAL" + 1)
TM_SLEEP=20

#Run Tests
echo -e "${YEL}Run asset mixing test cases and verify results${NC}"

FRA_ACCOUNT="fra1peaqwykz6vgpvhxfu60w6lngqw6jvwr0f8cyzzvmaqa3dn6xsy4qan9rne"
BAR_SEC_KEY="KtbqhEo_FJaQoq2fV5K4niayxz_VK8LHJBqU1eV5x0o="

ANON_SK_1="J7PqRhmBOE_gadFs4rB4lcKuz_YoWa5VSlALyKuZdQjNBryPSYZhRczonGNY3-mp86LWW8TJ6clirfk4gk03Tw=="
ANON_PK_1="zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08="
ANON_ENC_1="Gu558brzFchoqQR9oi8QP54KZKSQ18Djzt82C4YUyFg="
ANON_DEC_1="4GNC0J_qOXV2kww5BC5bOCyrTEfCodX5BoFaj06uN1s="

ANON_SK_2="MwdsbYhTp4Io062nV7E2HkJfsnaTCZpkdjr6aijv2Aem3KjuGWqf4TLB_-20b305Ja3Pop8NS8tgMNUOVXUL5Q=="
ANON_PK_2="ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U="
ANON_ENC_2="SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI="
ANON_DEC_2="AEq1ZUFk_fB__YaNjQ3D2taGOnMZAx4adpB6RbnPj24="

ANON_SK_3="vw41h9OciN5cDh8QSPFEodSA5AnuHxxcV5SKJ_q-JQMGUX_qogo-lXUvhnXo1gYuP8YsoJJro7kNloXx8wfDSA=="
ANON_PK_3="BlF_6qIKPpV1L4Z16NYGLj_GLKCSa6O5DZaF8fMHw0g="
ANON_ENC_3="NBY5yIhdriJVq-7BS59J2IxBgLhewr8TEE6suNc1elA="
ANON_DEC_3="OKh4F5o_Mw0eKi0xU8lQhi_lXMVd-hem7N12lvSy_WU="

FILE_ANON_KEYS="anon-keys-temp_1.keys"
FILE_ANON_KEYS_2="anon-keys-temp_2.keys"
FILE_ANON_KEYS_3="anon-keys-temp_3.keys"

set -e
./$TRIPLE_MASKING_SCRIPTS_PATH/create_test_bars.sh $FRA_ACCOUNT
#Verify FRA balance
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 840000000
echo
set +e

rm owned_commitments
rm sent_commitments

./$TRIPLE_MASKING_SCRIPTS_PATH/setup_wallets.sh
echo "\n ***** Setup accounts successfully! ***** "

set -e

$BIN/fn setup -O "$TM_REGRESSION_PATH"/mnemonic.key > /dev/null

echo "\n\n FRA Bar To Abar ..."
echo "==============================================================================="
TXO_SID=$(target/release/fn owned-utxos | head -4 | tail -1 |  awk -F ' ' '{print $1}')
target/release/fn convert-bar-to-abar --anon-keys $FILE_ANON_KEYS --txo-sid "$TXO_SID"
echo "waiting blockchain 20s..."
sleep 30

#Verify FRA balance
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 629980000
commitment1=$(tail -n 1 owned_commitments)
python $REGRESSION_PATH/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS --commitments $commitment1 --amount 210000000
echo

echo "\n\n Create Asset 1 ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --create --memo "asset1" --transferable 2> /dev/null
echo "waiting blockchain 20s..."
sleep $TM_SLEEP

echo "\n\n Create Asset 2 ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --create --memo "asset2" --transferable 2> /dev/null
echo "waiting blockchain 20s..."
sleep $TM_SLEEP

echo "\n\n Building assets ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --show --addr $FRA_ACCOUNT > tmp_file
ASSET1=$(awk 'FNR==1' tmp_file | awk -F ' ' '{print $2}'| sed 's/,*$//g')
ASSET2=$(awk 'FNR==2' tmp_file | awk -F ' ' '{print $2}'| sed 's/,*$//g')
echo $ASSET1
echo $ASSET2

echo "\n\n Issue Asset 1 ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --issue --code $ASSET1 --amount 100000000
echo "waiting blockchain 20s..."
sleep $TM_SLEEP

echo "\n\n\n Issue Asset 2 ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --issue --code $ASSET2 --amount 100000000
echo "waiting blockchain 20s..."
sleep $TM_SLEEP

echo "\n ***** Issue Asset & FRA successfully! ***** "

#Verify balance for custom assets
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 100000000 --asset $ASSET1
echo
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 100000000 --asset $ASSET2
echo

target/release/fn owned-utxos

echo "\n\n Asset 1 Bar To Abar ..."
echo "==============================================================================="
TXO_SID=$(target/release/fn owned-utxos --asset "$ASSET1" | head -4 | tail -1 | awk -F ' ' '{print $1}')
target/release/fn convert-bar-to-abar --anon-keys $FILE_ANON_KEYS --txo-sid "$TXO_SID"
echo "waiting blockchain 20s..."
sleep $TM_SLEEP

echo "\n\n Asset 2 Bar To Abar ..."
echo "==============================================================================="
TXO_SID=$(target/release/fn owned-utxos --asset "$ASSET2" | head -4 | tail -1 | awk -F ' ' '{print $1}')
target/release/fn convert-bar-to-abar --anon-keys $FILE_ANON_KEYS --txo-sid "$TXO_SID"
echo "waiting blockchain 20s..."
sleep $TM_SLEEP

#Verify balance for custom assets
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 0 --asset $ASSET1
commitmentAsset1=$(tail -n 2 owned_commitments | head -n 1)
python $REGRESSION_PATH/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS --commitments $commitmentAsset1 --amount 100000000 --asset $ASSET1
echo
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 0 --asset $ASSET2
commitmentAsset2=$(tail -n 1 owned_commitments)
python $REGRESSION_PATH/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS --commitments $commitmentAsset2 --amount 100000000 --asset $ASSET2
echo

#Verify FRA balance
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 629900000
echo

echo "\n\n Anon transfer Asset 1 ..."
echo "==============================================================================="
COMMITMENT=$(awk 'FNR==3' owned_commitments)
FRA_COMMITMENT=$(awk 'FNR==2' owned_commitments)
target/release/fn anon-transfer    \
  --amount 50000000                \
  --anon-keys $FILE_ANON_KEYS    \
  --commitment $COMMITMENT         \
  --fra-commitment $FRA_COMMITMENT \
  --to-axfr-public-key $ANON_PK_2  \
  --to-enc-key $ANON_ENC_2
echo "waiting for transaction to complete..."
sleep $TM_SLEEP

echo "\n\n Anon transfer batch: Asset 1 & Asset 2 & FRA ..."
echo "==============================================================================="
echo "input  => key1 * FRA + key1 * asset2 + key2 * asset1"
echo "output => key2 * FRA + key2 * asset2 + key3 * asset1"

ANON_KEY_1_FRA_COMMITMENT=$(awk 'FNR==6' owned_commitments)     # FRA
ANON_KEY_1_ASSET_1_COMMITMENT=$(awk 'FNR==5' owned_commitments) # ASSET 1
ANON_KEY_1_ASSET_2_COMMITMENT=$(awk 'FNR==4' owned_commitments) # ASSET 2
ANON_KEY_2_ASSET_1_COMMITMENT=$(awk 'FNR==2' sent_commitments)  # ASSET 1

BATCH_SK="batch_sk.keys"
BATCH_DEC="batch_dec.keys"
BATCH_C="batch_c.keys"
BATCH_PK="batch_pk.keys"
BATCH_ENC="batch_enc.keys"
BATCH_AMOUNT="batch_amount.keys"
BATCH_ASSET="batch_asset.keys"

echo $ANON_SK_1 > $BATCH_SK
echo $ANON_SK_1 >> $BATCH_SK
echo $ANON_SK_2 >> $BATCH_SK

echo $ANON_DEC_1 > $BATCH_DEC
echo $ANON_DEC_1 >> $BATCH_DEC
echo $ANON_DEC_2 >> $BATCH_DEC

echo $ANON_KEY_1_FRA_COMMITMENT > $BATCH_C
echo $ANON_KEY_1_ASSET_2_COMMITMENT >> $BATCH_C
echo $ANON_KEY_2_ASSET_1_COMMITMENT >> $BATCH_C

echo $ANON_PK_2 > $BATCH_PK
echo $ANON_PK_2 >> $BATCH_PK
echo $ANON_PK_3 >> $BATCH_PK

echo $ANON_ENC_2 > $BATCH_ENC
echo $ANON_ENC_2 >> $BATCH_ENC
echo $ANON_ENC_3 >> $BATCH_ENC

echo "" > $BATCH_ASSET
echo $ASSET2 >> $BATCH_ASSET
echo $ASSET1 >> $BATCH_ASSET

echo 10000000 > $BATCH_AMOUNT
echo 10000000 >> $BATCH_AMOUNT
echo 10000000 >> $BATCH_AMOUNT

echo ""
echo "Sending multi-asset transaction..."
target/release/fn anon-transfer-batch \
  --axfr-secretkey-file $BATCH_SK     \
  --decryption-key-file $BATCH_DEC    \
  --commitment-file $BATCH_C          \
  --to-axfr-public-key-file $BATCH_PK \
  --to-enc-key-file $BATCH_ENC        \
  --amount-file $BATCH_AMOUNT         \
  --asset-file $BATCH_ASSET         > /dev/null
echo "waiting for transaction to complete..."
sleep $TM_SLEEP

echo "checking..."
target/release/fn owned-abars --commitments $(awk 'FNR==3,FNR==4' sent_commitments | awk -v d="," '{s=(NR==1?s:s d)$0}END{print s}') --anon-keys ./$FILE_ANON_KEYS_2
target/release/fn owned-abars --commitments $(awk 'FNR==5' sent_commitments) --anon-keys ./$FILE_ANON_KEYS_3

python "$REGRESSION_PATH"/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS_2 --commitments "$(awk 'FNR==3' sent_commitments)" --amount 10000000
python "$REGRESSION_PATH"/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS_2 --commitments "$(awk 'FNR==4' sent_commitments)" --amount 10000000 --asset "$ASSET2"
python "$REGRESSION_PATH"/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS_3 --commitments "$(awk 'FNR==5' sent_commitments)" --amount 10000000 --asset "$ASSET1"


rm $BATCH_SK $BATCH_DEC $BATCH_C $BATCH_PK $BATCH_ENC $BATCH_AMOUNT $BATCH_ASSET
echo -e "\n ***** Test all successfully! ***** "