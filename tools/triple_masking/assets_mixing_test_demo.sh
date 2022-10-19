# If breaking because blockchain, add the sleep time.
set +e

source ./tools/devnet/env.sh || exit 1

FRA_ACCOUNT="fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5"

ANON_SK_1="6kpJDnAoL-_ZHekWoJBfrCmHnpYRs7WPMxdG_F9hJoQMhcLuDK2su2b4-IdYATM0Ou99yAPYcvSNdLSGnf5hBIA="
ANON_PK_1="DIXC7gytrLtm-PiHWAEzNDrvfcgD2HL0jXS0hp3-YQSA"

ANON_SK_2="fGAGGWYkAqcQ1DK7CbQhSbKde8WOcNld1fJlIOaDJmcdkecYCm24SiBZu44fw8uky5bBP1_1pILXyvj1O3ydJgA="
ANON_PK_2="HZHnGAptuEogWbuOH8PLpMuWwT9f9aSC18r49Tt8nSYA"

ANON_SK_3="7vgfKzBy8zRfLd_D5ft4ZCj1J0MzGXjfXA-vLt8mi8MnLZAYsaV9glPBbAxtCwW1ddDmcFagDBHSEhI9MB7XMQA="
ANON_PK_3="Jy2QGLGlfYJTwWwMbQsFtXXQ5nBWoAwR0hISPTAe1zEA"

FILE_MNEMONIC="mnemonic-temp.keys"
FILE_ANON_KEYS_1="anon-keys-temp_1.keys"
FILE_ANON_KEYS_2="anon-keys-temp_2.keys"
FILE_ANON_KEYS_3="anon-keys-temp_3.keys"

# FRA: fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" > $FILE_MNEMONIC

echo "
{
  \"spend_key\": \"6kpJDnAoL-_ZHekWoJBfrCmHnpYRs7WPMxdG_F9hJoQMhcLuDK2su2b4-IdYATM0Ou99yAPYcvSNdLSGnf5hBIA=\",
  \"pub_key\": \"DIXC7gytrLtm-PiHWAEzNDrvfcgD2HL0jXS0hp3-YQSA\"
}" > $FILE_ANON_KEYS_1

echo "
{
  \"spend_key\": \"fGAGGWYkAqcQ1DK7CbQhSbKde8WOcNld1fJlIOaDJmcdkecYCm24SiBZu44fw8uky5bBP1_1pILXyvj1O3ydJgA=\",
  \"pub_key\": \"HZHnGAptuEogWbuOH8PLpMuWwT9f9aSC18r49Tt8nSYA\"
}" > $FILE_ANON_KEYS_2

echo "
{
  \"spend_key\": \"7vgfKzBy8zRfLd_D5ft4ZCj1J0MzGXjfXA-vLt8mi8MnLZAYsaV9glPBbAxtCwW1ddDmcFagDBHSEhI9MB7XMQA=\",
  \"pub_key\": \"Jy2QGLGlfYJTwWwMbQsFtXXQ5nBWoAwR0hISPTAe1zEA\"
}" > $FILE_ANON_KEYS_3

rm owned_commitments
rm sent_commitments
echo "\n ***** Setup accounts successfully! ***** "

set -e

echo "\n\n Transfer FRA to test account..."
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 100000000 --asset FRA -T $FRA_ACCOUNT
echo "waiting blockchain 5s..."
sleep 5
# txo-sid = 3

echo "\n\n Transfer FRA to pay fee..."
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 100000000 --asset FRA -T $FRA_ACCOUNT
echo "waiting blockchain 5s..."
sleep 5
# txo-sid = 6

"$BIN"/fn setup -O $FILE_MNEMONIC -S http://0.0.0.0

echo "Changed to test account. BAR Balance:"
"$BIN"/fn wallet --show

echo "\n\n FRA Bar To Abar ..."
echo "==============================================================================="
TXO_SID=$("$BIN"/fn owned-utxos | head -4 | tail -1 |  awk -F ' ' '{print $1}')
"$BIN"/fn convert-bar-to-abar --anon-keys $FILE_ANON_KEYS_1 --txo-sid "$TXO_SID"
echo "waiting blockchain 5s..."
sleep 5
# txo-sid = 9

echo "\n\n Create Asset 1 ..."
echo "------------------------------------------------------------------------------"
"$BIN"/fn asset --create --memo "asset1" --transferable 2> /dev/null
echo "waiting blockchain 15s..."
sleep 15

echo "\n\n Create Asset 2 ..."
echo "------------------------------------------------------------------------------"
"$BIN"/fn asset --create --memo "asset2" --transferable 2> /dev/null
echo "waiting blockchain 15s..."
sleep 15

echo "\n\n Building assets ..."
echo "------------------------------------------------------------------------------"
"$BIN"/fn asset --show --addr $FRA_ACCOUNT > tmp_file
ASSET1=$(awk 'FNR==1' tmp_file | awk -F ' ' '{print $2}'| sed 's/,*$//g')
ASSET2=$(awk 'FNR==2' tmp_file | awk -F ' ' '{print $2}'| sed 's/,*$//g')
echo "$ASSET1"
echo "$ASSET2"
# rm tmp_file

echo "\n\n Issue Asset 1 ..."
echo "------------------------------------------------------------------------------"
echo $ASSET1
"$BIN"/fn asset --issue --code "$ASSET1" --amount 100000000
echo "waiting blockchain 15s..."
sleep 15
# txo-sid = 14(asset1) & 16

echo "\n\n\n Issue Asset 2 ..."
echo "------------------------------------------------------------------------------"
echo $ASSET2
"$BIN"/fn asset --issue --code "$ASSET2" --amount 100000000
echo "waiting blockchain 15s..."
sleep 15

echo "\n ***** Issue Asset & FRA successfully! ***** "
sleep 5
"$BIN"/fn owned-utxos

echo "\n\n Asset 1 Bar To Abar ..."
echo "==============================================================================="
TXO_SID=$("$BIN"/fn owned-utxos --asset "$ASSET1" | head -4 | tail -1 | awk -F ' ' '{print $1}')
"$BIN"/fn convert-bar-to-abar --anon-keys $FILE_ANON_KEYS_1 --txo-sid "$TXO_SID"
echo "waiting blockchain 15s..."
sleep 15

echo "\n\n Asset 2 Bar To Abar ..."
echo "==============================================================================="
TXO_SID=$("$BIN"/fn owned-utxos --asset "$ASSET2" | head -4 | tail -1 | awk -F ' ' '{print $1}')
"$BIN"/fn convert-bar-to-abar --anon-keys $FILE_ANON_KEYS_1 --txo-sid "$TXO_SID"
echo "waiting blockchain 15s..."
sleep 15

echo "\n\n Anon transfer Asset 1 ..."
echo "==============================================================================="
COMMITMENT=$(awk 'FNR==3' owned_commitments)
FRA_COMMITMENT=$(awk 'FNR==2' owned_commitments)
"$BIN"/fn anon-transfer    \
  --amount 50000000                \
  --anon-keys $FILE_ANON_KEYS_1    \
  --commitment $COMMITMENT         \
  --fra-commitment $FRA_COMMITMENT \
  --to-axfr-public-key $ANON_PK_2
echo "waiting blockchain 15s..."
sleep 15

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
echo ""
"$BIN"/fn anon-transfer-batch \
  --anon-keys $ANON_SK_1     \
  --commitment-file $BATCH_C          \
  --to-axfr-public-key-file $BATCH_PK \
  --amount-file $BATCH_AMOUNT         \
  --asset-file $BATCH_ASSET
echo "waiting blockchain 15s..."
sleep 15

echo "checking..."
"$BIN"/fn owned-abars --commitments $(awk 'FNR==3,FNR==4' sent_commitments | awk -v d="," '{s=(NR==1?s:s d)$0}END{print s}') --anon-keys ./$FILE_ANON_KEYS_2
"$BIN"/fn owned-abars --commitments $(awk 'FNR==5' sent_commitments) --anon-keys ./$FILE_ANON_KEYS_3

rm $BATCH_C $BATCH_PK $BATCH_AMOUNT $BATCH_ASSET
echo "\n ***** Test all successfully! ***** "
