# If breaking because blockchain, add the sleep time.
set +e

source ./tools/devnet/env.sh || exit 1

FRA_ACCOUNT="fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5"

ANON_SK_1="Ccv2h8u1g__HJBrsA8npcs4CiDQ_UHI-JGZCjXbu9Un8HU3qSTf3PdLEFvs1XwauSltgruFv-IRVFpaQkeIIAgRoRPXncS1VHYzRpQlghzgCcQKJnic90DFDiYxSPVjg"
ANON_VK_1="_B1N6kk39z3SxBb7NV8GrkpbYK7hb_iEVRaWkJHiCAI="
ANON_PK_1="BGhE9edxLVUdjNGlCWCHOAJxAomeJz3QMUOJjFI9WOA="

ANON_SK_2="h4MuWol8pWuNIMxPHwJ0ZAoF_n51QScj6AultG5IHU3yL-LR02XXw58uudwom_tahcy1e0oadfOw3oLxSs64A9yTOKFC1NqT6e-fWGEO-QpSZzf8otV7POguvdejoKhL"
ANON_VK_2="8i_i0dNl18OfLrncKJv7WoXMtXtKGnXzsN6C8UrOuAM="
ANON_PK_2="3JM4oULU2pPp759YYQ75ClJnN_yi1Xs86C6916OgqEs="

ANON_SK_3="bRrcmHV-87-na2jKuOEQZmVyLE6q4oVdCiMoWdqVHwOqkAlAXybyeheaNCyWw7j0lz4vlnxP5nUNpbnSwF3tBiXKJs7KF1X9zc9ZUy_3U8-2YnyrGSWbQ-QIpNVmBGvy"
ANON_VK_3="qpAJQF8m8noXmjQslsO49Jc-L5Z8T-Z1DaW50sBd7QY="
ANON_PK_3="JcomzsoXVf3Nz1lTL_dTz7ZifKsZJZtD5Aik1WYEa_I="

FILE_MNEMONIC="mnemonic-temp.keys"
FILE_ANON_KEYS_1="anon-keys-temp_1.keys"
FILE_ANON_KEYS_2="anon-keys-temp_2.keys"
FILE_ANON_KEYS_3="anon-keys-temp_3.keys"

# FRA: fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" > $FILE_MNEMONIC

echo "
{
  \"spend_key\": \"Ccv2h8u1g__HJBrsA8npcs4CiDQ_UHI-JGZCjXbu9Un8HU3qSTf3PdLEFvs1XwauSltgruFv-IRVFpaQkeIIAgRoRPXncS1VHYzRpQlghzgCcQKJnic90DFDiYxSPVjg\",
  \"view_key\": \"_B1N6kk39z3SxBb7NV8GrkpbYK7hb_iEVRaWkJHiCAI=\",
  \"pub_key\": \"BGhE9edxLVUdjNGlCWCHOAJxAomeJz3QMUOJjFI9WOA=\"
}" > $FILE_ANON_KEYS_1

echo "
{
  \"spend_key\": \"h4MuWol8pWuNIMxPHwJ0ZAoF_n51QScj6AultG5IHU3yL-LR02XXw58uudwom_tahcy1e0oadfOw3oLxSs64A9yTOKFC1NqT6e-fWGEO-QpSZzf8otV7POguvdejoKhL\",
  \"view_key\": \"8i_i0dNl18OfLrncKJv7WoXMtXtKGnXzsN6C8UrOuAM=\",
  \"pub_key\": \"3JM4oULU2pPp759YYQ75ClJnN_yi1Xs86C6916OgqEs=\"
}" > $FILE_ANON_KEYS_2

echo "
{
  \"spend_key\": \"bRrcmHV-87-na2jKuOEQZmVyLE6q4oVdCiMoWdqVHwOqkAlAXybyeheaNCyWw7j0lz4vlnxP5nUNpbnSwF3tBiXKJs7KF1X9zc9ZUy_3U8-2YnyrGSWbQ-QIpNVmBGvy\",
  \"view_key\": \"qpAJQF8m8noXmjQslsO49Jc-L5Z8T-Z1DaW50sBd7QY=\",
  \"pub_key\": \"JcomzsoXVf3Nz1lTL_dTz7ZifKsZJZtD5Aik1WYEa_I=\"
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
"$BIN"/fn asset --issue --code $ASSET1 --amount 100000000
echo "waiting blockchain 15s..."
sleep 15
# txo-sid = 14(asset1) & 16

echo "\n\n\n Issue Asset 2 ..."
echo "------------------------------------------------------------------------------"
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
ANON_KEY_2_ASSET_1_COMMITMENT=$(awk 'FNR==2' sent_commitments)  # ASSET 1

BATCH_SK="batch_sk.keys"
BATCH_C="batch_c.keys"
BATCH_PK="batch_pk.keys"
BATCH_AMOUNT="batch_amount.keys"
BATCH_ASSET="batch_asset.keys"

echo $ANON_SK_1 > $BATCH_SK
echo $ANON_SK_1 >> $BATCH_SK
echo $ANON_SK_2 >> $BATCH_SK

echo $ANON_KEY_1_FRA_COMMITMENT > $BATCH_C
echo $ANON_KEY_1_ASSET_2_COMMITMENT >> $BATCH_C
echo $ANON_KEY_2_ASSET_1_COMMITMENT >> $BATCH_C

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
  --axfr-secretkey-file $BATCH_SK     \
  --commitment-file $BATCH_C          \
  --to-axfr-public-key-file $BATCH_PK \
  --amount-file $BATCH_AMOUNT         \
  --asset-file $BATCH_ASSET
echo "waiting blockchain 15s..."
sleep 15

echo "checking..."
"$BIN"/fn owned-abars --commitments $(awk 'FNR==3,FNR==4' sent_commitments | awk -v d="," '{s=(NR==1?s:s d)$0}END{print s}') --anon-keys ./$FILE_ANON_KEYS_2
"$BIN"/fn owned-abars --commitments $(awk 'FNR==5' sent_commitments) --anon-keys ./$FILE_ANON_KEYS_3

rm $BATCH_SK $BATCH_C $BATCH_PK $BATCH_AMOUNT $BATCH_ASSET
echo "\n ***** Test all successfully! ***** "
