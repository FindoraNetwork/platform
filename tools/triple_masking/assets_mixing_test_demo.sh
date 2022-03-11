# If breaking because blockchain, add the sleep time.
set +e

FRA_ACCOUNT="fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5"

ANON_SK_1="J7PqRhmBOE_gadFs4rB4lcKuz_YoWa5VSlALyKuZdQjNBryPSYZhRczonGNY3-mp86LWW8TJ6clirfk4gk03Tw=="
ANON_PK_1="zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08="
ANON_ENC_1="Gu558brzFchoqQR9oi8QP54KZKSQ18Djzt82C4YUyFg="
ANON_DEC_1="4GNC0J_qOXV2kww5BC5bOCyrTEfCodX5BoFaj06uN1s="

ANON_SK_2="MwdsbYhTp4Io062nV7E2HkJfsnaTCZpkdjr6aijv2Aem3KjuGWqf4TLB_-20b305Ja3Pop8NS8tgMNUOVXUL5Q=="
ANON_PK_2="ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U="
ANON_ENC_2="SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI="
ANON_DEC_2="AEq1ZUFk_fB__YaNjQ3D2taGOnMZAx4adpB6RbnPj24="

ANON_SK_3="GlEc6A-SDpkLUY8meGBW4uTPJig_Doq9Nr0qS6SjCQRg0goM6ovtdrfS2Ofch2RQ3sT7fetbQP3NHf65mm5Tsw=="
ANON_PK_3="YNIKDOqL7Xa30tjn3IdkUN7E-33rW0D9zR3-uZpuU7M="
ANON_ENC_3="c_FygTv2K-Vdx6VS-b9retJCgnci19w2C1_ZpRCE2CE="
ANON_DEC_3="EHr6smaDNCEgCmz2DfkNtnXcDow0QS6Dg9ZXhd_EKH0="

FILE_MNEMONIC="mnemonic-temp.keys"
FILE_ANON_KEYS_1="anon-keys-temp_1.keys"
FILE_ANON_KEYS_2="anon-keys-temp_2.keys"
FILE_ANON_KEYS_3="anon-keys-temp_3.keys"

# FRA: fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" > $FILE_MNEMONIC

echo "
{
  \"axfr_secret_key\": \"J7PqRhmBOE_gadFs4rB4lcKuz_YoWa5VSlALyKuZdQjNBryPSYZhRczonGNY3-mp86LWW8TJ6clirfk4gk03Tw==\",
  \"axfr_public_key\": \"zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08=\",
  \"enc_key\": \"Gu558brzFchoqQR9oi8QP54KZKSQ18Djzt82C4YUyFg=\",
  \"dec_key\": \"4GNC0J_qOXV2kww5BC5bOCyrTEfCodX5BoFaj06uN1s=\"
}" > $FILE_ANON_KEYS_1

echo "
{
  \"axfr_secret_key\": \"MwdsbYhTp4Io062nV7E2HkJfsnaTCZpkdjr6aijv2Aem3KjuGWqf4TLB_-20b305Ja3Pop8NS8tgMNUOVXUL5Q==\",
  \"axfr_public_key\": \"ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U=\",
  \"enc_key\": \"SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI=\",
  \"dec_key\": \"AEq1ZUFk_fB__YaNjQ3D2taGOnMZAx4adpB6RbnPj24=\"
}" > $FILE_ANON_KEYS_2

echo "
{
  \"axfr_secret_key\": \"GlEc6A-SDpkLUY8meGBW4uTPJig_Doq9Nr0qS6SjCQRg0goM6ovtdrfS2Ofch2RQ3sT7fetbQP3NHf65mm5Tsw==\",
  \"axfr_public_key\": \"YNIKDOqL7Xa30tjn3IdkUN7E-33rW0D9zR3-uZpuU7M=\",
  \"enc_key\": \"c_FygTv2K-Vdx6VS-b9retJCgnci19w2C1_ZpRCE2CE=\",
  \"dec_key\": \"EHr6smaDNCEgCmz2DfkNtnXcDow0QS6Dg9ZXhd_EKH0=\"
}" > $FILE_ANON_KEYS_3

rm owned_randomizers
rm sent_randomizers
echo "\n ***** Setup accounts successfully! ***** "

set -e

echo "\n\n Transfer FRA to test account..."
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 100000000 --asset FRA -T $FRA_ACCOUNT 2> /dev/null
echo "waiting blockchain 5s..."
sleep 5
# txo-sid = 3

echo "\n\n Transfer FRA to pay fee..."
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 100000000 --asset FRA -T $FRA_ACCOUNT 2> /dev/null
echo "waiting blockchain 5s..."
sleep 5
# txo-sid = 6

target/release/fn setup -O $FILE_MNEMONIC -S http://0.0.0.0

echo "Changed to test account. BAR Balance:"
target/release/fn wallet --show

echo "\n\n FRA Bar To Abar ..."
echo "==============================================================================="
target/release/fn convert-bar-to-abar --anon-keys $FILE_ANON_KEYS_1 --txo-sid 3 2> /dev/null
echo "waiting blockchain 20s..."
sleep 20
# txo-sid = 9

echo "\n\n Create Asset 1 ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --create --memo "asset1" --transferable 2> /dev/null
echo "waiting blockchain 5s..."
sleep 5

echo "\n\n Create Asset 2 ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --create --memo "asset2" --transferable 2> /dev/null
echo "waiting blockchain 5s..."
sleep 5

echo "\n\n Bilding assets ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --show --addr $FRA_ACCOUNT > tmp_file
ASSET1=$(awk 'FNR==1' tmp_file)
ASSET2=$(awk 'FNR==2' tmp_file)
echo $ASSET1
echo $ASSET2
rm tmp_file

echo "\n\n Issue Asset 2 ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --issue --code $ASSET1 --amount 100000000 2> /dev/null
echo "waiting blockchain 5s..."
sleep 5
# txo-sid = 14(asset1) & 16

echo "\n\n\n Issue Asset 2 ..."
echo "------------------------------------------------------------------------------"
target/release/fn asset --issue --code $ASSET2 --amount 100000000 2> /dev/null
echo "waiting blockchain 5s..."
sleep 5
# txo-sid = 17(asset2) & 19

echo "\n ***** Issue Asset & FRA successfully! ***** "

echo "\n\n Asset 1 Bar To Abar ..."
echo "==============================================================================="
target/release/fn convert-bar-to-abar --anon-keys $FILE_ANON_KEYS_1 --txo-sid 14 2> /dev/null
echo "waiting blockchain 30s..."
sleep 30

echo "\n\n Asset 2 Bar To Abar ..."
echo "==============================================================================="
target/release/fn convert-bar-to-abar --anon-keys $FILE_ANON_KEYS_1 --txo-sid 17 2> /dev/null
echo "waiting blockchain 30s..."
sleep 30

echo "\n\n Anon transfer Asset 1 ..."
echo "==============================================================================="
RANDOMIZER=$(awk 'FNR==3' owned_randomizers)
FRA_RANDMOIZER=$(awk 'FNR==2' owned_randomizers)
target/release/fn anon-transfer --amount 50000000 --anon-keys $FILE_ANON_KEYS_1 --randomizer $RANDOMIZER --fra-randomizer $FRA_RANDMOIZER --to-axfr-public-key $ANON_PK_2 --to-enc-key $ANON_ENC_2 2> /dev/null
echo "waiting blockchain 60s..."
sleep 60

echo "\n\n Anon transfer batch: Asset 1 & Asset 2 & FRA ..."
echo "input  => key1 * FRA + key1 * asset2 + key2 * asset1"
echo "output => key2 * FRA + key2 * asset2 + key3 * asset1"

ANON_KEY_1_FRA_RANDOMIZER=$(awk 'FNR==6' owned_randomizers)     # FRA
ANON_KEY_1_ASSET_1_RANDOMIZER=$(awk 'FNR==5' owned_randomizers) # ASSET 1
ANON_KEY_1_ASSET_2_RANDOMIZER=$(awk 'FNR==4' owned_randomizers) # ASSET 2
ANON_KEY_2_ASSET_1_RANDOMIZER=$(awk 'FNR==2' sent_randomizers)  # ASSET 1

BATCH_SK="batch_sk.keys"
BATCH_DEC="batch_dec.keys"
BATCH_R="batch_r.keys"
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

echo $ANON_KEY_1_FRA_RANDOMIZER > $BATCH_R
echo $ANON_KEY_1_ASSET_2_RANDOMIZER >> $BATCH_R
echo $ANON_KEY_2_ASSET_1_RANDOMIZER >> $BATCH_R

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

target/release/fn anon-transfer-batch --axfr-secretkey-file $BATCH_SK --decryption-key-file $BATCH_DEC --randomizer-file $BATCH_R --to-axfr-public-key-file $BATCH_PK --to-enc-key-file $BATCH_ENC --amount-file $BATCH_AMOUNT --asset-file $BATCH_ASSET 2> /dev/null
echo "waiting blockchain 60s..."
sleep 60

echo "checking..."
target/release/fn owned-abars --axfr-public-key $ANON_PK_2 -r $(awk 'FNR==3' sent_randomizers)
target/release/fn owned-abars --axfr-public-key $ANON_PK_2 -r $(awk 'FNR==4' sent_randomizers)
target/release/fn owned-abars --axfr-public-key $ANON_PK_3 -r $(awk 'FNR==5' sent_randomizers)

rm $BATCH_SK $BATCH_DEC $BATCH_R $BATCH_PK $BATCH_ENC $BATCH_AMOUNT $BATCH_ASSET
echo "\n ***** Test all successfully! ***** "
