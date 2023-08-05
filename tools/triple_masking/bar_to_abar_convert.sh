
set -e

FRA_ACCOUNT="fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5"
FRA_ACCOUNT_MNEMONIC_FILE="mnemonic-temp.keys"

ANON_1_KEYS_FILE="anon-keys-temp.keys"

echo "\n\n\n Simple transfer 1"
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 210000000 --asset FRA -T $FRA_ACCOUNT
sleep 5

echo "\n\n\n Simple transfer 2"
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 210000000 --asset FRA -T $FRA_ACCOUNT
sleep 5

echo "\n\n\n Simple transfer 3"
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 210000000 --asset FRA -T $FRA_ACCOUNT
sleep 5

# setup the new wallet
set +e
rm $FRA_ACCOUNT_MNEMONIC_FILE $ANON_1_KEYS_FILE
echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" >> $FRA_ACCOUNT_MNEMONIC_FILE
echo "{
  \"xfr_secret_key\": \"72vvPwIQroj9tCpX8UE4YMHT8B1xzvF3KIAvGeeS2qk=\",
  \"xfr_address\": \"fra1grh9et5ld26mh8d7cjng5asr9uq25xpsd4gvycnwcy372m6nwfysdljx6n\"
}" >> $ANON_1_KEYS_FILE

set -e
echo "\n\n\n Bar To Abar Conversion"
echo "==============================================================================="
target/release/fn setup -O $FRA_ACCOUNT_MNEMONIC_FILE -S http://0.0.0.0
# convert bar to abar
sleep 10


ANON_1_ADDRESS=$(cat $ANON_1_KEYS_FILE | jq -r '.xfr_address')
target/release/fn convert-bar-to-abar --to-address $ANON_1_ADDRESS  --txo-sid 3

COMMITMENT_STRING=$(tail -n 1 owned_commitments)
echo $COMMITMENT_STRING
cat $ANON_1_KEYS_FILE | jq -r '.xfr_secret_key' > /tmp/tmp-sec-key
echo "Bar 2 Abar Conversion demo script executed successfully!"

sleep 10
target/release/fn owned-abars -s /tmp/tmp-sec-key  -c $COMMITMENT_STRING
