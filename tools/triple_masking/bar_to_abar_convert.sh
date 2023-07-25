
set -e

echo "\n\n\n Simple transfer 1"
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
sleep 5

echo "\n\n\n Simple transfer 2"
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
sleep 5

echo "\n\n\n Simple transfer 3"
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
sleep 5

# setup the new wallet
set +e
rm mnemonic-temp.keys anon-keys-temp.keys
echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" >> mnemonic-temp.keys
echo "{
  \"xfr_secret_key\": \"72vvPwIQroj9tCpX8UE4YMHT8B1xzvF3KIAvGeeS2qk=\",
  \"xfr_address\": \"fra1grh9et5ld26mh8d7cjng5asr9uq25xpsd4gvycnwcy372m6nwfysdljx6n\"
}" >> anon-keys-temp.keys

set -e
echo "\n\n\n Bar To Abar Conversion"
echo "==============================================================================="
target/release/fn setup -O mnemonic-temp.keys -S http://0.0.0.0
# convert bar to abar
sleep 10
ADDRESS=$(cat anon-keys-temp.keys | jq -r '.xfr_address')
echo $ADDRESS
target/release/fn convert-bar-to-abar --to-address $ADDRESS  --txo-sid 3

COMMITMENT_STRING=$(tail -n 1 owned_commitments)
echo $COMMITMENT_STRING
cat anon-keys-temp.keys | jq -r '.xfr_secret_key' > /tmp/tmp-sec-key
echo "Bar 2 Abar Conversion demo script executed successfully!"

sleep 5
target/release/fn owned-abars -s /tmp/tmp-sec-key  -c $COMMITMENT_STRING
