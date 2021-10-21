
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
rm mnemonic-temp anon-keys-temp
echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" >> mnemonic-temp
echo "
{
  \"axfr_secret_key\": \"HQ6lFbnibT0Lri+GJotVOQuitfmcdZVqDWxkmaEF7AR0UTKQ3jRMXwPqxy6EUSAhVTQvaPRy9iC/kLlmQFQZHw==\",
  \"axfr_public_key\": \"dFEykN40TF8D6scuhFEgIVU0L2j0cvYgv5C5ZkBUGR8=\",
  \"enc_key\": \"Cpg9izrzn385hbqkm5ugq3KPCoHbkMcH7vpUz79V6hY=\",
  \"dec_key\": \"mCGipK02JeXTGCibxbDc0tGUHyt3WuFJGUBsH8enaXw=\"
}" >> anon-keys-temp

set -e
echo "\n\n\n Bar To Abar Conversion"
echo "==============================================================================="
target/release/fn setup -O mnemonic-temp -S http://0.0.0.0
# convert bar to abar
sleep 1
target/release/fn convert-bar-to-abar --anon-keys ./anon-keys-temp  --txo-sid 3

echo "Bar 2 Abar Conversion demo script executed successfully!"
echo "To check generated Abar run \`target/release/fn owned-abars -p dFEykN40TF8D6scuhFEgIVU0L2j0cvYgv5C5ZkBUGR8= -r RANDOMIZER_STRING\`"
