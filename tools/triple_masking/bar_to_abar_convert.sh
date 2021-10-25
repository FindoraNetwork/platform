
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
rm mnemonic-temp anon-keys-temp anon-keys-temp2
echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" >> mnemonic-temp
echo "
{
  \"axfr_secret_key\": \"keQ0N8bNYef_3GhCrxgLwZKdiRl6QoTwYj6PLsW3DQR68cSPG2d5JH5sQe6EQai_tCDSFlIWE_EE_RCtiLUgbg==\",
  \"axfr_public_key\": \"evHEjxtneSR-bEHuhEGov7Qg0hZSFhPxBP0QrYi1IG4=\",
  \"enc_key\": \"LcZyndaw20-7FxmkNGorHpEbTYfWSaIFsiocDScD7nk=\",
  \"dec_key\": \"gN6-nw3NeHM3QBPnevuWmzsfP64X3u6j6ybPCQqHSXA=\"
}" >> anon-keys-temp

echo "
{
  \"axfr_secret_key\": \"EdQbauUfT4ig3Km8SH2MUfEwYq9Re0670KrsL-dt0wAByb3lQ2hsG31quzP_6cqbEshrjkJg_WhIyzPEUCaQzQ==\",
  \"axfr_public_key\": \"Acm95UNobBt9arsz_-nKmxLIa45CYP1oSMszxFAmkM0=\",
  \"enc_key\": \"0MTSVCEvwoVfW3NirjENvAuXtjTyBHYWE283c_i_C1Y=\",
  \"dec_key\": \"IDZNwpZohkun5MKGRF360YyyBFUbzkGq49Zn6oIg9VI=\"
}" >> anon-keys-temp2

set -e
echo "\n\n\n Bar To Abar Conversion"
echo "==============================================================================="
target/release/fn setup -O mnemonic-temp -S http://0.0.0.0
# convert bar to abar
sleep 1
target/release/fn convert-bar-to-abar --anon-keys ./anon-keys-temp  --txo-sid 3

echo "Bar 2 Abar Conversion demo script executed successfully!"
echo "To check generated Abar run \`target/release/fn owned-abars -p evHEjxtneSR-bEHuhEGov7Qg0hZSFhPxBP0QrYi1IG4= -r RANDOMIZER_STRING\`"
