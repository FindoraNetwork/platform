set -e
./tools/triple_masking/bar_to_abar_convert.sh
echo "\n\n Owned UTXOs of the converter"
target/release/fn owned-utxos

set +e
rm anon-keys-temp2.keys
echo "
{
  \"axfr_secret_key\": \"EdQbauUfT4ig3Km8SH2MUfEwYq9Re0670KrsL-dt0wAByb3lQ2hsG31quzP_6cqbEshrjkJg_WhIyzPEUCaQzQ==\",
  \"axfr_public_key\": \"Acm95UNobBt9arsz_-nKmxLIa45CYP1oSMszxFAmkM0=\",
  \"enc_key\": \"0MTSVCEvwoVfW3NirjENvAuXtjTyBHYWE283c_i_C1Y=\",
  \"dec_key\": \"IDZNwpZohkun5MKGRF360YyyBFUbzkGq49Zn6oIg9VI=\"
}" >> anon-keys-temp2.keys
sleep 5
set -e

echo "\n\n\n Bar To Abar Conversion 2"
sleep 10
target/release/fn convert-bar-to-abar --anon-keys ./anon-keys-temp2.keys  --txo-sid 9
#echo "Owned UTXOs of the converter"
#target/release/fn owned-utxos
#echo "\n\n\n Bar To Abar Conversion 3"
#sleep 10
#target/release/fn convert-bar-to-abar --anon-keys ./anon-keys-temp2.keys  --txo-sid 12

tail -n 2 randomizers > randomizer_file
randomiser=$(awk 'FNR>=1 && FNR<=1' randomizer_file)
echo "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 20
target/release/fn owned-abars -p evHEjxtneSR-bEHuhEGov7Qg0hZSFhPxBP0QrYi1IG4= -r $randomiser
randomiser=$(awk 'FNR>=2 && FNR<=2' randomizer_file)
echo "\n\n Owned Abars after Bar to Abar conversion 2"
sleep 20
target/release/fn owned-abars -p Acm95UNobBt9arsz_-nKmxLIa45CYP1oSMszxFAmkM0= -r $randomiser
#randomiser=$(awk 'FNR>=3 && FNR<=3' randomizer_file)
#echo "\n\n Owned Abars after Bar to Abar conversion 3"
#sleep 20
#target/release/fn owned-abars -p evHEjxtneSR-bEHuhEGov7Qg0hZSFhPxBP0QrYi1IG4= -r $randomiser

sleep 5
echo "keQ0N8bNYef_3GhCrxgLwZKdiRl6QoTwYj6PLsW3DQR68cSPG2d5JH5sQe6EQai_tCDSFlIWE_EE_RCtiLUgbg==
EdQbauUfT4ig3Km8SH2MUfEwYq9Re0670KrsL-dt0wAByb3lQ2hsG31quzP_6cqbEshrjkJg_WhIyzPEUCaQzQ==" > axfr_secretkey_file
echo "gN6-nw3NeHM3QBPnevuWmzsfP64X3u6j6ybPCQqHSXA=
IDZNwpZohkun5MKGRF360YyyBFUbzkGq49Zn6oIg9VI=" > decryption_key_file
echo "1ASVNYLgW2SzBEmAnHfaiJwVBd0M72aRhcReJluZo9M=
ovO3hR4HeZ_arSGhiLAFUeN9jPsxjajcNiRsV8dNjIg=" > to_axfr_public_key_file
echo "S9xkhvejemaNuzzZB0l5NnTf9l2Xt4nurnlKiRsPBB8=
e9d0qy8z2D3npp-pfXNcnolZBYXik6m0GhnDjK5Hvnk=" > to_enc_key_file
echo "210000000
210000000" > amount_file

echo "\n\n\n Batch Anonymous Transfer from Senders to Receivers"
echo "------------------------------------------------------------------------------"
sleep 5
target/release/fn anon-transfer-batch -n amount_file -s axfr_secretkey_file -d decryption_key_file --to-axfr-public-key-file to_axfr_public_key_file --to-enc-key-file to_enc_key_file -r randomizer_file

tail -n 2 randomizers > randomizer_file
randomiser=$(awk 'FNR>=1 && FNR<=1' randomizers)
echo "\n\n Owned Abars for Receiver1 after Batch Anon Transfer"
sleep 10
target/release/fn owned-abars -p 1ASVNYLgW2SzBEmAnHfaiJwVBd0M72aRhcReJluZo9M= -r $randomiser

randomiser=$(awk 'FNR>=2 && FNR<=2' randomizers)
echo "\n\n Owned Abars for Receiver2 after Batch Anon Transfer"
sleep 10
target/release/fn owned-abars -p ovO3hR4HeZ_arSGhiLAFUeN9jPsxjajcNiRsV8dNjIg= -r $randomiser

sleep 2
echo "\n\n\n Fetch merkle proof for Batch Anon Transfer"
echo "------------------------------------------------------------------------------"
target/release/fn anon-fetch-merkle-proof -a 3
rm axfr_secretkey_file decryption_key_file randomizer_file to_axfr_public_key_file to_enc_key_file amount_file
echo "\n\n Batch Anonymous Transfer demo script executed successfully!"