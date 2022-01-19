set -e
./tools/triple_masking/bar_to_abar_convert.sh

randomiser1=$(tail -n 1 randomizers)
echo "\n\n Owned Abars after Bar to Abar conversion"
sleep 20 #Do not remove/decrease
target/release/fn owned-abars -p evHEjxtneSR-bEHuhEGov7Qg0hZSFhPxBP0QrYi1IG4= -r $randomiser1

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

echo "\n\n\n Anonymous Transfer from Sender1 to Receiver1"
echo "------------------------------------------------------------------------------"
target/release/fn anon-transfer --amount 209990000 --anon-keys ./anon-keys-temp.keys --to-axfr-public-key Acm95UNobBt9arsz_-nKmxLIa45CYP1oSMszxFAmkM0= --to-enc-key 0MTSVCEvwoVfW3NirjENvAuXtjTyBHYWE283c_i_C1Y= --randomizer $randomiser1

randomiser2=$(tail -n 1 randomizers)
echo "\n\n Owned Abars for Receiver1 after Anon Transfer 1"
sleep 20
echo $randomiser2 > /dev/null
target/release/fn owned-abars -p Acm95UNobBt9arsz_-nKmxLIa45CYP1oSMszxFAmkM0= -r $randomiser2

echo "\n\n\n Anonymous Transfer from Receiver1 (Sender2) to Receiver2"
echo "------------------------------------------------------------------------------"
target/release/fn anon-transfer --amount 209990000 --anon-keys ./anon-keys-temp2.keys --to-axfr-public-key 1ASVNYLgW2SzBEmAnHfaiJwVBd0M72aRhcReJluZo9M= --to-enc-key S9xkhvejemaNuzzZB0l5NnTf9l2Xt4nurnlKiRsPBB8= --randomizer $randomiser2

randomiser3=$(tail -n 1 randomizers)
echo "\n\n Owned Abars for Receiver2 after Anon Transfer 2"
sleep 20
echo $randomiser3 > /dev/null
target/release/fn owned-abars -p 1ASVNYLgW2SzBEmAnHfaiJwVBd0M72aRhcReJluZo9M= -r $randomiser3

sleep 2
echo "\n\n\n Fetch merkle proof for Anon Transfer 2"
echo "------------------------------------------------------------------------------"
target/release/fn anon-fetch-merkle-proof -a 2

echo "\n\n Anonymous Transfer demo script executed successfully!"
