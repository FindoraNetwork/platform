source ./tools/devnet/env.sh || exit 1

# If breaking because of blockchain, increase the sleep time
FILE_ANON_KEYS="anon-keys-temp.keys"
FILE_ANON_KEYS_2="anon-keys-temp2.keys"

echo "
{
  \"spend_key\": \"fGAGGWYkAqcQ1DK7CbQhSbKde8WOcNld1fJlIOaDJmcdkecYCm24SiBZu44fw8uky5bBP1_1pILXyvj1O3ydJgA=\",
  \"pub_key\": \"HZHnGAptuEogWbuOH8PLpMuWwT9f9aSC18r49Tt8nSYA\"
}" > $FILE_ANON_KEYS_2

set -e
./tools/triple_masking/bar_to_abar_convert.sh

commitment1=$(tail -n 1 owned_commitments)
echo "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 30
"$BIN"/fn owned-abars --commitments "$commitment1" --anon-keys ./$FILE_ANON_KEYS

echo "\n\n\n Anonymous Transfer from Sender1 to Receiver1"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-transfer --amount 189990000 --anon-keys ./$FILE_ANON_KEYS --to-axfr-public-key HZHnGAptuEogWbuOH8PLpMuWwT9f9aSC18r49Tt8nSYA --commitment "$commitment1"

commitment2=$(tail -n 1 sent_commitments)
echo -e "\n\n Owned Abars for Receiver1 after Anon Transfer 1"
sleep 30
"$BIN"/fn owned-abars --commitments "$commitment2" --anon-keys ./$FILE_ANON_KEYS_2

echo "\n\n\n Anonymous Transfer from Receiver1 (Sender2) to Receiver2"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-transfer --amount 169990000 --anon-keys ./$FILE_ANON_KEYS_2 --to-axfr-public-key Jy2QGLGlfYJTwWwMbQsFtXXQ5nBWoAwR0hISPTAe1zEA --commitment "$commitment2"


sleep 2
echo "\n\n\n Fetch merkle proof for Anon Transfer 2"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-fetch-merkle-proof -a 2
echo "\n\n Anonymous Transfer demo script executed successfully!"
