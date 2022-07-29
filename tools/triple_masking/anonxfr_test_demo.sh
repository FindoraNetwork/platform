# If breaking because of blockchain, increase the sleep time
FILE_ANON_KEYS="anon-keys-temp.keys"
FILE_ANON_KEYS_2="anon-keys-temp2.keys"

echo "
{
  \"spend_key\": \"h4MuWol8pWuNIMxPHwJ0ZAoF_n51QScj6AultG5IHU3yL-LR02XXw58uudwom_tahcy1e0oadfOw3oLxSs64A9yTOKFC1NqT6e-fWGEO-QpSZzf8otV7POguvdejoKhL\",
  \"view_key\": \"8i_i0dNl18OfLrncKJv7WoXMtXtKGnXzsN6C8UrOuAM=\",
  \"pub_key\": \"3JM4oULU2pPp759YYQ75ClJnN_yi1Xs86C6916OgqEs=\"
}" > $FILE_ANON_KEYS_2

set -e
./tools/triple_masking/bar_to_abar_convert.sh

commitment1=$(tail -n 1 owned_commitments)
echo "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 30
"$BIN"/fn owned-abars --commitments "$commitment1" --anon-keys ./$FILE_ANON_KEYS

echo "\n\n\n Anonymous Transfer from Sender1 to Receiver1"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-transfer --amount 189990000 --anon-keys ./$FILE_ANON_KEYS --to-axfr-public-key 3JM4oULU2pPp759YYQ75ClJnN_yi1Xs86C6916OgqEs= --commitment "$commitment1"

commitment2=$(tail -n 1 sent_commitments)
echo -e "\n\n Owned Abars for Receiver1 after Anon Transfer 1"
sleep 30
"$BIN"/fn owned-abars --commitments "$commitment2" --anon-keys ./$FILE_ANON_KEYS_2

echo "\n\n\n Anonymous Transfer from Receiver1 (Sender2) to Receiver2"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-transfer --amount 169990000 --anon-keys ./$FILE_ANON_KEYS_2 --to-axfr-public-key JcomzsoXVf3Nz1lTL_dTz7ZifKsZJZtD5Aik1WYEa_I= --commitment "$commitment2"


sleep 2
echo "\n\n\n Fetch merkle proof for Anon Transfer 2"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-fetch-merkle-proof -a 2
echo "\n\n Anonymous Transfer demo script executed successfully!"
