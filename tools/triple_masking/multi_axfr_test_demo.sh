source ./tools/devnet/env.sh || exit 1

# If breaking because of blockchain, increase the sleep time
echo "HZHnGAptuEogWbuOH8PLpMuWwT9f9aSC18r49Tt8nSYA
Jy2QGLGlfYJTwWwMbQsFtXXQ5nBWoAwR0hISPTAe1zEA
HZHnGAptuEogWbuOH8PLpMuWwT9f9aSC18r49Tt8nSYA" > to_axfr_public_key_file
echo "100000000
100000000
119980000" > amount_file
echo "" > asset_file
echo "" >> asset_file
echo "" >> asset_file

FILE_ANON_KEYS="anon-keys-temp.keys"

echo -e "\n ***** Accounts setup successful! ***** "
set -e
./tools/triple_masking/bar_to_abar_convert.sh

tail -n 2 owned_commitments > commitment_file
commitment1=$(awk 'FNR==1' commitment_file)
echo -e "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 20
"$BIN"/fn owned-abars --commitments "$commitment1" --anon-keys $FILE_ANON_KEYS

commitment2=$(awk 'FNR==2' commitment_file)
echo "\n\n Owned Abars after Bar to Abar conversion 2"
sleep 20
"$BIN"/fn owned-abars --commitments "$commitment2" --anon-keys $FILE_ANON_KEYS

echo -e "\n\n\n Batch Anonymous Transfer from Senders to Receivers"
echo "------------------------------------------------------------------------------"
sleep 2
"$BIN"/fn anon-transfer-batch -n amount_file -a asset_file -s $FILE_ANON_KEYS --to-axfr-public-key-file to_axfr_public_key_file -c commitment_file

sleep 5
echo -e "\n\n\n Fetch merkle proof for Batch Anon Transfer"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-fetch-merkle-proof -a 3
rm commitment_file to_axfr_public_key_file amount_file asset_file
echo -e "\n\n Batch Anonymous Transfer demo script executed successfully!"
