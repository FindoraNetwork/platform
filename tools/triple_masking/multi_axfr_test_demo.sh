# If breaking because of blockchain, increase the sleep time
echo "Ccv2h8u1g__HJBrsA8npcs4CiDQ_UHI-JGZCjXbu9Un8HU3qSTf3PdLEFvs1XwauSltgruFv-IRVFpaQkeIIAgRoRPXncS1VHYzRpQlghzgCcQKJnic90DFDiYxSPVjg
Ccv2h8u1g__HJBrsA8npcs4CiDQ_UHI-JGZCjXbu9Un8HU3qSTf3PdLEFvs1XwauSltgruFv-IRVFpaQkeIIAgRoRPXncS1VHYzRpQlghzgCcQKJnic90DFDiYxSPVjg" > axfr_secretkey_file
echo "BGhE9edxLVUdjNGlCWCHOAJxAomeJz3QMUOJjFI9WOA=
JcomzsoXVf3Nz1lTL_dTz7ZifKsZJZtD5Aik1WYEa_I=
6dJt8oDrtXt3z-7__dOcDn7Q9lM8jd2RST0FJIfGspc=" > to_axfr_public_key_file
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
"$BIN"/fn anon-transfer-batch -n amount_file -a asset_file -s axfr_secretkey_file --to-axfr-public-key-file to_axfr_public_key_file -c commitment_file

sleep 5
echo -e "\n\n\n Fetch merkle proof for Batch Anon Transfer"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-fetch-merkle-proof -a 3
rm axfr_secretkey_file commitment_file to_axfr_public_key_file amount_file asset_file
echo -e "\n\n Batch Anonymous Transfer demo script executed successfully!"
