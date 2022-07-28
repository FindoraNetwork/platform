source ./tools/devnet/env.sh || exit 1

# If breaking because of blockchain, increase the sleep time
./tools/triple_masking/bar_to_abar_convert.sh
FILE_ANON_KEYS="anon-keys-temp.keys"
set -e

sleep 10
echo "BAR Balance:"
"$BIN"/fn wallet --show

tail -n 2 owned_commitments > commitment_file
commitment=$(awk 'FNR==1' commitment_file)
echo -e "\n\n Owned Abars after Bar to Abar conversion"
sleep 20
"$BIN"/fn owned-abars --commitments "$commitment" --anon-keys $FILE_ANON_KEYS

echo -e "\n\n\n ABAR To BAR Conversion"
echo "------------------------------------------------------------------------------"
sleep 5
"$BIN"/fn convert-abar-to-bar --anon-keys ./$FILE_ANON_KEYS --commitment "$commitment" --to-wallet-address  fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5

sleep 10
echo "BAR Balance: after AbarToBar"
"$BIN"/fn wallet --show
