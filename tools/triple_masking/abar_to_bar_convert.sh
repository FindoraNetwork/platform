# If breaking because of blockchain, increase the sleep time
./tools/triple_masking/bar_to_abar_convert.sh
FILE_ANON_KEYS="anon-keys-temp.keys"
set -e

sleep 10
echo "BAR Balance:"
target/release/fn wallet --show

tail -n 2 owned_commitments > commitment_file
commitment=$(awk 'FNR==1' commitment_file)
echo "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 20
target/release/fn owned-abars -c $commitment

fee_commitment=$(awk 'FNR==2' commitment_file)
echo "\n\n Owned Abars after Bar to Abar conversion 2 (for fee ABAR)"
sleep 20
target/release/fn owned-abars -c $fee_commitment

echo "\n\n\n ABAR To BAR Conversion"
echo "------------------------------------------------------------------------------"
sleep 5
target/release/fn convert-abar-to-bar --anon-keys ./$FILE_ANON_KEYS -c $commitment -F $fee_commitment --to-wallet-address  fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5

sleep 10
echo "BAR Balance: after AbarToBar"
target/release/fn wallet --show
