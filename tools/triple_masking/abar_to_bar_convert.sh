./tools/triple_masking/bar_to_abar_convert.sh
FILE_ANON_KEYS_LOCAL=$FILE_ANON_KEYS
set -e

sleep 10
echo "BAR Balance:"
target/release/fn wallet --show

target/release/fn owned-utxos
echo "\n\n\n Bar To Abar Conversion for fee ABAR"
sleep 10
TXO_SID=$(target/release/fn owned-utxos | head -4 | tail -1 | cut  -f1)
target/release/fn convert-bar-to-abar --anon-keys ./$FILE_ANON_KEYS_LOCAL  --txo-sid $TXO_SID 2> /dev/null

tail -n 2 owned_randomizers > randomizer_file
randomiser=$(awk 'FNR>=1 && FNR<=1' randomizer_file)
echo "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 20
target/release/fn owned-abars -p zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08= -r $randomiser
fee_randomiser=$(awk 'FNR>=2 && FNR<=2' randomizer_file)
echo "\n\n Owned Abars after Bar to Abar conversion 2"
sleep 20
target/release/fn owned-abars -p zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08= -r $fee_randomiser

echo "\n\n\n ABAR To BAR Conversion"
echo "------------------------------------------------------------------------------"
sleep 5
target/release/fn convert-abar-to-bar --anon-keys ./$FILE_ANON_KEYS_LOCAL -r $randomiser -F $fee_randomiser --to-wallet-address  fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5

sleep 10

echo "BAR Balance: after AbarToBar"
target/release/fn wallet --show