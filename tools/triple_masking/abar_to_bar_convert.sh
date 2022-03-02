set -e
./tools/triple_masking/bar_to_abar_convert.sh


sleep 10
echo "BAR Balance:"
target/release/fn wallet --show


randomiser=$(tail -n 1 owned_randomizers)
echo "\n\n Owned Abars after Bar to Abar conversion"
sleep 20 #Do not remove/decrease
target/release/fn owned-abars -p zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08= -r $randomiser

target/release/fn convert-abar-to-bar --anon-keys ./anon-keys-temp.keys -r $randomiser --to-wallet-address  fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5

sleep 10

echo "BAR Balance: after AbarToBar"
target/release/fn wallet --show