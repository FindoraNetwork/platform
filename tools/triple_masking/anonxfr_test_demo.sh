set -e
./tools/triple_masking/bar_to_abar_convert.sh

randomiser1=$(tail -n 1 randomizers)
echo "\n\n Owned Abars after Bar to Abar conversion"
sleep 20 #Do not remove/decrease
target/release/fn owned-abars -p zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08= -r $randomiser1

set +e
rm anon-keys-temp2.keys
echo "
{
  \"axfr_secret_key\": \"MwdsbYhTp4Io062nV7E2HkJfsnaTCZpkdjr6aijv2Aem3KjuGWqf4TLB_-20b305Ja3Pop8NS8tgMNUOVXUL5Q==\",
  \"axfr_public_key\": \"ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U=\",
  \"enc_key\": \"SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI=\",
  \"dec_key\": \"AEq1ZUFk_fB__YaNjQ3D2taGOnMZAx4adpB6RbnPj24=\"
}" >> anon-keys-temp2.keys
sleep 5
set -e

echo "\n\n\n Anonymous Transfer from Sender1 to Receiver1"
echo "------------------------------------------------------------------------------"
target/release/fn anon-transfer --amount 209990000 --anon-keys ./anon-keys-temp.keys --to-axfr-public-key ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U= --to-enc-key SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI= --randomizer $randomiser1

randomiser2=$(tail -n 1 randomizers)
echo "\n\n Owned Abars for Receiver1 after Anon Transfer 1"
sleep 20
echo $randomiser2 > /dev/null
target/release/fn owned-abars -p ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U= -r $randomiser2

echo "\n\n\n Anonymous Transfer from Receiver1 (Sender2) to Receiver2"
echo "------------------------------------------------------------------------------"
target/release/fn anon-transfer --amount 209990000 --anon-keys ./anon-keys-temp2.keys --to-axfr-public-key BdECoTzLNQHlKq1oGMI2kdh27yp_I2CZen0FGYLFkM0= --to-enc-key Ox5L-mGxzOFfd4fef7WZGJMdO-EKBVnnJypZiEl_9FQ= --randomizer $randomiser2

randomiser3=$(tail -n 1 randomizers)
echo "\n\n Owned Abars for Receiver2 after Anon Transfer 2"
sleep 20
echo $randomiser3 > /dev/null
target/release/fn owned-abars -p BdECoTzLNQHlKq1oGMI2kdh27yp_I2CZen0FGYLFkM0= -r $randomiser3

sleep 2
echo "\n\n\n Fetch merkle proof for Anon Transfer 2"
echo "------------------------------------------------------------------------------"
target/release/fn anon-fetch-merkle-proof -a 2

echo "\n\n Anonymous Transfer demo script executed successfully!"
