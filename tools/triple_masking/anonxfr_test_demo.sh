# If breaking because of blockchain, increase the sleep time
FILE_ANON_KEYS="anon-keys-temp.keys"
FILE_ANON_KEYS_2="anon-keys-temp2.keys"

echo "
{
  \"axfr_secret_key\": \"MwdsbYhTp4Io062nV7E2HkJfsnaTCZpkdjr6aijv2Aem3KjuGWqf4TLB_-20b305Ja3Pop8NS8tgMNUOVXUL5Q==\",
  \"axfr_public_key\": \"ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U=\",
  \"enc_key\": \"SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI=\",
  \"dec_key\": \"AEq1ZUFk_fB__YaNjQ3D2taGOnMZAx4adpB6RbnPj24=\"
}" > $FILE_ANON_KEYS_2

set -e
./tools/triple_masking/bar_to_abar_convert.sh

commitment1=$(tail -n 1 owned_commitments)
echo "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 20
target/release/fn owned-abars -c $commitment1

echo "\n\n\n Anonymous Transfer from Sender1 to Receiver1"
echo "------------------------------------------------------------------------------"
target/release/fn anon-transfer --amount 189990000 --anon-keys ./$FILE_ANON_KEYS --to-axfr-public-key ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U= --to-enc-key SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI= --commitment $commitment1

commitment2=$(tail -n 1 sent_commitments)
echo "\n\n Owned Abars for Receiver1 after Anon Transfer 1"
sleep 30
target/release/fn owned-abars -c $commitment2

echo "\n\n\n Anonymous Transfer from Receiver1 (Sender2) to Receiver2"
echo "------------------------------------------------------------------------------"
target/release/fn anon-transfer --amount 169990000 --anon-keys ./$FILE_ANON_KEYS_2 --to-axfr-public-key BdECoTzLNQHlKq1oGMI2kdh27yp_I2CZen0FGYLFkM0= --to-enc-key Ox5L-mGxzOFfd4fef7WZGJMdO-EKBVnnJypZiEl_9FQ= --commitment $commitment2

commitment3=$(tail -n 1 sent_commitments)
echo "\n\n Owned Abars for Receiver2 after Anon Transfer 2"
sleep 30
target/release/fn owned-abars -c $commitment3

sleep 2
echo "\n\n\n Fetch merkle proof for Anon Transfer 2"
echo "------------------------------------------------------------------------------"
target/release/fn anon-fetch-merkle-proof -a 2
echo "\n\n Anonymous Transfer demo script executed successfully!"
