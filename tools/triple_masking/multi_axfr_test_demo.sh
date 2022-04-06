set -e
./tools/triple_masking/bar_to_abar_convert.sh
echo "\n\n Owned UTXOs of the converter"
target/release/fn owned-utxos

set +e
rm anon-keys-temp2.keys
echo "
{
  \"axfr_secret_key\": \"MwdsbYhTp4Io062nV7E2HkJfsnaTCZpkdjr6aijv2Aem3KjuGWqf4TLB_-20b305Ja3Pop8NS8tgMNUOVXUL5Q==\",
  \"axfr_public_key\": \"ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U=\",
  \"enc_key\": \"SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI=\",
  \"dec_key\": \"AEq1ZUFk_fB__YaNjQ3D2taGOnMZAx4adpB6RbnPj24=\"
}" > anon-keys-temp2.keys
sleep 5
set -e

echo "\n\n\n Bar To Abar Conversion 2"
sleep 10

TXO_SID=$(target/release/fn owned-utxos | head -4 | tail -1 |  awk -F ' ' '{print $1}')
target/release/fn convert-bar-to-abar --anon-keys ./$FILE_ANON_KEYS  --txo-sid $TXO_SID 2> /dev/null

tail -n 2 owned_randomizers > randomizer_file
randomiser1=$(awk 'FNR>=1 && FNR<=1' randomizer_file)
echo "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 20
target/release/fn owned-abars -p zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08= -r $randomiser1
randomiser2=$(awk 'FNR>=2 && FNR<=2' randomizer_file)
echo "\n\n Owned Abars after Bar to Abar conversion 2"
sleep 20
target/release/fn owned-abars -p ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U= -r $randomiser2

sleep 5
echo "J7PqRhmBOE_gadFs4rB4lcKuz_YoWa5VSlALyKuZdQjNBryPSYZhRczonGNY3-mp86LWW8TJ6clirfk4gk03Tw==
MwdsbYhTp4Io062nV7E2HkJfsnaTCZpkdjr6aijv2Aem3KjuGWqf4TLB_-20b305Ja3Pop8NS8tgMNUOVXUL5Q==" > axfr_secretkey_file
echo "4GNC0J_qOXV2kww5BC5bOCyrTEfCodX5BoFaj06uN1s=
AEq1ZUFk_fB__YaNjQ3D2taGOnMZAx4adpB6RbnPj24=" > decryption_key_file
echo "BdECoTzLNQHlKq1oGMI2kdh27yp_I2CZen0FGYLFkM0=
EPGl5qbD_6mq7Zn8Ni1Z1LnR0WInFiHSnQ1P7qiaU4w=
6dJt8oDrtXt3z-7__dOcDn7Q9lM8jd2RST0FJIfGspc=" > to_axfr_public_key_file
echo "Ox5L-mGxzOFfd4fef7WZGJMdO-EKBVnnJypZiEl_9FQ=
ra4lQ6KhMhfLO1leXpI2Dj7qQcasbSOqNIVFAIRDHxw=
GrvIiB1yXLajRr5V5yZggPOmSelQ1Ga9zxWTzp0RnB8=" > to_enc_key_file
echo "100000000
100000000
119980000" > amount_file

echo "" > asset_file
echo "" >> asset_file
echo "" >> asset_file

echo "\n\n\n Batch Anonymous Transfer from Senders to Receivers"
echo "------------------------------------------------------------------------------"
sleep 5
target/release/fn anon-transfer-batch -n amount_file -a asset_file -s axfr_secretkey_file -d decryption_key_file --to-axfr-public-key-file to_axfr_public_key_file --to-enc-key-file to_enc_key_file -r randomizer_file

tail -n 3 sent_randomizers > randomizer_file2
randomiser4=$(awk 'FNR>=1 && FNR<=1' randomizer_file2)
echo "\n\n Owned Abars for Receiver1 after Batch Anon Transfer"
sleep 30
echo $randomiser4 > /dev/null
target/release/fn owned-abars -p BdECoTzLNQHlKq1oGMI2kdh27yp_I2CZen0FGYLFkM0= -r $randomiser4

randomiser5=$(awk 'FNR>=3 && FNR<=3' randomizer_file2)
echo "\n\n Owned Abars for Receiver3 after Batch Anon Transfer"
sleep 30
echo $randomiser5 > /dev/null
target/release/fn owned-abars -p 6dJt8oDrtXt3z-7__dOcDn7Q9lM8jd2RST0FJIfGspc= -r $randomiser5

sleep 2
echo "\n\n\n Fetch merkle proof for Batch Anon Transfer"
echo "------------------------------------------------------------------------------"
target/release/fn anon-fetch-merkle-proof -a 3
rm axfr_secretkey_file decryption_key_file randomizer_file randomizer_file2 to_axfr_public_key_file to_enc_key_file amount_file asset_file
echo "\n\n Batch Anonymous Transfer demo script executed successfully!"
