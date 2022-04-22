# If breaking because of blockchain, increase the sleep time
echo "J7PqRhmBOE_gadFs4rB4lcKuz_YoWa5VSlALyKuZdQjNBryPSYZhRczonGNY3-mp86LWW8TJ6clirfk4gk03Tw==
J7PqRhmBOE_gadFs4rB4lcKuz_YoWa5VSlALyKuZdQjNBryPSYZhRczonGNY3-mp86LWW8TJ6clirfk4gk03Tw==" > axfr_secretkey_file
echo "4GNC0J_qOXV2kww5BC5bOCyrTEfCodX5BoFaj06uN1s=
4GNC0J_qOXV2kww5BC5bOCyrTEfCodX5BoFaj06uN1s=" > decryption_key_file
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

echo "\n ***** Accounts setup successful! ***** "
set -e
./tools/triple_masking/bar_to_abar_convert.sh

tail -n 2 owned_commitments > commitment_file
commitment1=$(awk 'FNR==1' commitment_file)
echo "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 20
target/release/fn owned-abars --commitment $commitment1

commitment2=$(awk 'FNR==2' commitment_file)
echo "\n\n Owned Abars after Bar to Abar conversion 2"
sleep 20
target/release/fn owned-abars --commitment $commitment2

echo "\n\n\n Batch Anonymous Transfer from Senders to Receivers"
echo "------------------------------------------------------------------------------"
sleep 5
target/release/fn anon-transfer-batch -n amount_file -a asset_file -s axfr_secretkey_file -d decryption_key_file --to-axfr-public-key-file to_axfr_public_key_file --to-enc-key-file to_enc_key_file -c commitment_file

tail -n 3 sent_commitments > commitment_file2
commitment4=$(awk 'FNR==1' commitment_file2)
echo "\n\n Owned Abars for Receiver1 after Batch Anon Transfer"
sleep 30
target/release/fn owned-abars --commitment $commitment4

commitment5=$(awk 'FNR==3' commitment_file2)
echo "\n\n Owned Abars for Receiver3 after Batch Anon Transfer"
sleep 30
target/release/fn owned-abars --commitment $commitment5

sleep 2
echo "\n\n\n Fetch merkle proof for Batch Anon Transfer"
echo "------------------------------------------------------------------------------"
target/release/fn anon-fetch-merkle-proof -a 3
rm axfr_secretkey_file decryption_key_file commitment_file commitment_file2 to_axfr_public_key_file to_enc_key_file amount_file asset_file
echo "\n\n Batch Anonymous Transfer demo script executed successfully!"
