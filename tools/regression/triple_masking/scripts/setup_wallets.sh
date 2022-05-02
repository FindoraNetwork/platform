#rm mnemonic-temp.keys anon-keys-temp.keys

# These wallets would be used to move Anonymous funds around

echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" > mnemonic-temp.keys

echo "
{
  \"axfr_secret_key\": \"J7PqRhmBOE_gadFs4rB4lcKuz_YoWa5VSlALyKuZdQjNBryPSYZhRczonGNY3-mp86LWW8TJ6clirfk4gk03Tw==\",
  \"axfr_public_key\": \"zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08=\",
  \"enc_key\": \"Gu558brzFchoqQR9oi8QP54KZKSQ18Djzt82C4YUyFg=\",
  \"dec_key\": \"4GNC0J_qOXV2kww5BC5bOCyrTEfCodX5BoFaj06uN1s=\"
}" > "$FILE_ANON_KEYS"

fn setup -O mnemonic-temp.keys -S http://0.0.0.0

echo "New BAR wallet with Balance:"
target/release/fn wallet --show


rm anon-keys-temp2.keys
echo "
{
  \"axfr_secret_key\": \"MwdsbYhTp4Io062nV7E2HkJfsnaTCZpkdjr6aijv2Aem3KjuGWqf4TLB_-20b305Ja3Pop8NS8tgMNUOVXUL5Q==\",
  \"axfr_public_key\": \"ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U=\",
  \"enc_key\": \"SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI=\",
  \"dec_key\": \"AEq1ZUFk_fB__YaNjQ3D2taGOnMZAx4adpB6RbnPj24=\"
}" > "$FILE_ANON_KEYS_2"

echo "
 {
  \"axfr_secret_key\": \"vw41h9OciN5cDh8QSPFEodSA5AnuHxxcV5SKJ_q-JQMGUX_qogo-lXUvhnXo1gYuP8YsoJJro7kNloXx8wfDSA==\",
  \"axfr_public_key\": \"BlF_6qIKPpV1L4Z16NYGLj_GLKCSa6O5DZaF8fMHw0g=\",
  \"enc_key\": \"NBY5yIhdriJVq-7BS59J2IxBgLhewr8TEE6suNc1elA=\",
  \"dec_key\": \"OKh4F5o_Mw0eKi0xU8lQhi_lXMVd-hem7N12lvSy_WU=\"
}
"  > "$FILE_ANON_KEYS_3"
sleep 1