#!/usr/bin/env bash
source tools/regression/triple_masking/scripts/env.sh

export KEYPAIR=$($BIN/fn genkey)
echo "Generating eth prefix address..."
echo "$KEYPAIR"
export FRA_ADDRESS=$(echo "$KEYPAIR" |awk 'NR == 2' |awk '{print $3}')
export BAR_SEC_KEY=$(echo "$KEYPAIR" |awk 'NR == 6' |awk '{gsub(/"/,""); print $2}')

export ED_KEYPAIR=$($BIN/fn genkey --fra-address)
echo "Generating fra prefix address..."
echo "$ED_KEYPAIR"
export ED_ADDRESS=$(echo "$ED_KEYPAIR" |awk 'NR == 2' |awk '{print $3}')
export ED_SEC_KEY=$(echo "$ED_KEYPAIR" |awk 'NR == 6' |awk '{gsub(/"/,""); print $2}')
echo -n "${ED_SEC_KEY}" > "$FILE_FRA_KEY"
$BIN/fn transfer --amount 20000 --asset FRA -T $ED_ADDRESS
sleep $BLOCK_INTERVAL

echo -e "\n ***** 4 times Simple transfer from faucet to eth BAR address *****"
$BIN/fn transfer --amount 210000000 --asset FRA -T $FRA_ADDRESS
sleep $BLOCK_INTERVAL

$BIN/fn transfer --amount 210000000 --asset FRA -T $FRA_ADDRESS
sleep $BLOCK_INTERVAL
sleep $BLOCK_INTERVAL

$BIN/fn transfer --amount 210000000 --asset FRA -T $FRA_ADDRESS
sleep $BLOCK_INTERVAL
sleep $BLOCK_INTERVAL

$BIN/fn transfer --amount 210000000 --asset FRA -T $FRA_ADDRESS --confidential-amount
sleep $BLOCK_INTERVAL
sleep $BLOCK_INTERVAL

echo "$KEYPAIR" |awk 'NR == 3'|cut -d' ' -f 2- > $FILE_MNEMONIC

$BIN/fn setup -O $FILE_MNEMONIC -S $ENDPOINT

sleep $BLOCK_INTERVAL
echo "New BAR wallet with Balance:"
$BIN/fn wallet --show

# These wallets would be used to move Anonymous funds around
export ANON_KEYPAIR_1=$($BIN/fn gen-anon-keys)
#echo "$ANON_KEYPAIR_1" |awk 'NR==2 {gsub(" ","");};1' |awk 'NR==1 {gsub("Keys :","");};1'  > "$FILE_ANON_KEYS"
export ANON_SK_1=`echo "$ANON_KEYPAIR_1" |awk 'NR == 3' |awk '{gsub(/,$/,""); gsub(/"/,""); print $2}'`
export ANON_PK_1=`echo "$ANON_KEYPAIR_1" |awk 'NR == 4' |awk '{gsub(/"/,""); print $2}'`
echo "
{
  \"spend_key\": \"$ANON_SK_1\",
  \"pub_key\": \"$ANON_PK_1\"
}" > "$FILE_ANON_KEYS"

export ANON_KEYPAIR_2=$($BIN/fn gen-anon-keys)
#echo "$ANON_KEYPAIR_2" |awk 'NR > 1' |awk 'NR==1 {$1=$1};1' > "$FILE_ANON_KEYS_2"
export ANON_SK_2=`echo "$ANON_KEYPAIR_2" |awk 'NR == 3' |awk '{gsub(/,$/,""); gsub(/"/,""); print $2}'`
export ANON_PK_2=`echo "$ANON_KEYPAIR_2" |awk 'NR == 4' |awk '{gsub(/"/,""); print $2}'`
echo "
{
  \"spend_key\": \"$ANON_SK_2\",
  \"pub_key\": \"$ANON_PK_2\"
}" > "$FILE_ANON_KEYS_2"

export ANON_KEYPAIR_3=$($BIN/fn gen-anon-keys)
#echo "$ANON_KEYPAIR_3" |awk 'NR > 1' |awk 'NR==1 {$1=$1};1' > "$FILE_ANON_KEYS_3"
export ANON_SK_3=`echo "$ANON_KEYPAIR_3" |awk 'NR == 3' |awk '{gsub(/,$/,""); gsub(/"/,""); print $2}'`
export ANON_PK_3=`echo "$ANON_KEYPAIR_3" |awk 'NR == 4' |awk '{gsub(/"/,""); print $2}'`
echo "
{
  \"spend_key\": \"$ANON_SK_3\",
  \"pub_key\": \"$ANON_PK_3\"
}" > "$FILE_ANON_KEYS_3"
sleep 1