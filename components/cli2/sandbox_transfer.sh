#set -x
rm ~/.findora/*

source "tests/common.sh"

{ echo; echo; } | $CLI2 setup
echo -e "$PASSWORD\n$PASSWORD\n" | $CLI2 key-gen alice
echo -e "$PASSWORD\nmemo_alice\n$PASSWORD\nY\nY\n" | $CLI2 simple-define-asset alice AliceCoin
echo -e "$PASSWORD\n$PASSWORD\nY\nY\n" | $CLI2 simple-issue-asset AliceCoin 10000
echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key "arturo"
echo -e "$PASSWORD\n$PASSWORD" | $CLI2 key-gen bob

$CLI2 initialize-transaction "tx1"
$CLI2 transfer-assets --builder=tx1
$CLI2 build-transaction
$CLI2 submit "tx1"
$CLI2 list-txos --unspent=true
