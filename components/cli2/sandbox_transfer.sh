set -x
rm ~/.findora/*
$CLI2 key-gen alice
$CLI2 simple-define-asset alice AliceCoin
$CLI2 simple-issue-asset AliceCoin 10000
echo "\"i4-1NC50E4omcPdO4N28v7cBvp0pnPOFp6Jvyu4G3J4=\"" | $CLI2 load-public-key bob
$CLI2 initialize-transaction "tx1"
$CLI2 transfer-assets --builder=tx1

# KrH3DD94W2:utxo0
# 5000
# n
# n
# bob
# n
# password