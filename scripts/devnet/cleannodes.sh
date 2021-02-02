#!/usr/bin/env bash
RED='\e[31m'
GRN="\e[32m"
NC='\033[0m'

# paths
DEVNET="$LEDGER_DIR/devnet"

# stop nodes
script_stop=$(dirname "$0")/stopnodes.sh
bash "$script_stop"

# clean nodes
nodes=`ls -l $DEVNET | grep node | awk '(NR>0){print $9}'`
echo -n "cleaned: "
for node in $nodes
do

echo -en "${GRN}$node ${NC}"
# abcis
rm -rf $DEVNET/$node/abci/utxo_map
rm -rf $DEVNET/$node/abci/txn_merkle
rm -rf $DEVNET/$node/abci/txn_log
rm -rf $DEVNET/$node/abci/block_merkle

# tendermint
rm -rf $DEVNET/$node/data/*.db
rm -rf $DEVNET/$node/data/cs.wal
rm -rf $DEVNET/$node/config/addrbook.json
rm -rf $DEVNET/$node/abci_validator.log
rm -rf $DEVNET/$node/consensus.log
cat > $DEVNET/$node/data/priv_validator_state.json <<EOF
{
"height": "0",
"round": "0",
"step": 0
}
EOF
done

echo
