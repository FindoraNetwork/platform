#!/usr/bin/env bash

# env
source tools/devnet/env.sh || exit


# stop nodes
Node=""
if [ ! -z "$1" ];
then
    Node="node$1"
    tools/devnet/stopnodes.sh "$1"
else
    tools/devnet/stopnodes.sh
fi

# clean nodes
nodes=`ls -l $DEVNET | grep node | awk '(NR>0){print $9}' | sort -V`
echo -n "cleaned: "
for node in $nodes
do
if [ -z "$Node" ] || ([ ! -z "$Node" ] && [ "$Node" = "$node" ]); then
    echo -en "$node "
    # abcis
    ls -d $DEVNET/$node/abci/* | grep -v 'abci.toml' | xargs rm -rf

    # tendermint
    rm -rf $DEVNET/$node/data/*.db
    rm -rf $DEVNET/$node/data/cs.wal
    rm -rf $DEVNET/$node/config/addrbook.json
    rm -rf $DEVNET/$node/abcid.log
    rm -rf $DEVNET/$node/consensus.log
    cat > $DEVNET/$node/data/priv_validator_state.json <<EOF
{
  "height": "0",
  "round": "0",
  "step": 0
}
EOF

fi
done
echo
