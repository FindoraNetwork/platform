#!/usr/bin/env bash

# env
source tools/devnet/env.sh || exit 1

# paths
WALLET="$HOME/.findora"

# clean and recreate mnemonic
rm -rf $WALLET/mnenomic.key
rm -rf $WALLET/snapshot.tar.gz
echo "$MNEMONIC" > $WALLET/mnenomic.key

# show and confirm genesis keypair
echo -e "${GRN}step-0: keypair-------------------------------------------------${NC}"
echo -e "mnemonic    = ${YEL}$MNEMONIC${NC}"
echo -e "private_key = ${YEL}$PRIV_KEY${NC}"
echo
echo -n "confirm (y/n)? "
read answer
if [ "$answer" != "${answer#[Nn]}" ] ;then
    exit 0
fi
echo

# scripts
cleannodes=$(dirname "$0")/cleannodes.sh
startnodes=$(dirname "$0")/startnodes.sh
stopnodes=$(dirname "$0")/stopnodes.sh

# clean and restart nodes
echo -e "${GRN}step-1: run network------------------------------------------------${NC}"
./$cleannodes
./$startnodes
sleep 3
echo

# init network
echo -e "${GRN}step-2: init network-----------------------------------------------${NC}"
$BIN/fn setup -S http://0.0.0.0 > /dev/null
$BIN/fn setup -O $WALLET/mnenomic.key > /dev/null
echo -e "host: http://0.0.0.0"
echo -e "key : $WALLET/mnenomic.key"
sleep 3
#echo -e "$BIN/stt"
$BIN/stt init -i $BLOCK_INTERVAL -s
#./$stopnodes
echo

# done
echo -e "${GRN}step-3: harvest snapshot-------------------------------------------${NC}"
tar -czf $WALLET/snapshot.tar.gz -C $DEVNET . > /dev/null
echo -e "Done. Genesis snapshot saved to: ${GRN}$WALLET${NC}"
