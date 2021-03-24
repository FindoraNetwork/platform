#!/usr/bin/env bash
RED='\033[31m'
GRN="\033[32m"
YEL='\033[33m'
NC='\033[0m'

# paths
DEVNET="$LEDGER_DIR/devnet"
WALLET="$HOME/.findora"

# params
nick=$1
pswd=$2
token=$3
max_units=$4
issuance=$5
memo=$6
memo_updatable=$7

# show and confirm params
echo -e "${GRN}step-0: parameters-------------------------------------------------${NC}"
echo "user      = $nick"
echo "token     = $token"
echo "max_units = $max_units"
echo "issuance  = $issuance"
echo "memo      = $memo"
echo "memo_updatable = $memo_updatable"
echo
echo -n "confirm (y/n)? "
read answer
if [ "$answer" == "${answer#[Yy]}" ] ;then
    exit 0
fi
echo

# scripts
cleannodes=$(dirname "$0")/cleannodes.sh
startnodes=$(dirname "$0")/startnodes.sh
stopnodes=$(dirname "$0")/stopnodes.sh

# clean wallet if exists
rm -rf $WALLET/cli2_data.sqlite
rm -rf $WALLET/*_passphrase
rm -rf $WALLET/snapshot.tar.gz

# clean and restart nodes
echo -e "${GRN}step-1: run mainnet------------------------------------------------${NC}"
./$cleannodes
./$startnodes
echo

# run findora cli2
echo -e "${GRN}step-2: key gen----------------------------------------------------${NC}"
printf "http://localhost:8629\nhttp://localhost:8628\n$pswd\n$pswd\n" | findora key-gen $nick

# show genesis key
findora list-keys
echo

# query ledger state
echo -e "${GRN}step-3: query state------------------------------------------------${NC}"
printf "y\n" | findora query-ledger-state
echo

# clean and restart nodes
echo -e "${GRN}step-4: restart mainnet---------------------------------------------${NC}"
./$cleannodes
./$startnodes
echo

echo -e "${GRN}step-5: create genesis transaction----------------------------------${NC}"
findora initialize-transaction genesis

# define asset
echo -e "        step-5-1: define genesis asset------------------------------"
printf "$pswd\n$max_units\n$memo_updatable\n$memo\n" | findora define-asset --is-fra genesis $nick $token

# issue asset
echo -e "        step-5-2: issue genesis asset-------------------------------"
printf "$pswd\n" | findora issue-asset genesis $token 0 $issuance
echo

# clean and restart nodes
echo -e "${GRN}step-6: restart mainnet---------------------------------------------${NC}"
./$cleannodes
./$startnodes
sleep 1
echo

# submit genesis transaction
echo -e "${GRN}step-7: build and submit genesis transaction------------------------${NC}"
printf "$pswd\n" | findora build-transaction
printf "\n\n" | findora submit genesis
./$stopnodes
echo

# submit genesis transaction
echo -e "${GRN}step-8: harvest snapshot-------------------------------------------${NC}"
tar -czf $WALLET/snapshot.tar.gz -C $DEVNET . > /dev/null
echo -e "Done. Please take and secure snapshot files in: ${GRN}$WALLET${NC}"
