#!/usr/bin/env bash

unset LEDGER_DIR

RED='\033[31m'
GRN="\033[32m"
YEL='\033[33m'
NC='\033[0m'

# paths
WALLET="$HOME/.findora"

# mnemonic
MNEMONIC="then amused absent retire deputy omit torch know parrot matter bridge sustain moral excuse alert reward eight dust dress sheriff certain shed present february"

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

# clean wallet if exists
rm -rf $WALLET/cli2_data.sqlite
rm -rf $WALLET/*_passphrase
rm -rf $WALLET/snapshot.tar.gz

# run findora cli2
echo -e "${GRN}step-1: key gen----------------------------------------------------${NC}"
printf "http://localhost:8629\nhttp://localhost:8628\n$MNEMONIC\n$pswd\n$pswd\n" | findora restore-from-mnemonic-bip44 $nick

# show genesis key
findora list-keys
echo

## Start Network
echo -e "${GRN}step-2: start nodes----------------------------------------------------${NC}"
stopnodes.sh
resetnodes.sh 4 4
cleannodes.sh
startnodes.sh
echo

# query ledger state
echo -e "${GRN}step-3: query state------------------------------------------------${NC}"
printf "y\n" | findora query-ledger-state
echo

echo -e "${GRN}step-4: create genesis transaction----------------------------------${NC}"
findora initialize-transaction genesis

# define asset
echo -e "        step-4-1: define genesis asset------------------------------"
printf "$pswd\n$max_units\n$memo_updatable\n$memo\n" | findora define-asset --is-fra genesis $nick $token

# issue asset
echo -e "        step-4-2: issue genesis asset-------------------------------"
printf "$pswd\n" | findora issue-asset genesis $token 0 $issuance
echo

# submit genesis transaction
echo -e "${GRN}step-5: build and submit genesis transaction------------------------${NC}"
printf "$pswd\n" | findora build-transaction
printf "\n\n" | findora submit genesis
