#!/usr/bin/env bash
RED='\033[31m'
GRN="\033[32m"
YEL='\033[33m'
BLU='\033[34m'
NC='\033[0m'

# default environment values
DEFAULT_BIN_CFG="release"
DEFAULT_BLOCK_INTERVAL="10"
DEFAULT_ENDPOINT="http://0.0.0.0"

# binary config
BIN_CFG="${BIN_CFG:=$DEFAULT_BIN_CFG}"
export BIN="target/$BIN_CFG"

# block interval and endpoint
export BLOCK_INTERVAL="${BLOCK_INTERVAL:=$DEFAULT_BLOCK_INTERVAL}"
export ENDPOINT="${ENDPOINT:=$DEFAULT_ENDPOINT}"

# paths and misc
TMP_DEBUG=/tmp/findora
export FIN_DEBUG="${FIN_DEBUG:=$TMP_DEBUG}"
export DEVNET="$FIN_DEBUG/devnet"
export WALLET="$HOME/.findora"
export EVM_FIRST_BLOCK_HEIGHT="${EVM_FIRST_BLOCK_HEIGHT:=1}"

# logs
ABCI_LOG_LEVEL="abciapp=info,baseapp=info,account=info,ethereum=info,evm=info,eth_rpc=info"

# keypair
MNEMONIC="zoo nerve assault talk depend approve mercy surge bicycle ridge dismiss satoshi boring opera next fat cinnamon valley office actor above spray alcohol giant"
PRIV_KEY="o9gXFI5ft1VOkzYhvFpgUTWVoskM1CEih0zJcm3-EAQ="

# create directories and file
mkdir -p $WALLET
mkdir -p $DEVNET
echo "$MNEMONIC" > $WALLET/mnenomic.key

# setup endpoint
$BIN/fn setup -S $ENDPOINT > /dev/null
$BIN/fn setup -O $WALLET/mnenomic.key > /dev/null

# show envs
if [ "$1" == "s" ]; then
    echo "BIN_CFG     = $BIN_CFG"
    echo "INTERVAL    = $BLOCK_INTERVAL"
    echo "ENDPOINT    = $ENDPOINT"
    echo "WALLET      = $WALLET/mnenomic.key"
    echo "DEVNET      = $DEVNET"
    echo "PRIVATE_KEY = $PRIV_KEY"
fi
