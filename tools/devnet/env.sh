#!/usr/bin/env bash
RED='\033[31m'
GRN="\033[32m"
YEL='\033[33m'
NC='\033[0m'

# paths
TMP_DEBUG=/tmp/findora
export FIN_DEBUG="${FIN_DEBUG:=$TMP_DEBUG}"
export DEVNET="$FIN_DEBUG/devnet"

# binary config
BIN_CFG_DEFAULT=release
BIN_CFG="${BIN_CFG:=$BIN_CFG_DEFAULT}"
BIN="target/$BIN_CFG"

# logs
ABCI_LOG_LEVEL="abciapp=debug,baseapp=info,account=info,ethereum=info,evm=info,eth_rpc=info"

# keypair
MNEMONIC="zoo nerve assault talk depend approve mercy surge bicycle ridge dismiss satoshi boring opera next fat cinnamon valley office actor above spray alcohol giant"
PRIV_KEY="tffuZXQRo4XZcD7Xwm09JOoIJT6vdRe_8gZLxWRuB9U="

# other
export BLOCK_INTERVAL="6"

# show envs
if [ "$1" == "s" ]; then
    echo "FIN_DEBUG = $FIN_DEBUG"
    echo "DEVNET   = $DEVNET"
    echo "BIN_CFG  = $BIN_CFG"
    echo "MNEMONIC = $MNEMONIC"
    echo "PRIVATE_KEY = $PRIV_KEY"
fi
