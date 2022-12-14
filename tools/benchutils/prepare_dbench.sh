#!/usr/bin/env bash

#################################################
#### Ensure we are in the right path. ###########
#################################################
if [[ 0 -eq $(echo $0 | grep -c '^/') ]]; then
    # relative path
    EXEC_PATH=$(dirname "`pwd`/$0")
else
    # absolute path
    EXEC_PATH=$(dirname "$0")
fi

EXEC_PATH=$(echo ${EXEC_PATH} | sed 's@/\./@/@g' | sed 's@/\.*$@@')
cd $EXEC_PATH || die "$0 Line $LINENO"
#################################################

fn ddev create -i 1 -f || exit 1
sleep 10

phrase_path=/tmp/fn_dbench_root.phrase
fn ddev \
    | jq '.meta.custom_data.bank_account.mnemonic_words' \
    | sed 's/"//g' > $phrase_path || exit 1
fn setup -O $phrase_path || exit 1
fn setup -S "http://localhost" || exit 1 # useless, but must set a value

itv=$(fn ddev | jq '.meta.block_interval_in_seconds')

fn ddev init || exit 1
for i in $(seq 0 6); do
    sleep $itv
done

fn contract-deposit --addr $(cat static/root.addr) --amount 1000000000000000 || exit 1
sleep $itv
sleep $itv # 2 blocks

addr=$(fn ddev | jq '.meta.validator_or_full_nodes."1".host."addr"' | sed 's/"//g')
port=$(fn ddev | jq '.meta.validator_or_full_nodes."1".ports."web3_http_service"' | sed 's/"//g')

endpoint="http://${addr}:${port}"

echo $endpoint > /tmp/fn_dbench_rpc_endpoint
