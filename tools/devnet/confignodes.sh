#!/bin/bash

source tools/devnet/env.sh || exit 1

localhost="0.0.0.0"
base_url="tcp:\/\/${localhost}:"

default_proxy_app_port=26658
default_rpc_laddr_port=26657
default_p2p_laddr_port=26656
default_submission_port=8669
default_ledger_port=8668
default_evm_http_port=8545
default_evm_ws_port=8546
port_setp=10

blocks_interval=0
timeout_commit=15
if [ ! -z $BLOCK_INTERVAL ]
then
    timeout_commit=$BLOCK_INTERVAL
fi

i=0
nodes=`ls -d $DEVNET/* | sort -V`
for node in $nodes
do
    proxy_app_port=`expr ${default_proxy_app_port} + ${port_setp} \* ${i}`
    rpc_laddr_port=`expr ${default_rpc_laddr_port} + ${port_setp} \* ${i}`
    p2p_laddr_port=`expr ${default_p2p_laddr_port} + ${port_setp} \* ${i}`
    submission_port=`expr ${default_submission_port} + ${port_setp} \* ${i}`
    ledger_port=`expr ${default_ledger_port} + ${port_setp} \* ${i}`
    evm_http_port=`expr ${default_evm_http_port} + ${port_setp} \* ${i}`
    evm_ws_port=`expr ${default_evm_ws_port} + ${port_setp} \* ${i}`

    config_toml=$node/config/config.toml
    persistent_peers=`sed -n "/^persistent_peers = \"/p" $config_toml | awk -F '"' '{print $2}'`
    peers=(${persistent_peers//,/ })
    tmp_peer=""
    for j in "${!peers[@]}"
    do
        if [ $j == $i ]
        then
            continue
        fi
        tmp=`echo "${peers[j]}" | awk -F '@' '{print $1}'`
        if [ -z "$tmp_peer" ]
        then
            tmp_peer="$tmp@$localhost:`expr ${default_p2p_laddr_port} + ${port_setp} \* ${j}`"
        else
            tmp_peer="$tmp_peer,$tmp@$localhost:`expr ${default_p2p_laddr_port} + ${port_setp} \* ${j}`"
        fi
    done

    POSIX_BACKUP_OPTION=$(sed v < /dev/null 2> /dev/null || echo -n "''")
    sed -i $POSIX_BACKUP_OPTION "13s/proxy_app = .*/proxy_app = \"${base_url}${proxy_app_port}\"/g" $config_toml
    sed -i $POSIX_BACKUP_OPTION "84s/laddr = .*/laddr = \"${base_url}${rpc_laddr_port}\"/g" $config_toml
    sed -i $POSIX_BACKUP_OPTION "163s/laddr = .*/laddr = \"${base_url}${p2p_laddr_port}\"/g" $config_toml
    sed -i $POSIX_BACKUP_OPTION "175s/persistent_peers = .*/persistent_peers = \"${tmp_peer}\"/g" $config_toml
    sed -i $POSIX_BACKUP_OPTION "272s/timeout_commit = .*/timeout_commit = \"${timeout_commit}s\"/g" $config_toml
    sed -i $POSIX_BACKUP_OPTION "279s/create_empty_blocks_interval = .*/create_empty_blocks_interval = \"${blocks_interval}s\"/g" $config_toml
    sed -i $POSIX_BACKUP_OPTION "315s/index_all_keys = .*/index_all_keys = true/g" $config_toml

    abci_toml=$node/abci/abci.toml
    echo 'abci_host = "0.0.0.0"' >> $abci_toml
    echo "abci_port = \"${proxy_app_port}\"" >> $abci_toml
    echo '' >> $abci_toml
    echo 'tendermint_host = "0.0.0.0"' >> $abci_toml
    echo "tendermint_port = \"${rpc_laddr_port}\"">> $abci_toml
    echo '' >> $abci_toml
    echo 'submission_host = "0.0.0.0"' >> $abci_toml
    echo "submission_port =  \"${submission_port}\"" >> $abci_toml
    echo '' >> $abci_toml
    echo 'ledger_host = "0.0.0.0"' >> $abci_toml
    echo "ledger_port =  \"${ledger_port}\"" >> $abci_toml
    echo '' >> $abci_toml
    echo "evm_http_port =  \"${evm_http_port}\"" >> $abci_toml
    echo "evm_ws_port =  \"${evm_ws_port}\"" >> $abci_toml
    ((i++))
done
