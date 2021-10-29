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
cd $EXEC_PATH || exit 1
#################################################

ext_addr=$1
if [[ "" != $ext_addr ]]; then
    ext_addr="tcp://${ext_addr}:26656"
fi

if [[ -d /data/findora/config ]]; then
    rm -rf ~/.tendermint ~/config_bak
    mv /data/findora/config ~/config_bak
    findorad init --mainnet || exit 1
    cp -r ~/.tendermint/config /data/findora/ || exit 1
else
    rm -rf /data/findora/*
    findorad init -b /data/findora --mainnet || exit 1
fi

sed -ri "s#(external_address =).*#\1 \"${ext_addr}\"#" /data/findora/config/config.toml || exit 1

target_path=$(dirname $(systemctl cat network.target | grep -o '#.*/network.target' | sed -r 's/^\s*#\s+//g'))
cp -f abcid.service tendermint.service ${target_path}/ || exit 1

for s in abcid tendermint; do
    systemctl disable ${s}.service
    systemctl enable ${s}.service
    systemctl stop ${s}.service
    systemctl start ${s}.service
    systemctl status ${s}.service 2>/dev/null
done
