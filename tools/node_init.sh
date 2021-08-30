#!/usr/bin/env bash

env=$1

h="${HOME}/.__findora__"
th="${HOME}/.tendermint"
tc="${th}/config/config.toml"

check_env() {
    for i in wget curl perl; do
        which $i >/dev/null 2>&1
        if [[ 0 -ne $? ]]; then
            echo -e "\n\033[31;01m${i}\033[00m has not been installed properly!\n"
            exit 1
        fi
    done
}

serv_url=
sentry_peers=
set_env() {
    if [[ $env == "" ]]; then
        serv_url="http://localhost"
        sentry_peers=""
    elif [[ $env == "qa01" ]]; then
        serv_url="https://dev-qa01.dev.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@dev-qa01-us-west-2-sentry-000-public.dev.findora.org:26656"
    elif [[ $env == "qa02" ]]; then
        serv_url="https://dev-qa02.dev.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@dev-qa02-us-west-2-sentry-000-public.dev.findora.org:26656"
    elif [[ $env == "testnet" ]]; then
        serv_url="https://prod-testnet.prod.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@prod-testnet-us-west-2-sentry-000-public.prod.findora.org:26656"
    elif [[ $env == "mainnet" ]]; then
        serv_url="https://prod-mainnet.prod.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@prod-mainnet-us-west-2-sentry-000-public.prod.findora.org:26656"
    else
        echo -e "Unknown ENV !!!"
        exit 1
    fi

    rm -rf $h $th
    tendermint init

    if [[ "" != ${sentry_peers} ]]; then
        perl -pi -e "s/^(persistent_peers = ).*/\$1 \"${sentry_peers}\"/" $tc
    fi

    perl -pi -e 's/^(timeout_propose =).*/$1 "8s"/' $tc
    perl -pi -e 's/^(timeout_propose_delta =).*/$1 "100ms"/' $tc
    perl -pi -e 's/^(timeout_prevote =).*/$1 "4s"/' $tc
    perl -pi -e 's/^(timeout_prevote_delta =).*/$1 "100ms"/' $tc
    perl -pi -e 's/^(timeout_precommit =).*/$1 "4s"/' $tc
    perl -pi -e 's/^(timeout_precommit_delta =).*/$1 "100ms"/' $tc
    perl -pi -e 's/^(timeout_commit =).*/$1 "15s"/' $tc
    perl -pi -e 's/^(recheck =).*/$1 false/' $tc

    curl ${serv_url}:26657/genesis \
        | jq -c '.result.genesis' \
        | jq > ~/.tendermint/config/genesis.json || exit 1
}

check_env
set_env

###################
# Run local node #
###################

cd /tmp || exit 1
abcid -l -q \
    --tendermint-node-key-config-path="${th}/config/priv_validator_key.json" \
    >abcid.log 2>&1 &
tendermint node >tendermint.log 2>&1 &

echo "**** ABCId log path: /tmp/abcid.log"
echo "**** Tendermint log path: /tmp/tendermint.log"

curl $serv_url:8669/version
