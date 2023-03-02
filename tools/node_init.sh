#!/usr/bin/env bash

env=$1

th="${HOME}/.tendermint"
if [[ "" != ${TENDERMINT_HOME} ]]; then
    th=${TENDERMINT_HOME}
fi
echo -e "\n====\033[31;01m DATA PATH: ${TENDERMINT_HOME} \033[00m====\n"

tc="${th}/config/config.toml"
h="${th}/__findora__"
checkpoint=${th}/${env}.checkpoint

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
    if [[ $env == "debug_env" ]]; then
        if [[ "" == $DEBUG_ENV_IP ]]; then
            echo -e "\033[31;01mthe \$DEBUG_ENV_IP env var must be set before you start joining the debug_env !!!\033[0m"
            exit 1
        fi
        serv_url="http://${DEBUG_ENV_IP}"
        sentry_peers="c8beed5e3001d9a26dd1c8830190a65bfe9577c7\@${DEBUG_ENV_IP}:26616,ef34d99dbe6039dfd06d7d4dc923d7371b806207\@${DEBUG_ENV_IP}:26626,a2e8f2e4e30c68799acc8c891a2457da32ff4eeb\@${DEBUG_ENV_IP}:26636,1eb6e73fd9e3605bc0e8782f56e941768f83745c\@${DEBUG_ENV_IP}:26646,eae87d6e6b93234ee26690fe6009432053377718\@${DEBUG_ENV_IP}:26656,a20f5a677abc523a3778f2b4b5228a8ea511c171\@${DEBUG_ENV_IP}:26666,77ebc9c2c0e0baa06ff0565e51378d4accd11d16\@${DEBUG_ENV_IP}:26676,8160e4193f744d355b03a08d916f097b27447bdb\@${DEBUG_ENV_IP}:26686,ac220f9d34de57aeb616df09b99baaa1ca26c4be\@${DEBUG_ENV_IP}:26696,106bb438509111893b5fc699097a306acadd5329\@${DEBUG_ENV_IP}:26706,332acb1da85c3bea93b5c2200402873fb90f58c9\@${DEBUG_ENV_IP}:26716,67f24be5121c72c407686f95d5a671751190d6ef\@${DEBUG_ENV_IP}:26726,7fced2da63cfaa39c1480deec5eb88c533e907b9\@${DEBUG_ENV_IP}:26746,2bf6b9a21ea14854b5389b1ae28982c4314555c8\@${DEBUG_ENV_IP}:26756,b577986d0bf9f835f97ca629b71fc741e236ff00\@${DEBUG_ENV_IP}:26766,a9b1f6ef334a773dc9391bd657a7bb7e8c3825ce\@${DEBUG_ENV_IP}:26776,da13b5934c73c9f1d97ba0e3ddb4706031d1418b\@${DEBUG_ENV_IP}:26786,ed09729cdebc8a57c8150615491e389f4a4feeb7\@${DEBUG_ENV_IP}:26796,3542dc1c2a097f12eb235dc1dfc2b79b0de11355\@${DEBUG_ENV_IP}:26806,339dea1bf31c997e5ff6f476d5e5298d259fc8eb\@${DEBUG_ENV_IP}:26816"
        perl -pi -e 's/addr_book_strict\s*=.*/addr_book_strict = false/g' $tc
    elif [[ $env == "qa01" ]]; then
        serv_url="https://dev-qa01.dev.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@dev-qa01-us-west-2-sentry-000-public.dev.findora.org:26656"
    elif [[ $env == "qa02" ]]; then
        serv_url="https://dev-qa02.dev.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@dev-qa02-us-west-2-sentry-000-public.dev.findora.org:26656"
    elif [[ $env == "qa03" ]]; then
        serv_url="https://dev-qa03.dev.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@dev-qa03-us-west-2-sentry-000-public.dev.findora.org:26656"
    elif [[ $env == "testnet" ]]; then
        serv_url="https://prod-testnet.prod.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@prod-testnet-us-west-2-sentry-000-public.prod.findora.org:26656,d0c6e3e1589695ae6d650b288caf2efe9a998a50\@prod-testnet-us-west-2-sentry-001-public.prod.findora.org:26656,78661a9979c100e8f1303cbd121cb1b326ff694f\@prod-testnet-us-west-2-sentry-002-public.prod.findora.org:26656,6723af6a3aef14cd7eb5ee8d5d0ac227af1e9651\@prod-testnet-us-west-2-sentry-003-public.prod.findora.org:26656"
    elif [[ $env == "mainnet" ]]; then
        serv_url="https://prod-mainnet.prod.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@prod-mainnet-us-west-2-sentry-000-public.prod.findora.org:26656,d0c6e3e1589695ae6d650b288caf2efe9a998a50\@prod-mainnet-us-west-2-sentry-001-public.prod.findora.org:26656,78661a9979c100e8f1303cbd121cb1b326ff694f\@prod-mainnet-us-west-2-sentry-002-public.prod.findora.org:26656,6723af6a3aef14cd7eb5ee8d5d0ac227af1e9651\@prod-mainnet-us-west-2-sentry-003-public.prod.findora.org:26656"
    elif [[ $env == "evm" ]]; then
        serv_url="https://dev-evm.dev.findora.org"
        sentry_peers="b87304454c0a0a0c5ed6c483ac5adc487f3b21f6\@dev-evm-us-west-2-sentry-000-public.dev.findora.org:26656,d0c6e3e1589695ae6d650b288caf2efe9a998a50\@dev-evm-us-west-2-sentry-001-public.dev.findora.org:26656"
    else
        echo -e "Unknown ENV !!!"
        exit 1
    fi

    mkdir -p $h || exit 1
    rm -rf $th || exit 1
    mkdir -p $h || exit 1
    tendermint init --home $th || exit 1

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
    perl -pi -e 's/^(size =) 5000/$1 2000/' $tc
    # perl -pi -e 's/^(fast_sync =).*/$1 false/' $tc


    curl ${serv_url}:26657/genesis \
        | jq -c '.result.genesis' \
        | jq > ${th}/config/genesis.json || exit 1

    EVM_CHAIN_ID=$(curl -H 'Content-Type: application/json' --data '{"id":1, "method": "eth_chainId", "jsonrpc": "2.0"}' "${serv_url}:8545" | jq -c '.result' | sed 's/"//g')
    export EVM_CHAIN_ID=$(printf '%d' $EVM_CHAIN_ID)

    curl ${serv_url}:8667/display_checkpoint | jq > ${checkpoint}
    if [[ 0 != $? || 0 == $(wc -l ${checkpoint} | perl -pe 's/^\s+//g' | grep -o '^[0-9]') ]]; then
        printf "\nfail to get checkpoint file!\n"
        exit 1
    fi
}

if [[ "" == $2 ]]; then
    check_env
    set_env
fi

###################
# Run local node #
###################

cd /tmp || exit 1
echo -e "\n====\033[31;1m EVM_CHAIN_ID: $EVM_CHAIN_ID \033[0m====\n"
abcid -q -d ${h} \
    --checkpoint-file ${checkpoint} \
    --tendermint-node-key-config-path="${th}/config/priv_validator_key.json" \
    >${th}/log.abcid 2>&1 &
tendermint node --home ${th} >${th}/log.tendermint 2>&1 &

echo "**** ABCId log path: ${th}/log.abcid"
echo "**** Tendermint log path: ${th}/log.tendermint"

sleep 1
curl localhost:8669/version
