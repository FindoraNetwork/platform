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

source "utils.sh"

cnt=$1
is_dbench=$2 # is dbench or not

addr_file="/tmp/gsc_bench.addr.list"
phrase_file="/tmp/gsc_bench.phrase.list"
target_file="/tmp/gsc_target_list.json"

ROOT_PHRASE="$(cat static/root.phrase)"

which gsc >/dev/null 2>&1
if [[ 0 -ne $? ]]; then
    codebase=/tmp/__gsc_${RANDOM}_${RANDOM}__
    rm -rf $codebase 2>/dev/null
    git clone --depth 1 git@github.com:FindoraNetwork/gsc.git $codebase
    cargo install --path $codebase --bin gsc
    rm -rf $codebase
    export PATH=~/.cargo/bin:$PATH
fi

which gsc >/dev/null 2>&1
if [[ 0 -ne $? ]]; then
    die "$0 Line $LINENO: gsc binary not found!"
fi

function check_cnt() {
    x=$[$cnt + 1]
    if [[ 0 != $? ]]; then
        die "The number of accounts is not an integer!"
    fi

    if [[ 0 -eq $cnt ]]; then
        die "The nubmer of accounts should not be zero!"
    fi
}

function gen_accounts() {
    rm -rf $addr_file $phrase_file 2>/dev/null
    local tmpfile="/tmp/.${RANDOM}.${RANDOM}.${RANDOM}"
    gsc cli gen -n $[${cnt} * 5 / 4] >${tmpfile} || die "$0 Line $LINENO"
    awk 'NR%3==1 {print $0}' ${tmpfile} | grep -o '0x.*' | sort | uniq > ${addr_file}
    awk 'NR%3==2 {print $0}' ${tmpfile} | grep 'Phrase' | sed -r 's/^[^ ]+ //g' | sort | uniq > ${phrase_file}
    rm $tmpfile || die "$0 Line $LINENO"
    cnt1=$(cat ${phrase_file} | wc -l)
    cnt2=$(cat ${addr_file} | wc -l)
    if [[ $cnt -gt $cnt1 || $cnt1 -ne $cnt2 ]]; then
        die "Fatal: the generated accounts are invalid !"
    fi
}

function prepare_run() {
    local i=
    local k=

    local upper=${cnt}
    local amount=1

    local phrase_set=()
    local target_addr_set=()

    IFS_OLD=$IFS
    IFS=$'\n'

    i=0
    for addr in $(awk "NR>=1 && NR<=${upper} {print \$0}" ${addr_file}); do
        target_addr_set[${i}]="${addr}"
        let i+=1
    done

    IFS=$IFS_OLD

    dbg "Length of 'target_addr_set': ${#target_addr_set[@]}"

    local unit=$[${upper} / 32]
    for i in {0..31}; do
        printf "[" > ${target_file}${i} || die "$0 Line $LINENO"
        for ((k=0;k<${unit};k++)); do
            local addr_idx=$[$k + $i * ${unit}]
            local addr=${target_addr_set[${addr_idx}]}
            printf "[\"${ROOT_PHRASE}\",\"${addr}\",\"${amount}\"]," >> ${target_file}${i}
        done
        perl -pi -e 's/,$//' ${target_file}${i} || die "$0 Line $LINENO"
        printf "]" >> ${target_file}${i}
    done
}

function run() {
    local contract=$1

    dbg "Start sending ..."

    # 31 async tasks
    for i in {1..31}; do
        if [[ "" == ${contract} ]]; then
            gsc cli transfer -x "${SERV}" -b -D -f ${target_file}${i} 2>/dev/null 1>&2 &
        else
            gsc cli transfer -x "${SERV}" -b -D -f ${target_file}${i} -C $contract 2>/dev/null 1>&2 &
        fi
    done

    # 1 sync task
    if [[ "" == ${contract} ]]; then
        sleep 1
        gsc cli transfer -x "${SERV}" -b -D -f "${target_file}0" >/dev/null || log "ERROR $0 Line $LINENO"
    else
        sleep 1
        gsc cli transfer -x "${SERV}" -b -D -f "${target_file}0" -C $contract >/dev/null || log "ERROR $0 Line $LINENO"
    fi
}

check_cnt

if [[ "" == ${is_dbench} ]]; then
    fn dev create -i 1 -f || die "$0 Line $LINENO"
    log "Waiting RPC-server, sleep 6 seconds"
    sleep 6

    phrase_path=/tmp/gsc_bench_root.phrase
    fn dev \
        | jq '.meta.custom_data.bank_account.mnemonic_words' \
        | sed 's/"//g' > $phrase_path || exit 1
    fn setup -O $phrase_path || exit 1
    fn setup -S "http://localhost" || exit 1 # useless, but must set a value

    block_itv=$(fn dev | jq '.meta.block_interval_in_seconds')

    fn dev init || exit 1
    for i in {0..8}; do
        sleep $block_itv
    done

    fn contract-deposit --addr $(cat static/root.addr) --amount 1000000000000000 || exit 1
    for i in {0..4}; do
        sleep $block_itv
    done

    web3_port=$(fn dev | jq '.meta.validator_or_full_nodes."1"."ports"."web3_http_service"')
    export SERV="http://localhost:${web3_port}"

else

    fn ddev create -i 1 -f || die "$0 Line $LINENO"
    log "Waiting RPC-server, sleep 12 seconds"
    sleep 12

    phrase_path=/tmp/fn_dbench_root.phrase
    fn ddev \
        | jq '.meta.custom_data.bank_account.mnemonic_words' \
        | sed 's/"//g' > $phrase_path || exit 1
    fn setup -O $phrase_path || exit 1
    fn setup -S "http://localhost" || exit 1 # useless, but must set a value

    block_itv=$(fn ddev | jq '.meta.block_interval_in_seconds')

    fn ddev init || exit 1
    for i in {0..6}; do
        sleep $block_itv
    done

    fn contract-deposit --addr $(cat static/root.addr) --amount 1000000000000000 || exit 1
    for i in {0..4}; do
        sleep $block_itv
    done

    serv=""
    # 3 nodes will be created at least
    for i in {0..2}; do
        addr=$(fn ddev | jq ".meta.validator_or_full_nodes.\"${i}\".host.\"addr\"" | sed 's/"//g')
        port=$(fn ddev | jq ".meta.validator_or_full_nodes.\"${i}\".ports.web3_http_service" | sed 's/"//g')
        serv="http://${addr}:${port},${serv}"
    done
    export SERV=$serv
fi

log "RPC Server endpoints: ${SERV}"

gen_accounts
prepare_run

log "Deploying ERC20 contract ..."
contract_addr=$(gsc cli deploy-erc20 -x $SERV -O "${ROOT_PHRASE}")
for i in {0..4}; do
    sleep $block_itv
done

first_serv=$(echo $SERV | sed 's/,.*//')

curl_post_ret=
curl_post() {
    method=$1
    params=$2
    curl_post_ret=$(curl $first_serv -H 'Content-Type: application/json' -X POST --data "{\"jsonrpc\":\"2.0\",\"method\":\"${method}\",\"params\":[${params}],\"id\":1}" 2>/dev/null)
}

function start_cnter() {
    curl_post eth_blockNumber
    # echo $curl_post_ret
    BlockNumber=$(echo $curl_post_ret | jq -c ".result"| sed 's/"//g' | sed 's/0x//')
    BlockNumber=$(printf "%d" "0x${BlockNumber}" || die "$0 Line $LINENO ${BlockNumber}")
    log "BlockNumber before sending: ${BlockNumber}"
}

function get_results() {
    let cnter=6
    while :; do
        curl_post eth_getBlockTransactionCountByNumber \"latest\"
        n=$(echo $curl_post_ret | jq -c ".result"| sed 's/"//g' | sed 's/0x//')
        n=$(printf "%d" "0x${n}")
        if [[ 0 -ne $? ]]; then
            break
        elif [[ 0 -eq $n ]]; then
            if [[ 0 == $cnter ]]; then
                break;
            else
                let cnter-=1;
                sleep $block_itv
            fi
        else
            let cnter=6
            sleep $block_itv
        fi
    done

    ts=$(cat /tmp/gsc_cli_batch.start.timestamp)
    ts=$[2 + $ts]
    log "Timestamp before sending: ${ts}"
    ts2=$[$(date +%s) - 2]
    log "Timestamp after sending: ${ts2}"

    curl_post eth_blockNumber
    BlockNumber2=$(echo $curl_post_ret | jq -c ".result"| sed 's/"//g' | sed 's/0x//')
    BlockNumber2=$(printf "%d" "0x${BlockNumber2}")
    log "BlockNumber after sending: ${BlockNumber2}"

    let tx_total=0
    for bn in $(seq ${BlockNumber} ${BlockNumber2}); do
        curl_post eth_getBlockTransactionCountByNumber \"${bn}\"
        n=$(echo $curl_post_ret | jq -c ".result"| sed 's/"//g' | sed 's/0x//')
        n=$(printf "%d" "0x${n}")
        let tx_total+=${n}
    done

    log "Total number of on-chain transactions: ${tx_total}"
    log "TPS(transaction per second): $[${tx_total} / (${ts2} - ${ts})]"
}

echo
log "Transfering native token ..."
echo

start_cnter
run
get_results

echo
sleep 10 # avoid data tail
log "Transfering erc20 token ..."
echo

start_cnter
run $contract_addr
get_results
