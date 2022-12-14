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

TOOLS_DIR=$(dirname ${EXEC_PATH})
ROOT_DIR=$(dirname ${TOOLS_DIR})

cnt=$1
serv=$2

DEFAULT_SERV="http://localhost:8545"
if [[ "" != ${serv} ]]; then
    SERV="${serv}"
elif [[ "" != ${GSC_BENCH_SERV} ]]; then
    SERV=${GSC_BENCH_SERV}
else
    SERV="${DEFAULT_SERV}"
fi

addr_file="/tmp/gsc_bench.addr.list"
phrase_file="/tmp/gsc_bench.phrase.list"
target_file="/tmp/gsc_target_list.json"

root_phrase="$(cat static/root.phrase)"

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

    local unit=$[${upper} / 4]
    for i in {0..3}; do
        printf "[" > ${target_file}${i} || die "$0 Line $LINENO"
        for ((k=0;k<${unit};k++)); do
            local addr_idx=$[$k + $i * ${unit}]
            local addr=${target_addr_set[${addr_idx}]}
            printf "[\"${root_phrase}\",\"${addr}\",\"${amount}\"]," >> ${target_file}${i}
        done
        perl -pi -e 's/,$//' ${target_file}${i} || die "$0 Line $LINENO"
        printf "]" >> ${target_file}${i}
    done
}

function run() {
    local contract=$1

    dbg "Start sending ..."

    # 3 async tasks
    for i in {1..3}; do
        if [[ "" == ${contract} ]]; then
            gsc cli transfer -x "${SERV}" -b -D -f ${target_file}${i} 2>/dev/null 1>&2 &
        else
            gsc cli transfer -x "${SERV}" -b -D -f ${target_file}${i} -C $contract 2>/dev/null 1>&2 &
        fi
    done

    # 1 sync task
    if [[ "" == ${contract} ]]; then
        gsc cli transfer -x "${SERV}" -b -D -f "${target_file}0" >/dev/null || log "ERROR $0 Line $LINENO"
    else
        gsc cli transfer -x "${SERV}" -b -D -f "${target_file}0" -C $contract >/dev/null || log "ERROR $0 Line $LINENO"
    fi
}

check_cnt
gen_accounts
prepare_run

if [[ ${SERV} == ${DEFAULT_SERV} ]]; then
    fn dev create -i 1 -f || die "$0 Line $LINENO"
    log "Waiting RPC-server, sleep 6 seconds"
    sleep 6

    phrase_path=/tmp/gsc_bench_root.phrase
    fn dev \
        | jq '.meta.custom_data.bank_account.mnemonic_words' \
        | sed 's/"//g' > $phrase_path || exit 1
    fn setup -O $phrase_path || exit 1
    fn setup -S "http://localhost" || exit 1 # useless, but must set a value

    itv=$(fn dev | jq '.meta.block_interval_in_seconds')

    fn dev init || exit 1
    for i in $(seq 0 6); do
        sleep $itv
    done

    fn contract-deposit --addr $(cat static/root.addr) --amount 1000000000000000 || exit 1
    sleep $itv
    sleep $itv # 2 blocks

    web3_port=$(fn dev | jq '.meta.validator_or_full_nodes."1"."ports"."web3_http_service"')
    export SERV="http://localhost:${web3_port}"
    log "RPC Server endpoints: ${SERV}"
fi

log "\n\nTransfering native token...\n\n"

first_serv=$(echo $SERV | sed 's/,.*//')

curl_post_ret=
curl_post() {
    method=$1
    params=$2
    curl_post_ret=$(curl $first_serv -H 'Content-Type: application/json' -X POST --data "{\"jsonrpc\":\"2.0\",\"method\":\"${method}\",\"params\":[${params}],\"id\":1}" 2>/dev/null)
}

curl_post eth_blockNumber
echo $curl_post_ret
BlockNumber=$(echo $curl_post_ret | jq -c ".result"| sed 's/"//g' | sed 's/0x//')
BlockNumber=$(printf "%d" "0x${BlockNumber}" || die "$0 Line $LINENO ${BlockNumber}")
log "BlockNumber before sending: ${BlockNumber}"

run

sleep 2

let cnter=4;
while :; do
    curl_post eth_getBlockTransactionCountByNumber \"latest\"
    n=$(echo $curl_post_ret | jq -c ".result"| sed 's/"//g' | sed 's/0x//')
    n=$(printf "%d" "0x${n}")
    if [[ 0 -ne $? ]]; then
        break
    elif [[ 0 -eq $n ]]; then
        if [[ 0 == $cnter ]]; then break; else let cnter-=1; fi
    else
        sleep 0.5
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
