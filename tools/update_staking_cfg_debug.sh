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

cargo run --bin staking_cfg_generator || exit 1

cd ../src/ledger/src/staking/init || exit 1

ml_path="${EXEC_PATH}/../src/components/finutils/src/bins/stt/mnemonic_list.const"
echo "[" > $ml_path || exit 1
grep -Ev '=|\[|\]|}|{' staking_config.json.keys | grep -E '( [a-z]+){9,}' >> $ml_path
echo "]" >> $ml_path

tal_path="${EXEC_PATH}/../src/components/finutils/src/bins/stt/td_addr_list.const"
echo "[" > $tal_path || exit 1
grep '"td_addr"' staking_config.json | sed 's/ \+"td_addr": *//g' >> $tal_path
echo "]" >> $tal_path

tal_path_debug_env="${EXEC_PATH}/../src/components/finutils/src/bins/stt/td_addr_list.const.debug_env"
echo "[" > $tal_path_debug_env || exit 1
grep '"td_addr"' staking_config_debug_env.json | sed 's/ \+"td_addr": *//g' >> $tal_path_debug_env
echo "]" >> $tal_path_debug_env

OS=$(uname -s)

if [[ "Linux" == $OS ]]; then
    SED="sed -i"
elif [[ "FreeBSD" == $OS || "Darwin" == $OS ]]; then
    SED="sed -i ''"
else
    echo -e '\033[31;01mUnsupported OS !!\033[00m'
    exit 1
fi

for ((i=1;i<=$(grep -c '"id"' staking_config.json);i++)); do
    idx=$(grep -n '"id"' staking_config_debug_env.json | grep -o '^[0-9]\+' | head -n $i | tail -1)
    $SED "${idx}s/.*/$(grep '"id"' staking_config.json | head -n $i | tail -1)/" staking_config_debug_env.json
done

for ((i=1;i<=$(grep -c '"id"' staking_config.json);i++)); do
    idx=$(grep -n '"id"' staking_config_abci_mock.json | grep -o '^[0-9]\+' | head -n $i | tail -1)
    $SED "${idx}s/.*/$(grep '"id"' staking_config.json | head -n $i | tail -1)/" staking_config_abci_mock.json
done

cd $EXEC_PATH || exit 1
make -C .. build_release_debug
