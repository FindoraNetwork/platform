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

MAINNET_0_2_X_VALIDATOR_ID_LIST="$(pwd)/mainnet_0_2_x_validator_id.list" \
    cargo run --bin staking_cfg_generator || exit 1

cd ../src/ledger/src/staking/init || exit 1

echo "[]" > staking_config.json.keys
echo
echo -e "\033[31;1m'Secret info' is invalid in this scene, so we clean it up!\033[0m"
echo

cd $EXEC_PATH || exit 1
make -C ..
