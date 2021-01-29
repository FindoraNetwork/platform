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

export LC_ALL=en_US.UTF-8 # perl
export LANGUAGE=en_US.UTF-8 # perl

for file in $(find .. -type f \
    -name "*.rs" \
    -o -name "*.c" \
    -o -name "*.h" \
    -o -name "*.sh" \
    -o -name "*.toml" \
    -o -name "*.json" \
    -o -name "*.md"\
    -o -name "rc.local"\
    | grep -v "$(basename $0)" \
    | grep -v 'target/' \
    | grep -v 'postgres'); do

    perl -p -i -e 's/　/ /g' $file
    perl -p -i -e 's/！/!/g' $file
    perl -p -i -e 's/（/(/g' $file
    perl -p -i -e 's/）/)/g' $file

    perl -p -i -e 's/：/: /g' $file
    perl -p -i -e 's/， */, /g' $file
    perl -p -i -e 's/。 */. /g' $file
    perl -p -i -e 's/、 +/、/g' $file

    perl -p -i -e 's/, +/, /g' $file
    perl -p -i -e 's/\. +/. /g' $file

    perl -p -i -e 's/\t/    /g' $file
    perl -p -i -e 's/ +$//g' $file
done

cd .. && cargo +nightly fmt
