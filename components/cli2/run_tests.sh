set -euo pipefail


# Build the executable
# cargo build

# Run clippy
# cargo clippy

# Run the tests
# cargo test

children=()

# Black box tests written in shell
FINDORA_HOME=$(mktemp -d) bats tests/asset-type-escape.sh &
children+=($!)
FINDORA_HOME=$(mktemp -d) bats tests/advanced-cli.sh &
children+=($!)
FINDORA_HOME=$(mktemp -d) bats tests/simple-cli.sh &
children+=($!)
FINDORA_HOME=$(mktemp -d) bats tests/error-handling.sh &
children+=($!)
FINDORA_HOME=$(mktemp -d) bats tests/balances.sh &
children+=($!)
FINDORA_HOME=$(mktemp -d) bats tests/transfers.sh &
children+=($!)

echo ${children[@]}
count=${#children[@]}
echo "$count children"

for i in `seq 1 $count`; do
  if wait -n ${children[@]}; then
    echo 'SUCCESS'
  else
    echo 'FAILURE'
    ret=$?
    for c in ${children[@]}; do
      echo "killing $c..."
      kill $c || true
      sleep 0.01s
      kill -9 $c || true
      echo "killed $c..."
    done
    exit $ret
  fi
done

