set -euo pipefail

export FINDORA_HOME=$(mktemp -d)
echo "FINDORA_HOME: ${FINDORA_HOME}"

# build the executable
cargo build

# cargo unittests
cargo test

#  black box tests written in shell
$BATS tests/hello_world.sh
$BATS tests/cli.sh

rm -d $FINDORA_HOME || {
  rm $FINDORA_HOME/cli2_data.sqlite;
  rm -d $FINDORA_HOME;
}

