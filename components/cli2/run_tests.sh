set -euo pipefail

export FINDORA_HOME=$(mktemp -d)
echo "FINDORA_HOME: ${FINDORA_HOME}"

# build the executable
cargo build

# cargo unittests
cargo test

#  black box tests written in shell
$BATS tests/advanced-cli.sh
$BATS tests/simple-cli.sh
