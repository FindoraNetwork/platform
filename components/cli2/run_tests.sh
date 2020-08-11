set -euo pipefail

export FINDORA_HOME=$(mktemp -d)
echo "FINDORA_HOME: ${FINDORA_HOME}"

# build the executable
cargo build

# Run the tests
# cargo test

#  black box tests written in shell
#bats tests/advanced-cli.sh
bats tests/simple-cli.sh
bats tests/error-handling.sh
