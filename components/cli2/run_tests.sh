set -euo pipefail

export FINDORA_HOME=$(mktemp -d)
echo "FINDORA_HOME: ${FINDORA_HOME}"

# Build the executable
# cargo build

# Run clippy
# cargo clippy

# Run the tests
# cargo test

# Black box tests written in shell
bats tests/advanced-cli.sh
bats tests/simple-cli.sh
bats tests/error-handling.sh
bats tests/balances.sh
bats tests/transfers.sh
