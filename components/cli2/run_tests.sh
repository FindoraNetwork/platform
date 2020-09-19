set -euo pipefail


# Build the executable
# cargo build

# Run clippy
# cargo clippy

# Run the tests
# cargo test

# Black box tests written in shell
{
  echo tests/asset-type-escape.sh;
  echo tests/big-transaction.sh;
  echo tests/advanced-cli.sh;
  echo tests/simple-cli.sh;
  echo tests/error-handling.sh;
  echo tests/balances.sh;
  echo tests/transfers.sh;
} | parallel bats {}

