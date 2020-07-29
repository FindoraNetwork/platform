# build the executable
cargo build

# cargo unittests
cargo test

#  black box tests written in shell
$BATS tests/hello_world.sh
$BATS tests/cli.sh

