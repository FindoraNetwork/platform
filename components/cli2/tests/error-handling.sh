#!/usr/bin/env bats

source "tests/common.sh"

@test "error handling" {
  run $CLI2 query-asset-type does_not_exist does_not_exist
  [ "$status" -eq 101 ]  # Should be 0
}