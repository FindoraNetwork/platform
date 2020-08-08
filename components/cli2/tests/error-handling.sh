#!/usr/bin/env bats

source "tests/common.sh"

@test "error handling" {
  run $CLI2 query-asset-type does_not_exist does_not_exist
  debug_lines
  echo "STATUS: $status"
  [ "$status" -eq 1 ]
}