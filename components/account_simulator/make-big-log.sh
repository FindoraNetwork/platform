#!/usr/bin/env bash
exec cargo run --release -- generate -f -c -s 10 1000 1000 1000000 /dev/null >(gzip - >$1)
