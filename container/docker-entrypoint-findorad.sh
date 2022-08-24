#!/bin/bash
set -e
cat /var/checkpoint.toml
exec findorad "$@"
