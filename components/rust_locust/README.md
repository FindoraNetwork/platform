# Benchmarking and load testing with locust

## Setup
Install locust using Conda or pip.

Set environment variable LOG_FILE to the file containing transactions. You'll need to acquire a source of transactions in CSV format to run these

## Running the replay programs (ledger standalone or testnet)

`locust --no-web -c 1 -t 20s --host http://localhost -f scripts/locustfile.py`
`locust --no-web -c 1 -t 20s --host https://testnet.findora.org -f scripts/locustfile.py`

## Running the AIR test gen

`locust --no-web -c 1 -t 20s --host http://localhost -f scripts/air_test.py`

