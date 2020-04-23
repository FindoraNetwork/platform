# Benchmarking and load testing with locust

## Setup
Install locust using Conda or pip.

Set environment variable LOG_FILE to the file containing transactions

## Running the programs (ledger standalone or testnet)

`locust --no-web -c 1 -t 20s --host http://localhost -f locust_files/locustfile.py`
`locust --no-web -c 1 -t 20s --host https://testnet.findora.org -f locust_files/locustfile.py`
