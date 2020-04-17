# Benchmarking and load testing with locust

## Setup
Install locust using Conda or pip.

## Running the programs (ledger standalone or testnet)

`locust --no-web -c 1 -t 20s --host http://localhost -f locust_files/locustfile.py`
`locust --no-web -c 1 -t 20s --host https://testnet.findora.org -f locust_files/locustfile.py`
