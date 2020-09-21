# Benchmarking and load testing with locust

## Setup
Install locust using Conda or pip.

Set environment variable LOG_FILE to the file containing transactions. You'll need to acquire a source of transactions in CSV format to run these examples.

## Example: running the replay programs (ledger standalone or testnet). See https://locust.io
`locust -f locust_files/submit_txn.py  --host http://localhost --headless -u 20 -r 1 -t 1h  --csv=batched_16_local`
`locust -f locust_files/submit_txn.py  --host https://staging.findora.org --headless -u 20 -r 1 -t 1h  --csv=batched_16`

We use environment variables to control some aspects of the benchmark.

LOG_FILE: the file we draw the list of transactions to be submit
BATCH_SIZE: the number of posts to submit before doing a force_end_block


