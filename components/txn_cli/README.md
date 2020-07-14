# Transaction Builder Command Line Interface

The `txn_cli` application is intended to help one experiment with the RESTful API and access the Ledger from scripts. The `txn_cli` provides a command line interface for constructing and submitting requests to the Ledger. The `txn_cli` application creates transactions and submits them to the ledger server.

The typical workflow of P2P lending is as follows
* Create a new empty transaction. See `txn_cli create_txn_builder`.
* Create new users. See `txn_cli asset_issuer sign_up`, `txn_cli credential_issuer sign_up`, `txn_cli lender sign_up` and `txn_cli borrower sign_up`.
* Borrower: adds or updates a credential record. See `txn_cli borrower create_or_overwrite_credential`.
* Borrower: requests a loan. See `txn_cli borrower request_loan`.
* Lender: fulfills the loan. See `txn_cli lender fulfill_loan`.
* Borrower: loads funds. See `txn_cli borrower load_funds`.
* Borrower: pays off the loan. See `txn_cli borrower pay_loan`.
* Query the ledger with the transaction ID to see if the transaction was committed using a web browser or command line tool.

## Command help

To get documentation on a specific command, use the keyword help
followed by the command. To get help on a subcommand, use help and
the command and subcommand.

For example, for help defining an asset

```
txn_cli asset_issuer define_asset --help
```

**Note**:
* Even if the subcommand is unique, it is still necessary to
supply the command name as well. This is true for both help and the
actual subcommands.
* By default, all the generated files will be stored in `~./findora`, unless specified otherwise. For example, if the current directory is `platform/target/debug`, running `txn_cli keygen` will put the generated key pair in ~./findora, but `txn_cli keygen --name keys/key_pair` will store the key pair to `platform/target/debug/keys/key_pair`.
* Examples below are assuming that `txn_cli` is in the `$PATH`. Typically it resides in either `platform/target/debug` or `platform/target/release`.

## Submit a transaction
After a transaction `tb` is composed by `air_assign`, `define_asset`, `issue_asset`, `transfer_asset`, or `issue_and_transfer_asset`, we can submit it like so:
```
txn_cli --txn tb submit
```
By default, `https://testnet.findora.org` is used. To switch to `http://localhost`, run
```
txn_cli --local --txn tb submit
```

## Asset issuer account
### Sign up an asset issuer account
In the initial data, there's one asset issuer, Izzie. To sign up a new asset issuer account:
```
txn_cli asset_issuer sign_up --name 'Issuer Name'
```

### Store sids to file
If only one utxo sid is needed for an asset transfer, there's no need to use the `store_sids` subcommand. When submitting the asset issuing transaction, simply use `--sids_file` to specify a file to store the utxo sid.

Otherwise, add `--get_sids` when submitting asset issuing transactions, and note the utxo sids shown in the outputs. After all the transactions needed are submitted, store all the utxo sids to one file:
```
txn_cli asset_issuer store_sids --file sids_file --indices 1,2,3
```

### Store asset tracer memo and owner memo
Asset tracer memo and owner memo are necessary for asset tracing. To store them:
```
txn_cli asset_issuer --id 0 store_memo --file memo_file --amount 100 --token_code ibIaBlHV-PdQkvSuEg6YSA==
```
For confidential token amount, add `--confidential_amount`.

### Assign to AIR (Address Identity Registry)
1) Create an empty transaction named `txn_air` in the current directory
```
txn_cli create_txn_builder --name txn_air
```
2) Assign to AIR
```
txn_cli --txn txn_air asset_issuer --id 0 air_assign --address air_key --data 'Some data.'
```

3) Submit the transaction
```
txn_cli --txn txn_air submit
```

### Define an asset
1) Create an empty transaction
```
txn_cli create_txn_builder --name txn_define
```
2) Define an asset
```
txn_cli --txn txn_define asset_issuer --id 0 define_asset --memo 'Define an asset.'
```
By default, a randomly generated token code will be used. To specify a code, use `--token_code`.
To define a fiat asset, add `--fiat`.
To allow update or tracing, add `--updatable` or `--traceable`, respectively.

3) Submit the transaction
```
txn_cli --txn txn_define submit
```

### Issue units of an asset
After an asset is defined and the transaction is submitted:
1) Create an empty transaction
```
txn_cli create_txn_builder --name txn_issue
```
2) Issue the asset (FIXME: this makesdoesn't change `txn_issue`) 
```
txn_cli --txn txn_issue asset_issuer --id 0 issue_asset --token_code ibIaBlHV-PdQkvSuEg6YSA== --amount 100
```
To make the token amount confidential, add `--confidential_amount`.
3) Submit the transaction
```
txn_cli --txn txn_issue submit
```
To display the utxo sids, add `--get_sids`. To store the sids to a file, use `--sids_file`.

### Transfer units of an asset. See `txn_cli add transfer_asset`.
After an asset is defined and issued, transactions are submitted, and utxo sids are stored:
1) Create an empty transaction
```
txn_cli create_txn_builder --name txn_transfer
```
2) Transfer (FIXME: this doesn't change `txn_transfer`) 
After blind asset record and associated memos are stored:
```
txn_cli --txn txn_transfer asset_issuer --id 0 transfer_asset --sids_file s --recipients 0,1 --issuance_txn_files txn_issue --input_amounts 45 --output_amounts 10,35
```
3) Submit the transaction
```
txn_cli --txn txn_transfer submit
```

### Issue and transfer units of an asset
After an asset is defined and the transaction is submitted:
1) Create an empty transaction
```
txn_cli create_txn_builder --name txn_issue_and_transfer
```
2) Issue and transfer the asset
```
txn_cli --txn txn_issue_and_transfer asset_issuer --id 0 issue_and_transfer_asset --recipient 0 --amount 1000 --token_code ibIaBlHV-PdQkvSuEg6YSA==
```
To make the token amount confidential, add `--confidential_amount`.
To store the asset tracer memo and owner memo, use `--memo_file`.
3) Submit the transaction (FIXME: error in check_txn_effects `transaction invalid: Error at: ledger/src/store/store.rs:533:72`)
```
txn_cli --txn txn_issue_and_transfer submit
```
To get the utxo sids, add `--get_sids`.

### Trace and verify an asset
After the asset tracer memo and owner memo are stored by `store_memo` or `issue_and_transfer_asset --memo_file`, trace the asset and verify the token amount.
```
txn_cli asset_issuer --id 0 trace_and_verify_asset --memo_file memos --expected_amount 50
```

## Credential issuer account
### Sign up a credential issuer account
In the initial data, there's one credential issuer, Ivy. To sign up a new credential issuer account:
```
txn_cli credential_issuer sign_up --name 'Issuer Name'
```

## Lender account
### Sign up a lender account
In the initial data, there are two issuers, Lenny and Luna. To sign up a new lender account:
```
txn_cli lender sign_up --name 'Lender Name'
```

### View loans
* View all loans of a lender
```
txn_cli lender --id 0 view_loan
```
* View a specific loan
```
txn_cli lender --id 0 view_loan --loan 0
```
Make sure the specified loan is owned by the lender.
* View loans with a filter
To filter the loans, add `--filter` with one of the following:
  * `requested`: loans that have been requested but not fulfilled
  * `fulfilled`: loans that have been fulfilled, either paid off or not
  * `declined`: loans that have been declined
  * `active`: loans that have been fulfilled but not paid off
  * `complete`: Loans that have been paid off
For example:
```
txn_cli lender --id 0 view_loan --filter active
```

### Fulfill a loan
* Create an empty transaction
```
txn_cli create_txn_builder --name txn_fulfill
```
* Fulfill the loan
```
txn_cli --txn txn_fulfill lender --id 0 fulfill_loan --loan 0 --issuer 0
```
By default, `https://testnet.findora.org` is used. To switch to `http://localhost`, add `--http --localhost`.

### Create or overwrite credential requirement
Currently supported attributes are min_credit_score, min_income and citizenship.
For example, to create a requirement on min_credit_score:
```
txn_cli lender --id 0 create_or_overwrite_requirement --attribute min_credit_score --requirement 650
```
If the requirement already exists, the previous value will be overwritten.

## Borrower account
### Sign up a borrower account
In the initial data, there's one borrower, Ben. To sign up a new borrower account:
```
txn_cli borrower sign_up --name 'Borrower Name'
```

### Load funds
* Create an empty transaction
```
txn_cli create_txn_builder --name txn_load
```
* Load funds
```
txn_cli --txn txn_load borrower --id 0 load_funds --issuer 0 --amount 500
```
By default, `https://testnet.findora.org` is used. To switch to `http://localhost`, add `--http --localhost`.
To store the asset tracer memo and owner memo, use `--memo_file`.

### View loans
* View all loans of a borrower
```
txn_cli borrower --id 0 view_loan
```
* View a specific loan
```
txn_cli borrower --id 0 view_loan --loan 0
```
Make sure the specified loan is owned by the borrower.
* View loans with a filter
To filter the loans, add `--filter` with one of the following:
  * `requested`: loans that have been requested but not fulfilled
  * `fulfilled`: loans that have been fulfilled, either paid off or not
  * `declined`: loans that have been declined
  * `active`: loans that have been fulfilled but not paid off
  * `complete`: Loans that have been paid off
For example:
```
txn_cli borrower --id 0 view_loan --filter active
```

### Request a loan
```
txn_cli borrower --id 0 request_loan --lender 0 --amount 500 --interest_per_mille 80 --duration 5
```

### Pay off a loan
* Create an empty transaction
```
txn_cli create_txn_builder --name txn_pay
```
* Pay off the loan
```
txn_cli --txn txn_pay borrower --id 0 pay_loan --loan 0 --amount 200
```
By default, `https://testnet.findora.org` is used. To switch to `http://localhost`, add `--http --localhost`.

### View credentials
```
txn_cli borrower --id 0 view_credential
```
To viea a specific credential attribute, use `--attribute`.

### Create or overwrite a credential
Currently supported attributes are min_credit_score, min_income and citizenship.
For example, to create a min_credit_score credential:
```
txn_cli borrower --id 0 create_or_overwrite_credential --attribute min_credit_score --value 650
```
If the credential already exists, the original record will be overwritten.

### Get asset record
After the owner memo is stored:
```
txn_cli borrower --id 0 get_asset_record --sid 1 --memo_file m_file
```
By default, `https://testnet.findora.org` is used. To switch to `http://localhost`, add `--http --localhost`.

## Querying the ledger server

The ledger server provides a [RESTful
API](https://en.wikipedia.org/wiki/Representational_state_transfer)
that can be accessed by a web browser or command line tool for making
web requests such as `wget` or `curl`.

```
$ curl https://testnet.findora.org:8669/txn_status/4977619fd7c7dd1c6b917ced37abc718e815a71b3488ece555c8b022286c6974
{"Committed":[0,[]]}
```

### Listing blocks

It is possible to list all the transaction blocks since a given
block sequence number. The `blocks_since` route takes a
block sequence number and returns a JSON expression with all the
transactions in the blocks from the one specified by the sequence
number to the most recent block.

```
$ curl https://testnet.findora.org:8668/blocks_since/0
[[0,[{"txn":{"operations":[{"DefineAsset":{"body":{"asset":{"code":{"val":[241,87,161,27,80,75,66,213,73,161,157,52,166,158,219,106]},"issuer":{"key":"IP26ybELdYe7p7W8FjvOaeeW1x5O1EwQ/LRIhon3oUQ="},"memo":"My asset 1","confidential_memo":null,"updatable":false,"traceable":false}},"pubkey":{"key":"IP26ybELdYe7p7W8FjvOaeeW1x5O1EwQ/LRIhon3oUQ="},"signature":"hwLNqlyHjXOvdHtbUx54bpDr6WhMA31SJMvaUXpYyTTPbInlrBS24uKATxfUAiyRxKRv3vhsw5JFwKCFtLIABw=="}}],"credentials":[],"memos":[]},"tx_id":0,"merkle_id":0}]]]
```

This looks nicer in a web browser that formats JSON nicely such as Firefox.
![Expanding outline](./doc/ledger_json.png)


The `block_log` route returns all the transactions as tabular HTML.

```
$ curl https://testnet.findora.org:8669/block_log
```
![Table of blocks](./doc/block_log.png)

# Example of confidential transfer
## Sign up an asset issuer account for Ian
```
txn_cli asset_issuer sign_up --name Ian
```
Note from the output that Ian's id is `1`: 
```
Ian's id is 1.
```

## Sign up a borrower account for Bill
```
txn_cli borrower sign_up --name Bill
```
Note from the output that Bill's id is `1`:
```
Bill's id is 1.
```

## Ian: defines an asset
### Create an empty transaction
```
txn_cli create_txn_builder --name txn_define
```

### Define an asset
```
txn_cli --txn txn_define asset_issuer --id 1 define_asset --memo 'Define an asset.'
```
Note from the output that the asset token code is `7hAA3TTJQHhDGs-_mpP12Q==`, or `[238, 16, 0, 221, 52, 201, 64, 120, 67, 26, 207, 191, 154, 147, 245, 217]`:
```
Creating asset with token code "7hAA3TTJQHhDGs-_mpP12Q==": [238, 16, 0, 221, 52, 201, 64, 120, 67, 26, 207, 191, 154, 147, 245, 217]
```

### Submit the transaction
```
txn_cli --txn txn_define submit
```

## Ian: issues and transfers the asset confidentially to Bill
### Create an empty transaction
```
txn_cli create_txn_builder --name txn_issue_and_transfer
```

### Issue and transfer the asset
```
txn_cli --txn txn_issue_and_transfer asset_issuer --id 1 issue_and_transfer_asset --recipient 1 --amount 100 --token_code 7hAA3TTJQHhDGs-_mpP12Q== --confidential_amount --memo_file memo
```

### Submit the transaction and get the utxo
```
txn_cli --txn txn_issue_and_transfer submit --get_sids
```
Note from the last line of the output that the utxo is `429`:
```
Utxo: [TxoSID(429)]
```

## Trace and verify the asset
### Bill: verifies the received asset
```
txn_cli borrower --id 1 get_asset_record --sid 429 --memo_file memo
```
Note from the last line of the output that the asset token code is indeed `[238, 16, 0, 221, 52, 201, 64, 120, 67, 26, 207, 191, 154, 147, 245, 217]`, and the amount Bill owns is `100`.
```
Bill owns 100 of asset [238, 16, 0, 221, 52, 201, 64, 120, 67, 26, 207, 191, 154, 147, 245, 217].
```

### Ian: traces the asset transferred to Bill and verifies the amount
```
txn_cli asset_issuer --id 1 trace_and_verify_asset --memo_file memo --expected_amount 100
```
