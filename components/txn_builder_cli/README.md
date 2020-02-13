# Transaction Builder Command Line Interface

The `txn_builder_cli` application creates transactions and submits
them to the ledger server. The typical workflow is as follows
* Generate a cryptographic key pair. Just once. See `txn_builder_cli keygen`
* Create a new empty transaction. See `txn_builder_cli create`.
* Add operations to the transaction. See `txn_builder_cli add`.
* Submit the transaction to the ledger. See `txn_builder_cli submit`
  and note the transaction ID reported.
* Query the ledger with the transaction ID to see if the transaction
  was committed using a web browser or command line tool.

## Command help

To get documentation on a specific command, use the keyword help
followed by the the command. To get help on a subcommand, use help and
the command and subcommand.

For example, for help defining an asset

```
./txn_builder_cli help add define_asset
```

**Note**:
* Even if the subcommand is unique, it is still necessary to
supply the command name as well. This is true for both help and the
actual subcommands.
* By default, all the generated files will be stored in ```~./findora```, unless specified otherwise. For example, if the current directory is ```platform/target/debug```, running ```./txn_builder_cli keygen``` will put the generated key pair in ~./findora, but ```./txn_builder_cli keygen --name keys/key_pair``` will store the key pair to ```platform/target/debug/keys/key_pair```.
* Examples below are assuming the current directory is ```platform/target/debug```. If not, change ```./txn_builder_cli``` to the path to ```./txn_builder_cli```.

## Generate a key pair
Before composing a transaction, generate and save a cryptographic key pair.
```
./txn_builder_cli keygen --name kp
```

## Composing a transaction

### Create an empty transaction
```
./txn_builder_cli create --name tb
```

### Add operations to the transaction. Three operations can be added:
* Define a new asset. See `txn_builder_cli add define_asset`.
```
./txn_builder_cli --txn tb --key_pair kp add define_asset --token_code ibIaBlHV-PdQkvSuEg6YSA== --memo 'define an asset'
```
* Issue units of an asset. See `txn_builder_cli add issue_asset`.
```
./txn_builder_cli --txn tb --key_pair kp add issue_asset --token_code ibIaBlHV-PdQkvSuEg6YSA== --sequence_number 1 --amount 100
```
* Transfer units of an asset. See `txn_builder_cli add transfer_asset`.
  * Create input and output public keys
  ```
  ./txn_builder_cli pubkeygen --name pki1
  ./txn_builder_cli pubkeygen --name pki2
  ./txn_builder_cli pubkeygen --name pko1
  ./txn_builder_cli pubkeygen --name pko2
  ./txn_builder_cli pubkeygen --name pko3
  ```
  * Store sids and blind asset records
  ```
  ./txn_builder_cli store sids --path s --indices 2,4
  ./txn_builder_cli store blind_asset_record --path bar1 --amount 100 --asset_type ibIaBlHV-PdQkvSuEg6YSA== --pub_key_path pki1
  ./txn_builder_cli store blind_asset_record --path bar2 --amount 1000 --asset_type ibIaBlHV-PdQkvSuEg6YSA== --pub_key_path pki2
  ```
  * Transfer
  ```
  ./txn_builder_cli --txn tb --key_pair kp add transfer_asset --sids_path s --blind_asset_record_paths bar1,bar2 --input_amounts 15,45 --output_amounts 10,20,30 --address_paths pko1,pko2,pko3
  ```

## Submitting a transaction
After a transaction is composed:
```
./txn_builder_cli --txn tb --key_pair kp submit
```

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


The `block_log` route returns all the transaction as tabular HTML.

```
$ curl https://testnet.findora.org:8669/block_log
```
![Table of blocks](./doc/block_log.png)


