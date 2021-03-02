# Solvency Command Line Interface
The `solvency_cli` is a command line interface for querying asset and liability information from the ledger and proving the solvency.

Note:
* Examples below are assuming the current directory is `platform/target/debug` or `platform/target/release`. If not, change `./solvency_cli` to the path to `solvency_cli`.

The typical workflow of solvency proof is:
## Set a conversion rate
See `set_rate`. E.g.:
```
./solvency_cli set_rate --code ibIaBlHV-PdQkvSuEg6YSA== --rate 2
```

## Add an asset or liability record
See `add_asset_or_liability`. E.g.:
```
./solvency_cli add_asset_or_liability --type liability --amount 10 --code ibIaBlHV-PdQkvSuEg6YSA== --utxo 1234
```
* `--type` is either asset or liability.
* `--blinds` is required if the asset or liability amount is confidential.
* `--utxo` is the UTXO SID of the transfer transaction.

## Prove and verify the solvency
See `prove_and_verify_solvency`. E.g.:
```
./solvency_cli prove_and_verify_solvency
```
Before proving and verifying the solvency, make sure the associated conversion rates have been set.

# Example of solvency proof and verification
If you need to rerun from the beginning, delete or rename the `solvency_data.json` file to restore the data.
```
rm solvency_data.json
```