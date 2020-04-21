# Solvency Command Line Interface

# Example of solvency proof and verification
If you need to rerun from the beginning, delete or rename the `solvency_data.json` file to restore the data.
```
rm solvency_data.json
```

## Generate three assets and set their conversion rates to 1, 2 and 300
```
./solvency set_asset_and_rate --rate 1
./solvency set_asset_and_rate --rate 2
./solvency set_asset_and_rate --rate 300
```

## Add assets and liabilities
```
./solvency add_asset_or_liability --type public_asset --id 0 --amount 10
./solvency add_asset_or_liability --type hidden_asset --id 0 --amount 20
./solvency add_asset_or_liability --type hidden_asset --id 1 --amount 30
./solvency add_asset_or_liability --type hidden_asset --id 2 --amount 4
./solvency add_asset_or_liability --type public_liability --id 1 --amount 100
./solvency add_asset_or_liability --type hidden_liability --id 0 --amount 200
./solvency add_asset_or_liability --type hidden_liability --id 1 --amount 300
```
* The total asset amount is 1x10 + 1x20 + 2x30 + 300x4 = 1290.
* The total liability amount is 1x200 + 2x100 + 2x300 = 1000, less than the total asset amount.

## Prove and verify the solvency
```
./solvency prove_and_verify_solvency
```
* Note from the output that the verification succeeded.

## Add additional liabilities
```
./solvency add_asset_or_liability --type hidden_liability --id 2 --amount 1
```
* The total liability amount is now 1000 + 300x1 = 1300, greater than the total asset amount.

## Prove and verify the solvency for the second time
```
./solvency prove_and_verify_solvency
```
* Note from the output that the verification failed.

## Add additional assets
```
./solvency add_asset_or_liability --type public_asset --id 0 --amount 20
```
* The total asset amount is now 1290 + 1x20 = 1310, greater than the total liability amount.

## Prove and verify the solvency for the third time
```
./solvency prove_and_verify_solvency
```
* Note from the output that the verification succeeded again.