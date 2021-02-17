# Transaction sample with fee

### 1. set server address

- findora setup
    - http://localhost:8669
    - http://localhost:8668

### 2. create two users

- findora key-gen alice
- findora key-gen bob

### 3. define and issue some FRA

- findora simple-define-asset --is-fra alice FRA
- findora simple-issue-asset FRA 1000

### 4. define and issue some AliceCoin, and pay 1 FRA-unit as fee

- findora initialize-transaction 1
- findora define-asset 1 alice AliceCoin
- findora issue-asset 1 AliceCoin 0 100
- findora transfer-assets --builder=1
    - transfer 1 FRA to the 'fee' user
- findora build-transaction
- findora submit 1

### 4. query the results

- curl 'localhost:26657/tx_search?query="tx.timestamp>0%20AND%20tx.timestamp<999999999999"'
    - [**Cmdline Outputs**](./sample_outputs/tx_search_all.md)

- echo "<the tx body of issuing AliceCoin>" | base64 -d | jq
    - [**Cmdline Outputs**](./sample_outputs/tx_search_last_decoded.md)

- curl 'localhost:8667/get_created_assets/nwA1kSDevBVXNJgM38IHH7OQ0HwfdzWTZGvzWiBX0WM=' | jq
    - [**Cmdline Outputs**](./sample_outputs/get_created_assets.md)

### 5. alice transfer some AliceCoin to bob with encrypted outputs

- findora initialize-transaction 2
- findora transfer-assets --builder=2
    - transfer 1 AliceCoin to the 'bob' user
    - transfer 1 FRA to the 'bob' user
    - transfer 1 FRA to the 'fee' user
- findora build-transaction
- findora submit 2

### 6. query the results

- curl 'localhost:26657/tx_search?query="tx.timestamp>0%20AND%20tx.timestamp<999999999999"'
    - [**Cmdline Outputs**](./sample_outputs/with_secret_tx.md)

- echo "<the new added tx body>" | base64 -d | jq
    - [**Cmdline Outputs**](./sample_outputs/with_secret_tx_decoded.md)

### 7. transfer assets to external address(pubkey)

- load external pubkey first, and do the same operations described above
    ```shell
    findora load-public-key <NickName>
    ```
