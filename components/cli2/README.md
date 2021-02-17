# Findora command line tool

Interacting with the Findora blockchain can be done in several ways.
One of them is through the `findora` command line.

> **SEE ALSO**
>
> - [**Another Example**](./sample.md), may be clearer and easier to read

## Installation

* Go to the root of this project
```shell
> pwd
<some path>/findora/platform
```

* Build the command line executable
```shell
> cd components/cli2/
> cargo build
```

* Configure autocompletion

It is possible to configure your shell so that the command line autocompletes the arguments /
options provided. Bash, elvish, fish and powershell are currently supported.
For example if you run a bash shell do as follows:
```
> findora gen-completions --bash > fin-completion.sh
> source fin-completion.sh
```

* Run the command

The executable can be found at `<some path>/findora/platform/target/debug/findora`.
If you are inside the nix-shell simply type `findora`:

```shell
> findora
Build and manage transactions and assets on a findora ledger

USAGE:
    findora <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    balances                   Display the amounts available for each asset types and key pair
    build-transaction          Finalize the current transaction, preparing it for submission
    define-asset               Create the definition of an asset and put it in a transaction builder
    delete-keypair             Permanently delete the key pair for <nick>
    delete-public-key          Permanently delete the public key for <nick>
    gen-completions            Generate bash/zsh/fish/powershell completion files for this CLI
    help                       Prints this message or the help of the given subcommand(s)
    ...
```

## Example

Now the executable is installed, let us see how to use the command line with a simple example.
First we will create two key pairs, one for Alice and one for Bob.
Then Alice will define and issue a token called *AliceCoin*.
Finally Alice will tranfer 1000 *AliceCoin* to Bob.

* Set server address

```shell
> findora setup
Submission Server? (default=https://testnet-new.findora.org:8669): http://localhost:8669
Ledger Access Server? (default=https://testnet-new.findora.org:8668): http://localhost:8668
```

* Key generation

Let us create a key pair for Alice.

```shell
> findora key-gen alice
Enter password for alice:
Enter password again:
New key pair added for `alice`
```

It is Bob's turn now to get his keys.

```
> findora key-gen bob
Enter password for bob:
Enter password again:
New key pair added for `bob`
```

* Alice defines FRA

```shell
> findora simple-define-asset --is-fra alice FRA
Saving ledger signing key '"KR3ROBJ79e_QWMh928BAIfL-n13mMzHcA157xR86d18="'
New state retrieved.
Submission server: http://localhost:8669
Ledger access server: http://localhost:8668
Ledger public signing key: "KR3ROBJ79e_QWMh928BAIfL-n13mMzHcA157xR86d18="
Ledger state commitment: dCNOmK_nSY-12vHzasLXiswzlGT5UHA7jAGYkvmCuQs=
Ledger block idx: 0
Current focused transaction builder: zC66WGgoC0
Directory of wallet: /home/fh/.findora
Preparing transaction '98PJe4plBs' for block id '0'...
Done.
Enter password for alice:
max units? (default=unlimited): 88888888
memo updatable? (y/N): y
memo?: FRA
FRA:
 issuer nickname: alice
 issuer public key: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk="
 code: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
 memo: 'FRA'
 issue_seq_number: 0
Building '98PJe4plBs'
Enter password for alice:
Built transaction '98PJe4plBs'
Submitting to 'http://localhost:8669/submit_transaction':
 seq_id: 0
 Handle: <UNKNOWN>
 Status: <UNKNOWN>
 Operations:
  DefineAsset 'FRA'
   issued by 'alice'
 New asset types defined:
  FRA:
   issuer nickname: alice
   issuer public key: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk="
   code: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
   memo: 'FRA'
   issue_seq_number: 0
 New asset records:
 Signers:
  - 'alice'
 Consuming TXOs:
Is this correct? (Y/n): y
Submitted '98PJe4plBs': got handle '312a41095642ebfba5a87b88c7d44b5bd317a424d6aef83281d559d7e0b0670f'
Retrieve its status? (Y/n): y
Got status: Ok("{\"Committed\":[0,[]]}")
Committed!
Updating, status is None
New asset type 'FRA'...
Done caching TXOs.
```

* Alice issues 1000 FRA-units

```shell
> findora simple-issue-asset FRA 1000
Preparing transaction 'kvDZnUiEhr' for block id '0'...
Done.
Enter password for alice:
IssueAsset: 1000 of 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=' (FRA), authorized by 'alice'
Successfully added to 'kvDZnUiEhr'
Building 'kvDZnUiEhr'
Enter password for alice:
Built transaction 'kvDZnUiEhr'
Submitting to 'http://localhost:8669/submit_transaction':
 seq_id: 0
 Handle: <UNKNOWN>
 Status: <UNKNOWN>
 Operations:
  IssueAsset 1000 of 'FRA'
   issued to 'alice' as issuance #0 named 'utxo0'
 New asset types defined:
 New asset records:
  utxo0 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 1000
   Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
   Decrypted Amount: 1000
   Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
   Spent? Unspent
   Have owner memo? No
 Signers:
  - 'alice'
 Consuming TXOs:
Is this correct? (Y/n): y
Submitted 'kvDZnUiEhr': got handle '9b89a15e4b5137beff8a6cacdb7f90336ca83c1027396b360239b09d4c7ba3b5'
Retrieve its status? (Y/n): y
Got status: Ok("{\"Committed\":[1,[0]]}")
Committed!
Updating, status is None
Caching TXO 'kvDZnUiEhr:utxo0':
 sid: 0
 Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
 Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
 Amount: 1000
 Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
 Decrypted Amount: 1000
 Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
 Spent? Unspent
 Have owner memo? No
Done caching TXOs.
```

> **Fee is paid in the form of a normal `TransferAssets Operation` !**
>
> A transfer of assets is made in three steps.
> 1. Create an empty transaction.
> 2. Fill the transaction with the information of the transfer(include an operation for fee).
> 3. Submit the transaction to the blockchain.

* Later operations of Alice
    - defines a custom asset named AliceCoin
    - issue some AliceCoin
    - transfer some AliceCoin and FRA to bob
    - pay 1 FRA-unit to 'fee'(a pre-defined user)

```shell
> findora initialize-transaction 1
Preparing transaction '1' for block id '0'...
Done.

> findora define-asset 1 alice AliceCoin
Enter password for alice:
max units? (default=unlimited): 10000000
memo updatable? (y/N): y
memo?: AliceCoin
AliceCoin:
 issuer nickname: alice
 issuer public key: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk="
 code: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
 memo: 'AliceCoin'
 issue_seq_number: 0

> findora issue-asset 1 AliceCoin 0 10000
Enter password for alice:
IssueAsset: 10000 of '56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=' (AliceCoin), authorized by 'alice'
Successfully added to '1'

> findora transfer-assets --builder=1 # transfer fee / FRA / AliceCoin
TXOs from this transaction:
 utxo0: 10000 (PUBLIC) of 'AliceCoin' (PUBLIC) owned by 'alice'
Other TXOs:
 kvDZnUiEhr:utxo0: 1000 (PUBLIC) of 'FRA' (PUBLIC) owned by 'alice'
Which input would you like?: utxo0
 Adding utxo0: 10000 (PUBLIC) of 'AliceCoin' (PUBLIC) owned by 'alice'
Add another input? (Y/n): y
TXOs from this transaction:
Other TXOs:
 kvDZnUiEhr:utxo0: 1000 (PUBLIC) of 'FRA' (PUBLIC) owned by 'alice'
Which input would you like?: kvDZnUiEhr:utxo0
 Adding kvDZnUiEhr:utxo0: 1000 (PUBLIC) of 'FRA' (PUBLIC) owned by 'alice'
Remaining to spend:
 10000 of 'AliceCoin'
 1000 of 'FRA'
Which type of output?: FRA
How much 'FRA'? (1000 available): 1
Secret amount? (Y/n): n
Secret asset type? (Y/n): n
For whom?: fee
Add another output? (Y/n): y
Remaining to spend:
 10000 of 'AliceCoin'
 999 of 'FRA'
Which type of output?: FRA
How much 'FRA'? (999 available): 99
Secret amount? (Y/n): n
Secret asset type? (Y/n): n
For whom?: bob
Add another output? (Y/n): y
Remaining to spend:
 10000 of 'AliceCoin'
 900 of 'FRA'
Which type of output?: FRA
How much 'FRA'? (900 available): 900
Secret amount? (Y/n): n
Secret asset type? (Y/n): n
For whom?: alice
Add another output? (Y/n): y
Remaining to spend:
 10000 of 'AliceCoin'
How much 'AliceCoin'? (10000 available): 5000
Secret amount? (Y/n): n
Secret asset type? (Y/n): n
For whom?: bob
Add another output? (Y/n): y
Remaining to spend:
 5000 of 'AliceCoin'
How much 'AliceCoin'? (5000 available): 5000
Secret amount? (Y/n): n
Secret asset type? (Y/n): n
For whom?: alice
Recipient 'bob' is a local keypair.
Unlock this output? (Y/n): y
Enter password for bob:
Recipient 'alice' is a local keypair.
Unlock this output? (Y/n): y
Enter password for alice:
Recipient 'bob' is a local keypair.
Unlock this output? (Y/n): y
Enter password for bob:
Recipient 'alice' is a local keypair.
Unlock this output? (Y/n): y
Enter password for alice:
Signing for input #1 (utxo0): 10000 (PUBLIC) of 'AliceCoin' (PUBLIC) owned by 'alice'
Enter password for alice:
Signing for input #2 (kvDZnUiEhr:utxo0): 1000 (PUBLIC) of 'FRA' (PUBLIC) owned by 'alice'
'alice' has already signed.
Adding Transfer:
 TransferAssets:
  Inputs:
  utxo0 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 10000
   Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
   Decrypted Amount: 10000
   Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
   Spent? Unspent
   Have owner memo? No
  kvDZnUiEhr:utxo0 (Not finalized):
   sid: 0
   Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 1000
   Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
   Decrypted Amount: 1000
   Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
   Spent? Unspent
   Have owner memo? No
  Outputs:
  utxo1 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" (fee)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 1
   Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
   Spent? Unspent
   Have owner memo? No
  utxo2 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "chAqPNO5Uw7ppfoK9ShCfWL44h3m0osW4TZrHtSpDkM=" (bob)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 99
   Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
   Decrypted Amount: 99
   Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
   Spent? Unspent
   Have owner memo? No
  utxo3 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 900
   Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
   Decrypted Amount: 900
   Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
   Spent? Unspent
   Have owner memo? No
  utxo4 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "chAqPNO5Uw7ppfoK9ShCfWL44h3m0osW4TZrHtSpDkM=" (bob)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 5000
   Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
   Decrypted Amount: 5000
   Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
   Spent? Unspent
   Have owner memo? No
  utxo5 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 5000
   Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
   Decrypted Amount: 5000
   Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
   Spent? Unspent
   Have owner memo? No
Successfully added to '1'

> findora build-transaction
Building '1'
Enter password for alice:
Built transaction '1'

> findora submit 1
Submitting to 'http://localhost:8669/submit_transaction':
 seq_id: 0
 Handle: <UNKNOWN>
 Status: <UNKNOWN>
 Operations:
  DefineAsset 'AliceCoin'
   issued by 'alice'
  IssueAsset 10000 of 'AliceCoin'
   issued to 'alice' as issuance #0 named 'utxo0'
  TransferAssets:
   Inputs:
   utxo0 (Not finalized):
    sid: <UNKNOWN>
    Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 10000
    Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
    Decrypted Amount: 10000
    Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
    Spent? Unspent
    Have owner memo? No
   kvDZnUiEhr:utxo0 (Not finalized):
    sid: 0
    Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 1000
    Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
    Decrypted Amount: 1000
    Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
    Spent? Unspent
    Have owner memo? No
   Outputs:
   utxo1 (Not finalized):
    sid: <UNKNOWN>
    Owned by: "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" (fee)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 1
    Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
    Spent? Unspent
    Have owner memo? No
   utxo2 (Not finalized):
    sid: <UNKNOWN>
    Owned by: "chAqPNO5Uw7ppfoK9ShCfWL44h3m0osW4TZrHtSpDkM=" (bob)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 99
    Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
    Decrypted Amount: 99
    Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
    Spent? Unspent
    Have owner memo? No
   utxo3 (Not finalized):
    sid: <UNKNOWN>
    Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 900
    Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
    Decrypted Amount: 900
    Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
    Spent? Unspent
    Have owner memo? No
   utxo4 (Not finalized):
    sid: <UNKNOWN>
    Owned by: "chAqPNO5Uw7ppfoK9ShCfWL44h3m0osW4TZrHtSpDkM=" (bob)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 5000
    Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
    Decrypted Amount: 5000
    Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
    Spent? Unspent
    Have owner memo? No
   utxo5 (Not finalized):
    sid: <UNKNOWN>
    Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 5000
    Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
    Decrypted Amount: 5000
    Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
    Spent? Unspent
    Have owner memo? No
 New asset types defined:
  AliceCoin:
   issuer nickname: alice
   issuer public key: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk="
   code: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
   memo: 'AliceCoin'
   issue_seq_number: 0
 New asset records:
  utxo0 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 10000
   Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
   Decrypted Amount: 10000
   Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
   Spent? Spent
   Have owner memo? No
  utxo1 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" (fee)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 1
   Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
   Spent? Unspent
   Have owner memo? No
  utxo2 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "chAqPNO5Uw7ppfoK9ShCfWL44h3m0osW4TZrHtSpDkM=" (bob)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 99
   Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
   Decrypted Amount: 99
   Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
   Spent? Unspent
   Have owner memo? No
  utxo3 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 900
   Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
   Decrypted Amount: 900
   Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
   Spent? Unspent
   Have owner memo? No
  utxo4 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "chAqPNO5Uw7ppfoK9ShCfWL44h3m0osW4TZrHtSpDkM=" (bob)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 5000
   Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
   Decrypted Amount: 5000
   Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
   Spent? Unspent
   Have owner memo? No
  utxo5 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 5000
   Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
   Decrypted Amount: 5000
   Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
   Spent? Unspent
   Have owner memo? No
 Signers:
  - 'alice'
 Consuming TXOs: kvDZnUiEhr:utxo0
Is this correct? (Y/n): y
Submitted '1': got handle 'c3165ac81ad90bb47173e0e3aee7ab8d5dc27bfec47bf7d11f3becd88019d0f2'
Retrieve its status? (Y/n): y
Got status: Ok("{\"Committed\":[2,[2,3,4,5,6]]}")
Committed!
Updating, status is None
Spending TXO 'kvDZnUiEhr:utxo0'...
New asset type 'AliceCoin'...
Caching TXO '1:utxo1':
 sid: 2
 Owned by: "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" (fee)
 Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
 Amount: 1
 Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
 Spent? Unspent
 Have owner memo? No
Caching TXO '1:utxo2':
 sid: 3
 Owned by: "chAqPNO5Uw7ppfoK9ShCfWL44h3m0osW4TZrHtSpDkM=" (bob)
 Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
 Amount: 99
 Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
 Decrypted Amount: 99
 Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
 Spent? Unspent
 Have owner memo? No
Caching TXO '1:utxo3':
 sid: 4
 Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
 Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
 Amount: 900
 Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (FRA)
 Decrypted Amount: 900
 Decrypted Type: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
 Spent? Unspent
 Have owner memo? No
Caching TXO '1:utxo4':
 sid: 5
 Owned by: "chAqPNO5Uw7ppfoK9ShCfWL44h3m0osW4TZrHtSpDkM=" (bob)
 Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
 Amount: 5000
 Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
 Decrypted Amount: 5000
 Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
 Spent? Unspent
 Have owner memo? No
Caching TXO '1:utxo5':
 sid: 6
 Owned by: "_-befD4bvhn4oKkNIzZ8RIQjsmgQZY8TV2KGKnDIIsk=" (alice)
 Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
 Amount: 5000
 Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI= (AliceCoin)
 Decrypted Amount: 5000
 Decrypted Type: 56Ky6MkHAmq_T4isuDkECO11J-dUOUNB0n3CbBw4RcI=
 Spent? Unspent
 Have owner memo? No
Done caching TXOs.
```

Et voilà!

Finally we can check the balances of Bob and Alice after this transaction.

```shell
> findora balances
=== Balances ===
(alice,AliceCoin):5000
(alice,FRA):900
(bob,AliceCoin):5000
(bob,FRA):99
```
