# Findora command line tool

Interacting with the Findora blockchain can be done in several ways.
One of them is through the `findora` command line. 

## Installation

In order to build the executable we recommend to use [nix](https://nixos.org/download.html).

* Go to the root of this project
```bash
> cd ../..
> pwd
<some path>/findora/platform
```

* Enter the nix shell
```bash
> nix-shell
> [nix-shell:<some path>/findora/platform]$ 
```

* Build the command line executable
```bash
> cd components/cli2/
> cargo build
```

* Run the command
The executable can be found at `<some path>/findora/platform/target/debug/findora`.
If you are inside the nix-shell simply type `findora`:

```bash
> [nix-shell:<some path>/findora/platform] findora$ 
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
Note that all the transaction will be stored on the testnet chain.

* Key generation

Let us create a key pair for Alice.

```bash
> findora key-gen alice
No config found at "/home/philippe/.findora/cli2_data.sqlite" -- triggering first-time setup
Submission Server? (default=https://testnet.findora.org:8669): 
Ledger Access Server? (default=https://testnet.findora.org:8668): 
Enter password for alice: 
Enter password again:
New key pair added for `alice`
```

Note that the first time you run the command line a configuration file
is created. By default the url of the testnet is provided.

It is Bob's turn now to get his keys.

```
> findora key-gen bob
Enter password for bob: 
Enter password again:
New key pair added for `bob`
```

* Alice defines an asset
```bash
> findora simple-define-asset alice AliceCoin
Saving ledger signing key `"s4KFTBQnGkpCP1kv8PzTI2Y7L72_baf8jUNHh70yxUI="`
New state retrieved.
Submission server: https://testnet.findora.org:8669
Ledger access server: https://testnet.findora.org:8668
Ledger public signing key: "s4KFTBQnGkpCP1kv8PzTI2Y7L72_baf8jUNHh70yxUI="
Ledger state commitment: D17PgKgElHNGrcmqho-vvxAsht6tIMaGMWdlozNhSM0=
Ledger block idx: 2001
Current focused transaction builder: <NONE>
Preparing transaction `vv2WwRwyA9` for block id `2001`...
Done.
Enter password for alice: 
memo?: memo
AliceCoin:
 issuer nickname: alice
 issuer public key: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk="
 code: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
 memo: `memo`
 issue_seq_number: 0
Building `vv2WwRwyA9`
Enter password for alice: 
Built transaction `vv2WwRwyA9`
Submitting to `https://testnet.findora.org:8669/submit_transaction`:
 seq_id: 2001
 Handle: <UNKNOWN>
 Status: <UNKNOWN>
 Operations:
  DefineAsset `AliceCoin`
   issued by `alice`
 New asset types defined:
  AliceCoin:
   issuer nickname: alice
   issuer public key: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk="
   code: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
   memo: `memo`
   issue_seq_number: 0
 New asset records:
 Signers:
  - `alice`
 Consuming TXOs:
Is this correct? (Y/n): Y
Submitted `vv2WwRwyA9`: got handle `bab5127d783a2af620dc4ac51fd8a41030e60d2c41f3dda3785495d517b44357`
Problem parsing response https://testnet.findora.org:8669/txn_status/bab5127d783a2af620dc4ac51fd8a41030e60d2c41f3dda3785495d517b44357, error decoding response body: expected value at line 1 column 1
Retrieve its status? (Y/n): Y
Got status: Ok("{\"Committed\":[2005,[]]}")
Committed!
Updating, status is None
New asset type `AliceCoin`...
Done caching TXOs.
```

A transaction containing the description of the new asset has been created and submitted
to the Findora blockchain. Now Alice can issue some *AliceCoin*.

```bash
> findora simple-issue-asset AliceCoin 10000
Preparing transaction `bibBlifyuN` for block id `2001`...
Done.
Enter password for alice: 
IssueAsset: 10000 of `btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=` (AliceCoin), authorized by `alice`
Successfully added to `bibBlifyuN`
Building `bibBlifyuN`
Enter password for alice: 
Built transaction `bibBlifyuN`
Submitting to `https://testnet.findora.org:8669/submit_transaction`:
 seq_id: 2001
 Handle: <UNKNOWN>
 Status: <UNKNOWN>
 Operations:
  IssueAsset 10000 of `AliceCoin`
   issued to `alice` as issuance #0 named `utxo0`
 New asset types defined:
 New asset records:
  utxo0 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 10000
   Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
   Decrypted Amount: 10000
   Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
   Spent? Unspent
   Have owner memo? No
 Signers:
  - `alice`
 Consuming TXOs:
Is this correct? (Y/n): Y
Submitted `bibBlifyuN`: got handle `d8b86e375be454d7d594e78b710d4f35feb1ea19606f25139c9780018728ca0f`
Retrieve its status? (Y/n): Y
Got status: Ok("{\"Committed\":[2006,[1854]]}")
Committed!
Updating, status is None
Caching TXO `bibBlifyuN:utxo0`:
 sid: 1854
 Owned by: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk=" (alice)
 Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
 Amount: 10000
 Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
 Decrypted Amount: 10000
 Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
 Spent? Unspent
 Have owner memo? No
Done caching TXOs.
```

* Transfer some coins to Bob

Alice now owns the coins she has issued. Next she will transfer some of these coins
to Bob.

A transfer of assets is made in three steps. 
1. Create an empty transaction.
1. Fill the transaction with the information of the transfer.
1. Submit the transaction to the blockchain.

So let us first create an empty transaction.
```bash
> findora initialize-transaction 1
Preparing transaction `1` for block id `2001`...
Done.
```
We assign the number *1* to the transaction so that we can reference it later.

Now let us fill in this transaction.
```bash
> findora transfer-assets --builder=1 
TXOs from this transaction:
Other TXOs:
 bibBlifyuN:utxo0 (SID 1854): 10000 (PUBLIC) of `AliceCoin` (PUBLIC) owned by `alice`
Which input would you like?: 
```
You are asked to pick a transaction output. In this case there is only one available and its identifier
is **bibBlifyuN:utxo0**. So we copy this identifier and press enter.


```bash
> findora transfer-assets --builder=1 
TXOs from this transaction:
Other TXOs:
 bibBlifyuN:utxo0 (SID 1854): 10000 (PUBLIC) of `AliceCoin` (PUBLIC) owned by `alice`
Which input would you like?: bibBlifyuN:utxo0
 Adding bibBlifyuN:utxo0: 10000 (PUBLIC) of `AliceCoin` (PUBLIC) owned by `alice`
Remaining to spend:
 10000 of `AliceCoin`
How much `AliceCoin`? (10000 available): 6000
Secret amount? (Y/n): n
Secret asset type? (Y/n): n
For whom?: bob
Add another output? (Y/n):
```
We then tell the command line to pick 6000 coins for the transfer to Bob.
Note that we chose a non-confidential transfer where the amount and the asset type (AliceCoin)
will be visible by all on the blockchain.

Next we are asked if we want to spend another output. Indeed if Alice does not send
the change (1000-6000=4000) back to her address, she will loose 4000 *AliceCoin*!
So we specify that we want to send the remaining *AliceCoin* back to Alice.

```bash
> findora transfer-assets --builder=1
TXOs from this transaction:
Other TXOs:
 bibBlifyuN:utxo0 (SID 1854): 10000 (PUBLIC) of `AliceCoin` (PUBLIC) owned by `alice`
Which input would you like?: bibBlifyuN:utxo0
 Adding bibBlifyuN:utxo0: 10000 (PUBLIC) of `AliceCoin` (PUBLIC) owned by `alice`
Remaining to spend:
 10000 of `AliceCoin`
How much `AliceCoin`? (10000 available): 6000
Secret amount? (Y/n): n
Secret asset type? (Y/n): n
For whom?: bob
Add another output? (Y/n): Y
Remaining to spend:
 4000 of `AliceCoin`
How much `AliceCoin`? (4000 available): 4000
Secret amount? (Y/n): n
Secret asset type? (Y/n): n
For whom?: alice
Recipient `bob` is a local keypair.
Unlock this output? (Y/n): Y
Enter password for bob: 
Recipient `alice` is a local keypair.
Unlock this output? (Y/n): Y
Enter password for alice: 
Signing for input #1 (bibBlifyuN:utxo0): 10000 (PUBLIC) of `AliceCoin` (PUBLIC) owned by `alice`
Enter password for alice: 
Adding Transfer:
 TransferAssets:
  Inputs:
  bibBlifyuN:utxo0 (Not finalized):
   sid: 1854
   Owned by: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 10000
   Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
   Decrypted Amount: 10000
   Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
   Spent? Unspent
   Have owner memo? No
  Outputs:
  utxo0 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "NUtxD3lc77qO4fZXvXrgG7Nb80gt1Vwr1V9SmybwiOA=" (bob)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 6000
   Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
   Decrypted Amount: 6000
   Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
   Spent? Unspent
   Have owner memo? No
  utxo1 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 4000
   Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
   Decrypted Amount: 4000
   Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
   Spent? Unspent
   Have owner memo? No
Successfully added to `1`
```

The transaction has now all the information we need. Let us
build it and submit it to the network.

```bash
> findora build-transaction  
Building `1`
Built transaction `1`
> findora submit 1
Submitting to `https://testnet.findora.org:8669/submit_transaction`:
 seq_id: 2001
 Handle: <UNKNOWN>
 Status: <UNKNOWN>
 Operations:
  TransferAssets:
   Inputs:
   bibBlifyuN:utxo0 (Not finalized):
    sid: 1854
    Owned by: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk=" (alice)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 10000
    Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
    Decrypted Amount: 10000
    Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
    Spent? Unspent
    Have owner memo? No
   Outputs:
   utxo0 (Not finalized):
    sid: <UNKNOWN>
    Owned by: "NUtxD3lc77qO4fZXvXrgG7Nb80gt1Vwr1V9SmybwiOA=" (bob)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 6000
    Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
    Decrypted Amount: 6000
    Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
    Spent? Unspent
    Have owner memo? No
   utxo1 (Not finalized):
    sid: <UNKNOWN>
    Owned by: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk=" (alice)
    Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
    Amount: 4000
    Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
    Decrypted Amount: 4000
    Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
    Spent? Unspent
    Have owner memo? No
 New asset types defined:
 New asset records:
  utxo0 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "NUtxD3lc77qO4fZXvXrgG7Nb80gt1Vwr1V9SmybwiOA=" (bob)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 6000
   Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
   Decrypted Amount: 6000
   Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
   Spent? Unspent
   Have owner memo? No
  utxo1 (Not finalized):
   sid: <UNKNOWN>
   Owned by: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk=" (alice)
   Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
   Amount: 4000
   Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
   Decrypted Amount: 4000
   Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
   Spent? Unspent
   Have owner memo? No
 Signers:
 Consuming TXOs: bibBlifyuN:utxo0
Is this correct? (Y/n): Y
Submitted `1`: got handle `07bbfcd6c41b84f0bc3f909105d2c715bcdfbb54b3a83ddf6673a987f1c2d430`
Problem parsing response https://testnet.findora.org:8669/txn_status/07bbfcd6c41b84f0bc3f909105d2c715bcdfbb54b3a83ddf6673a987f1c2d430, error decoding response body: expected value at line 1 column 1
Retrieve its status? (Y/n): Y
Got status: Ok("{\"Committed\":[2007,[1855,1856]]}")
Committed!
Updating, status is None
Spending TXO `bibBlifyuN:utxo0`...
Caching TXO `1:utxo0`:
 sid: 1855
 Owned by: "NUtxD3lc77qO4fZXvXrgG7Nb80gt1Vwr1V9SmybwiOA=" (bob)
 Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
 Amount: 6000
 Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
 Decrypted Amount: 6000
 Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
 Spent? Unspent
 Have owner memo? No
Caching TXO `1:utxo1`:
 sid: 1856
 Owned by: "3L8ZRU7bnBr-BBnYnC5NEcM0wKlHwCRgQADDeBePsOk=" (alice)
 Record Type: "NonConfidentialAmount_NonConfidentialAssetType"
 Amount: 4000
 Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c= (AliceCoin)
 Decrypted Amount: 4000
 Decrypted Type: btf0k1_DvMJwib47h5A9IEQJtjx5qzVCE_i8h2hxC_c=
 Spent? Unspent
 Have owner memo? No
Done caching TXOs.
```

Et voilà!

Finally we can check the balances of Bob and Alice after this transaction.

```bash
> findora balances
=== Balances ===
(alice,AliceCoin):4000
(bob,AliceCoin):6000
```
