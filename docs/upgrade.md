# Version upgrade guide

## 0.1 => 0.2

1. Do `make staking_cfg` in the platform repo
2. Move `platform/src/ledger/src/staking/init/staking_config.json.keys` to a new path out of the repo
    - **VERY IMPORTMANT**: do NOT commit this file to the code base
    - **VERY IMPORTMANT**: do NOT push this file to GitHub!
3. Ask 'community partners' to transfer some amounts of mainnet-FRA to these new addresses in `staking_config.json.keys`
    - From 8000009.1 FRAs to 8190009.1 FRAs, that is '800,0009.1/801,0009.1/802,0009.1/.../819,0009.1'
4. Check the results of the transfer-operations in < step 3 > on mainnet
5. Send a copy of each key to the co-responding owner
6. Commit the changes to the local git repo
7. Create a new release tag
8. Push the new tag to GitHub
9. Wait the CI-compiling work done
10. Cut off the network entrance of mainnet
11. Make a snapshot based on the final data state after the mainnet stops public services
12. Deploy the new compiled version(docker image) to an new environment
    - The new environment should be initialized with the snapshot created in < step 11 >
    - Let's call it 'mainnet-new'
13. Do `stt init --mainet` on 'mainnet-new', the following scenarios will be checked by `stt` automatically
    - The number of validators should be 20
    - The corresponding voting power of each validator should be correct
        - should be equal to `{total balance} - 9(reserved for testing) - 0.1(reserved for fee)`
    - The state of each validator should be `online`(the online/offline mark in `fn show`)
    - All major functions should run well, such as transfer FRAs, custom assets, delegation ...
    - Should be able to create 10 new blocks without any errors after all the above operations
14. Switch the public network entrances between mainnet and mainnet-new
15. Open the network entrance of mainnet-new
    - Keep the old mainnet running for 30 days
    - From now on, Findora Network is a decentralized autonomous network!
