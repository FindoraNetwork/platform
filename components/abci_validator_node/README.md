# Tendermint consensus integration
The Findora ledger, replicated on multiple consensus nodes each with the same RESTful APIs and local state as a standalone ledger, can be run with the tendermint consensus protocol.
* To set up, use https://github.com/findoraorg/service-dockers/tree/master/tendermint_validator

You should be able to use the scripts that Mat created (thanks, Mat!) to control it, e.g.
```
./manage-tendermint init
./manage-tendermint start
```
If you are testing a locally built intstance, you'll want to put your version of `abci_validator_node` in each container.
