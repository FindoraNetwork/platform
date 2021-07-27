# Staking Development Progress

## Application Level

### Functions

- [x] delegation
  - [x] increase delegation at any time
  - [x] delegate to multiple different validators
- [x] undelegation
  - [x] undelegate at any time
  - [x] N days frozen time after proposing a undelegation
  - partial undelegation ( is this necessary? )
- [x] claim
  - [x] claim rewards at any time
  - [x] partial claim

### User Interaction

- [x] support in command line
- [x] support in wallet

## Consensus Level

### Functions

- [x] validator management
  - [x] dynamic validator list
  - [x] dynamic voting power
  - [x] support staking validators
  - [x] support staking commission rate
  - [x] support initial validators based on multi-signature
- [x] governance
  - [x] on-chain governance
    - [x] duplicate vote, auto-detected by tendermint
    - [x] light client attack, auto-detected by tendermint
    - [x] offline, aka unavailable
    - other more
  - [x] off-chain governance
    - [x] slash consensus based on multi-signature
  - [x] validator slash
    - [x] support principal slash
    - [x] support rewards slash
  - [x] delegator slash
    - [x] support principal slash
    - [x] support rewards slash
- [x] FRA distribution
  - [x] automated distribution
  - [x] coinbase(fake) implementation
- dynamic fee (out of plan)

### User Interaction

- [x] support in command line
- support in wallet (seems not necessary)
