# Development log

## Change plan

> Features to be added.

#### v0.3.x

**plan to launch at November 2021**

- Smart contact, and so on

## Change log

> Functions that have been added.

#### v0.2.4-release

- Support generating offline transactions

#### v0.2.3-release

- Adjust minimal staking amount
- Avoid punish unstaked validator and its delegators
- Upgrade rust to the 2021 edition

#### v0.2.2-release

- Fix a BUG in the logic of some special partial undelegations
    - [Issue 75, #75](https://github.com/FindoraNetwork/platform/issues/75)
- Fix a BUG about the voting power in the logic of un-delegation
- Fix some issues in the history-style API about POS
- optimize the usage of 'bnc'
- Optimize ABCI checker
    - Avoid invalid transactions from being stored
- Add balance checker for coinbase
    - Avoid wrong rewards when the reward pool is empty
- Enhance stability by using seed nodes in `findorad init`

#### v0.2.1-release (Yanked !)

- Fix a BUG in delegation logic
    - [Issue 65, #65](https://github.com/FindoraNetwork/platform/issues/65)

#### v0.2.0-release (Yanked !)

- POS function added
- Code optimization
- Stability enhancement

#### v0.1.0-release (Yanked !)

**launched at April 2021**

- Transfer function with privacy attributes
