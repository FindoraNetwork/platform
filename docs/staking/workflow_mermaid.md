# Staking Work Flow

> For simplicity, all the flowcharts and timing diagrams in this article will not show the logics about non-essential nodes, such as SeedNode, SentryNode, etc.

## Macro View

### \<Timing Diagrams No.1\>: Delegation

```mermaid
sequenceDiagram
  participant d as Delegator (User)
  participant w as Wallet
  participant f as FullNode
  participant v as Validator[s]

  Note over f,v: [A] an existing blockchain system

  loop P2P consensus
    v -->> v: verify, vote, reward, slash, pay ...
  end

  loop block sync
    f -->> v: info
    v -->> f: block[s]
  end

  Note over d,v: [B] delegation<br/>create a new one or append to an existing one

  alt balance insufficient
  d -->> d: buy FRAs
  d -->> w: #
  end

  w ->> f: send 'Delegation' tx[s]<br/>{ carry in: principals, fee }

  opt check tx
    f -->> f: #
  end

  f ->> v: forward tx
  v -->> f: block sync

  loop result confirmation
    w -->> f: req
    f -->> w: resp
  end

  w ->> d: delegation confirmed

  Note over d,v: [C] query infomation<br/>the principal may decrease due to slash, but the rewards may only increase

  d ->> w: check wallet
  w ->> f: req
  f ->> w: resp
  w ->> d: real-time rewards

  Note over d,v: [D] claim rewards

  d ->> w: check wallet
  w ->> f: send 'Claim' tx[s]<br/>{ carry in: fee }

opt check tx
  f -->> f: #
end

  f ->> v: forward tx
  v -->> f: block sync

  loop result confirmation
    w -->> f: req
    f -->> w: resp
  end

  w ->> d: claim confirmed<br/>{ carry in: requested rewards }

  Note over d,v: [E] undelegation

  d ->> w: check wallet
  w ->> f: send 'UnDelegation' tx<br/>{ carry in: fee }
  opt check tx
    f -->> f: #
  end
  f ->> v: forward tx
  v -->> f: block sync

  loop result confirmation
    w -->> f: req
    f -->> w: resp
  end

  w ->> d: undelegation confirmed<br/>

  loop wait 21 days
    d -->> d: #
    Note over d: delegator can still get delegation rewards during this period
  end

  w ->> d: got paid from CoinBase<br/>{ carry in: principals, all rewards }
```

### \<Timing Diagrams No.2\> Staking(delegate as a new validator)

> **NOTE**: '**......**' means that the logic there is exactly the same as the delegation process, so the related description will not be repeated.

```mermaid
sequenceDiagram
   participant n as NewValidator
   participant d as Staker(User)
   participant w as CmdLine
   participant f as FullNode
   participant v as ExistingValidator[s]

  Note over f,v: [A] an existing blockchain system<br/>......

  Note over n,v: [B.1] staking<br/>staking is essentially an advanced version of delegation,<br/>the only difference is that it adds a new validator structure to the 'Deleteagion' tx.<br/>in addition, repeated staking is not allowed,<br/>an existing staker can increase his voting power by the basic version of delegation.

  alt balance insufficient
  d -->> d: buy FRAs
  d -->> w: #
  end

  w ->> f: send an advanced verion of 'Delegation' tx[s]<br/>{ carry in: principals, fee }<br/>{ carry in: 'NewValidator' structure }
  opt check tx
    f -->> f: #
  end
  f ->> v: forward tx
  v -->> f: block sync

  loop result confirmation
    w -->> f: req
    f -->> w: resp
  end

  w ->> d: delegation confirmed

  Note over n,v: [B.2] 'NewValidator' join as a new candidate validator

  alt power ranks within the top 50
    alt online
      n -->> v: participate in network consensus
      v -->> n: get validator rewards
    else offline
        v -->> n: be slashed
    end
  end

  Note over n,v: [C] query infomation<br/>......

  Note over n,v: [D] claim rewards<br/>......

  Note over n,v: [E] unstaking

  d ->> w: check CmdLine(or Wallet)
  w ->> f: send 'UnDelegation' tx<br/>{ carry in: fee }
  opt check tx
    f -->> f: #
  end
  f ->> v: forward tx
  v -->> f: block sync

  loop result confirmation
    w -->> f: req
    f -->> w: resp
  end

  w ->> d: undelegation confirmed<br/>

  loop update validator set
    v ->> v: #
    Note over v: 'NewValidator' will be removed automatically,<br/>because its power has been decreased to zero
  end

  loop wait 21 days
    d -->> d: #
    Note over d: staker can still get delegation rewards during this period
  end

  w ->> d: got paid from CoinBase<br/>{ carry in: principals, all rewards }

```

### \<Flowchart No.1\> Deployment

- **TODO**

### Inner Details

- **TODO**
