### `lib.rs`


---

### `net.rs` 
exposes methods to interact with the RPC APIs under the net_ namespace.
* `version` => `web3.net.version`  
Returns the current network id (523).

* `peer_count` => `web3.net.peer_count`  
Returns number of peers currently connected to the client.

* `is_listening` => `web3.net.listening`  
Returns true if client is actively listening for network connections.

---

### `web3.rs` 
exposes the following APIs.  

* `client_version` => `web3.clientVersion`  
Returns the current client version.  

* `sha3` => `web3.sha3(primitive=None, hexstr=None, text=None)`  
Returns the Keccak SHA256 of the given value.  

---

### `eth.rs`
exposes the following properties and methods to interact with the RPC APIs under the eth_ namespace.

---

### `eth_pubsub.rs`  
* `subscribe`  
subscribe to specific events in the blockchain.  

* `unsubscribe`  

support subscription types:  
```rust
    /// New block headers subscription.
    NewHeads,
    /// Logs subscription.
    Logs,
    /// New Pending Transactions subscription.
    NewPendingTransactions,
    /// Node syncing status subscription.
    Syncing,
```

---

### `eth_filter.rs` 
Interacting with the filtering APIs.


