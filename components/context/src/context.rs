#![allow(clippy::field_reassign_with_default)]
#![deny(warnings)]
use blake2_rfc::blake2b::Blake2b;
use parking_lot::RwLock;
use rand_core::{CryptoRng, RngCore};
use ruc::*;
use std::borrow::{Borrow, BorrowMut};
use std::path::Path;
use std::sync::Arc;
use storage::db::{FinDB, KVBatch};
use storage::state::ChainState;
use storage::state::State as storage_state;

pub type Hash = Vec<u8>;
pub type Height = u64;
pub const HASH_LENGTH: usize = 20;

// Context is hold the application context .
// This should be created initially on app startup ,and hold all necessary pointers for transaction execution
// Mutable references of this context will be passed to individual transactions
#[allow(dead_code)]
pub struct Context<RNG>
where
    RNG: RngCore + CryptoRng,
{
    chain_state: Arc<RwLock<ChainState<FinDB>>>,
    deliver_state: storage_state<FinDB>,
    check_state: storage_state<FinDB>,
    prng: RNG,
}

impl<RNG> Context<RNG>
where
    RNG: RngCore + CryptoRng,
{
    pub fn new(path: &Path, prng: RNG) -> Result<Self> {
        let fdb = FinDB::open(path)?;
        let chain_state =
            Arc::new(RwLock::new(ChainState::new(fdb, "findora_db".to_string())));

        let deliver_state = storage_state::new(chain_state.clone());
        let check_state = storage_state::new(chain_state.clone());
        Ok(Context {
            chain_state,
            deliver_state,
            check_state,
            prng,
        })
    }
    pub fn commit_deliver_state(&mut self, height: Height) -> Result<(Hash, Height)> {
        // Hash returned from commit is already of length 20
        self.deliver_state.commit(height)
    }
    pub fn get_mutable_deliver(&mut self) -> &mut storage_state<FinDB> {
        self.deliver_state.borrow_mut()
    }
    pub fn get_mutable_check(&mut self) -> &mut storage_state<FinDB> {
        self.check_state.borrow_mut()
    }
    pub fn get_immutable_check(&mut self) -> &storage_state<FinDB> {
        self.check_state.borrow()
    }
    pub fn get_immutable_deliver(&mut self) -> &storage_state<FinDB> {
        self.deliver_state.borrow()
    }
    pub fn commit_session_deliver(&mut self) -> KVBatch {
        self.deliver_state.commit_session()
    }
    pub fn commit_session_check(&mut self) -> KVBatch {
        self.check_state.commit_session()
    }
    pub fn discard_session_deliver(&mut self) {
        self.deliver_state.discard_session()
    }
    pub fn discard_session_check(&mut self) {
        self.check_state.discard_session()
    }
}

// generate_hash generates a hash from the two provided hashes
// THe two hashes are ledgerAccess hash (in memory ledger ) and Merk back Chainstate .
// Note ledgerAccess will be deprecated in future
pub fn generate_hash(hash1: Hash, hash2: Hash) -> Hash {
    let mut hasher = Blake2b::new(HASH_LENGTH);
    hasher.update(&*hash1);
    hasher.update(&*hash2);
    let res = hasher.finalize();
    let mut hash: Hash = Vec::with_capacity(HASH_LENGTH);
    // allocating fixed size to vector
    // size for hasher already allocated to HASH_LENGTH earlier
    unsafe { hash.set_len(HASH_LENGTH) }

    hash.copy_from_slice(res.as_bytes());
    hash
}
