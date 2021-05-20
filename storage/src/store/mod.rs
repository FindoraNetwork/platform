use crate::db::{IterOrder, KValue, MerkleDB};
use crate::state::{KVecMap, State};
use ruc::*;
use serde::{de, Serialize};
use std::collections::btree_map::IntoIter;

mod util;
pub use util::Prefix;

/// statable
pub trait Stated<'a, D: MerkleDB> {
    /// set state
    fn set_state(&mut self, state: &'a mut State<D>);

    /// get state
    fn state(&self) -> &State<D>;

    /// get mut state
    fn state_mut(&mut self) -> &mut State<D>;

    /// get base prefix
    fn prefix(&self) -> Prefix;
}

/// store for mempool/consensus/query connection
pub trait Store<'a, D>: Stated<'a, D>
where
    D: MerkleDB,
{
    //===========================read=============================
    fn with_state(&mut self, state: &'a mut State<D>) -> &Self {
        self.set_state(state);
        self
    }

    /// get object by key
    ///
    /// returns deserialized object if key exists or None otherwise
    fn get_obj<T>(&self, key: &[u8]) -> Result<Option<T>>
    where
        T: de::DeserializeOwned,
    {
        match self.get(key).c(d!())? {
            Some(value) => {
                let obj = self.from_vec(&value).c(d!())?;
                Ok(Some(obj))
            }
            None => Ok(None),
        }
    }

    /// deserialize object from Vec<u8>
    fn from_vec<T>(&self, value: &[u8]) -> Result<T>
    where
        T: de::DeserializeOwned,
    {
        let obj = serde_json::from_slice::<T>(value).c(d!())?;
        Ok(obj)
    }

    /// get object by key
    ///
    /// return deserialized object if key exists or default object otherwise
    fn get_obj_or<T>(&self, key: &[u8], default: T) -> Result<T>
    where
        T: de::DeserializeOwned,
    {
        match self.get(key).c(d!())? {
            Some(value) => {
                let obj = serde_json::from_slice::<T>(value.as_ref()).c(d!())?;
                Ok(obj)
            }
            None => Ok(default),
        }
    }

    /// get value. Returns None if deleted
    fn get(&self, key: &[u8]) -> Result<Option<Vec<u8>>> {
        self.state().get(key)
    }

    /// iterate db only
    fn iter_db(
        &self,
        prefix: Prefix,
        asc: bool,
        func: &mut dyn FnMut(KValue) -> bool,
    ) -> bool {
        let mut iter_order = IterOrder::Desc;
        if asc {
            iter_order = IterOrder::Asc
        }
        self.state()
            .iterate(&prefix.begin(), &prefix.end(), iter_order, func)
    }

    /// iterate db AND cache combined
    fn iter_cur(&self, prefix: Prefix) -> IntoIter<Vec<u8>, Vec<u8>> {
        // Iterate chain state
        let mut kv_map = KVecMap::new();
        self.state().iterate(
            &prefix.begin(),
            &prefix.end(),
            IterOrder::Asc,
            &mut |(k, v)| -> bool {
                kv_map.insert(k, v);
                false
            },
        );

        // Iterate cache
        self.state().iterate_cache(prefix.as_ref(), &mut kv_map);
        kv_map.into_iter()
    }

    /// key exists or not. Returns false if deleted
    fn exists(&self, key: &[u8]) -> Result<bool> {
        self.state().exists(key)
    }

    /// KV touched or not in current block
    fn touched(&self, key: &[u8]) -> bool {
        self.state().touched(key)
    }

    /// get current height
    fn height(&self) -> Result<u64> {
        self.state().height()
    }

    /// dump data to json string. TBD!!
    fn dump_state() -> Result<String> {
        Ok(String::from(""))
    }

    //===========================write=============================
    fn with_state_mut(&mut self, state: &'a mut State<D>) -> &mut Self {
        self.set_state(state);
        self
    }

    /// put/update object by key
    fn set_obj<T>(&mut self, key: &[u8], obj: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let value = serde_json::to_vec(obj).c(d!())?;
        self.set(key.as_ref(), value);
        Ok(())
    }

    /// put/update KV
    fn set(&mut self, key: &[u8], value: Vec<u8>) {
        self.state_mut().set(key, value);
    }

    /// delete KV. Nothing happens if key not found
    fn delete(&mut self, key: &[u8]) -> Result<()> {
        self.state_mut().delete(key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::TempFinDB;
    use crate::state::ChainState;
    use parking_lot::RwLock;
    use rand::Rng;
    use std::sync::Arc;
    use std::{thread, time};

    // a example store
    struct StakeStore<'a, D: MerkleDB> {
        pfx: Prefix,
        state: &'a mut State<D>,
    }

    // impl Store
    impl<'a, D: MerkleDB> Store<'a, D> for StakeStore<'a, D> {}

    // impl Stated
    impl<'a, D: MerkleDB> Stated<'a, D> for StakeStore<'a, D> {
        fn set_state(&mut self, state: &'a mut State<D>) {
            self.state = state;
        }

        fn state(&self) -> &State<D> {
            &self.state
        }

        fn state_mut(&mut self) -> &mut State<D> {
            self.state
        }

        fn prefix(&self) -> Prefix {
            self.pfx.clone()
        }
    }

    // impl Store business interfaces
    impl<'a, D: MerkleDB> StakeStore<'a, D> {
        pub fn new(prefix: &str, state: &'a mut State<D>) -> Self {
            StakeStore {
                pfx: Prefix::new(prefix.as_bytes()),
                state,
            }
        }

        /// "stake_validator_fraxxxxx" ==> "amount"
        pub fn get_stake(&self, addr: &str) -> Result<u64> {
            let key = self.stake_key(addr);
            let amt = self.get_amount(key.as_ref()).c(d!())?;
            Ok(amt)
        }

        /// "stake_pool" ==> "amount"
        pub fn get_pool(&self) -> Result<u64> {
            let key = self.pool_key();
            let amt = self.get_amount(key.as_ref()).c(d!())?;
            Ok(amt)
        }

        /// "stake_validator_fraxxxxx" ==> "+amount"
        ///
        /// "stake_pool" ================> "+amount"
        pub fn stake(&mut self, addr: &str, amount: u64) -> Result<()> {
            let key = self.stake_key(addr);
            if self.touched(key.as_ref()) {
                return Err(eg!("Staking/Unstaking is alowed only once a block"));
            }
            self.add_amount(key.as_ref(), amount).c(d!())?;

            let key_pool = self.pool_key();
            self.add_amount(key_pool.as_ref(), amount).c(d!())?;
            Ok(())
        }

        /// "stake_validator_fraxxxxx" ==> "-amount"
        ///
        /// "stake_pool" ================> "-amount"
        pub fn unstake(&mut self, addr: &str, amount: u64) -> Result<()> {
            let key = self.stake_key(addr);
            if self.touched(key.as_ref()) {
                return Err(eg!("Staking/Unstaking is alowed only once a block"));
            }
            self.minus_amount(key.as_ref(), amount).c(d!())?;

            let key_pool = self.pool_key();
            self.minus_amount(key_pool.as_ref(), amount).c(d!())?;
            Ok(())
        }

        fn get_amount(&self, key: &[u8]) -> Result<u64> {
            let amt = self.get_obj_or(key.as_ref(), 0_u64).c(d!())?;
            Ok(amt)
        }

        fn add_amount(&mut self, key: &[u8], amount: u64) -> Result<()> {
            let amt = self.get_amount(key).c(d!())?;
            let amt_new = amt.checked_add(amount).c(d!())?;
            self.set_obj(key.as_ref(), &amt_new).c(d!())?;
            Ok(())
        }

        fn minus_amount(&mut self, key: &[u8], amount: u64) -> Result<()> {
            let amt = self.get_amount(key).c(d!())?;
            if amt >= amount {
                self.set_obj(key.as_ref(), &(amt - amount)).c(d!())?;
            } else {
                eg!("low balance");
            }
            Ok(())
        }

        // stake key is range-based
        fn stake_key(&self, addr: &str) -> Prefix {
            self.pfx.push_sub(b"validator", addr.as_ref())
        }

        // pool key isn't range-based
        fn pool_key(&self) -> Prefix {
            self.pfx.push(b"pool")
        }
    }

    #[test]
    fn store_stake() {
        // create State
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");
        let cs = Arc::new(RwLock::new(ChainState::new(fdb, "findora_db".to_string())));
        let mut check = State::new(cs);
        let mut store = StakeStore::new("stake", &mut check);

        // default stake MUST be zero
        assert_eq!(store.get_stake("fra1111").unwrap(), 0);
        assert_eq!(store.get_pool().unwrap(), 0);

        // stake some coins
        store.stake("fra1111", 100).unwrap();
        store.stake("fra2222", 200).unwrap();
        store.stake("fra3333", 300).unwrap();
        store.stake("fra4455", 400).unwrap();

        // check stakes
        assert_eq!(store.get_stake("fra1111").unwrap(), 100);
        assert_eq!(store.get_stake("fra2222").unwrap(), 200);
        assert_eq!(store.get_stake("fra3333").unwrap(), 300);
        assert_eq!(store.get_stake("fra4455").unwrap(), 400);
        assert_eq!(store.get_pool().unwrap(), 1000);
        store.state_mut().commit(1).unwrap();

        // stake more
        store.stake("fra1111", 10).unwrap();
        store.stake("fra2222", 20).unwrap();
        store.stake("fra3333", 30).unwrap();
        store.stake("fra4455", 40).unwrap();

        // check stakes again
        assert_eq!(store.get_stake("fra1111").unwrap(), 110);
        assert_eq!(store.get_stake("fra2222").unwrap(), 220);
        assert_eq!(store.get_stake("fra3333").unwrap(), 330);
        assert_eq!(store.get_stake("fra4455").unwrap(), 440);
        assert_eq!(store.get_pool().unwrap(), 1100);
    }

    #[test]
    fn store_unstake() {
        // create State
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");
        let cs = Arc::new(RwLock::new(ChainState::new(fdb, "findora_db".to_string())));
        let mut check = State::new(cs);
        let mut store = StakeStore::new("stake", &mut check);

        // stake some coins
        store.stake("fra1111", 100).unwrap();
        store.stake("fra2222", 200).unwrap();
        store.stake("fra3333", 300).unwrap();
        store.stake("fra4455", 400).unwrap();
        store.state_mut().commit(1).unwrap();

        // unstake
        store.unstake("fra1111", 10).unwrap();
        store.unstake("fra2222", 20).unwrap();
        store.unstake("fra3333", 30).unwrap();
        store.unstake("fra4455", 40).unwrap();

        // check stakes again
        assert_eq!(store.get_stake("fra1111").unwrap(), 90);
        assert_eq!(store.get_stake("fra2222").unwrap(), 180);
        assert_eq!(store.get_stake("fra3333").unwrap(), 270);
        assert_eq!(store.get_stake("fra4455").unwrap(), 360);
        assert_eq!(store.get_pool().unwrap(), 900);
    }

    #[test]
    fn store_stake_unstake_too_fast() {
        // create State
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");
        let cs = Arc::new(RwLock::new(ChainState::new(fdb, "findora_db".to_string())));
        let mut check = State::new(cs);
        let mut store = StakeStore::new("stake", &mut check);

        // stake some coins at height 1
        store.stake("fra1111", 100).unwrap();
        assert!(store.stake("fra1111", 150).is_err()); // MUST fail
        store.stake("fra2222", 200).unwrap();
        store.stake("fra3333", 300).unwrap();
        assert!(store.stake("fra3333", 250).is_err()); // MUST fail
        store.stake("fra4455", 400).unwrap();
        store.state_mut().commit(1).unwrap();

        // check stakes after commit
        assert_eq!(store.get_stake("fra1111").unwrap(), 100);
        assert_eq!(store.get_stake("fra2222").unwrap(), 200);
        assert_eq!(store.get_stake("fra3333").unwrap(), 300);
        assert_eq!(store.get_stake("fra4455").unwrap(), 400);
        assert_eq!(store.get_pool().unwrap(), 1000);

        // unstake some coins at height 2
        store.unstake("fra1111", 10).unwrap();
        store.unstake("fra2222", 20).unwrap();
        assert!(store.unstake("fra2222", 20).is_err()); // MUST fail
        store.unstake("fra3333", 30).unwrap();
        store.unstake("fra4455", 40).unwrap();
        assert!(store.unstake("fra4455", 40).is_err()); // MUST fail

        // check stakes without commit
        assert_eq!(store.get_stake("fra1111").unwrap(), 90);
        assert_eq!(store.get_stake("fra2222").unwrap(), 180);
        assert_eq!(store.get_stake("fra3333").unwrap(), 270);
        assert_eq!(store.get_stake("fra4455").unwrap(), 360);
        assert_eq!(store.get_pool().unwrap(), 900);
    }

    #[test]
    fn store_iter_db() {
        // create State
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");
        let cs = Arc::new(RwLock::new(ChainState::new(fdb, "findora_db".to_string())));
        let mut check = State::new(cs);
        let mut store = StakeStore::new("stake", &mut check);

        // stake some coins
        store.stake("fra1111", 100).unwrap();
        store.stake("fra2222", 200).unwrap();
        store.stake("fra3333", 300).unwrap();
        store.stake("fra4455", 400).unwrap();

        // commit block 1
        let (hash1, height) = store.state_mut().commit(1).unwrap();
        assert_eq!(height, 1);

        // unstake some coins in session of block 2
        store.unstake("fra1111", 10).unwrap();
        store.unstake("fra2222", 20).unwrap();
        store.unstake("fra3333", 300).unwrap();
        assert!(store.delete(store.stake_key("fra3333").as_ref()).is_ok());
        store.unstake("fra4455", 40).unwrap();

        // check stakes before committing block 2
        let expected = vec![
            (b"stake_validator_fra1111".to_vec(), 100_u64),
            (b"stake_validator_fra2222".to_vec(), 200_u64),
            (b"stake_validator_fra3333".to_vec(), 300_u64),
            (b"stake_validator_fra4455".to_vec(), 400_u64),
        ];
        let pfx_v = store.prefix().push(b"validator");
        let mut actual = vec![];
        store.iter_db(pfx_v, true, &mut |(k, v)| {
            let amt = store.from_vec::<u64>(&v);
            actual.push((k, amt.unwrap()));
            false
        });
        assert_eq!(actual, expected);
        assert_eq!(store.get_pool().unwrap(), 630);

        // commit block 2
        let (hash2, height) = store.state_mut().commit(2).unwrap();
        assert_eq!(height, 2);
        assert_ne!(hash1, hash2);

        // check stakes after committing block 2
        let expected = vec![
            (b"stake_validator_fra1111".to_vec(), 90_u64),
            (b"stake_validator_fra2222".to_vec(), 180_u64),
            (b"stake_validator_fra4455".to_vec(), 360_u64),
        ];
        let pfx_v = store.prefix().push(b"validator");
        let mut actual = vec![];
        store.iter_db(pfx_v, true, &mut |(k, v)| {
            let amt = store.from_vec::<u64>(&v);
            actual.push((k, amt.unwrap()));
            false
        });
        assert_eq!(actual, expected);
        assert_eq!(store.get_pool().unwrap(), 630);
    }
    #[test]
    fn store_iter_cur() {
        // create State
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");
        let cs = Arc::new(RwLock::new(ChainState::new(fdb, "findora_db".to_string())));
        let mut check = State::new(cs);
        let mut store = StakeStore::new("stake", &mut check);

        // stake some coins and commit block 1
        store.stake("fra1111", 100).unwrap();
        store.state_mut().commit(1).unwrap();

        // stake, unstake, delete and commit tx session
        store.unstake("fra1111", 100).unwrap();
        assert!(store.delete(store.stake_key("fra1111").as_ref()).is_ok());
        store.stake("fra2222", 200).unwrap();
        store.stake("fra3333", 300).unwrap();
        let _res = store.state.commit_session();

        // stake, unstake again
        store.stake("fra44555", 400).unwrap();
        store.stake("fra55667", 500).unwrap();

        // check cached stakes before committing tx session
        let kvs: Vec<_> = store.iter_cur(store.prefix()).collect();
        let expected = vec![
            (b"stake_pool".to_vec(), b"1400".to_vec()),
            (b"stake_validator_fra2222".to_vec(), b"200".to_vec()),
            (b"stake_validator_fra3333".to_vec(), b"300".to_vec()),
            (b"stake_validator_fra44555".to_vec(), b"400".to_vec()),
            (b"stake_validator_fra55667".to_vec(), b"500".to_vec()),
        ];
        assert_eq!(kvs, expected);

        // check cached stakes after committing tx session
        let _res = store.state.commit_session();
        let kvs: Vec<_> = store.iter_cur(store.prefix()).collect();
        assert_eq!(kvs, expected);
    }

    #[test]
    fn store_threading() {
        // create State
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");
        let cs = Arc::new(RwLock::new(ChainState::new(fdb, "findora_db".to_string())));
        let mut state = State::new(cs.clone());
        let mut store = StakeStore::new("stake", &mut state);

        // stake initial coins and commit
        let validators = Arc::new(vec![
            "fra1111".to_owned(),
            "fra2222".to_owned(),
            "fra3333".to_owned(),
            "fra4444".to_owned(),
            "fra5555".to_owned(),
        ]);
        for v in validators.iter() {
            store.stake(v, 1000000).unwrap();
        }
        let (hash, height) = store.state_mut().commit(1).unwrap();
        assert_eq!(1, height);

        // read/write times for each thread
        let times = 2500;

        // thread 1: query
        let mut threads = vec![];
        let vldts_1 = validators.clone();
        let cs_1 = cs.clone();
        threads.push(thread::spawn(move || {
            // create store
            let mut query = State::new(cs_1.clone());
            let store = StakeStore::new("stake", &mut query);

            // starts
            let mut rng = rand::thread_rng();
            let mut reads = 0;
            while reads < times {
                thread::sleep(time::Duration::from_micros(1));

                // read height and get a random validator's staking amount
                let height = store.height().unwrap();
                let addr = &vldts_1[rng.gen_range(0..5)];
                let amount = store.get_stake(addr).unwrap();
                let amount_pool = store.get_pool().unwrap();
                assert!(height >= 1);
                // rough check
                assert!(amount_pool > amount);
                reads += 1;
            }
        }));

        // thread 2: query
        let cs_2 = cs.clone();
        threads.push(thread::spawn(move || {
            // create store
            let mut query = State::new(cs_2.clone());
            let store = StakeStore::new("stake", &mut query);

            let mut reads = 0;
            while reads < times {
                thread::sleep(time::Duration::from_micros(1));

                // iterates commited staking amounts
                let mut total = 0_u64;
                let pfx_v = store.prefix().push(b"validator");
                store.iter_db(pfx_v, true, &mut |(_, v)| {
                    let amt = store.from_vec::<u64>(&v);
                    total += amt.unwrap();
                    false
                });
                let height = store.height().unwrap();
                let amount_pool = store.get_pool().unwrap();
                assert!(height >= 1);
                assert!(total > 0);
                assert!(amount_pool > 0);
                reads += 1;
            }
        }));

        // thread 3: check_tx
        let cs_3 = cs.clone();
        threads.push(thread::spawn(move || {
            // create store
            let mut check = State::new(cs_3.clone());
            let store = StakeStore::new("stake", &mut check);

            let mut reads = 0;
            while reads < times {
                thread::sleep(time::Duration::from_micros(1));

                // iterates commited staking amounts
                let mut total = 0_u64;
                let pfx_v = store.prefix().push(b"validator");
                for (_, v) in store.iter_cur(pfx_v) {
                    let amt = store.from_vec::<u64>(&v);
                    total += amt.unwrap();
                }
                let height = store.height().unwrap();
                let amount_pool = store.get_pool().unwrap();
                assert!(height >= 1);
                assert!(total > 0);
                assert!(amount_pool > 0);
                reads += 1;
            }
        }));

        // thread 4: deliver_tx
        let vldts_2 = validators;
        let cs_4 = cs;
        threads.push(thread::spawn(move || {
            // create store
            let mut deliver = State::new(cs_4.clone());
            let mut store = StakeStore::new("stake", &mut deliver);

            // starts
            let mut rng = rand::thread_rng();
            let mut hash = store.state().root_hash();
            let mut height = store.height().unwrap();

            // commit blocks [2...times]
            while height < times {
                thread::sleep(time::Duration::from_micros(1));

                // random validator stake/unstake random amount
                let addr = &vldts_2[rng.gen_range(0..5)];
                let amt = rng.gen_range(1..=100);
                if rng.gen_bool(0.5) {
                    assert!(store.stake(addr, amt).is_ok());
                } else {
                    assert!(store.unstake(addr, amt).is_ok());
                }
                height += 1;
                let (hash_new, height_new) = store.state_mut().commit(height).unwrap();
                assert_eq!(height, height_new);
                assert_ne!(hash, hash_new);
                hash = hash_new;
            }
        }));

        // join 4 child threads
        for t in threads {
            let _ = t.join();
        }

        // checks height, hash
        assert_eq!(times, store.height().unwrap());
        assert_ne!(hash, store.state().root_hash());

        // check amounts
        let mut total = 0_u64;
        let pfx_v = store.prefix().push(b"validator");
        store.iter_db(pfx_v, true, &mut |(_, v)| {
            let amt = store.from_vec::<u64>(&v);
            total += amt.unwrap();
            false
        });
        let amount_pool = store.get_pool().unwrap();
        assert_eq!(total, amount_pool);
    }
}
