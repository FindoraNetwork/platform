use crate::db::KVBatch;
use std::collections::BTreeMap;
use std::iter::Iterator;

/// key-value map
pub type KVMap = BTreeMap<Vec<u8>, Option<Vec<u8>>>;
pub type KVecMap = BTreeMap<Vec<u8>, Vec<u8>>;

/// cache iterator
pub struct CacheIter<'a> {
    iter: std::collections::btree_map::Iter<'a, Vec<u8>, Option<Vec<u8>>>,
}

impl<'a> Iterator for CacheIter<'a> {
    type Item = (&'a Vec<u8>, &'a Option<Vec<u8>>);
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

/// sessioned KV cache
pub struct SessionedCache {
    cur: KVMap,
    base: KVMap,
}

#[allow(clippy::new_without_default)]
impl SessionedCache {
    pub fn new() -> Self {
        SessionedCache {
            cur: KVMap::new(),
            base: KVMap::new(),
        }
    }

    /// put/update value by key
    pub fn put(&mut self, key: &[u8], value: Vec<u8>) {
        self.cur.insert(key.to_owned(), Some(value));
    }

    /// delete key-pair by marking as None
    pub fn delete(&mut self, key: &[u8]) {
        self.cur.insert(key.to_owned(), None);
    }

    /// Remove Key from cur
    ///
    /// key may still exist in base after removal
    pub fn remove(&mut self, key: &[u8]) {
        self.cur.remove(key);
    }

    /// commits pending KVs in session
    pub fn commit(&mut self) -> KVBatch {
        // Merge current key value updates to the base version
        self.rebase();

        // Return current batch
        self.values()
    }

    /// discards pending KVs in session
    ///
    /// rollback to base
    pub fn discard(&mut self) {
        self.cur = self.base.clone();
    }

    /// KV touched or not so far
    ///
    /// KV is touched whever it gets updated or deleted
    ///
    /// KV is touched even when value stays same
    ///
    /// use case: when KV is not allowed to updated twice
    pub fn touched(&self, key: &[u8]) -> bool {
        self.cur.contains_key(key)
    }

    /// KV deleted or not
    ///
    /// use case: stop reading KV from db if already deleted
    pub fn deleted(&self, key: &[u8]) -> bool {
        if let Some(None) = self.cur.get(key) {
            return true;
        }
        false
    }

    /// keys that have been touched
    pub fn keys(&self) -> Vec<Vec<u8>> {
        let keys: Vec<_> = self.cur.keys().cloned().collect();
        keys
    }

    /// get all KVs
    pub fn values(&self) -> KVBatch {
        let kvs: Vec<_> = self
            .cur
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        kvs
    }

    /// has value or not
    ///
    /// returns true  if new KV inserted
    ///
    /// returns true  if existing KV gets updated
    ///
    /// returns false if existing KV gets deleted
    ///
    /// use case: get from cache instead of db whenever hasv() returns true.
    pub fn hasv(&self, key: &[u8]) -> bool {
        match self.cur.get(key) {
            // has value
            Some(Some(_)) => true,
            // deleted
            Some(None) => false,
            // nerver see it
            None => false,
        }
    }

    /// get value by key
    ///
    /// returns Some(value) if available
    ///
    /// returns None otherwise
    pub fn getv(&self, key: &[u8]) -> Option<Vec<u8>> {
        match self.cur.get(key) {
            Some(Some(value)) => Some(value.clone()),
            Some(None) => None,
            None => None,
        }
    }

    /// get value by key
    ///
    /// returns Some(Some(value)) if available
    ///
    /// returns Some(Some(None))  if deleted
    ///
    /// returns None otherwise
    pub fn get(&self, key: &[u8]) -> Option<Option<Vec<u8>>> {
        match self.cur.get(key) {
            Some(Some(value)) => Some(Some(value.clone())),
            Some(None) => Some(None),
            None => None,
        }
    }

    /// iterator
    pub fn iter(&self) -> CacheIter {
        CacheIter {
            iter: self.cur.iter(),
        }
    }

    /// prefix iterator
    pub fn iter_prefix(&self, prefix: &[u8], map: &mut KVecMap) {
        // insert/update new KVs and remove deleted KVs
        for (k, v) in self.cur.iter() {
            if k.starts_with(prefix) {
                if v.is_some() {
                    map.insert(k.to_owned(), v.to_owned().unwrap());
                } else {
                    map.remove(k);
                }
            }
        }
    }

    /// rebases cur KVs onto base
    fn rebase(&mut self) {
        self.base = self.cur.clone();
    }
}

#[cfg(test)]
mod tests {
    use super::SessionedCache;
    use crate::state::cache::KVecMap;

    #[test]
    fn cache_put_n_get() {
        let mut cache = SessionedCache::new();

        // put data
        cache.put(b"k10", b"v10".to_vec());
        cache.put(b"k20", b"v20".to_vec());

        // verify touched() flag
        assert!(cache.touched(b"k10"));
        assert!(cache.touched(b"k20"));

        // verify deleted() flag
        assert!(!cache.deleted(b"k10"));
        assert!(!cache.deleted(b"k20"));

        // verify keys
        assert_eq!(cache.keys(), vec![b"k10".to_vec(), b"k20".to_vec()]);

        // verify hasv() flag
        assert!(cache.hasv(b"k10"));
        assert!(cache.hasv(b"k20"));

        // getv and compare
        assert_eq!(cache.getv(b"k10").unwrap(), b"v10".to_vec());
        assert_eq!(cache.getv(b"k20").unwrap(), b"v20".to_vec());

        // get and compare
        assert_eq!(cache.get(b"k10").unwrap().unwrap(), b"v10".to_vec());
        assert_eq!(cache.get(b"k20").unwrap().unwrap(), b"v20".to_vec());
    }

    #[test]
    fn cache_put_n_update() {
        let mut cache = SessionedCache::new();

        // put data
        cache.put(b"k10", b"v10".to_vec());
        cache.put(b"k20", b"v20".to_vec());

        // update data
        cache.put(b"k10", b"v11".to_vec());
        cache.put(b"k20", b"v21".to_vec());

        // verify touched() flag
        assert!(cache.touched(b"k10"));
        assert!(cache.touched(b"k20"));

        // verify deleted() flag
        assert!(!cache.deleted(b"k10"));
        assert!(!cache.deleted(b"k20"));

        // verify keys
        assert_eq!(cache.keys(), vec![b"k10".to_vec(), b"k20".to_vec()]);

        // verify hasv() flag
        assert!(cache.hasv(b"k10"));
        assert!(cache.hasv(b"k20"));

        // getv and compare
        assert_eq!(cache.getv(b"k10").unwrap(), b"v11".to_vec());
        assert_eq!(cache.getv(b"k20").unwrap(), b"v21".to_vec());

        // get and compare
        assert_eq!(cache.get(b"k10").unwrap().unwrap(), b"v11".to_vec());
        assert_eq!(cache.get(b"k20").unwrap().unwrap(), b"v21".to_vec());
    }

    #[test]
    fn cache_put_n_delete() {
        let mut cache = SessionedCache::new();

        // put data
        cache.put(b"k10", b"v10".to_vec());
        cache.put(b"k20", b"v20".to_vec());

        // delete data
        cache.delete(b"k10");
        cache.delete(b"k20");

        // verify touched() flag
        assert!(cache.touched(b"k10"));
        assert!(cache.touched(b"k20"));

        // verify deleted() flag
        assert!(cache.deleted(b"k10"));
        assert!(cache.deleted(b"k20"));

        // verify keys
        assert_eq!(cache.keys(), vec![b"k10".to_vec(), b"k20".to_vec()]);

        // verify hasv() flag
        assert!(!cache.hasv(b"k10"));
        assert!(!cache.hasv(b"k20"));

        // getv and compare
        assert_eq!(cache.getv(b"k10"), None);
        assert_eq!(cache.getv(b"k20"), None);

        // get and compare
        assert_eq!(cache.get(b"k10").unwrap(), None);
        assert_eq!(cache.get(b"k20").unwrap(), None);
    }

    #[test]
    fn cache_put_n_commit() {
        let mut cache = SessionedCache::new();

        // put data and commit
        cache.put(b"k10", b"v10".to_vec());
        cache.put(b"k20", b"v20".to_vec());
        cache.commit();

        // verify touched() flag
        assert!(cache.touched(b"k10"));
        assert!(cache.touched(b"k20"));

        // verify deleted() flag
        assert!(!cache.deleted(b"k10"));
        assert!(!cache.deleted(b"k20"));

        // verify keys
        assert_eq!(cache.keys(), vec![b"k10".to_vec(), b"k20".to_vec()]);

        // verify hasv() flag
        assert!(cache.hasv(b"k10"));
        assert!(cache.hasv(b"k20"));

        // getv and compare
        assert_eq!(cache.getv(b"k10").unwrap(), b"v10".to_vec());
        assert_eq!(cache.getv(b"k20").unwrap(), b"v20".to_vec());

        // get and compare
        assert_eq!(cache.get(b"k10").unwrap().unwrap(), b"v10".to_vec());
        assert_eq!(cache.get(b"k20").unwrap().unwrap(), b"v20".to_vec());
    }

    #[test]
    fn cache_commit_n_put_delete_again() {
        let mut cache = SessionedCache::new();

        // put data and commit
        cache.put(b"k10", b"v10".to_vec());
        cache.put(b"k20", b"v20".to_vec());
        cache.put(b"k30", b"v30".to_vec());
        cache.commit();

        // put/delete data again
        cache.put(b"k10", b"v11".to_vec());
        cache.delete(b"k20");
        cache.delete(b"k30");
        cache.put(b"k40", b"v40".to_vec());

        // verify touched() flag
        assert!(cache.touched(b"k10"));
        assert!(cache.touched(b"k20"));
        assert!(cache.touched(b"k30"));
        assert!(cache.touched(b"k40"));

        // verify deleted() flag
        assert!(!cache.deleted(b"k10"));
        assert!(cache.deleted(b"k20"));
        assert!(cache.deleted(b"k30"));
        assert!(!cache.deleted(b"k40"));

        // verify keys
        assert_eq!(
            cache.keys(),
            vec![
                b"k10".to_vec(),
                b"k20".to_vec(),
                b"k30".to_vec(),
                b"k40".to_vec()
            ]
        );

        // verify hasv() flag
        assert!(cache.hasv(b"k10"));
        assert!(!cache.hasv(b"k20"));
        assert!(!cache.hasv(b"k30"));
        assert!(cache.hasv(b"k40"));

        // getv and compare
        assert_eq!(cache.getv(b"k10").unwrap(), b"v11".to_vec());
        assert_eq!(cache.getv(b"k20"), None);
        assert_eq!(cache.getv(b"k30"), None);
        assert_eq!(cache.getv(b"k40").unwrap(), b"v40".to_vec());

        // get and compare
        assert_eq!(cache.get(b"k10").unwrap().unwrap(), b"v11".to_vec());
        assert_eq!(cache.get(b"k20").unwrap(), None);
        assert_eq!(cache.get(b"k30").unwrap(), None);
        assert_eq!(cache.get(b"k40").unwrap().unwrap(), b"v40".to_vec());
    }

    #[test]
    fn cache_commit_n_put_delete_n_discard() {
        let mut cache = SessionedCache::new();

        // put data and commit
        cache.put(b"k10", b"v10".to_vec());
        cache.put(b"k20", b"v20".to_vec());
        cache.commit();

        // put/delete data again
        cache.put(b"k10", b"v11".to_vec());
        cache.delete(b"k20");
        cache.put(b"k30", b"v30".to_vec());
        cache.put(b"k40", b"v40".to_vec());

        // discard this session
        cache.discard();

        // verify touched() flag
        assert!(cache.touched(b"k10"));
        assert!(cache.touched(b"k20"));
        assert!(!cache.touched(b"k30"));
        assert!(!cache.touched(b"k40"));

        // verify deleted() flag
        assert!(!cache.deleted(b"k10"));
        assert!(!cache.deleted(b"k20"));
        assert!(!cache.deleted(b"k30"));
        assert!(!cache.deleted(b"k40"));

        // verify keys
        assert_eq!(cache.keys(), vec![b"k10".to_vec(), b"k20".to_vec()]);

        // verify hasv() flag
        assert!(cache.hasv(b"k10"));
        assert!(cache.hasv(b"k20"));
        assert!(!cache.hasv(b"k30"));
        assert!(!cache.hasv(b"k40"));

        // getv and compare
        assert_eq!(cache.getv(b"k10").unwrap(), b"v10".to_vec());
        assert_eq!(cache.getv(b"k20").unwrap(), b"v20".to_vec());
        assert_eq!(cache.getv(b"k30"), None);
        assert_eq!(cache.getv(b"k40"), None);

        // get and compare
        assert_eq!(cache.get(b"k10").unwrap().unwrap(), b"v10".to_vec());
        assert_eq!(cache.get(b"k20").unwrap().unwrap(), b"v20".to_vec());
        assert_eq!(cache.get(b"k30"), None);
        assert_eq!(cache.get(b"k40"), None);
    }

    #[test]
    fn cache_put_n_iterate() {
        let mut cache = SessionedCache::new();

        // put data in random order
        cache.put(b"k10", b"v10".to_vec());
        cache.put(b"k30", b"v30".to_vec());
        cache.put(b"k20", b"v20".to_vec());

        // iterate
        let actual = cache.values();
        let expected = vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k20".to_vec(), Some(b"v20".to_vec())),
            (b"k30".to_vec(), Some(b"v30".to_vec())),
        ];

        // check
        assert_eq!(actual, expected);
    }

    #[test]
    fn cache_put_delete_n_iterate() {
        let mut cache = SessionedCache::new();

        // put data in random order
        cache.put(b"k10", b"v10".to_vec());
        cache.put(b"k40", b"v40".to_vec());
        cache.put(b"k30", b"v30".to_vec());
        cache.put(b"k20", b"v20".to_vec());

        // delete some and double-delete shouldn't hurt
        cache.delete(b"k10");
        cache.delete(b"k10");
        cache.delete(b"k30");

        // iterate
        let actual = cache.values();
        let expected = vec![
            (b"k10".to_vec(), None),
            (b"k20".to_vec(), Some(b"v20".to_vec())),
            (b"k30".to_vec(), None),
            (b"k40".to_vec(), Some(b"v40".to_vec())),
        ];

        // check
        assert_eq!(actual, expected);
    }

    #[test]
    fn cache_commit_n_put_delete_discard_n_iterate() {
        let mut cache = SessionedCache::new();

        // put data in random order and delete one
        cache.put(b"k10", b"v10".to_vec());
        cache.put(b"k40", b"v40".to_vec());
        cache.put(b"k30", b"v30".to_vec());
        cache.put(b"k20", b"v20".to_vec());
        cache.delete(b"k10");
        cache.commit();

        // put/delete data again
        cache.put(b"k10", b"v11".to_vec());
        cache.delete(b"k20");
        cache.delete(b"k30");
        cache.put(b"k40", b"v41".to_vec());
        cache.put(b"k50", b"v50".to_vec());

        // iterate and check
        let actual = cache.values();
        let expected = vec![
            (b"k10".to_vec(), Some(b"v11".to_vec())),
            (b"k20".to_vec(), None),
            (b"k30".to_vec(), None),
            (b"k40".to_vec(), Some(b"v41".to_vec())),
            (b"k50".to_vec(), Some(b"v50".to_vec())),
        ];
        assert_eq!(actual, expected);

        // discard this session
        cache.discard();

        // iterate and check
        let actual = cache.values();
        let expected = vec![
            (b"k10".to_vec(), None),
            (b"k20".to_vec(), Some(b"v20".to_vec())),
            (b"k30".to_vec(), Some(b"v30".to_vec())),
            (b"k40".to_vec(), Some(b"v40".to_vec())),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_remove() {
        let mut cache = SessionedCache::new();

        //Put some date into cache
        cache.put(b"k40", b"v40".to_vec());
        cache.put(b"k30", b"v30".to_vec());
        cache.put(b"k20", b"v20".to_vec());
        cache.commit();

        //Remove one of the values
        cache.remove(b"k40");
        assert_eq!(cache.get(b"k40"), None);

        //Roll back above removal
        cache.discard();
        assert_eq!(cache.get(b"k40").unwrap(), Some(b"v40".to_vec()));

        //Remove it again and commit
        cache.remove(b"k40");
        cache.commit();
        assert_eq!(cache.get(b"k40"), None);

        //Remove a value that doesn't exist
        cache.remove(b"k50");
        assert_eq!(cache.get(b"k50"), None);
    }

    #[test]
    fn test_iterate_prefix() {
        let mut cache = SessionedCache::new();
        let mut my_cache = KVecMap::new();

        //Put some date into cache
        cache.put(b"validator_1", b"v10".to_vec());
        cache.put(b"k30", b"v30".to_vec());
        cache.put(b"k20", b"v20".to_vec());
        cache.put(b"validator_5", b"v50".to_vec());
        cache.put(b"validator_3", b"v30".to_vec());
        cache.put(b"validator_2", b"v20".to_vec());
        cache.put(b"validator_4", b"v40".to_vec());

        //Del two of validators
        cache.delete(b"validator_1");
        cache.delete(b"validator_3");

        cache.iter_prefix(b"validator", &mut my_cache);

        let expected = vec![
            (b"validator_2".to_vec(), b"v20".to_vec()),
            (b"validator_4".to_vec(), b"v40".to_vec()),
            (b"validator_5".to_vec(), b"v50".to_vec()),
        ];

        let values: Vec<_> = my_cache
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        assert_eq!(values, expected);
    }
}
