/// ChainState is a storage for latest blockchain state data
///
/// This Structure will be the main interface to the persistence layer provided by MerkleDB
/// and RocksDB backend.
///
use crate::db::{IterOrder, KVBatch, KValue, MerkleDB};
use merk::tree::Tree;
use ruc::*;

const HEIGHT_KEY: &[u8; 6] = b"Height";

/// Concrete ChainState struct containing a reference to an instance of MerkleDB, a name and
/// current tree height.
pub struct ChainState<D>
where
    D: MerkleDB,
{
    name: String,
    db: D,
}

/// Implementation of of the concrete ChainState struct
impl<D> ChainState<D>
where
    D: MerkleDB,
{
    /// Creates a new instance of the ChainState.
    ///
    /// A default name is used if not provided and a reference to a struct implementing the
    /// MerkleDB trait is assigned.
    ///
    /// Returns the implicit struct
    pub fn new(db: D, name: String) -> Self {
        let mut db_name = "chain-state";
        if !name.is_empty() {
            db_name = name.as_str();
        }

        ChainState {
            name: db_name.to_string(),
            db,
        }
    }

    /// Gets a value for the given key from the primary data section in RocksDB
    pub fn get(&self, key: &[u8]) -> Result<Option<Vec<u8>>> {
        self.db.get(key)
    }

    /// Gets a value for the given key from the auxiliary data section in RocksDB.
    ///
    /// This section of data is not used for root hash calculations.
    pub fn get_aux(&self, key: &[u8]) -> Result<Option<Vec<u8>>> {
        self.db.get_aux(key)
    }

    /// Iterates MerkleDB for a given range of keys.
    ///
    /// Executes a closure passed as a parameter with the corresponding key value pairs.
    pub fn iterate(
        &self,
        lower: &[u8],
        upper: &[u8],
        order: IterOrder,
        func: &mut dyn FnMut(KValue) -> bool,
    ) -> bool {
        // Get DB iterator
        let mut db_iter = self.db.iter(lower, upper, order);
        let mut stop = false;

        // Loop through each entry in range
        while !stop {
            let kv_pair = match db_iter.next() {
                Some(result) => result,
                None => break,
            };

            let kv = Tree::decode(kv_pair.0.to_vec(), &kv_pair.1);
            let key = kv.key();
            let value = kv.value();

            let entry: KValue = (key.to_vec(), value.to_vec());
            stop = func(entry);
        }
        true
    }

    /// Iterates MerkleDB allocated in auxiliary section for a given range of keys.
    ///
    /// Executes a closure passed as a parameter with the corresponding key value pairs.
    pub fn iterate_aux(
        &self,
        lower: &[u8],
        upper: &[u8],
        order: IterOrder,
        func: &mut dyn FnMut(KValue) -> bool,
    ) -> bool {
        // Get DB iterator
        let mut db_iter = self.db.iter_aux(lower, upper, order);
        let mut stop = false;

        // Loop through each entry in range
        while !stop {
            let kv_pair = match db_iter.next() {
                Some(result) => result,
                None => break,
            };

            //AUX data doesn't need to be decoded
            let key = kv_pair.0;
            let value = kv_pair.1;

            let entry: KValue = (key.to_vec(), value.to_vec());
            stop = func(entry);
        }
        true
    }

    /// Queries the DB for existence of a key.
    ///
    /// Returns a bool wrapped in a result as the query involves DB access.
    pub fn exists(&self, key: &[u8]) -> Result<bool> {
        match self.get(key).c(d!())? {
            Some(_) => Ok(true),
            None => Ok(false),
        }
    }

    /// Commits a key value batch to the MerkleDB.
    ///
    /// The current height is updated in the ChainState as well as in the auxiliary data of the DB.
    /// An optional flag is also passed to indicate whether RocksDB should flush its mem table
    /// to disk.
    ///
    /// Due to the requirements of MerkleDB, the batch needs to be sorted prior to a commit.
    ///
    /// Returns the current height as well as the updated root hash of the Merkle Tree.
    pub fn commit(
        &mut self,
        mut batch: KVBatch,
        height: u64,
        flush: bool,
    ) -> Result<(Vec<u8>, u64)> {
        // Update height value in batch
        let height_str = height.to_string();

        batch.sort();
        self.db.put_batch(batch).c(d!())?;
        self.db
            .commit(
                vec![(HEIGHT_KEY.to_vec(), Some(height_str.as_bytes().to_vec()))],
                flush,
            )
            .c(d!())?;

        Ok((self.db.root_hash(), height))
    }

    /// Calculate and returns current root hash of the Merkle tree
    pub fn root_hash(&self) -> Vec<u8> {
        self.db.root_hash()
    }

    /// Returns current height of the ChainState
    pub fn height(&self) -> Result<u64> {
        let height = self.db.get_aux(HEIGHT_KEY).c(d!())?;
        if let Some(value) = height {
            let height_str = String::from_utf8(value).c(d!())?;
            let last_height = height_str.parse::<u64>().c(d!())?;

            return Ok(last_height);
        }
        Ok(0u64)
    }

    /// Returns the Name of the ChainState
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// This function will prune the tree of spent transaction outputs to reduce memory usage
    pub fn prune_tree() {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use crate::db::{IterOrder, KValue, MerkleDB, TempFinDB};
    use crate::state::chain_state;
    use std::thread;

    #[test]
    fn test_new_chain_state() {
        //Create new database
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");

        //Create new Chain State with new database
        let _cs = chain_state::ChainState::new(fdb, "test_db".to_string());
    }

    #[test]
    fn test_get() {
        let path = thread::current().name().unwrap().to_owned();
        let mut fdb = TempFinDB::open(path).expect("failed to open db");

        // put data
        fdb.put_batch(vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k20".to_vec(), Some(b"v20".to_vec())),
        ])
        .unwrap();

        // commit data without flushing to disk
        fdb.commit(vec![(b"height".to_vec(), Some(b"25".to_vec()))], false)
            .unwrap();

        //Create new Chain State with new database
        let cs = chain_state::ChainState::new(fdb, "test_db".to_string());

        assert_eq!(cs.get(&b"k10".to_vec()).unwrap(), Some(b"v10".to_vec()));
        assert_eq!(cs.get(&b"k20".to_vec()).unwrap(), Some(b"v20".to_vec()));
        assert_eq!(cs.get(&b"kN/A".to_vec()).unwrap(), None);
        assert_eq!(
            cs.get_aux(&b"height".to_vec()).unwrap(),
            Some(b"25".to_vec())
        );
    }

    #[test]
    fn test_iterate() {
        let path = thread::current().name().unwrap().to_owned();
        let mut fdb = TempFinDB::open(path).expect("failed to open db");

        let mut index = 0;
        let batch = vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k20".to_vec(), Some(b"v20".to_vec())),
            (b"k30".to_vec(), Some(b"v30".to_vec())),
            (b"k40".to_vec(), Some(b"v40".to_vec())),
            (b"k50".to_vec(), Some(b"v50".to_vec())),
            (b"k60".to_vec(), Some(b"v60".to_vec())),
            (b"k70".to_vec(), Some(b"v70".to_vec())),
            (b"k80".to_vec(), Some(b"v80".to_vec())),
        ];

        let batch_clone = batch.clone();

        // put data
        fdb.put_batch(batch).unwrap();

        // commit data without flushing to disk
        fdb.commit(vec![(b"height".to_vec(), Some(b"26".to_vec()))], false)
            .unwrap();

        //Create new Chain State with new database
        let cs = chain_state::ChainState::new(fdb, "test_db".to_string());
        let mut func_iter = |entry: KValue| {
            println!("Key: {:?}, Value: {:?}", entry.0, entry.1);
            //Assert Keys are equal
            assert_eq!(entry.0, batch_clone[index].0);
            //Assert Values are equal
            assert_eq!(entry.1, batch_clone[index].1.clone().unwrap());

            index += 1;
            false
        };
        cs.iterate(
            &b"k10".to_vec(),
            &b"k80".to_vec(),
            IterOrder::Asc,
            &mut func_iter,
        );
    }

    #[test]
    fn test_aux_iterator() {
        let path = thread::current().name().unwrap().to_owned();
        let mut fdb = TempFinDB::open(path).expect("failed to open db");

        let mut index = 0;
        let batch = vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k20".to_vec(), Some(b"v20".to_vec())),
            (b"k30".to_vec(), Some(b"v30".to_vec())),
            (b"k40".to_vec(), Some(b"v40".to_vec())),
            (b"k50".to_vec(), Some(b"v50".to_vec())),
            (b"k60".to_vec(), Some(b"v60".to_vec())),
            (b"k70".to_vec(), Some(b"v70".to_vec())),
            (b"k80".to_vec(), Some(b"v80".to_vec())),
        ];

        let batch_clone = batch.clone();

        // put data
        fdb.put_batch(vec![]).unwrap();

        // commit data without flushing to disk, the batch is passed to the aux data here
        fdb.commit(batch, false).unwrap();

        //Create new Chain State with new database
        let cs = chain_state::ChainState::new(fdb, "test_db".to_string());
        let mut func_iter = |entry: KValue| {
            println!("Key: {:?}, Value: {:?}", entry.0, entry.1);
            //Assert Keys are equal
            assert_eq!(entry.0, batch_clone[index].0);
            //Assert Values are equal
            assert_eq!(entry.1, batch_clone[index].1.clone().unwrap());

            index += 1;
            false
        };
        cs.iterate_aux(
            &b"k10".to_vec(),
            &b"k80".to_vec(),
            IterOrder::Asc,
            &mut func_iter,
        );
    }

    #[test]
    fn test_exists() {
        let path = thread::current().name().unwrap().to_owned();
        let mut fdb = TempFinDB::open(path).expect("failed to open db");

        // put data
        fdb.put_batch(vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k20".to_vec(), Some(b"v20".to_vec())),
        ])
        .unwrap();

        // commit data without flushing to disk
        fdb.commit(vec![(b"height".to_vec(), Some(b"26".to_vec()))], false)
            .unwrap();

        //Create new Chain State with new database
        let cs = chain_state::ChainState::new(fdb, "test_db".to_string());

        assert_eq!(cs.exists(&b"k10".to_vec()).unwrap(), true);
        assert_eq!(cs.exists(&b"k20".to_vec()).unwrap(), true);
        assert_eq!(cs.exists(&b"kN/A".to_vec()).unwrap(), false);
    }

    #[test]
    fn test_commit() {
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");

        let mut index = 0;
        let batch = vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k20".to_vec(), Some(b"v20".to_vec())),
            (b"k30".to_vec(), Some(b"v30".to_vec())),
            (b"k40".to_vec(), Some(b"v40".to_vec())),
            (b"k50".to_vec(), Some(b"v50".to_vec())),
            (b"k60".to_vec(), Some(b"v60".to_vec())),
            (b"k70".to_vec(), Some(b"v70".to_vec())),
            (b"k80".to_vec(), Some(b"v80".to_vec())),
        ];
        let batch_clone = batch.clone();

        //Create new Chain State with new database
        let mut cs = chain_state::ChainState::new(fdb, "test_db".to_string());

        // Commit batch to db, in production the flush would be true
        let result = cs.commit(batch, 55, false).unwrap();
        assert_eq!(result.1, 55);

        let mut func_iter = |entry: KValue| {
            //Assert Keys are equal
            assert_eq!(entry.0, batch_clone[index].0);
            //Assert Values are equal
            assert_eq!(entry.1, batch_clone[index].1.clone().unwrap());

            index += 1;
            false
        };
        cs.iterate(
            &b"k10".to_vec(),
            &b"k80".to_vec(),
            IterOrder::Asc,
            &mut func_iter,
        );
    }

    #[test]
    fn test_aux_commit() {
        let path = thread::current().name().unwrap().to_owned();
        let mut fdb = TempFinDB::open(path).expect("failed to open db");

        // put data
        fdb.put_batch(vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k20".to_vec(), Some(b"v20".to_vec())),
        ])
        .unwrap();

        // commit data without flushing to disk
        fdb.commit(vec![(b"height".to_vec(), Some(b"25".to_vec()))], false)
            .unwrap();

        let cs = chain_state::ChainState::new(fdb, "test_db".to_string());
        let height_aux = cs.get_aux(&b"height".to_vec()).unwrap();
        let height = cs.get(&b"height".to_vec());

        //Make sure height was saved to auxiliary section of the db.
        assert_eq!(height_aux, Some(b"25".to_vec()));

        //Make sure the height isn't accessible from the main merkle tree.
        assert_ne!(height.unwrap(), Some(b"25".to_vec()));
    }

    #[test]
    fn test_root_hash() {
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");

        let batch = vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k70".to_vec(), Some(b"v70".to_vec())),
            (b"k80".to_vec(), Some(b"v80".to_vec())),
        ];

        //Create new Chain State with new database
        let mut cs = chain_state::ChainState::new(fdb, "test_db".to_string());
        let (root_hash1, _) = cs.commit(batch, 32, false).unwrap();

        let batch2 = vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k70".to_vec(), Some(b"v20".to_vec())),
            (b"k80".to_vec(), Some(b"v30".to_vec())),
        ];

        let (root_hash2, _) = cs.commit(batch2, 33, false).unwrap();
        assert_ne!(root_hash1, root_hash2);

        let (root_hash3, _) = cs.commit(vec![], 34, false).unwrap();
        assert_eq!(root_hash2, root_hash3);
    }

    #[test]
    fn test_height() {
        let path = thread::current().name().unwrap().to_owned();
        let fdb = TempFinDB::open(path).expect("failed to open db");

        let batch = vec![
            (b"k10".to_vec(), Some(b"v10".to_vec())),
            (b"k70".to_vec(), Some(b"v70".to_vec())),
            (b"k80".to_vec(), Some(b"v80".to_vec())),
        ];

        //Create new Chain State with new database
        let mut cs = chain_state::ChainState::new(fdb, "test_db".to_string());

        assert_eq!(cs.height().unwrap(), 0u64);

        let (_, _) = cs.commit(batch, 32, false).unwrap();
        assert_eq!(cs.height().unwrap(), 32);

        let batch = vec![(b"k10".to_vec(), Some(b"v60".to_vec()))];

        let (_, _) = cs.commit(batch, 33, false).unwrap();
        assert_eq!(cs.height().unwrap(), 33);

        let batch = vec![(b"k10".to_vec(), Some(b"v100".to_vec()))];

        let (_, _) = cs.commit(batch, 34, false).unwrap();
        assert_eq!(cs.height().unwrap(), 34);
    }
}
