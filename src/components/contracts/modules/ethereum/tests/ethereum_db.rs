use baseapp::BaseApp;
use ethereum::{TransactionAction, TransactionSignature, TransactionV0};
use fp_core::context::Context;
use fp_storage::{Borrow, BorrowMut, RwLock};
use fp_types::crypto::HA256;
use fp_types::{H256, U256};
use module_ethereum::storage::TransactionIndex;
use sha3::{Digest, Keccak256};
use std::env::temp_dir;
use std::sync::Arc;
use std::time::SystemTime;
use storage::db::{FinDB, RocksDB};
use storage::state::ChainState;

fn setup() -> Context {
    let time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let mut path = temp_dir();
    path.push(format!("temp-findora-db–{}", time));

    let fdb = FinDB::open(path).unwrap();
    let chain_state = Arc::new(RwLock::new(ChainState::new(
        fdb,
        "temp_db".to_string(),
        100,
    )));

    let mut rocks_path = temp_dir();
    rocks_path.push(format!("temp-rocks-db–{}", time));

    let rdb = RocksDB::open(rocks_path).unwrap();
    let chain_db = Arc::new(RwLock::new(ChainState::new(
        rdb,
        "temp_rocks_db".to_string(),
        0,
    )));

    Context::new(chain_state, chain_db)
}

#[test]
fn test_eth_db_migrate_txn_index() {
    let mut ctx = setup();

    //Create txn hashes, block numbers and indices and save them to chain-state
    let mut txns = Vec::with_capacity(5);
    for i in 0..5 {
        let txn = TransactionV0 {
            nonce: U256::from(i),
            gas_price: Default::default(),
            gas_limit: Default::default(),
            action: TransactionAction::Create,
            value: Default::default(),
            input: vec![],
            signature: TransactionSignature::new(27, H256::random(), H256::random())
                .unwrap(),
        };

        let transaction_hash =
            H256::from_slice(Keccak256::digest(&rlp::encode(&txn)).as_slice());
        //Save Transaction index
        txns.push((HA256::new(transaction_hash), (U256::from(i), i)));
        let _ = TransactionIndex::insert(
            ctx.state.write().borrow_mut(),
            &HA256::new(transaction_hash),
            &(U256::from(i), i),
        );
    }

    //Call migrate on ethereum module.
    let _ = module_ethereum::App::<BaseApp>::migrate(ctx.borrow_mut());

    //Confirm transaction index values were migrated to rocksdb instance from the context.
    for txn in txns {
        let value: Option<(U256, u32)> =
            TransactionIndex::get(ctx.db.read().borrow(), &txn.0);
        assert!(value.is_some());
        assert_eq!(value.unwrap(), txn.1);
    }
}
