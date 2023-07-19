#![allow(suspicious_double_ref_op)]

use baseapp::BaseApp;
use ethereum::{TransactionAction, TransactionSignature, TransactionV0};
use fin_db::{FinDB, RocksDB};
use fp_core::context::Context;
use fp_evm::BlockId;
use fp_storage::{Borrow, BorrowMut, RwLock};
use fp_types::crypto::HA256;
use fp_types::{H256, U256};
use module_ethereum::storage::TransactionIndex;
use module_ethereum::App;
use sha3::{Digest, Keccak256};
use std::{env::temp_dir, sync::Arc, time::SystemTime};
use storage::state::ChainState;

fn setup() -> Context {
    let time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let mut path = temp_dir();
    path.push(format!("temp-findora-db–{time}"));

    let fdb = FinDB::open(path).unwrap();
    let chain_state = Arc::new(RwLock::new(ChainState::new(
        fdb,
        "temp_db".to_string(),
        100,
    )));

    let mut rocks_path = temp_dir();
    rocks_path.push(format!("temp-rocks-db–{time}"));

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

// Set evm_first_block_height = 1 to run this test
#[ignore]
#[test]
fn test_eth_db_migrate_block_data() {
    let mut ctx = setup();
    let mut app = App::<BaseApp>::default();
    let mut blocks = Vec::new();

    //Store blocks with default state roots
    for i in 1..6 {
        let block_id = Some(BlockId::Number(U256::from(i)));
        let _ = app.store_block(ctx.borrow_mut(), U256::from(i));
        blocks.push(app.current_block(ctx.borrow(), block_id));
    }

    //Store blocks with random state roots
    for k in 6..11 {
        let block_id = Some(BlockId::Number(U256::from(k)));
        let _ = app.store_block(ctx.borrow_mut(), U256::from(k));
        blocks.push(app.current_block(ctx.borrow(), block_id));
    }

    //Add some data to chain-state
    let _ = ctx
        .state
        .write()
        .set("test_key".as_bytes(), "test_value".as_bytes().to_vec());
    ctx.state.write().commit_session();
    let _ = ctx.state.write().commit(10);
    let current_root_hash = ctx.state.read().root_hash();

    //Call migrate on ethereum module.
    let _ = module_ethereum::App::<BaseApp>::migrate(ctx.borrow_mut());

    //Confirm state root has shifted from block 6 to 5
    assert_eq!(
        blocks[5].as_ref().unwrap().header.state_root,
        app.current_block(ctx.borrow(), Some(BlockId::Number(U256::from(5))))
            .unwrap()
            .header
            .state_root
    );

    //Confirm state root of block 9 is equal to state root of previous block 10
    assert_eq!(
        blocks[9].as_ref().unwrap().header.state_root,
        app.current_block(ctx.borrow(), Some(BlockId::Number(U256::from(9))))
            .unwrap()
            .header
            .state_root
    );

    //Confirm state root of block 10 is equal to current_root_hash
    assert_eq!(
        H256::from_slice(current_root_hash.as_slice()),
        app.current_block(ctx.borrow(), Some(BlockId::Number(U256::from(10))))
            .unwrap()
            .header
            .state_root
    );

    //-----------------------------------------------------------------------------------------
    //Call migrate again on the app to make sure block state roots remain the same.
    //Migration of state root should only happen once.
    let _ = module_ethereum::App::<BaseApp>::migrate(ctx.borrow_mut());

    //Confirm state root has shifted from block 6 to 5
    assert_eq!(
        blocks[5].as_ref().unwrap().header.state_root,
        app.current_block(ctx.borrow(), Some(BlockId::Number(U256::from(5))))
            .unwrap()
            .header
            .state_root
    );

    //Confirm state root of block 9 is equal to state root of previous block 10
    assert_eq!(
        blocks[9].as_ref().unwrap().header.state_root,
        app.current_block(ctx.borrow(), Some(BlockId::Number(U256::from(9))))
            .unwrap()
            .header
            .state_root
    );

    //Confirm state root of block 10 is equal to current_root_hash
    assert_eq!(
        H256::from_slice(current_root_hash.as_slice()),
        app.current_block(ctx.borrow(), Some(BlockId::Number(U256::from(10))))
            .unwrap()
            .header
            .state_root
    );

    // println!(
    //     "BLOCKS: {:?}, number: {:?}",
    //     blocks[9].as_ref().unwrap().header.state_root,
    //     blocks[9].as_ref().unwrap().header.number,
    // );
    // println!(
    //     "BLOCKS db: {:?}, number {:?}",
    //     app.current_block(ctx.borrow(), Some(BlockId::Number(U256::from(9))))
    //         .unwrap()
    //         .header
    //         .state_root,
    //     app.current_block(ctx.borrow(), Some(BlockId::Number(U256::from(9))))
    //         .unwrap()
    //         .header
    //         .number,
    // );
}
