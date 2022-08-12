use crate::storage::*;
use crate::App;
use fin_db::{FinDB, RocksDB};
use fp_core::{account::SmartAccount, context::Context};
use fp_storage::{Borrow, BorrowMut};
use fp_traits::account::AccountAsset;
use fp_types::crypto::Address;
use fp_types::U256;
use parking_lot::RwLock;
use rand_chacha::rand_core::SeedableRng;
use rand_chacha::ChaChaRng;
use std::{env::temp_dir, sync::Arc, time::SystemTime};
use storage::state::ChainState;
use zei::xfr::sig::XfrKeyPair;

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
fn test_accounts_set_get() {
    //Setup database
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let address = Address::from(key.pub_key);

    //Generate SmartAccount
    let account = SmartAccount {
        balance: 123.into(),
        ..Default::default()
    };

    //Call set and get
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address, &account).is_ok()
    );
    assert_eq!(
        account,
        AccountStore::get(ctx.state.read().borrow(), &address).unwrap()
    );
    assert!(ctx.state.write().commit(1).is_ok());
    assert_eq!(
        account,
        AccountStore::get(ctx.state.read().borrow(), &address).unwrap()
    );
}

#[test]
fn test_account_of() {
    //Setup database
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let addr = Address::from(key.pub_key);

    //Generate SmartAccount
    let sa1 = SmartAccount {
        balance: 123.into(),
        ..Default::default()
    };

    //Call set and account_of
    AccountStore::insert(ctx.state.write().borrow_mut(), &addr, &sa1).unwrap();
    assert_eq!(App::<()>::account_of(&ctx, &addr, None), Some(sa1.clone()));
    assert_eq!(App::<()>::account_of(&ctx, &addr, Some(2)), None);

    //Verify after commit block 1
    assert!(ctx.state.write().commit(1).is_ok());
    assert_eq!(App::<()>::account_of(&ctx, &addr, None), Some(sa1.clone()));
    assert_eq!(
        App::<()>::account_of(&ctx, &addr, Some(0)),
        Some(sa1.clone())
    );
    assert_eq!(
        App::<()>::account_of(&ctx, &addr, Some(1)),
        Some(sa1.clone())
    );

    //Update SmartAccount
    let sa2 = SmartAccount {
        balance: 456.into(),
        ..Default::default()
    };

    //Call set and account_of with version
    AccountStore::insert(ctx.state.write().borrow_mut(), &addr, &sa2).unwrap();
    assert_eq!(App::<()>::account_of(&ctx, &addr, None), Some(sa2.clone()));

    //Verify after commit block 2
    assert!(ctx.state.write().commit(2).is_ok());
    assert_eq!(App::<()>::account_of(&ctx, &addr, None), Some(sa2.clone()));
    assert_eq!(
        App::<()>::account_of(&ctx, &addr, Some(0)),
        Some(sa2.clone())
    );
    assert!(App::<()>::account_of(&ctx, &addr, Some(1)) == Some(sa1.clone()));
    assert!(App::<()>::account_of(&ctx, &addr, Some(2)) == Some(sa2.clone()));

    //Update SmartAccount again
    let sa3 = SmartAccount {
        balance: 789.into(),
        ..Default::default()
    };

    //Call set and account_of with version again
    AccountStore::insert(ctx.state.write().borrow_mut(), &addr, &sa3).unwrap();
    assert_eq!(App::<()>::account_of(&ctx, &addr, None), Some(sa3.clone()));

    //Verify after commit block 3
    assert!(ctx.state.write().commit(3).is_ok());
    assert_eq!(App::<()>::account_of(&ctx, &addr, None), Some(sa3.clone()));
    assert_eq!(
        App::<()>::account_of(&ctx, &addr, Some(0)),
        Some(sa3.clone())
    );
    assert_eq!(App::<()>::account_of(&ctx, &addr, Some(1)), Some(sa1));
    assert_eq!(App::<()>::account_of(&ctx, &addr, Some(2)), Some(sa2));
    assert_eq!(App::<()>::account_of(&ctx, &addr, Some(3)), Some(sa3));
}

#[test]
fn test_account_balance() {
    //Setup database
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let address = Address::from(key.pub_key);

    //Generate SmartAccount
    let account = SmartAccount {
        balance: 123.into(),
        ..Default::default()
    };

    //Call set and balance
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address, &account).is_ok()
    );
    assert_eq!(account.balance, App::<()>::balance(&ctx, &address));
    assert!(ctx.state.write().commit(3).is_ok());
    assert_eq!(account.balance, App::<()>::balance(&ctx, &address));
}

#[test]
fn test_account_nonce() {
    //Setup database
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let address = Address::from(key.pub_key);

    //Generate SmartAccount
    let account = SmartAccount {
        nonce: 100001.into(),
        ..Default::default()
    };

    //Call set and nonce
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address, &account).is_ok()
    );
    assert_eq!(account.nonce, App::<()>::nonce(&ctx, &address));
    assert!(ctx.state.write().commit(4).is_ok());
    assert_eq!(account.nonce, App::<()>::nonce(&ctx, &address));
}

#[test]
fn test_account_inc_nonce() {
    //Setup database
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let address = Address::from(key.pub_key);

    //Generate SmartAccount
    let account = SmartAccount {
        nonce: 100001.into(),
        ..Default::default()
    };

    //Call set and inc_nonce, commit and check nonce again
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address, &account).is_ok()
    );
    assert_eq!(
        account.nonce + 1,
        App::<()>::inc_nonce(&ctx, &address).unwrap()
    );
    assert!(ctx.state.write().commit(3).is_ok());
    assert_eq!(account.nonce + 1, App::<()>::nonce(&ctx, &address));
}

#[test]
fn test_account_transfer() {
    //Setup database
    let ctx = setup();

    //Generate Addresses
    let mut prng = ChaChaRng::from_entropy();
    let key1 = XfrKeyPair::generate(&mut prng);
    let key2 = XfrKeyPair::generate(&mut prng);
    let address1 = Address::from(key1.pub_key);
    let address2 = Address::from(key2.pub_key);

    //Generate accounts with different amounts
    let mut acct1 = SmartAccount::default();
    let mut acct2 = SmartAccount::default();
    acct1.balance = 300.into();
    acct2.balance = 200.into();

    //Setup accounts in database
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address1, &acct1).is_ok()
    );
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address2, &acct2).is_ok()
    );

    //Transfer 100 from acct2 to acct1
    assert!(App::<()>::transfer(&ctx, &address2, &address1, 100.into()).is_ok());
    assert_eq!(App::<()>::balance(&ctx, &address2), 100.into());
    assert_eq!(App::<()>::balance(&ctx, &address1), 400.into());

    //Transfer 500 from acct2 to acct1 - Should fail
    assert!(App::<()>::transfer(&ctx, &address2, &address1, 500.into()).is_err());

    //Transfer 300 from acct1 to acct2
    assert!(App::<()>::transfer(&ctx, &address1, &address2, 400.into()).is_ok());
    assert_eq!(App::<()>::balance(&ctx, &address2), 500.into());
    assert_eq!(App::<()>::balance(&ctx, &address1), 0.into());
}

#[test]
fn test_account_mint() {
    //Setup db
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let address = Address::from(key.pub_key);

    //Generate SmartAccount
    let account = SmartAccount {
        balance: 100.into(),
        ..Default::default()
    };

    //Call set and inc_nonce, commit and check nonce again
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address, &account).is_ok()
    );

    //MintOutputs FRA for account
    assert!(App::<()>::mint(&ctx, &address, 500.into()).is_ok());
    assert_eq!(App::<()>::balance(&ctx, &address), 600.into());
}

#[test]
fn test_account_burn() {
    //Setup db
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let address = Address::from(key.pub_key);

    //Generate SmartAccount
    let account = SmartAccount {
        balance: 500.into(),
        ..Default::default()
    };

    //Add account to database
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address, &account).is_ok()
    );
    assert!(TotalIssuance::put(ctx.state.write().borrow_mut(), &500.into()).is_ok());

    //burn FRA
    assert!(App::<()>::burn(&ctx, &address, 200.into()).is_ok());
    assert_eq!(App::<()>::balance(&ctx, &address), 300.into());
}

#[test]
fn test_account_withdraw() {
    //Setup db
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let address = Address::from(key.pub_key);

    //Generate SmartAccount
    let account = SmartAccount {
        balance: 500.into(),
        ..Default::default()
    };

    //Add account to database
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address, &account).is_ok()
    );

    //Withdraw some funds
    assert!(App::<()>::withdraw(&ctx, &address, 300.into()).is_ok());
    assert_eq!(App::<()>::balance(&ctx, &address), 200.into());

    //Withdraw more funds than available
    assert!(App::<()>::withdraw(&ctx, &address, 600.into()).is_err());
}

#[test]
fn test_account_refund() {
    //Setup db
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let address = Address::from(key.pub_key);

    //Generate SmartAccount
    let account = SmartAccount {
        nonce: U256::zero(),
        balance: 500.into(),
        reserved: 1000.into(),
    };

    //Add account to database
    assert!(
        AccountStore::insert(ctx.state.write().borrow_mut(), &address, &account).is_ok()
    );

    //Refund an amount to address
    assert!(App::<()>::refund(&ctx, &address, 700.into()).is_ok());
    assert_eq!(App::<()>::balance(&ctx, &address), 1200.into());
    assert_eq!(App::<()>::reserved_balance(&ctx, &address), 300.into());
}
