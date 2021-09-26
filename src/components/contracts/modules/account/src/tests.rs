use crate::storage::*;
use crate::App;
use fp_core::{account::SmartAccount, context::Context};
use fp_traits::account::AccountAsset;
use fp_types::actions::account::MintOutput;
use fp_types::crypto::Address;
use ledger::data_model::ASSET_TYPE_FRA;
use parking_lot::RwLock;
use rand_chacha::rand_core::SeedableRng;
use rand_chacha::ChaChaRng;
use std::env::temp_dir;
use std::sync::Arc;
use std::time::SystemTime;
use storage::db::FinDB;
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

    Context::new(chain_state)
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
        balance: 123,
        ..Default::default()
    };

    //Call set and get
    assert!(AccountStore::insert(ctx.store.clone(), &address, &account).is_ok());
    assert_eq!(
        account,
        AccountStore::get(ctx.store.clone(), &address).unwrap()
    );
    assert!(ctx.store.write().commit(1).is_ok());
    assert_eq!(account, AccountStore::get(ctx.store, &address).unwrap());
}

#[test]
fn test_account_of() {
    //Setup database
    let ctx = setup();

    //Generate Address
    let mut prng = ChaChaRng::from_entropy();
    let key = XfrKeyPair::generate(&mut prng);
    let address = Address::from(key.pub_key);

    //Generate SmartAccount
    let account = SmartAccount {
        balance: 123,
        ..Default::default()
    };

    //Call set and account_of
    assert!(AccountStore::insert(ctx.store.clone(), &address, &account).is_ok());
    assert_eq!(account, App::<()>::account_of(&ctx, &address).unwrap());
    assert!(ctx.store.write().commit(2).is_ok());
    assert_eq!(account, App::<()>::account_of(&ctx, &address).unwrap());
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
        balance: 123,
        ..Default::default()
    };

    //Call set and balance
    assert!(AccountStore::insert(ctx.store.clone(), &address, &account).is_ok());
    assert_eq!(account.balance, App::<()>::balance(&ctx, &address));
    assert!(ctx.store.write().commit(3).is_ok());
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
        nonce: 100001,
        ..Default::default()
    };

    //Call set and nonce
    assert!(AccountStore::insert(ctx.store.clone(), &address, &account).is_ok());
    assert_eq!(account.nonce, App::<()>::nonce(&ctx, &address));
    assert!(ctx.store.write().commit(4).is_ok());
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
        nonce: 100001,
        ..Default::default()
    };

    //Call set and inc_nonce, commit and check nonce again
    assert!(AccountStore::insert(ctx.store.clone(), &address, &account).is_ok());
    assert_eq!(
        account.nonce + 1,
        App::<()>::inc_nonce(&ctx, &address).unwrap()
    );
    assert!(ctx.store.write().commit(3).is_ok());
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
    acct1.balance = 300;
    acct2.balance = 200;

    //Setup accounts in database
    assert!(AccountStore::insert(ctx.store.clone(), &address1, &acct1).is_ok());
    assert!(AccountStore::insert(ctx.store.clone(), &address2, &acct2).is_ok());

    //Transfer 100 from acct2 to acct1
    assert!(App::<()>::transfer(&ctx, &address2, &address1, 100).is_ok());
    assert_eq!(100, App::<()>::balance(&ctx, &address2));
    assert_eq!(400, App::<()>::balance(&ctx, &address1));

    //Transfer 500 from acct2 to acct1 - Should fail
    assert!(App::<()>::transfer(&ctx, &address2, &address1, 500).is_err());

    //Transfer 300 from acct1 to acct2
    assert!(App::<()>::transfer(&ctx, &address1, &address2, 400).is_ok());
    assert_eq!(500, App::<()>::balance(&ctx, &address2));
    assert_eq!(0, App::<()>::balance(&ctx, &address1));
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
        balance: 100,
        ..Default::default()
    };

    //Call set and inc_nonce, commit and check nonce again
    assert!(AccountStore::insert(ctx.store.clone(), &address, &account).is_ok());

    //MintOutputs FRA for account
    assert!(App::<()>::mint(&ctx, &address, 500).is_ok());
    assert_eq!(600, App::<()>::balance(&ctx, &address));
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
        balance: 500,
        ..Default::default()
    };

    //Add account to database
    assert!(AccountStore::insert(ctx.store.clone(), &address, &account).is_ok());

    //burn FRA
    assert!(App::<()>::burn(&ctx, &address, 200).is_ok());
    assert_eq!(300, App::<()>::balance(&ctx, &address));
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
        balance: 500,
        ..Default::default()
    };

    //Add account to database
    assert!(AccountStore::insert(ctx.store.clone(), &address, &account).is_ok());

    //Withdraw some funds
    assert!(App::<()>::withdraw(&ctx, &address, 300).is_ok());
    assert_eq!(App::<()>::balance(&ctx, &address), 200);

    //Withdraw more funds than available
    assert!(App::<()>::withdraw(&ctx, &address, 600).is_err());
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
        balance: 500,
        ..Default::default()
    };

    //Add account to database
    assert!(AccountStore::insert(ctx.store.clone(), &address, &account).is_ok());

    //Refund an amount to address
    assert!(App::<()>::refund(&ctx, &address, 700).is_ok());
    assert_eq!(App::<()>::balance(&ctx, &address), 1200);
}

#[test]
fn test_add_mint() {
    //Setup state and db
    let ctx = setup();

    //Setup Output Vector
    let mut outputs = Vec::new();
    for n in 1..4 {
        let mut output = MintOutput {
            asset: ASSET_TYPE_FRA,
            amount: n * 100,
            ..Default::default()
        };

        let mut prng = ChaChaRng::from_entropy();
        output.target = XfrKeyPair::generate(&mut prng).pub_key;

        outputs.push(output)
    }

    //Add mint outputs and check if it was stored correctly
    let mut ref_outputs = outputs.clone();
    assert!(App::<()>::add_mint(&ctx, outputs).is_ok());
    assert_eq!(MintOutputs::get(ctx.store.clone()).unwrap(), ref_outputs);

    assert!(ctx.store.write().commit(100).is_ok());

    //Append mint outputs and read if it was stored
    let mut new_outputs = Vec::new();
    let mut output_new = MintOutput {
        asset: ASSET_TYPE_FRA,
        amount: 416,
        ..Default::default()
    };

    let mut prng = ChaChaRng::from_entropy();
    output_new.target = XfrKeyPair::generate(&mut prng).pub_key;
    new_outputs.push(output_new);

    //Outputs are moved, need to make a copy to assert
    let mut ref_new_outputs = new_outputs.clone();
    assert!(App::<()>::add_mint(&ctx, new_outputs).is_ok());

    //Confirm new output was appended to output list
    ref_outputs.append(&mut ref_new_outputs);
    assert_eq!(MintOutputs::get(ctx.store).unwrap(), ref_outputs);
}

#[test]
fn test_consume_mint() {
    //Setup state and db
    let ctx = setup();
    const OUTPUTS_LEN: usize = 5;

    //Setup Output Vector
    let mut outputs = Vec::new();
    for n in 1..OUTPUTS_LEN {
        let mut output = MintOutput {
            asset: ASSET_TYPE_FRA,
            amount: (n * 100) as u64,
            ..Default::default()
        };

        let mut prng = ChaChaRng::from_entropy();
        output.target = XfrKeyPair::generate(&mut prng).pub_key;

        outputs.push(output)
    }
    let mut ref_outputs = outputs.clone();

    //Add mint outputs
    assert!(App::<()>::add_mint(&ctx, outputs).is_ok());

    //Consume mint outputs < total len
    let consumed = App::<()>::consume_mint(&ctx, 1).unwrap();
    let avail_outputs = ref_outputs.split_off(1);
    assert_eq!(consumed, ref_outputs);

    //Consume mint outputs >= total len
    let consumed = App::<()>::consume_mint(&ctx, OUTPUTS_LEN).unwrap();
    assert_eq!(consumed, avail_outputs);
}
