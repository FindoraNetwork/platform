use crate::hash::*;
use crate::*;
use sha2::Digest;
use std::env::temp_dir;
use std::time::SystemTime;
use storage::state::{ChainState, State};
use temp_db::TempFinDB;

fn setup_temp_db() -> Arc<RwLock<State<TempFinDB>>> {
    let time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let mut path = temp_dir();
    path.push(format!("temp-findora-db–{time}"));
    let fdb = TempFinDB::open(path).unwrap();
    let chain_state = Arc::new(RwLock::new(ChainState::new(
        fdb,
        "temp_db".to_string(),
        100,
    )));
    Arc::new(RwLock::new(State::new(chain_state, true)))
}

#[test]
fn storage_hasher_works() {
    let text = b"hello world";

    assert_eq!(sha2::Sha256::digest(text).as_slice(), Sha256::hash(text));
}

#[test]
fn storage_value_works() {
    generate_storage!(Findora, Number => Value<u32>);

    assert_eq!(Number::module_prefix(), b"Findora");
    assert_eq!(Number::storage_prefix(), b"Number");
    assert_eq!(
        <Number as StorageHashedKey>::store_key().as_slice(),
        sha2::Sha256::digest(b"FindoraNumber").as_slice()
    );

    let state = setup_temp_db();

    assert!(Number::put(state.write().borrow_mut(), &10).is_ok());

    assert_eq!(Number::get(&state.read()), Some(10));
    assert!(Number::exists(&state.read()));
    Number::delete(state.write().borrow_mut());
    assert_eq!(Number::get(&state.read()), None);
    assert!(!Number::exists(&state.read()));
}

#[test]
fn storage_map_test() {
    generate_storage!(Findora, Account => Map<String, u32>);

    assert_eq!(Account::module_prefix(), b"Findora");
    assert_eq!(Account::storage_prefix(), b"Account");

    let state = setup_temp_db();
    assert!(
        Account::insert(state.write().borrow_mut(), &"abc".to_string(), &10).is_ok()
    );
    assert!(Account::insert(state.write().borrow_mut(), &"b".to_string(), &20).is_ok());
    assert!(Account::insert(state.write().borrow_mut(), &"c".to_string(), &30).is_ok());

    assert_eq!(Account::get(&state.read(), &"abc".to_string()), Some(10));
    assert_eq!(
        Account::get_unique_prefix(&state.read(), &"ab".to_string()),
        Some(("abc".to_string(), 10))
    );
    assert!(Account::contains_key(&state.read(), &"abc".to_string()));
    Account::remove(state.write().borrow_mut(), &"abc".to_string());
    assert_eq!(Account::get(&state.read(), &"abc".to_string()), None);
    assert!(!Account::contains_key(&state.read(), &"abc".to_string()),);

    let kvs = Account::iterate(&state.read());
    assert_eq!(kvs, vec![("b".to_string(), 20), ("c".to_string(), 30)]);

    state.write().commit(1).unwrap();
    let kvs = Account::iterate(&state.read());
    assert_eq!(kvs, vec![("b".to_string(), 20), ("c".to_string(), 30)]);
}

#[test]
fn storage_double_map_test() {
    generate_storage!(Findora, Data => DoubleMap<u32, u32, u32>);

    assert_eq!(Data::module_prefix(), b"Findora");
    assert_eq!(Data::storage_prefix(), b"Data");

    let state = setup_temp_db();
    assert!(Data::insert(state.write().borrow_mut(), &1, &2, &10).is_ok());
    assert!(Data::insert(state.write().borrow_mut(), &1, &3, &20).is_ok());
    assert!(Data::insert(state.write().borrow_mut(), &2, &3, &30).is_ok());
    assert!(Data::insert(state.write().borrow_mut(), &2, &4, &40).is_ok());

    assert_eq!(Data::get(&state.read(), &1, &2), Some(10));
    assert!(Data::contains_key(&state.read(), &1, &2));
    Data::remove(state.write().borrow_mut(), &1, &2);
    assert_eq!(Data::get(&state.read(), &1, &2), None);
    assert!(!Data::contains_key(&state.read(), &1, &2));

    let kvs = Data::iterate_prefix(&state.read(), &1);
    assert_eq!(kvs, vec![(3, 20)]);

    let kvs = Data::iterate_prefix(&state.read(), &2);
    assert_eq!(kvs, vec![(3, 30), (4, 40)]);

    Data::remove_prefix(state.write().borrow_mut(), &2);
    let kvs = Data::iterate_prefix(&state.read(), &2);
    assert_eq!(kvs, vec![]);

    state.write().commit(1).unwrap();
    let kvs = Data::iterate_prefix(&state.read(), &1);
    assert_eq!(kvs, vec![(3, 20)]);
}
