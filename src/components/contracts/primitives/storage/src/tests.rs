use crate::hash::*;
use crate::*;
use sha2::Digest;
use std::env::temp_dir;
use std::time::SystemTime;
use storage::db::TempFinDB;
use storage::state::{ChainState, State};

fn setup_temp_db() -> Arc<RwLock<State<TempFinDB>>> {
    let time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let mut path = temp_dir();
    path.push(format!("temp-findora-db–{}", time));
    let fdb = TempFinDB::open(path).unwrap();
    let chain_state = Arc::new(RwLock::new(ChainState::new(
        fdb,
        "temp_db".to_string(),
        100,
    )));
    Arc::new(RwLock::new(State::new(chain_state, false)))
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

    assert!(Number::put(state.clone(), &10).is_ok());

    assert_eq!(Number::get(state.clone()), Some(10));
    assert!(Number::exists(state.clone()));
    Number::delete(state.clone());
    assert_eq!(Number::get(state.clone()), None);
    assert!(!Number::exists(state));
}

#[test]
fn storage_map_test() {
    generate_storage!(Findora, Account => Map<String, u32>);

    assert_eq!(Account::module_prefix(), b"Findora");
    assert_eq!(Account::storage_prefix(), b"Account");

    let state = setup_temp_db();
    assert!(Account::insert(state.clone(), &"a".to_string(), &10).is_ok());
    assert!(Account::insert(state.clone(), &"b".to_string(), &20).is_ok());
    assert!(Account::insert(state.clone(), &"c".to_string(), &30).is_ok());

    assert_eq!(Account::get(state.clone(), &"a".to_string()), Some(10));
    assert!(Account::contains_key(state.clone(), &"a".to_string()));
    Account::remove(state.clone(), &"a".to_string());
    assert_eq!(Account::get(state.clone(), &"a".to_string()), None);
    assert!(!Account::contains_key(state.clone(), &"a".to_string()),);

    let kvs = Account::iterate(state.clone());
    assert_eq!(kvs, vec![("b".to_string(), 20), ("c".to_string(), 30)]);

    state.write().commit(1).unwrap();
    let kvs = Account::iterate(state);
    assert_eq!(kvs, vec![("b".to_string(), 20), ("c".to_string(), 30)]);
}

#[test]
fn storage_double_map_test() {
    generate_storage!(Findora, Data => DoubleMap<u32, u32, u32>);

    assert_eq!(Data::module_prefix(), b"Findora");
    assert_eq!(Data::storage_prefix(), b"Data");

    let state = setup_temp_db();
    assert!(Data::insert(state.clone(), &1, &2, &10).is_ok());
    assert!(Data::insert(state.clone(), &1, &3, &20).is_ok());
    assert!(Data::insert(state.clone(), &2, &3, &30).is_ok());
    assert!(Data::insert(state.clone(), &2, &4, &40).is_ok());

    assert_eq!(Data::get(state.clone(), &1, &2), Some(10));
    assert!(Data::contains_key(state.clone(), &1, &2));
    Data::remove(state.clone(), &1, &2);
    assert_eq!(Data::get(state.clone(), &1, &2), None);
    assert!(!Data::contains_key(state.clone(), &1, &2));

    let kvs = Data::iterate_prefix(state.clone(), &1);
    assert_eq!(kvs, vec![(3, 20)]);

    let kvs = Data::iterate_prefix(state.clone(), &2);
    assert_eq!(kvs, vec![(3, 30), (4, 40)]);

    Data::remove_prefix(state.clone(), &2);
    let kvs = Data::iterate_prefix(state.clone(), &2);
    assert_eq!(kvs, vec![]);

    state.write().commit(1).unwrap();
    let kvs = Data::iterate_prefix(state, &1);
    assert_eq!(kvs, vec![(3, 20)]);
}
