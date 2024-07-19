use evm_exporter::{
    Block as EnterpriseBlock, Getter, PgGetter, PgSetter, Receipt as EnterpriseReceipt,
    Setter, State as EnterpriseState, TransactionStatus as EnterpriseTxState,
};
use lazy_static::lazy_static;
use primitive_types::{H160, H256, U256};
use ruc::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub type State = EnterpriseState;
pub type Block = EnterpriseBlock;
pub type Receipt = EnterpriseReceipt;
pub type TxState = EnterpriseTxState;

pub struct AllowancesKey {
    pub owner_address: H160,
    pub spender_address: H160,
}

lazy_static! {
    pub static ref STATE_UPDATE_LIST: Arc<Mutex<Vec<State>>> =
        Arc::new(Mutex::new(vec![]));
    pub static ref NONCE_MAP: Arc<Mutex<HashMap<H160, U256>>> =
        Arc::new(Mutex::new(HashMap::new()));
    pub static ref BALANCE_MAP: Arc<Mutex<HashMap<H160, U256>>> =
        Arc::new(Mutex::new(HashMap::new()));
    pub static ref CODE_MAP: Arc<Mutex<HashMap<H160, Vec<u8>>>> =
        Arc::new(Mutex::new(HashMap::new()));
    pub static ref BLOCK: Arc<Mutex<Option<Block>>> = Arc::new(Mutex::new(None));
    pub static ref RECEIPTS: Arc<Mutex<Vec<Receipt>>> = Arc::new(Mutex::new(vec![]));
    pub static ref TXS: Arc<Mutex<Vec<TxState>>> = Arc::new(Mutex::new(vec![]));
    pub static ref REDIS_CLIENT: Arc<Mutex<dyn Setter>> =
        Arc::new(Mutex::new(gen_redis_client()));
    pub static ref WEB3_SERVICE_START_HEIGHT: u64 = load_start_height();
    pub static ref PENDING_CODE_MAP: Arc<Mutex<HashMap<H160, Vec<u8>>>> =
        Arc::new(Mutex::new(HashMap::new()));
    pub static ref PENDING_STATE_UPDATE_LIST: Arc<Mutex<Vec<State>>> =
        Arc::new(Mutex::new(vec![]));
    pub static ref REMOVE_PENDING_CODE_MAP: Arc<Mutex<Vec<H160>>> =
        Arc::new(Mutex::new(vec![]));
    pub static ref REMOVE_PENDING_STATE_UPDATE_LIST: Arc<Mutex<Vec<(H160, H256)>>> =
        Arc::new(Mutex::new(vec![]));
    pub static ref TOTAL_ISSUANCE: Arc<Mutex<Option<U256>>> = Arc::new(Mutex::new(None));
    pub static ref ALLOWANCES: Arc<Mutex<Vec<(AllowancesKey, U256)>>> =
        Arc::new(Mutex::new(Vec::new()));
}

fn gen_redis_client() -> dyn Setter {
    let uri = std::env::var("POSTGRES_URI").expect("loading env POSTGRES_URI failed");
    PgSetter::new(ConnectionType::Postgres(uri), String::new())
}

fn load_start_height() -> u64 {
    let uri = std::env::var("POSTGRES_URI").expect("loading env POSTGRES_URI failed");
    let getter: dyn Getter = PgGetter::new(ConnectionType::Postgres(uri), String::new());
    let last_height = getter.latest_height().expect("redis latest_height error");
    last_height as u64
}
