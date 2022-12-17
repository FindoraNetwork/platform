use {
    evm_exporter::{
        Block as EnterpriseBlock, Receipt as EnterpriseReceipt,
        State as EnterpriseState, TransactionStatus as EnterpriseTxState,
    },
    futures::executor::ThreadPool,
    lazy_static::lazy_static,
    primitive_types::{H160, H256, U256},
    redis::Client,
    ruc::*,
    std::collections::HashMap,
    std::sync::{Arc, Mutex},
};

pub use evm_exporter::{Getter, Setter};

pub type State = EnterpriseState;
pub type Block = EnterpriseBlock;
pub type Receipt = EnterpriseReceipt;
pub type TxState = EnterpriseTxState;

lazy_static! {
    pub static ref EXECUTOR: ThreadPool =
        ThreadPool::new().expect("Failed to create thread pool executor");
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
    pub static ref REDIS_POOL: Arc<r2d2::Pool<Client>> = Arc::new(gen_redis_pool());
    pub static ref WEB3_SERVICE_START_HEIGHT: u64 = load_start_height();
    pub static ref PENDING_CODE_MAP: Arc<Mutex<HashMap<H160, Vec<u8>>>> =
        Arc::new(Mutex::new(HashMap::new()));
    pub static ref PENDING_STATE_UPDATE_LIST: Arc<Mutex<Vec<State>>> =
        Arc::new(Mutex::new(vec![]));
    pub static ref REMOVE_PENDING_CODE_MAP: Arc<Mutex<Vec<H160>>> =
        Arc::new(Mutex::new(vec![]));
    pub static ref REMOVE_PENDING_STATE_UPDATE_LIST: Arc<Mutex<Vec<(H160, H256)>>> =
        Arc::new(Mutex::new(vec![]));
    pub static ref SAVE_HISTORY_NUM: u32 = get_save_history_num();
}

fn get_save_history_num() -> u32 {
    let num = std::env::var("SAVE_HISTORY_NUM").unwrap_or(String::from("10000000"));
    pnk!(num.parse().map_err(|e| eg!(e)))
}

fn gen_redis_pool() -> r2d2::Pool<Client> {
    let redis_addr = pnk!(std::env::var("REDIS_ADDR"));
    let client = pnk!(Client::open(redis_addr));
    let pool = pnk!(r2d2::Pool::builder().max_size(50).build(client));
    pool
}

fn load_start_height() -> u64 {
    let redis_pool = REDIS_POOL.clone();
    let mut conn = redis_pool.get().expect("get redis connect");
    let mut getter = Getter::new(&mut *conn, "evm".to_string());
    let last_height = getter.latest_height().expect("redis latest_height error");
    last_height as u64
}
