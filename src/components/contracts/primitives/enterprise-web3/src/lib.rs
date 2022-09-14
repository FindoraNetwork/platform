use evm_exporter::{
    Block as EnterpriseBlock, Exporter, Receipt as EnterpriseReceipt, Setter,
    State as EnterpriseState, TransactionStatus as EnterpriseTxState,
};
use lazy_static::lazy_static;
use primitive_types::{H160, U256};
use redis::{Client, Connection};
use ruc::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub type State = EnterpriseState;
pub type Block = EnterpriseBlock;
pub type Receipt = EnterpriseReceipt;
pub type TxState = EnterpriseTxState;

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
    static ref REDIS_CLIENT: Arc<Mutex<Client>> =
        Arc::new(Mutex::new(gen_redis_client()));
    pub static ref WEB3_SERVICE_START_HEIGHT: u64 = load_start_height();
}

fn gen_redis_client() -> Client {
    let redis_addr = std::env::var("REDIS_ADDR").unwrap();
    let client = Client::open(redis_addr).c(d!()).unwrap();
    client
}

fn load_start_height() -> u64 {
    let start_height =
        std::env::var("WEB3_SERVICE_START_HEIGHT").unwrap_or_else(|_|String::from("0"));
    start_height.parse::<u64>().unwrap()
}

pub fn get_exporter() -> Result<Exporter<Connection>> {
    let client = REDIS_CLIENT.lock().c(d!())?;
    let con = client.get_connection().c(d!())?;
    Ok(Exporter::new(con, "evm".to_string()))
}

pub fn setter() -> Result<Setter<Connection>> {
    let client = REDIS_CLIENT.lock().c(d!())?;
    let con = client.get_connection().c(d!())?;
    Ok(Setter::new(con, "evm".to_string()))
}
