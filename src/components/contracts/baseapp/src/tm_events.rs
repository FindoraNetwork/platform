use {
    crate::notify::Notifications,
    ethereum_types::H256,
    fp_utils::hashing::sha2_256,
    once_cell::sync::OnceCell,
    ruc::eg,
    serde::{Deserialize, Serialize},
    std::{
        mem::MaybeUninit,
        sync::{Arc, Mutex, Once},
        thread, time,
    },
};
static URL: OnceCell<String> = OnceCell::new();

#[inline(always)]
pub fn init_url(url: &str) -> ruc::Result<()> {
    URL.set(url.to_string()).map_err(|e| eg!(@e))
}

#[derive(Default, Serialize, Deserialize)]
pub struct NewPendingTxResult {
    pub n_txs: String,
    pub total: String,
    pub total_bytes: String,
    pub txs: Vec<String>,
}
#[derive(Default, Serialize, Deserialize)]
pub struct JsonResponse<T> {
    pub jsonrpc: String,
    pub id: String,
    pub result: T,
}

fn get_pending_hash() -> Result<Vec<H256>, attohttpc::Error> {
    let default_url = String::from("http://127.0.0.1:26658");
    let url = URL.get().unwrap_or(&default_url);
    let json = String::from(
        "{\"jsonrpc\":\"2.0\",\"id\":\"anything\",\"method\":\"unconfirmed_txs\",\"params\":{}}",
    );
    let mut pending_hash = vec![];

    attohttpc::post(url)
        .header(attohttpc::header::CONTENT_TYPE, "application/json")
        .text(json)
        .send()
        .map(|resp| {
            resp.json::<JsonResponse<NewPendingTxResult>>()
                .unwrap_or_default()
        })
        .map(|json_resp| {
            for tx in json_resp.result.txs {
                base64::decode(&tx)
                    .map(|bytes| {
                        let hasher = sha2_256(&bytes);
                        pending_hash.push(H256::from_slice(&hasher))
                    })
                    .unwrap_or_default();
            }
            pending_hash
        })
}

#[derive(Default, Serialize, Deserialize)]
pub struct ProtocolVersion {
    p2p: String,
    block: String,
    app: String,
}
#[derive(Default, Serialize, Deserialize)]
pub struct Other {
    tx_index: String,
    rpc_address: String,
}

#[derive(Default, Serialize, Deserialize)]
pub struct NodeInfo {
    protocol_version: ProtocolVersion,
    id: String,
    listen_addr: String,
    network: String,
    version: String,
    channels: String,
    moniker: String,
    other: Other,
}

#[derive(Default, Serialize, Deserialize)]
pub struct SyncInfo {
    latest_block_hash: String,
    latest_app_hash: String,
    latest_block_height: String,
    latest_block_time: String,
    earliest_block_hash: String,
    earliest_app_hash: String,
    earliest_block_height: String,
    earliest_block_time: String,
    catching_up: bool,
}

#[derive(Default, Serialize, Deserialize)]
pub struct PubKey {
    key_type: String,
    value: String,
}

#[derive(Default, Serialize, Deserialize)]
pub struct ValidatorInfo {
    address: String,
    pub_key: PubKey,
    voting_power: String,
}
#[derive(Default, Serialize, Deserialize)]
pub struct StatusResult {
    node_info: NodeInfo,
    sync_info: SyncInfo,
    validator_info: ValidatorInfo,
}
fn get_status() -> Result<bool, attohttpc::Error> {
    let default_url = String::from("http://127.0.0.1:26658");
    let url = URL.get().unwrap_or(&default_url);
    let json = String::from(
        "{\"jsonrpc\":\"2.0\",\"id\":\"anything\",\"method\":\"status\",\"params\":{}}",
    );
    attohttpc::post(url)
        .header(attohttpc::header::CONTENT_TYPE, "application/json")
        .text(json)
        .send()
        .map(|resp| {
            resp.json::<JsonResponse<StatusResult>>()
                .unwrap_or_default()
        })
        .map(|json_resp| json_resp.result.sync_info.catching_up)
}

#[derive(Default)]
pub struct PendingTx {
    millis: u64,
    pub event_notify: Arc<Notifications<H256>>,
    last_txhash: Arc<Mutex<Vec<H256>>>,
}
pub fn get_pendingtx() -> &'static Mutex<PendingTx> {
    static mut PENDINGTX: MaybeUninit<Mutex<PendingTx>> = MaybeUninit::uninit();
    static ONCE: Once = Once::new();
    ONCE.call_once(|| unsafe {
        let pt = PendingTx::new();
        pt.start_get_pending();
        PENDINGTX.as_mut_ptr().write(Mutex::new(pt));
    });
    unsafe { &*PENDINGTX.as_ptr() }
}

impl PendingTx {
    pub fn new() -> PendingTx {
        PendingTx {
            millis: 100,
            event_notify: Arc::new(Notifications::new()),
            last_txhash: Arc::new(Mutex::new(vec![])),
        }
    }
    pub fn start_get_pending(&self) {
        let ten_millis = time::Duration::from_millis(self.millis);
        let event_notify = self.event_notify.clone();
        let last_txhash = self.last_txhash.clone();
        thread::spawn(move || loop {
            thread::sleep(ten_millis);
            let pending_hashs = get_pending_hash().unwrap_or_default();
            let mut last = last_txhash.lock().unwrap();
            for hash in pending_hashs.clone() {
                if !last.contains(&hash) {
                    event_notify.notify(hash).unwrap_or_default();
                }
            }
            last.clear();
            last.extend(pending_hashs.into_iter());
        });
    }
}

#[derive(Default)]
pub struct SyncStatus {
    millis: u64,
    pub event_notify: Arc<Notifications<bool>>,
    is_first: Arc<Mutex<bool>>,
    last_status: Arc<Mutex<bool>>,
}
pub fn get_sync_status() -> &'static Mutex<SyncStatus> {
    static mut SYNC_STATUS: MaybeUninit<Mutex<SyncStatus>> = MaybeUninit::uninit();
    static ONCE: Once = Once::new();
    ONCE.call_once(|| unsafe {
        let pt = SyncStatus::new();
        pt.start_get_status();
        SYNC_STATUS.as_mut_ptr().write(Mutex::new(pt));
    });
    unsafe { &*SYNC_STATUS.as_ptr() }
}

impl SyncStatus {
    pub fn new() -> SyncStatus {
        SyncStatus {
            millis: 100,
            event_notify: Arc::new(Notifications::new()),
            is_first: Arc::new(Mutex::new(true)),
            last_status: Arc::new(Mutex::new(true)),
        }
    }
    pub fn start_get_status(&self) {
        let ten_millis = time::Duration::from_millis(self.millis);
        let event_notify = self.event_notify.clone();
        let last_status = self.last_status.clone();
        let is_first = self.is_first.clone();
        thread::spawn(move || loop {
            thread::sleep(ten_millis);
            let _ = get_status().map(|status| {
                let mut first = is_first.lock().unwrap();
                let mut last = last_status.lock().unwrap();
                if *last != status || (*first) {
                    event_notify.notify(status).unwrap_or_default();
                    *last = status;
                    if *first {
                        (*first) = false;
                    }
                }
            });
        });
    }
}
