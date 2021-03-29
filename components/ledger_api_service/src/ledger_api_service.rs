#![deny(warnings)]

extern crate actix_rt;
extern crate actix_web;
extern crate ledger;
extern crate serde_json;

use actix_cors::Cors;
use actix_web::{dev, error, middleware, test, web, App, HttpResponse, HttpServer};
use ledger::data_model::*;
use ledger::store::{ArchiveAccess, LedgerAccess, LedgerState};
use ledger::{inp_fail, ser_fail};
use log::warn;
use parking_lot::RwLock;
use ruc::*;
use serde::Serialize;
use sparse_merkle_tree::Key;
use std::marker::{Send, Sync};
use std::sync::Arc;
use utils::{http_get_request, HashOf, NetworkRoute, SignatureOf};
use zei::xfr::sig::XfrPublicKey;

use futures::executor;
use log::info;

pub struct RestfulApiService {
    web_runtime: actix_rt::SystemRunner,
}

// Ping route to check for liveness of API
#[allow(clippy::unnecessary_wraps)]
async fn ping() -> actix_web::Result<String> {
    Ok("success".into())
}

/// Returns the git commit hash and commit date of this build
#[allow(clippy::unnecessary_wraps)]
async fn version() -> actix_web::Result<String> {
    Ok(format!(
        "Build: {} {}",
        option_env!("VERGEN_SHA_SHORT_EXTERN").unwrap_or(env!("VERGEN_SHA_SHORT")),
        env!("VERGEN_BUILD_DATE")
    ))
}

// Future refactor:
// Merge query functions
//
// Query functions for LedgerAccess are very similar, especially these three:
//   query_asset
//   query_policy
//   query_contract
// If we add more functions with the similar pattern, it will be good to merge them

pub async fn query_utxo<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<AuthenticatedUtxo>>
where
    LA: LedgerAccess,
{
    // TODO noah figure out how to make bitmap serialization not require a mutable ref
    // https://bugtracker.findora.org/issues/165
    let mut writer = data.write();
    if let Ok(txo_sid) = info.parse::<u64>() {
        if let Some(txo) = writer.get_utxo(TxoSID(txo_sid)) {
            Ok(web::Json(txo))
        } else {
            Err(actix_web::error::ErrorNotFound(
                "Specified txo does not currently exist.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid txo sid encoding",
        ))
    }
}

pub async fn query_asset_issuance_num<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<u64>>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&*info) {
        if let Some(iss_num) = reader.get_issuance_num(&token_code) {
            Ok(web::Json(iss_num))
        } else {
            Err(actix_web::error::ErrorNotFound(
                "Specified asset definition does not currently exist.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid asset definition encoding.",
        ))
    }
}

pub async fn query_utxos<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<Option<AuthenticatedUtxo>>>>
where
    LA: LedgerAccess,
{
    let mut writer = data.write();
    if let Ok(txo_sid_list) = info.parse::<TxoSIDList>() {
        if txo_sid_list.0.len() > 10 || txo_sid_list.0.len() == 0 {
            return Err(actix_web::error::ErrorBadRequest("Invalid Query List"));
        }
        Ok(web::Json(writer.get_utxos(txo_sid_list)))
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid txo sid encoding for list of sid",
        ))
    }
}

pub async fn query_asset<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<AssetType>>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&*info) {
        if let Some(asset) = reader.get_asset_type(&token_code) {
            Ok(web::Json(asset.clone()))
        } else {
            Err(actix_web::error::ErrorNotFound(
                "Specified asset definition does not currently exist.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid asset definition encoding.",
        ))
    }
}

pub async fn query_txn<AA>(
    data: web::Data<Arc<RwLock<AA>>>,
    info: web::Path<String>,
) -> actix_web::Result<String>
where
    AA: ArchiveAccess,
{
    let reader = data.read();
    if let Ok(txn_sid) = info.parse::<usize>() {
        if let Some(mut txn) = reader.get_transaction(TxnSID(txn_sid)) {
            txn.finalized_txn.set_txo_id();
            Ok(serde_json::to_string(&txn)?)
        } else {
            Err(actix_web::error::ErrorNotFound(
                "Specified transaction does not exist.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid txn sid encoding.",
        ))
    }
}

pub async fn query_public_key<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
) -> web::Json<XfrPublicKey>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    web::Json(*reader.public_key())
}

#[allow(clippy::type_complexity)]
pub async fn query_global_state<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
) -> web::Json<(
    HashOf<Option<StateCommitmentData>>,
    u64,
    SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
)>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    let (hash, seq_id) = reader.get_state_commitment();
    let sig = reader.sign_message(&(hash.clone(), seq_id));
    web::Json((hash, seq_id, sig))
}

pub async fn query_global_state_version<AA>(
    data: web::Data<Arc<RwLock<AA>>>,
    version: web::Path<u64>,
) -> web::Json<Option<HashOf<Option<StateCommitmentData>>>>
where
    AA: ArchiveAccess,
{
    let reader = data.read();
    let hash = reader.get_state_commitment_at_block_height(*version);
    web::Json(hash)
}

async fn query_blocks_since<AA>(
    data: web::Data<Arc<RwLock<AA>>>,
    block_id: web::Path<usize>,
) -> web::Json<Vec<(usize, Vec<FinalizedTransaction>)>>
where
    AA: ArchiveAccess,
{
    let reader = data.read();
    let mut ret = Vec::new();
    let blk_cnt = reader.get_block_count();

    // upper limit: 1000 txns of blocks per query
    const TXNS_TO_CUTOFF: usize = 1000;
    let mut txn_cnt = 0;
    for ix in block_id.into_inner()..blk_cnt {
        let sid = BlockSID(ix);
        if let Some(authenticated_block) = reader.get_block(sid) {
            txn_cnt += authenticated_block.block.txns.len();
            ret.push((sid.0, authenticated_block.block.txns.clone()));
        } else {
            warn!("query_blocks_since failed, range= [{}, {})", ix, blk_cnt);
            ret.clear();
            break;
        }
        // stop adding more blocks when reaches upper limit
        if txn_cnt >= TXNS_TO_CUTOFF {
            break;
        }
    }
    web::Json(ret)
}

pub async fn query_air<AA>(
    data: web::Data<Arc<RwLock<AA>>>,
    addr: web::Path<String>,
) -> actix_web::Result<web::Json<AuthenticatedAIRResult>>
where
    AA: ArchiveAccess,
{
    let reader = data.read();
    let key = addr.into_inner();
    let air_result = reader.get_air_data(&key);
    Ok(web::Json(air_result))
}

pub async fn query_kv<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    addr: web::Path<String>,
) -> actix_web::Result<web::Json<AuthenticatedKVLookup>>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    let key = Key::from_base64(&*addr)
        .c(d!())
        .map_err(|e| actix_web::error::ErrorBadRequest(e.generate_log()))?;
    let result = reader.get_kv_entry(key);
    Ok(web::Json(result))
}

async fn query_block_log<AA>(
    data: web::Data<Arc<RwLock<AA>>>,
) -> impl actix_web::Responder
where
    AA: ArchiveAccess,
{
    let reader = data.read();
    let mut res = String::new();
    res.push_str("<html><head><META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\"></head><body><table border=\"1\">");
    res.push_str("<tr>");
    res.push_str("<th>Block ID</th>");
    res.push_str("<th>Transactions</th>");
    res.push_str("</tr>");
    for ix in 0..reader.get_block_count() {
        let authenticated_block = reader.get_block(BlockSID(ix)).unwrap();
        res.push_str("<tr>");

        res.push_str(&format!("<td>{}</td>", ix));

        res.push_str("<td><table border=\"1\">");
        res.push_str("<tr>");
        res.push_str("<th>TXN ID</th>");
        res.push_str("<th>merkle id</th>");
        res.push_str("<th>Operations</th>");
        res.push_str("</tr>");

        for txn in authenticated_block.block.txns.iter() {
            res.push_str("<tr>");
            res.push_str(&format!("<td>{}</td>", txn.tx_id.0));
            res.push_str(&format!("<td>{}</td>", txn.merkle_id));
            res.push_str("<td>");
            res.push_str("<table border=\"1\">");
            for op in txn.txn.body.operations.iter() {
                res.push_str(&format!(
                    "<tr><td><pre>{}</pre></td></tr>",
                    serde_json::to_string_pretty(&op).unwrap()
                ));
            }
            res.push_str("</table></td>");
            res.push_str("</tr>");
        }

        res.push_str("</table></td></tr>");
    }
    res.push_str("</table></body></html>");
    HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body(res)
}

async fn query_utxo_map<AA>(
    data: web::Data<Arc<RwLock<AA>>>,
) -> actix_web::Result<String>
where
    AA: ArchiveAccess,
{
    let mut reader = data.write();

    let vec = reader.serialize_utxo_map();
    Ok(serde_json::to_string(&vec)?)
}

async fn query_utxo_map_checksum<AA>(
    data: web::Data<Arc<RwLock<AA>>>,
    info: web::Path<String>,
) -> actix_web::Result<String>
where
    AA: ArchiveAccess,
{
    if let Ok(version) = info.parse::<u64>() {
        let reader = data.read();

        if let Some(vec) = reader.get_utxo_checksum(version) {
            Ok(serde_json::to_string(&vec)?)
        } else {
            Err(actix_web::error::ErrorNotFound(
                "That version is unavailable.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid version encoding.",
        ))
    }
}

#[allow(unused)]
fn parse_blocks(block_input: String) -> Option<Vec<usize>> {
    let blocks = block_input.split(',');
    let mut result = Vec::new();

    for block_str in blocks {
        if let Ok(block_usize) = block_str.parse::<usize>() {
            result.push(block_usize);
        } else {
            return None;
        }
    }

    Some(result)
}

#[allow(unused)]
async fn query_utxo_partial_map<AA>(
    data: web::Data<Arc<RwLock<AA>>>,
    info: web::Path<String>,
) -> actix_web::Result<String>
where
    AA: ArchiveAccess,
{
    // TODO(joe?): Implement this
    Err(actix_web::error::ErrorBadRequest("unimplemented"))
    // if let Some(block_list) = parse_blocks(info.to_string()) {
    //   let mut reader = data.write().c(d!())?;

    //   if let Some(vec) = reader.get_utxos(block_list) {
    //     Ok(serde_json::to_string(&vec)?)
    //   } else {
    //     Err(actix_web::error::ErrorNotFound("The map is unavailable."))
    //   }
    // } else {
    //   Err(actix_web::error::ErrorBadRequest("Invalid block list encoding."))
    // }
}

enum ServiceInterface {
    LedgerAccess,
    ArchiveAccess,
}

pub enum LedgerAccessRoutes {
    UtxoSid,
    UtxoSidList,
    AssetIssuanceNum,
    AssetToken,
    PublicKey,
    GlobalState,
    KVLookup,
}

impl NetworkRoute for LedgerAccessRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            LedgerAccessRoutes::UtxoSid => "utxo_sid",
            LedgerAccessRoutes::UtxoSidList => "utxo_sid_list",
            LedgerAccessRoutes::AssetIssuanceNum => "asset_issuance_num",
            LedgerAccessRoutes::AssetToken => "asset_token",
            LedgerAccessRoutes::PublicKey => "public_key",
            LedgerAccessRoutes::GlobalState => "global_state",
            LedgerAccessRoutes::KVLookup => "kv_lookup",
        };
        "/".to_owned() + endpoint
    }
}

pub enum LedgerArchiveRoutes {
    TxnSid,
    AirAddress,
    BlockLog,
    GlobalStateVersion,
    BlocksSince,
    UtxoMap,
    UtxoMapChecksum,
    UtxoPartialMap,
}

impl NetworkRoute for LedgerArchiveRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            LedgerArchiveRoutes::TxnSid => "txn_sid",
            LedgerArchiveRoutes::AirAddress => "air_address",
            LedgerArchiveRoutes::BlockLog => "block_log",
            LedgerArchiveRoutes::BlocksSince => "blocks_since",
            LedgerArchiveRoutes::GlobalStateVersion => "global_state_version",
            LedgerArchiveRoutes::UtxoMap => "utxo_map",
            LedgerArchiveRoutes::UtxoMapChecksum => "utxo_map_checksum",
            LedgerArchiveRoutes::UtxoPartialMap => "utxo_partial_map",
        };
        "/".to_owned() + endpoint
    }
}

trait Route {
    fn set_route<LA: 'static + LedgerAccess + ArchiveAccess + Sync + Send>(
        self,
        service_interface: ServiceInterface,
    ) -> Self;

    fn set_route_for_ledger_access<LA: 'static + LedgerAccess + Sync + Send>(
        self,
    ) -> Self;

    fn set_route_for_archive_access<AA: 'static + ArchiveAccess + Sync + Send>(
        self,
    ) -> Self;
}

impl<T, B> Route for App<T, B>
where
    B: actix_web::dev::MessageBody,
    T: actix_service::ServiceFactory<
        Config = (),
        Request = dev::ServiceRequest,
        Response = dev::ServiceResponse<B>,
        Error = error::Error,
        InitError = (),
    >,
{
    // Call the appropraite function depending on the interface
    fn set_route<LA: 'static + LedgerAccess + ArchiveAccess + Sync + Send>(
        self,
        service_interface: ServiceInterface,
    ) -> Self {
        match service_interface {
            ServiceInterface::LedgerAccess => self.set_route_for_ledger_access::<LA>(),
            ServiceInterface::ArchiveAccess => self.set_route_for_archive_access::<LA>(),
        }
    }

    // Set routes for the LedgerAccess interface
    fn set_route_for_ledger_access<LA: 'static + LedgerAccess + Sync + Send>(
        self,
    ) -> Self {
        self.route(
            &LedgerAccessRoutes::UtxoSid.with_arg_template("sid"),
            web::get().to(query_utxo::<LA>),
        )
        .route(
            &LedgerAccessRoutes::UtxoSidList.with_arg_template("sid_list"),
            web::get().to(query_utxos::<LA>),
        )
        .route(
            &LedgerAccessRoutes::AssetIssuanceNum.with_arg_template("code"),
            web::get().to(query_asset_issuance_num::<LA>),
        )
        .route(
            &LedgerAccessRoutes::AssetToken.with_arg_template("code"),
            web::get().to(query_asset::<LA>),
        )
        .route(
            &LedgerAccessRoutes::PublicKey.route(),
            web::get().to(query_public_key::<LA>),
        )
        .route(
            &LedgerAccessRoutes::GlobalState.route(),
            web::get().to(query_global_state::<LA>),
        )
        .route(
            &LedgerAccessRoutes::KVLookup.with_arg_template("addr"),
            web::get().to(query_kv::<LA>),
        )
    }

    // Set routes for the ArchiveAccess interface
    fn set_route_for_archive_access<AA: 'static + ArchiveAccess + Sync + Send>(
        self,
    ) -> Self {
        self.route(
            &LedgerArchiveRoutes::TxnSid.with_arg_template("sid"),
            web::get().to(query_txn::<AA>),
        )
        .route(
            &LedgerArchiveRoutes::AirAddress.with_arg_template("key"),
            web::get().to(query_air::<AA>),
        )
        .route(
            &LedgerArchiveRoutes::BlockLog.route(),
            web::get().to(query_block_log::<AA>),
        )
        .route(
            &LedgerArchiveRoutes::GlobalStateVersion.with_arg_template("version"),
            web::get().to(query_global_state_version::<AA>),
        )
        .route(
            &LedgerArchiveRoutes::BlocksSince.with_arg_template("block_sid"),
            web::get().to(query_blocks_since::<AA>),
        )
        .route(
            &LedgerArchiveRoutes::UtxoMap.route(),
            web::get().to(query_utxo_map::<AA>),
        )
        .route(
            &LedgerArchiveRoutes::UtxoMapChecksum.route(),
            web::get().to(query_utxo_map_checksum::<AA>),
        )
        .route(
            &LedgerArchiveRoutes::UtxoPartialMap.with_arg_template("sidlist"),
            web::get().to(query_utxo_partial_map::<AA>),
        )
    }
}

impl RestfulApiService {
    pub fn create<LA: 'static + LedgerAccess + ArchiveAccess + Sync + Send>(
        ledger_access: Arc<RwLock<LA>>,
        host: &str,
        port: &str,
    ) -> Result<RestfulApiService> {
        let web_runtime = actix_rt::System::new("findora API");

        HttpServer::new(move || {
            App::new()
                .wrap(middleware::Logger::default())
                .wrap(Cors::permissive().supports_credentials())
                .data(ledger_access.clone())
                .route("/ping", web::get().to(ping))
                .route("/version", web::get().to(version))
                .set_route::<LA>(ServiceInterface::LedgerAccess)
                .set_route::<LA>(ServiceInterface::ArchiveAccess)
        })
        .bind(&format!("{}:{}", host, port))
        .c(d!())?
        .run();

        info!("RestfulApi server started");

        Ok(RestfulApiService { web_runtime })
    }
    // call from a thread; this will block.
    pub fn run(self) -> Result<()> {
        self.web_runtime.run().c(d!())
    }
}

pub trait RestfulLedgerAccess {
    fn get_utxo(&self, addr: TxoSID) -> Result<AuthenticatedUtxo>;

    fn get_utxos(&self, addr: TxoSIDList) -> Result<Vec<Option<AuthenticatedUtxo>>>;

    fn get_issuance_num(&self, code: &AssetTypeCode) -> Result<u64>;

    fn get_asset_type(&self, code: &AssetTypeCode) -> Result<AssetType>;

    #[allow(clippy::type_complexity)]
    fn get_state_commitment(
        &self,
    ) -> Result<(
        HashOf<Option<StateCommitmentData>>,
        u64,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    )>;

    fn get_block_commit_count(&self) -> Result<u64>;

    fn get_kv_entry(&self, addr: Key) -> Result<AuthenticatedKVLookup>;

    fn public_key(&self) -> Result<XfrPublicKey>;

    fn sign_message<T: Serialize + serde::de::DeserializeOwned>(
        &self,
        msg: &T,
    ) -> Result<SignatureOf<T>>;
}

pub trait RestfulArchiveAccess {
    fn get_blocks_since(
        &self,
        addr: BlockSID,
    ) -> Result<Vec<(usize, Vec<FinalizedTransaction>)>>;
    // For debug purposes. Returns the location of ledger being communicated with.
    fn get_source(&self) -> String;
}

impl RestfulArchiveAccess for MockLedgerClient {
    fn get_blocks_since(
        &self,
        _addr: BlockSID,
    ) -> Result<Vec<(usize, Vec<FinalizedTransaction>)>> {
        unimplemented!();
    }

    fn get_source(&self) -> String {
        unimplemented!();
    }
}

pub struct MockLedgerClient {
    mock_ledger: Arc<RwLock<LedgerState>>,
}

impl MockLedgerClient {
    pub fn new(state: &Arc<RwLock<LedgerState>>) -> Self {
        MockLedgerClient {
            mock_ledger: Arc::clone(state),
        }
    }
}

impl RestfulLedgerAccess for MockLedgerClient {
    fn get_utxo(&self, addr: TxoSID) -> Result<AuthenticatedUtxo> {
        let mut app = executor::block_on(test::init_service(
            App::new().data(Arc::clone(&self.mock_ledger)).route(
                &LedgerAccessRoutes::UtxoSid.with_arg_template("sid"),
                web::get().to(query_utxo::<LedgerState>),
            ),
        ));
        let req = test::TestRequest::get()
            .uri(&LedgerAccessRoutes::UtxoSid.with_arg(&addr.0))
            .to_request();

        Ok(executor::block_on(test::read_response_json(&mut app, req)))
    }
    fn get_utxos(&self, addr: TxoSIDList) -> Result<Vec<Option<AuthenticatedUtxo>>> {
        let mut app = executor::block_on(test::init_service(
            App::new().data(Arc::clone(&self.mock_ledger)).route(
                &LedgerAccessRoutes::UtxoSidList.with_arg_template("sid_list"),
                web::get().to(query_utxos::<LedgerState>),
            ),
        ));
        let req = test::TestRequest::get()
            .uri(&LedgerAccessRoutes::UtxoSidList.with_arg(&addr))
            .to_request();

        Ok(executor::block_on(test::read_response_json(&mut app, req)))
    }

    fn get_issuance_num(&self, code: &AssetTypeCode) -> Result<u64> {
        let mut app = executor::block_on(test::init_service(
            App::new().data(Arc::clone(&self.mock_ledger)).route(
                &LedgerAccessRoutes::AssetIssuanceNum.with_arg_template("code"),
                web::get().to(query_asset_issuance_num::<LedgerState>),
            ),
        ));
        let req = test::TestRequest::get()
            .uri(&LedgerAccessRoutes::AssetIssuanceNum.with_arg(&code.to_base64()))
            .to_request();
        Ok(executor::block_on(test::read_response_json(&mut app, req)))
    }

    fn get_asset_type(&self, code: &AssetTypeCode) -> Result<AssetType> {
        let mut app = executor::block_on(test::init_service(
            App::new().data(Arc::clone(&self.mock_ledger)).route(
                &LedgerAccessRoutes::AssetToken.with_arg_template("code"),
                web::get().to(query_asset::<LedgerState>),
            ),
        ));
        let req = test::TestRequest::get()
            .uri(&LedgerAccessRoutes::AssetToken.with_arg(&code.to_base64()))
            .to_request();
        Ok(executor::block_on(test::read_response_json(&mut app, req)))
    }

    #[allow(clippy::type_complexity)]
    fn get_state_commitment(
        &self,
    ) -> Result<(
        HashOf<Option<StateCommitmentData>>,
        u64,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    )> {
        let mut app = executor::block_on(test::init_service(
            App::new().data(Arc::clone(&self.mock_ledger)).route(
                &LedgerAccessRoutes::GlobalState.route(),
                web::get().to(query_global_state::<LedgerState>),
            ),
        ));
        let req = test::TestRequest::get()
            .uri(&LedgerAccessRoutes::GlobalState.route())
            .to_request();
        Ok(executor::block_on(test::read_response_json(&mut app, req)))
    }

    fn get_block_commit_count(&self) -> Result<u64> {
        unimplemented!();
    }

    fn get_kv_entry(&self, _addr: Key) -> Result<AuthenticatedKVLookup> {
        unimplemented!();
    }

    fn public_key(&self) -> Result<XfrPublicKey> {
        unimplemented!();
    }

    fn sign_message<T: Serialize + serde::de::DeserializeOwned>(
        &self,
        _msg: &T,
    ) -> Result<SignatureOf<T>> {
        unimplemented!();
    }
}

pub struct ActixLedgerClient {
    port: usize,
    host: String,
    protocol: String,
}

impl ActixLedgerClient {
    pub fn new(port: usize, host: &str, protocol: &str) -> Self {
        ActixLedgerClient {
            port,
            host: String::from(host),
            protocol: String::from(protocol),
        }
    }
}

impl RestfulArchiveAccess for ActixLedgerClient {
    fn get_blocks_since(
        &self,
        addr: BlockSID,
    ) -> Result<Vec<(usize, Vec<FinalizedTransaction>)>> {
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            LedgerArchiveRoutes::BlocksSince.with_arg(&addr.0)
        );
        let text = http_get_request(&query).c(d!(inp_fail!()))?;
        serde_json::from_str::<Vec<(usize, Vec<FinalizedTransaction>)>>(&text)
            .c(d!(ser_fail!()))
    }

    fn get_source(&self) -> String {
        format!("{}://{}:{}", self.protocol, self.host, self.port)
    }
}

impl RestfulLedgerAccess for ActixLedgerClient {
    fn get_utxo(&self, addr: TxoSID) -> Result<AuthenticatedUtxo> {
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            LedgerAccessRoutes::UtxoSid.with_arg(&addr.0)
        );
        let text = http_get_request(&query).c(d!(inp_fail!()))?;
        serde_json::from_str::<AuthenticatedUtxo>(&text).c(d!(ser_fail!()))
    }

    fn get_utxos(&self, addr: TxoSIDList) -> Result<Vec<Option<AuthenticatedUtxo>>> {
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            LedgerAccessRoutes::UtxoSidList.with_arg(&addr)
        );
        let text = http_get_request(&query).c(d!(inp_fail!()))?;
        serde_json::from_str::<Vec<Option<AuthenticatedUtxo>>>(&text).c(d!(ser_fail!()))
    }

    fn get_issuance_num(&self, code: &AssetTypeCode) -> Result<u64> {
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            LedgerAccessRoutes::AssetIssuanceNum.with_arg(&code.to_base64())
        );
        let text = http_get_request(&query).c(d!(inp_fail!()))?;
        serde_json::from_str::<u64>(&text).c(d!(ser_fail!()))
    }

    fn get_asset_type(&self, code: &AssetTypeCode) -> Result<AssetType> {
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            LedgerAccessRoutes::AssetToken.with_arg(&code.to_base64())
        );
        let text = http_get_request(&query).c(d!(inp_fail!()))?;
        serde_json::from_str::<AssetType>(&text).c(d!(ser_fail!()))
    }

    #[allow(clippy::type_complexity)]
    fn get_state_commitment(
        &self,
    ) -> Result<(
        HashOf<Option<StateCommitmentData>>,
        u64,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    )> {
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            LedgerAccessRoutes::GlobalState.route()
        );
        let text = http_get_request(&query).c(d!(inp_fail!()))?;
        serde_json::from_str::<_>(&text).c(d!(ser_fail!()))
    }

    fn get_block_commit_count(&self) -> Result<u64> {
        unimplemented!();
    }

    fn get_kv_entry(&self, _addr: Key) -> Result<AuthenticatedKVLookup> {
        unimplemented!();
    }

    fn public_key(&self) -> Result<XfrPublicKey> {
        let query = format!(
            "{}://{}:{}{}",
            self.protocol,
            self.host,
            self.port,
            LedgerAccessRoutes::PublicKey.route()
        );
        let text = http_get_request(&query).c(d!(inp_fail!()))?;
        serde_json::from_str::<_>(&text).c(d!(ser_fail!()))
    }

    fn sign_message<T: Serialize + serde::de::DeserializeOwned>(
        &self,
        _msg: &T,
    ) -> Result<SignatureOf<T>> {
        unimplemented!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_service::Service;
    use actix_web::{web, App};
    use ledger::data_model::{Operation, Transaction, TxnEffect};
    use ledger::store::helpers::*;
    use ledger::store::{LedgerState, LedgerUpdate};
    use rand_chacha::ChaChaRng;
    use rand_core::SeedableRng;

    #[test]
    fn test_query_utxo() {}

    #[test]
    fn test_query_txn() {}

    #[test]
    fn test_query_policy() {}

    #[test]
    fn test_query_proof() {}

    #[test]
    fn test_query_contract() {}

    #[test]
    #[allow(clippy::type_complexity)]
    fn test_query_state_commitment() {
        let mut prng = ChaChaRng::from_seed([0u8; 32]);
        let mut state = LedgerState::test_ledger();
        let (_, seq_id) = state.get_state_commitment();
        let mut tx = Transaction::from_seq_id(seq_id);

        let token_code1 = AssetTypeCode::gen_random();
        let keypair = build_keys(&mut prng);

        let asset_body = asset_creation_body(
            &token_code1,
            keypair.get_pk_ref(),
            AssetRules::default(),
            None,
            None,
        );
        let asset_create = asset_creation_operation(&asset_body, &keypair);
        tx.body
            .operations
            .push(Operation::DefineAsset(asset_create));

        let effect = TxnEffect::compute_effect(tx).unwrap();
        {
            let mut block = state.start_block().unwrap();
            state.apply_transaction(&mut block, effect).unwrap();
            state.finish_block(block).unwrap();
        }

        let state_lock = Arc::new(RwLock::new(state));

        let mut app = executor::block_on(test::init_service(
            App::new()
                .data(state_lock.clone())
                .route(
                    "/global_state",
                    web::get().to(query_global_state::<LedgerState>),
                )
                .route(
                    "/global_state_version/{version}",
                    web::get().to(query_global_state_version::<LedgerState>),
                ),
        ));

        let req = test::TestRequest::get().uri("/global_state").to_request();

        let second_req = test::TestRequest::get()
            .uri("/global_state_version/1")
            .to_request();

        let state_reader = state_lock.read();
        let (comm1, idx, _sig): (
            _,
            _,
            SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
        ) = executor::block_on(test::read_response_json(&mut app, req));
        let comm2 = executor::block_on(test::read_response_json(&mut app, second_req));
        assert!((comm1, idx) == state_reader.get_state_commitment());
        assert!((comm2, idx) == state_reader.get_state_commitment());
    }

    #[test]
    // Tests that the server
    //  (a) responds with the same public key across a transaction
    //  (b) responds to /global_state with a response signed by the public
    //      key from /public_key
    #[allow(clippy::type_complexity)]
    fn test_query_public_key() {
        let mut prng = ChaChaRng::from_seed([0u8; 32]);
        let mut state = LedgerState::test_ledger();
        let (_, seq_id) = state.get_state_commitment();
        let mut tx = Transaction::from_seq_id(seq_id);

        let orig_key = *state.public_key();

        let token_code1 = AssetTypeCode::gen_random();
        let keypair = build_keys(&mut prng);

        let asset_body = asset_creation_body(
            &token_code1,
            keypair.get_pk_ref(),
            AssetRules::default(),
            None,
            None,
        );
        let asset_create = asset_creation_operation(&asset_body, &keypair);
        tx.body
            .operations
            .push(Operation::DefineAsset(asset_create));

        let effect = TxnEffect::compute_effect(tx).unwrap();
        {
            let mut block = state.start_block().unwrap();
            state.apply_transaction(&mut block, effect).unwrap();
            state.finish_block(block).unwrap();
        }

        let state_lock = Arc::new(RwLock::new(state));

        let mut app = executor::block_on(test::init_service(
            App::new()
                .data(state_lock.clone())
                .route(
                    "/global_state",
                    web::get().to(query_global_state::<LedgerState>),
                )
                .route(
                    "/public_key",
                    web::get().to(query_public_key::<LedgerState>),
                ),
        ));

        let req_pk = test::TestRequest::get().uri("/public_key").to_request();
        let req_comm = test::TestRequest::get().uri("/global_state").to_request();

        let state_reader = state_lock.read();
        let k: XfrPublicKey =
            executor::block_on(test::read_response_json(&mut app, req_pk));
        let (comm, idx, sig): (
            HashOf<Option<StateCommitmentData>>,
            u64,
            SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
        ) = executor::block_on(test::read_response_json(&mut app, req_comm));
        sig.verify(&k, &(comm, idx)).unwrap();
        assert!(k == orig_key);
        assert!(k == *state_reader.public_key());
    }

    #[test]
    fn test_query_asset() {
        let mut prng = ChaChaRng::from_entropy();
        let mut state = LedgerState::test_ledger();
        let (_, seq_id) = state.get_state_commitment();
        let mut tx = Transaction::from_seq_id(seq_id);

        let token_code1 = AssetTypeCode::gen_random();
        let keypair = build_keys(&mut prng);

        let asset_body = asset_creation_body(
            &token_code1,
            keypair.get_pk_ref(),
            AssetRules::default(),
            None,
            None,
        );
        let asset_create = asset_creation_operation(&asset_body, &keypair);
        tx.body
            .operations
            .push(Operation::DefineAsset(asset_create));

        let effect = TxnEffect::compute_effect(tx).unwrap();
        {
            let mut block = state.start_block().unwrap();
            state.apply_transaction(&mut block, effect).unwrap();
            state.finish_block(block).unwrap();
        }

        let mut app = executor::block_on(test::init_service(
            App::new().data(Arc::new(RwLock::new(state))).route(
                "/asset_token/{token}",
                web::get().to(query_asset::<LedgerState>),
            ),
        ));

        let req = test::TestRequest::get()
            .uri(&format!("/asset_token/{}", token_code1.to_base64()))
            .to_request();
        let resp = executor::block_on(app.call(req)).unwrap();

        assert!(resp.status().is_success());
    }
}
