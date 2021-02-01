#![allow(clippy::field_reassign_with_default)]
use abci::*;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{Operation, Transaction, TxnEffect, TxnSID};
use ledger::store::*;
use ledger::{error_location, sub_fail};
use ledger_api_service::RestfulApiService;
use log::info;
use protobuf::RepeatedField;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::Serialize;
use std::net::SocketAddr;
use std::path::Path;
use std::sync::{Arc, RwLock};
use std::thread;
use submission_api::SubmissionApi;
use submission_server::{convert_tx, SubmissionServer, TxnForward};
use utils::HashOf;
use zei::xfr::structs::{XfrAmount, XfrAssetType};

#[derive(Default)]
pub struct TendermintForward {
    tendermint_reply: String,
}

impl TxnForward for TendermintForward {
    fn forward_txn(&self, txn: Transaction) -> Result<(), PlatformError> {
        let txn_json = serde_json::to_string(&txn)?;
        info!("raw txn: {}", &txn_json);
        let txn_b64 = base64::encode_config(&txn_json.as_str(), base64::URL_SAFE);
        let json_rpc = format!(
            "{{\"jsonrpc\":\"2.0\",\"id\":\"anything\",\"method\":\"broadcast_tx_async\",\"params\": {{\"tx\": \"{}\"}}}}",
            &txn_b64
        );

        info!("forward_txn: \'{}\'", &json_rpc);
        let client = reqwest::blocking::Client::builder()
            .timeout(None)
            .build()
            .unwrap();
        let tendermint_reply = format!("http://{}", self.tendermint_reply);
        thread::spawn(move || {
            let _response = client
                .post(&tendermint_reply)
                .body(json_rpc)
                .header(reqwest::header::CONTENT_TYPE, "application/json")
                .send()
                .map_err(|e| sub_fail!(e))
                .unwrap();
        });
        info!("forward_txn call complete");
        Ok(())
    }
}

struct ABCISubmissionServer {
    la: Arc<RwLock<SubmissionServer<ChaChaRng, LedgerState, TendermintForward>>>,
}

impl ABCISubmissionServer {
    fn new(
        base_dir: Option<&Path>,
        tendermint_reply: String,
    ) -> Result<ABCISubmissionServer, PlatformError> {
        info!("tendermint reply url: {}", &tendermint_reply);
        let ledger_state = match base_dir {
            None => LedgerState::test_ledger(),
            Some(base_dir) => LedgerState::load_or_init(base_dir).unwrap(),
        };
        let prng = rand_chacha::ChaChaRng::from_entropy();
        Ok(ABCISubmissionServer {
            la: Arc::new(RwLock::new(SubmissionServer::new_no_auto_commit(
                prng,
                Arc::new(RwLock::new(ledger_state)),
                Some(TendermintForward { tendermint_reply }),
            )?)),
        })
    }
}

// TODO: implement abci hooks
impl abci::Application for ABCISubmissionServer {
    fn info(&mut self, _req: &RequestInfo) -> ResponseInfo {
        let mut resp = ResponseInfo::new();
        info!("locking for read: {}", error_location!());
        if let Ok(la) = self.la.read() {
            info!("locking state for read: {}", error_location!());
            if let Ok(state) = la.get_committed_state().read() {
                let commitment = state.get_state_commitment();
                if commitment.1 > 0 {
                    let tendermint_height = commitment.1 + state.get_pulse_count();
                    resp.set_last_block_height(tendermint_height as i64);
                    resp.set_last_block_app_hash(commitment.0.as_ref().to_vec());
                }
                info!("app hash: {:?}", resp.get_last_block_app_hash());
                info!("unlocking state for read: {}", error_location!());
            }
            info!("unlocking for read: {}", error_location!());
        }
        resp
    }

    fn check_tx(&mut self, req: &RequestCheckTx) -> ResponseCheckTx {
        // Get the Tx [u8] and convert to u64
        let mut resp = ResponseCheckTx::new();
        info!(
            "Transaction to check: \"{}\"",
            &std::str::from_utf8(req.get_tx()).unwrap_or("invalid format")
        );

        if let Some(tx) = convert_tx(req.get_tx()) {
            info!("converted: {:?}", tx);
            if TxnEffect::compute_effect(tx).is_err() {
                resp.set_code(1);
                resp.set_log(String::from("Check failed"));
            }
            info!("done check_tx");
        } else {
            resp.set_code(1);
            resp.set_log(String::from("Could not unpack transaction"));
        }

        resp
    }

    fn deliver_tx(&mut self, req: &RequestDeliverTx) -> ResponseDeliverTx {
        // Get the Tx [u8]
        info!(
            "Transaction to cache: \"{}\"",
            &std::str::from_utf8(req.get_tx()).unwrap_or("invalid format")
        );

        let mut resp = ResponseDeliverTx::new();
        if let Some(tx) = convert_tx(req.get_tx()) {
            info!("converted: {:?}", tx);
            info!("locking for write: {}", error_location!());
            if let Ok(mut la) = self.la.write() {
                // set attr(tags) if any
                let attr = gen_tendermint_attr(&tx);
                if 0 < attr.len() {
                    resp.set_events(attr);
                }

                info!("locked for write");
                la.cache_transaction(tx);
                info!("unlocking for write: {}", error_location!());

                return resp;
            }
        }
        resp.set_code(1);
        resp.set_log(String::from("Failed to deliver transaction!"));
        resp
    }

    fn begin_block(&mut self, _req: &RequestBeginBlock) -> ResponseBeginBlock {
        info!("locking for write: {}", error_location!());
        if let Ok(mut la) = self.la.write() {
            if !la.all_commited() {
                assert!(la.block_pulse_count() > 0);
                info!(
                    "begin_block: continuation, block pulse count is {}",
                    la.block_pulse_count()
                );
            } else {
                info!("begin_block: new block");
                la.begin_block();
            }
            info!("unlocking for write: {}", error_location!());
        }
        ResponseBeginBlock::new()
    }

    fn end_block(&mut self, _req: &RequestEndBlock) -> ResponseEndBlock {
        // TODO: this should propagate errors instead of panicking
        if let Ok(mut la) = self.la.write() {
            if la.block_txn_count() == 0 {
                info!("end_block: pulsing block");
                la.pulse_block();
            } else if !la.all_commited() {
                info!("end_block: ending block");
                if let Err(e) = la.end_block() {
                    info!("end_block failure: {:?}", e);
                }
            }
        }
        ResponseEndBlock::new()
    }

    fn commit(&mut self, _req: &RequestCommit) -> ResponseCommit {
        // Tendermint does not accept an error return type here.
        let error_commitment = (HashOf::new(&None), 0);
        let mut r = ResponseCommit::new();
        info!("locking for read: {}", error_location!());
        if let Ok(la) = self.la.read() {
            // la.begin_commit();
            info!("locking state for read: {}", error_location!());
            let commitment = if let Ok(state) = la.get_committed_state().read() {
                let ret = state.get_state_commitment();
                info!("unlocking state for read: {}", error_location!());
                ret
            } else {
                error_commitment
            };
            // la.end_commit();
            info!("commit: hash is {:?}", commitment.0.as_ref());
            r.set_data(commitment.0.as_ref().to_vec());
            info!("unlocking for read: {}", error_location!());
        }
        r
    }
}

fn main() {
    // Tendermint ABCI port
    flexi_logger::Logger::with_env().start().unwrap();
    info!(concat!(
        "Build: ",
        env!("VERGEN_SHA_SHORT"),
        " ",
        env!("VERGEN_BUILD_DATE")
    ));
    let base_dir = std::env::var_os("LEDGER_DIR").filter(|x| !x.is_empty());
    let base_dir = base_dir.as_ref().map(Path::new);

    let tendermint_port = std::env::var_os("TENDERMINT_PORT").filter(|x| !x.is_empty());
    let tendermint_port = tendermint_port
        .and_then(|x| x.into_string().ok())
        .unwrap_or_else(|| "26657".into());

    let tendermint_host = std::env::var_os("TENDERMINT_HOST").filter(|x| !x.is_empty());
    let tendermint_host = tendermint_host
        .and_then(|x| x.into_string().ok())
        .unwrap_or_else(|| "localhost".into());

    let app = ABCISubmissionServer::new(
        base_dir,
        format!("{}:{}", tendermint_host, tendermint_port),
    )
    .unwrap();
    let submission_server = Arc::clone(&app.la);
    let cloned_lock = { submission_server.read().unwrap().borrowable_ledger_state() };

    let host = std::env::var_os("SERVER_HOST")
        .filter(|x| !x.is_empty())
        .unwrap_or_else(|| "localhost".into());
    let host2 = host.clone();
    let submission_port = std::env::var_os("SUBMISSION_PORT")
        .filter(|x| !x.is_empty())
        .unwrap_or_else(|| "8669".into());
    let ledger_port = std::env::var_os("LEDGER_PORT")
        .filter(|x| !x.is_empty())
        .unwrap_or_else(|| "8668".into());
    thread::spawn(move || {
        let submission_api = SubmissionApi::create(
            submission_server,
            host.to_str().unwrap(),
            submission_port.to_str().unwrap(),
        )
        .unwrap();
        println!("Starting submission service");
        match submission_api.run() {
            Ok(_) => println!("Successfully ran submission service"),
            Err(_) => println!("Error running submission service"),
        }
    });

    thread::spawn(move || {
        let query_service = RestfulApiService::create(
            cloned_lock,
            host2.to_str().unwrap(),
            ledger_port.to_str().unwrap(),
        )
        .unwrap();
        println!("Starting ledger service");
        match query_service.run() {
            Ok(_) => println!("Successfully ran validator"),
            Err(_) => println!("Error running validator"),
        }
    });

    let abci_host = std::env::var_os("ABCI_HOST").filter(|x| !x.is_empty());
    let abci_host = abci_host
        .and_then(|x| x.into_string().ok())
        .unwrap_or_else(|| "0.0.0.0".into());
    let abci_port = std::env::var_os("ABCI_PORT").filter(|x| !x.is_empty());
    let abci_port = abci_port
        .and_then(|x| x.into_string().ok())
        .unwrap_or_else(|| "26658".into());

    // TODO: pass the address and port in on the command line
    let addr_str = format!("{}:{}", abci_host, abci_port);
    let addr: SocketAddr = addr_str.parse().expect("Unable to parse socket address");

    // handle SIGINT signal
    ctrlc::set_handler(move || {
        std::process::exit(0);
    })
    .expect("Error setting Ctrl-C handler");

    println!("Starting ABCI service");
    abci::run(addr, app);
}

/////////////////////////////////////////////////////////////////////////////////

/// generate attr(tags) for index-ops of tendermint
///   - "tx.exist" => "y"
///   - "addr.from" => "Json<TagAttr>"
///   - "addr.to" => "Json<TagAttr>"
///   - "addr.from.<addr>" => "y"
///   - "addr.to.<addr>" => "y"
fn gen_tendermint_attr(tx: &Transaction) -> RepeatedField<Event> {
    let mut res = vec![];

    // index txs without block info
    let mut ev = Event::new();
    ev.set_field_type("tx".to_owned());

    let mut kv = vec![Pair::new()];
    kv[0].set_key("prehash".as_bytes().to_vec());
    kv[0].set_value(hex::encode(tx.hash(TxnSID(0))).into_bytes());

    ev.set_attributes(RepeatedField::from_vec(kv));
    res.push(ev);

    let (from, to) = gen_tendermint_attr_addr(tx);

    // `from` maybe empty, but `to` must be **NOT**
    if !to.is_empty() {
        let mut ev = Event::new();
        ev.set_field_type("addr".to_owned());

        let mut kv = vec![Pair::new(), Pair::new()];
        kv[0].set_key("from".as_bytes().to_vec());
        kv[0].set_value(serde_json::to_vec(&from).unwrap());
        kv[1].set_key("to".as_bytes().to_vec());
        kv[1].set_value(serde_json::to_vec(&to).unwrap());

        ev.set_attributes(RepeatedField::from_vec(kv));
        res.push(ev);

        macro_rules! index_addr {
            ($attr: expr, $ty: expr) => {
                let kv = $attr
                    .into_iter()
                    .map(|i| {
                        let mut p = Pair::new();
                        p.set_key(i.addr.into_bytes());
                        p.set_value("y".as_bytes().to_vec());
                        p
                    })
                    .collect::<Vec<_>>();

                if !kv.is_empty() {
                    let mut ev = Event::new();
                    ev.set_field_type($ty.to_owned());
                    ev.set_attributes(RepeatedField::from_vec(kv));
                    res.push(ev);
                }
            };
        }

        index_addr!(from, "addr.from");
        index_addr!(to, "addr.to");
    }

    RepeatedField::from_vec(res)
}

// collect informations of inputs and outputs
// # return: ([from ...], [to ...])
fn gen_tendermint_attr_addr(tx: &Transaction) -> (Vec<TagAttr>, Vec<TagAttr>) {
    tx.body
        .operations
        .iter()
        .fold((vec![], vec![]), |mut base, new| {
            macro_rules! append_attr {
                // trasfer\bind\release
                ($data: expr, $direction: tt, $idx: tt) => {
                    $data.body.transfer.$direction.iter().for_each(|i| {
                        let mut attr = TagAttr::default();
                        attr.addr = wallet::public_key_to_bech32(&i.public_key);
                        if let XfrAssetType::NonConfidential(ty) = i.asset_type {
                            attr.asset_type = Some(hex::encode(&ty.0[..]));
                        }
                        if let XfrAmount::NonConfidential(am) = i.amount {
                            attr.asset_amount = Some(am);
                        }
                        base.$idx.push(attr);
                    });
                };
                // define\issue\AIR\memo
                ($data: expr) => {
                    let mut attr = TagAttr::default();
                    attr.addr = wallet::public_key_to_bech32(&$data.pubkey);
                    base.0.push(attr);
                };
            }

            match new {
                Operation::TransferAsset(d) => {
                    append_attr!(d, inputs, 0);
                    append_attr!(d, outputs, 1);
                }
                Operation::BindAssets(d) => {
                    append_attr!(d, inputs, 0);
                    append_attr!(d, outputs, 1);
                }
                Operation::ReleaseAssets(d) => {
                    append_attr!(d, inputs, 0);
                    append_attr!(d, outputs, 1);
                }
                Operation::DefineAsset(d) => {
                    append_attr!(d);
                }
                Operation::IssueAsset(d) => {
                    append_attr!(d);
                }
                Operation::AIRAssign(d) => {
                    append_attr!(d);
                }
                Operation::UpdateMemo(d) => {
                    append_attr!(d);
                }
                Operation::KVStoreUpdate(_) => {}
            }

            base
        })
}

#[derive(Serialize, Default)]
struct TagAttr {
    // FRA address
    addr: String,
    // hex.encode(asset_type)
    asset_type: Option<String>,
    asset_amount: Option<u64>,
}
