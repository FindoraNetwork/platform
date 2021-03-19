#![deny(warnings)]

use ledger_api_service::ActixLedgerClient;
use log::{error, info};
use metrics_exporter_prometheus::PrometheusHandle;
use parking_lot::RwLock;
use query_api::QueryApi;
use query_server::QueryServer;
use std::sync::Arc;
use std::thread;
use std::time;
use utils::MetricsRenderer;

pub struct PromHandle(metrics_exporter_prometheus::PrometheusHandle);

impl PromHandle {
    pub fn new(h: PrometheusHandle) -> PromHandle {
        PromHandle(h)
    }
}

impl MetricsRenderer for PromHandle {
    fn rendered(&self) -> String {
        self.0.render()
    }
}

fn main() {
    let query_host = std::env::var_os("QUERY_SERVER_HOST")
        .filter(|x| !x.is_empty())
        .unwrap_or_else(|| "localhost".into());
    let ledger_host = std::env::var_os("LEDGER_HOST")
        .filter(|x| !x.is_empty())
        .unwrap_or_else(|| "localhost".into());
    let ledger_protocol = std::env::var_os("LEDGER_PROTOCOL")
        .filter(|x| !x.is_empty())
        .unwrap_or_else(|| "http".into());
    let query_port = std::env::var_os("QUERY_PORT")
        .filter(|x| !x.is_empty())
        .unwrap_or_else(|| "8667".into());
    let ledger_port: usize = std::env::var_os("LEDGER_PORT")
        .filter(|x| !x.is_empty())
        .unwrap_or_else(|| "8668".into())
        .to_str()
        .unwrap()
        .parse()
        .unwrap();

    let rest_client = ActixLedgerClient::new(
        ledger_port,
        ledger_host.to_str().unwrap(),
        ledger_protocol.to_str().unwrap(),
    );

    // Extract prometheus handle
    let builder = metrics_exporter_prometheus::PrometheusBuilder::new();
    let recorder = builder.build();
    let handle = PromHandle::new(recorder.handle());

    // Register recorder
    let _ = metrics::set_boxed_recorder(Box::new(recorder));

    // Create query api
    let query_server = QueryServer::new(rest_client, handle);
    let wrapped_server = Arc::new(RwLock::new(query_server));

    println!("Starting query service");
    QueryApi::create(
        wrapped_server.clone(),
        query_host.to_str().unwrap(),
        query_port.to_str().unwrap(),
    )
    .unwrap();

    // polling blocks
    loop {
        {
            let mut server = wrapped_server.write();
            match server.poll_new_blocks() {
                Ok(_) => info!("Block successfuly polled"),
                Err(_) => error!("Error fetching blocks"),
            }
        }
        let poll_time = time::Duration::from_millis(1000);
        thread::sleep(poll_time);
    }
}
