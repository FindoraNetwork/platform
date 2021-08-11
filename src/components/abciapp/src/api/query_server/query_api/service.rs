use super::{
    query_server::{QueryServer, BLOCK_CREATED},
    QueryApi,
};
use ledger::store::LedgerState;
use metrics_exporter_prometheus::PrometheusHandle;
use parking_lot::RwLock;
use ruc::*;
use std::{sync::Arc, thread};
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

pub fn start_query_server(
    ledger: Arc<RwLock<LedgerState>>,
    host: &str,
    port: u16,
) -> Result<Arc<RwLock<QueryServer<PromHandle>>>> {
    // Extract prometheus handle
    let builder = metrics_exporter_prometheus::PrometheusBuilder::new();
    let recorder = builder.build();
    let handle = PromHandle::new(recorder.handle());

    // Register recorder
    let _ = metrics::set_boxed_recorder(Box::new(recorder));

    let qs = Arc::new(RwLock::new(QueryServer::new(ledger, handle)));
    QueryApi::create(Arc::clone(&qs), host, port)
        .c(d!())
        .map(|_| {
            let qs1 = Arc::clone(&qs);
            thread::spawn(move || loop {
                let mut created = BLOCK_CREATED.0.lock();
                if !*created {
                    BLOCK_CREATED.1.wait(&mut created);
                }
                qs1.write().update();
                *created = false;
            });
            qs
        })
}
