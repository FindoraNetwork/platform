//!
//! new query server
//!

use {
    super::{
        server::{QueryServer, BLOCK_CREATED},
        QueryApi,
    },
    ledger::store::LedgerState,
    parking_lot::RwLock,
    ruc::*,
    std::{sync::Arc, thread},
    tendermint_rpc::HttpClient,
};

pub(crate) fn start_query_server(
    tm_client: &Arc<HttpClient>,
    ledger: Arc<RwLock<LedgerState>>,
    addrs: &[(&str, u16)],
) -> Result<Arc<RwLock<QueryServer>>> {
    let qs = Arc::new(RwLock::new(QueryServer::new(&tm_client, ledger)));
    let qs1 = Arc::clone(&qs);
    let qs2 = Arc::clone(&qs);

    QueryApi::create(qs1, addrs).c(d!()).map(|_| {
        thread::spawn(move || loop {
            let mut created = BLOCK_CREATED.0.lock();
            if !*created {
                BLOCK_CREATED.1.wait(&mut created);
            }
            qs2.write().update();
            *created = false;
        });
        qs
    })
}
