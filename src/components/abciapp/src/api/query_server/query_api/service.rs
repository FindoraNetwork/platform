use super::{
    query_server::{QueryServer, BLOCK_CREATED},
    QueryApi,
};
use ledger::store::LedgerState;
use parking_lot::RwLock;
use ruc::*;
use std::{sync::Arc, thread};

pub fn start_query_server(
    ledger: Arc<RwLock<LedgerState>>,
    host: &str,
    port: u16,
) -> Result<Arc<RwLock<QueryServer>>> {
    let qs = Arc::new(RwLock::new(QueryServer::new(ledger)));
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
