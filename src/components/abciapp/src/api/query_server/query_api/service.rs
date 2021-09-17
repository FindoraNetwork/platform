//!
//! new query server
//!

use super::{
    server::{QueryServer, BLOCK_CREATED},
    QueryApi,
};
use ledger::store::LedgerState;
use parking_lot::RwLock;
use ruc::*;
use std::{sync::Arc, thread};

/// create query server
pub fn start_query_server(
    ledger: Arc<RwLock<LedgerState>>,
    addrs: &[(&str, u16)],
    basedir: Option<&str>,
) -> Result<Arc<RwLock<QueryServer>>> {
    let qs = QueryServer::new(ledger, basedir).c(d!())?;
    let basedir = qs.basedir.clone();

    let qs = Arc::new(RwLock::new(qs));
    let qs1 = Arc::clone(&qs);
    let qs2 = Arc::clone(&qs);

    QueryApi::create(qs1, addrs).c(d!()).map(|_| {
        thread::spawn(move || loop {
            let mut created = BLOCK_CREATED.0.lock();
            if !*created {
                BLOCK_CREATED.1.wait(&mut created);
            }
            qs2.write().update(&basedir);
            *created = false;
        });
        qs
    })
}
