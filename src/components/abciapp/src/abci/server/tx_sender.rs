//!
//! # send the transaction to tendermint or abci_mock
//!

#[cfg(not(feature = "abci_mock"))]
pub use real::{forward_txn_with_mode, TendermintForward};

#[cfg(feature = "abci_mock")]
pub use mocker::{forward_txn_with_mode, TendermintForward, CHAN};

#[cfg(not(feature = "abci_mock"))]
mod real {
    use crate::api::submission_server::TxnForward;
    use futures::executor::{ThreadPool, ThreadPoolBuilder};
    use lazy_static::lazy_static;
    use ledger::data_model::Transaction;
    use ruc::*;
    use std::sync::atomic::{AtomicU16, Ordering};

    static TX_PENDING_CNT: AtomicU16 = AtomicU16::new(0);

    lazy_static! {
        static ref POOL: ThreadPool =
            pnk!(ThreadPoolBuilder::new().pool_size(4).create());
    }

    pub struct TendermintForward {
        pub tendermint_reply: String,
    }

    impl AsRef<str> for TendermintForward {
        fn as_ref(&self) -> &str {
            self.tendermint_reply.as_str()
        }
    }

    impl TxnForward for TendermintForward {
        fn forward_txn(&self, txn: Transaction) -> Result<()> {
            forward_txn_with_mode(self.as_ref(), txn, false)
        }
    }

    pub fn forward_txn_with_mode(
        url: &str,
        txn: Transaction,
        async_mode: bool,
    ) -> Result<()> {
        const SYNC_API: &str = "broadcast_tx_sync";
        const ASYNC_API: &str = "broadcast_tx_async";

        let txn_json = serde_json::to_string(&txn).c(d!())?;
        let txn_b64 = base64::encode_config(&txn_json.as_str(), base64::URL_SAFE);

        let json_rpc = if async_mode {
            format!(
                "{{\"jsonrpc\":\"2.0\",\"id\":\"anything\",\"method\":\"{}\",\"params\": {{\"tx\": \"{}\"}}}}",
                ASYNC_API, &txn_b64
            )
        } else {
            format!(
                "{{\"jsonrpc\":\"2.0\",\"id\":\"anything\",\"method\":\"{}\",\"params\": {{\"tx\": \"{}\"}}}}",
                SYNC_API, &txn_b64
            )
        };

        let tendermint_reply = format!("http://{}", url);
        if 100 > TX_PENDING_CNT.fetch_add(1, Ordering::Relaxed) {
            POOL.spawn_ok(async move {
                ruc::info_omit!(attohttpc::post(&tendermint_reply)
                    .header(attohttpc::header::CONTENT_TYPE, "application/json")
                    .text(json_rpc)
                    .send()
                    .c(d!()));
                TX_PENDING_CNT.fetch_sub(1, Ordering::Relaxed);
            });
        } else {
            TX_PENDING_CNT.fetch_sub(1, Ordering::Relaxed);
            return Err(eg!("Too many pending tasks"));
        }

        Ok(())
    }
}

#[cfg(feature = "abci_mock")]
mod mocker {
    use crate::api::submission_server::TxnForward;
    use lazy_static::lazy_static;
    use ledger::data_model::Transaction;
    use parking_lot::Mutex;
    use ruc::*;
    use std::sync::{
        mpsc::{channel, Receiver, Sender},
        Arc,
    };

    lazy_static! {
        /// global channel
        pub static ref CHAN: ChanPair = {
            let (s, r) = channel();
            (Arc::new(Mutex::new(s)), Arc::new(Mutex::new(r)))
        };
    }

    /// define channel type
    type ChanPair = (
        Arc<Mutex<Sender<Transaction>>>,
        Arc<Mutex<Receiver<Transaction>>>,
    );

    /// define the tendermint of the simulation,forwarding messages
    /// use when creating ABCISubmissionServer
    pub struct TendermintForward {
        pub tendermint_reply: String,
    }

    impl AsRef<str> for TendermintForward {
        fn as_ref(&self) -> &str {
            self.tendermint_reply.as_str()
        }
    }

    impl TxnForward for TendermintForward {
        /// impl submission_server `TxnForward` trait
        fn forward_txn(&self, txn: Transaction) -> Result<()> {
            forward_txn_with_mode(self.as_ref(), txn, false)
        }
    }

    /// send message
    pub fn forward_txn_with_mode(
        _url: &str,
        tx: Transaction,
        _async_mode: bool,
    ) -> Result<()> {
        CHAN.0.lock().send(tx).c(d!())
    }
}
