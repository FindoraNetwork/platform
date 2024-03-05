//!
//! # send the transaction to tendermint
//!

use {
    crate::{abci::POOL, api::submission_server::TxnForward},
    ledger::data_model::Transaction,
    ruc::*,
    std::sync::atomic::{AtomicU16, Ordering},
};

static TX_PENDING_CNT: AtomicU16 = AtomicU16::new(0);

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
    let txn_b64 = base64::encode_config(txn_json.as_str(), base64::URL_SAFE);

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

    let tendermint_reply = format!("http://{url}");
    if 2000 > TX_PENDING_CNT.fetch_add(1, Ordering::Relaxed) {
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
