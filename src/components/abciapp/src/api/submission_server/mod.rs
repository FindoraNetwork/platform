//!
//! # service of operating tx
//!

pub mod submission_api;

use {
    fp_utils::tx::EVM_TX_TAG,
    ledger::{
        data_model::{BlockEffect, Transaction, TxnEffect, TxnSID, TxnTempSID, TxoSID},
        store::LedgerState,
    },
    parking_lot::RwLock,
    rand_core::{CryptoRng, RngCore},
    ruc::*,
    serde::{Deserialize, Serialize},
    std::{collections::HashMap, fmt, sync::Arc},
};

/// Query handle for user
#[derive(Debug, Hash, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct TxnHandle(pub String);

#[allow(missing_docs)]
impl TxnHandle {
    pub fn new(txn: &Transaction) -> Self {
        TxnHandle(txn.handle())
    }
}

impl fmt::Display for TxnHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TxnHandle: {}", self.0)
    }
}

/// Indicates whether a transaction has been committed to the ledger
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum TxnStatus {
    Rejected(String),
    Committed((TxnSID, Vec<TxoSID>)),
    Pending,
}

/// use to create submissionServer
pub enum CommitMode {
    /// all block
    FullBlock,
    /// Every submission is committed as soon as it arrives.
    EveryTransaction,
    /// no auto commit
    Manual, // Somebody else calls commit. Not this code.
}

/// Define txforward trait,
/// the impl of different functions is different, and the specific impl is in
/// `src/components/abciapp/server/tx_sender.rs`
#[allow(missing_docs)]
pub trait TxnForward: AsRef<str> {
    fn forward_txn(&self, txn: Transaction) -> Result<()>;
}

/// Define SubmissionServer
pub struct SubmissionServer<RNG, TF>
where
    RNG: RngCore + CryptoRng,
    TF: TxnForward,
{
    committed_state: Arc<RwLock<LedgerState>>,
    block: Option<BlockEffect>,
    pending_txns: Vec<(TxnTempSID, TxnHandle, Transaction)>,
    txn_status: HashMap<TxnHandle, TxnStatus>,
    block_capacity: usize,
    prng: RNG,
    commit_mode: CommitMode,
    txn_forwarder: TF,
}

impl<RNG, TF> SubmissionServer<RNG, TF>
where
    RNG: RngCore + CryptoRng,
    TF: TxnForward,
{
    /// Create to full block commit
    pub fn new(
        prng: RNG,
        ledger_state: Arc<RwLock<LedgerState>>,
        block_capacity: usize,
        txn_forwarder: TF,
    ) -> Result<SubmissionServer<RNG, TF>> {
        Ok(SubmissionServer {
            committed_state: ledger_state,
            block: None,
            txn_status: HashMap::new(),
            pending_txns: vec![],
            prng,
            block_capacity,
            commit_mode: CommitMode::FullBlock,
            txn_forwarder,
        })
    }

    /// Create to no auto commit
    pub fn new_no_auto_commit(
        prng: RNG,
        ledger_state: Arc<RwLock<LedgerState>>,
        txn_forwarder: TF,
    ) -> Result<SubmissionServer<RNG, TF>> {
        Ok(SubmissionServer {
            committed_state: ledger_state,
            block: None,
            txn_status: HashMap::new(),
            pending_txns: vec![],
            prng,
            block_capacity: 0,
            commit_mode: CommitMode::Manual,
            txn_forwarder,
        })
    }

    /// Query operation results
    pub fn get_txn_status(&self, txn_handle: &TxnHandle) -> Option<TxnStatus> {
        self.txn_status.get(txn_handle).cloned()
    }

    /// Determine if block is empty
    pub fn all_commited(&self) -> bool {
        self.block.is_none()
    }

    /// Determine commit_mode type
    pub fn eligible_to_commit(&self) -> bool {
        match self.commit_mode {
            CommitMode::FullBlock => self.pending_txns.len() == self.block_capacity,
            CommitMode::EveryTransaction => true,
            CommitMode::Manual => false,
        }
    }

    /// Get prng
    pub fn get_prng(&mut self) -> &mut RNG {
        &mut self.prng
    }

    /// Borrow ledgerState
    pub fn borrowable_ledger_state(&self) -> Arc<RwLock<LedgerState>> {
        self.committed_state.clone()
    }

    /// Get ledgerState
    pub fn get_committed_state(&self) -> &RwLock<LedgerState> {
        &self.committed_state
    }

    // TODO(joe): what should these do?
    #[allow(missing_docs)]
    pub fn begin_commit(&mut self) {}
    #[allow(missing_docs)]
    pub fn end_commit(&mut self) {}

    /// Get the `block_ctx` in `ledgerState`
    pub fn begin_block(&mut self) {
        self.block = Some(pnk!(self.committed_state.write().start_block()));
    }

    /// In abci's begin_block, if the block is empty,
    /// call this method to update the staking in the ledgerState to the submission_server's block
    pub fn update_staking_simulator(&mut self) -> Result<()> {
        let staking = self.committed_state.read().get_staking().clone();
        self.block
            .as_mut()
            .map(|b| {
                *b.get_staking_simulator_mut() = staking;
            })
            .ok_or(eg!())
    }

    /// In abci's end_block, this method will be called
    /// if the block is not empty and the block in the submission_server is not empty,
    /// it is the logic to write the block to the ledgerState
    pub fn end_block(&mut self) -> Result<()> {
        if let Some(block) = self.block.take() {
            let mut ledger = self.committed_state.write();
            let finalized_txns = ledger.finish_block(block).c(d!())?;

            // Update status of all committed transactions
            for (txn_temp_sid, handle, _txn) in self.pending_txns.drain(..) {
                let committed_txn_info = finalized_txns.get(&txn_temp_sid).c(d!())?;
                self.txn_status
                    .insert(handle, TxnStatus::Committed(committed_txn_info.clone()));
            }

            self.pending_txns = Vec::new();
            return Ok(());
        }

        Err(eg!("Cannot finish block because there are no pending txns"))
    }

    /// Get txs number of pending
    pub fn block_txn_count(&self) -> usize {
        self.pending_txns.len()
    }

    /// The transaction will be applied to the effect_block after a series of judgments,
    /// and will be classified as pending or rejected depending on the result of the processing.
    pub fn cache_transaction(&mut self, txn: Transaction) -> Result<TxnHandle> {
        // Begin a block if the previous one has been commited
        if self.all_commited() {
            self.begin_block();
        }

        // The if statement above guarantees that we have a block.
        let mut block = self.block.as_mut().unwrap();
        let ledger = self.committed_state.read();
        let handle = TxnHandle::new(&txn);
        let temp_sid = TxnEffect::compute_effect(txn.clone())
            .c(d!("Failed to compute txn effect"))
            .and_then(|txn_effect| {
                ledger
                    .apply_transaction(&mut block, txn_effect)
                    .c(d!("Failed to apply transaction"))
            });
        match temp_sid {
            Ok(temp_sid) => {
                self.pending_txns.push((temp_sid, handle.clone(), txn));
                self.txn_status.insert(handle.clone(), TxnStatus::Pending);
                Ok(handle)
            }
            Err(e) => {
                log::error!("Error in cache_transaction {}", e);
                self.txn_status
                    .insert(handle, TxnStatus::Rejected(e.to_string()));
                Err(e)
            }
        }
    }

    /// Handle the whole process when there's a new transaction
    pub fn handle_transaction(&mut self, txn: Transaction) -> Result<TxnHandle> {
        let txn_handle = TxnHandle::new(&txn);
        self.txn_forwarder.forward_txn(txn).c(d!())?;
        Ok(txn_handle)
    }

    #[allow(missing_docs)]
    pub fn get_fwder(&self) -> &TF {
        &self.txn_forwarder
    }
}

/// Convert incoming tx data to the proper Transaction format
#[inline(always)]
pub fn convert_tx(tx: &[u8]) -> Result<Transaction> {
    serde_json::from_slice(tx).c(d!())
}

/// Tx Catalog
pub enum TxCatalog {
    /// findora tx
    FindoraTx,

    /// evm tx
    EvmTx,

    /// unknown tx
    Unknown,
}

/// Check Tx Catalog
pub fn try_tx_catalog(tx: &[u8], log: bool) -> TxCatalog {
    // print tx
    if log {
        log::info!(target: "abciapp", "try_tx_catalog: {:?}", base64::encode(tx));
    }

    let len = EVM_TX_TAG.len();
    if tx.len() <= len {
        return TxCatalog::Unknown;
    }

    if EVM_TX_TAG.eq(&tx[..len]) {
        return TxCatalog::EvmTx;
    }

    TxCatalog::FindoraTx
}
