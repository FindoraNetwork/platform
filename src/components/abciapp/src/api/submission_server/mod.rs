pub mod submission_api;

use ledger::{
    data_model::{BlockEffect, Transaction, TxnEffect, TxnSID, TxnTempSID, TxoSID},
    store::*,
};
use log::info;
use parking_lot::RwLock;
use rand_core::{CryptoRng, RngCore};
use ruc::*;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap, fmt, ops::Deref, result::Result as StdResult, sync::Arc,
};

// Query handle for user
#[derive(Debug, Hash, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct TxnHandle(pub String);

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

// Indicates whether a transaction has been committed to the ledger
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum TxnStatus {
    Rejected(String),
    Committed((TxnSID, Vec<TxoSID>)),
    Pending,
}

pub enum CommitMode {
    FullBlock,
    EveryTransaction, // Every submission is committed as soon as it arrives.
    Manual,           // Somebody else calls commit. Not this code.
}

pub trait TxnForward: AsRef<str> {
    fn forward_txn(&self, txn: Transaction) -> Result<()>;
}

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

    pub fn get_txn_status(&self, txn_handle: &TxnHandle) -> Option<TxnStatus> {
        self.txn_status.get(txn_handle).cloned()
    }

    pub fn all_commited(&self) -> bool {
        self.block.is_none()
    }

    pub fn eligible_to_commit(&self) -> bool {
        match self.commit_mode {
            CommitMode::FullBlock => self.pending_txns.len() == self.block_capacity,
            CommitMode::EveryTransaction => true,
            CommitMode::Manual => false,
        }
    }

    pub fn get_prng(&mut self) -> &mut RNG {
        &mut self.prng
    }

    pub fn borrowable_ledger_state(&self) -> Arc<RwLock<LedgerState>> {
        self.committed_state.clone()
    }

    pub fn get_committed_state(&self) -> &RwLock<LedgerState> {
        &self.committed_state
    }

    // TODO(joe): what should these do?
    pub fn begin_commit(&mut self) {}
    pub fn end_commit(&mut self) {}

    pub fn begin_block(&mut self) {
        self.block = Some(pnk!(self.committed_state.write().start_block()));
    }

    pub fn update_staking_simulator(&mut self) -> Result<()> {
        let staking = self.committed_state.read().get_staking().deref().clone();
        self.block
            .as_mut()
            .map(|b| {
                *b.get_staking_simulator_mut() = staking;
            })
            .ok_or(eg!())
    }

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
            info!("Block ended. Statuses of committed transactions are now updated");
            // Empty temp_sids after the block is finished
            // If begin_commit or end_commit is no longer empty, move this line to the end of end_commit
            self.pending_txns = Vec::new();
            // Finally, return the finalized txn sids
            debug_assert!(self.block.is_none());
            return Ok(());
        }
        Err(eg!("Cannot finish block because there are no pending txns"))
    }

    pub fn block_txn_count(&self) -> usize {
        self.pending_txns.len()
    }

    pub fn cache_transaction(
        &mut self,
        txn: Transaction,
    ) -> StdResult<TxnHandle, TxnHandle> {
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
                    .apply_transaction(&mut block, txn_effect, false)
                    .c(d!("Failed to apply transaction"))
            });
        match temp_sid {
            Ok(temp_sid) => {
                self.pending_txns.push((temp_sid, handle.clone(), txn));
                self.txn_status.insert(handle.clone(), TxnStatus::Pending);
                Ok(handle)
            }
            Err(e) => {
                e.print();
                self.txn_status
                    .insert(handle.clone(), TxnStatus::Rejected(e.generate_log()));
                Err(handle)
            }
        }
    }

    pub fn abort_block(&mut self) {
        let mut block = None;
        std::mem::swap(&mut self.block, &mut block);
        if let Some(block) = block {
            self.committed_state.write().abort_block(block);
        }
    }

    // Handle the whole process when there's a new transaction
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

// Convert incoming tx data to the proper Transaction format
pub fn convert_tx(tx: &[u8]) -> Option<Transaction> {
    let transaction: Option<Transaction> = serde_json::from_slice(tx).ok();
    transaction
}
