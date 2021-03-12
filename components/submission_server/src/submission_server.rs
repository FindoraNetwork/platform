#![deny(warnings)]

use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
    Operation, Transaction, TxnEffect, TxnSID, TxnTempSID, TxoSID,
};
use ledger::store::*;
use log::info;
use parking_lot::RwLock;
use rand_core::{CryptoRng, RngCore};
use ruc::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::result::Result as StdResult;
use std::sync::Arc;

macro_rules! fail {
    () => {
        PlatformError::SubmissionServerError(None)
    };
    ($s:expr) => {
        PlatformError::SubmissionServerError(Some(format!("{}", &$s)))
    };
}

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

pub trait TxnForward {
    fn forward_txn(&self, txn: Transaction) -> Result<()>;
}

/// Don't forward transactions; handle them in the default way.
pub struct NoTF;

impl TxnForward for NoTF {
    fn forward_txn(&self, _: Transaction) -> Result<()> {
        // Need implementation for the None case even though never used.
        unimplemented!();
    }
}

pub struct SubmissionServer<RNG, LU, TF>
where
    RNG: RngCore + CryptoRng,
    LU: LedgerUpdate<RNG>,
    TF: TxnForward,
{
    committed_state: Arc<RwLock<LU>>,
    block: Option<LU::Block>,
    pending_txns: Vec<(TxnTempSID, TxnHandle, Transaction)>,
    txn_status: HashMap<TxnHandle, TxnStatus>,
    block_capacity: usize,
    prng: RNG,
    commit_mode: CommitMode,
    txn_forwarder: Option<TF>,
}

impl<RNG, LU, TF> SubmissionServer<RNG, LU, TF>
where
    RNG: RngCore + CryptoRng,
    LU: LedgerUpdate<RNG>,
    TF: TxnForward,
{
    pub fn new(
        prng: RNG,
        ledger_state: Arc<RwLock<LU>>,
        block_capacity: usize,
    ) -> Result<SubmissionServer<RNG, LU, TF>> {
        Ok(SubmissionServer {
            committed_state: ledger_state,
            block: None,
            txn_status: HashMap::new(),
            pending_txns: vec![],
            prng,
            block_capacity,
            commit_mode: CommitMode::FullBlock,
            txn_forwarder: None,
        })
    }
    pub fn new_no_auto_commit(
        prng: RNG,
        ledger_state: Arc<RwLock<LU>>,
        txn_forwarder: Option<TF>,
    ) -> Result<SubmissionServer<RNG, LU, TF>> {
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
        self.txn_status.get(&txn_handle).cloned()
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

    pub fn borrowable_ledger_state(&self) -> Arc<RwLock<LU>> {
        self.committed_state.clone()
    }

    pub fn get_committed_state(&self) -> &RwLock<LU> {
        &self.committed_state
    }

    // TODO(joe): what should these do?
    pub fn begin_commit(&mut self) {}
    pub fn end_commit(&mut self) {}

    pub fn begin_block(&mut self) {
        debug_assert!(self.block.is_none());
        self.block = Some(
            self.committed_state
                .write()
                .start_block()
                .expect("Ledger could not start block"),
        );
        info!("New block started");
    }

    pub fn end_block(&mut self) -> Result<()> {
        let mut block = None;
        std::mem::swap(&mut self.block, &mut block);
        if let Some(block) = block {
            let mut ledger = self.committed_state.write();
            let finalized_txns = ledger
                .finish_block(block)
                .expect("Ledger could not finish block");
            // Update status of all committed transactions
            for (txn_temp_sid, handle, txn) in self.pending_txns.drain(..) {
                let committed_txn_info = finalized_txns.get(&txn_temp_sid).c(d!())?;
                self.txn_status
                    .insert(handle, TxnStatus::Committed(committed_txn_info.clone()));

                txn_log_info(&txn);
            }
            info!("Block ended. Statuses of committed transactions are now updated");
            // Empty temp_sids after the block is finished
            // If begin_commit or end_commit is no longer empty, move this line to the end of end_commit
            self.pending_txns = Vec::new();
            // Finally, return the finalized txn sids
            debug_assert!(self.block.is_none());
            return Ok(());
        }
        Err(eg!(fail!(
            "Cannot finish block because there are no pending txns"
        )))
    }

    pub fn block_pulse_count(&self) -> u64 {
        if let Some(block) = &self.block {
            LU::block_pulse_count(&block)
        } else {
            0
        }
    }

    pub fn block_txn_count(&self) -> usize {
        self.pending_txns.len()
    }

    pub fn pulse_block(&mut self) {
        if let Some(block) = &mut self.block {
            LU::pulse_block(block);
        }
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
        let temp_sid =
            TxnEffect::compute_effect(txn.clone())
                .c(d!())
                .and_then(|txn_effect| {
                    ledger.apply_transaction(&mut block, txn_effect).c(d!())
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
        match self.txn_forwarder {
            None => {
                let handle = match self.cache_transaction(txn) {
                    Ok(h) => h,
                    Err(h) => h,
                };
                info!(
                    "Transaction added to cache and will be committed in the next block"
                );
                // End the current block if it's eligible to commit
                if self.eligible_to_commit() {
                    // If the ledger is eligible for a commit, end block will not return an error
                    self.end_block().c(d!())?;

                    // If begin_commit and end_commit are no longer empty, call them here
                }
                Ok(handle)
            }
            Some(ref forwarder) => {
                let txn_handle = TxnHandle::new(&txn);
                forwarder.forward_txn(txn).c(d!())?;
                Ok(txn_handle)
            }
        }
    }
}

pub fn txn_log_info(txn: &Transaction) {
    for op in &txn.body.operations {
        match op {
            Operation::KVStoreUpdate(update) => info!(
                "Key-Value store update: {}",
                &serde_json::to_string(update).unwrap()
            ),
            Operation::DefineAsset(define_asset_op) => info!(
                "Asset Definition: New asset with code {} defined",
                define_asset_op.body.asset.code.to_base64()
            ),
            Operation::IssueAsset(issue_asset_op) => {
                info!(
                    "Asset Issuance: Issued asset {} with {} new outputs. Sequence number is {}.",
                    issue_asset_op.body.code.to_base64(),
                    issue_asset_op.body.num_outputs,
                    issue_asset_op.body.seq_num
                );
            }
            Operation::TransferAsset(xfr_asset_op) => {
                info!(
                    "Asset Transfer: Transfer with {} inputs and {} outputs",
                    xfr_asset_op.body.inputs.len(),
                    xfr_asset_op.body.outputs.len()
                );
            }
            Operation::BindAssets(bind_assets_op) => {
                info!(
                    "Asset Bind: Bind of {} inputs",
                    bind_assets_op.body.inputs.len()
                );
            }
            Operation::ReleaseAssets(release_assets_op) => {
                info!(
                    "Asset Release: Release of lien {:?} ({} inputs) into {} outputs",
                    release_assets_op.body.lien,
                    release_assets_op.body.transfer.inputs.len(),
                    release_assets_op.body.transfer.outputs.len(),
                );
            }
            Operation::AIRAssign(air_assign_op) => {
                info!(
                    "Assigning to AIR: AIR[{}] <- {:?}",
                    serde_json::to_string(&air_assign_op.body.addr).unwrap(),
                    air_assign_op.body.data
                );
            }
            Operation::UpdateMemo(update_memo) => {
                info!(
                    "Updating memo of asset type {} to {}",
                    update_memo.body.asset_type.to_base64(),
                    update_memo.body.new_memo.0
                );
            }
        };
    }
}

// Convert incoming tx data to the proper Transaction format
pub fn convert_tx(tx: &[u8]) -> Option<Transaction> {
    let transaction: Option<Transaction> = serde_json::from_slice(tx).ok();
    transaction
}

#[cfg(test)]
mod tests {
    use super::*;
    use ledger::data_model::{AssetRules, AssetTypeCode};
    use rand_core::SeedableRng;
    use txn_builder::{BuildsTransactions, PolicyChoice, TransactionBuilder};
    use zei::xfr::sig::XfrKeyPair;

    #[test]
    fn test_cache_transaction() {
        // Create a SubmissionServer
        let block_capacity = 8;
        let ledger_state = LedgerState::test_ledger();
        let mut prng = rand_chacha::ChaChaRng::from_entropy();
        let seq_id = ledger_state.get_block_commit_count();
        let mut submission_server = SubmissionServer::<_, _, NoTF>::new(
            prng.clone(),
            Arc::new(RwLock::new(ledger_state)),
            block_capacity,
        )
        .unwrap();

        submission_server.begin_block();

        // Create values to be used to build transactions
        let keypair = XfrKeyPair::generate(&mut prng);
        let token_code = "test";
        let asset_token = AssetTypeCode::new_from_base64(&token_code).unwrap();

        // Build transactions
        let mut txn_builder_0 = TransactionBuilder::from_seq_id(seq_id);
        let mut txn_builder_1 = TransactionBuilder::from_seq_id(seq_id);

        txn_builder_0
            .add_operation_create_asset(
                &keypair,
                Some(asset_token),
                AssetRules::default(),
                &String::from("{}"),
                PolicyChoice::Fungible(),
            )
            .unwrap();

        txn_builder_1
            .add_operation_create_asset(
                &keypair,
                None,
                AssetRules::default(),
                "test",
                PolicyChoice::Fungible(),
            )
            .unwrap();

        // Cache transactions
        submission_server
            .cache_transaction(txn_builder_0.transaction().clone())
            .unwrap();
        submission_server
            .cache_transaction(txn_builder_1.transaction().clone())
            .unwrap();

        // Verify temp_sids
        let temp_sid_0 = submission_server.pending_txns.get(0).unwrap();
        let temp_sid_1 = submission_server.pending_txns.get(1).unwrap();

        assert_eq!(temp_sid_0.0, TxnTempSID(0));
        assert_eq!(temp_sid_1.0, TxnTempSID(1));
    }

    #[test]
    fn test_eligible_to_commit() {
        // Create a SubmissionServer
        let block_capacity = 8;
        let ledger_state = LedgerState::test_ledger();
        let prng = rand_chacha::ChaChaRng::from_entropy();
        let seq_id = ledger_state.get_block_commit_count();
        let mut submission_server = SubmissionServer::<_, _, NoTF>::new(
            prng,
            Arc::new(RwLock::new(ledger_state)),
            block_capacity,
        )
        .unwrap();

        submission_server.begin_block();

        let transaction = Transaction::from_seq_id(seq_id);

        // Verify that it's ineligible to commit if #transactions < BLOCK_CAPACITY
        for _i in 0..(block_capacity - 1) {
            omit!(submission_server.cache_transaction(transaction.clone()));
            assert!(!submission_server.eligible_to_commit());
        }

        // Verify that it's eligible to commit if #transactions == BLOCK_CAPACITY
        omit!(submission_server.cache_transaction(transaction));
        // Need to consider replay prevention
        assert!(!submission_server.eligible_to_commit());
    }

    #[test]
    fn test_txn_status() {
        let block_capacity = 2;
        let ledger_state = LedgerState::test_ledger();
        let prng = rand_chacha::ChaChaRng::from_entropy();
        let seq_id = ledger_state.get_block_commit_count();
        let mut submission_server = SubmissionServer::<_, _, NoTF>::new(
            prng,
            Arc::new(RwLock::new(ledger_state)),
            block_capacity,
        )
        .unwrap();

        // Submit the first transcation. Ensure that the txn is pending.
        let transaction = Transaction::from_seq_id(seq_id);
        let txn_handle = submission_server
            .handle_transaction(transaction.clone())
            .unwrap();
        let status = submission_server
            .txn_status
            .get(&txn_handle)
            .expect("handle should be in map")
            .clone();
        assert_eq!(status, TxnStatus::Pending);

        // Submit a second transaction and ensure that it is tracked as committed
        submission_server.handle_transaction(transaction).unwrap();
        // In this case, both transactions have the same handle. Because transactions are unique and
        // We are using a collision resistant hash function, this will not occur on a live ledger.
        let status = submission_server
            .txn_status
            .get(&txn_handle)
            .expect("handle should be in map")
            .clone();

        match status {
            TxnStatus::Rejected(_) => {} // No replay token
            TxnStatus::Pending => {
                panic!("txn pending");
            }
            TxnStatus::Committed((_sid, _txos)) => {
                panic!("txn commited");
            }
        }
        // assert_eq!(status, TxnStatus::Rejected()); // TxnStatus::Committed((TxnSID(1), Vec::new())));

        // Now test that invalid transactions show up as rejected
        // Provide the completely wrong sequence number
        let bad_transaction = Transaction::from_seq_id(666);
        let txn_handle = submission_server
            .handle_transaction(bad_transaction)
            .unwrap();
        let status = submission_server
            .txn_status
            .get(&txn_handle)
            .expect("The handle should be in map by now.")
            .clone();
        match status {
            TxnStatus::Rejected(_) => {}
            TxnStatus::Pending => {
                panic!("txn pending");
            }
            TxnStatus::Committed((_sid, _txos)) => {
                panic!("txn commited");
            }
        }
    }
}
