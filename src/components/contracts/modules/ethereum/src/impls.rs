use crate::storage::*;
use crate::{App, Config, ContractLog, TransactionExecuted};
use config::abci::global_cfg::CFG;
use ethereum::{
    BlockV0 as Block, LegacyTransactionMessage, ReceiptV0 as Receipt,
    TransactionV0 as Transaction,
};
use ethereum_types::{Bloom, BloomInput, H160, H256, H64, U256};
use evm::{ExitFatal, ExitReason};
use fp_core::{
    context::{Context, RunTxMode},
    macros::Get,
    module::AppModuleBasic,
    transaction::ActionResult,
};
use fp_events::Event;
use fp_evm::{BlockId, CallOrCreateInfo, Runner, TransactionStatus};
use fp_storage::{Borrow, BorrowMut};
use fp_types::{
    actions::evm as EvmAction,
    crypto::{secp256k1_ecdsa_recover, HA256},
};
use fp_utils::{proposer_converter, timestamp_converter};
use log::{debug, info};
use ruc::*;
use sha3::{Digest, Keccak256};
const BLOCK_MIGRATE: &str = "block_migrate";

impl<C: Config> App<C> {
    pub fn recover_signer_fast(
        ctx: &Context,
        transaction: &Transaction,
    ) -> Option<H160> {
        let transaction_hash =
            H256::from_slice(Keccak256::digest(&rlp::encode(transaction)).as_slice());

        // Check historical cache first for Deliver Context, while holding the read lock
        if ctx.run_mode == RunTxMode::Deliver {
            let history_1 = ctx.eth_cache.history_1.read();
            let history_n = ctx.eth_cache.history_n.read();

            if let Some(signer) = history_1
                .get(&transaction_hash)
                .or_else(|| history_n.get(&transaction_hash))
            {
                return *signer;
            }
        }

        // recover_signer() calculation is necessary in 2 scenarios:
        // 1- During CheckTx on fresh transaction
        // 2- During DeliverTx if we NEVER see the transaction's signer in history (cache)
        let mut txn_signers = ctx.eth_cache.current.write();
        match txn_signers.get(&transaction_hash) {
            Some(signer) => *signer,
            None => Self::recover_signer(transaction).map(|signer| {
                txn_signers.insert(transaction_hash, Some(signer));
                signer
            }),
        }
    }

    pub fn recover_signer(transaction: &Transaction) -> Option<H160> {
        let mut sig = [0u8; 65];
        let mut msg = [0u8; 32];
        sig[0..32].copy_from_slice(&transaction.signature.r()[..]);
        sig[32..64].copy_from_slice(&transaction.signature.s()[..]);
        sig[64] = transaction.signature.standard_v();
        msg.copy_from_slice(
            &LegacyTransactionMessage::from(transaction.clone()).hash()[..],
        );

        let pubkey = secp256k1_ecdsa_recover(&sig, &msg).ok()?;
        Some(H160::from(H256::from_slice(
            Keccak256::digest(&pubkey).as_slice(),
        )))
    }

    pub fn store_block(
        &mut self,
        ctx: &mut Context,
        block_number: U256,
        root_hash: &[u8],
    ) -> Result<()> {
        // #[cfg(feature = "debug_env")]
        // const EVM_FIRST_BLOCK_HEIGHT: U256 = U256::from(142_5000);
        //
        // #[cfg(not(feature = "debug_env"))]
        // const EVM_FIRST_BLOCK_HEIGHT: U256 = U256::zero();

        let mut transactions: Vec<Transaction> = Vec::new();
        let mut statuses: Vec<TransactionStatus> = Vec::new();
        let mut receipts: Vec<Receipt> = Vec::new();
        let mut logs_bloom = Bloom::default();
        let mut is_store_block = true;

        let pending_txs: Vec<(Transaction, TransactionStatus, Receipt)> = {
            let mut txns = DELIVER_PENDING_TRANSACTIONS.lock().c(d!())?;
            std::mem::take(&mut txns)
        };

        if block_number < U256::from(CFG.checkpoint.evm_first_block_height)
            || (pending_txs.is_empty() && self.disable_eth_empty_blocks)
        {
            is_store_block = false;
        }

        for (transaction, status, receipt) in pending_txs {
            Self::logs_bloom(receipt.logs.clone(), &mut logs_bloom);
            transactions.push(transaction);
            statuses.push(status);
            receipts.push(receipt);
        }

        let ommers = Vec::<ethereum::Header>::new();
        let receipts_root =
            ethereum::util::ordered_trie_root(receipts.iter().map(rlp::encode));
        let block_timestamp = ctx.header.time.clone().unwrap_or_default();

        let mut state_root = H256::default();
        if !root_hash.is_empty() {
            state_root = H256::from_slice(root_hash);
        }

        let partial_header = ethereum::PartialHeader {
            parent_hash: Self::block_hash(
                ctx,
                Some(BlockId::Number(block_number.saturating_sub(U256::one()))),
            )
            .unwrap_or_default(),
            beneficiary: proposer_converter(ctx.header.proposer_address.clone())
                .unwrap_or_default(),
            state_root,
            receipts_root,
            logs_bloom,
            difficulty: U256::zero(),
            number: block_number,
            gas_limit: C::BlockGasLimit::get(),
            gas_used: receipts
                .clone()
                .into_iter()
                .fold(U256::zero(), |acc, r| acc + r.used_gas),
            timestamp: timestamp_converter(block_timestamp),
            extra_data: Vec::new(),
            mix_hash: H256::default(),
            nonce: H64::default(),
        };
        let block = Block::new(partial_header, transactions, ommers);
        let block_hash = HA256::new(block.header.hash());

        CurrentBlockNumber::put(ctx.db.write().borrow_mut(), &block_number)?;
        BlockHash::insert(ctx.db.write().borrow_mut(), &block_number, &block_hash)?;
        if is_store_block {
            CurrentBlock::insert(ctx.db.write().borrow_mut(), &block_hash, &block)?;
            CurrentReceipts::insert(
                ctx.db.write().borrow_mut(),
                &block_hash,
                &receipts,
            )?;
            CurrentTransactionStatuses::insert(
                ctx.db.write().borrow_mut(),
                &block_hash,
                &statuses,
            )?;
        }

        debug!(target: "ethereum", "store new ethereum block: {}", block_number);
        Ok(())
    }

    pub fn do_transact(ctx: &Context, transaction: Transaction) -> Result<ActionResult> {
        debug!(target: "ethereum", "transact ethereum transaction: {:?}", transaction);

        let mut events = vec![];
        let just_check = ctx.run_mode != RunTxMode::Deliver;

        let source = Self::recover_signer_fast(ctx, &transaction)
            .ok_or_else(|| eg!("ExecuteTransaction: InvalidSignature"))?;

        let transaction_hash =
            H256::from_slice(Keccak256::digest(&rlp::encode(&transaction)).as_slice());

        let transaction_index = if just_check {
            0
        } else {
            let txns = DELIVER_PENDING_TRANSACTIONS.lock().c(d!())?;
            // let txns = txns_guard.get_mut();
            txns.len() as u32
        };

        let gas_limit = transaction.gas_limit;

        let execute_ret = Self::execute_transaction(
            ctx,
            source,
            transaction.input.clone(),
            transaction.value,
            transaction.gas_limit,
            Some(transaction.gas_price),
            Some(transaction.nonce),
            transaction.action,
        );

        if let Err(e) = execute_ret {
            let mut to = Default::default();
            if let ethereum::TransactionAction::Call(target) = transaction.action {
                to = target;
            }
            events.push(Event::emit_event(
                Self::name(),
                TransactionExecuted {
                    sender: source,
                    to,
                    contract_address: Default::default(),
                    transaction_hash,
                    reason: ExitReason::Fatal(ExitFatal::UnhandledInterrupt),
                },
            ));

            return Ok(ActionResult {
                code: 1,
                data: vec![],
                log: format!("{}", e),
                gas_wanted: gas_limit.low_u64(),
                gas_used: 0,
                events,
            });
        }

        let (to, contract_address, info) = execute_ret.unwrap();

        let (reason, data, status, used_gas) = match info.clone() {
            CallOrCreateInfo::Call(info) => (
                info.exit_reason,
                info.value.clone(),
                TransactionStatus {
                    transaction_hash,
                    transaction_index,
                    from: source,
                    to,
                    contract_address: None,
                    logs: info.logs.clone(),
                    logs_bloom: {
                        let mut bloom: Bloom = Bloom::default();
                        Self::logs_bloom(info.logs, &mut bloom);
                        bloom
                    },
                },
                info.used_gas,
            ),
            CallOrCreateInfo::Create(info) => (
                info.exit_reason,
                vec![],
                TransactionStatus {
                    transaction_hash,
                    transaction_index,
                    from: source,
                    to,
                    contract_address: Some(info.value),
                    logs: info.logs.clone(),
                    logs_bloom: {
                        let mut bloom: Bloom = Bloom::default();
                        Self::logs_bloom(info.logs, &mut bloom);
                        bloom
                    },
                },
                info.used_gas,
            ),
        };

        for log in &status.logs {
            debug!(target: "ethereum", "transaction status log: block: {:?}, address: {:?}, topics: {:?}, data: {:?}",
                ctx.header.height, log.address, log.topics.clone(), log.data.clone());

            events.push(Event::emit_event(
                Self::name(),
                ContractLog {
                    address: log.address,
                    topics: log.topics.clone(),
                    data: log.data.clone(),
                },
            ));
        }

        let (code, message) = match reason {
            ExitReason::Succeed(_) => (0, String::new()),
            // code 1 indicates `Failed to execute evm function`, see above
            ExitReason::Error(_) => (2, "EVM error".to_string()),
            ExitReason::Revert(_) => {
                let mut message =
                    "VM Exception while processing transaction: revert".to_string();
                // A minimum size of error function selector (4) + offset (32) + string length (32)
                // should contain a utf-8 encoded revert reason.
                if data.len() > 68 {
                    let message_len = data[36..68].iter().sum::<u8>();
                    let body: &[u8] = &data[68..68 + message_len as usize];
                    if let Ok(reason) = std::str::from_utf8(body) {
                        message = format!("{} {}", message, reason);
                    }
                }
                (3, message)
            }
            ExitReason::Fatal(_) => (4, "EVM Fatal error".to_string()),
        };

        if code != 0 {
            info!(target: "ethereum", "evm execute result: reason {:?} status {:?} used_gas {}", reason, status, used_gas);
        }

        let receipt = ethereum::ReceiptV0 {
            state_root: match reason {
                ExitReason::Succeed(_) => H256::from_low_u64_be(1),
                ExitReason::Error(_) => H256::from_low_u64_le(0),
                ExitReason::Revert(_) => H256::from_low_u64_le(0),
                ExitReason::Fatal(_) => H256::from_low_u64_le(0),
            },
            used_gas,
            logs_bloom: status.logs_bloom,
            logs: status.logs.clone(),
        };

        if !just_check {
            {
                let mut pending_txs = DELIVER_PENDING_TRANSACTIONS.lock().c(d!())?;
                pending_txs.push((transaction, status, receipt))
            }

            TransactionIndex::insert(
                ctx.db.write().borrow_mut(),
                &HA256::new(transaction_hash),
                &(ctx.header.height.into(), transaction_index),
            )?;
        }

        events.push(Event::emit_event(
            Self::name(),
            TransactionExecuted {
                sender: source,
                to: to.unwrap_or_default(),
                contract_address: contract_address.unwrap_or_default(),
                transaction_hash,
                reason,
            },
        ));

        Ok(ActionResult {
            code,
            data: serde_json::to_vec(&info).unwrap_or_default(),
            log: message,
            gas_wanted: gas_limit.low_u64(),
            gas_used: used_gas.low_u64(),
            events,
        })
    }

    #[allow(clippy::too_many_arguments)]
    /// Execute an Ethereum transaction.
    pub fn execute_transaction(
        ctx: &Context,
        from: H160,
        input: Vec<u8>,
        value: U256,
        gas_limit: U256,
        gas_price: Option<U256>,
        nonce: Option<U256>,
        action: ethereum::TransactionAction,
    ) -> Result<(Option<H160>, Option<H160>, CallOrCreateInfo)> {
        match action {
            ethereum::TransactionAction::Call(target) => {
                let res = C::Runner::call(
                    ctx,
                    EvmAction::Call {
                        source: from,
                        target,
                        input,
                        value,
                        gas_limit: gas_limit.low_u64(),
                        gas_price,
                        nonce,
                    },
                    C::config(),
                )?;

                Ok((Some(target), None, CallOrCreateInfo::Call(res)))
            }
            ethereum::TransactionAction::Create => {
                let res = C::Runner::create(
                    ctx,
                    EvmAction::Create {
                        source: from,
                        init: input,
                        value,
                        gas_limit: gas_limit.low_u64(),
                        gas_price,
                        nonce,
                    },
                    C::config(),
                )?;

                Ok((None, Some(res.value), CallOrCreateInfo::Create(res)))
            }
        }
    }

    /// Get the transaction status with given block id.
    pub fn current_transaction_statuses(
        &self,
        ctx: &Context,
        id: Option<BlockId>,
    ) -> Option<Vec<TransactionStatus>> {
        let hash = HA256::new(Self::block_hash(ctx, id).unwrap_or_default());
        CurrentTransactionStatuses::get(ctx.db.read().borrow(), &hash)
    }

    /// Get the block with given block id.
    pub fn current_block(&self, ctx: &Context, id: Option<BlockId>) -> Option<Block> {
        let hash = HA256::new(Self::block_hash(ctx, id).unwrap_or_default());
        CurrentBlock::get(ctx.db.read().borrow(), &hash)
    }

    /// Get receipts with given block id.
    pub fn current_receipts(
        &self,
        ctx: &Context,
        id: Option<BlockId>,
    ) -> Option<Vec<ethereum::ReceiptV0>> {
        let hash = HA256::new(Self::block_hash(ctx, id).unwrap_or_default());
        CurrentReceipts::get(ctx.db.read().borrow(), &hash)
    }

    /// Get current block hash
    pub fn current_block_hash(ctx: &Context) -> Option<H256> {
        if let Some(number) = CurrentBlockNumber::get(ctx.db.read().borrow()) {
            Self::get_hash(ctx, number)
        } else {
            None
        }
    }

    /// Get current block number
    pub fn current_block_number(ctx: &Context) -> Option<U256> {
        CurrentBlockNumber::get(ctx.db.read().borrow())
    }

    /// Get header hash of given block id.
    pub fn block_hash(ctx: &Context, id: Option<BlockId>) -> Option<H256> {
        if let Some(id) = id {
            match id {
                BlockId::Hash(h) => Some(h),
                BlockId::Number(n) => Self::get_hash(ctx, n),
            }
        } else {
            Self::current_block_hash(ctx)
        }
    }

    /// The index of the transaction in the block
    pub fn transaction_index(ctx: &Context, hash: H256) -> Option<(U256, u32)> {
        TransactionIndex::get(ctx.db.read().borrow(), &HA256::new(hash))
    }

    fn logs_bloom(logs: Vec<ethereum::Log>, bloom: &mut Bloom) {
        for log in logs {
            bloom.accrue(BloomInput::Raw(&log.address[..]));
            for topic in log.topics {
                bloom.accrue(BloomInput::Raw(&topic[..]));
            }
        }
    }

    fn get_hash(ctx: &Context, number: U256) -> Option<H256> {
        if let Some(hash) = BlockHash::get(ctx.db.read().borrow(), &number) {
            return Some(hash.h256());
        }
        None
    }

    //Migrate existing transaction indices from chain-state to rocksdb.
    fn migrate_txn_idxs(ctx: &mut Context) -> Result<()> {
        let txn_idxs: Vec<(HA256, (U256, u32))> =
            TransactionIndex::iterate(ctx.state.read().borrow());
        let txn_idxs_cnt = txn_idxs.len();

        for idx in txn_idxs {
            TransactionIndex::insert(ctx.db.write().borrow_mut(), &idx.0, &idx.1)?;
            debug!(target: "ethereum", "hash: 0x{}, block: {}, index: {}", idx.0.to_string(), idx.1 .0, idx.1 .1);
        }
        ctx.db.write().commit_session();
        info!(target: "ethereum", "{} transaction indexes migrated to db", txn_idxs_cnt);

        Ok(())
    }

    //Find the first block with a non zero state_root.
    //Create range between the first non zero state_root to the latest block
    //Loop in descending order from latest block to the first block with non-zero state_root
    //Replace state root of block x with state root from block x + 1
    fn migrate_block_data(ctx: &mut Context) -> Result<()> {
        // 1. Find range of blocks that require adjustments
        let mut first_block = U256::from(CFG.checkpoint.evm_first_block_height);
        let current_block_num =
            CurrentBlockNumber::get(ctx.db.read().borrow()).unwrap_or_default();
        if current_block_num <= first_block {
            return Err(eg!("migrate_block_data: could not get latest block number"));
        }
        let mut i = U256::from(CFG.checkpoint.evm_first_block_height);
        while i <= current_block_num {
            // get block data for block (i)
            let id = Some(BlockId::Number(i));
            let hash = HA256::new(Self::block_hash(ctx, id).unwrap_or_default());
            let block = CurrentBlock::get(ctx.db.read().borrow(), &hash);
            if block.is_none() {
                return Err(eg!(
                    "migrate_block_data: unable to get block at height {:?}",
                    i
                ));
            }
            // check if state_root is non-zero
            if !block.unwrap().header.state_root.is_zero() {
                first_block = i;
                break;
            }
            i += U256::from(1);
        }

        // 2. Loop in descending order from latest block to the first block with transactions
        //      replace state_root of block x with state_root of block x + 1
        let mut k = current_block_num;
        let mut prev_state_root =
            H256::from_slice(ctx.state.read().root_hash().as_slice());
        while k >= first_block.saturating_sub(U256::from(1)) {
            // Get Block Data
            let id = Some(BlockId::Number(k));
            let hash = HA256::new(Self::block_hash(ctx, id).unwrap_or_default());
            let block: Option<Block> = CurrentBlock::get(ctx.db.read().borrow(), &hash);
            if block.is_none() {
                return Err(eg!(
                    "migrate_block_data: unable to get block at height {:?}",
                    k
                ));
            }
            let mut block_data = block.unwrap();

            // Replace state root of block x with state root from block x + 1
            std::mem::swap(&mut block_data.header.state_root, &mut prev_state_root);

            CurrentBlock::insert(ctx.db.write().borrow_mut(), &hash, &block_data)?;
            k -= U256::from(1);
        }
        ctx.db.write().commit_session();
        info!(target: "ethereum", "state root adjusted for evm block store");
        Ok(())
    }

    pub fn migrate(ctx: &mut Context) -> Result<()> {
        //Migrate transaction indices
        Self::migrate_txn_idxs(ctx)?;
        //Migrate state root data for blocks
        let key = String::from(BLOCK_MIGRATE);
        if !Migrated::contains_key(ctx.db.read().borrow(), key.borrow()) {
            Self::migrate_block_data(ctx)?;
            Migrated::insert(ctx.db.write().borrow_mut(), key.borrow(), &true)?;
            ctx.db.write().commit_session();
        }
        Ok(())
    }
}
