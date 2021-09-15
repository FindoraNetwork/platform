use crate::storage::*;
use crate::{App, Config, ContractLog, TransactionExecuted, PENDING_TRANSACTIONS};
use ethereum_types::{Bloom, BloomInput, H160, H256, H64, U256};
use evm::ExitReason;
use fp_core::{
    context::Context, macros::Get, module::AppModuleBasic, transaction::ActionResult,
};
use fp_events::Event;
use fp_evm::{BlockId, CallOrCreateInfo, Runner, TransactionStatus};
use fp_traits::evm::DecimalsMapping;
use fp_types::{actions::evm as EvmAction, crypto::secp256k1_ecdsa_recover};
use fp_utils::{proposer_converter, timestamp_converter};
use log::debug;
use ruc::*;
use sha3::{Digest, Keccak256};

impl<C: Config> App<C> {
    pub fn recover_signer(transaction: &ethereum::Transaction) -> Option<H160> {
        let mut sig = [0u8; 65];
        let mut msg = [0u8; 32];
        sig[0..32].copy_from_slice(&transaction.signature.r()[..]);
        sig[32..64].copy_from_slice(&transaction.signature.s()[..]);
        sig[64] = transaction.signature.standard_v();
        msg.copy_from_slice(
            &ethereum::TransactionMessage::from(transaction.clone()).hash()[..],
        );

        let pubkey = secp256k1_ecdsa_recover(&sig, &msg).ok()?;
        Some(H160::from(H256::from_slice(
            Keccak256::digest(&pubkey).as_slice(),
        )))
    }

    pub fn store_block(&mut self, ctx: &mut Context, block_number: U256) -> Result<()> {
        let mut transactions: Vec<ethereum::Transaction> = Vec::new();
        let mut statuses: Vec<TransactionStatus> = Vec::new();
        let mut receipts: Vec<ethereum::Receipt> = Vec::new();
        let mut logs_bloom = Bloom::default();

        for (transaction, status, receipt) in
            PENDING_TRANSACTIONS.lock().clone().into_iter()
        {
            Self::logs_bloom(receipt.logs.clone(), &mut logs_bloom);
            transactions.push(transaction);
            statuses.push(status);
            receipts.push(receipt);
        }

        let ommers = Vec::<ethereum::Header>::new();
        let receipts_root =
            ethereum::util::ordered_trie_root(receipts.iter().map(|r| rlp::encode(r)));
        let block_timestamp = ctx.header.time.clone().unwrap_or_default();

        let mut state_root = H256::default();
        let root_hash = ctx.store.read().root_hash();
        if !root_hash.is_empty() {
            state_root = H256::from_slice(&root_hash);
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
        let block = ethereum::Block::new(partial_header, transactions, ommers);
        let block_hash = block.header.hash();

        CurrentBlockNumber::put(ctx.store.clone(), &block_number)?;
        // CurrentBlock::insert(ctx.store.clone(), &block_hash, &block);
        // CurrentReceipts::insert(ctx.store.clone(), &block_hash, &receipts);
        // CurrentTransactionStatuses::insert(ctx.store.clone(), &block_hash, &statuses);
        BlockHash::insert(ctx.store.clone(), &block_number, &block_hash)?;

        PENDING_TRANSACTIONS.lock().clear();

        self.blocks.insert(block_hash, block);
        self.receipts.insert(block_hash, receipts);
        self.transaction_statuses.insert(block_hash, statuses);

        debug!(target: "ethereum", "store new ethereum block: {}", block_number);
        Ok(())
    }

    pub fn do_transact(
        ctx: &Context,
        transaction: ethereum::Transaction,
    ) -> Result<ActionResult> {
        debug!(target: "evm", "transact ethereum transaction: {:?}", transaction);

        let mut events = vec![];

        let source = Self::recover_signer(&transaction)
            .ok_or_else(|| eg!("ExecuteTransaction: InvalidSignature"))?;

        let transaction_hash =
            H256::from_slice(Keccak256::digest(&rlp::encode(&transaction)).as_slice());

        let transaction_index = PENDING_TRANSACTIONS.lock().len() as u32;

        let gas_limit = transaction.gas_limit;
        let transferred_value =
            C::DecimalsMapping::convert_to_native_token(transaction.value);

        let (to, contract_address, info) = Self::execute_transaction(
            ctx,
            source,
            transaction.input.clone(),
            transferred_value,
            transaction.gas_limit,
            Some(transaction.gas_price),
            Some(transaction.nonce),
            transaction.action,
        )?;

        let (reason, status, used_gas) = match info.clone() {
            CallOrCreateInfo::Call(info) => (
                info.exit_reason,
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
            events.push(Event::emit_event(
                App::<C>::name(),
                ContractLog {
                    address: log.address,
                    topics: log.topics.clone(),
                    data: log.data.clone(),
                },
            ));
        }

        let receipt = ethereum::Receipt {
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

        PENDING_TRANSACTIONS
            .lock()
            .push((transaction, status, receipt));

        events.push(Event::emit_event(
            Self::name(),
            TransactionExecuted {
                sender: source,
                contract_address: contract_address.unwrap_or_default(),
                transaction_hash,
                reason,
            },
        ));

        Ok(ActionResult {
            data: serde_json::to_vec(&info).unwrap_or_default(),
            log: "".to_string(),
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
        let hash = Self::block_hash(ctx, id).unwrap_or_default();
        self.transaction_statuses.get(&hash)
    }

    /// Get the block with given block id.
    pub fn current_block(
        &self,
        ctx: &Context,
        id: Option<BlockId>,
    ) -> Option<ethereum::Block> {
        let hash = Self::block_hash(ctx, id).unwrap_or_default();
        self.blocks.get(&hash)
    }

    /// Get receipts with given block id.
    pub fn current_receipts(
        &self,
        ctx: &Context,
        id: Option<BlockId>,
    ) -> Option<Vec<ethereum::Receipt>> {
        let hash = Self::block_hash(ctx, id).unwrap_or_default();
        self.receipts.get(&hash)
    }

    /// Get current block hash
    pub fn current_block_hash(ctx: &Context) -> Option<H256> {
        if let Some(number) = CurrentBlockNumber::get(ctx.store.clone()) {
            BlockHash::get(ctx.store.clone(), &number)
        } else {
            None
        }
    }

    /// Get current block number
    pub fn current_block_number(ctx: &Context) -> Option<U256> {
        CurrentBlockNumber::get(ctx.store.clone())
    }

    /// Set the latest block number
    pub fn update_block_number(
        &mut self,
        ctx: &Context,
        block_number: &U256,
    ) -> Result<()> {
        CurrentBlockNumber::put(ctx.store.clone(), block_number)?;
        self.is_store_block = true;
        Ok(())
    }

    /// Get header hash of given block id.
    pub fn block_hash(ctx: &Context, id: Option<BlockId>) -> Option<H256> {
        if let Some(id) = id {
            match id {
                BlockId::Hash(h) => Some(h),
                BlockId::Number(n) => BlockHash::get(ctx.store.clone(), &n),
            }
        } else {
            Self::current_block_hash(ctx)
        }
    }

    fn logs_bloom(logs: Vec<ethereum::Log>, bloom: &mut Bloom) {
        for log in logs {
            bloom.accrue(BloomInput::Raw(&log.address[..]));
            for topic in log.topics {
                bloom.accrue(BloomInput::Raw(&topic[..]));
            }
        }
    }
}
