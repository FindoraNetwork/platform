use baseapp::BaseApp;
use ethereum::{BlockV0 as EthereumBlock, Receipt};
use ethereum_types::{H256, U256};
use fp_evm::BlockId;
use fp_rpc_core::{
    types::{
        pubsub::{Kind, Metadata, Params, Result as PubSubResult},
        Bytes, FilteredParams, Header, Log, Rich,
    },
    EthPubSubApi::{self as EthPubSubApiT},
};
use fp_traits::base::BaseProvider;
use futures::{
    executor::ThreadPool,
    task::{FutureObj, Spawn, SpawnError},
    FutureExt, SinkExt, StreamExt,
};
use jsonrpc_core::Result as JsonRpcResult;
use jsonrpc_pubsub::{manager::SubscriptionManager, typed::Subscriber, SubscriptionId};
use lazy_static::lazy_static;
use log::{debug, warn};
use parking_lot::RwLock;
use sha3::{Digest, Keccak256};
use std::collections::BTreeMap;
use std::sync::Arc;

lazy_static! {
    static ref EXECUTOR: ThreadPool =
        ThreadPool::new().expect("Failed to create thread pool executor");
}

pub struct SubscriptionTaskExecutor;

impl Spawn for SubscriptionTaskExecutor {
    fn spawn_obj(&self, future: FutureObj<'static, ()>) -> Result<(), SpawnError> {
        EXECUTOR.spawn_ok(future);
        Ok(())
    }

    fn status(&self) -> Result<(), SpawnError> {
        Ok(())
    }
}

pub struct EthPubSubApiImpl {
    account_base_app: Arc<RwLock<BaseApp>>,
    subscriptions: SubscriptionManager,
}

impl EthPubSubApiImpl {
    pub fn new(account_base_app: Arc<RwLock<BaseApp>>) -> Self {
        Self {
            account_base_app,
            subscriptions: SubscriptionManager::new(Arc::new(SubscriptionTaskExecutor)),
        }
    }
}

impl EthPubSubApiT for EthPubSubApiImpl {
    type Metadata = Metadata;

    fn subscribe(
        &self,
        _metadata: Self::Metadata,
        subscriber: Subscriber<PubSubResult>,
        kind: Kind,
        params: Option<Params>,
    ) {
        debug!(target: "eth_rpc", "new subscribe: {:?}", kind);
        let filtered_params = match params {
            Some(Params::Logs(filter)) => FilteredParams::new(Some(filter)),
            _ => FilteredParams::default(),
        };

        let app = self.account_base_app.clone();
        match kind {
            Kind::Logs => {
                self.subscriptions.add(subscriber, |sink| {
                    let stream = self.account_base_app
                        .read()
                        .event_notify
                        .notification_stream()
                        .filter_map(move |block_id| {
                            debug!(target: "eth_rpc", "subscribe [Logs] received new block: {}", block_id);
                            let latest_block = app.read().current_block_number().unwrap();
                            let is_new_block = match block_id {
                                BlockId::Hash(_) => false,
                                BlockId::Number(number) => latest_block == number,
                            };

                            if is_new_block {
                                let block = app
                                    .read()
                                    .current_block(Some(block_id.clone()));
                                let receipts = app
                                    .read()
                                    .current_receipts(Some(block_id));

                                match (receipts, block) {
                                    (Some(receipts), Some(block)) => {
                                        futures::future::ready(Some((block, receipts)))
                                    }
                                    _ => futures::future::ready(None),
                                }
                            } else {
                                futures::future::ready(None)
                            }
                        })
                        .flat_map(move |(block, receipts)| {
                            futures::stream::iter(SubscriptionResult::new().logs(
                                block,
                                receipts,
                                &filtered_params,
                            ))
                        })
                        .map(|x| {
                            Ok::<
                                Result<PubSubResult, jsonrpc_core::types::error::Error>,
                                (),
                            >(Ok(
                                PubSubResult::Log(Box::new(x)),
                            ))
                        });
                    stream
                        .forward(sink.sink_map_err(|e| {
                            warn!(target: "eth_rpc", "Error sending notifications: {:?}", e)
                        }))
                        .map(|_| ())
                });
            }
            Kind::NewHeads => {
                self.subscriptions.add(subscriber, |sink| {
                    let stream = self.account_base_app.read().event_notify
                        .notification_stream()
                        .filter_map(move |block_id| {
                            debug!(target: "eth_rpc", "subscribe [NewHeads] received new block: {}", block_id);
                            let latest_block = app.read().current_block_number().unwrap();
                            let is_new_block = match block_id {
                                BlockId::Hash(_) => false,
                                BlockId::Number(number) => latest_block == number,
                            };

                            if is_new_block {
                                let block = app
                                    .read()
                                    .current_block(Some(block_id));
                                futures::future::ready(block)
                            } else {
                                futures::future::ready(None)
                            }
                        })
                        .map(|block| {
                            Ok::<_, ()>(Ok(SubscriptionResult::new().new_heads(block)))
                        });
                    stream
                        .forward(
                            sink.sink_map_err(|e| warn!(target: "eth_rpc", "Error sending notifications: {:?}", e)),
                        )
                        .map(|_| ())
                });
            }
            Kind::NewPendingTransactions => {
                warn!(target: "eth_rpc", "subscribe NewPendingTransactions unimplemented");
            }
            Kind::Syncing => {
                warn!(target: "eth_rpc", "subscribe Syncing unimplemented");
            }
        }
    }

    fn unsubscribe(
        &self,
        _metadata: Option<Self::Metadata>,
        subscription_id: SubscriptionId,
    ) -> JsonRpcResult<bool> {
        debug!(target: "eth_rpc", "unsubscribe id: {:?}", subscription_id);
        Ok(self.subscriptions.cancel(subscription_id))
    }
}

struct SubscriptionResult {}

impl SubscriptionResult {
    pub fn new() -> Self {
        SubscriptionResult {}
    }
    pub fn new_heads(&self, block: EthereumBlock) -> PubSubResult {
        PubSubResult::Header(Box::new(Rich {
            inner: Header {
                hash: Some(H256::from_slice(
                    Keccak256::digest(&rlp::encode(&block.header)).as_slice(),
                )),
                parent_hash: block.header.parent_hash,
                uncles_hash: block.header.ommers_hash,
                author: block.header.beneficiary,
                miner: block.header.beneficiary,
                state_root: block.header.state_root,
                transactions_root: block.header.transactions_root,
                receipts_root: block.header.receipts_root,
                number: Some(block.header.number),
                gas_used: block.header.gas_used,
                gas_limit: block.header.gas_limit,
                extra_data: Bytes(block.header.extra_data.clone()),
                logs_bloom: block.header.logs_bloom,
                timestamp: U256::from(block.header.timestamp),
                difficulty: block.header.difficulty,
                seal_fields: vec![
                    Bytes(block.header.mix_hash.as_bytes().to_vec()),
                    Bytes(block.header.nonce.as_bytes().to_vec()),
                ],
                size: Some(U256::from(rlp::encode(&block).len() as u32)),
            },
            extra_info: BTreeMap::new(),
        }))
    }
    pub fn logs(
        &self,
        block: EthereumBlock,
        receipts: Vec<Receipt>,
        params: &FilteredParams,
    ) -> Vec<Log> {
        let block_hash = Some(H256::from_slice(
            Keccak256::digest(&rlp::encode(&block.header)).as_slice(),
        ));
        let mut logs: Vec<Log> = vec![];
        let mut log_index: u32 = 0;
        for (receipt_index, receipt) in receipts.into_iter().enumerate() {
            let transaction_hash: Option<H256> = if !receipt.logs.is_empty() {
                Some(H256::from_slice(
                    Keccak256::digest(&rlp::encode(
                        &block.transactions[receipt_index as usize],
                    ))
                    .as_slice(),
                ))
            } else {
                None
            };
            for (transaction_log_index, log) in receipt.logs.into_iter().enumerate() {
                if self.add_log(block_hash.unwrap(), &log, &block, params) {
                    logs.push(Log {
                        address: log.address,
                        topics: log.topics,
                        data: Bytes(log.data),
                        block_hash,
                        block_number: Some(block.header.number),
                        transaction_hash,
                        transaction_index: Some(U256::from(receipt_index)),
                        log_index: Some(U256::from(log_index)),
                        transaction_log_index: Some(U256::from(transaction_log_index)),
                        removed: false,
                    });
                }
                log_index += 1;
            }
        }
        logs
    }
    fn add_log(
        &self,
        block_hash: H256,
        ethereum_log: &ethereum::Log,
        block: &EthereumBlock,
        params: &FilteredParams,
    ) -> bool {
        let log = Log {
            address: ethereum_log.address,
            topics: ethereum_log.topics.clone(),
            data: Bytes(ethereum_log.data.clone()),
            block_hash: None,
            block_number: None,
            transaction_hash: None,
            transaction_index: None,
            log_index: None,
            transaction_log_index: None,
            removed: false,
        };
        if params.filter.is_some() {
            let block_number = block.header.number.as_u64();
            if !params.filter_block_range(block_number)
                || !params.filter_block_hash(block_hash)
                || !params.filter_address(&log)
                || !params.filter_topics(&log)
            {
                return false;
            }
        }
        true
    }
}
