use crate::{filter_block_logs, internal_err};
use baseapp::BaseApp;
use ethereum::BlockV0 as EthereumBlock;
use ethereum_types::{H256, U256};
use fp_evm::{BlockId, TransactionStatus};
use fp_rpc_core::types::{
    BlockNumber, Filter, FilterChanges, FilterPool, FilterPoolItem, FilterType,
    FilteredParams, Index, Log,
};
use fp_rpc_core::EthFilterApi;
use fp_traits::base::BaseProvider;
use jsonrpc_core::Result;

use futures::{executor::ThreadPool, StreamExt};
use lazy_static::lazy_static;
use lru::LruCache;
use parking_lot::RwLock;
use std::{
    collections::BTreeMap,
    num::NonZeroUsize,
    sync::{Arc, Mutex},
    time,
};

lazy_static! {
    static ref POOL_FILTER: ThreadPool =
        ThreadPool::new().expect("Failed to create EthFilter thread pool executor");
}

const MAX_FILTER_SECS: u64 = 10;
const BLOCK_CACHE_SIZE: usize = 50;
const STATS_CACHE_SIZE: usize = 50;
const FILTER_RETAIN_THRESHOLD: u64 = 100;

pub struct EthFilterApiImpl {
    filter_pool: FilterPool,
    max_past_logs: u32,
    max_stored_filters: usize,
    block_data_cache: Arc<EthBlockDataCache>,
    account_base_app: Arc<RwLock<BaseApp>>,
}

impl EthFilterApiImpl {
    pub fn new(
        account_base_app: Arc<RwLock<BaseApp>>,
        max_past_logs: u32,
        max_stored_filters: usize,
    ) -> Self {
        let pool = Arc::new(Mutex::new(BTreeMap::new()));
        let instance = Self {
            filter_pool: pool.clone(),
            max_past_logs,
            max_stored_filters,
            block_data_cache: Arc::new(EthBlockDataCache::new(
                BLOCK_CACHE_SIZE,
                STATS_CACHE_SIZE,
            )),
            account_base_app: account_base_app.clone(),
        };
        POOL_FILTER.spawn_ok(Self::filter_pool_task(account_base_app, pool));
        instance
    }

    fn block_number(&self) -> Result<u64> {
        self.account_base_app
            .read()
            .chain_state
            .read()
            .height()
            .map_err(internal_err)
    }

    fn create_filter(&self, filter_type: FilterType) -> Result<U256> {
        let block_number = self.block_number()?;

        let pool = self.filter_pool.clone();
        let response = if let Ok(locked) = &mut pool.lock() {
            if locked.len() >= self.max_stored_filters {
                return Err(internal_err(format!(
                    "Filter pool is full (limit {:?}).",
                    self.max_stored_filters
                )));
            }
            let last_key = match locked.iter().next_back() {
                Some((k, _)) => *k,
                None => U256::zero(),
            };
            // Assume `max_stored_filters` is always < U256::max.
            let key = last_key.checked_add(U256::one()).unwrap();
            locked.insert(
                key,
                FilterPoolItem {
                    last_poll: BlockNumber::Num(block_number),
                    filter_type,
                    at_block: block_number,
                },
            );
            Ok(key)
        } else {
            Err(internal_err("Filter pool is not available."))
        };
        response
    }

    fn filter_range_logs(
        &self,
        ret: &mut Vec<Log>,
        max_past_logs: u32,
        filter: &Filter,
        from: u64,
        to: u64,
    ) -> Result<()> {
        // Max request duration of 10 seconds.
        let max_duration = time::Duration::from_secs(MAX_FILTER_SECS);
        let begin_request = time::Instant::now();

        // Pre-calculate BloomInput for reuse.
        let topics_input = if filter.topics.is_some() {
            let filtered_params = FilteredParams::new(Some(filter.clone()));
            Some(filtered_params.flat_topics)
        } else {
            None
        };
        let address_bloom_filter =
            FilteredParams::addresses_bloom_filter(&filter.address);
        let topics_bloom_filter = FilteredParams::topics_bloom_filter(&topics_input);

        let mut current = to;
        while current >= from {
            let block = self
                .block_data_cache
                .current_block(&self.account_base_app, current.into());

            if let Some(block) = block {
                if FilteredParams::address_in_bloom(
                    block.header.logs_bloom,
                    &address_bloom_filter,
                ) && FilteredParams::topics_in_bloom(
                    block.header.logs_bloom,
                    &topics_bloom_filter,
                ) {
                    let statuses = self.block_data_cache.current_transaction_statuses(
                        &self.account_base_app,
                        current.into(),
                    );
                    if let Some(statuses) = statuses {
                        filter_block_logs(ret, filter, block, statuses);
                    }
                }
            } else {
                // stop when past the 1st ethereum block
                break;
            }

            // Check for restrictions
            if ret.len() as u32 > max_past_logs {
                return Err(internal_err(format!(
                    "query returned more than {} results",
                    max_past_logs
                )));
            }
            if begin_request.elapsed() > max_duration {
                return Err(internal_err(format!(
                    "query timeout of {} seconds exceeded",
                    max_duration.as_secs()
                )));
            }
            current = current.saturating_sub(1);
        }
        Ok(())
    }

    async fn filter_pool_task(
        app: Arc<RwLock<BaseApp>>,
        filter_pool: Arc<Mutex<BTreeMap<U256, FilterPoolItem>>>,
    ) {
        let mut stream = app.read().event_notify.notification_stream();

        while let Some(block_id) = stream.next().await {
            let latest_block = app.read().current_block_number().unwrap();
            let (is_new_block, number) = match block_id {
                BlockId::Hash(_) => (false, U256::zero()),
                BlockId::Number(number) => (latest_block == number, number),
            };

            if is_new_block {
                if let Ok(filter_pool) = &mut filter_pool.lock() {
                    // 1. We collect all keys to remove.
                    // 2. We remove them.
                    let remove_list: Vec<_> = filter_pool
                        .iter()
                        .filter_map(|(&k, v)| {
                            let lifespan_limit = v.at_block + FILTER_RETAIN_THRESHOLD;
                            if U256::from(lifespan_limit) <= number {
                                Some(k)
                            } else {
                                None
                            }
                        })
                        .collect();

                    for key in remove_list {
                        filter_pool.remove(&key);
                    }
                }
            }
        }
    }
}

impl EthFilterApi for EthFilterApiImpl {
    fn new_filter(&self, filter: Filter) -> Result<U256> {
        self.create_filter(FilterType::Log(filter))
    }

    fn new_block_filter(&self) -> Result<U256> {
        self.create_filter(FilterType::Block)
    }

    fn new_pending_transaction_filter(&self) -> Result<U256> {
        Err(internal_err(
            "new_pending_transaction_filter method not available.",
        ))
    }

    fn filter_changes(&self, index: Index) -> Result<FilterChanges> {
        let key = U256::from(index.value());
        let cur_number = self.block_number()?;
        let pool = self.filter_pool.clone();
        // Try to lock.
        let response = if let Ok(locked) = &mut pool.lock() {
            // Try to get key.
            if let Some(pool_item) = locked.clone().get(&key) {
                match &pool_item.filter_type {
                    // For each block created since last poll, get a vector of ethereum hashes.
                    FilterType::Block => {
                        let last = pool_item.last_poll.to_min_block_num().unwrap();
                        let next = cur_number + 1;
                        let mut ethereum_hashes: Vec<H256> = Vec::new();
                        for n in last..next {
                            let block = self
                                .block_data_cache
                                .current_block(&self.account_base_app, n.into());
                            if let Some(block) = block {
                                ethereum_hashes.push(block.header.hash())
                            }
                        }
                        // Update filter `last_poll`.
                        locked.insert(
                            key,
                            FilterPoolItem {
                                last_poll: BlockNumber::Num(next),
                                filter_type: pool_item.clone().filter_type,
                                at_block: pool_item.at_block,
                            },
                        );
                        Ok(FilterChanges::Hashes(ethereum_hashes))
                    }
                    // For each event since last poll, get a vector of ethereum logs.
                    FilterType::Log(filter) => {
                        // Either the filter-specific `to` block or current block.
                        let mut to_number = filter
                            .to_block
                            .clone()
                            .and_then(|v| v.to_min_block_num())
                            .unwrap_or(cur_number);

                        if to_number > cur_number {
                            to_number = cur_number;
                        }

                        // The from clause is the max(last_poll, filter_from).
                        let last_poll = pool_item.last_poll.to_min_block_num().unwrap();
                        let filter_from = filter
                            .from_block
                            .clone()
                            .and_then(|v| v.to_min_block_num())
                            .unwrap_or(last_poll);

                        let from_number = std::cmp::max(last_poll, filter_from);

                        // Build the response.
                        let mut ret: Vec<Log> = Vec::new();
                        self.filter_range_logs(
                            &mut ret,
                            self.max_past_logs,
                            filter,
                            from_number,
                            to_number,
                        )?;
                        // Update filter `last_poll`.
                        locked.insert(
                            key,
                            FilterPoolItem {
                                last_poll: BlockNumber::Num(cur_number + 1),
                                filter_type: pool_item.clone().filter_type,
                                at_block: pool_item.at_block,
                            },
                        );
                        Ok(FilterChanges::Logs(ret))
                    }
                    // Should never reach here.
                    _ => Err(internal_err("Method not available.")),
                }
            } else {
                Err(internal_err(format!("Filter id {:?} does not exist.", key)))
            }
        } else {
            Err(internal_err("Filter pool is not available."))
        };
        response
    }

    fn filter_logs(&self, index: Index) -> Result<Vec<Log>> {
        let key = U256::from(index.value());
        let pool = self.filter_pool.clone();
        // Try to lock.
        let response = if let Ok(locked) = &mut pool.lock() {
            // Try to get key.
            if let Some(pool_item) = locked.clone().get(&key) {
                match &pool_item.filter_type {
                    FilterType::Log(filter) => {
                        let cur_number = self.block_number()?;
                        let from_number = filter
                            .from_block
                            .clone()
                            .and_then(|v| v.to_min_block_num())
                            .unwrap_or(cur_number);
                        let mut to_number = filter
                            .to_block
                            .clone()
                            .and_then(|v| v.to_min_block_num())
                            .unwrap_or(cur_number);

                        if to_number > cur_number {
                            to_number = cur_number;
                        }

                        let mut ret: Vec<Log> = Vec::new();
                        self.filter_range_logs(
                            &mut ret,
                            self.max_past_logs,
                            filter,
                            from_number,
                            to_number,
                        )?;
                        Ok(ret)
                    }
                    _ => Err(internal_err(format!(
                        "Filter id {:?} is not a Log filter.",
                        key
                    ))),
                }
            } else {
                Err(internal_err(format!("Filter id {:?} does not exist.", key)))
            }
        } else {
            Err(internal_err("Filter pool is not available."))
        };
        response
    }

    fn uninstall_filter(&self, index: Index) -> Result<bool> {
        let key = U256::from(index.value());
        let pool = self.filter_pool.clone();
        // Try to lock.
        let response = if let Ok(locked) = &mut pool.lock() {
            if locked.remove(&key).is_some() {
                Ok(true)
            } else {
                Err(internal_err(format!("Filter id {:?} does not exist.", key)))
            }
        } else {
            Err(internal_err("Filter pool is not available."))
        };
        response
    }
}

/// Stores an LRU cache for block data and their transaction statuses.
/// These are large and take a lot of time to fetch from the database.
/// Storing them in an LRU cache will allow to reduce database accesses
/// when many subsequent requests are related to the same blocks.
pub struct EthBlockDataCache {
    blocks: parking_lot::Mutex<LruCache<U256, EthereumBlock>>,
    statuses: parking_lot::Mutex<LruCache<U256, Vec<TransactionStatus>>>,
}

impl EthBlockDataCache {
    /// Create a new cache with provided cache sizes.
    pub fn new(blocks_cache_size: usize, statuses_cache_size: usize) -> Self {
        let blocks_cache_size = NonZeroUsize::new(blocks_cache_size)
            .unwrap_or(unsafe { NonZeroUsize::new_unchecked(BLOCK_CACHE_SIZE) });
        let statuses_cache_size = NonZeroUsize::new(statuses_cache_size)
            .unwrap_or(unsafe { NonZeroUsize::new_unchecked(STATS_CACHE_SIZE) });
        Self {
            blocks: parking_lot::Mutex::new(LruCache::new(blocks_cache_size)),
            statuses: parking_lot::Mutex::new(LruCache::new(statuses_cache_size)),
        }
    }

    /// Cache for `handler.current_block`.
    pub fn current_block(
        &self,
        handler: &Arc<RwLock<BaseApp>>,
        id: U256,
    ) -> Option<EthereumBlock> {
        {
            let mut cache = self.blocks.lock();
            if let Some(block) = cache.get(&id).cloned() {
                return Some(block);
            }
        }

        if let Some(block) = handler.read().current_block(Some(BlockId::Number(id))) {
            let mut cache = self.blocks.lock();
            cache.put(id, block.clone());

            return Some(block);
        }

        None
    }

    /// Cache for `handler.current_transaction_statuses`.
    pub fn current_transaction_statuses(
        &self,
        handler: &Arc<RwLock<BaseApp>>,
        id: U256,
    ) -> Option<Vec<TransactionStatus>> {
        {
            let mut cache = self.statuses.lock();
            if let Some(statuses) = cache.get(&id).cloned() {
                return Some(statuses);
            }
        }

        if let Some(statuses) = handler
            .read()
            .current_transaction_statuses(Some(BlockId::Number(id)))
        {
            let mut cache = self.statuses.lock();
            cache.put(id, statuses.clone());

            return Some(statuses);
        }

        None
    }
}
