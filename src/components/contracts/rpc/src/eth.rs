use crate::utils::{
    // build_method_not_found,
    convert_error_to_rpc_error,
    convert_join_error_to_rpc_error,
};
use crate::{error_on_execution_failure, internal_err};
use baseapp::{extensions::SignedExtra, BaseApp};
use config::abci::global_cfg::CFG;
use ethereum::{
    BlockV2 as EthereumBlock,
    // LegacyTransactionMessage as EthereumTransactionMessage,
    TransactionV2 as EthereumTransaction,
};
use ethereum_types::{BigEndianHash, Bloom, H160, H256, H512, H64, U256, U64};
use evm::{ExitError, ExitReason};
use fp_evm::{BlockId, Runner, TransactionStatus};
use fp_rpc_core::types::{
    Block, BlockNumber, BlockTransactions, Bytes, CallRequest, Filter, FilteredParams,
    Index, Log, Receipt, Rich, RichBlock, SyncStatus, Transaction, TransactionMessage,
    TransactionRequest, Work,
};
use fp_rpc_core::EthApi;
use fp_traits::{
    base::BaseProvider,
    evm::{AddressMapping, EthereumAddressMapping, FeeCalculator},
};
use fp_types::{
    actions,
    actions::evm::{Call, CallEip1559, Create, CreateEip1559},
    assemble::UncheckedTransaction,
};
use fp_utils::ecdsa::SecpPair;
use fp_utils::tx::EvmRawTxWrapper;
use hex_literal::hex;
use jsonrpc_core::{futures::future, BoxFuture, Result};
use lazy_static::lazy_static;
use parking_lot::RwLock;
use ruc::eg;
use sha3::{Digest, Keccak256};
use std::collections::BTreeMap;
use std::convert::Into;
use std::ops::Range;
use std::sync::Arc;
use tendermint::abci::Code;
use tendermint_rpc::{Client, HttpClient};
use tokio::runtime::Runtime;
use tokio::task::spawn_blocking;
use tracing::{debug, warn};

lazy_static! {
    static ref RT: Runtime =
        Runtime::new().expect("Failed to create thread pool executor");
    static ref EVM_FIRST_BLOCK_HEIGHT: u64 = {
        let h: u64 = std::env::var("EVM_FIRST_BLOCK_HEIGHT")
            .map(|h| {
                h.parse()
                    .expect("`EVM_FIRST_BLOCK_HEIGHT` is not set correctly.")
            })
            .unwrap_or(1424654);
        h
    };
}

pub struct EthApiImpl {
    account_base_app: Arc<RwLock<BaseApp>>,
    signers: Vec<SecpPair>,
    tm_client: Arc<HttpClient>,
    max_past_logs: u32,
}

impl EthApiImpl {
    pub fn new(
        url: String,
        account_base_app: Arc<RwLock<BaseApp>>,
        signers: Vec<SecpPair>,
        max_past_logs: u32,
    ) -> Self {
        Self {
            account_base_app,
            signers,
            tm_client: Arc::new(HttpClient::new(url.as_str()).unwrap()),
            max_past_logs,
        }
    }

    /// Some(height): versioned
    ///
    /// Some(0): latest
    ///
    /// None: pending
    pub fn block_number_to_height(
        account_base_app: Arc<RwLock<BaseApp>>,
        number: Option<BlockNumber>,
    ) -> Result<Option<u64>> {
        let range = Self::version_range(account_base_app.clone())?;
        let height = match number.unwrap_or(BlockNumber::Latest) {
            BlockNumber::Hash {
                hash,
                require_canonical: _,
            } => match account_base_app
                .read()
                .current_block(Some(BlockId::Hash(hash)))
            {
                Some(block) => Some(block.header.number.as_u64()),
                None => {
                    return Err(internal_err(format!(
                        "block number not found, hash: {:?}",
                        hash
                    )))
                }
            },
            BlockNumber::Num(num) => {
                if range.contains(&num) || num == range.end {
                    Some(num)
                } else {
                    return Err(internal_err(format!(
                        "block number: {} exceeds version range: {:?}",
                        num, range
                    )));
                }
            }
            BlockNumber::Latest => Some(0),
            BlockNumber::Earliest => Some(range.start),
            BlockNumber::Pending => None,
        };
        Ok(height)
    }

    /// get the range of queryable versioned data [lower, upper)
    pub fn version_range(account_base_app: Arc<RwLock<BaseApp>>) -> Result<Range<u64>> {
        let range = account_base_app
            .read()
            .chain_state
            .read()
            .get_ver_range()
            .map_err(internal_err)?;
        Ok(range)
    }

    fn balance_sync(
        account_base_app: Arc<RwLock<BaseApp>>,
        address: H160,
        number: Option<BlockNumber>,
    ) -> Result<U256> {
        debug!(target: "eth_rpc", "balance, address:{:?}, number:{:?}", address, number);

        // let height = self.block_number_to_height(number)?;
        let height = Self::block_number_to_height(account_base_app.clone(), number)?;
        let account_id = EthereumAddressMapping::convert_to_account_id(address);
        if let Ok(sa) = account_base_app.read().account_of(&account_id, height) {
            Ok(sa.balance)
        } else {
            Ok(U256::zero())
        }
    }

    fn accounts_sync(&self) -> Result<Vec<H160>> {
        let mut accounts = Vec::new();
        for signer in self.signers.iter() {
            accounts.push(signer.address());
        }
        Ok(accounts)
    }
}

impl EthApi for EthApiImpl {
    fn protocol_version(&self) -> BoxFuture<Result<u64>> {
        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || account_base_app.read().app_version);

        Box::pin(async move { task.await.map_err(convert_join_error_to_rpc_error) })
    }

    fn hashrate(&self) -> BoxFuture<Result<U256>> {
        Box::pin(async move { Ok(U256::zero()) })
    }

    fn chain_id(&self) -> Result<Option<U64>> {
        Ok(Some(<BaseApp as module_evm::Config>::ChainId::get().into()))
    }

    // let curr_height = match self.block_number() {
    //     Ok(h) => h.as_u64(),
    //     Err(e) => return Box::pin(future::err(e)),
    // };

    // let message = EthereumTransactionMessage {
    //     nonce,
    //     gas_price: request.gas_price.unwrap_or_else(|| {
    //         <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price(
    //             curr_height,
    //         )
    //     }),
    //     gas_limit: request.gas.unwrap_or_else(U256::max_value),
    //     value: request.value.unwrap_or_else(U256::zero),
    //     input: request.data.map(|s| s.into_vec()).unwrap_or_default(),
    //     action: match request.to {
    //         Some(to) => ethereum::TransactionAction::Call(to),
    //         None => ethereum::TransactionAction::Create,
    //     },
    //     chain_id: chain_id.map(|s| s.as_u64()),
    // };

    fn accounts(&self) -> Result<Vec<H160>> {
        self.accounts_sync()
    }

    fn balance(
        &self,
        address: H160,
        number: Option<BlockNumber>,
    ) -> BoxFuture<Result<U256>> {
        debug!(target: "eth_rpc", "balance, address:{:?}, number:{:?}", address, number);
        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            Self::balance_sync(account_base_app, address, number)
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn send_transaction(&self, request: TransactionRequest) -> BoxFuture<Result<H256>> {
        debug!(target: "eth_rpc", "send_transaction, request:{:?}", request);

        let from = match request.from {
            Some(from) => from,
            None => {
                let accounts = match self.accounts() {
                    Ok(accounts) => accounts,
                    Err(e) => return Box::pin(future::err(e)),
                };

                match accounts.get(0) {
                    Some(account) => *account,
                    None => {
                        return Box::pin(future::err(internal_err(
                            "no signer available",
                        )));
                    }
                }
            }
        };

        let nonce = match request.nonce {
            Some(nonce) => nonce,
            None => match self.transaction_count(from, None) {
                Ok(nonce) => nonce,
                Err(e) => return Box::pin(future::err(e)),
            },
        };

        let chain_id = match self.chain_id() {
            Ok(Some(chain_id)) => chain_id.as_u64(),
            Ok(None) => {
                return Box::pin(future::err(internal_err("chain id not available")))
            }
            Err(e) => return Box::pin(future::err(e)),
        };

        // let hash = self.client.info().best_hash;
        let curr_height = match self.block_number() {
            Ok(h) => h.as_u64(),
            Err(e) => return Box::pin(future::err(e)),
        };

        let gas_price = request.gas_price.unwrap_or_else(|| {
            <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price(curr_height)
        });
        let block = self.account_base_app.read().current_block(None);
        let gas_limit = match request.gas {
            Some(gas_limit) => gas_limit,
            None => {
                if let Some(block) = block {
                    block.header.gas_limit
                } else {
                    <BaseApp as module_evm::Config>::BlockGasLimit::get()
                }
            }
        };

        let max_fee_per_gas = request.max_fee_per_gas;

        let message: Option<TransactionMessage> = request.into();
        let message = match message {
            Some(TransactionMessage::Legacy(mut m)) => {
                m.nonce = nonce;
                m.chain_id = Some(chain_id);
                m.gas_limit = gas_limit;
                m.gas_price = gas_price;
                TransactionMessage::Legacy(m)
            }
            Some(TransactionMessage::EIP1559(mut m)) => {
                if CFG.checkpoint.enable_eip1559_height > curr_height {
                    return Box::pin(future::err(internal_err(format!(
                        "eip1559 not enabled at height: {:?}",
                        curr_height
                    ))));
                }
                m.nonce = nonce;
                m.chain_id = chain_id;
                m.gas_limit = gas_limit;
                if max_fee_per_gas.is_none() {
                    m.max_fee_per_gas = self.gas_price().unwrap_or_default();
                }
                TransactionMessage::EIP1559(m)
            }
            _ => {
                return Box::pin(future::err(internal_err("Transaction Type Error")));
            }
        };

        let mut transaction = None;
        for signer in &self.signers {
            if signer.address() == from {
                match sign_transaction_message(message, &H256::from(signer.seed()))
                    .map_err(internal_err)
                {
                    Ok(tx) => transaction = Some(tx),
                    Err(e) => return Box::pin(future::err(e)),
                }
                break;
            }
        }

        let transaction = match transaction {
            Some(transaction) => transaction,
            None => {
                return Box::pin(future::err(internal_err("no signer available")));
            }
        };
        let transaction_hash = transaction.hash();

        let function =
            actions::Action::Ethereum(actions::ethereum::Action::Transact(transaction));
        let txn = serde_json::to_vec(
            &UncheckedTransaction::<SignedExtra>::new_unsigned(function),
        )
        .map_err(internal_err);
        if let Err(e) = txn {
            return Box::pin(future::err(e));
        }

        // check_tx and broadcast
        let client = self.tm_client.clone();
        let txn_with_tag = EvmRawTxWrapper::wrap(&txn.unwrap());
        let (tx, rx) = std::sync::mpsc::channel();
        RT.spawn(async move {
            let resp = client.broadcast_tx_sync(txn_with_tag.into()).await;
            tx.send(resp).unwrap();
        });

        // fetch response
        if let Ok(resp) = rx.recv().unwrap() {
            if resp.code != Code::Ok {
                return Box::pin(future::err(internal_err(resp.log)));
            }
        } else {
            return Box::pin(future::err(internal_err(String::from(
                "send_transaction: broadcast_tx_sync failed",
            ))));
        }

        Box::pin(future::ok(transaction_hash))

        // Err(build_method_not_found())
    }

    fn call(
        &self,
        request: CallRequest,
        _: Option<BlockNumber>,
    ) -> BoxFuture<Result<Bytes>> {
        debug!(target: "eth_rpc", "call, request:{:?}", request);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || -> Result<Bytes> {
            let CallRequest {
                from,
                to,
                gas_price,
                max_fee_per_gas,
                max_priority_fee_per_gas,
                gas,
                value,
                data,
                nonce,
            } = request;

            let is_eip1559 = max_fee_per_gas.is_some()
                && max_priority_fee_per_gas.is_some()
                && max_fee_per_gas.unwrap() > U256::from(0)
                && max_priority_fee_per_gas.unwrap() > U256::from(0);

            let (gas_price, max_fee_per_gas, max_priority_fee_per_gas) = {
                let details =
                    fee_details(gas_price, max_fee_per_gas, max_priority_fee_per_gas)?;
                (
                    details.gas_price,
                    details.max_fee_per_gas,
                    details.max_priority_fee_per_gas,
                )
            };

            let block = account_base_app.read().current_block(None);
            // use given gas limit or query current block's limit
            let gas_limit = match gas {
                Some(amount) => amount,
                None => {
                    if let Some(block) = block.clone() {
                        block.header.gas_limit
                    } else {
                        <BaseApp as module_evm::Config>::BlockGasLimit::get()
                    }
                }
            };
            let data = data.map(|d| d.0).unwrap_or_default();

            let mut config = <BaseApp as module_ethereum::Config>::config().clone();
            config.estimate = true;

            let mut ctx = account_base_app
                .read()
                .create_query_context(None, false)
                .map_err(|err| {
                    internal_err(format!("create query context error: {:?}", err))
                })?;
            if let Some(block) = block {
                ctx.header
                    .mut_time()
                    .set_seconds(block.header.timestamp as i64);
                ctx.header.height = block.header.number.as_u64() as i64;
                ctx.header.proposer_address =
                    Vec::from(block.header.beneficiary.as_bytes())
            }

            if !is_eip1559 {
                match to {
                    Some(to) => {
                        let call = Call {
                            source: from.unwrap_or_default(),
                            target: to,
                            input: data,
                            value: value.unwrap_or_default(),
                            gas_limit: gas_limit.as_u64(),
                            gas_price,
                            nonce,
                        };

                        let info = <BaseApp as module_ethereum::Config>::Runner::call(
                            &ctx, call, &config,
                        )
                        .map_err(|err| {
                            internal_err(format!("evm runner call error: {:?}", err))
                        })?;
                        debug!(target: "eth_rpc", "evm runner call result: {:?}", info);

                        error_on_execution_failure(&info.exit_reason, &info.value)?;

                        Ok(Bytes(info.value))
                    }
                    None => {
                        let create = Create {
                            source: from.unwrap_or_default(),
                            init: data,
                            value: value.unwrap_or_default(),
                            gas_limit: gas_limit.as_u64(),
                            gas_price,
                            nonce,
                        };

                        let info = <BaseApp as module_ethereum::Config>::Runner::create(
                            &ctx, create, &config,
                        )
                        .map_err(|err| {
                            internal_err(format!("evm runner create error: {:?}", err))
                        })?;
                        debug!(target: "eth_rpc", "evm runner create result: {:?}", info);

                        error_on_execution_failure(&info.exit_reason, &[])?;

                        Ok(Bytes(info.value[..].to_vec()))
                    }
                }
            } else {
                match to {
                    Some(to) => {
                        let call = CallEip1559 {
                            source: from.unwrap_or_default(),
                            target: to,
                            input: data,
                            value: value.unwrap_or_default(),
                            gas_limit: gas_limit.as_u64(),
                            max_fee_per_gas,
                            max_priority_fee_per_gas,
                            nonce,
                        };

                        let info =
                            <BaseApp as module_ethereum::Config>::Runner::call_eip1559(
                                &ctx, call, &config,
                            )
                            .map_err(|err| {
                                internal_err(format!("evm runner call error: {:?}", err))
                            })?;
                        debug!(target: "eth_rpc", "evm runner call result: {:?}", info);

                        error_on_execution_failure(&info.exit_reason, &info.value)?;

                        Ok(Bytes(info.value))
                    }
                    None => {
                        let create = CreateEip1559 {
                            source: from.unwrap_or_default(),
                            init: data,
                            value: value.unwrap_or_default(),
                            gas_limit: gas_limit.as_u64(),
                            max_fee_per_gas,
                            max_priority_fee_per_gas,
                            nonce,
                        };

                        let info =
                            <BaseApp as module_ethereum::Config>::Runner::create_eip1559(
                                &ctx, create, &config,
                            )
                            .map_err(|err| {
                                internal_err(format!("evm runner create error: {:?}", err))
                            })?;
                        debug!(target: "eth_rpc", "evm runner create result: {:?}", info);

                        error_on_execution_failure(&info.exit_reason, &[])?;

                        Ok(Bytes(info.value[..].to_vec()))
                    }
                }
            }
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn syncing(&self) -> BoxFuture<Result<SyncStatus>> {
        // TODO
        Box::pin(async move { Ok(SyncStatus::None) })
    }

    fn author(&self) -> BoxFuture<Result<H160>> {
        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let block = account_base_app.read().current_block(None);
            if let Some(block) = block {
                Ok(block.header.beneficiary)
            } else {
                Err(internal_err("not found author"))
            }
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn is_mining(&self) -> BoxFuture<Result<bool>> {
        Box::pin(async move { Ok(false) })
    }

    fn gas_price(&self) -> Result<U256> {
        Ok(
            <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price(
                self.block_number()?.as_u64(),
            ),
        )
    }

    fn block_number(&self) -> Result<U256> {
        let height = self
            .account_base_app
            .read()
            .chain_state
            .read()
            .height()
            .map_err(internal_err)?;
        Ok(U256::from(height))
    }

    fn storage_at(
        &self,
        address: H160,
        index: U256,
        number: Option<BlockNumber>,
    ) -> BoxFuture<Result<H256>> {
        debug!(target: "eth_rpc", "storage_at, address:{:?}, index:{:?}, number:{:?}", address, index, number);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let height = Self::block_number_to_height(account_base_app.clone(), number)?;
            Ok(account_base_app
                .read()
                .account_storage_at(address, H256::from_uint(&index), height)
                .unwrap_or_default())
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn block_by_hash(
        &self,
        hash: H256,
        full: bool,
    ) -> BoxFuture<Result<Option<RichBlock>>> {
        debug!(target: "eth_rpc", "block_by_hash, hash:{:?}, full:{:?}", hash, full);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let block = account_base_app
                .read()
                .current_block(Some(BlockId::Hash(hash)));
            let statuses = account_base_app
                .read()
                .current_transaction_statuses(Some(BlockId::Hash(hash)));

            let base_fee = account_base_app.read().base_fee(Some(BlockId::Hash(hash)));

            match (block, statuses) {
                (Some(block), Some(statuses)) => Ok(Some(rich_block_build(
                    block,
                    statuses.into_iter().map(Some).collect(),
                    Some(hash),
                    full,
                    base_fee,
                ))),
                _ => Ok(None),
            }
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn block_by_number(
        &self,
        number: BlockNumber,
        full: bool,
    ) -> BoxFuture<Result<Option<RichBlock>>> {
        debug!(target: "eth_rpc", "block_by_number, number:{:?}, full:{:?}", number, full);

        let account_base_app = self.account_base_app.clone();
        //check if height exits.
        let height = match number {
            BlockNumber::Num(h) => Some(h),
            _ => None,
        };

        let task = spawn_blocking(move || -> Result<Option<RichBlock>> {
            if let Some(h) = height {
                if 0 < h && h < CFG.checkpoint.evm_first_block_height as u64 {
                    return Ok(Some(dummy_block(h, full)));
                }
            }

            let id = native_block_id(Some(number));
            let block = account_base_app.read().current_block(id.clone());
            let statuses = account_base_app
                .read()
                .current_transaction_statuses(id.clone());

            let base_fee = account_base_app.read().base_fee(id);

            match (block, statuses) {
                (Some(block), Some(statuses)) => {
                    let hash = block.header.hash();

                    Ok(Some(rich_block_build(
                        block,
                        statuses.into_iter().map(Some).collect(),
                        Some(hash),
                        full,
                        base_fee,
                    )))
                }
                _ => Ok(if let Some(h) = height {
                    if 0 < h && h < *EVM_FIRST_BLOCK_HEIGHT {
                        Some(dummy_block(h, full))
                    } else {
                        None
                    }
                } else {
                    None
                }),
            }
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn transaction_count(
        &self,
        address: H160,
        number: Option<BlockNumber>,
    ) -> Result<U256> {
        debug!(target: "eth_rpc", "transaction_count, address:{:?}, number:{:?}", address, number);

        let account_base_app = self.account_base_app.clone();

        let height = Self::block_number_to_height(account_base_app.clone(), number)?;
        let account_id =
            <BaseApp as module_evm::Config>::AddressMapping::convert_to_account_id(
                address,
            );
        let sa = account_base_app
            .read()
            .account_of(&account_id, height)
            .unwrap_or_default();
        Ok(sa.nonce)
    }

    fn block_transaction_count_by_hash(
        &self,
        hash: H256,
    ) -> BoxFuture<Result<Option<U256>>> {
        debug!(target: "eth_rpc", "block_transaction_count_by_hash, hash:{:?}", hash);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let block = account_base_app
                .read()
                .current_block(Some(BlockId::Hash(hash)));
            match block {
                Some(block) => Ok(Some(U256::from(block.transactions.len()))),
                None => Ok(None),
            }
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn block_transaction_count_by_number(
        &self,
        number: BlockNumber,
    ) -> BoxFuture<Result<Option<U256>>> {
        debug!(target: "eth_rpc", "block_transaction_count_by_number, number:{:?}", number);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let id = native_block_id(Some(number));
            let block = account_base_app.read().current_block(id);
            match block {
                Some(block) => Ok(Some(U256::from(block.transactions.len()))),
                None => Ok(None),
            }
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn block_uncles_count_by_hash(&self, _: H256) -> BoxFuture<Result<U256>> {
        Box::pin(async move { Ok(U256::zero()) })
    }

    fn block_uncles_count_by_number(&self, _: BlockNumber) -> BoxFuture<Result<U256>> {
        Box::pin(async move { Ok(U256::zero()) })
    }

    fn code_at(
        &self,
        address: H160,
        number: Option<BlockNumber>,
    ) -> BoxFuture<Result<Bytes>> {
        debug!(target: "eth_rpc", "code_at, address:{:?}, number:{:?}", address, number);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            // FRA (FRC20 precompile)
            if address == H160::from_low_u64_be(0x1000) {
                return Ok(Bytes::new(b"fra".to_vec()));
            }

            let height = Self::block_number_to_height(account_base_app.clone(), number)?;
            Ok(account_base_app
                .read()
                .account_code_at(address, height)
                .unwrap_or_default()
                .into())
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn send_raw_transaction(&self, bytes: Bytes) -> BoxFuture<Result<H256>> {
        let slice = &bytes.0[..];
        if slice.is_empty() {
            return Box::pin(future::err(internal_err("transaction data is empty")));
        }
        // let first = slice.get(0).unwrap();
        let first = slice.first().unwrap();
        let transaction = if first > &0x7f {
            // Legacy transaction. Decode and wrap in envelope.
            match rlp::decode::<ethereum::TransactionV0>(slice) {
                Ok(transaction) => ethereum::TransactionV2::Legacy(transaction),
                Err(_) => {
                    return Box::pin(future::err(internal_err(
                        "decode transaction failed",
                    )));
                }
            }
        } else {
            let curr_height = match self.block_number() {
                Ok(h) => h.as_u64(),
                Err(e) => return Box::pin(future::err(e)),
            };

            if CFG.checkpoint.enable_eip1559_height > curr_height {
                return Box::pin(future::err(internal_err(format!(
                    "eip1559 not enabled at height: {:?}",
                    curr_height
                ))));
            }
            // Typed Transaction.
            // `ethereum` crate decode implementation for `TransactionV2` expects a valid rlp input,
            // and EIP-1559 breaks that assumption by prepending a version byte.
            // We re-encode the payload input to get a valid rlp, and the decode implementation will strip
            // them to check the transaction version byte.
            let extend = rlp::encode(&slice);
            match rlp::decode::<ethereum::TransactionV2>(&extend[..]) {
                Ok(transaction) => transaction,
                Err(_) => {
                    return Box::pin(future::err(internal_err(
                        "decode transaction failed",
                    )))
                }
            }
        };

        debug!(target: "eth_rpc", "send_raw_transaction :{:?}", transaction);

        let transaction_hash = transaction.hash();

        let function =
            actions::Action::Ethereum(actions::ethereum::Action::Transact(transaction));
        let txn = serde_json::to_vec(
            &UncheckedTransaction::<SignedExtra>::new_unsigned(function),
        )
        .map_err(internal_err);
        if let Err(e) = txn {
            return Box::pin(future::err(e));
        }

        // check_tx and broadcast
        let client = self.tm_client.clone();
        let txn_with_tag = EvmRawTxWrapper::wrap(&txn.unwrap());

        Box::pin(async move {
            let resp = client.broadcast_tx_sync(txn_with_tag.into()).await;

            match resp {
                Ok(resp) => {
                    if resp.code != Code::Ok {
                        return Err(convert_error_to_rpc_error(resp));
                    }

                    Ok(transaction_hash)
                }
                Err(e) => Err(convert_error_to_rpc_error(e)),
            }
        })
    }

    fn estimate_gas(
        &self,
        request: CallRequest,
        number: Option<BlockNumber>,
    ) -> BoxFuture<Result<U256>> {
        debug!(target: "eth_rpc", "estimate_gas, block number {:?} request:{:?}", number, request);

        let is_eip1559 = request.max_fee_per_gas.is_some()
            && request.max_priority_fee_per_gas.is_some()
            && request.max_fee_per_gas.unwrap() > U256::from(0)
            && request.max_priority_fee_per_gas.unwrap() > U256::from(0);

        let (gas_price, max_fee_per_gas, max_priority_fee_per_gas) = {
            if let Ok(details) = fee_details(
                request.gas_price,
                request.max_fee_per_gas,
                request.max_priority_fee_per_gas,
            ) {
                (
                    details.gas_price,
                    details.max_fee_per_gas,
                    details.max_priority_fee_per_gas,
                )
            } else {
                return Box::pin(async move {
                    Err(internal_err("estimate_gas fee_details err!!!".to_string()))
                });
            }
        };

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let (block_id, pending) = match number.unwrap_or(BlockNumber::Latest) {
                BlockNumber::Num(num) => {
                    let range = Self::version_range(account_base_app.clone())?;
                    if range.contains(&num) || num == range.end {
                        (Some(BlockId::Number(U256::from(num))), false)
                    } else {
                        return Err(internal_err(format!(
                            "block number: {} exceeds version range: {:?}",
                            num, range
                        )));
                    }
                }
                BlockNumber::Earliest => (
                    Some(BlockId::Number(U256::from(
                        Self::version_range(account_base_app.clone())?.start,
                    ))),
                    false,
                ),
                BlockNumber::Hash {
                    hash,
                    require_canonical: _,
                } => {
                    if let Some(block) = account_base_app
                        .read()
                        .current_block(Some(BlockId::Hash(hash)))
                    {
                        (Some(BlockId::Number(block.header.number)), false)
                    } else {
                        return Err(internal_err("Cannot find the specified block"));
                    }
                }
                BlockNumber::Latest => (None, false),
                BlockNumber::Pending => (None, true),
            };

            let gas_limit = <BaseApp as module_evm::Config>::BlockGasLimit::get();

            let mut highest = if let Some(gas) = request.gas {
                gas
            } else if let Some(block) = account_base_app.read().current_block(block_id) {
                block.header.gas_limit
            } else {
                gas_limit
            };

            // recap gas limit according to account balance
            if let Some(from) = request.from {
                let gas_price = request.gas_price.unwrap_or_default();
                if gas_price > U256::zero() {
                    let balance =
                        Self::balance_sync(account_base_app.clone(), from, None)
                            .unwrap_or_default();
                    let mut available = balance;
                    if let Some(value) = request.value {
                        if value > available {
                            return Err(internal_err("insufficient funds for transfer"));
                        }
                        available -= value;
                    }
                    let allowance = available / gas_price;
                    if highest < allowance {
                        tracing::warn!(
                        "Gas estimation capped by limited funds original {} balance {} sent {} feecap {} fundable {}",
                        highest,
                        balance,
                        request.value.unwrap_or_default(),
                        gas_price,
                        allowance
                    );
                        highest = allowance;
                    }
                }
            }

            struct ExecuteResult {
                data: Vec<u8>,
                exit_reason: ExitReason,
                used_gas: U256,
            }

            let execute_call_or_create = move |request: CallRequest,
                                               gas_limit|
                  -> Result<ExecuteResult> {
                let ctx = account_base_app
                    .read()
                    .create_query_context(if pending { None } else { Some(0) }, false)
                    .map_err(|err| {
                        internal_err(format!("create query context error: {:?}", err))
                    })?;

                let CallRequest {
                    from,
                    to,
                    gas_price: _,
                    max_fee_per_gas: _,
                    max_priority_fee_per_gas: _,
                    gas,
                    value,
                    data,
                    nonce,
                } = request;

                let gas_limit = core::cmp::min(
                    gas.unwrap_or_else(|| U256::from(gas_limit)).low_u64(),
                    gas_limit,
                );

                let mut config = <BaseApp as module_ethereum::Config>::config().clone();
                config.estimate = true;

                if !is_eip1559 {
                    match to {
                        Some(to) => {
                            let call = Call {
                                source: from.unwrap_or_default(),
                                target: to,
                                input: data.map(|d| d.0).unwrap_or_default(),
                                value: value.unwrap_or_default(),
                                gas_limit,
                                gas_price,
                                nonce,
                            };

                            let info =
                                <BaseApp as module_ethereum::Config>::Runner::call(
                                    &ctx, call, &config,
                                )
                                .map_err(|err| {
                                    internal_err(format!(
                                        "evm runner call error: {:?}",
                                        err
                                    ))
                                })?;
                            debug!(target: "eth_rpc", "evm runner call result: {:?}", info);

                            Ok(ExecuteResult {
                                data: info.value,
                                exit_reason: info.exit_reason,
                                used_gas: info.used_gas,
                            })
                        }
                        None => {
                            let create = Create {
                                source: from.unwrap_or_default(),
                                init: data.map(|d| d.0).unwrap_or_default(),
                                value: value.unwrap_or_default(),
                                gas_limit,
                                gas_price,
                                nonce,
                            };

                            let info =
                                <BaseApp as module_ethereum::Config>::Runner::create(
                                    &ctx, create, &config,
                                )
                                .map_err(|err| {
                                    internal_err(format!(
                                        "evm runner create error: {:?}",
                                        err
                                    ))
                                })?;
                            debug!(target: "eth_rpc", "evm runner create result: {:?}", info);

                            Ok(ExecuteResult {
                                data: vec![],
                                exit_reason: info.exit_reason,
                                used_gas: info.used_gas,
                            })
                        }
                    }
                } else {
                    match to {
                        Some(to) => {
                            let call = CallEip1559 {
                                source: from.unwrap_or_default(),
                                target: to,
                                input: data.map(|d| d.0).unwrap_or_default(),
                                value: value.unwrap_or_default(),
                                gas_limit,
                                max_fee_per_gas,
                                max_priority_fee_per_gas,
                                nonce,
                            };

                            let info =
                            <BaseApp as module_ethereum::Config>::Runner::call_eip1559(
                                &ctx, call, &config,
                            )
                            .map_err(|err| {
                                internal_err(format!("evm runner call error: {:?}", err))
                            })?;
                            debug!(target: "eth_rpc", "evm runner call result: {:?}", info);

                            Ok(ExecuteResult {
                                data: info.value,
                                exit_reason: info.exit_reason,
                                used_gas: info.used_gas,
                            })
                        }
                        None => {
                            let create = CreateEip1559 {
                                source: from.unwrap_or_default(),
                                init: data.map(|d| d.0).unwrap_or_default(),
                                value: value.unwrap_or_default(),
                                gas_limit,
                                max_fee_per_gas,
                                max_priority_fee_per_gas,
                                nonce,
                            };

                            let info = <BaseApp as module_ethereum::Config>::Runner::create_eip1559(
                                &ctx, create, &config,
                            )
                            .map_err(|err| {
                                internal_err(format!(
                                    "evm runner create error: {:?}", 
                                    err
                                ))
                            })?;
                            debug!(target: "eth_rpc", "evm runner create result: {:?}", info);

                            Ok(ExecuteResult {
                                data: vec![],
                                exit_reason: info.exit_reason,
                                used_gas: info.used_gas,
                            })
                        }
                    }
                }
            };

            let result = execute_call_or_create(request.clone(), highest.low_u64())?;

            error_on_execution_failure(&result.exit_reason, &result.data)?;

            {
                let mut lowest = U256::from(21_000);
                let mut mid = std::cmp::min(result.used_gas * 3, (highest + lowest) / 2);
                let mut previous_highest = highest;

                while (highest - lowest) > U256::one() {
                    let ExecuteResult {
                        data,
                        exit_reason,
                        used_gas: _,
                    } = execute_call_or_create(request.clone(), mid.low_u64())?;
                    match exit_reason {
                        ExitReason::Succeed(_) => {
                            highest = mid;
                            if (previous_highest - highest) * 10 / previous_highest
                                < U256::one()
                            {
                                return Ok(highest);
                            }
                            previous_highest = highest;
                        }
                        ExitReason::Revert(_)
                        | ExitReason::Error(ExitError::OutOfGas) => {
                            lowest = mid;
                        }
                        other => error_on_execution_failure(&other, &data)?,
                    }
                    mid = (highest + lowest) / 2;
                }
            }

            Ok(result.used_gas)
        });
        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn transaction_by_hash(&self, hash: H256) -> BoxFuture<Result<Option<Transaction>>> {
        debug!(target: "eth_rpc", "transaction_by_hash, hash:{:?}", hash);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let mut id = None;
            let mut index = 0;
            if let Some((number, idx)) = account_base_app.read().transaction_index(hash)
            {
                id = Some(BlockId::Number(number));
                index = idx as usize
            }
            println!("{:?}, {:?}", id, index);

            let block = account_base_app.read().current_block(id.clone());
            let statuses = account_base_app
                .read()
                .current_transaction_statuses(id.clone());

            match (block, statuses) {
                (Some(block), Some(statuses)) => {
                    if id.is_none() {
                        if let Some(idx) =
                            statuses.iter().position(|t| t.transaction_hash == hash)
                        {
                            index = idx;
                        } else {
                            return Ok(None);
                        }
                    }

                    let is_eip1559 = !matches!(
                        &block.transactions[index],
                        EthereumTransaction::Legacy(_)
                    );
                    // match &block.transactions[index] {
                    //     EthereumTransaction::Legacy(_) => false,
                    //     _ => true,
                    // };

                    let base_fee = account_base_app.read().base_fee(id);

                    Ok(Some(transaction_build(
                        block.transactions[index].clone(),
                        Some(block),
                        Some(statuses[index].clone()),
                        is_eip1559,
                        base_fee,
                    )))
                }
                _ => Ok(None),
            }
        });
        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn transaction_by_block_hash_and_index(
        &self,
        hash: H256,
        index: Index,
    ) -> BoxFuture<Result<Option<Transaction>>> {
        debug!(target: "eth_rpc", "transaction_by_block_hash_and_index, hash:{:?}, index:{:?}", hash, index);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let index = index.value();
            let block = account_base_app
                .read()
                .current_block(Some(BlockId::Hash(hash)));
            let statuses = account_base_app
                .read()
                .current_transaction_statuses(Some(BlockId::Hash(hash)));

            // match &block.transactions[index] {
            //     EthereumTransaction::Legacy(_) => false,
            //     _ => true,
            // };

            let base_fee = account_base_app.read().base_fee(Some(BlockId::Hash(hash)));

            match (block, statuses) {
                (Some(block), Some(statuses)) => {
                    if index >= block.transactions.len() {
                        return Ok(None);
                    }

                    let is_eip1559 = !matches!(
                        &block.transactions[index],
                        EthereumTransaction::Legacy(_)
                    );

                    Ok(Some(transaction_build(
                        block.transactions[index].clone(),
                        Some(block),
                        Some(statuses[index].clone()),
                        is_eip1559,
                        base_fee,
                    )))
                }
                _ => Ok(None),
            }
        });
        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn transaction_by_block_number_and_index(
        &self,
        number: BlockNumber,
        index: Index,
    ) -> BoxFuture<Result<Option<Transaction>>> {
        debug!(target: "eth_rpc", "transaction_by_block_number_and_index, number:{:?}, index:{:?}", number, index);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let id = native_block_id(Some(number));
            let index = index.value();
            let block = account_base_app.read().current_block(id.clone());
            let statuses = account_base_app
                .read()
                .current_transaction_statuses(id.clone());

            match (block, statuses) {
                (Some(block), Some(statuses)) => {
                    if index >= block.transactions.len() {
                        return Ok(None);
                    }

                    let is_eip1559 = !matches!(
                        &block.transactions[index],
                        EthereumTransaction::Legacy(_)
                    );
                    // match &block.transactions[index] {
                    //     EthereumTransaction::Legacy(_) => true,
                    //     _ => false,
                    // };

                    let base_fee = account_base_app.read().base_fee(id);

                    Ok(Some(transaction_build(
                        block.transactions[index].clone(),
                        Some(block),
                        Some(statuses[index].clone()),
                        is_eip1559,
                        base_fee,
                    )))
                }
                _ => Ok(None),
            }
        });
        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn transaction_receipt(&self, hash: H256) -> BoxFuture<Result<Option<Receipt>>> {
        debug!(target: "eth_rpc", "transaction_receipt, hash:{:?}", hash);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let mut id = None;
            let mut index = 0;
            if let Some((number, idx)) = account_base_app.read().transaction_index(hash)
            {
                id = Some(BlockId::Number(number));
                index = idx as usize
            }

            let block = account_base_app.read().current_block(id.clone());
            let statuses = account_base_app
                .read()
                .current_transaction_statuses(id.clone());
            let receipts = account_base_app.read().current_receipts(id.clone());

            let base_fee = account_base_app.read().base_fee(id.clone());

            match (block, statuses, receipts, base_fee) {
                (Some(block), Some(statuses), Some(receipts), Some(base_fee)) => {
                    if id.is_none() {
                        if let Some(idx) =
                            statuses.iter().position(|t| t.transaction_hash == hash)
                        {
                            index = idx;
                        } else {
                            return Ok(None);
                        }
                    }

                    let _leng = receipts.len();
                    let block_hash = H256::from_slice(
                        Keccak256::digest(&rlp::encode(&block.header)).as_slice(),
                    );
                    let receipt = receipts[index].clone();
                    let status = statuses[index].clone();
                    let mut cumulative_receipts = receipts;
                    cumulative_receipts
                        .truncate((status.transaction_index + 1) as usize);

                    let transaction = block.transactions[index].clone();
                    let effective_gas_price = match transaction {
                        EthereumTransaction::Legacy(t) => t.gas_price,
                        EthereumTransaction::EIP1559(t) => base_fee
                            .checked_add(t.max_priority_fee_per_gas)
                            .unwrap_or_else(U256::max_value),
                        EthereumTransaction::EIP2930(t) => t.gas_price,
                    };

                    return Ok(Some(Receipt {
                        transaction_hash: Some(status.transaction_hash),
                        transaction_index: Some(status.transaction_index.into()),
                        block_hash: Some(block_hash),
                        from: Some(status.from),
                        to: status.to,
                        block_number: Some(block.header.number),
                        cumulative_gas_used: {
                            let cumulative_gas: u32 = cumulative_receipts
                                .iter()
                                .map(|r| r.used_gas.as_u32())
                                .sum();
                            U256::from(cumulative_gas)
                        },
                        gas_used: Some(receipt.used_gas),
                        contract_address: status.contract_address,
                        logs: {
                            let mut pre_receipts_log_index = None;
                            if !cumulative_receipts.is_empty() {
                                cumulative_receipts
                                    .truncate(cumulative_receipts.len() - 1);
                                pre_receipts_log_index = Some(
                                    cumulative_receipts
                                        .iter()
                                        .map(|r| r.logs.len() as u32)
                                        .sum::<u32>(),
                                );
                            }
                            receipt
                                .logs
                                .iter()
                                .enumerate()
                                .map(|(i, log)| Log {
                                    address: log.address,
                                    topics: log.topics.clone(),
                                    data: Bytes(log.data.clone()),
                                    block_hash: Some(block_hash),
                                    block_number: Some(block.header.number),
                                    transaction_hash: Some(status.transaction_hash),
                                    transaction_index: Some(
                                        status.transaction_index.into(),
                                    ),
                                    log_index: Some(U256::from(
                                        (pre_receipts_log_index.unwrap_or(0)) + i as u32,
                                    )),
                                    transaction_log_index: Some(U256::from(i)),
                                    removed: false,
                                })
                                .collect()
                        },
                        status_code: Some(U64::from(receipt.state_root.to_low_u64_be())),
                        logs_bloom: receipt.logs_bloom,
                        state_root: None,
                        effective_gas_price,
                    }));
                }
                _ => Ok(None),
            }
        });
        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn uncle_by_block_hash_and_index(
        &self,
        _: H256,
        _: Index,
    ) -> Result<Option<RichBlock>> {
        Ok(None)
    }

    fn uncle_by_block_number_and_index(
        &self,
        _: BlockNumber,
        _: Index,
    ) -> Result<Option<RichBlock>> {
        Ok(None)
    }

    fn logs(&self, filter: Filter) -> BoxFuture<Result<Vec<Log>>> {
        debug!(target: "eth_rpc", "logs, filter:{:?}", filter);

        let account_base_app = self.account_base_app.clone();

        let max_past_logs = self.max_past_logs;

        let task = spawn_blocking(move || {
            let mut ret: Vec<Log> = Vec::new();
            if let Some(hash) = filter.block_hash {
                let block = account_base_app
                    .read()
                    .current_block(Some(BlockId::Hash(hash)));
                let statuses = account_base_app
                    .read()
                    .current_transaction_statuses(Some(BlockId::Hash(hash)));

                if let (Some(block), Some(statuses)) = (block, statuses) {
                    filter_block_logs(&mut ret, &filter, block, statuses);
                }
            } else {
                let current_number = account_base_app
                    .read()
                    .current_block_number()
                    .unwrap_or_default();
                let mut to_number = filter
                    .to_block
                    .clone()
                    .and_then(|v| v.to_min_block_num())
                    .map(|s| s.into())
                    .unwrap_or(current_number);

                if to_number > current_number {
                    to_number = current_number;
                }

                let from_number = filter
                    .from_block
                    .clone()
                    .and_then(|v| v.to_min_block_num())
                    .map(|s| s.into())
                    .unwrap_or(current_number);

                filter_range_logs(
                    account_base_app.clone(),
                    &mut ret,
                    max_past_logs,
                    &filter,
                    from_number,
                    to_number,
                )?;
            }
            debug!(target: "eth_rpc", "logs, ret: {:?}", ret);
            Ok(ret)
        });
        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn work(&self) -> Result<Work> {
        Ok(Work {
            pow_hash: H256::default(),
            seed_hash: H256::default(),
            target: H256::default(),
            number: None,
        })
    }

    fn submit_work(&self, _: H64, _: H256, _: H256) -> Result<bool> {
        Ok(false)
    }

    fn submit_hashrate(&self, _: U256, _: H256) -> Result<bool> {
        Ok(false)
    }
}

pub fn sign_transaction_message(
    message: TransactionMessage,
    private_key: &H256,
) -> ruc::Result<EthereumTransaction> {
    match message {
        TransactionMessage::Legacy(m) => {
            let signing_message = libsecp256k1::Message::parse_slice(&m.hash()[..])
                .map_err(|_| eg!("invalid signing message"))?;
            let secret = &libsecp256k1::SecretKey::parse_slice(&private_key[..])
                .map_err(|_| eg!("invalid secret"))?;
            let (signature, recid) = libsecp256k1::sign(&signing_message, secret);

            let v = match m.chain_id {
                None => 27 + recid.serialize() as u64,
                Some(chain_id) => 2 * chain_id + 35 + recid.serialize() as u64,
            };
            let rs = signature.serialize();
            let r = H256::from_slice(&rs[0..32]);
            let s = H256::from_slice(&rs[32..64]);

            Ok(EthereumTransaction::Legacy(ethereum::LegacyTransaction {
                nonce: m.nonce,
                gas_price: m.gas_price,
                gas_limit: m.gas_limit,
                action: m.action,
                value: m.value,
                input: m.input,
                signature: ethereum::TransactionSignature::new(v, r, s)
                    .ok_or(eg!("signer generated invalid signature"))?,
            }))
        }
        TransactionMessage::EIP1559(m) => {
            let signing_message = libsecp256k1::Message::parse_slice(&m.hash()[..])
                .map_err(|_| eg!("invalid signing message"))?;
            let secret = &libsecp256k1::SecretKey::parse_slice(&private_key[..])
                .map_err(|_| eg!("invalid secret"))?;
            let (signature, recid) = libsecp256k1::sign(&signing_message, secret);
            let rs = signature.serialize();
            let r = H256::from_slice(&rs[0..32]);
            let s = H256::from_slice(&rs[32..64]);

            Ok(EthereumTransaction::EIP1559(ethereum::EIP1559Transaction {
                chain_id: m.chain_id,
                nonce: m.nonce,
                max_priority_fee_per_gas: m.max_priority_fee_per_gas,
                max_fee_per_gas: m.max_fee_per_gas,
                gas_limit: m.gas_limit,
                action: m.action,
                value: m.value,
                input: m.input.clone(),
                access_list: m.access_list,
                odd_y_parity: recid.serialize() != 0,
                r,
                s,
            }))
        }
    }
}

fn rich_block_build(
    block: EthereumBlock,
    statuses: Vec<Option<TransactionStatus>>,
    hash: Option<H256>,
    full_transactions: bool,
    base_fee: Option<U256>,
) -> RichBlock {
    Rich {
        inner: Block {
            hash: Some(hash.unwrap_or_else(|| {
                H256::from_slice(
                    Keccak256::digest(&rlp::encode(&block.header)).as_slice(),
                )
            })),
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
            logs_bloom: Some(block.header.logs_bloom),
            timestamp: U256::from(block.header.timestamp),
            difficulty: block.header.difficulty,
            total_difficulty: U256::zero(),
            seal_fields: vec![
                Bytes(block.header.mix_hash.as_bytes().to_vec()),
                Bytes(block.header.nonce.as_bytes().to_vec()),
            ],
            uncles: vec![],
            transactions: {
                if full_transactions {
                    BlockTransactions::Full(
                        block
                            .transactions
                            .iter()
                            .enumerate()
                            .map(|(index, transaction)| {
                                let is_eip1559 = !matches!(
                                    &block.transactions[index],
                                    EthereumTransaction::Legacy(_)
                                );
                                // match &transaction {
                                //     EthereumTransaction::Legacy(_) => true,
                                //     _ => false,
                                // };

                                transaction_build(
                                    transaction.clone(),
                                    Some(block.clone()),
                                    Some(statuses[index].clone().unwrap_or_default()),
                                    is_eip1559,
                                    base_fee,
                                )
                            })
                            .collect(),
                    )
                } else {
                    BlockTransactions::Hashes(
                        block
                            .transactions
                            .iter()
                            .map(|transaction| {
                                H256::from_slice(
                                    Keccak256::digest(&rlp::encode(
                                        &transaction.clone(),
                                    ))
                                    .as_slice(),
                                )
                            })
                            .collect(),
                    )
                }
            },
            size: Some(U256::from(rlp::encode(&block).len() as u32)),
            base_fee_per_gas: base_fee,
        },
        extra_info: BTreeMap::new(),
    }
}

fn transaction_build(
    ethereum_transaction: EthereumTransaction,
    block: Option<ethereum::Block<EthereumTransaction>>,
    status: Option<TransactionStatus>,
    is_eip1559: bool,
    base_fee: Option<U256>,
) -> Transaction {
    let mut transaction: Transaction = ethereum_transaction.clone().into();

    if let EthereumTransaction::EIP1559(_) = ethereum_transaction {
        if block.is_none() && status.is_none() {
            transaction.gas_price = transaction.max_fee_per_gas;
        } else {
            let base_fee = base_fee.unwrap_or(U256::zero());
            let max_priority_fee_per_gas =
                transaction.max_priority_fee_per_gas.unwrap_or(U256::zero());
            transaction.gas_price = Some(
                base_fee
                    .checked_add(max_priority_fee_per_gas)
                    .unwrap_or_else(U256::max_value),
            );
        }
    } else if !is_eip1559 {
        transaction.max_fee_per_gas = None;
        transaction.max_priority_fee_per_gas = None;
    }

    let pubkey = match public_key(&ethereum_transaction) {
        Ok(p) => Some(p),
        Err(_e) => None,
    };

    // Block hash.
    // transaction.block_hash = block.as_ref().map_or(None, |block| {
    //     Some(H256::from_slice(
    //         Keccak256::digest(&rlp::encode(&block.header)).as_slice(),
    //     ))
    // });
    transaction.block_hash = block.as_ref().map(|block| {
        H256::from_slice(Keccak256::digest(&rlp::encode(&block.header)).as_slice())
    });

    // Block number.
    transaction.block_number = block.as_ref().map(|block| block.header.number);
    // Transaction index.

    transaction.transaction_index = status
        .as_ref()
        .map(|status| U256::from(status.transaction_index));

    transaction.from = status.as_ref().map_or(
        {
            match pubkey {
                Some(pk) => {
                    H160::from(H256::from_slice(Keccak256::digest(pk).as_slice()))
                }
                _ => H160::default(),
            }
        },
        |status| status.from,
    );
    // To.
    transaction.to = status.as_ref().map_or(
        {
            let action = match ethereum_transaction.clone() {
                EthereumTransaction::Legacy(t) => t.action,
                EthereumTransaction::EIP1559(t) => t.action,
                EthereumTransaction::EIP2930(t) => t.action,
            };
            match action {
                ethereum::TransactionAction::Call(to) => Some(to),
                _ => None,
            }
        },
        |status| status.to,
    );
    // Creates.
    // transaction.creates = status
    //     .as_ref()
    //     .map_or(None, |status| status.contract_address);
    transaction.creates = status.as_ref().and_then(|status| status.contract_address);

    // Public key.
    transaction.public_key = pubkey.as_ref().map(H512::from);

    transaction.hash = if let Some(status) = &status {
        status.transaction_hash
    } else {
        match ethereum_transaction {
            EthereumTransaction::Legacy(t) => {
                H256::from_slice(Keccak256::digest(&rlp::encode(&t)).as_slice())
            }
            EthereumTransaction::EIP1559(t) => {
                H256::from_slice(Keccak256::digest(&rlp::encode(&t)).as_slice())
            }
            EthereumTransaction::EIP2930(t) => {
                H256::from_slice(Keccak256::digest(&rlp::encode(&t)).as_slice())
            }
        }
    };

    // // Block hash.
    // transaction.block_hash = ;
    // // Block number.
    // transaction.block_number = ;
    // // Transaction index.
    // transaction.transaction_index = ;
    // // From.
    // transaction.from;
    // // To.
    // transaction.to;
    // // Creates.
    // transaction.creates;
    // // Public key.
    // transaction.public_key;

    transaction
}

pub fn public_key(transaction: &EthereumTransaction) -> ruc::Result<[u8; 64]> {
    // let mut sig = [0u8; 65];
    // let mut msg = [0u8; 32];
    // sig[0..32].copy_from_slice(&transaction.signature.r()[..]);
    // sig[32..64].copy_from_slice(&transaction.signature.s()[..]);
    // sig[64] = transaction.signature.standard_v();
    // msg.copy_from_slice(
    //     &EthereumTransactionMessage::from(transaction.clone()).hash()[..],
    // );

    // fp_types::crypto::secp256k1_ecdsa_recover(&sig, &msg)

    let mut sig = [0u8; 65];
    let mut msg = [0u8; 32];
    match transaction {
        EthereumTransaction::Legacy(t) => {
            sig[0..32].copy_from_slice(&t.signature.r()[..]);
            sig[32..64].copy_from_slice(&t.signature.s()[..]);
            sig[64] = t.signature.standard_v();
            msg.copy_from_slice(
                &ethereum::LegacyTransactionMessage::from(t.clone()).hash()[..],
            );
        }
        EthereumTransaction::EIP1559(t) => {
            sig[0..32].copy_from_slice(&t.r[..]);
            sig[32..64].copy_from_slice(&t.s[..]);
            sig[64] = t.odd_y_parity as u8;
            msg.copy_from_slice(
                &ethereum::EIP1559TransactionMessage::from(t.clone()).hash()[..],
            );
        }
        _ => {
            return Err(eg!("Transaction Type Error"));
        }
    }
    fp_types::crypto::secp256k1_ecdsa_recover(&sig, &msg)
}

fn filter_range_logs(
    app: Arc<RwLock<BaseApp>>,
    ret: &mut Vec<Log>,
    max_past_logs: u32,
    filter: &Filter,
    from: U256,
    to: U256,
) -> Result<()> {
    let mut current = to;

    let topics_input = if filter.topics.is_some() {
        let filtered_params = FilteredParams::new(Some(filter.clone()));
        Some(filtered_params.flat_topics)
    } else {
        None
    };
    let address_bloom_filter = FilteredParams::addresses_bloom_filter(&filter.address);
    let topics_bloom_filter = FilteredParams::topics_bloom_filter(&topics_input);

    while current >= from {
        let id = BlockId::Number(current);
        let block = app.read().current_block(Some(id.clone()));

        if let Some(block) = block {
            if FilteredParams::address_in_bloom(
                block.header.logs_bloom,
                &address_bloom_filter,
            ) && FilteredParams::topics_in_bloom(
                block.header.logs_bloom,
                &topics_bloom_filter,
            ) {
                let mut logs: Vec<Log> = Vec::new();
                let statuses = app.read().current_transaction_statuses(Some(id));
                if let Some(statuses) = statuses {
                    filter_block_logs(&mut logs, filter, block, statuses);
                }

                // insert logs at the beginning of ret
                if !logs.is_empty() {
                    logs.append(ret);
                    ret.append(&mut logs);
                }
            }
        }
        // Check for restrictions
        if ret.len() as u32 > max_past_logs {
            warn!(target: "eth_rpc", "max_past_logs reached at block {:?}", current);
            break;
        }
        if current == U256::zero() {
            break;
        } else {
            current = current.saturating_sub(U256::one());
        }
    }
    Ok(())
}

pub fn filter_block_logs<'a>(
    ret: &'a mut Vec<Log>,
    filter: &'a Filter,
    block: EthereumBlock,
    transaction_statuses: Vec<TransactionStatus>,
) -> &'a Vec<Log> {
    let params = FilteredParams::new(Some(filter.clone()));
    let mut block_log_index: u32 = 0;
    let block_hash =
        H256::from_slice(Keccak256::digest(&rlp::encode(&block.header)).as_slice());
    for status in transaction_statuses.iter() {
        let logs = status.logs.clone();
        let transaction_hash = status.transaction_hash;
        for (transaction_log_index, ethereum_log) in logs.into_iter().enumerate() {
            let mut log = Log {
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
            let mut add: bool = true;
            match (filter.address.clone(), filter.topics.clone()) {
                (Some(_), Some(_)) => {
                    if !params.filter_address(&log) || !params.filter_topics(&log) {
                        add = false;
                    }
                }
                (Some(_), None) => {
                    if !params.filter_address(&log) {
                        add = false;
                    }
                }
                (None, Some(_)) => {
                    if !params.filter_topics(&log) {
                        add = false;
                    }
                }
                (None, None) => {}
            }
            if add {
                log.block_hash = Some(block_hash);
                log.block_number = Some(block.header.number);
                log.transaction_hash = Some(transaction_hash);
                log.transaction_index = Some(U256::from(status.transaction_index));
                log.log_index = Some(U256::from(block_log_index));
                log.transaction_log_index = Some(U256::from(transaction_log_index));
                ret.push(log);
            }
            block_log_index += 1;
        }
    }
    ret
}

fn native_block_id(number: Option<BlockNumber>) -> Option<BlockId> {
    match number.unwrap_or(BlockNumber::Latest) {
        BlockNumber::Hash { hash, .. } => Some(BlockId::Hash(hash)),
        BlockNumber::Num(number) => Some(BlockId::Number(number.into())),
        BlockNumber::Latest => None,
        BlockNumber::Earliest => Some(BlockId::Number(U256::zero())),
        BlockNumber::Pending => None,
    }
}

struct FeeDetails {
    gas_price: Option<U256>,
    max_fee_per_gas: Option<U256>,
    max_priority_fee_per_gas: Option<U256>,
}

fn fee_details(
    request_gas_price: Option<U256>,
    request_max_fee: Option<U256>,
    request_priority: Option<U256>,
) -> Result<FeeDetails> {
    match (request_gas_price, request_max_fee, request_priority) {
        (gas_price, None, None) => {
            // Legacy request, all default to gas price.
            Ok(FeeDetails {
                gas_price,
                max_fee_per_gas: gas_price,
                max_priority_fee_per_gas: gas_price,
            })
        }
        (_, max_fee, max_priority) => {
            // eip-1559
            // Ensure `max_priority_fee_per_gas` is less or equal to `max_fee_per_gas`.
            if let Some(max_priority) = max_priority {
                let max_fee = max_fee.unwrap_or_default();
                if max_priority > max_fee {
                    return Err(internal_err(
						"Invalid input: `max_priority_fee_per_gas` greater than `max_fee_per_gas`"
					));
                }
            }
            Ok(FeeDetails {
                gas_price: max_fee,
                max_fee_per_gas: max_fee,
                max_priority_fee_per_gas: max_priority,
            })
        }
    }
}
fn dummy_block(height: u64, full: bool) -> Rich<Block> {
    let hash = if height == *EVM_FIRST_BLOCK_HEIGHT - 1 {
        H256([0; 32])
    } else {
        H256::from_slice(&sha3::Keccak256::digest(height.to_le_bytes()))
    };

    let parent_hash =
        H256::from_slice(&sha3::Keccak256::digest((height - 1).to_le_bytes()));

    let transactions = if full {
        BlockTransactions::Full(vec![])
    } else {
        BlockTransactions::Hashes(vec![])
    };

    let gas_price =
        <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price(height);

    let inner = Block {
        hash: Some(hash),
        parent_hash,
        uncles_hash: H256(hex!(
            "1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"
        )),
        author: H160(hex!("1d8f397fa03b357dc94303086a91ce5c8c7af1e6")),
        miner: H160(hex!("1d8f397fa03b357dc94303086a91ce5c8c7af1e6")),
        state_root: H256(hex!(
            "0000000000000000000000000000000000000000000000000000000000000000"
        )),
        transactions_root: H256(hex!(
            "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"
        )),
        receipts_root: H256(hex!(
            "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"
        )),
        number: Some(U256::from(height)),
        gas_used: U256::zero(),
        gas_limit: U256::from(0xffffffff_u32),
        extra_data: Bytes::new(vec![]),
        logs_bloom: Some(Bloom::default()),
        timestamp: U256::from(0x61b839d9_u32),
        difficulty: U256::zero(),
        total_difficulty: U256::zero(),
        seal_fields: vec![
            Bytes::new(
                hex!("0000000000000000000000000000000000000000000000000000000000000000")
                    .to_vec(),
            ),
            Bytes::new(hex!("0000000000000000").to_vec()),
        ],
        uncles: vec![],
        transactions,
        size: Some(U256::from(0x1_u32)),
        base_fee_per_gas: Some(gas_price),
    };

    Rich {
        inner,
        extra_info: BTreeMap::new(),
    }
}
