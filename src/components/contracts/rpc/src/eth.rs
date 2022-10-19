use crate::utils::{
    build_method_not_found, convert_error_to_rpc_error, convert_join_error_to_rpc_error,
};
use crate::{error_on_execution_failure, internal_err};
use baseapp::{extensions::SignedExtra, BaseApp};
use ethereum::{
    BlockV0 as EthereumBlock, LegacyTransactionMessage as EthereumTransactionMessage,
    TransactionV0 as EthereumTransaction,
};
use ethereum_types::{BigEndianHash, Bloom, H160, H256, H512, H64, U256, U64};
use evm::{ExitError, ExitReason};
use fp_evm::{BlockId, Runner, TransactionStatus};
use fp_rpc_core::types::{
    Block, BlockNumber, BlockTransactions, Bytes, CallRequest, Filter, FilteredParams,
    Index, Log, Receipt, Rich, RichBlock, SyncStatus, Transaction, TransactionRequest,
    Work,
};
use fp_rpc_core::EthApi;
use fp_traits::{
    base::BaseProvider,
    evm::{AddressMapping, EthereumAddressMapping, FeeCalculator},
};
use fp_types::{
    actions,
    actions::evm::{Call, Create},
    assemble::UncheckedTransaction,
};
use fp_utils::ecdsa::SecpPair;
use fp_utils::tx::EvmRawTxWrapper;
use hex_literal::hex;
use jsonrpc_core::{futures::future, BoxFuture, Result};
use lazy_static::lazy_static;
use log::{debug, warn};
use parking_lot::RwLock;
use sha3::{Digest, Keccak256};
use std::collections::BTreeMap;
use std::convert::Into;
use std::ops::Range;
use std::sync::Arc;
use tendermint::abci::Code;
use tendermint_rpc::{Client, HttpClient};
use tokio::runtime::Runtime;
use tokio::task::spawn_blocking;

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

    /*     fn _balance(&self, address: H160, number: Option<BlockNumber>) -> Result<U256> { */
    /* debug!(target: "eth_rpc", "balance, address:{:?}, number:{:?}", address, number); */
    /*  */
    /* let height = self.block_number_to_height(number)?; */
    /* let account_id = EthereumAddressMapping::convert_to_account_id(address); */
    /* if let Ok(sa) = self.account_base_app.read().account_of(&account_id, height) { */
    /*     Ok(sa.balance) */
    /* } else { */
    /*     Ok(U256::zero()) */
    /* } */
    /*     } */

    fn _accounts(&self) -> Result<Vec<H160>> {
        let mut accounts = Vec::new();
        for signer in self.signers.iter() {
            accounts.push(signer.address());
        }
        Ok(accounts)
    }

    fn _balance(
        account_base_app: Arc<RwLock<BaseApp>>,
        address: H160,
        number: Option<BlockNumber>,
    ) -> Result<U256> {
        let height = Self::block_number_to_height(account_base_app.clone(), number)?;
        let account_id = EthereumAddressMapping::convert_to_account_id(address);
        if let Ok(sa) = account_base_app.read().account_of(&account_id, height) {
            Ok(sa.balance)
        } else {
            Ok(U256::zero())
        }
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

    fn chain_id(&self) -> BoxFuture<Result<Option<U64>>> {
        Box::pin(async move {
            Ok(Some(<BaseApp as module_evm::Config>::ChainId::get().into()))
        })
    }

    fn accounts(&self) -> Result<Vec<H160>> {
        self._accounts()
    }

    fn balance(
        &self,
        address: H160,
        number: Option<BlockNumber>,
    ) -> BoxFuture<Result<U256>> {
        debug!(target: "eth_rpc", "balance, address:{:?}, number:{:?}", address, number);
        let account_base_app = self.account_base_app.clone();

        let task =
            spawn_blocking(move || Self::_balance(account_base_app, address, number));

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
    }

    fn send_transaction(&self, _request: TransactionRequest) -> Result<H256> {
        /*         debug!(target: "eth_rpc", "send_transaction, request:{:?}", request); */
        /*  */
        /* let from = match request.from { */
        /*     Some(from) => from, */
        /*     None => { */
        /*         let accounts = match self._accounts() { */
        /*             Ok(accounts) => accounts, */
        /*             Err(e) => return Box::pin(future::err(e)), */
        /*         }; */
        /*  */
        /*         match accounts.get(0) { */
        /*             Some(account) => *account, */
        /*             None => { */
        /*                 return Box::pin(future::err(internal_err( */
        /*                     "no signer available", */
        /*                 ))); */
        /*             } */
        /*         } */
        /*     } */
        /* }; */
        /*  */
        /* let nonce = match request.nonce { */
        /*     Some(nonce) => nonce, */
        /*     None => match self.transaction_count(from, None) { */
        /*         Ok(nonce) => nonce, */
        /*         Err(e) => return Box::pin(future::err(e)), */
        /*     }, */
        /* }; */
        /*  */
        /* let chain_id = match self.chain_id() { */
        /*     Ok(chain_id) => chain_id, */
        /*     Err(e) => return Box::pin(future::err(e)), */
        /* }; */
        /*  */
        /* let message = EthereumTransactionMessage { */
        /*     nonce, */
        /*     gas_price: request.gas_price.unwrap_or_else( */
        /*         <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price, */
        /*     ), */
        /*     gas_limit: request.gas.unwrap_or_else(U256::max_value), */
        /*     value: request.value.unwrap_or_else(U256::zero), */
        /*     input: request.data.map(|s| s.into_vec()).unwrap_or_default(), */
        /*     action: match request.to { */
        /*         Some(to) => ethereum::TransactionAction::Call(to), */
        /*         None => ethereum::TransactionAction::Create, */
        /*     }, */
        /*     chain_id: chain_id.map(|s| s.as_u64()), */
        /* }; */
        /*  */
        /* let mut transaction = None; */
        /* for signer in &self.signers { */
        /*     if signer.address() == from { */
        /*         match sign_transaction_message(message, &H256::from(signer.seed())) */
        /*             .map_err(internal_err) */
        /*         { */
        /*             Ok(tx) => transaction = Some(tx), */
        /*             Err(e) => return Box::pin(future::err(e)), */
        /*         } */
        /*         break; */
        /*     } */
        /* } */
        /*  */
        /* let transaction = match transaction { */
        /*     Some(transaction) => transaction, */
        /*     None => { */
        /*         return Box::pin(future::err(internal_err("no signer available"))); */
        /*     } */
        /* }; */
        /* let transaction_hash = */
        /*     H256::from_slice(Keccak256::digest(&rlp::encode(&transaction)).as_slice()); */
        /* let function = */
        /*     actions::Action::Ethereum(actions::ethereum::Action::Transact(transaction)); */
        /* let txn = serde_json::to_vec( */
        /*     &UncheckedTransaction::<SignedExtra>::new_unsigned(function), */
        /* ) */
        /* .map_err(internal_err); */
        /* if let Err(e) = txn { */
        /*     return Box::pin(future::err(e)); */
        /* } */
        /*  */
        /* // check_tx and broadcast */
        /* let client = self.tm_client.clone(); */
        /* let txn_with_tag = EvmRawTxWrapper::wrap(&txn.unwrap()); */
        /* let (tx, rx) = mpsc::channel(); */
        /* RT.spawn(async move { */
        /*     let resp = client.broadcast_tx_sync(txn_with_tag.into()).await; */
        /*     tx.send(resp).unwrap(); */
        /* }); */
        /*  */
        /* // fetch response */
        /* if let Ok(resp) = rx.recv().unwrap() { */
        /*     if resp.code != Code::Ok { */
        /*         return Box::pin(future::err(internal_err(resp.log))); */
        /*     } */
        /* } else { */
        /*     return Box::pin(future::err(internal_err(String::from( */
        /*         "send_transaction: broadcast_tx_sync failed", */
        /*     )))); */
        /* } */
        /*  */
        /* Box::pin(future::ok(transaction_hash)) */
        Err(build_method_not_found())
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
                gas,
                value,
                data,
                nonce,
            } = request;

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
                if let Some(time) = ctx.header.time.as_mut() {
                    time.seconds = block.header.timestamp as i64;
                }
                ctx.header.height = block.header.number.as_u64() as i64;
                ctx.header.proposer_address =
                    Vec::from(block.header.beneficiary.as_bytes())
            }

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

    fn gas_price(&self) -> BoxFuture<Result<U256>> {
        Box::pin(async move {
            Ok(<BaseApp as module_evm::Config>::FeeCalculator::min_gas_price())
        })
    }

    fn block_number(&self) -> BoxFuture<Result<U256>> {
        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
            let height = account_base_app
                .read()
                .chain_state
                .read()
                .height()
                .map_err(internal_err)?;
            Ok(U256::from(height))
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
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

            match (block, statuses) {
                (Some(block), Some(statuses)) => Ok(Some(rich_block_build(
                    block,
                    statuses.into_iter().map(Some).collect(),
                    Some(hash),
                    full,
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
                if 0 < h && h < *EVM_FIRST_BLOCK_HEIGHT {
                    return Ok(Some(dummy_block(h, full)));
                }
            }

            let id = native_block_id(Some(number));
            let block = account_base_app.read().current_block(id.clone());
            let statuses = account_base_app.read().current_transaction_statuses(id);

            match (block, statuses) {
                (Some(block), Some(statuses)) => {
                    let hash = block.header.hash();

                    Ok(Some(rich_block_build(
                        block,
                        statuses.into_iter().map(Some).collect(),
                        Some(hash),
                        full,
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

    fn transaction_count(
        &self,
        address: H160,
        number: Option<BlockNumber>,
    ) -> BoxFuture<Result<U256>> {
        debug!(target: "eth_rpc", "transaction_count, address:{:?}, number:{:?}", address, number);

        let account_base_app = self.account_base_app.clone();

        let task = spawn_blocking(move || {
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
        });

        Box::pin(async move {
            match task.await {
                Ok(r) => r,
                Err(e) => Err(convert_join_error_to_rpc_error(e)),
            }
        })
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
        let transaction = match rlp::decode::<EthereumTransaction>(&bytes.0[..]) {
            Ok(transaction) => transaction,
            Err(_) => {
                return Box::pin(future::err(internal_err("decode transaction failed")));
            }
        };
        debug!(target: "eth_rpc", "send_raw_transaction :{:?}", transaction);

        let transaction_hash =
            H256::from_slice(Keccak256::digest(&rlp::encode(&transaction)).as_slice());
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
                    let balance = Self::_balance(account_base_app.clone(), from, None)
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
                        log::warn!(
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
                    gas_price,
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

                        let info = <BaseApp as module_ethereum::Config>::Runner::call(
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
                        let create = Create {
                            source: from.unwrap_or_default(),
                            init: data.map(|d| d.0).unwrap_or_default(),
                            value: value.unwrap_or_default(),
                            gas_limit,
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

                        Ok(ExecuteResult {
                            data: vec![],
                            exit_reason: info.exit_reason,
                            used_gas: info.used_gas,
                        })
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

                    Ok(Some(transaction_build(
                        block.transactions[index].clone(),
                        Some(block),
                        Some(statuses[index].clone()),
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

            match (block, statuses) {
                (Some(block), Some(statuses)) => {
                    if index >= block.transactions.len() {
                        return Ok(None);
                    }

                    Ok(Some(transaction_build(
                        block.transactions[index].clone(),
                        Some(block),
                        Some(statuses[index].clone()),
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
            let statuses = account_base_app.read().current_transaction_statuses(id);

            match (block, statuses) {
                (Some(block), Some(statuses)) => {
                    if index >= block.transactions.len() {
                        return Ok(None);
                    }

                    Ok(Some(transaction_build(
                        block.transactions[index].clone(),
                        Some(block),
                        Some(statuses[index].clone()),
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

            match (block, statuses, receipts) {
                (Some(block), Some(statuses), Some(receipts)) => {
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

/* pub fn sign_transaction_message( */
/*     message: LegacyTransactionMessage, */
/*     private_key: &H256, */
/* ) -> ruc::Result<EthereumTransaction> { */
/*     let signing_message = libsecp256k1::Message::parse_slice(&message.hash()[..]) */
/*         .map_err(|_| eg!("invalid signing message"))?; */
/*     let secret = &libsecp256k1::SecretKey::parse_slice(&private_key[..]) */
/*         .map_err(|_| eg!("invalid secret"))?; */
/*     let (signature, recid) = libsecp256k1::sign(&signing_message, secret); */
/*  */
/*     let v = match message.chain_id { */
/*         None => 27 + recid.serialize() as u64, */
/*         Some(chain_id) => 2 * chain_id + 35 + recid.serialize() as u64, */
/*     }; */
/*     let rs = signature.serialize(); */
/*     let r = H256::from_slice(&rs[0..32]); */
/*     let s = H256::from_slice(&rs[32..64]); */
/*  */
/*     Ok(EthereumTransaction { */
/*         nonce: message.nonce, */
/*         gas_price: message.gas_price, */
/*         gas_limit: message.gas_limit, */
/*         action: message.action, */
/*         value: message.value, */
/*         input: message.input, */
/*         signature: ethereum::TransactionSignature::new(v, r, s) */
/*             .ok_or(eg!("signer generated invalid signature"))?, */
/*     }) */
/* } */
/*  */
fn rich_block_build(
    block: EthereumBlock,
    statuses: Vec<Option<TransactionStatus>>,
    hash: Option<H256>,
    full_transactions: bool,
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
                                transaction_build(
                                    transaction.clone(),
                                    Some(block.clone()),
                                    Some(statuses[index].clone().unwrap_or_default()),
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
        },
        extra_info: BTreeMap::new(),
    }
}

fn transaction_build(
    transaction: EthereumTransaction,
    block: Option<EthereumBlock>,
    status: Option<TransactionStatus>,
) -> Transaction {
    let pubkey = match public_key(&transaction) {
        Ok(p) => Some(p),
        Err(_e) => None,
    };

    Transaction {
        hash: H256::from_slice(Keccak256::digest(&rlp::encode(&transaction)).as_slice()),
        nonce: transaction.nonce,
        block_hash: block.as_ref().map(|block| {
            H256::from_slice(Keccak256::digest(&rlp::encode(&block.header)).as_slice())
        }),
        block_number: block.as_ref().map(|block| block.header.number),
        transaction_index: status
            .as_ref()
            .map(|status| U256::from(status.transaction_index)),
        from: status.as_ref().map_or(
            {
                match pubkey {
                    Some(pk) => {
                        H160::from(H256::from_slice(Keccak256::digest(&pk).as_slice()))
                    }
                    _ => H160::default(),
                }
            },
            |status| status.from,
        ),
        to: status.as_ref().map_or(
            {
                match transaction.action {
                    ethereum::TransactionAction::Call(to) => Some(to),
                    _ => None,
                }
            },
            |status| status.to,
        ),
        value: transaction.value,
        gas_price: transaction.gas_price,
        gas: transaction.gas_limit,
        input: Bytes(transaction.clone().input),
        creates: status.as_ref().and_then(|status| status.contract_address),
        raw: Bytes(rlp::encode(&transaction).to_vec()),
        public_key: pubkey.as_ref().map(H512::from),
        chain_id: transaction.signature.chain_id().map(U64::from),
        standard_v: U256::from(transaction.signature.standard_v()),
        v: U256::from(transaction.signature.v()),
        r: U256::from(transaction.signature.r().as_bytes()),
        s: U256::from(transaction.signature.s().as_bytes()),
    }
}

pub fn public_key(transaction: &EthereumTransaction) -> ruc::Result<[u8; 64]> {
    let mut sig = [0u8; 65];
    let mut msg = [0u8; 32];
    sig[0..32].copy_from_slice(&transaction.signature.r()[..]);
    sig[32..64].copy_from_slice(&transaction.signature.s()[..]);
    sig[64] = transaction.signature.standard_v();
    msg.copy_from_slice(
        &EthereumTransactionMessage::from(transaction.clone()).hash()[..],
    );

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

fn dummy_block(height: u64, full: bool) -> Rich<Block> {
    let hash = if height == *EVM_FIRST_BLOCK_HEIGHT - 1 {
        H256([0; 32])
    } else {
        H256::from_slice(&sha3::Keccak256::digest(&height.to_le_bytes()))
    };

    let parent_hash =
        H256::from_slice(&sha3::Keccak256::digest(&(height - 1).to_le_bytes()));

    let transactions = if full {
        BlockTransactions::Full(vec![])
    } else {
        BlockTransactions::Hashes(vec![])
    };

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
    };

    Rich {
        inner,
        extra_info: BTreeMap::new(),
    }
}
