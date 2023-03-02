use crate::extensions::SignedExtra;
use abci::*;
use config::abci::global_cfg::CFG;
use fp_core::context::RunTxMode;
use fp_evm::BlockId;
use fp_types::{
    actions::xhub::NonConfidentialOutput, assemble::convert_unchecked_transaction,
};
use fp_utils::tx::EvmRawTxWrapper;
use module_evm::utils::{deposit_asset_event_topic_str, parse_deposit_asset_event};
use primitive_types::U256;
use ruc::*;
use tracing::{debug, error, info};

impl crate::BaseApp {
    /// info implements the ABCI interface.
    /// - Returns chain info (las height and hash where the node left off)
    /// - Tendermint uses info to decide from which height/hash to continue
    pub fn info(&mut self, _req: &RequestInfo) -> ResponseInfo {
        let mut info: ResponseInfo = Default::default();
        info.data = self.name.clone();
        info.version = self.version.clone();
        info.app_version = self.app_version;

        let height = self.chain_state.read().height().unwrap();
        let hash = self.chain_state.read().root_hash();
        info.last_block_height = height as i64;
        info.last_block_app_hash = hash;

        info
    }

    /// query implements the ABCI interface.
    pub fn query(&mut self, req: &RequestQuery) -> ResponseQuery {
        let err_resp = |err: String| -> ResponseQuery {
            let mut resp: ResponseQuery = Default::default();
            resp.code = 1;
            resp.log = err;
            resp
        };

        if req.height < 0 {
            return err_resp(
                "cannot query with height < 0; please provide a valid height"
                    .to_string(),
            );
        }

        // example: "module/evm/code"
        let mut path: Vec<_> = req.path.split('/').collect();
        if path.is_empty() {
            return err_resp("Empty query path!".to_string());
        }

        let ctx = self.create_query_context(Some(req.height as u64), req.prove);
        if let Err(e) = ctx {
            return err_resp(format!("Cannot create query context with err: {e}!"));
        }

        match path.remove(0) {
            // "store" => self.store.query(path, req),
            "module" => self.modules.query(ctx.unwrap(), path, req),
            _ => err_resp("Invalid query path!".to_string()),
        }
    }

    /// check_tx implements the ABCI interface and executes a tx in Check/ReCheck mode.
    pub fn check_tx(&self, req: &RequestCheckTx) -> ResponseCheckTx {
        let mut resp = ResponseCheckTx::new();

        let raw_tx = if let Ok(tx) = EvmRawTxWrapper::unwrap(req.get_tx()) {
            tx
        } else {
            info!(target: "baseapp", "Transaction evm tag check failed");
            resp.code = 1;
            resp.log = String::from("Transaction evm tag check failed");
            return resp;
        };

        if let Ok(tx) = convert_unchecked_transaction::<SignedExtra>(raw_tx) {
            #[cfg(feature = "enterprise-web3")]
            let tmp_tx = tx.clone();
            let check_fn = |mode: RunTxMode| {
                let ctx = {
                    let mut ctx = self.check_state.clone();
                    ctx.run_mode = mode;
                    ctx
                };
                let result = self.modules.process_tx::<SignedExtra>(ctx, tx);
                match result {
                    Ok(ar) => {
                        #[cfg(feature = "enterprise-web3")]
                        {
                            use enterprise_web3::{
                                Setter, PENDING_CODE_MAP, PENDING_STATE_UPDATE_LIST,
                                REDIS_CLIENT,
                            };
                            use std::{collections::HashMap, mem::replace};
                            let code_map =
                                if let Ok(mut code_map) = PENDING_CODE_MAP.lock() {
                                    replace(&mut *code_map, HashMap::new())
                                } else {
                                    error!("{}", "");
                                    Default::default()
                                };
                            let state_list = if let Ok(mut state_list) =
                                PENDING_STATE_UPDATE_LIST.lock()
                            {
                                replace(&mut *state_list, vec![])
                            } else {
                                error!("{}", "");
                                Default::default()
                            };
                            if 0 == ar.code {
                                if let fp_types::actions::Action::Ethereum(
                                    fp_types::actions::ethereum::Action::Transact(tx),
                                ) = tmp_tx.function
                                {
                                    let redis_pool =
                                        REDIS_CLIENT.lock().expect("REDIS_CLIENT error");
                                    let mut conn =
                                        redis_pool.get().expect("get redis connect");
                                    let mut setter =
                                        Setter::new(&mut *conn, "evm".to_string());

                                    setter
                                        .set_pending_tx(tx)
                                        .map_err(|e| error!("{e:?}"))
                                        .unwrap_or(());
                                    for (addr, code) in code_map.iter() {
                                        setter
                                            .set_pending_code(*addr, code.clone())
                                            .map_err(|e| error!("{e:?}"))
                                            .unwrap_or(());
                                    }
                                    for state in state_list.iter() {
                                        setter
                                            .set_pending_state(
                                                state.address.clone(),
                                                state.index.clone(),
                                                state.value.clone(),
                                            )
                                            .map_err(|e| error!("{e:?}"))
                                            .unwrap_or(());
                                    }
                                }
                            }
                        }
                        resp.code = ar.code;
                        if ar.code != 0 {
                            info!(target: "baseapp", "Transaction check error, action result {ar:?}");
                            resp.log = ar.log;
                        }
                    }
                    Err(e) => {
                        info!(target: "baseapp", "Transaction check error: {e}");
                        resp.code = 1;
                        resp.log = format!("Transaction check error: {e}");
                    }
                }
            };
            match req.get_field_type() {
                CheckTxType::New => check_fn(RunTxMode::Check),
                CheckTxType::Recheck => check_fn(RunTxMode::ReCheck),
            }
        } else {
            info!(target: "baseapp", "Could not unpack transaction");
        }
        resp
    }

    /// init_chain implements the ABCI interface.
    pub fn init_chain(&mut self, req: &RequestInitChain) -> ResponseInitChain {
        let mut init_header: Header = Default::default();
        init_header.chain_id = req.chain_id.clone();
        init_header.time = req.time.clone();

        // initialize the deliver state and check state with a correct header
        Self::update_state(&mut self.deliver_state, init_header.clone(), vec![]);
        Self::update_state(&mut self.check_state, init_header, vec![]);

        ResponseInitChain::default()
    }

    /// begin_block implements the ABCI application interface.
    pub fn begin_block(&mut self, req: &RequestBeginBlock) -> ResponseBeginBlock {
        pnk!(self.validate_height(req.header.clone().unwrap_or_default().height));

        // Initialize the DeliverTx state. If this is the first block, it should
        // already be initialized in InitChain. Otherwise app.deliverState will be
        // nil, since it is reset on Commit.
        Self::update_state(
            &mut self.deliver_state,
            req.header.clone().unwrap_or_default(),
            req.hash.clone(),
        );

        self.update_deliver_state_cache();

        self.modules.begin_block(&mut self.deliver_state, req);

        ResponseBeginBlock::default()
    }

    pub fn deliver_tx(
        &mut self,
        req: &RequestDeliverTx,
    ) -> (ResponseDeliverTx, Vec<NonConfidentialOutput>) {
        let mut resp = ResponseDeliverTx::new();
        let mut non_confidential_outputs = Vec::new();

        let raw_tx = if let Ok(tx) = EvmRawTxWrapper::unwrap(req.get_tx()) {
            tx
        } else {
            info!(target: "baseapp", "Transaction deliver tx unwrap evm tag failed");
            resp.code = 1;
            resp.log = String::from("Transaction deliver tx unwrap evm tag failed");
            return (resp, non_confidential_outputs);
        };

        if let Ok(tx) = convert_unchecked_transaction::<SignedExtra>(raw_tx) {
            let ctx = self.retrieve_context(RunTxMode::Deliver).clone();
            #[cfg(feature = "enterprise-web3")]
            let tmp_tx = tx.clone();
            let ret = self.modules.process_tx::<SignedExtra>(ctx, tx);
            match ret {
                Ok(ar) => {
                    #[cfg(feature = "enterprise-web3")]
                    {
                        use enterprise_web3::{
                            Setter, REDIS_CLIENT, REMOVE_PENDING_CODE_MAP,
                            REMOVE_PENDING_STATE_UPDATE_LIST,
                        };
                        use std::{mem::replace, ops::DerefMut};
                        let code_map =
                            if let Ok(mut code_map) = REMOVE_PENDING_CODE_MAP.lock() {
                                let m = code_map.deref_mut();
                                let map = replace(m, vec![]);
                                map.clone()
                            } else {
                                error!("{}", "");
                                Default::default()
                            };
                        let state_list = if let Ok(mut state_list) =
                            REMOVE_PENDING_STATE_UPDATE_LIST.lock()
                        {
                            let v = state_list.deref_mut();
                            let v2 = replace(v, vec![]);
                            v2.clone()
                        } else {
                            error!("{}", "");
                            Default::default()
                        };
                        if 0 == ar.code {
                            if let fp_types::actions::Action::Ethereum(
                                fp_types::actions::ethereum::Action::Transact(tx),
                            ) = tmp_tx.function
                            {
                                let redis_pool =
                                    REDIS_CLIENT.lock().expect("REDIS_CLIENT error");
                                let mut conn =
                                    redis_pool.get().expect("get redis connect");
                                let mut setter =
                                    Setter::new(&mut *conn, "evm".to_string());

                                setter
                                    .remove_pending_tx(tx)
                                    .map_err(|e| error!("{:?}", e))
                                    .unwrap_or(());

                                for addr in code_map.iter() {
                                    setter
                                        .remove_pending_code(*addr)
                                        .map_err(|e| error!("{:?}", e))
                                        .unwrap_or(());
                                }

                                for (address, index) in state_list.iter() {
                                    setter
                                        .remove_pending_state(
                                            address.clone(),
                                            index.clone(),
                                        )
                                        .map_err(|e| error!("{:?}", e))
                                        .unwrap_or(());
                                }
                            }
                        }
                    }

                    if ar.code != 0 {
                        info!(target: "baseapp", "deliver tx with result: {:?}", ar);
                    } else {
                        debug!(target: "baseapp", "deliver tx succeed with result: {:?}", ar);
                    }
                    resp.code = ar.code;
                    resp.data = ar.data;
                    resp.log = ar.log;
                    resp.gas_wanted = ar.gas_wanted as i64;
                    resp.gas_used = ar.gas_used as i64;
                    resp.events = protobuf::RepeatedField::from_vec(ar.events);
                    let td_height = self.deliver_state.block_header().height;
                    if td_height > CFG.checkpoint.prismxx_inital_height && 0 == resp.code
                    {
                        let deposit_asset_topic = deposit_asset_event_topic_str();

                        for evt in resp.events.iter() {
                            if evt.field_type == *"ethereum_ContractLog" {
                                let mut bridge_contract_found = false;
                                let mut deposit_asset_foud = false;

                                for pair in evt.attributes.iter() {
                                    let key = String::from_utf8(pair.key.clone())
                                        .unwrap_or_default();
                                    if key == *"address" {
                                        let addr = String::from_utf8(pair.value.clone())
                                            .unwrap_or_default();
                                        if addr
                                            == CFG
                                                .checkpoint
                                                .prism_bridge_address
                                                .to_lowercase()
                                        {
                                            bridge_contract_found = true
                                        }
                                    }
                                    if key == *"topics" {
                                        let topic =
                                            String::from_utf8(pair.value.clone())
                                                .unwrap_or_default();
                                        if topic == deposit_asset_topic {
                                            deposit_asset_foud = true
                                        }
                                    }
                                    if key == *"data"
                                        && bridge_contract_found
                                        && deposit_asset_foud
                                    {
                                        let data = String::from_utf8(pair.value.clone())
                                            .unwrap_or_default();

                                        let data_vec =
                                            serde_json::from_str(&data).unwrap();

                                        let deposit_asset =
                                            parse_deposit_asset_event(data_vec);

                                        match deposit_asset {
                                            Ok(deposit) => {
                                                non_confidential_outputs.push(deposit)
                                            }
                                            Err(e) => {
                                                resp.code = 1;
                                                resp.log = e.to_string();
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    (resp, non_confidential_outputs)
                }
                Err(e) => {
                    error!(target: "baseapp", "Ethereum transaction deliver error: {e}");
                    resp.code = 1;
                    resp.log = format!("Ethereum transaction deliver error: {e}");
                    (resp, non_confidential_outputs)
                }
            }
        } else {
            resp.code = 1;
            resp.log = String::from("Failed to convert transaction when deliver tx!");
            (resp, non_confidential_outputs)
        }
    }

    #[cfg(any(feature = "abci_mock", test))]
    pub fn end_block(&mut self, _req: &RequestEndBlock) -> ResponseEndBlock {
        Default::default()
    }

    #[cfg(all(not(feature = "abci_mock"), not(test)))]
    pub fn end_block(&mut self, req: &RequestEndBlock) -> ResponseEndBlock {
        self.modules.end_block(&mut self.deliver_state, req)
    }

    pub fn commit(&mut self, _req: &RequestCommit) -> ResponseCommit {
        // Reset the Check state to the latest committed.
        // Cache data are dropped and cleared in check_state
        self.check_state = self.deliver_state.copy_with_new_state();

        let block_height = self.deliver_state.block_header().height as u64;

        self.deliver_state
            .db
            .write()
            .commit(block_height)
            .unwrap_or_else(|_| {
                panic!("Failed to commit chain db at height: {block_height}")
            });

        // Write the DeliverTx state into branched storage and commit the Store.
        // The write to the DeliverTx state writes all state transitions to the root
        // Store so when commit() is called is persists those values.
        let (root_hash, _) = self
            .deliver_state
            .state
            .write()
            .commit(block_height)
            .unwrap_or_else(|_| {
                panic!("Failed to commit chain state at height: {block_height}")
            });

        // Reset the deliver state, but keep the ethereum cache
        Self::update_state(&mut self.deliver_state, Default::default(), vec![]);

        pnk!(self
            .event_notify
            .notify(BlockId::Number(U256::from(block_height))));

        // Set root hash
        let mut res: ResponseCommit = Default::default();
        res.data = root_hash;
        res
    }
}
