use crate::extensions::SignedExtra;
use abci::*;
use fp_core::context::RunTxMode;
use fp_evm::BlockId;
use fp_types::assemble::convert_unchecked_transaction;
use fp_utils::tx::EvmRawTxWrapper;
use log::{debug, error, info};
use primitive_types::U256;
use ruc::*;

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
            return err_resp(format!("Cannot create query context with err: {}!", e));
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
            let check_fn = |mode: RunTxMode| {
                let ctx = {
                    let mut ctx = self.check_state.clone();
                    ctx.run_mode = mode;
                    ctx
                };
                let result = self.modules.process_tx::<SignedExtra>(ctx, tx);
                match result {
                    Ok(ar) => {
                        resp.code = ar.code;
                        if ar.code != 0 {
                            info!(target: "baseapp", "Transaction check error, action result {:?}", ar);
                            resp.log = ar.log;
                        }
                    }
                    Err(e) => {
                        info!(target: "baseapp", "Transaction check error: {}", e);
                        resp.code = 1;
                        resp.log = format!("Transaction check error: {}", e);
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

        self.modules.begin_block(&mut self.deliver_state, req);

        ResponseBeginBlock::default()
    }

    pub fn deliver_tx(&mut self, req: &RequestDeliverTx) -> ResponseDeliverTx {
        let mut resp = ResponseDeliverTx::new();

        let raw_tx = if let Ok(tx) = EvmRawTxWrapper::unwrap(req.get_tx()) {
            tx
        } else {
            info!(target: "baseapp", "Transaction deliver tx unwrap evm tag failed");
            resp.code = 1;
            resp.log = String::from("Transaction deliver tx unwrap evm tag failed");
            return resp;
        };

        if let Ok(tx) = convert_unchecked_transaction::<SignedExtra>(raw_tx) {
            let ctx = self.retrieve_context(RunTxMode::Deliver).clone();

            let ret = self.modules.process_tx::<SignedExtra>(ctx, tx);
            match ret {
                Ok(ar) => {
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
                    resp
                }
                Err(e) => {
                    error!(target: "baseapp", "Ethereum transaction deliver error: {}", e);
                    resp.code = 1;
                    resp.log = format!("Ethereum transaction deliver error: {}", e);
                    resp
                }
            }
        } else {
            resp.code = 1;
            resp.log = String::from("Failed to convert transaction when deliver tx!");
            resp
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
        self.check_state = self.deliver_state.copy_with_new_state();

        let block_height = self.deliver_state.block_header().height as u64;
        //let mut ctx = self.retrieve_context(RunTxMode::Deliver).clone(); LOCAL FIXED

        // Write the DeliverTx state into branched storage and commit the Store.
        // The write to the DeliverTx state writes all state transitions to the root
        // Store so when commit() is called is persists those values.
        let (root_hash, _) = self
            .deliver_state
            .state
            .write()
            .commit(block_height)
            .unwrap_or_else(|e| {
                println!("{:?}", e);
                panic!("Failed to commit chain state at height: {}", block_height)
            });

        // Commit module data based on root_hash
      /*  let _ = ruc::info!(self.modules.commit(&mut ctx, block_height, &root_hash));

        // Commit non chain-state data
        self.deliver_state
            .db
            .write()
            .commit(block_height)
            .unwrap_or_else(|_| {
                panic!("Failed to commit chain db at height: {}", block_height)
            });
        */
        // Reset the deliver state
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
