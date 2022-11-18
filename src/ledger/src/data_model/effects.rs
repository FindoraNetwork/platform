use {
    crate::{
        data_model::{
            AssetType, AssetTypeCode, DefineAsset, IssueAsset, IssuerPublicKey, Memo,
            NoReplayToken, Operation, Transaction, TransferAsset, TransferType,
            TxOutput, TxnTempSID, TxoRef, TxoSID, UpdateMemo,
        },
        staking::{
            self,
            ops::{
                claim::ClaimOps, delegation::DelegationOps,
                fra_distribution::FraDistributionOps, governance::GovernanceOps,
                replace_staker::ReplaceStakerOps, undelegation::UnDelegationOps,
                update_staker::UpdateStakerOps, update_validator::UpdateValidatorOps,
            },
        },
    },
    globutils::HashOf,
    lazy_static::lazy_static,
    parking_lot::Mutex,
    rand_chacha::{ChaCha20Rng, ChaChaRng},
    rand_core::SeedableRng,
    ruc::*,
    serde::Serialize,
    std::{
        collections::{HashMap, HashSet},
        sync::Arc,
    },
    zei::{
        serialization::ZeiFromToBytes,
        setup::PublicParams,
        xfr::{
            lib::verify_xfr_body,
            sig::XfrPublicKey,
            structs::{XfrAmount, XfrAssetType},
        },
    },
};

lazy_static! {
    static ref PRNG: Arc<Mutex<ChaCha20Rng>> =
        Arc::new(Mutex::new(ChaChaRng::from_entropy()));
    static ref PARAMS: Arc<Mutex<PublicParams>> =
        Arc::new(Mutex::new(PublicParams::default()));
}

/// Check operations in the context of a tx, partially.
#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct TxnEffect {
    /// The Transaction object this represents
    pub txn: Transaction,
    /// Internally-spent TXOs are None, UTXOs are Some(...)
    pub txos: Vec<Option<TxOutput>>,
    /// Which TXOs this consumes
    pub input_txos: HashMap<TxoSID, TxOutput>,
    /// List of internally-spent TXOs. This does not include input txos;
    pub internally_spent_txos: Vec<TxOutput>,
    /// Which new asset types this defines
    pub new_asset_codes: HashMap<AssetTypeCode, AssetType>,
    /// Which new TXO issuance sequence numbers are used, in sorted order
    /// The vec should be nonempty unless this asset code is being created in
    /// this transaction.
    pub new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>>,
    /// Which public key is being used to issue each asset type
    pub issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey>,
    /// New issuance amounts
    pub issuance_amounts: HashMap<AssetTypeCode, u64>,
    /// Asset types that have issuances with confidential outputs. Issuances cannot be confidential
    /// if there is an issuance cap
    pub confidential_issuance_types: HashSet<AssetTypeCode>,
    /// Non-confidential asset types involved in confidential transfers
    pub confidential_transfer_inputs: HashSet<AssetTypeCode>,

    /// Asset types involved in this tx
    pub asset_types_involved: HashSet<AssetTypeCode>,
    /// Memo updates
    pub memo_updates: Vec<(AssetTypeCode, XfrPublicKey, Memo)>,

    /// Staking operations
    pub delegations: Vec<DelegationOps>,
    /// Staking operations
    pub undelegations: Vec<UnDelegationOps>,
    /// Staking operations
    pub claims: Vec<ClaimOps>,
    /// Staking operations
    pub governances: Vec<GovernanceOps>,
    /// Staking operations
    pub update_validators: HashMap<staking::BlockHeight, UpdateValidatorOps>,
    /// Staking operations
    pub fra_distributions: Vec<FraDistributionOps>,
    /// Staking operations
    pub update_stakers: Vec<UpdateStakerOps>,
    /// replace staker operations
    pub replace_stakers: Vec<ReplaceStakerOps>,
}

impl TxnEffect {
    /// Internally validates the transaction as well.
    /// If the transaction is invalid, it is dropped, so if you need to inspect
    /// the transaction in order to diagnose the error, clone it first!
    ///
    /// Sequentially go through the operations, validating intrinsic or
    /// local-to-the-transaction properties, then recording effects and
    /// external properties.
    ///
    /// Incrementally recording operations in this way is necessary since
    /// validity can depend upon earlier operations within a single
    /// transaction (eg, a single transaction containing two Transfers which
    /// consume the same TXO is invalid).
    ///
    /// This process should be a complete internal check of a transaction.
    /// In particular, functions consuming a TxnEffect should be able to
    /// assume that all internal consistency checks are valid, and that the
    /// validity of the whole transaction now only depends on the
    /// relationship between the outside world and the TxnEffect's fields
    /// (eg, any input TXO SIDs of a Transfer should be recorded in
    /// `input_txos` and that Transfer should be valid if all those TXO SIDs
    /// exist unspent in the ledger and correspond to the correct
    /// TxOutput).
    pub fn compute_effect(txn: Transaction) -> Result<TxnEffect> {
        let mut te = TxnEffect::default();
        let mut txo_count: usize = 0;

        for op in txn.body.operations.iter() {
            macro_rules! check_nonce {
                ($i: expr) => {
                    if $i.get_nonce() != txn.body.no_replay_token {
                        return Err(eg!(("nonce does not match")));
                    }
                };
            }

            match op {
                Operation::MintFra(i) => {
                    i.entries.iter().for_each(|et| {
                        te.txos.push(Some(et.utxo.clone()));
                        txo_count += 1;
                    });
                }
                Operation::TransferAsset(trn) => {
                    te.add_transfer_asset(trn, &mut txo_count).c(d!())?;
                }
                Operation::Claim(i) => {
                    check_nonce!(i);
                    i.verify().c(d!())?;
                    te.claims.push(i.clone());
                }
                Operation::Delegation(i) => {
                    check_nonce!(i);
                    i.verify().c(d!())?;
                    te.delegations.push(i.clone());
                }
                Operation::UnDelegation(i) => {
                    check_nonce!(i);
                    i.verify().c(d!())?;
                    te.undelegations.push(i.as_ref().clone());
                }
                Operation::UpdateStaker(i) => {
                    check_nonce!(i);
                    i.verify().c(d!())?;
                    te.update_stakers.push(i.clone());
                }
                Operation::ReplaceStaker(i) => {
                    check_nonce!(i);
                    i.verify().c(d!())?;
                    te.replace_stakers.push(i.clone());
                }
                Operation::UpdateValidator(i) => {
                    check_nonce!(i);
                    // Only one update is allowed at the same height.
                    if te
                        .update_validators
                        .insert(i.data.height, i.clone())
                        .is_some()
                    {
                        return Err(eg!("dup entries"));
                    }
                }
                Operation::DefineAsset(def) => {
                    te.add_define_asset(def).c(d!())?;
                }
                Operation::IssueAsset(iss) => {
                    te.add_issue_asset(iss, &mut txo_count).c(d!())?;
                }
                Operation::UpdateMemo(update_memo) => {
                    te.add_update_memo(&txn, update_memo).c(d!())?;
                }
                Operation::Governance(i) => {
                    check_nonce!(i);
                    te.governances.push(i.clone());
                }
                Operation::FraDistribution(i) => {
                    check_nonce!(i);
                    te.fra_distributions.push(i.clone());
                }
                Operation::ConvertAccount(i) => {
                    check_nonce!(i)
                }
            }
        }

        te.txn = txn;
        Ok(te)
    }

    // An asset creation is valid iff:
    //     1) The signature is valid.
    //         - Fully checked here
    //     2) The token id is available.
    //         - Partially checked here
    fn add_define_asset(&mut self, def: &DefineAsset) -> Result<()> {
        // (1)
        def.signature.verify(&def.pubkey.key, &def.body).c(d!())?;

        let code = def.body.asset.code;
        let token = AssetType {
            properties: *def.body.asset.clone(),
            ..Default::default()
        };

        // (2), only within this transaction
        if self.new_asset_codes.contains_key(&code)
            || self.new_issuance_nums.contains_key(&code)
        {
            return Err(eg!());
        }

        self.issuance_keys.insert(code, token.properties.issuer);
        self.new_asset_codes.insert(code, token);
        self.new_issuance_nums.insert(code, vec![]);

        Ok(())
    }

    // The asset issuance is valid iff:
    //      1) The operation is unique (not a replay).
    //          - Partially checked here
    //      2) The signature is valid.
    //          - Fully checked here
    //      3) The signature belongs to the anchor (the issuer).
    //          - Either checked here or recorded in `new_issuance_keys`
    //      4) The assets in the TxOutputs are owned by the signatory.
    //          - Fully checked here
    //      5) The assets in the TxOutputs have a non-confidential
    //         asset type which agrees with the stated asset type.
    //          - Fully checked here
    fn add_issue_asset(
        &mut self,
        iss: &IssueAsset,
        txo_count: &mut usize,
    ) -> Result<()> {
        if iss.body.num_outputs != iss.body.records.len() {
            return Err(eg!());
        }

        let code = iss.body.code;
        let seq_num = iss.body.seq_num;

        self.asset_types_involved.insert(code);

        // (1), within this transaction
        //let v = vec![];
        let iss_nums = self.new_issuance_nums.entry(code).or_insert_with(Vec::new);

        if let Some(last_num) = iss_nums.last() {
            if seq_num <= *last_num {
                return Err(eg!());
            }
        }
        iss_nums.push(seq_num);

        // (2)
        iss.signature.verify(&iss.pubkey.key, &iss.body).c(d!())?;

        // (3)
        if let Some(prior_key) = self.issuance_keys.get(&code) {
            if iss.pubkey != *prior_key {
                return Err(eg!());
            }
        } else {
            self.issuance_keys.insert(code, iss.pubkey);
        }
        // Increment amounts
        self.txos.reserve(iss.body.records.len());
        for (output, _) in iss.body.records.iter() {
            // (4)
            if output.record.public_key != iss.pubkey.key {
                return Err(eg!());
            }

            // ONLY SIMPLE TxOutputs!
            if output
                != &(TxOutput {
                    id: None,
                    record: output.record.clone(),
                    lien: None,
                })
            {
                return Err(eg!());
            }

            // (5)
            if output.record.asset_type != XfrAssetType::NonConfidential(code.val) {
                return Err(eg!());
            }

            if let XfrAmount::NonConfidential(amt) = output.record.amount {
                let issuance_amount = self.issuance_amounts.entry(code).or_insert(0);
                *issuance_amount = (*issuance_amount).checked_add(amt).c(d!())?;
            } else {
                self.confidential_issuance_types.insert(code);
            }

            self.txos.push(Some(output.clone()));
            *txo_count += 1;
        }
        Ok(())
    }

    // An asset transfer is valid iff:
    //     1) The signatures on the body (a) all are valid and (b)
    //        there is a signature for each input key
    //          - Fully checked here
    //     2) The UTXOs (a) exist on the ledger and (b) match the zei transaction.
    //          - Partially checked here -- anything which hasn't
    //            been checked will appear in `input_txos`
    //     3) The zei transaction is valid.
    //          - Checked here and in check_txn_effects
    //     4) Lien assignments match up
    //          - Checked within a transaction here, recorded for
    //            external checks later
    //          - For simplicity, only Standard transfers are allowed
    //            to have lien assignments
    fn add_transfer_asset(
        &mut self,
        trn: &TransferAsset,
        txo_count: &mut usize,
    ) -> Result<()> {
        let params = &mut *PARAMS.lock();
        let prng = &mut *PRNG.lock();

        if trn.body.inputs.len() != trn.body.transfer.inputs.len() {
            return Err(eg!());
        }
        if trn.body.outputs.len() != trn.body.transfer.outputs.len() {
            return Err(eg!());
        }

        // Transfer outputs must match outputs zei transaction
        for (output, record) in trn
            .body
            .outputs
            .iter()
            .zip(trn.body.transfer.outputs.iter())
        {
            if output.record != *record {
                return Err(eg!());
            }
        }

        // Simplify (4)
        if !trn.body.lien_assignments.is_empty()
            || trn.body.transfer_type != TransferType::Standard
        {
            return Err(eg!());
        }
        let (lien_inputs, lien_outputs) = {
            let mut inps = trn
                .body
                .transfer
                .inputs
                .iter()
                .map(|_| None)
                .collect::<Vec<_>>();
            let mut outs = trn
                .body
                .transfer
                .outputs
                .iter()
                .map(|_| None)
                .collect::<Vec<_>>();
            for (inp_ix, out_ix, hash) in trn.body.lien_assignments.iter() {
                let (inp_ix, out_ix) = (*inp_ix, *out_ix);
                match (inps.get_mut(inp_ix), outs.get_mut(out_ix)) {
                    (Some(ele_in), Some(ele_out)) => {
                        *ele_in = Some(hash);
                        *ele_out = Some(hash);
                    }
                    _ => {
                        return Err(eg!());
                    }
                }
            }
            (inps, outs)
        };

        match trn.body.transfer_type {
            TransferType::DebtSwap => {}
            TransferType::Standard => {
                let mut input_keys = HashSet::new();
                // (1a) all body signatures are valid
                for sig in &trn.body_signatures {
                    if !trn.body.verify_body_signature(sig) {
                        return Err(eg!());
                    }
                    input_keys.insert(sig.address.key.zei_to_bytes());
                }

                // (1b) all input record owners have signed
                for record in trn.body.transfer.inputs.iter() {
                    if !input_keys.contains(&record.public_key.zei_to_bytes()) {
                        return Err(eg!());
                    }
                }

                verify_xfr_body(
                    prng,
                    params,
                    &trn.body.transfer,
                    &trn.body.policies.to_ref(),
                )
                .c(d!())?;
            }
        }
        // (3)
        let mut input_types = HashSet::new();
        for ((inp, record), lien) in trn
            .body
            .inputs
            .iter()
            .zip(trn.body.transfer.inputs.iter())
            .zip(lien_inputs)
        {
            // NOTE: We assume that any confidential-type asset records
            // have no atypical transfer restrictions. Be careful!
            if let Some(inp_code) = record.asset_type.get_asset_type() {
                input_types.insert(AssetTypeCode { val: inp_code });
                //asset_types_involved.insert(AssetTypeCode { val: inp_code });
            }

            // (2), checking within this transaction and recording
            // external UTXOs
            match *inp {
                TxoRef::Relative(offs) => {
                    // (2).(a)
                    if offs as usize >= *txo_count {
                        return Err(eg!());
                    }
                    let ix = (*txo_count - 1) - (offs as usize);
                    match &self.txos[ix] {
                        None => {
                            return Err(eg!());
                        }
                        Some(txo) => {
                            // (2).(b)
                            if &txo.record != record || txo.lien != lien.cloned() {
                                return Err(eg!());
                            }
                            self.internally_spent_txos.push(txo.clone());
                        }
                    }
                    self.txos[ix] = None;
                }
                TxoRef::Absolute(txo_sid) => {
                    // (2).(a), partially
                    if self.input_txos.contains_key(&txo_sid) {
                        return Err(eg!());
                    }

                    self.input_txos.insert(
                        txo_sid,
                        TxOutput {
                            id: None,
                            record: record.clone(),
                            lien: lien.cloned(),
                        },
                    );
                }
            }
        }

        self.txos.reserve(trn.body.transfer.outputs.len());
        let mut conf_transfer = false;
        for (out, lien) in trn.body.transfer.outputs.iter().zip(lien_outputs) {
            if let XfrAssetType::Confidential(_) = out.asset_type {
                conf_transfer = true;
            }
            if let Some(out_code) = out.asset_type.get_asset_type() {
                self.asset_types_involved
                    .insert(AssetTypeCode { val: out_code });
            }
            self.txos.push(Some(TxOutput {
                id: None,
                record: out.clone(),
                lien: lien.cloned(),
            }));
            *txo_count += 1;
        }
        // Until we can distinguish assets that have policies that invoke transfer restrictions
        // from those that don't, make note of all non-confidential inputs of confidential
        // transfers
        self.asset_types_involved.extend(&input_types);
        if conf_transfer {
            self.confidential_transfer_inputs.extend(&input_types);
        }

        Ok(())
    }

    // A memo update is valid iff:
    // 1) The signature is valid.
    // 2) The asset type is updatable (checked later).
    // 3) The signing key is the asset issuer key (checked later).
    fn add_update_memo(
        &mut self,
        txn: &Transaction,
        update_memo: &UpdateMemo,
    ) -> Result<()> {
        let pk = update_memo.pubkey;
        if txn.body.no_replay_token != update_memo.body.no_replay_token {
            return Err(eg!("replay token not match"));
        }
        // 1)
        update_memo
            .signature
            .verify(&pk, &update_memo.body)
            .c(d!())?;
        self.memo_updates.push((
            update_memo.body.asset_type,
            pk,
            update_memo.body.new_memo.clone(),
        ));

        Ok(())
    }
}

/// Check tx in the context of a block, partially.
#[derive(Debug, Clone, Eq, PartialEq, Default, Serialize)]
pub struct BlockEffect {
    /// All Transaction objects validated in this block
    pub txns: Vec<Transaction>,
    /// All NoReplayTokens seen in this block
    pub no_replay_tokens: Vec<NoReplayToken>,
    /// Identifiers within this block for each transaction
    /// (currently just an index into `txns`)
    pub temp_sids: Vec<TxnTempSID>,
    /// Internally-spent TXOs are None, UTXOs are Some(...)
    /// Should line up element-wise with `txns`
    pub txos: Vec<Vec<Option<TxOutput>>>,
    /// Which TXOs this consumes
    pub input_txos: HashMap<TxoSID, TxOutput>,
    /// Which new asset types this defines
    pub new_asset_codes: HashMap<AssetTypeCode, AssetType>,
    /// Which new TXO issuance sequence numbers are used, in sorted order
    /// The vec should be nonempty unless this asset code is being created in
    /// this transaction.
    pub new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>>,
    /// New issuance amounts
    pub issuance_amounts: HashMap<AssetTypeCode, u64>,
    /// Which public key is being used to issue each asset type
    pub issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey>,
    /// Memo updates
    pub memo_updates: HashMap<AssetTypeCode, Memo>,
    /// counter for consensus integration; will add to a running count when applied.
    pub pulse_count: u64,
    /// simulator for safety
    pub staking_simulator: staking::Staking,
}

impl BlockEffect {
    /// Combine a TxnEffect into this block.
    ///
    /// NOTE: this does not check the TxnEffect against the rest of the ledger
    /// state, so each TxnEffect should be passed through
    /// LedgerStatus::check_txn_effects *first*.
    ///
    /// Returns:
    ///   if `txn` would not interfere with any transaction in the block, the
    ///       new temp SID representing the transaction.
    ///   Otherwise, Err(...)
    pub fn add_txn_effect(&mut self, txn_effect: TxnEffect) -> Result<TxnTempSID> {
        self.check_txn_effect(&txn_effect).c(d!())?;

        // By construction, no_replay_tokens entries are unique
        self.no_replay_tokens
            .push(txn_effect.txn.body.no_replay_token);

        let temp_sid = TxnTempSID(self.txns.len());
        self.txns.push(txn_effect.txn);
        self.temp_sids.push(temp_sid);
        self.txos.push(txn_effect.txos);

        for (input_sid, record) in txn_effect.input_txos {
            self.input_txos.insert(input_sid, record);
        }

        for (type_code, asset_type) in txn_effect.new_asset_codes {
            self.new_asset_codes.insert(type_code, asset_type);
        }

        for (type_code, issuance_nums) in txn_effect.new_issuance_nums {
            self.new_issuance_nums.insert(type_code, issuance_nums);
        }

        for (type_code, amount) in txn_effect.issuance_amounts.iter() {
            let issuance_amount = self.issuance_amounts.entry(*type_code).or_insert(0);
            *issuance_amount += amount;
        }

        for (code, _, memo) in txn_effect.memo_updates {
            self.memo_updates.insert(code, memo);
        }

        Ok(temp_sid)
    }

    fn check_txn_effect(&mut self, txn_effect: &TxnEffect) -> Result<()> {
        // Check that no inputs are consumed twice
        for (input_sid, _) in txn_effect.input_txos.iter() {
            if self.input_txos.contains_key(&input_sid) {
                return Err(eg!());
            }
        }

        // Check that no AssetType is affected by both the block so far and
        // this transaction
        {
            for (type_code, _) in txn_effect.new_asset_codes.iter() {
                if self.new_asset_codes.contains_key(&type_code)
                    || self.new_issuance_nums.contains_key(&type_code)
                {
                    return Err(eg!());
                }
            }

            for (type_code, nums) in txn_effect.new_issuance_nums.iter() {
                if self.new_asset_codes.contains_key(&type_code)
                    || self.new_issuance_nums.contains_key(&type_code)
                {
                    return Err(eg!());
                }

                // Debug-check that issued assets are registered in `issuance_keys`
                if !nums.is_empty() && !txn_effect.issuance_keys.contains_key(&type_code)
                {
                    return Err(eg!());
                }
            }
            // Ensure that each asset's memo can only be updated once per block
            for (type_code, _, _) in txn_effect.memo_updates.iter() {
                if self.memo_updates.contains_key(&type_code) {
                    return Err(eg!());
                }
            }
        }

        // Check that no operations are duplicated as in a replay attack
        // Note that we need to check here as well as in LedgerStatus::check_txn_effect
        for txn in self.txns.iter() {
            if txn.body.no_replay_token == txn_effect.txn.body.no_replay_token {
                return Err(eg!());
            }
        }

        // NOTE: set at the last position
        self.check_staking(&txn_effect).c(d!())?;

        Ok(())
    }

    fn check_staking(&mut self, txn_effect: &TxnEffect) -> Result<()> {
        for i in txn_effect.update_stakers.iter() {
            i.check_run(&mut self.staking_simulator, &txn_effect.txn)
                .c(d!())?;
        }

        for i in txn_effect.replace_stakers.iter() {
            i.check_run(&mut self.staking_simulator, &txn_effect.txn)
                .c(d!())?;
        }

        for i in txn_effect.delegations.iter() {
            i.check_run(&mut self.staking_simulator, &txn_effect.txn)
                .c(d!())?;
        }

        for i in txn_effect.undelegations.iter() {
            i.check_run(&mut self.staking_simulator, &txn_effect.txn)
                .c(d!())?;
        }

        for i in txn_effect.claims.iter() {
            i.check_run(&mut self.staking_simulator).c(d!())?;
        }

        for i in txn_effect.update_validators.values() {
            i.check_run(&mut self.staking_simulator).c(d!())?;
        }

        for i in txn_effect.governances.iter() {
            i.check_run(&mut self.staking_simulator).c(d!())?;
        }

        for i in txn_effect.fra_distributions.iter() {
            i.check_run(&mut self.staking_simulator, &txn_effect.txn)
                .c(d!())?;
        }

        Ok(())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn compute_txns_in_block_hash(&self) -> HashOf<Vec<Transaction>> {
        let txns: Vec<Transaction> = self
            .txns
            .iter()
            .map(|tx| {
                let mut tx = tx.clone();
                tx.pubkey_sign_map = Default::default();
                tx
            })
            .collect();

        HashOf::new(&txns)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_staking_simulator_mut(&mut self) -> &mut staking::Staking {
        &mut self.staking_simulator
    }
}
