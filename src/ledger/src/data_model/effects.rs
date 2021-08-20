use crate::{
    data_model::*,
    staking::{
        self,
        ops::{
            claim::ClaimOps, delegation::DelegationOps,
            fra_distribution::FraDistributionOps, governance::GovernanceOps,
            undelegation::UnDelegationOps, update_staker::UpdateStakerOps,
            update_validator::UpdateValidatorOps,
        },
    },
};
use globutils::HashOf;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ruc::*;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use zei::{
    serialization::ZeiFromToBytes,
    xfr::{
        lib::verify_xfr_body,
        sig::XfrPublicKey,
        structs::{TracingPolicies, XfrAmount, XfrAssetType},
    },
};

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct TxnEffect {
    // The Transaction object this represents
    pub txn: Transaction,
    // Internally-spent TXOs are None, UTXOs are Some(...)
    pub txos: Vec<Option<TxOutput>>,
    // Which TXOs this consumes
    pub input_txos: HashMap<TxoSID, TxOutput>,
    // List of internally-spent TXOs. This does not include input txos;
    pub internally_spent_txos: Vec<TxOutput>,
    // Which new asset types this defines
    pub new_asset_codes: HashMap<AssetTypeCode, AssetType>,
    // Which new TXO issuance sequence numbers are used, in sorted order
    // The vec should be nonempty unless this asset code is being created in
    // this transaction.
    pub new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>>,
    // Which public key is being used to issue each asset type
    pub issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey>,
    // New issuance amounts
    pub issuance_amounts: HashMap<AssetTypeCode, u64>,
    // Asset types that have issuances with confidential outputs. Issuances cannot be confidential
    // if there is an issuance cap
    pub confidential_issuance_types: HashSet<AssetTypeCode>,
    // Mapping of (op index, xfr input idx) tuples to set of valid signature keys
    // i.e. (2, 1) -> { AlicePk, BobPk } means that Alice and Bob both have valid signatures on the 2nd input of the 1st
    // operation
    pub cosig_keys: HashMap<(usize, usize), HashSet<Vec<u8>>>,
    // Non-confidential asset types involved in confidential transfers
    pub confidential_transfer_inputs: HashSet<AssetTypeCode>,
    // Tracing policies that input/outputs types were validated under
    pub tracing_policies: HashMap<AssetTypeCode, TracingPolicies>,

    pub asset_types_involved: HashSet<AssetTypeCode>,
    // Memo updates
    pub memo_updates: Vec<(AssetTypeCode, XfrPublicKey, Memo)>,

    pub update_stakers: Vec<UpdateStakerOps>,
    pub delegations: Vec<DelegationOps>,
    pub undelegations: Vec<UnDelegationOps>,
    pub claims: Vec<ClaimOps>,
    pub update_validators: HashMap<staking::BlockHeight, UpdateValidatorOps>,
    pub governances: Vec<GovernanceOps>,
    pub fra_distributions: Vec<FraDistributionOps>,
}

// Internally validates the transaction as well.
// If the transaction is invalid, it is dropped, so if you need to inspect
// the transaction in order to diagnose the error, clone it first!
#[allow(clippy::cognitive_complexity)]
impl TxnEffect {
    pub fn compute_effect(txn: Transaction) -> Result<TxnEffect> {
        let mut txo_count: usize = 0;
        let mut txos: Vec<Option<TxOutput>> = Vec::new();
        let mut internally_spent_txos = Vec::new();
        let mut input_txos: HashMap<TxoSID, TxOutput> = HashMap::new();
        let mut memo_updates = Vec::new();
        let mut new_asset_codes: HashMap<AssetTypeCode, AssetType> = HashMap::new();
        let mut cosig_keys = HashMap::new();
        let mut new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>> = HashMap::new();
        let mut issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey> = HashMap::new();
        let mut issuance_amounts: HashMap<AssetTypeCode, u64> = HashMap::new();
        let mut tracing_policies: HashMap<AssetTypeCode, TracingPolicies> =
            HashMap::new();
        let mut asset_types_involved: HashSet<AssetTypeCode> = HashSet::new();
        let mut confidential_issuance_types = HashSet::new();
        let mut confidential_transfer_inputs = HashSet::new();
        let mut update_stakers = vec![];
        let mut delegations = vec![];
        let mut undelegations = vec![];
        let mut claims = vec![];
        let mut update_validators = map! {};
        let mut governances = vec![];
        let mut fra_distributions = vec![];

        let mut params = zei::setup::PublicParams::default(); // TODO pass these in
        let mut prng = ChaChaRng::from_entropy();

        // Sequentially go through the operations, validating intrinsic or
        // local-to-the-transaction properties, then recording effects and
        // external properties.
        //
        // Incrementally recording operations in this way is necessary since
        // validity can depend upon earlier operations within a single
        // transaction (eg, a single transaction containing two Transfers which
        // consume the same TXO is invalid).
        //
        // This process should be a complete internal check of a transaction.
        // In particular, functions consuming a TxnEffect should be able to
        // assume that all internal consistency checks are valid, and that the
        // validity of the whole transaction now only depends on the
        // relationship between the outside world and the TxnEffect's fields
        // (eg, any input TXO SIDs of a Transfer should be recorded in
        // `input_txos` and that Transfer should be valid if all those TXO SIDs
        // exist unspent in the ledger and correspond to the correct
        // TxOutput).
        for (op_idx, op) in txn.body.operations.iter().enumerate() {
            macro_rules! check_nonce {
                ($i: expr) => {
                    if $i.get_nonce() != txn.body.no_replay_token {
                        return Err(eg!(("nonce does not match")));
                    }
                };
            }

            match op {
                Operation::UpdateStaker(i) => {
                    check_nonce!(i);
                    i.verify().c(d!())?;
                    update_stakers.push(i.clone());
                }
                Operation::Delegation(i) => {
                    check_nonce!(i);
                    i.verify().c(d!())?;
                    delegations.push(i.clone());
                }
                Operation::UnDelegation(i) => {
                    check_nonce!(i);
                    i.verify().c(d!())?;
                    undelegations.push(i.as_ref().clone());
                }
                Operation::Claim(i) => {
                    check_nonce!(i);
                    i.verify().c(d!())?;
                    claims.push(i.clone());
                }
                Operation::UpdateValidator(i) => {
                    check_nonce!(i);
                    // Only one update is allowed at the same height.
                    if update_validators.insert(i.data.height, i.clone()).is_some() {
                        return Err(eg!("dup entries"));
                    }
                }
                Operation::Governance(i) => {
                    check_nonce!(i);
                    governances.push(i.clone());
                }
                Operation::FraDistribution(i) => {
                    check_nonce!(i);
                    fra_distributions.push(i.clone());
                }
                Operation::MintFra(i) => {
                    i.entries.iter().for_each(|et| {
                        txos.push(Some(et.utxo.clone()));
                        txo_count += 1;
                    });
                }

                // An asset creation is valid iff:
                //     1) The signature is valid.
                //         - Fully checked here
                //     2) The token id is available.
                //         - Partially checked here
                Operation::DefineAsset(def) => {
                    // (1)
                    // TODO(joe?): like the note in data_model, should the public key
                    // used here match `def.body.asset.issuer`?
                    def.signature.verify(&def.pubkey.key, &def.body).c(d!())?;

                    let code = def.body.asset.code;
                    let token = AssetType {
                        properties: *def.body.asset.clone(),
                        ..Default::default()
                    };

                    // (2), only within this transaction
                    if new_asset_codes.contains_key(&code)
                        || new_issuance_nums.contains_key(&code)
                    {
                        return Err(eg!());
                    }

                    issuance_keys.insert(code, token.properties.issuer);
                    new_asset_codes.insert(code, token);
                    new_issuance_nums.insert(code, vec![]);
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
                Operation::IssueAsset(iss) => {
                    if iss.body.num_outputs != iss.body.records.len() {
                        return Err(eg!());
                    }

                    debug_assert!(iss.body.num_outputs == iss.body.records.len());

                    let code = iss.body.code;
                    let seq_num = iss.body.seq_num;

                    asset_types_involved.insert(code);

                    // (1), within this transaction
                    //let v = vec![];
                    let iss_nums =
                        new_issuance_nums.entry(code).or_insert_with(Vec::new);

                    if let Some(last_num) = iss_nums.last() {
                        if seq_num <= *last_num {
                            return Err(eg!());
                        }
                    }
                    iss_nums.push(seq_num);

                    // (2)
                    iss.signature.verify(&iss.pubkey.key, &iss.body).c(d!())?;

                    // (3)
                    if let Some(prior_key) = issuance_keys.get(&code) {
                        if iss.pubkey != *prior_key {
                            return Err(eg!());
                        }
                    } else {
                        issuance_keys.insert(code, iss.pubkey);
                    }
                    // Increment amounts
                    txos.reserve(iss.body.records.len());
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
                        if output.record.asset_type
                            != XfrAssetType::NonConfidential(code.val)
                        {
                            return Err(eg!());
                        }

                        if let XfrAmount::NonConfidential(amt) = output.record.amount {
                            let issuance_amount =
                                issuance_amounts.entry(code).or_insert(0);
                            *issuance_amount =
                                (*issuance_amount).checked_add(amt).c(d!())?;
                        } else {
                            confidential_issuance_types.insert(code);
                        }

                        txos.push(Some(output.clone()));
                        txo_count += 1;
                    }
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
                Operation::TransferAsset(trn) => {
                    if trn.body.inputs.len() != trn.body.transfer.inputs.len() {
                        return Err(eg!());
                    }
                    if trn.body.outputs.len() != trn.body.transfer.outputs.len() {
                        return Err(eg!());
                    }
                    debug_assert!(
                        trn.body.inputs.len() == trn.body.transfer.inputs.len()
                    );
                    debug_assert!(
                        trn.body.outputs.len() == trn.body.transfer.outputs.len()
                    );

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
                        && trn.body.transfer_type != TransferType::Standard
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
                                if let Some(input_idx) = sig.input_idx {
                                    let sig_keys = cosig_keys
                                        .entry((op_idx, input_idx))
                                        .or_insert_with(HashSet::new);
                                    (*sig_keys).insert(sig.address.key.zei_to_bytes());
                                } else {
                                    input_keys.insert(sig.address.key.zei_to_bytes());
                                }
                            }

                            // (1b) all input record owners have signed
                            for (input_idx, record) in
                                trn.body.transfer.inputs.iter().enumerate()
                            {
                                if !input_keys
                                    .contains(&record.public_key.zei_to_bytes())
                                {
                                    return Err(eg!());
                                }
                                cosig_keys
                                    .entry((op_idx, input_idx))
                                    .or_insert_with(HashSet::new);
                            }

                            let policies = trn.body.policies.to_ref();
                            verify_xfr_body(
                                &mut prng,
                                &mut params,
                                &trn.body.transfer,
                                &policies,
                            )
                            .c(d!())?;

                            // Track policies that each asset was validated under
                            for (input_policies, record) in trn
                                .body
                                .policies
                                .inputs_tracing_policies
                                .iter()
                                .zip(trn.body.transfer.inputs.iter())
                                .chain(
                                    trn.body
                                        .policies
                                        .outputs_tracing_policies
                                        .iter()
                                        .zip(trn.body.transfer.outputs.iter()),
                                )
                            {
                                // Only non-confidential assets can be traced
                                if let Some(inp_code) =
                                    record.asset_type.get_asset_type()
                                {
                                    let prev_policies = tracing_policies.insert(
                                        AssetTypeCode { val: inp_code },
                                        input_policies.clone(),
                                    );

                                    // Tracing policies must be consistent w.r.t asset type (cant change)
                                    if prev_policies.is_some()
                                        && prev_policies.c(d!())? != *input_policies
                                    {
                                        return Err(eg!());
                                    }
                                }
                            }
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
                                if offs as usize >= txo_count {
                                    return Err(eg!());
                                }
                                let ix = (txo_count - 1) - (offs as usize);
                                match &txos[ix] {
                                    None => {
                                        return Err(eg!());
                                    }
                                    Some(txo) => {
                                        // (2).(b)
                                        if &txo.record != record
                                            || txo.lien != lien.cloned()
                                        {
                                            return Err(eg!());
                                        }
                                        internally_spent_txos.push(txo.clone());
                                    }
                                }
                                txos[ix] = None;
                            }
                            TxoRef::Absolute(txo_sid) => {
                                // (2).(a), partially
                                if input_txos.contains_key(&txo_sid) {
                                    return Err(eg!());
                                }

                                input_txos.insert(
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

                    txos.reserve(trn.body.transfer.outputs.len());
                    let mut conf_transfer = false;
                    for (out, lien) in trn.body.transfer.outputs.iter().zip(lien_outputs)
                    {
                        if let XfrAssetType::Confidential(_) = out.asset_type {
                            conf_transfer = true;
                        }
                        if let Some(out_code) = out.asset_type.get_asset_type() {
                            asset_types_involved.insert(AssetTypeCode { val: out_code });
                        }
                        txos.push(Some(TxOutput {
                            id: None,
                            record: out.clone(),
                            lien: lien.cloned(),
                        }));
                        txo_count += 1;
                    }
                    // Until we can distinguish assets that have policies that invoke transfer restrictions
                    // from those that don't, make note of all non-confidential inputs of confidential
                    // transfers
                    asset_types_involved.extend(&input_types);
                    if conf_transfer {
                        confidential_transfer_inputs.extend(&input_types);
                    }
                }

                // A memo update is valid iff:
                // 1) The signature is valid.
                // 2) The asset type is updatable (checked later).
                // 3) The signing key is the asset issuer key (checked later).
                Operation::UpdateMemo(update_memo) => {
                    let pk = update_memo.pubkey;
                    if txn.body.no_replay_token != update_memo.body.no_replay_token {
                        return Err(eg!(
                            ("compute_effect: txn body token not equal to the token for this UpdateMemo operation")
                        ));
                    }
                    // 1)
                    update_memo
                        .signature
                        .verify(&pk, &update_memo.body)
                        .c(d!())?;

                    memo_updates.push((
                        update_memo.body.asset_type,
                        pk,
                        update_memo.body.new_memo.clone(),
                    ));
                }
            } // end -- match op {...}
        } // end -- for op in txn.body.operations.iter() {...}

        let txn_effect = TxnEffect {
            txn,
            txos,
            input_txos,
            internally_spent_txos,
            new_asset_codes,
            new_issuance_nums,
            issuance_keys,
            issuance_amounts,
            confidential_issuance_types,
            cosig_keys,
            confidential_transfer_inputs,
            tracing_policies,
            asset_types_involved,
            memo_updates,
            update_stakers,
            delegations,
            undelegations,
            claims,
            update_validators,
            governances,
            fra_distributions,
        };

        Ok(txn_effect)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Default, Serialize)]
pub struct BlockEffect {
    // All Transaction objects validated in this block
    pub txns: Vec<Transaction>,
    // All NoReplayTokens seen in this block
    pub no_replay_tokens: Vec<NoReplayToken>,
    // Identifiers within this block for each transaction
    // (currently just an index into `txns`)
    pub temp_sids: Vec<TxnTempSID>,
    // Internally-spent TXOs are None, UTXOs are Some(...)
    // Should line up element-wise with `txns`
    pub txos: Vec<Vec<Option<TxOutput>>>,
    // Which TXOs this consumes
    pub input_txos: HashMap<TxoSID, TxOutput>,
    // Which new asset types this defines
    pub new_asset_codes: HashMap<AssetTypeCode, AssetType>,
    // Which new TXO issuance sequence numbers are used, in sorted order
    // The vec should be nonempty unless this asset code is being created in
    // this transaction.
    pub new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>>,
    // New issuance amounts
    pub issuance_amounts: HashMap<AssetTypeCode, u64>,
    // Which public key is being used to issue each asset type
    pub issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey>,
    // Memo updates
    pub memo_updates: HashMap<AssetTypeCode, Memo>,
    // counter for consensus integration; will add to a running count when applied.
    pub pulse_count: u64,

    pub staking_simulator: staking::Staking,
}

impl BlockEffect {
    pub fn get_staking_simulator_mut(&mut self) -> &mut staking::Staking {
        &mut self.staking_simulator
    }
}

impl BlockEffect {
    pub fn new() -> BlockEffect {
        Default::default()
    }

    // Combine a TxnEffect into this block.
    //
    // NOTE: this does not check the TxnEffect against the rest of the ledger
    // state, so each TxnEffect should be passed through
    // LedgerStatus::check_txn_effects *first*.
    //
    // Returns:
    //   if `txn` would not interfere with any transaction in the block, the
    //       new temp SID representing the transaction.
    //   Otherwise, Err(...)
    #[allow(clippy::cognitive_complexity)]
    pub fn add_txn_effect(
        &mut self,
        txn_effect: TxnEffect,
        is_loading: bool,
    ) -> Result<TxnTempSID> {
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
                if !nums.is_empty() {
                    debug_assert!(txn_effect.issuance_keys.contains_key(&type_code));
                }
            }
            // Ensure that each asset's memo can only be updated once per block
            for (type_code, _, _) in txn_effect.memo_updates.iter() {
                if self.memo_updates.contains_key(&type_code) {
                    return Err(eg!());
                }
            }
        }

        let no_replay_token = txn_effect.txn.body.no_replay_token;
        // Check that no operations are duplicated as in a replay attack
        // Note that we need to check here as well as in LedgerStatus::check_txn_effect
        for txn in self.txns.iter() {
            if txn.body.no_replay_token == no_replay_token {
                return Err(eg!());
            }
        }

        // NOTE: set at the last position
        if !is_loading {
            self.check_staking(&txn_effect).c(d!())?;
        }

        ///////////////////////////////////////////////////////
        ///////////////////////////////////////////////////////
        ///////////////////////////////////////////////////////

        // By construction, no_replay_tokens entries are unique
        self.no_replay_tokens.push(no_replay_token);

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

    fn check_staking(&mut self, txn_effect: &TxnEffect) -> Result<()> {
        for i in txn_effect.update_stakers.iter() {
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

    pub fn get_pulse_count(&self) -> u64 {
        self.pulse_count
    }

    pub fn add_pulse(&mut self) -> u64 {
        self.pulse_count += 1;
        self.pulse_count
    }
    pub fn compute_txns_in_block_hash(&self) -> HashOf<Vec<Transaction>> {
        HashOf::new(&self.txns)
    }
}
