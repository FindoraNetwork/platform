use std::collections::HashMap;

use globutils::{HashOf, SignatureOf};
use ledger::{
    converter::ConvertAccount,
    data_model::{
        CredentialProof, DefineAsset, IndexedSignature, IssueAsset, Memo, NoReplayToken,
        Operation, Transaction, TransactionBody, TransferAsset, TransferAssetBody,
        TransferType, TxOutput, TxnEffect, TxnSID, TxoRef, TxoSID, UpdateMemo,
        XfrAddress, __trash__::TxnPolicyData, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY,
        TX_FEE_MIN,
    },
    staking::ops::{
        claim::ClaimOps, delegation::DelegationOps,
        fra_distribution::FraDistributionOps, governance::GovernanceOps,
        mint_fra::MintFraOps, replace_staker::ReplaceStakerOps,
        undelegation::UnDelegationOps, update_staker::UpdateStakerOps,
        update_validator::UpdateValidatorOps,
    },
};
use rand_chacha::ChaChaRng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use ruc::*;
use serde::{Deserialize, Serialize};
use zei::xfr::{
    lib::{gen_xfr_body, XfrNotePolicies},
    sig::{XfrKeyPair, XfrPublicKey},
    structs::{
        AssetRecord, BlindAssetRecord, OwnerMemo, TracingPolicies, XfrAmount,
        XfrAssetType, XfrBody,
    },
};

#[inline(always)]
fn is_default<T: Default + PartialEq>(x: &T) -> bool {
    x == &T::default()
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BuildTxOutput {
    pub id: Option<TxoSID>,
    pub record: BlindAssetRecord,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub lien: Option<HashOf<Vec<TxOutput>>>,
    #[serde(skip_serializing_if = "is_default")]
    pub memo: Option<String>,
}
impl From<TxOutput> for BuildTxOutput {
    fn from(value: TxOutput) -> Self {
        Self {
            id: value.id,
            record: value.record,
            lien: value.lien,
            memo: None,
        }
    }
}
impl From<BuildTxOutput> for TxOutput {
    fn from(value: BuildTxOutput) -> Self {
        Self {
            id: value.id,
            record: value.record,
            lien: value.lien,
        }
    }
}

/// The inner data of Transfer Operation
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BuildTransferAssetBody {
    /// Ledger address of inputs
    pub inputs: Vec<TxoRef>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    /// Transfer policies
    pub policies: XfrNotePolicies,
    /// A array of transaction outputs
    pub outputs: Vec<BuildTxOutput>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    /// (inp_idx,out_idx,hash) triples signifying that the lien `hash` on
    /// the input `inp_idx` gets assigned to the output `out_idx`
    pub lien_assignments: Vec<(usize, usize, HashOf<Vec<TxOutput>>)>,
    /// TODO(joe): we probably don't need the whole XfrNote with input records
    /// once it's on the chain
    /// Encrypted transfer note
    pub transfer: Box<XfrBody>,

    /// Only Standard type supported
    pub transfer_type: TransferType,
}
impl From<TransferAssetBody> for BuildTransferAssetBody {
    fn from(value: TransferAssetBody) -> Self {
        Self {
            inputs: value.inputs,
            policies: value.policies,
            outputs: value.outputs.iter().map(|v| v.clone().into()).collect(),
            lien_assignments: value.lien_assignments,
            transfer: value.transfer,
            transfer_type: value.transfer_type,
        }
    }
}
impl From<BuildTransferAssetBody> for TransferAssetBody {
    fn from(value: BuildTransferAssetBody) -> Self {
        Self {
            inputs: value.inputs,
            policies: value.policies,
            outputs: value.outputs.iter().map(|v| v.clone().into()).collect(),
            lien_assignments: value.lien_assignments,
            transfer: value.transfer,
            transfer_type: value.transfer_type,
        }
    }
}

impl BuildTransferAssetBody {
    #[allow(missing_docs)]
    #[allow(clippy::too_many_arguments)]
    pub fn new<R: CryptoRng + RngCore>(
        prng: &mut R,
        input_refs: Vec<TxoRef>,
        input_records: &[AssetRecord],
        output_records: &[AssetRecord],
        output_memos: &[Option<String>],
        policies: Option<XfrNotePolicies>,
        lien_assignments: Vec<(usize, usize, HashOf<Vec<TxOutput>>)>,
        transfer_type: TransferType,
    ) -> Result<Self> {
        let num_inputs = input_records.len();
        let num_outputs = output_records.len();

        if num_inputs == 0 {
            return Err(eg!());
        }

        // If no policies specified, construct set of empty policies
        let policies = policies.unwrap_or_else(|| {
            let no_policies = TracingPolicies::new();
            XfrNotePolicies::new(
                vec![no_policies.clone(); num_inputs],
                vec![None; num_inputs],
                vec![no_policies; num_outputs],
                vec![None; num_outputs],
            )
        });

        // Verify that for each input and output, there is a corresponding policy and credential commitment
        if num_inputs != policies.inputs_tracing_policies.len()
            || num_inputs != policies.inputs_sig_commitments.len()
            || num_outputs != policies.outputs_tracing_policies.len()
            || num_outputs != policies.outputs_sig_commitments.len()
        {
            return Err(eg!());
        }

        let transfer =
            Box::new(gen_xfr_body(prng, input_records, output_records).c(d!())?);
        let outputs = transfer
            .outputs
            .iter()
            .zip(output_memos.iter())
            .map(|(rec, memo)| BuildTxOutput {
                id: None,
                record: rec.clone(),
                lien: None,
                memo: memo.clone(),
            })
            .collect();
        Ok(Self {
            inputs: input_refs,
            outputs,
            policies,
            lien_assignments,
            transfer,
            transfer_type,
        })
    }
    /// Computes a body signature. A body signature represents consent to some part of the asset transfer. If an
    /// input_idx is specified, the signature is a co-signature.
    #[inline(always)]
    pub fn compute_body_signature(
        &self,
        keypair: &XfrKeyPair,
        input_idx: Option<usize>,
    ) -> IndexedSignature<TransferAssetBody> {
        let public_key = keypair.get_pk_ref();
        IndexedSignature {
            signature: SignatureOf::new(keypair, &(self.clone().into(), input_idx)),
            address: XfrAddress { key: *public_key },
            input_idx,
        }
    }
} /*
      /// Verifies a body signature
      #[inline(always)]
      pub fn verify_body_signature(
          &self,
          signature: &IndexedSignature<TransferAssetBody>,
      ) -> bool {
          signature.verify(&self.into())
      }
  }
  */
#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BuildTransferAsset {
    pub body: BuildTransferAssetBody,
    pub body_signatures: Vec<IndexedSignature<TransferAssetBody>>,
}
impl From<TransferAsset> for BuildTransferAsset {
    fn from(value: TransferAsset) -> Self {
        Self {
            body: value.body.into(),
            body_signatures: value.body_signatures,
        }
    }
}
impl From<BuildTransferAsset> for TransferAsset {
    fn from(value: BuildTransferAsset) -> Self {
        Self {
            body: value.body.into(),
            body_signatures: value.body_signatures,
        }
    }
}
impl BuildTransferAsset {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(transfer_body: BuildTransferAssetBody) -> Result<Self> {
        Ok(Self {
            body: transfer_body,
            body_signatures: Vec::new(),
        })
    }
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn sign(&mut self, keypair: &XfrKeyPair) {
        let sig = self.create_input_signature(keypair);
        self.attach_signature(sig).unwrap()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn attach_signature(
        &mut self,
        sig: IndexedSignature<TransferAssetBody>,
    ) -> Result<()> {
        if !sig.verify(&self.body.clone().into()) {
            return Err(eg!());
        }
        self.body_signatures.push(sig);
        Ok(())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn create_input_signature(
        &self,
        keypair: &XfrKeyPair,
    ) -> IndexedSignature<TransferAssetBody> {
        self.body.compute_body_signature(keypair, None)
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_owner_memos_ref(&self) -> Vec<Option<&OwnerMemo>> {
        self.body
            .transfer
            .owners_memos
            .iter()
            .map(|mem| mem.as_ref())
            .collect()
    }
} /*
      #[inline(always)]
      #[allow(missing_docs)]
      pub fn get_owner_addresses(&self) -> Vec<XfrPublicKey> {
          self.body
              .transfer
              .inputs
              .iter()
              .map(|record| record.public_key)
              .collect()
      }

      #[inline(always)]
      #[allow(missing_docs)]
      pub fn get_outputs_ref(&self) -> Vec<&BuildTxOutput> {
          self.body.outputs.iter().collect()
      }
  }
  */
/// Operation list supported in findora network
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum BuildOperation {
    /// Transfer a findora asset, FRA or custom asset
    TransferAsset(BuildTransferAsset),
    /// Issue a custom asset in findora network
    IssueAsset(IssueAsset),
    /// Create a new asset for a findora account
    DefineAsset(DefineAsset),
    /// Update memo for a findora custom asset
    UpdateMemo(UpdateMemo),
    /// Add or remove validator from findora network
    UpdateStaker(UpdateStakerOps),
    /// Delegate FRA token to existed validator or self-delegation
    Delegation(DelegationOps),
    /// Withdraw FRA token from findora network
    UnDelegation(Box<UnDelegationOps>),
    /// Claim rewards
    Claim(ClaimOps),
    /// Update initial validator list
    UpdateValidator(UpdateValidatorOps),
    /// Findora network goverance operation
    Governance(GovernanceOps),
    /// Update FRA distribution
    FraDistribution(FraDistributionOps),
    /// Coinbase operation
    MintFra(MintFraOps),
    /// Convert UTXOs to EVM Account balance
    ConvertAccount(ConvertAccount),
    ///replace staker.
    ReplaceStaker(ReplaceStakerOps),
}
impl From<&Operation> for BuildOperation {
    fn from(value: &Operation) -> Self {
        match value.clone() {
            Operation::TransferAsset(op) => Self::TransferAsset(op.into()),
            Operation::IssueAsset(op) => Self::IssueAsset(op),
            Operation::DefineAsset(op) => Self::DefineAsset(op),
            Operation::UpdateMemo(op) => Self::UpdateMemo(op),
            Operation::UpdateStaker(op) => Self::UpdateStaker(op),
            Operation::Delegation(op) => Self::Delegation(op),
            Operation::UnDelegation(op) => Self::UnDelegation(op),
            Operation::Claim(op) => Self::Claim(op),
            Operation::UpdateValidator(op) => Self::UpdateValidator(op),
            Operation::Governance(op) => Self::Governance(op),
            Operation::FraDistribution(op) => Self::FraDistribution(op),
            Operation::MintFra(op) => Self::MintFra(op),
            Operation::ConvertAccount(op) => Self::ConvertAccount(op),
            Operation::ReplaceStaker(op) => Self::ReplaceStaker(op),
        }
    }
}
impl From<&BuildOperation> for Operation {
    fn from(value: &BuildOperation) -> Self {
        match value.clone() {
            BuildOperation::TransferAsset(op) => Self::TransferAsset(op.into()),
            BuildOperation::IssueAsset(op) => Self::IssueAsset(op),
            BuildOperation::DefineAsset(op) => Self::DefineAsset(op),
            BuildOperation::UpdateMemo(op) => Self::UpdateMemo(op),
            BuildOperation::UpdateStaker(op) => Self::UpdateStaker(op),
            BuildOperation::Delegation(op) => Self::Delegation(op),
            BuildOperation::UnDelegation(op) => Self::UnDelegation(op),
            BuildOperation::Claim(op) => Self::Claim(op),
            BuildOperation::UpdateValidator(op) => Self::UpdateValidator(op),
            BuildOperation::Governance(op) => Self::Governance(op),
            BuildOperation::FraDistribution(op) => Self::FraDistribution(op),
            BuildOperation::MintFra(op) => Self::MintFra(op),
            BuildOperation::ConvertAccount(op) => Self::ConvertAccount(op),
            BuildOperation::ReplaceStaker(op) => Self::ReplaceStaker(op),
        }
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize, Default)]
pub struct BuildTransactionBody {
    pub no_replay_token: NoReplayToken,
    pub operations: Vec<BuildOperation>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub credentials: Vec<CredentialProof>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub policy_options: Option<TxnPolicyData>,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub memos: Vec<Memo>,
}

impl From<TransactionBody> for BuildTransactionBody {
    fn from(value: TransactionBody) -> Self {
        Self {
            no_replay_token: value.no_replay_token,
            operations: value.operations.iter().map(|v| v.into()).collect(),
            credentials: value.credentials,
            policy_options: value.policy_options,
            memos: value.memos,
        }
    }
}

impl From<BuildTransactionBody> for TransactionBody {
    fn from(value: BuildTransactionBody) -> Self {
        Self {
            no_replay_token: value.no_replay_token,
            operations: value.operations.iter().map(|v| v.into()).collect(),
            credentials: value.credentials,
            policy_options: value.policy_options,
            memos: value.memos,
        }
    }
}

impl BuildTransactionBody {
    #[inline(always)]
    fn from_token(no_replay_token: NoReplayToken) -> Self {
        let mut result = Self::default();
        result.no_replay_token = no_replay_token;
        result
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct BuildTransaction {
    pub body: BuildTransactionBody,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_default")]
    pub signatures: Vec<SignatureOf<TransactionBody>>,
    #[serde(default)]
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub pubkey_sign_map: HashMap<XfrPublicKey, SignatureOf<TransactionBody>>,
}

impl From<Transaction> for BuildTransaction {
    fn from(value: Transaction) -> Self {
        Self {
            body: value.body.into(),
            signatures: value.signatures,
            pubkey_sign_map: value.pubkey_sign_map,
        }
    }
}

impl From<BuildTransaction> for Transaction {
    fn from(value: BuildTransaction) -> Self {
        Self {
            body: value.body.into(),
            signatures: value.signatures,
            pubkey_sign_map: value.pubkey_sign_map,
        }
    }
}

impl BuildTransaction {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn is_coinbase_tx(&self) -> bool {
        self.body
            .operations
            .iter()
            .any(|o| matches!(o, BuildOperation::MintFra(_)))
    }

    // /// All-in-one checker
    // #[inline(always)]
    // pub fn valid_in_abci(&self) -> bool {
    //     self.check_fee() && !self.is_coinbase_tx()
    // }

    /// A simple fee checker
    ///
    /// The check logic is as follows:
    /// - Only `NonConfidential Operation` can be used as fee
    /// - FRA code == [0; ASSET_TYPE_LENGTH]
    /// - Fee destination == BLACK_HOLE_PUBKEY
    /// - A transaction with an `Operation` of defining/issuing FRA need NOT fee
    /// - A transaction with all addresses of inputs equal to BLACK_HOLE_PUBKEY need NOT fee
    pub fn check_fee(&self) -> bool {
        // This method can not completely solve the DOS risk,
        // we should further limit the number of txo[s] in every operation.
        //
        // But it seems enough when we combine it with limiting
        // the payload size of submission-server's http-requests.
        self.is_coinbase_tx()
            || self.body.operations.iter().any(|ops| {
                if let BuildOperation::TransferAsset(ref x) = ops {
                    return x.body.outputs.iter().any(|o| {
                        if let XfrAssetType::NonConfidential(ty) = o.record.asset_type {
                            if ty == ASSET_TYPE_FRA
                                && *BLACK_HOLE_PUBKEY == o.record.public_key
                            {
                                if let XfrAmount::NonConfidential(am) = o.record.amount {
                                    if am > (TX_FEE_MIN - 1) {
                                        return true;
                                    }
                                }
                            }
                        }
                        false
                    });
                } else if let BuildOperation::DefineAsset(ref x) = ops {
                    if x.body.asset.code.val == ASSET_TYPE_FRA {
                        return true;
                    }
                } else if let BuildOperation::IssueAsset(ref x) = ops {
                    if x.body.code.val == ASSET_TYPE_FRA {
                        return true;
                    }
                } else if matches!(ops, BuildOperation::UpdateValidator(_)) {
                    return true;
                }
                false
            })
    }

    /// findora hash
    #[inline(always)]
    pub fn hash(&self, id: TxnSID) -> HashOf<(TxnSID, Transaction)> {
        HashOf::new(&(id, self.clone().into()))
    }

    // /// tendermint hash
    // #[inline(always)]
    // pub fn hash_tm(&self) -> HashOf<Transaction> {
    //     HashOf::new(self)
    // }

    // #[inline(always)]
    // #[allow(missing_docs)]
    // pub fn hash_tm_rawbytes(&self) -> Vec<u8> {
    //     self.hash_tm().0.hash.as_ref().to_vec()
    // }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn handle(&self) -> String {
        let digest = self.hash(TxnSID(0));
        hex::encode(digest)
    }

    /// Create a transaction from seq id
    #[inline(always)]
    pub fn from_seq_id(seq_id: u64) -> Self {
        let mut prng = ChaChaRng::from_entropy();
        let no_replay_token = NoReplayToken::new(&mut prng, seq_id);
        Self {
            body: BuildTransactionBody::from_token(no_replay_token),
            signatures: Vec::new(),
            pubkey_sign_map: Default::default(),
        }
    }

    /// Create a transaction from a operation
    // #[inline(always)]
    // pub fn from_operation(op: Operation, seq_id: u64) -> Self {
    //     let mut tx = Transaction::from_seq_id(seq_id);
    //     tx.add_operation(op);
    //     tx
    // }

    // /// Create a transaction from coinbase operation
    // #[inline(always)]
    // pub fn from_operation_coinbase_mint(op: Operation, seq_id: u64) -> Self {
    //     let mut tx = Transaction {
    //         body: TransactionBody::from_token(NoReplayToken::unsafe_new(
    //             seq_id.saturating_add(1357).saturating_mul(89),
    //             seq_id,
    //         )),
    //         signatures: Vec::new(),
    //         pubkey_sign_map: Default::default(),
    //     };
    //     tx.add_operation(op);
    //     tx
    // }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn add_operation(&mut self, mut op: BuildOperation) {
        set_no_replay_token(&mut op, self.body.no_replay_token);
        self.body.operations.push(op);
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn sign(&mut self, keypair: &XfrKeyPair) {
        self.signatures
            .push(SignatureOf::new(keypair, &self.body.clone().into()));
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn sign_to_map(&mut self, keypair: &XfrKeyPair) {
        self.pubkey_sign_map.insert(
            keypair.pub_key,
            SignatureOf::new(keypair, &self.body.clone().into()),
        );
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn check_signature(
        &self,
        public_key: &XfrPublicKey,
        sig: &SignatureOf<TransactionBody>,
    ) -> Result<()> {
        sig.verify(public_key, &self.body.clone().into()).c(d!())
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_owner_memos_ref(&self) -> Vec<Option<&OwnerMemo>> {
        let mut memos = Vec::new();
        for op in self.body.operations.iter() {
            match op {
                BuildOperation::TransferAsset(xfr_asset) => {
                    memos.append(&mut xfr_asset.get_owner_memos_ref());
                }
                BuildOperation::MintFra(mint_asset) => {
                    memos.append(&mut mint_asset.get_owner_memos_ref());
                }
                BuildOperation::IssueAsset(issue_asset) => {
                    memos.append(&mut issue_asset.get_owner_memos_ref());
                }
                _ => {}
            }
        }
        memos
    }

    /// Returns the outputs of a transaction. Internally spent outputs can be optionally included.
    /// This will never panic on a well formed transaction, but may panic on a malformed one.
    #[inline(always)]
    pub fn get_outputs_ref(&self, include_spent: bool) -> Vec<TxOutput> {
        let eff = TxnEffect::compute_effect(self.clone().into()).unwrap();
        if !include_spent {
            eff.txos.into_iter().flatten().collect()
        } else {
            let mut spent = eff.internally_spent_txos.into_iter();
            let mut ret = Vec::new();
            for txo in eff.txos.into_iter() {
                if let Some(txo) = txo {
                    ret.push(txo);
                } else {
                    ret.push(spent.next().unwrap());
                }
            }
            ret
        }
    }

    // /// NOTE: this does *not* guarantee that a private key affiliated with
    // /// `public_key` has signed this transaction! If `public_key` is derived
    // /// from `self` somehow, then it is infeasible for someone to forge a
    // /// passing signature, but it is plausible for someone to generate an
    // /// unrelated `public_key` which can pass this signature check!
    // #[inline(always)]
    // pub fn check_has_signature(&self, public_key: &XfrPublicKey) -> Result<()> {
    //     let serialized = Serialized::new(&self.body);
    //     for sig in self.signatures.iter() {
    //         match sig.0.verify(public_key, &serialized) {
    //             Err(_) => {}
    //             Ok(_) => {
    //                 return Ok(());
    //             }
    //         }
    //     }
    //     Err(eg!())
    // }

    // #[inline(always)]
    // #[allow(missing_docs)]
    // pub fn check_has_signature_from_map(&self, public_key: &XfrPublicKey) -> Result<()> {
    //     if let Some(sign) = self.pubkey_sign_map.get(public_key) {
    //         sign.0.verify(public_key, &Serialized::new(&self.body))
    //     } else {
    //         Err(eg!(
    //             "the pubkey not match: {}",
    //             public_key_to_base64(public_key)
    //         ))
    //     }
    // }

    // /// NOTE: This method is used to verify the signature in the transaction,
    // /// when the user constructs the transaction not only needs to sign each `operation`,
    // /// but also needs to sign the whole transaction, otherwise it will not be passed here
    // #[inline(always)]
    // pub fn check_tx(&self) -> Result<()> {
    //     let select_check = |tx: &Transaction, pk: &XfrPublicKey| -> Result<()> {
    //         if tx.signatures.is_empty() {
    //             tx.check_has_signature_from_map(pk)
    //         } else {
    //             tx.check_has_signature(pk)
    //         }
    //     };

    //     for operation in self.body.operations.iter() {
    //         match operation {
    //             Operation::TransferAsset(o) => {
    //                 for pk in o.get_owner_addresses().iter() {
    //                     select_check(self, pk).c(d!())?;
    //                 }
    //             }
    //             Operation::IssueAsset(o) => {
    //                 select_check(self, &o.pubkey.key).c(d!())?;
    //             }
    //             Operation::DefineAsset(o) => {
    //                 select_check(self, &o.pubkey.key).c(d!())?;
    //             }
    //             Operation::UpdateMemo(o) => {
    //                 select_check(self, &o.pubkey).c(d!())?;
    //             }
    //             Operation::UpdateStaker(o) => {
    //                 select_check(self, &o.pubkey).c(d!())?;
    //             }
    //             Operation::Delegation(o) => {
    //                 select_check(self, &o.pubkey).c(d!())?;
    //             }
    //             Operation::UnDelegation(o) => {
    //                 select_check(self, &o.pubkey).c(d!())?;
    //             }
    //             Operation::Claim(o) => {
    //                 select_check(self, &o.pubkey).c(d!())?;
    //             }
    //             Operation::UpdateValidator(_) => {}
    //             Operation::Governance(_) => {}
    //             Operation::FraDistribution(_) => {}
    //             Operation::MintFra(_) => {}
    //             Operation::ConvertAccount(o) => {
    //                 select_check(self, &o.signer).c(d!())?;
    //             }
    //             Operation::ReplaceStaker(o) => {
    //                 if !o.get_related_pubkeys().is_empty() {
    //                     for pk in o.get_related_pubkeys() {
    //                         select_check(self, &pk).c(Ok!())?;
    //                     }
    //                 }
    //             }
    //         }
    //     }

    //     d(())
    // }
}
#[allow(missing_docs)]
pub fn set_no_replay_token(op: &mut BuildOperation, no_replay_token: NoReplayToken) {
    match op {
        BuildOperation::UpdateStaker(i) => {
            i.set_nonce(no_replay_token);
        }
        BuildOperation::Delegation(i) => {
            i.set_nonce(no_replay_token);
        }
        BuildOperation::UnDelegation(i) => {
            i.set_nonce(no_replay_token);
        }
        BuildOperation::Claim(i) => {
            i.set_nonce(no_replay_token);
        }
        BuildOperation::FraDistribution(i) => {
            i.set_nonce(no_replay_token);
        }
        BuildOperation::UpdateValidator(i) => {
            i.set_nonce(no_replay_token);
        }
        BuildOperation::Governance(i) => {
            i.set_nonce(no_replay_token);
        }
        BuildOperation::UpdateMemo(i) => i.body.no_replay_token = no_replay_token,
        BuildOperation::ConvertAccount(i) => i.set_nonce(no_replay_token),
        _ => {}
    }
}
