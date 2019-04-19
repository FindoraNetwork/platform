use crate::data_model::{
  AssetCreation, AssetIssuance, AssetPolicyKey, AssetToken, AssetTokenCode, AssetTransfer,
  CustomAssetPolicy, Operation, SmartContract, SmartContractKey, Transaction, TxOutput, TxoSID,
  Utxo,
};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use zei::transfers::verify_xfr_note;
pub mod errors;

pub trait LedgerAccess {
  fn check_utxo(&self, addr: TxoSID) -> Option<Utxo>;
  fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken>;
  fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy>;
  fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract>;
}

pub trait LedgerUpdate {
  fn apply_transaction(&mut self, txn: &Transaction) -> ();
}

pub trait LedgerValidate {
  fn validate_transaction(&mut self, txn: &Transaction) -> bool;
}

pub trait ArchiveUpdate {
  fn append_transaction(&mut self, txn: Transaction) -> ();
}

pub trait ArchiveAccess {
  fn get_transaction(&self, addr: TxoSID) -> Option<Transaction>;
}

pub fn compute_sha256_hash<T>(msg: &T) -> [u8; 32]
  where T: std::convert::AsRef<[u8]>
{
  let mut hasher = Sha256::new();
  hasher.input(msg);
  let result = hasher.result();
  let hash = array_ref![&result, 0, 32];

  *hash
}

#[derive(Default)]
pub struct LedgerState {
  txs: Vec<Transaction>, //will need to be replaced by merkle tree...
  utxos: HashMap<TxoSID, Utxo>,
  contracts: HashMap<SmartContractKey, SmartContract>,
  policies: HashMap<AssetPolicyKey, CustomAssetPolicy>,
  tokens: HashMap<AssetTokenCode, AssetToken>,
  issuance_num: HashMap<AssetTokenCode, u64>,
  max_index: TxoSID,
}

impl LedgerState {
  fn add_txo(&mut self, txo: (&TxoSID, TxOutput)) {
    let utxo_addr = *txo.0;
    let utxo_ref = Utxo { digest: compute_sha256_hash(&serde_json::to_vec(&txo.1).unwrap()),
                          output: txo.1 };

    self.utxos.insert(utxo_addr, utxo_ref);
    self.max_index = *txo.0;
  }

  fn apply_asset_transfer(&mut self, transfer: &AssetTransfer) {
    for utxo in &transfer.body.inputs {
      self.utxos.remove(&utxo);
    }

    //
    for out in
      transfer.body.outputs.iter().zip(transfer.body
                                               .transfer
                                               .outputs_iter()
                                               .map(|o| TxOutput::BlindAssetRecord(o.clone())))
    {
      self.add_txo(out);
    }
  }

  fn apply_asset_issuance(&mut self, issue: &AssetIssuance) {
    for out in issue.body
                    .outputs
                    .iter()
                    .zip(issue.body.records.iter().map(|ref o| (*o).clone()))
    {
      self.add_txo(out);
    }
    //TODO Change updating AssetToken to work with Zei Output types
    // match &out.asset_type {
    //    AssetType::Normal(a) => {
    //        if let Some(token) = self.tokens.get_mut(&a.code) {
    //            token.units += a.amount;
    //        }
    //        else {
    //         println!("Normal Asset Issuance has failed!")
    //        }
    //        //TODO: We should never have the if statement above fail, but should we write something if it does
    //    }
    //    //TODO: Implement Private Asset Issuance
    //    AssetType::Private(_) => println!("Private Issuance Not Implemented!"),
    // }
  }

  fn apply_asset_creation(&mut self, create: &AssetCreation) {
    let token: AssetToken = AssetToken { properties: create.body.asset.clone(),
                                         ..Default::default() };
    self.tokens.insert(token.properties.code, token);
  }

  fn apply_operation(&mut self, op: &Operation) {
    match op {
      Operation::AssetTransfer(transfer) => self.apply_asset_transfer(transfer),
      Operation::AssetIssuance(issuance) => self.apply_asset_issuance(issuance),
      Operation::AssetCreation(creation) => self.apply_asset_creation(creation),
    }
  }

  // Asset Transfer is valid if UTXOs exist on ledger and match zei transaction, zei transaction is valid, and if additional signatures are valid
  fn validate_asset_transfer(&mut self, transfer: &AssetTransfer) -> bool {
    // [1] signatures are valid
    for signature in &transfer.body_signatures {
      if !signature.verify(&serde_json::to_vec(&transfer.body).unwrap()) {
        return false;
      }
    }

    // [2] utxos exist on ledger - need to match zei transaction
    for utxo_addr in &transfer.body.inputs {
      if self.check_utxo(*utxo_addr).is_none() {
        return false;
      }

      if verify_xfr_note(&transfer.body.transfer).is_err() {
        return false;
      }
    }

    true
  }

  // Asset Issuance is Valid if Signature is valid, the operation is unique, and the assets in the TxOutputs are owned by the signatory
  fn validate_asset_issuance(&mut self, issue: &AssetIssuance) -> bool {
    //[1] token has been created
    if !self.tokens.contains_key(&issue.body.code)
       || !self.issuance_num.contains_key(&issue.body.code)
    {
      return false;
    }

    let token = &self.tokens[&issue.body.code];
    let last_issuance_num = &self.issuance_num[&issue.body.code];

    //[2] replay attack - not issued before=====
    if issue.body.seq_num <= *last_issuance_num {
      return false;
    }

    //[3] signature is correct on body
    if issue.pubkey
            .key
            .verify(&serde_json::to_vec(&issue.body).unwrap(), &issue.signature)
            .is_err()
    {
      return false;
    }

    //[4] signature belongs to anchor??
    if !(token.properties.issuer == issue.pubkey) {
      return false;
    }

    true
  }

  // Asset Creation is invalid if the signature is not valid or the code is already used by a different asset
  fn validate_asset_creation(&mut self, create: &AssetCreation) -> bool {
    //[1] the token is not already created, [2] the signature is correct.
    !self.tokens.contains_key(&create.body.asset.code)
    && create.pubkey
             .key
             .verify(&serde_json::to_vec(&create.body).unwrap(),
                     &create.signature)
             .is_ok()
  }

  fn validate_operation(&mut self, op: &Operation) -> bool {
    match op {
      Operation::AssetTransfer(transfer) => self.validate_asset_transfer(transfer),
      Operation::AssetIssuance(issuance) => self.validate_asset_issuance(issuance),
      Operation::AssetCreation(creation) => self.validate_asset_creation(creation),
    }
  }
}

impl LedgerUpdate for LedgerState {
  fn apply_transaction(&mut self, txn: &Transaction) {
    // Apply the operations
    for op in &txn.operations {
      self.apply_operation(op);
    }
  }
}

impl ArchiveUpdate for LedgerState {
  fn append_transaction(&mut self, txn: Transaction) {
    self.txs.push(txn);
  }
}

impl LedgerValidate for LedgerState {
  fn validate_transaction(&mut self, txn: &Transaction) -> bool {
    for op in &txn.operations {
      if !self.validate_operation(op) {
        return false;
      }
    }
    true
  }
}

impl LedgerAccess for LedgerState {
  fn check_utxo(&self, addr: TxoSID) -> Option<Utxo> {
    match self.utxos.get(&addr) {
      Some(utxo) => Some(utxo.clone()),
      None => None,
    }
  }

  fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken> {
    match self.tokens.get(code) {
      Some(token) => Some(token.clone()),
      None => None,
    }
  }

  fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy> {
    match self.policies.get(key) {
      Some(policy) => Some(policy.clone()),
      None => None,
    }
  }

  fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract> {
    match self.contracts.get(key) {
      Some(contract) => Some(contract.clone()),
      None => None,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::data_model::{
    Asset, AssetCreationBody, AssetIssuanceBody, ConfidentialMemo, IssuerPublicKey, Memo,
  };
  use rand::{CryptoRng, Rng, SeedableRng};
  use rand_chacha::ChaChaRng;
  use zei::basic_crypto::signatures::{XfrKeyPair, XfrPublicKey, XfrSecretKey, XfrSignature};

  fn build_keys<R: CryptoRng + Rng>(prng: &mut R) -> (XfrPublicKey, XfrSecretKey) {
    let keypair = XfrKeyPair::generate(prng);

    (keypair.get_pk_ref().clone(), keypair.get_sk())
  }

  fn compute_signature<T>(secret_key: &XfrSecretKey,
                          public_key: &XfrPublicKey,
                          asset_body: &T)
                          -> XfrSignature
    where T: serde::Serialize
  {
    secret_key.sign(&serde_json::to_vec(&asset_body).unwrap(), &public_key)
  }

  fn asset_creation_body(token_code: &AssetTokenCode,
                         asset_type: &String,
                         issuer_key: &XfrPublicKey,
                         updatable: bool,
                         memo: &Option<Memo>,
                         confidential_memo: &Option<ConfidentialMemo>)
                         -> AssetCreationBody {
    let mut token_properties: Asset = Default::default();
    token_properties.code = token_code.clone();
    token_properties.issuer = IssuerPublicKey { key: issuer_key.clone() };
    token_properties.updatable = updatable;
    token_properties.asset_type = asset_type.clone();

    if memo.is_some() {
      token_properties.memo = memo.as_ref().unwrap().clone();
    } else {
      token_properties.memo = Memo {};
    }

    if confidential_memo.is_some() {
      token_properties.confidential_memo = confidential_memo.as_ref().unwrap().clone();
    } else {
      token_properties.confidential_memo = ConfidentialMemo {};
    }

    AssetCreationBody { asset: token_properties,
                        outputs: Vec::new() }
  }

  fn asset_creation_operation(asset_body: &AssetCreationBody,
                              public_key: &XfrPublicKey,
                              secret_key: &XfrSecretKey)
                              -> AssetCreation {
    let sign = compute_signature(&secret_key, &public_key, &asset_body);
    AssetCreation { body: asset_body.clone(),
                    pubkey: IssuerPublicKey { key: public_key.clone() },
                    signature: sign }
  }

  #[test]
  fn test_asset_creation_valid() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::default();
    let mut tx = Transaction::create_empty();

    let token_code1 = AssetTokenCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);
    let asset_type = String::from("token1");

    let asset_body =
      asset_creation_body(&token_code1, &asset_type, &public_key, true, &None, &None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::AssetCreation(asset_create));

    assert_eq!(true, state.validate_transaction(&tx));

    state.apply_transaction(&tx);
    state.append_transaction(tx);
    assert_eq!(true, state.get_asset_token(&token_code1).is_some());

    assert_eq!(asset_body.asset,
               state.get_asset_token(&token_code1).unwrap().properties);

    assert_eq!(0, state.get_asset_token(&token_code1).unwrap().units);
  }

  //update signature to have wrong public key
  #[test]
  fn test_asset_creation_invalid_public_key() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::default();
    let mut tx = Transaction::create_empty();

    let token_code1 = AssetTokenCode { val: [1; 16] };
    let (public_key1, secret_key1) = build_keys(&mut prng);
    let asset_type = String::from("token1");;


    let asset_body =
      asset_creation_body(&token_code1, &asset_type, &public_key1, true, &None, &None);
    let mut asset_create = asset_creation_operation(&asset_body, &public_key1, &secret_key1);

    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let (public_key2, _secret_key2) = build_keys(&mut prng);
    asset_create.pubkey.key = public_key2;

    tx.operations.push(Operation::AssetCreation(asset_create));

    assert_eq!(false, state.validate_transaction(&tx));
  }

  //update signature to sign with different key
  #[test]
  fn test_asset_creation_invalid_signature() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::default();
    let mut tx = Transaction::create_empty();

    let token_code1 = AssetTokenCode { val: [1; 16] };
    let (public_key1, secret_key1) = build_keys(&mut prng);
    let asset_type = String::from("token1");;

    let asset_body =
      asset_creation_body(&token_code1, &asset_type, &public_key1, true, &None, &None);
    let mut asset_create = asset_creation_operation(&asset_body, &public_key1, &secret_key1);

    //update signature to have wrong public key]
    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let (public_key2, _secret_key2) = build_keys(&mut prng);
    asset_create.pubkey.key = public_key2;

    tx.operations.push(Operation::AssetCreation(asset_create));

    assert_eq!(false, state.validate_transaction(&tx));
  }

  #[test]
  fn asset_issued() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::default();
    let mut tx = Transaction::create_empty();

    let token_code1 = AssetTokenCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);
    let asset_type = String::from("token1");

    let asset_body =
      asset_creation_body(&token_code1, &asset_type, &public_key, true, &None, &None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::AssetCreation(asset_create));

    assert_eq!(true, state.validate_transaction(&tx));

    state.apply_transaction(tx);

    let mut tx = Transaction::create_empty();

    let asset_issuance_body = AssetIssuanceBody { seq_num: 0,
                                                  code: token_code1,
                                                  outputs: Vec::new(),
                                                  records: Vec::new() };

    let sign = compute_signature(&secret_key, &public_key, &asset_issuance_body);

    let asset_issuance_operation = AssetIssuance { body: asset_issuance_body,
                                                   pubkey: IssuerPublicKey { key:
                                                                               public_key.clone() },
                                                   signature: sign };

    let issue_op = Operation::AssetIssuance(asset_issuance_operation);

    tx.operations.push(issue_op);

    assert_eq!(true, state.validate_transaction(&tx));
    state.apply_transaction(&tx);
    state.append_transaction(tx);

    // Update units as would be done once asset is issued
    assert_eq!(100, state.get_asset_token(&token_code1).unwrap().units);
    // let utxo_loc = TxoSID {
    //     transaction_id: TxSequenceNumber { val: 0 },
    //     operation_index: 1,
    //     output_index: 0,
    // };
    // assert_eq!(true, state.check_utxo(&utxo_loc).is_some());
    // assert_eq!(issued.address, state.check_utxo(&utxo_loc).unwrap().address);
    // assert_eq!(issued.asset, state.check_utxo(&utxo_loc).unwrap().asset);
  }

  // #[test]
  // fn asset_transferred() {
  //     let mut state = LedgerState::new();
  //     let token_code1 = AssetTokenCode { val: [1; 16] };
  //     let mut token_properties: AssetTokenProperties = Default::default();
  //     token_properties.code = token_code1;

  //     let mut tx = Transaction::create_empty();
  //     let create_asset = AssetCreation {
  //         properties: token_properties,
  //         signature: [0; 32],
  //     };

  //     let create_op = Operation::AssetCreation(create_asset);
  //     tx.operations.push(create_op);
  //     let issued = TxOutput {
  //         address: Address { key: [5; 32] },
  //         asset: AssetType::Normal(Asset {
  //             code: AssetTokenCode { val: [1; 16] },
  //             amount: 100,
  //         }),
  //     };
  //     let issue_op = Operation::AssetIssuance(AssetIssuance {
  //         nonce: 0,
  //         code: token_code1,
  //         outputs: vec![issued.clone()],
  //         signature: [0; 32], //Empty signature
  //     });
  //     tx.operations.push(issue_op);
  //     //let issue_op = O
  //     state.apply_transaction(&tx);
  //     // Update units as would be done once asset is issued
  //     let utxo_loc = TxoSID {
  //         transaction_id: TxSequenceNumber { val: 0 },
  //         operation_index: 1,
  //         output_index: 0,
  //     };
  //     assert_eq!(true, state.check_utxo(&utxo_loc).is_some());
  //     assert_eq!(issued.address, state.check_utxo(&utxo_loc).unwrap().address);
  //     assert_eq!(issued.asset, state.check_utxo(&utxo_loc).unwrap().asset);

  //     let mut tx2 = Transaction::create_empty();
  //     let transfer_to = TxOutput {
  //         address: Address { key: [7; 32] },
  //         asset: AssetType::Normal(Asset {
  //             code: AssetTokenCode { val: [1; 16] },
  //             amount: 100,
  //         }),
  //     };
  //     let transfer_op = Operation::AssetTransfer(AssetTransfer {
  //         nonce: 0,
  //         variables: Vec::new(),
  //         confidential_asset_flag: false,
  //         confidential_amount_flag: false,
  //         input_utxos: vec![state.check_utxo(&utxo_loc).unwrap()],
  //         outputs: vec![transfer_to.clone()],
  //         signatures: vec![[0; 32]],
  //     });
  //     tx2.operations.push(transfer_op);
  //     state.apply_transaction(&tx2);
  //     assert_eq!(true, state.check_utxo(&utxo_loc).is_none());
  //     let utxo_loc = TxoSID {
  //         transaction_id: TxSequenceNumber { val: 1 },
  //         operation_index: 0,
  //         output_index: 0,
  //     };
  //     assert_eq!(true, state.check_utxo(&utxo_loc).is_some());
  //     assert_eq!(
  //         transfer_to.address,
  //         state.check_utxo(&utxo_loc).unwrap().address
  //     );
  //     assert_eq!(
  //         transfer_to.asset,
  //         state.check_utxo(&utxo_loc).unwrap().asset
  //     );
  // }
}
