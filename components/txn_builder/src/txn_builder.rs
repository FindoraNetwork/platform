#![deny(warnings)]
extern crate ledger;
extern crate serde;
extern crate zei;
#[macro_use]
extern crate serde_derive;

use ledger::data_model::errors::PlatformError;
use ledger::data_model::*;
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record};
use zei::xfr::sig::{XfrKeyPair, XfrSecretKey};
use zei::xfr::structs::{AssetIssuerPubKeys, AssetRecord, BlindAssetRecord, OpenAssetRecord};

pub trait BuildsTransactions {
  fn transaction(&self) -> &Transaction;
  #[allow(clippy::too_many_arguments)]
  fn add_operation_create_asset(&mut self,
                                pub_key: &IssuerPublicKey,
                                priv_key: &XfrSecretKey,
                                token_code: Option<AssetTypeCode>,
                                updatable: bool,
                                traceable: bool,
                                memo: &str)
                                -> Result<(), PlatformError>;
  fn add_operation_issue_asset(&mut self,
                               pub_key: &IssuerPublicKey,
                               priv_key: &XfrSecretKey,
                               token_code: &AssetTypeCode,
                               seq_num: u64,
                               records: &[TxOutput])
                               -> Result<(), PlatformError>;
  fn add_operation_transfer_asset(&mut self,
                                  input_sids: Vec<TxoRef>,
                                  input_key: &XfrKeyPair,
                                  input_records: &[OpenAssetRecord],
                                  output_records: &[AssetRecord])
                                  -> Result<(), PlatformError>;
  fn serialize(&self) -> Result<Vec<u8>, PlatformError>;
  fn serialize_str(&self) -> Result<String, PlatformError>;

  fn add_basic_issue_asset(&mut self,
                           pub_key: &IssuerPublicKey,
                           priv_key: &XfrSecretKey,
                           tracking_keys: &Option<AssetIssuerPubKeys>,
                           token_code: &AssetTypeCode,
                           seq_num: u64,
                           amount: u64)
                           -> Result<(), PlatformError> {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let params = PublicParams::new();
    let ar = AssetRecord::new(amount, token_code.val, pub_key.key)?;
    let ba = build_blind_asset_record(&mut prng, &params.pc_gens, &ar, true, true, tracking_keys);
    self.add_operation_issue_asset(pub_key, priv_key, token_code, seq_num, &[TxOutput(ba)])
  }

  #[allow(clippy::comparison_chain)]
  fn add_basic_transfer_asset(&mut self,
                              key_pair: &XfrKeyPair,
                              transfer_from: &[(&TxoRef, &BlindAssetRecord, u64)],
                              transfer_to: &[(u64, &AccountAddress)])
                              -> Result<(), PlatformError> {
    let input_sids: Vec<TxoRef> = transfer_from.iter()
                                               .map(|(ref txo_sid, _, _)| *(*txo_sid))
                                               .collect();
    let input_amounts: Vec<u64> = transfer_from.iter().map(|(_, _, amount)| *amount).collect();
    let input_oars: Result<Vec<OpenAssetRecord>, _> =
      transfer_from.iter()
                   .map(|(_, ref ba, _)| open_asset_record(&ba, &key_pair.get_sk_ref()))
                   .collect();
    let input_oars = input_oars?;
    let input_total: u64 = input_amounts.iter().sum();
    let mut partially_consumed_inputs = Vec::new();
    for (input_amount, oar) in input_amounts.iter().zip(input_oars.iter()) {
      if input_amount > oar.get_amount() {
        return Err(PlatformError::InputsError);
      } else if input_amount < oar.get_amount() {
        let ar = AssetRecord::new(oar.get_amount() - input_amount,
                                  *oar.get_asset_type(),
                                  *oar.get_pub_key())?;
        partially_consumed_inputs.push(ar);
      }
    }
    let output_total = transfer_to.iter().fold(0, |acc, (amount, _)| acc + amount);
    if input_total != output_total {
      return Err(PlatformError::InputsError);
    }
    let asset_type = input_oars[0].get_asset_type();
    let output_ars: Result<Vec<AssetRecord>, _> =
      transfer_to.iter()
                 .map(|(amount, ref addr)| AssetRecord::new(*amount, *asset_type, addr.key))
                 .collect();
    let mut output_ars = output_ars?;
    output_ars.append(&mut partially_consumed_inputs);
    self.add_operation_transfer_asset(input_sids, &key_pair, &input_oars, &output_ars)
  }
}

#[derive(Default, Serialize, Deserialize)]
pub struct TransactionBuilder {
  txn: Transaction,
  outputs: u64,
}

impl BuildsTransactions for TransactionBuilder {
  fn transaction(&self) -> &Transaction {
    &self.txn
  }
  fn add_operation_create_asset(&mut self,
                                pub_key: &IssuerPublicKey,
                                priv_key: &XfrSecretKey,
                                token_code: Option<AssetTypeCode>,
                                updatable: bool,
                                traceable: bool,
                                _memo: &str)
                                -> Result<(), PlatformError> {
    self.txn.add_operation(Operation::DefineAsset(DefineAsset::new(DefineAssetBody::new(&token_code.unwrap_or_else(AssetTypeCode::gen_random), pub_key, updatable, traceable, None, Some(ConfidentialMemo {}))?, pub_key, priv_key)?));
    Ok(())
  }
  fn add_operation_issue_asset(&mut self,
                               pub_key: &IssuerPublicKey,
                               priv_key: &XfrSecretKey,
                               token_code: &AssetTypeCode,
                               seq_num: u64,
                               records: &[TxOutput])
                               -> Result<(), PlatformError> {
    self.txn
        .add_operation(Operation::IssueAsset(IssueAsset::new(IssueAssetBody::new(token_code,
                                                                                 seq_num,
                                                                                 records)?,
                                                             pub_key,
                                                             priv_key)?));
    Ok(())
  }
  fn add_operation_transfer_asset(&mut self,
                                  input_sids: Vec<TxoRef>,
                                  input_key: &XfrKeyPair,
                                  input_records: &[OpenAssetRecord],
                                  output_records: &[AssetRecord])
                                  -> Result<(), PlatformError> {
    // TODO(joe/noah): keep a prng around somewhere?
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([0u8; 32]);

    let input_keys = &[XfrKeyPair::zei_from_bytes(&input_key.zei_to_bytes())];

    self.txn.add_operation(Operation::TransferAsset(TransferAsset::new(TransferAssetBody::new(&mut prng, input_sids, input_records, output_records, input_keys)?, &[input_key], TransferType::Standard)?));
    Ok(())
  }
  fn serialize(&self) -> Result<Vec<u8>, PlatformError> {
    let j = serde_json::to_string(&self.txn)?;
    Ok(j.as_bytes().to_vec())
  }

  fn serialize_str(&self) -> Result<String, PlatformError> {
    if let Ok(serialized) = serde_json::to_string(&self.txn) {
      Ok(serialized)
    } else {
      Err(PlatformError::SerializationError)
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::TxoRef;
  use quickcheck::{Arbitrary, Gen};
  use quickcheck_macros::quickcheck;
  use rand::{Rng, SeedableRng};
  use rand_chacha::ChaChaRng;
  use zei::serialization::ZeiFromToBytes;
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record};
  use zei::xfr::lib::{gen_xfr_note, verify_xfr_note};
  use zei::xfr::sig::XfrKeyPair;
  use zei::xfr::structs::{AssetRecord, OpenAssetRecord};

  // Defines an asset type
  #[derive(Clone, Debug, Eq, PartialEq)]
  struct AssetType(pub u8);

  #[derive(Clone, Debug, Eq, PartialEq)]
  struct KeyPair(pub u8);

  #[derive(Clone, Debug, Eq, PartialEq)]
  struct TxoReference(pub u64);

  // Defines an input record
  // (type, amount, conf_type, conf_amount, traceable)
  #[derive(Clone, Debug, Eq, PartialEq)]
  struct InputRecord(pub u64, pub AssetType, pub bool, pub bool, pub bool);

  // Defines an output record
  // (amount, asset type, keypair)
  #[derive(Clone, Debug, Eq, PartialEq)]
  struct OutputRecord(pub u64, pub AssetType, pub KeyPair);

  impl Arbitrary for OutputRecord {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      OutputRecord(u64::arbitrary(g),
                   AssetType::arbitrary(g),
                   KeyPair::arbitrary(g))
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0
                   .shrink()
                   .zip(self.1.shrink())
                   .zip(self.2.shrink())
                   .map(|((amount, asset_type), key_pair)| {
                     OutputRecord(amount, asset_type, key_pair)
                   }))
    }
  }

  impl Arbitrary for InputRecord {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      InputRecord(u64::arbitrary(g),
                  AssetType::arbitrary(g),
                  bool::arbitrary(g),
                  bool::arbitrary(g),
                  bool::arbitrary(g))
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0
                   .shrink()
                   .zip(self.1.shrink())
                   .zip(self.2.shrink())
                   .zip(self.3.shrink())
                   .zip(self.4.shrink())
                   .map(|((((amount, asset_type), conf_type), conf_amount), traceable)| {
                          InputRecord(amount, asset_type, conf_type, conf_amount, traceable)
                        }))
    }
  }

  impl Arbitrary for AssetType {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      AssetType(u8::arbitrary(g))
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0.shrink().map(AssetType))
    }
  }

  impl Arbitrary for TxoReference {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      TxoReference(g.gen::<u64>() % 10)
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0.shrink().map(TxoReference))
    }
  }

  impl Arbitrary for KeyPair {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      // We can generate 10 possible key pairs
      KeyPair(g.gen::<u8>() % 10)
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
      Box::new(self.0.shrink().map(KeyPair))
    }
  }

  #[quickcheck]
  #[ignore]
  fn test_compose_transfer_txn(inputs: Vec<InputRecord>,
                               outputs: Vec<OutputRecord>,
                               key_pair: KeyPair,
                               input_sids: Vec<TxoReference>) {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let params = PublicParams::new();

    //TODO: noah asset records should be buildable by reference
    let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_seed([key_pair.0; 32]));
    let key_pair_copy = XfrKeyPair::zei_from_bytes(&key_pair.zei_to_bytes());

    // Compose input records
    let input_records: Result<Vec<OpenAssetRecord>, _> =
      inputs.iter()
            .map(|InputRecord(amount, asset_type, conf_type, conf_amount, _)| {
                   let ar = AssetRecord::new(*amount,
                                             [asset_type.0; 16],
                                             *key_pair_copy.get_pk_ref()).unwrap();
                   let ba = build_blind_asset_record(&mut prng,
                                                     &params.pc_gens,
                                                     &ar,
                                                     *conf_type,
                                                     *conf_amount,
                                                     &None);
                   return open_asset_record(&ba, &key_pair.get_sk_ref());
                 })
            .collect();

    // Compose output records
    let output_records: Result<Vec<AssetRecord>, _> =
      outputs.iter()
             .map(|OutputRecord(amount, asset_type, key_pair)| {
               let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_seed([key_pair.0; 32]));
               AssetRecord::new(*amount, [asset_type.0; 16], *key_pair.get_pk_ref())
             })
             .collect();

    let _input_sids: Vec<TxoRef> = input_sids.iter()
                                             .map(|TxoReference(sid)| TxoRef::Relative(*sid))
                                             .collect();
    let id_proofs = vec![];
    let note = gen_xfr_note(&mut prng,
                            &input_records.unwrap(),
                            &output_records.unwrap(),
                            &[key_pair],
                            &id_proofs);
    if let Ok(xfr_note) = note {
      let null_policies = vec![];
      assert!(verify_xfr_note(&mut prng, &xfr_note, &null_policies).is_ok())
    }
  }
}
