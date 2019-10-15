extern crate ledger;
extern crate serde;
extern crate zei;
#[macro_use]
extern crate serde_derive;

use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  AccountAddress, DefineAsset, DefineAssetBody, IssueAsset, IssueAssetBody,
  AssetTokenCode, TransferAsset, TransferAssetBody, ConfidentialMemo, IssuerPublicKey, Memo,
  Operation, Transaction, TxOutput, TxoSID,
};
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use zei::basic_crypto::signatures::XfrSecretKey;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record};
use zei::xfr::structs::{AssetRecord, BlindAssetRecord, OpenAssetRecord};

pub trait BuildsTransactions {
  fn transaction(&self) -> &Transaction;
  fn add_operation_create_asset(&mut self,
                                pub_key: &IssuerPublicKey,
                                priv_key: &XfrSecretKey,
                                token_code: Option<AssetTokenCode>,
                                updatable: bool,
                                traceable: bool,
                                memo: &str,
                                make_confidential: bool)
                                -> Result<(), PlatformError>;
  fn add_operation_issue_asset(&mut self,
                               pub_key: &IssuerPublicKey,
                               priv_key: &XfrSecretKey,
                               token_code: &AssetTokenCode,
                               seq_num: u64,
                               records: &[TxOutput])
                               -> Result<(), PlatformError>;
  fn add_operation_transfer_asset(&mut self,
                                  input_sids: Vec<TxoSID>,
                                  input_records: &[OpenAssetRecord],
                                  output_records: &[AssetRecord])
                                  -> Result<(), PlatformError>;
  fn serialize(&self) -> Result<Vec<u8>, PlatformError>;

  fn add_basic_issue_asset(&mut self,
                           pub_key: &IssuerPublicKey,
                           priv_key: &XfrSecretKey,
                           token_code: &AssetTokenCode,
                           seq_num: u64,
                           amount: u64)
                           -> Result<(), PlatformError> {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let params = PublicParams::new();
    let asset_type = [0u8; 16];
    let ar = AssetRecord::new(amount, asset_type, pub_key.key)?;
    let ba = build_blind_asset_record(&mut prng, &params.pc_gens, &ar, false, false, &None);
    self.add_operation_issue_asset(pub_key,
                                   priv_key,
                                   token_code,
                                   seq_num,
                                   &[TxOutput::BlindAssetRecord(ba)])
  }

  fn add_basic_transfer_asset(&mut self,
                              transfer_from: &[(&TxoSID,
                                 &BlindAssetRecord,
                                 u64,
                                 &XfrSecretKey)],
                              transfer_to: &[(u64, &AccountAddress)])
                              -> Result<(), PlatformError> {
    let input_sids: Vec<TxoSID> = transfer_from.iter()
                                               .map(|(ref txo_sid, _, _, _)| *(*txo_sid))
                                               .collect();
    let input_amounts: Vec<u64> = transfer_from.iter()
                                               .map(|(_, _, amount, _)| *amount)
                                               .collect();
    let input_oars: Result<Vec<OpenAssetRecord>, _> =
      transfer_from.iter()
                   .map(|(_, ref ba, _, ref sk)| open_asset_record(&ba, &sk))
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
    self.add_operation_transfer_asset(input_sids, &input_oars, &output_ars)
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
                                token_code: Option<AssetTokenCode>,
                                updatable: bool,
                                traceable: bool,
                                _memo: &str,
                                make_confidential: bool)
                                -> Result<(), PlatformError> {
    let memo;
    let confidential_memo = if make_confidential {
      memo = None;
      Some(ConfidentialMemo {})
    } else {
      memo = Some(Memo {});
      None
    };

    self.txn.add_operation(Operation::DefineAsset(DefineAsset::new(DefineAssetBody::new(&token_code.unwrap_or_else(AssetTokenCode::gen_random), pub_key, updatable, traceable, memo, confidential_memo)?, pub_key, priv_key)?));
    Ok(())
  }
  fn add_operation_issue_asset(&mut self,
                               pub_key: &IssuerPublicKey,
                               priv_key: &XfrSecretKey,
                               token_code: &AssetTokenCode,
                               seq_num: u64,
                               records: &[TxOutput])
                               -> Result<(), PlatformError> {
    let mut outputs = self.txn.outputs;
    self.txn.add_operation(Operation::IssueAsset(IssueAsset::new(IssueAssetBody::new(token_code, seq_num, records, &mut outputs)?, pub_key, priv_key)?));
    self.txn.outputs = outputs;
    Ok(())
  }
  fn add_operation_transfer_asset(&mut self,
                                  input_sids: Vec<TxoSID>,
                                  input_records: &[OpenAssetRecord],
                                  output_records: &[AssetRecord])
                                  -> Result<(), PlatformError> {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([0u8; 32]);
    let input_keys = Vec::new(); // TODO: multisig support...
    let mut outputs = self.txn.outputs;
    self.txn.add_operation(Operation::TransferAsset(TransferAsset::new(TransferAssetBody::new(&mut prng, input_sids, input_records, output_records, &input_keys, &mut outputs)?)?));
    self.txn.outputs = outputs;
    Ok(())
  }
  fn serialize(&self) -> Result<Vec<u8>, PlatformError> {
    let j = serde_json::to_string(&self.txn)?;
    Ok(j.as_bytes().to_vec())
  }
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
