use crate::{
  indent_of, serialize_or_str, AssetTypeEntry, AssetTypeName, OpMetadata, TxnBuilderEntry,
  TxnMetadata, TxoCacheEntry, TxoName,
};
use ledger::data_model::{AssetTypeCode, Transaction};
use std::collections::BTreeMap;

pub fn display_op_metadata(indent_level: u64, ent: &OpMetadata) {
  let ind = indent_of(indent_level);
  match ent {
    OpMetadata::DefineAsset { asset_nick,
                              issuer_nick, } => {
      println!("{}DefineAsset `{}`", ind, asset_nick.0);
      println!("{} issued by `{}`", ind, issuer_nick.0);
    }
    OpMetadata::IssueAsset { issuer_nick,
                             asset_nick,
                             output_name,
                             output_amt,
                             issue_seq_num, } => {
      println!("{}IssueAsset {} of `{}`", ind, output_amt, asset_nick.0);
      println!("{} issued to `{}` as issuance #{} named `{}`",
               ind, issuer_nick.0, issue_seq_num, output_name);
    }
    OpMetadata::TransferAssets { inputs, outputs } => {
      println!("{}TransferAssets:", ind);
      println!("{} Inputs:", ind);
      display_txos(indent_level + 1, &inputs, &None);
      println!("{} Outputs:", ind);
      display_txos(indent_level + 1, &outputs, &None);
    }
  }
}

pub fn display_asset_type_defs(indent_level: u64, ent: &BTreeMap<AssetTypeName, AssetTypeEntry>) {
  let ind = indent_of(indent_level);
  for (nick, asset_ent) in ent.iter() {
    println!("{}{}:", ind, nick.0);
    display_asset_type(indent_level + 1, asset_ent);
  }
}

pub fn display_operations(indent_level: u64, operations: &[OpMetadata]) {
  for op in operations.iter() {
    display_op_metadata(indent_level, op);
  }
}

pub fn display_txo_entry(indent_level: u64, txo: &TxoCacheEntry) {
  let ind = indent_of(indent_level);
  println!("{}sid: {}", ind, serialize_or_str(&txo.sid, "<UNKNOWN>"));
  println!("{}Owned by: {}{}",
           ind,
           serde_json::to_string(&txo.record.0.public_key).unwrap(),
           if let Some(o) = txo.owner.as_ref() {
             format!(" ({})", o.0)
           } else {
             "".to_string()
           });
  println!("{}Record Type: {}",
           ind,
           serde_json::to_string(&txo.record.0.get_record_type()).unwrap());
  println!("{}Amount: {}",
           ind,
           txo.record
              .0
              .amount
              .get_amount()
              .map(|x| format!("{}", x))
              .unwrap_or_else(|| "<SECRET>".to_string()));
  println!("{}Type: {}{}",
           ind,
           txo.record
              .0
              .asset_type
              .get_asset_type()
              .map(|x| AssetTypeCode { val: x }.to_base64())
              .unwrap_or_else(|| "<SECRET>".to_string()),
           if let Some(o) = txo.asset_type.as_ref() {
             format!(" ({})", o.0)
           } else {
             "".to_string()
           });
  if let Some(open_ar) = txo.opened_record.as_ref() {
    println!("{}Decrypted Amount: {}", ind, open_ar.amount);
    println!("{}Decrypted Type: {}",
             ind,
             AssetTypeCode { val: open_ar.asset_type }.to_base64());
  }
  println!("{}Spent? {}",
           ind,
           if txo.unspent { "Unspent" } else { "Spent" });
  println!("{}Have owner memo? {}",
           ind,
           if txo.owner_memo.is_some() {
             "Yes"
           } else {
             "No"
           });
}

pub fn display_txos(indent_level: u64,
                    txos: &[(String, TxoCacheEntry)],
                    finalized_names: &Option<Vec<TxoName>>) {
  let ind = indent_of(indent_level);
  for (i, (nick, txo)) in txos.iter().enumerate() {
    let fin = finalized_names.as_ref()
                             .and_then(|x| x.get(i))
                             .map(|x| x.0.to_string())
                             .unwrap_or_else(|| "Not finalized".to_string());
    println!("{}{} ({}):", ind, nick, fin);
    display_txo_entry(indent_level + 1, txo);
  }
}

pub fn display_txn_builder(indent_level: u64, ent: &TxnBuilderEntry) {
  let ind = indent_of(indent_level);
  println!("{}Operations:", ind);
  display_operations(indent_level + 1, &ent.operations);

  println!("{}New asset types defined:", ind);
  display_asset_type_defs(indent_level + 1, &ent.new_asset_types);

  println!("{}New asset records:", ind);
  display_txos(indent_level + 1, &ent.new_txos, &None);

  println!("{}Signers:", ind);
  for nick in ent.signers.iter() {
    println!("{} - `{}`", ind, nick.0);
  }

  print!("{}Consuming TXOs:", ind);
  for n in ent.spent_txos.iter() {
    print!(" {}", n.0);
  }
  println!();
}

pub fn display_txn(indent_level: u64, ent: &(Transaction, TxnMetadata)) {
  let ind = indent_of(indent_level);
  println!("{}seq_id: {}", ind, ent.0.seq_id);
  println!("{}Handle: {}",
           ind,
           serialize_or_str(&ent.1.handle, "<UNKNOWN>"));
  println!("{}Status: {}",
           ind,
           serialize_or_str(&ent.1.status, "<UNKNOWN>"));
  println!("{}Operations:", ind);
  display_operations(indent_level + 1, &ent.1.operations);

  println!("{}New asset types defined:", ind);
  for (nick, asset_ent) in ent.1.new_asset_types.iter() {
    println!("{} {}:", ind, nick.0);
    display_asset_type(indent_level + 2, asset_ent);
  }

  println!("{}New asset records:", ind);
  display_txos(indent_level + 1, &ent.1.new_txos, &ent.1.finalized_txos);

  println!("{}Signers:", ind);
  for nick in ent.1.signers.iter() {
    println!("{} - `{}`", ind, nick.0);
  }

  print!("{}Consuming TXOs:", ind);
  for n in ent.1.spent_txos.iter() {
    print!(" {}", n.0);
  }
  println!();
}

pub fn display_asset_type(indent_level: u64, ent: &AssetTypeEntry) {
  let ind = indent_of(indent_level);
  println!("{}issuer nickname: {}",
           ind,
           ent.issuer_nick
              .as_ref()
              .map(|x| x.0.clone())
              .unwrap_or_else(|| "<UNKNOWN>".to_string()));
  println!("{}issuer public key: {}",
           ind,
           serde_json::to_string(&ent.asset.issuer.key).unwrap()); // TODO Philippe is this unwrap safe?
  println!("{}code: {}", ind, ent.asset.code.to_base64());
  println!("{}memo: `{}`", ind, ent.asset.memo.0);
  println!("{}issue_seq_number: {}", ind, ent.issue_seq_num);
}
