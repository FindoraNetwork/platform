use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
  AccountAddress, AssetTypeCode, IssuerPublicKey, Operation, TransferType, TxoRef,
};
use ledger::store::LedgerState;
use ledger::store::*;
use rand_chacha::ChaChaRng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use txn_builder::{BuildsTransactions, TransactionBuilder, TransferOperationBuilder};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::asset_record::open_asset_record;
use zei::xfr::lib::verify_xfr_body;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey};

#[test]
fn test_create_asset() -> Result<(), PlatformError> {
  let mut prng = ChaChaRng::from_seed([0u8; 32]);
  let mut state = LedgerState::test_ledger();
  let code = AssetTypeCode { val: [1; 16] };
  let keys = XfrKeyPair::generate(&mut prng);
  let keys_copy = XfrKeyPair::zei_from_bytes(&keys.zei_to_bytes());
  let bob_keys = XfrKeyPair::generate(&mut prng);
  let mut builder = TransactionBuilder::default();
  let issuer_pub_key = IssuerPublicKey { key: *keys.get_pk_ref() };

  // Define
  let tx = builder.add_operation_create_asset(&issuer_pub_key,
                                              keys.get_sk_ref(),
                                              Some(code),
                                              false,
                                              false,
                                              "test".into())?
                  .transaction();
  let effect = TxnEffect::compute_effect(&mut state.get_prng(), tx.clone()).unwrap();
  {
    let mut block = state.start_block().unwrap();
    state.apply_transaction(&mut block, effect).unwrap();
    state.finish_block(block);
  }
  assert!(state.get_asset_type(&code).is_some());

  // Issue
  let mut builder = TransactionBuilder::default();
  let tx =
    builder.add_basic_issue_asset(&issuer_pub_key, keys.get_sk_ref(), &None, &code, 0, 1000)?
           .add_basic_issue_asset(&issuer_pub_key, keys.get_sk_ref(), &None, &code, 1, 1000)?
           .transaction();
  let effect = TxnEffect::compute_effect(&mut state.get_prng(), tx.clone()).unwrap();

  let mut block = state.start_block().unwrap();
  let temp_sid = state.apply_transaction(&mut block, effect).unwrap();
  let (txn_sid, txos) = state.finish_block(block).remove(&temp_sid).unwrap();

  // Basic transfer
  let bar1 = ((state.get_utxo(txos[0]).unwrap().0).0).clone();
  let bar2 = ((state.get_utxo(txos[0]).unwrap().0).0).clone();
  let oar1 = open_asset_record(&bar1, keys.get_sk_ref()).unwrap();
  let oar2 = open_asset_record(&bar1, keys.get_sk_ref()).unwrap();
  let op = TransferOperationBuilder::new().add_input(TxoRef::Absolute(txos[0]), oar1, 1000)?
                                          .add_input(TxoRef::Absolute(txos[1]), oar2, 1000)?
                                          .add_output(2000, keys.get_pk_ref(), code)?
                                          .create(TransferType::Standard)?
                                          .sign(&keys)?
                                          .transaction()?;
  let mut builder = TransactionBuilder::default();
  let tx = builder.add_operation(op).transaction();
  let effect = TxnEffect::compute_effect(&mut state.get_prng(), tx.clone()).unwrap();
  {
    let mut block = state.start_block().unwrap();
    state.apply_transaction(&mut block, effect).unwrap();
    state.finish_block(block);
  }

  Ok(())
}
