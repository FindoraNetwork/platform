#![allow(missing_docs)]

use super::*;
use actix_service::Service;
use actix_web::{test, web, App};
use futures::executor;
use ledger::{
    data_model::{AssetRules, Operation, Transaction, TxnEffect},
    store::{helpers::*, LedgerState},
};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;

#[test]
fn test_query_asset() {
    let mut prng = ChaChaRng::from_entropy();
    let mut state = LedgerState::tmp_ledger();
    let (_, seq_id) = state.get_state_commitment();
    let mut tx = Transaction::from_seq_id(seq_id);

    let token_code1 = AssetTypeCode::gen_random();
    let keypair = build_keys(&mut prng);

    let asset_body = asset_creation_body(
        &token_code1,
        keypair.get_pk_ref(),
        AssetRules::default(),
        None,
        None,
    );
    let asset_create = asset_creation_operation(&asset_body, &keypair);
    tx.body
        .operations
        .push(Operation::DefineAsset(asset_create));

    let effect = TxnEffect::compute_effect(tx).unwrap();
    {
        let mut block = state.start_block().unwrap();
        state.apply_transaction(&mut block, effect, false).unwrap();
        state.finish_block(block).unwrap();
    }

    let mut app = executor::block_on(test::init_service(
        App::new()
            .data(Arc::new(RwLock::new(state)))
            .route("/asset_token/{token}", web::get().to(query_asset)),
    ));

    let req = test::TestRequest::get()
        .uri(&format!("/asset_token/{}", token_code1.to_base64()))
        .to_request();
    let resp = executor::block_on(app.call(req)).unwrap();

    assert!(resp.status().is_success());
}
