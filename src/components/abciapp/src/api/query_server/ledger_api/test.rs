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
#[allow(clippy::type_complexity)]
fn test_query_state_commitment() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::test_ledger();
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

    let state_lock = Arc::new(RwLock::new(state));

    let mut app = executor::block_on(test::init_service(
        App::new()
            .data(state_lock.clone())
            .route("/global_state", web::get().to(query_global_state))
            .route(
                "/global_state_version/{version}",
                web::get().to(query_global_state_version),
            ),
    ));

    let req = test::TestRequest::get().uri("/global_state").to_request();

    let second_req = test::TestRequest::get()
        .uri("/global_state_version/1")
        .to_request();

    let state_reader = state_lock.read();
    let (comm1, idx, _sig): (
        _,
        _,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    ) = executor::block_on(test::read_response_json(&mut app, req));
    let comm2 = executor::block_on(test::read_response_json(&mut app, second_req));
    assert!((comm1, idx) == state_reader.get_state_commitment());
    assert!((comm2, idx) == state_reader.get_state_commitment());
}

#[test]
// Tests that the server
//  (a) responds with the same public key across a transaction
//  (b) responds to /global_state with a response signed by the public
//      key from /public_key
#[allow(clippy::type_complexity)]
fn test_query_public_key() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::test_ledger();
    let (_, seq_id) = state.get_state_commitment();
    let mut tx = Transaction::from_seq_id(seq_id);

    let orig_key = *state.public_key();

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

    let state_lock = Arc::new(RwLock::new(state));

    let mut app = executor::block_on(test::init_service(
        App::new()
            .data(state_lock.clone())
            .route("/global_state", web::get().to(query_global_state))
            .route("/public_key", web::get().to(query_public_key)),
    ));

    let req_pk = test::TestRequest::get().uri("/public_key").to_request();
    let req_comm = test::TestRequest::get().uri("/global_state").to_request();

    let state_reader = state_lock.read();
    let k: XfrPublicKey = executor::block_on(test::read_response_json(&mut app, req_pk));
    let (comm, idx, sig): (
        HashOf<Option<StateCommitmentData>>,
        u64,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    ) = executor::block_on(test::read_response_json(&mut app, req_comm));
    sig.verify(&k, &(comm, idx)).unwrap();
    assert!(k == orig_key);
    assert!(k == *state_reader.public_key());
}

#[test]
fn test_query_asset() {
    let mut prng = ChaChaRng::from_entropy();
    let mut state = LedgerState::test_ledger();
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
