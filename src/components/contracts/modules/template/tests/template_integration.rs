#![allow(clippy::field_reassign_with_default)]

//! Template module integration tests.
use abci::*;
use fp_mocks::*;
use fp_traits::{account::AccountAsset, evm::FeeCalculator};
use fp_types::{actions::template::Action as TemplateAction, actions::Action, U256};
use fp_utils::tx::EvmRawTxWrapper;
use module_template::ValueStore;
pub use std::borrow::Borrow;
use std::convert::TryInto;

#[test]
fn run_all_tests() {
    test_abci_info();
    test_abci_init_chain();
    test_mint_balance(
        &ALICE_XFR.pub_key.into(),
        100_0000_0000_0000_0000_u64.into(),
        0,
    );
    test_abci_check_tx();
    test_abci_begin_block();
    test_abci_deliver_tx();
    test_abci_end_block();
    test_abci_commit();
    test_abci_query();
    test_abci_check_tx_with_bad_nonce();
}

fn test_abci_info() {
    let resp = BASE_APP.lock().unwrap().info(&RequestInfo::default());
    assert_eq!(resp.data, "findora".to_string());
    assert_eq!(resp.version, "1.0.0".to_string());
    assert_eq!(resp.app_version, 1);
    assert_eq!(resp.last_block_height, 0);
}

fn test_abci_init_chain() {
    let mut req = RequestInitChain::default();
    req.chain_id = "findora test".to_string();
    let _ = BASE_APP.lock().unwrap().init_chain(&req);

    assert_eq!(
        &BASE_APP
            .lock()
            .unwrap()
            .deliver_state
            .block_header()
            .chain_id,
        "findora test"
    );
    assert_eq!(
        BASE_APP.lock().unwrap().deliver_state.block_header().height,
        0
    );
}

fn test_abci_check_tx() {
    let mut req = RequestCheckTx::default();

    let function = Action::Template(TemplateAction::SetValue(10));
    let tx = serde_json::to_vec(&build_signed_transaction(
        function,
        &ALICE_XFR,
        U256::zero(),
    ))
    .unwrap();
    req.tx = EvmRawTxWrapper::wrap(&tx);
    let resp = BASE_APP.lock().unwrap().check_tx(&req);
    assert_eq!(
        resp.code, 0,
        "check tx failed, code: {}, log: {}",
        resp.code, resp.log
    );

    // check balance
    // fees are not deducted since tx is not simulated
    assert_eq!(
        module_account::App::<BaseApp>::balance(
            &BASE_APP.lock().unwrap().check_state,
            &ALICE_XFR.pub_key.into()
        ),
        U256::from(100_0000_0000_0000_0000_u64)
    );
}

fn test_abci_begin_block() {
    let mut req = RequestBeginBlock::default();
    req.hash = b"test".to_vec();
    let mut header = Header::default();
    header.height = 1;
    req.set_header(header.clone());
    let _ = BASE_APP.lock().unwrap().begin_block(&req);
    let _ = BASE_APP.lock().unwrap().commit(&RequestCommit::new());
    header.height = 2;
    req.set_header(header);
    let _ = BASE_APP.lock().unwrap().begin_block(&req);
}

fn test_abci_deliver_tx() {
    let mut req = RequestDeliverTx::default();
    let function = Action::Template(TemplateAction::SetValue(10));
    let tx = serde_json::to_vec(&build_signed_transaction(
        function,
        &ALICE_XFR,
        U256::zero(),
    ))
    .unwrap();
    req.tx = EvmRawTxWrapper::wrap(&tx);
    let resp = BASE_APP.lock().unwrap().deliver_tx(&req);
    assert_eq!(
        resp.code, 0,
        "deliver tx failed, code: {}, log: {}",
        resp.code, resp.log
    );

    assert_eq!(u64::from_be_bytes(resp.data.try_into().unwrap()), 10);

    assert_eq!(
        ValueStore::get(BASE_APP.lock().unwrap().deliver_state.state.read().borrow()),
        Some(10)
    );
}

fn test_abci_end_block() {
    let mut req = RequestEndBlock::default();
    req.height = 2;
    let _ = BASE_APP.lock().unwrap().end_block(&req);
}

fn test_abci_commit() {
    let _ = BASE_APP.lock().unwrap().commit(&RequestCommit::new());
    assert_eq!(
        BASE_APP
            .lock()
            .unwrap()
            .chain_state
            .read()
            .height()
            .unwrap(),
        2
    );

    // check balance = initial - fee
    assert_eq!(
        module_account::App::<BaseApp>::balance(
            &BASE_APP.lock().unwrap().deliver_state,
            &ALICE_XFR.pub_key.into()
        ),
        U256::from(100_0000_0000_0000_0000_u64).saturating_sub(
            <BaseApp as module_account::Config>::FeeCalculator::min_gas_price(
                BASE_APP.lock().unwrap().deliver_state.header.height as u64,
            )
        )
    );
}

fn test_abci_query() {
    let mut req = RequestQuery::default();
    req.path = String::from("module/template/value");
    let resp = BASE_APP.lock().unwrap().query(&req);

    assert_eq!(
        resp.code, 0,
        "query tx failed, code: {}, log: {}",
        resp.code, resp.log
    );

    assert_eq!(
        serde_json::from_slice::<u64>(resp.value.as_slice()).unwrap(),
        10
    );
}

fn test_abci_check_tx_with_bad_nonce() {
    let mut req = RequestCheckTx::default();

    let function = Action::Template(TemplateAction::SetValue(10));
    let tx = serde_json::to_vec(&build_signed_transaction(
        function,
        &ALICE_XFR,
        U256::zero(),
    ))
    .unwrap();
    req.tx = EvmRawTxWrapper::wrap(&tx);
    let resp = BASE_APP.lock().unwrap().check_tx(&req);
    assert!(
        resp.code == 1 && resp.log.contains("InvalidNonce, expected: 1, actual: 0"),
        "resp log: {}",
        resp.log
    );
}
