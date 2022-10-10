//! Ethereum module integration tests.

#![allow(clippy::field_reassign_with_default)]

use abci::*;
use baseapp::{BaseApp, ChainId};
use ethereum_types::{H160, U256};
use fp_mocks::*;
use fp_traits::{account::AccountAsset, evm::FeeCalculator};
use fp_types::{
    actions::{ethereum::Action as EthereumAction, Action},
    assemble::UncheckedTransaction,
};
use fp_utils::tx::EvmRawTxWrapper;

#[test]
fn run_all_tests() {
    test_abci_check_tx();
    test_abci_begin_block();
    test_abci_deliver_tx();
    test_abci_end_block();
    test_abci_commit();
    test_abci_query()
}

fn base_transfer_fee() -> U256 {
    U256::from(21000).saturating_mul(
        <BaseApp as module_ethereum::Config>::FeeCalculator::min_gas_price(
            100_0000_0000_u64,
        ),
    )
}

fn build_transfer_transaction(
    to: H160,
    balance: U256,
    nonce: U256,
) -> UncheckedTransaction<()> {
    let tx = UnsignedTransaction {
        nonce,
        gas_price: <BaseApp as module_ethereum::Config>::FeeCalculator::min_gas_price(
            100_0000_0000_u64,
        ),
        gas_limit: U256::from(0x100000),
        action: ethereum::TransactionAction::Call(to),
        value: balance,
        input: Vec::new(),
    };

    let raw_tx = tx.sign(&ALICE_ECDSA.private_key, ChainId::get());
    let function = Action::Ethereum(EthereumAction::Transact(raw_tx));
    UncheckedTransaction::new_unsigned(function)
}

fn test_abci_check_tx() {
    let mut req = RequestCheckTx::default();
    let tx = serde_json::to_vec(&build_transfer_transaction(
        BOB_ECDSA.address,
        10.into(),
        U256::from(0),
    ))
    .unwrap();
    req.tx = EvmRawTxWrapper::wrap(&tx);
    let resp = BASE_APP.lock().unwrap().check_tx(&req);
    assert!(
        resp.code == 1 && resp.log.contains("InsufficientBalance"),
        "resp log: {}",
        resp.log
    );

    test_mint_balance(
        &ALICE_ECDSA.account_id,
        100_0000_0000_0000_0000_u64.into(),
        2,
    );

    let tx2 = serde_json::to_vec(&build_transfer_transaction(
        BOB_ECDSA.address,
        10.into(),
        U256::from(0),
    ))
    .unwrap();
    req.tx = EvmRawTxWrapper::wrap(&tx2);
    let resp = BASE_APP.lock().unwrap().check_tx(&req);
    assert_eq!(
        resp.code, 0,
        "check tx failed, code: {}, log: {}",
        resp.code, resp.log
    );
}

fn test_abci_begin_block() {
    let mut req = RequestBeginBlock::default();
    req.hash = b"test".to_vec();
    let mut header = Header::default();
    header.height = 3;
    req.set_header(header);
    let _ = BASE_APP.lock().unwrap().begin_block(&req);
}

fn test_abci_deliver_tx() {
    let mut req = RequestDeliverTx::default();
    let tx = serde_json::to_vec(&build_transfer_transaction(
        BOB_ECDSA.address,
        10.into(),
        U256::from(0),
    ))
    .unwrap();
    req.tx = EvmRawTxWrapper::wrap(&tx);
    let resp = BASE_APP.lock().unwrap().deliver_tx(&req);
    assert_eq!(
        resp.code, 0,
        "deliver tx failed, code: {}, log: {}",
        resp.code, resp.log
    );

    println!("transfer resp: {:?}", resp);

    // initial balance = 100_0000_0000_0000_0000_u64, gas limit = 21000, transfer balance = 10
    assert_eq!(
        module_account::App::<BaseApp>::balance(
            &BASE_APP.lock().unwrap().deliver_state,
            &ALICE_ECDSA.account_id
        ),
        U256::from(100_0000_0000_0000_0000_u64)
            .saturating_sub(base_transfer_fee())
            .saturating_sub(10.into())
    );

    assert_eq!(
        module_account::App::<BaseApp>::balance(
            &BASE_APP.lock().unwrap().deliver_state,
            &BOB_ECDSA.account_id
        ),
        10.into()
    );
}

fn test_abci_end_block() {
    let mut req = RequestEndBlock::default();
    req.height = 3;
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
        3
    );
}

fn test_abci_query() {
    let ctx = BASE_APP
        .lock()
        .unwrap()
        .create_query_context(Some(3), false)
        .unwrap();
    assert_eq!(
        module_account::App::<BaseApp>::balance(&ctx, &ALICE_ECDSA.account_id),
        U256::from(100_0000_0000_0000_0000_u64)
            .saturating_sub(base_transfer_fee())
            .saturating_sub(10.into())
    );

    assert_eq!(
        module_account::App::<BaseApp>::balance(&ctx, &BOB_ECDSA.account_id),
        10.into()
    );
}
