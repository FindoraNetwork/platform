use crate::*;
use ethereum_types::H160;
use fp_mocks::*;

//Test from eth-pairings (eip1962) repository https://github.com/FindoraNetwork/eip1962
#[test]
fn test_g1_mul() {
    use hex;
    let hex_string = "02820d8080001026e1318f230000000000000080be6dc885e544bee65620747e191023316695af6989f85b230000000000044eccc6886286fbaee7561d483d50d89e9e9e95af2989f85b230000000000044eccc688628631";
    let data = hex::decode(hex_string).unwrap();

    assert!(EthPairing::execute(
        &EvmDataWriter::new()
            .write_selector(Call::ExecuteOperation)
            .write_raw_bytes(&data)
            .build(),
        None,
        &evm::Context {
            address: H160::from_low_u64_be(2001),
            caller: ALICE_ECDSA.address,
            apparent_value: From::from(0),
        },
        &BASE_APP.lock().unwrap().deliver_state,
    )
    .is_ok());
}
