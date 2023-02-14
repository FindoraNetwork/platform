use crate::*;
use ethereum_types::H160;
use fp_mocks::*;

// Test from eth-pairings (eip1962) repository https://github.com/FindoraNetwork/eip1962
#[test]
fn test_bls12_pairing() {
    use hex;
    let hex_string = "07202912811758d871b77a9c3635c28570dc020576f9fc2719d8d0494439162b2b89000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011603e65409b693c8a08aeb3478d10aa3732a6672ba06d12912811758d871b77a9c3635c28570dc020576f9fc2719d8d0494439162b2b84000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010206059efde42f3701020127ccb2831f0011c1d547c491b9dffbd4cfdb87598a4b370f5873ea62094a2f201faa6cb7f6fcca66de3f308a25776dac3f61edb792948fbe53e4d18a3d8aefbe011a7e9f75f3fdc77b83a97f7acd58326a0545f8aa37b69bfb32c52dc195763e8c17176e0ad4ee94d9c720e922d42688127c4b812cd7c2f8cf6126acd4c3d7568121e48b3fefe66c279f2ec71f0d6f8156a3343d1cfa54b808d747cd02419278290ad2d7d03f5de1e7b3c97732f53dbe1dfd42e51f9571f7fee3d9c1785d5a1ed6010b4f7f211a0a5f4425728e2df580196d3e3b85ef148ed769acd23e9be6e8440726cb40655787f48eaf46154cb740e2a58db5b96fa02d83fb9d0f94320da1471e0104ece4c46ac4f05a7c28ecda84292f15999747bb77c530c65448f1f837a47dd70e972c4065d0b39d40b5d550a55901516afa7f02b395963d1535fcba1705e31a117cb4beab1dc582198c4ab0c02e96a22f7bd10dde3bbbdbc9182a9596cb0ed32121616b692e8036437efb4c3816f018f11e643c6e0a049da431986a3a722b06";
    let data = hex::decode(hex_string).unwrap();

    let output = EthPairing::execute(
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
    );

    assert!(output.is_ok());
    assert_eq!(output.as_ref().unwrap().cost, 164986);
    assert_eq!(output.unwrap().output, vec![0x1]);
}
