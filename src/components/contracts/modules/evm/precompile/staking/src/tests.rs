use crate::*;
use baseapp::BaseApp;
use fp_mocks::*;

use evm_precompile_utils::EvmDataWriter;
use fp_utils::hashing::keccak_256;
use module_evm::precompile::Precompile;
use zei_algebra::serialization::ZeiFromToBytes;

#[test]
fn test_fra_pubkey() {
    let evm_from_bytes = keccak_256(ALICE_XFR.pub_key.as_bytes());
    let evm_from_addr = H160::from_slice(&evm_from_bytes[..20]);

    test_insert_address_mapping(&evm_from_addr, &ALICE_XFR.pub_key);

    let resp = Staking::<BaseApp>::execute(
        &EvmDataWriter::new()
            .write_selector(Call::FraPubkey)
            .write(Address(evm_from_addr))
            .build(),
        None,
        &evm::Context {
            address: Default::default(),
            caller: evm_from_addr,
            apparent_value: From::from(0),
        },
        &BASE_APP.lock().unwrap().deliver_state,
    )
    .unwrap();

    assert_eq!(
        resp,
        PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: 0,
            output: EvmDataWriter::new()
                .write(ALICE_XFR.pub_key.zei_to_bytes())
                .build(),
            logs: Default::default(),
        }
    )
}
