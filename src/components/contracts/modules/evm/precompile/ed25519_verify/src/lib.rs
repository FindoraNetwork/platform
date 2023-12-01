use evm::executor::stack::{PrecompileFailure, PrecompileOutput};
use evm::{Context, ExitError, ExitSucceed};
use module_evm::precompile::{FinState, Precompile, PrecompileId, PrecompileResult};
use zei::{
    serialization::ZeiFromToBytes,
    xfr::sig::{XfrPublicKey, XfrSignature},
};

pub struct Ed25519Verify;

impl Ed25519Verify {
    const GAS_COST: u64 = 50000; // https://eips.ethereum.org/EIPS/eip-1108
}

impl PrecompileId for Ed25519Verify {
    fn contract_id() -> u64 {
        0x2003
    }
}

impl Precompile for Ed25519Verify {
    fn execute(
        input: &[u8],
        _target_gas: Option<u64>,
        _context: &Context,
        _state: &FinState,
    ) -> PrecompileResult {
        if input.len() < 128 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::Other("input must contain 128 bytes".into()),
            });
        };
        let pk = &input[0..32];
        let msg = &input[32..64];
        let sig = &input[64..128];
        let pub_key =
            XfrPublicKey::zei_from_bytes(pk).map_err(|_| PrecompileFailure::Error {
                exit_status: ExitError::Other("Public key recover failed".into()),
            })?;
        let sig =
            XfrSignature::zei_from_bytes(sig).map_err(|_| PrecompileFailure::Error {
                exit_status: ExitError::Other("Signature recover failed".into()),
            })?;

        let mut buf = [0u8; 4];
        if pub_key.verify(msg, &sig).is_ok() {
            buf[3] = 0u8;
        } else {
            buf[3] = 1u8;
        };

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: Self::GAS_COST,
            output: buf.to_vec(),
            logs: Default::default(),
        })
    }
}
