#[cfg(test)]
mod tests;

use config::abci::global_cfg::CFG;
use ethereum_types::{H160, U256};
use evm::{
    executor::stack::{PrecompileFailure, PrecompileOutput},
    Context, ExitSucceed,
};
use evm_precompile_utils::{
    error, AsVec, Bytes, EvmDataReader2, EvmDataWriter2, EvmResult, Gasometer, ToBytes,
};
use module_evm::precompile::{FinState, Precompile, PrecompileId, PrecompileResult};
use num::Zero;
use rand::thread_rng;
// use slices::u8_slice;
use std::vec;
use tracing::debug;

// zkcard support
use ark_ec::{
    models::short_weierstrass_jacobian::GroupAffine,
    short_weierstrass_jacobian::GroupProjective,
};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use ark_std::One;
use barnett_smart_card_protocol::{
    discrete_log_cards::{
        Card, DLCards, MaskedCard, Parameters, PublicKey, RevealToken,
    },
    BarnettSmartProtocol, Reveal,
};
use proof_essentials::{
    homomorphic_encryption::el_gamal::{ElGamal, Plaintext},
    vector_commitment::pedersen::PedersenCommitment,
    zkp::{
        arguments::shuffle::proof::Proof as ProofA,
        proofs::{
            chaum_pedersen_dl_equality::proof::Proof as ProofB,
            schnorr_identification::proof::Proof as ProofC,
        },
    },
};
use starknet_curve::{Projective, StarkwareParameters};

type CCurve = Projective;
type CCardProtocol<'a> = DLCards<'a, CCurve>;
type CCardParameters = Parameters<CCurve>;
type CMaskedCard = MaskedCard<CCurve>;
type CRevealToken = RevealToken<CCurve>;
type CPublicKey = PublicKey<CCurve>;
type CAggregatePublicKey = GroupAffine<StarkwareParameters>;
type CScalar = starknet_curve::Fr;
type CZKProofShuffle = ProofA<CScalar, ElGamal<CCurve>, PedersenCommitment<CCurve>>;
type CRevealProof = ProofB<CCurve>;
type CProof = ProofC<CCurve>;
type CCard = Card<CCurve>;


// The gas used value is obtained according to the standard erc20 call.
// https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v4.3.2/contracts/token/ERC20/ERC20.sol
const GAS_VERIFYKEYOWNERSHIP: u64 = 1000;
const GAS_COMPUTEAGGREGATEKEY: u64 = 1000;
const GAS_VERIFYSHUFFLE: u64 = 1000;
const GAS_VERIFYREVEAL: u64 = 1000;
const GAS_REVEAL: u64 = 1000;
const GAS_MASK: u64 = 1000;
const GAS_TEST: u64 = 1000;

pub struct ZkCard;

impl PrecompileId for ZkCard {
    fn contract_id() -> u64 {
        0x3000
    }
}

#[evm_precompile_utils::generate_function_selector]
#[derive(Debug, PartialEq, Eq, num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
pub enum Call {
    VerifyKeyOwnership = "verifyKeyOwnership(bytes,bytes,bytes,bytes)",
    VerifyReveal = "verifyReveal(bytes,bytes,bytes,bytes,bytes)",
    ComputeAggregateKey = "computeAggregateKey(bytes[])",
    VerifyShuffle = "verifyShuffle(bytes,bytes,bytes[],bytes[],bytes)",
    Reveal = "reveal(bytes[],bytes)",
    Mask = "mask(bytes,bytes,bytes)",
    Test = "test(bytes,bytes[])",
}

impl Precompile for ZkCard {
    fn execute(
        input: &[u8],
        target_gas: Option<u64>,
        context: &Context,
        state: &FinState,
    ) -> PrecompileResult {
        if CFG.checkpoint.disable_delegate_zkcard < state.header.height {
            let addr = context.address;
            if addr != H160::from_low_u64_be(Self::contract_id()) {
                return Err(PrecompileFailure::Error {
                    exit_status: error("No delegatecall support"),
                });
            }
        }

        let mut input = EvmDataReader2::new(input);

        let selector = match input.read_selector::<Call>() {
            Ok(v) => v,
            Err(e) => {
                return Err(PrecompileFailure::Error { exit_status: e });
            }
        };

        match &selector {
            Call::VerifyKeyOwnership => {
                match Self::verify_key_ownership(input, target_gas) {
                    Ok(v) => Ok(v),
                    Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
                }
            }
            Call::ComputeAggregateKey => {
                match Self::compute_aggregate_key(input, target_gas) {
                    Ok(v) => Ok(v),
                    Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
                }
            }
            Call::VerifyShuffle => match Self::verify_shuffle(input, target_gas) {
                Ok(v) => Ok(v),
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::VerifyReveal => match Self::verify_reveal(input, target_gas) {
                Ok(v) => Ok(v),
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::Reveal => match Self::reveal(input, target_gas) {
                Ok(v) => Ok(v),
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::Mask => match Self::mask(input, target_gas) {
                Ok(v) => Ok(v),
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::Test => match Self::test(input, target_gas) {
                Ok(v) => Ok(v),
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
        }
    }
}

impl ZkCard {
    fn test(
        mut input: EvmDataReader2,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        debug!(target: "evm", "ZkCard#name: Findora");

        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_TEST)?;

        input.expect_arguments(2)?;
        let _: U256 = input.read()?;

        let res: Bytes = b"call test".to_bytes();

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter2::new().write(res).build(),
            logs: vec![],
        })
    }

    fn verify_key_ownership(
        mut input: EvmDataReader2,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        debug!(target: "evm", "ZkCard#name: Findora");

        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_VERIFYKEYOWNERSHIP)?;

        input.expect_arguments(4)?;

        let params = input.read::<Bytes>()?;
        let pub_key = input.read::<Bytes>()?;
        let memo = input.read::<Bytes>()?;
        let key_proof = input.read::<Bytes>()?;

        let params: CCardParameters =
            match CCardParameters::deserialize(params.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("params error: {:?}", e))),
            };
        let pub_key: CPublicKey = match CPublicKey::deserialize(pub_key.as_slice()) {
            Ok(v) => v,
            Err(e) => return Err(error(format!("pub_key error: {:?}", e))),
        };
        let memo: Vec<u8> = match Vec::<u8>::deserialize(memo.as_slice()) {
            Ok(v) => v,
            Err(e) => return Err(error(format!("memo error: {:?}", e))),
        };
        let key_proof: CProof = match CProof::deserialize(key_proof.as_slice()) {
            Ok(v) => v,
            Err(e) => return Err(error(format!("key_proof error: {:?}", e))),
        };
        let res = match CCardProtocol::verify_key_ownership(
            &params, &pub_key, &memo, &key_proof,
        ) {
            Ok(_) => true,
            Err(_) => false,
        };

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter2::new().write(res).build(),
            logs: vec![],
        })
    }

    fn compute_aggregate_key(
        mut input: EvmDataReader2,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_COMPUTEAGGREGATEKEY)?;

        input.expect_arguments(1)?;

        let pub_keys = input.read::<Vec<Bytes>>()?;

        let mut aggregate_pub_key = CPublicKey::zero();
        for v_pub_key in pub_keys {
            let v_pub_key: CPublicKey =
                match CanonicalDeserialize::deserialize(v_pub_key.as_slice()) {
                    Ok(v) => v,
                    Err(e) => return Err(error(format!("pub_keys error: {:?}", e))),
                };
            aggregate_pub_key = aggregate_pub_key + v_pub_key;
        }

        let mut res = Vec::with_capacity(aggregate_pub_key.serialized_size() + 100);
        match aggregate_pub_key.serialize(&mut res) {
            Ok(v) => v,
            Err(e) => return Err(error(format!("serialize error: {:?}", e))),
        };

        let res: Bytes = res.to_bytes();

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter2::new().write(res).build(),
            logs: vec![],
        })
    }

    fn verify_shuffle(
        mut input: EvmDataReader2,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_VERIFYSHUFFLE)?;

        input.expect_arguments(5)?;

        let params = input.read::<Bytes>()?;
        let shared_key = input.read::<Bytes>()?;
        let cur_decks = input.read::<Vec<Bytes>>()?;
        let new_decks = input.read::<Vec<Bytes>>()?;
        let shuffle_proof = input.read::<Bytes>()?;

        let params: CCardParameters =
            match CanonicalDeserialize::deserialize(params.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("params error: {:?}", e))),
            };
        let shared_key: CAggregatePublicKey =
            match CanonicalDeserialize::deserialize(shared_key.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("shared_key error: {:?}", e))),
            };
        let mut cur_decks2: Vec<CMaskedCard> = Vec::new();
        for v_cur_deck in cur_decks {
            let v_cur_deck: CMaskedCard =
                match CanonicalDeserialize::deserialize(v_cur_deck.as_slice()) {
                    Ok(v) => v,
                    Err(e) => return Err(error(format!("cur_deck error: {:?}", e))),
                };
            cur_decks2.push(v_cur_deck);
        }
        let mut new_decks2: Vec<CMaskedCard> = Vec::new();
        for v_new_deck in new_decks {
            let v_new_deck: CMaskedCard =
                match CanonicalDeserialize::deserialize(v_new_deck.as_slice()) {
                    Ok(v) => v,
                    Err(e) => return Err(error(format!("new_deck error: {:?}", e))),
                };
            new_decks2.push(v_new_deck);
        }
        let shuffle_proof: CZKProofShuffle =
            match CanonicalDeserialize::deserialize(shuffle_proof.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("shuffle_proof error: {:?}", e))),
            };

        let res = match CCardProtocol::verify_shuffle(
            &params,
            &shared_key,
            &cur_decks2,
            &new_decks2,
            &shuffle_proof,
        ) {
            Ok(_) => true,
            Err(_) => false,
        };

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter2::new().write(res).build(),
            logs: vec![],
        })
    }

    fn verify_reveal(
        mut input: EvmDataReader2,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_VERIFYREVEAL)?;

        input.expect_arguments(5)?;

        let params = input.read::<Bytes>()?;
        let pub_key = input.read::<Bytes>()?;
        let reveal_token = input.read::<Bytes>()?;
        let masked = input.read::<Bytes>()?;
        let reveal_proof = input.read::<Bytes>()?;

        let params: CCardParameters =
            match CanonicalDeserialize::deserialize(params.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("params error: {:?}", e))),
            };
        let pub_key: CPublicKey =
            match CanonicalDeserialize::deserialize(pub_key.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("pub_key error: {:?}", e))),
            };
        let reveal_token: CRevealToken =
            match CanonicalDeserialize::deserialize(reveal_token.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("reveal_token error: {:?}", e))),
            };
        let masked: CMaskedCard =
            match CanonicalDeserialize::deserialize(masked.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("masked error: {:?}", e))),
            };
        let reveal_proof: CRevealProof =
            match CanonicalDeserialize::deserialize(reveal_proof.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("reveal_proof error: {:?}", e))),
            };

        let res = match CCardProtocol::verify_reveal(
            &params,
            &pub_key,
            &reveal_token,
            &masked,
            &reveal_proof,
        ) {
            Ok(_) => true,
            Err(_) => false,
        };

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter2::new().write(res).build(),
            logs: vec![],
        })
    }

    fn reveal(
        mut input: EvmDataReader2,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_REVEAL)?;

        input.expect_arguments(2)?;

        let reveal_tokens = input.read::<Vec<Bytes>>()?;
        let masked = input.read::<Bytes>()?;

        let mut aggregate_reveal_token = CRevealToken::zero();

        for reveal_token in reveal_tokens {
            let reveal_token: Plaintext<GroupProjective<StarkwareParameters>> =
                match CanonicalDeserialize::deserialize(reveal_token.as_slice()) {
                    Ok(v) => v,
                    Err(e) => {
                        return Err(error(format!("reveal_tokens error: {:?}", e)))
                    }
                };
            aggregate_reveal_token = aggregate_reveal_token + reveal_token;
        }
        let masked: CMaskedCard =
            match CanonicalDeserialize::deserialize(masked.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("masked error: {:?}", e))),
            };

        let decrypted = match aggregate_reveal_token.reveal(&masked) {
            Ok(v) => v,
            Err(e) => return Err(error(format!("reveal error: {:?}", e))),
        };

        let mut res = Vec::with_capacity(decrypted.serialized_size() + 100);
        match decrypted.serialize(&mut res) {
            Ok(v) => v,
            Err(e) => return Err(error(format!("serialize error: {:?}", e))),
        };

        let res: Bytes = res.to_bytes();

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter2::new().write(res).build(),
            logs: vec![],
        })
    }

    fn mask(
        mut input: EvmDataReader2,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_MASK)?;

        input.expect_arguments(3)?;

        let params = input.read::<Bytes>()?;
        let shared_key = input.read::<Bytes>()?;
        let encoded = input.read::<Bytes>()?;

        let params: CCardParameters =
            match CanonicalDeserialize::deserialize(params.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("params error: {:?}", e))),
            };
        let shared_key: CPublicKey =
            match CanonicalDeserialize::deserialize(shared_key.as_slice()) {
                Ok(v) => v,
                Err(e) => return Err(error(format!("pub_key error: {:?}", e))),
            };
        let encoded: CCard = match CanonicalDeserialize::deserialize(encoded.as_slice())
        {
            Ok(v) => v,
            Err(e) => return Err(error(format!("key_proof error: {:?}", e))),
        };
        let rng = &mut thread_rng();

        let mask_res = match CCardProtocol::mask(
            rng,
            &params,
            &shared_key,
            &encoded,
            &CScalar::one(),
        ) {
            Ok(v) => v,
            Err(e) => return Err(error(format!("reveal error: {:?}", e))),
        };

        let mut res = Vec::with_capacity(mask_res.serialized_size() + 100);
        match mask_res.serialize(&mut res) {
            Ok(v) => v,
            Err(e) => return Err(error(format!("serialize error: {:?}", e))),
        };

        let res: Bytes = res.to_bytes();

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter2::new().write(res).build(),
            logs: vec![],
        })
    }
}
