use ethereum_types::{Address, U256};
use platform_lib_noah::{
    noah_algebra::serialization::NoahFromToBytes,
    noah_api::{
        anon_xfr::{
            ar_to_abar::{gen_ar_to_abar_body, verify_ar_to_abar_body, ArToAbarBody},
            structs::{AnonAssetRecord, AxfrOwnerMemo},
            AXfrPlonkPf,
        },
        keys::PublicKey,
        parameters::{AddressFormat, ProverParams, VerifierParams},
        xfr::structs::{
            AssetType, BlindAssetRecord, OpenAssetRecord, XfrAmount, XfrAssetType,
            ASSET_TYPE_LENGTH,
        },
    },
};
use rand_core::{CryptoRng, RngCore};
use ruc::*;
use serde::{Deserialize, Serialize};

/// The AbarToEvmNote
#[derive(Debug, Serialize, Deserialize, Eq, Clone, PartialEq)]
pub struct EvmToAbarNote {
    pub output: AnonAssetRecord,
    pub proof: AXfrPlonkPf,
    pub memo: AxfrOwnerMemo,
}

impl EvmToAbarNote {
    pub fn verify(
        &self,
        params: &VerifierParams,
        source_asset: Address,
        source_amount: U256,
    ) -> Result<()> {
        let mut asset = [0u8; ASSET_TYPE_LENGTH];
        asset[..20].copy_from_slice(source_asset.as_bytes());
        let input = BlindAssetRecord {
            amount: XfrAmount::NonConfidential(source_amount.as_u64()),
            asset_type: XfrAssetType::NonConfidential(AssetType(asset)),
            public_key: PublicKey::default(AddressFormat::SECP256K1), // no effect
        };
        let body = ArToAbarBody {
            input,
            output: self.output.clone(),
            proof: self.proof.clone(),
            memo: self.memo.clone(),
        };
        verify_ar_to_abar_body(params, &body)
    }

    pub fn prove<R: CryptoRng + RngCore>(
        prng: &mut R,
        params: &ProverParams,
        source_asset: Address,
        source_amount: U256,
        target_address: &PublicKey,
    ) -> Result<EvmToAbarNote> {
        let mut asset = [0u8; ASSET_TYPE_LENGTH];
        asset[..20].copy_from_slice(source_asset.as_bytes());
        let obar = OpenAssetRecord {
            amount: source_amount.as_u64(),
            asset_type: AssetType(asset),
            blind_asset_record: BlindAssetRecord {
                amount: XfrAmount::NonConfidential(0),
                asset_type: XfrAssetType::NonConfidential(AssetType([0u8; 32])),
                public_key: PublicKey::default(AddressFormat::SECP256K1),
            }, // no effect
            amount_blinds: Default::default(), // no effect
            type_blind: Default::default(),    // no effect
        };
        let ArToAbarBody {
            input: _,
            output,
            proof,
            memo,
        } = gen_ar_to_abar_body(prng, params, &obar, target_address)?;

        Ok(EvmToAbarNote {
            output,
            proof,
            memo,
        })
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        bincode::serialize(&self).unwrap_or(vec![])
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<EvmToAbarNote> {
        bincode::deserialize(bytes).c(d!())
    }

    pub fn returns(&self) -> Vec<u8> {
        let mut bytes = vec![];
        bytes.extend(self.output.commitment.noah_to_bytes());
        bytes.extend(self.memo.0.clone());
        bytes
    }
}

/// The AbarToEvmNote
pub struct AbarToEvmNote {}
