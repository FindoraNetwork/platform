//!
//! # Mint FRA
//!
//! A more standard CoinBase implementation.
//!

use {
    crate::staking::BlockHeight,
    crate::{
        data_model::TxOutput,
        staking::{Amount, FRA},
    },
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    serde::{Deserialize, Serialize},
    zei::{
        setup::PublicParams,
        xfr::{
            asset_record::{build_blind_asset_record, AssetRecordType},
            sig::XfrPublicKey,
            structs::{AssetRecordTemplate, AssetType as ZeiAssetType, OwnerMemo},
        },
    },
};

/// 420 million FRAs
pub const MINT_AMOUNT_LIMIT: Amount = 420 * 100_0000 * FRA;

#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct MintFraOps {
    pub height: BlockHeight,
    pub entries: Vec<MintEntry>,
}

impl MintFraOps {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(height: BlockHeight, entries: Vec<MintEntry>) -> Self {
        MintFraOps { height, entries }
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_related_pubkeys(&self) -> Vec<XfrPublicKey> {
        self.entries.iter().map(|e| e.target_pk).collect()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn get_owner_memos_ref(&self) -> Vec<Option<&OwnerMemo>> {
        self.entries.iter().map(|e| e.owner_memo.as_ref()).collect()
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct MintEntry {
    pub kind: MintKind,
    pub target_pk: XfrPublicKey,
    pub amount: Amount,
    pub utxo: TxOutput,
    pub asset_type: ZeiAssetType,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner_memo: Option<OwnerMemo>,
}

impl MintEntry {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(
        kind: MintKind,
        target_pk: XfrPublicKey,
        receiver_pk: Option<XfrPublicKey>,
        amount: Amount,
        asset_type: ZeiAssetType,
        confidential_am: bool,
        confidential_ty: bool,
    ) -> Self {
        let mut prng = ChaChaRng::seed_from_u64(0);
        let art = match (confidential_am, confidential_ty) {
            (true, true) => AssetRecordType::ConfidentialAmount_ConfidentialAssetType,
            (true, false) => {
                AssetRecordType::ConfidentialAmount_NonConfidentialAssetType
            }
            (false, true) => {
                AssetRecordType::NonConfidentialAmount_ConfidentialAssetType
            }
            _ => AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        };

        let ar = AssetRecordTemplate::with_no_asset_tracing(
            amount,
            asset_type,
            art,
            receiver_pk.unwrap_or(target_pk),
        );
        let pc_gens = PublicParams::default().pc_gens;
        let (ba, _, owner_memo) =
            build_blind_asset_record(&mut prng, &pc_gens, &ar, vec![]);

        let utxo = TxOutput {
            id: None,
            record: ba,
            lien: None,
        };

        MintEntry {
            kind,
            target_pk,
            amount,
            utxo,
            asset_type,
            owner_memo,
        }
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum MintKind {
    Claim,
    UnStake,
    Erc20ToAsset,
    Other,
}
