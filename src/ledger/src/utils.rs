//!
//! # Test only
//!

use noah::xfr::structs::AssetType;

use {
    crate::{
        data_model::{
            AssetRules, AssetTypeCode, AssetTypePrefix, DefineAsset, DefineAssetBody,
            IssueAsset, IssueAssetBody, IssuerKeyPair, IssuerPublicKey, Memo, Operation,
            Transaction, TxOutput, ASSET_TYPE_FRA, FRA_DECIMALS,
        },
        staking::FRA_PRE_ISSUE_AMOUNT,
    },
    fbnc::NumKey,
    fp_utils::hashing::keccak_256,
    noah::xfr::{
        asset_record::{build_blind_asset_record, AssetRecordType},
        sig::XfrKeyPair,
        structs::AssetRecordTemplate,
    },
    noah_crypto::basic::pedersen_comm::PedersenCommitmentRistretto,
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
};

/// Create a transaction to define a custom asset
pub fn create_definition_transaction(
    code: &AssetTypeCode,
    keypair: &XfrKeyPair,
    asset_rules: AssetRules,
    memo: Option<Memo>,
    seq_id: u64,
) -> Result<(Transaction, AssetTypeCode)> {
    let issuer_key = IssuerPublicKey {
        key: *keypair.get_pk_ref(),
    };
    let asset_body =
        DefineAssetBody::new(&code, &issuer_key, asset_rules, memo, None).c(d!())?;
    let asset_create =
        DefineAsset::new(asset_body, &IssuerKeyPair { keypair: &keypair }).c(d!())?;

    let code = if code.val == ASSET_TYPE_FRA {
        *code
    } else {
        let mut asset_code = AssetTypePrefix::UserDefined.bytes();
        asset_code.append(&mut code.to_bytes());
        AssetTypeCode {
            val: AssetType(keccak_256(&asset_code)),
        }
    };

    Ok((
        Transaction::from_operation(Operation::DefineAsset(asset_create), seq_id),
        code,
    ))
}

/// Define and Issue FRA.
/// Currently this should only be used for tests.
#[allow(unused)]
pub fn fra_gen_initial_tx(fra_owner_kp: &XfrKeyPair) -> Transaction {
    /*
     * Define FRA
     **/

    let fra_code = AssetTypeCode {
        val: ASSET_TYPE_FRA,
    };

    let (mut tx, _) = pnk!(create_definition_transaction(
        &fra_code,
        fra_owner_kp,
        AssetRules {
            max_units: Some(1000 + FRA_PRE_ISSUE_AMOUNT),
            decimals: FRA_DECIMALS,
            ..AssetRules::default()
        },
        Some(Memo("FRA".to_owned())),
        0,
    ));

    /*
     * Issue FRA
     **/

    let template = AssetRecordTemplate::with_no_asset_tracing(
        FRA_PRE_ISSUE_AMOUNT / 2,
        fra_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        fra_owner_kp.get_pk(),
    );

    let pc_gens = PedersenCommitmentRistretto::default();
    let outputs = (0..2)
        .map(|_| {
            let (ba, _, _) = build_blind_asset_record(
                &mut ChaChaRng::from_entropy(),
                &pc_gens,
                &template,
                vec![],
            );
            (
                TxOutput {
                    id: None,
                    record: ba,
                    lien: None,
                },
                None,
            )
        })
        .collect::<Vec<_>>();
    let asset_issuance_body = IssueAssetBody::new(&fra_code, 0, &outputs).unwrap();

    let asset_issuance_operation = IssueAsset::new(
        asset_issuance_body,
        &IssuerKeyPair {
            keypair: fra_owner_kp,
        },
    )
    .unwrap();

    tx.add_operation(Operation::IssueAsset(asset_issuance_operation));

    tx.sign_to_map(fra_owner_kp);

    tx
}
