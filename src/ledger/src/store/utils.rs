//!
//! # Test only
//!

use {
    super::helpers,
    crate::{
        data_model::{
            AssetRules, AssetTypeCode, IssueAsset, IssueAssetBody, IssuerKeyPair, Memo,
            Operation, Transaction, TxOutput, ASSET_TYPE_FRA, FRA_DECIMALS,
        },
        staking::FRA_PRE_ISSUE_AMOUNT,
    },
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    zei::xfr::{
        asset_record::{build_blind_asset_record, AssetRecordType},
        sig::XfrKeyPair,
        structs::AssetRecordTemplate,
    },
    zei_crypto::basic::ristretto_pedersen_comm::RistrettoPedersenCommitment,
};

/// Define and Issue FRA.
/// Currently this should only be used for tests.
pub fn fra_gen_initial_tx(fra_owner_kp: &XfrKeyPair) -> Transaction {
    /*
     * Define FRA
     **/

    let fra_code = AssetTypeCode {
        val: ASSET_TYPE_FRA,
    };

    let (mut tx, _) = pnk!(helpers::create_definition_transaction(
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

    let pc_gens = RistrettoPedersenCommitment::default();
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
