#![cfg(test)]
#![allow(missing_docs)]
use {
    super::*,
    curve25519_dalek::ristretto::CompressedRistretto,
    rand_core::SeedableRng,
    std::cmp::min,
    zei::{
        ristretto,
        xfr::structs::{AssetTypeAndAmountProof, XfrBody, XfrProofs},
    },
    zeiutils::msg_eq,
};

const UTF8_ASSET_TYPES_WORK: bool = false;

// This test may fail as it is a statistical test that sometimes fails (but very rarely)
// It uses the central limit theorem, but essentially testing the rand crate
#[test]
fn test_gen_random_with_rng() {
    let mut sum: u64 = 0;
    let mut sample_size = 0;

    let mut rng = rand::thread_rng();
    for _ in 0..1000 {
        let code = AssetTypeCode::gen_random_with_rng(&mut rng);
        let mut failed = true;

        for byte in code.val.0.iter() {
            if *byte != 0 {
                failed = false;
            }

            sum += *byte as u64;
            sample_size += 1;
        }

        assert!(!failed);
    }

    // Use the central limit theorem. The standard deviation of the
    // sample mean should be normal(127.5, uniform variance). Work
    // from the standard deviation of uniform(0, 1), sqrt(1/12). The
    // expected average (mu) is 127.5 if the random number generator
    // is unbiased.
    let uniform_stddev = 1.0 / (12.0f64).sqrt();
    let average = sum as f64 / sample_size as f64;
    let stddev = (uniform_stddev * 255.0) / (sample_size as f64).sqrt();
    println!("Average {average}, stddev {stddev}");
    assert!(average > 127.5 - 5.0 * stddev);
    assert!(average < 127.5 + 5.0 * stddev);
}

#[test]
// Test that an error is returned if the asset code is greater than 32 byts and a safe conversion is chosen
fn test_base64_from_to_utf8_safe() {
    if UTF8_ASSET_TYPES_WORK {
        let code = "My 资产 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$";
        let result = AssetTypeCode::new_from_utf8_safe(code);
        match result {
            Err(e) => {
                msg_eq!("...", e);
            }
            _ => panic!("InputsError expected."),
        }
    }
}

#[test]
// Test that a customized asset code can be converted to and from base 64 correctly
fn test_base64_from_to_utf8_truncate() {
    if UTF8_ASSET_TYPES_WORK {
        let customized_code = "❤️💰 My 资产 $";
        let code = AssetTypeCode::new_from_utf8_truncate(customized_code);
        let utf8 = AssetTypeCode::to_utf8(code).unwrap();
        assert_eq!(utf8, customized_code);
    }
}

#[test]
// Test that a customized asset code is truncated correctly if the lenght is greater than 32
fn test_utf8_truncate() {
    if UTF8_ASSET_TYPES_WORK {
        let customized_code_short = "My 资产 $";
        let customized_code_32_bytes = "My 资产 $$$$$$$$$$$$$$$$$$$$$$";
        let customized_code_to_truncate = "My 资产 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$";

        let code_short = AssetTypeCode::new_from_utf8_truncate(customized_code_short);
        let code_32_bytes =
            AssetTypeCode::new_from_utf8_truncate(customized_code_32_bytes);
        let code_to_truncate =
            AssetTypeCode::new_from_utf8_truncate(customized_code_to_truncate);
        assert_ne!(code_short, code_32_bytes);
        assert_eq!(code_32_bytes, code_to_truncate);

        let utf8 = AssetTypeCode::to_utf8(code_32_bytes).unwrap();
        assert_eq!(utf8, customized_code_32_bytes);
    }
}

#[test]
fn test_new_from_str() {
    let value = "1";
    let mut input = "".to_string();

    for i in 0..64 {
        let code = AssetTypeCode::new_from_str(&input);
        let mut checked = 0;

        for j in 0..min(i, code.val.0.len()) {
            assert!(code.val.0[j] == value.as_bytes()[0]);
            checked += 1;
        }

        for j in i..code.val.0.len() {
            assert!(code.val.0[j] == 0);
            checked += 1;
        }

        assert!(checked == code.val.0.len());
        input += value;
    }
}

#[test]
fn test_new_from_base64() {
    let base64 = "ZGVmZ2hpamtsbW5vcHFycw==";
    let result = Code::new_from_base64(base64);

    assert_eq!(
        result.ok(),
        Some(Code {
            val: [
                100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
                114, 115
            ]
        })
    );
}

#[test]
fn test_code_to_base64() {
    let code = Code {
        val: [
            100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115,
        ],
    };
    assert_eq!(code.to_base64(), "ZGVmZ2hpamtsbW5vcHFycw==");
}

// Test Transaction::add_operation
// Below are not directly tested but called:
//   TransferAssetBody::new
//   IssueAssetBody::new
//   DefineAssetBody::new
//   TransferAsset::new
//   IssueAsset::new
//   DefineAsset::new
fn gen_sample_tx() -> Transaction {
    // Create values to be used to instantiate operations. Just make up a seq_id, since
    // it will never be sent to a real ledger
    let mut transaction: Transaction = Transaction::from_seq_id(666);

    let mut prng = rand_chacha::ChaChaRng::from_entropy();

    let keypair = XfrKeyPair::generate(&mut prng);

    let xfr_note = XfrBody {
        inputs: Vec::new(),
        outputs: Vec::new(),
        proofs: XfrProofs {
            asset_type_and_amount_proof: AssetTypeAndAmountProof::NoProof,
            asset_tracing_proof: Default::default(),
        },
        asset_tracing_memos: Vec::new(),
        owners_memos: Vec::new(),
    };

    let no_policies = TracingPolicies::new();

    let policies = XfrNotePolicies::new(
        vec![no_policies.clone()],
        vec![None],
        vec![no_policies],
        vec![None],
    );

    let asset_transfer_body = TransferAssetBody {
        inputs: Vec::new(),
        outputs: Vec::new(),
        policies,
        transfer: Box::new(xfr_note),
        lien_assignments: Vec::new(),
        transfer_type: TransferType::Standard,
    };

    let asset_transfer = {
        let mut ret = TransferAsset::new(asset_transfer_body).unwrap();
        ret.sign(&keypair);
        ret
    };

    let transfer_operation = Operation::TransferAsset(asset_transfer.clone());

    // Instantiate an IssueAsset operation
    let asset_issuance_body = IssueAssetBody {
        code: AssetTypeCode::gen_random(),
        seq_num: 0,
        num_outputs: 0,
        records: Vec::new(),
    };

    let asset_issuance =
        IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &keypair })
            .unwrap();

    let issuance_operation = Operation::IssueAsset(asset_issuance.clone());

    // Instantiate an DefineAsset operation
    let mut asset = Box::<Asset>::default();
    asset.code = AssetTypeCode::gen_random();

    let asset_creation = DefineAsset::new(
        DefineAssetBody { asset },
        &IssuerKeyPair { keypair: &keypair },
    )
    .unwrap();

    let creation_operation = Operation::DefineAsset(asset_creation.clone());

    // Add operations to the transaction
    transaction.add_operation(transfer_operation);
    transaction.add_operation(issuance_operation);
    transaction.add_operation(creation_operation);

    // Verify operatoins
    assert_eq!(transaction.body.operations.len(), 3);

    assert_eq!(
        transaction.body.operations.first(),
        Some(&Operation::TransferAsset(asset_transfer))
    );
    assert_eq!(
        transaction.body.operations.get(1),
        Some(&Operation::IssueAsset(asset_issuance))
    );
    assert_eq!(
        transaction.body.operations.get(2),
        Some(&Operation::DefineAsset(asset_creation))
    );

    transaction
}

#[test]
fn test_add_operation() {
    gen_sample_tx();
}

fn gen_fee_operation(
    amount: Option<u64>,
    asset_type: Option<ZeiAssetType>,
    dest_pubkey: XfrPublicKey,
) -> Operation {
    Operation::TransferAsset(TransferAsset {
        body: TransferAssetBody {
            inputs: Vec::new(),
            policies: XfrNotePolicies::default(),
            outputs: vec![TxOutput {
                id: None,
                record: BlindAssetRecord {
                    amount: amount.map(XfrAmount::NonConfidential).unwrap_or(
                        XfrAmount::Confidential((
                            ristretto::CompressedRistretto(CompressedRistretto([0; 32])),
                            ristretto::CompressedRistretto(CompressedRistretto([0; 32])),
                        )),
                    ),
                    asset_type: asset_type.map(XfrAssetType::NonConfidential).unwrap_or(
                        XfrAssetType::Confidential(ristretto::CompressedRistretto(
                            CompressedRistretto([0; 32]),
                        )),
                    ),
                    public_key: dest_pubkey,
                },
                lien: None,
                memo: None,
            }],
            lien_assignments: Vec::new(),
            transfer: Box::new(XfrBody {
                inputs: Vec::new(),
                outputs: Vec::new(),
                proofs: XfrProofs {
                    asset_type_and_amount_proof: AssetTypeAndAmountProof::NoProof,
                    asset_tracing_proof: Default::default(),
                },
                asset_tracing_memos: Vec::new(),
                owners_memos: Vec::new(),
            }),
            transfer_type: TransferType::Standard,
        },
        body_signatures: Vec::new(),
    })
}

#[test]
fn test_check_fee() {
    let mut tx = gen_sample_tx();
    assert!(!tx.check_fee());

    let invalid_confidential_type =
        gen_fee_operation(Some(TX_FEE_MIN), None, *BLACK_HOLE_PUBKEY);
    let invalid_confidential_amount = gen_fee_operation(
        None,
        Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
        *BLACK_HOLE_PUBKEY,
    );
    let invalid_nonconfidential_not_fra_code = gen_fee_operation(
        Some(TX_FEE_MIN),
        Some(ZeiAssetType([9; ASSET_TYPE_LENGTH])),
        *BLACK_HOLE_PUBKEY,
    );
    let invalid_nonconfidential_fee_too_little = gen_fee_operation(
        Some(TX_FEE_MIN - 1),
        Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
        *BLACK_HOLE_PUBKEY,
    );
    let invalid_destination_not_black_hole = gen_fee_operation(
        Some(TX_FEE_MIN),
        Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
        XfrPublicKey::zei_from_bytes(&[9; ed25519_dalek::PUBLIC_KEY_LENGTH][..])
            .unwrap(),
    );
    let valid = gen_fee_operation(
        Some(TX_FEE_MIN),
        Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
        *BLACK_HOLE_PUBKEY,
    );
    let valid2 = gen_fee_operation(
        Some(TX_FEE_MIN + 999),
        Some(ZeiAssetType([0; ASSET_TYPE_LENGTH])),
        *BLACK_HOLE_PUBKEY,
    );

    // tx.add_operation(invalid_confidential_type.clone());
    // assert!(!tx.check_fee());
    //
    // tx.add_operation(invalid_confidential_amount.clone());
    // assert!(!tx.check_fee());
    //
    // tx.add_operation(invalid_nonconfidential_not_fra_code.clone());
    // assert!(!tx.check_fee());

    // tx.add_operation(invalid_nonconfidential_fee_too_little.clone());
    // assert!(!tx.check_fee());

    // tx.add_operation(invalid_destination_not_black_hole.clone());
    // assert!(!tx.check_fee());

    tx.add_operation(valid);
    assert!(tx.check_fee());

    tx.add_operation(invalid_confidential_type);
    assert!(tx.check_fee());

    tx.add_operation(invalid_confidential_amount);
    assert!(tx.check_fee());

    tx.add_operation(valid2);
    assert!(tx.check_fee());

    tx.add_operation(invalid_nonconfidential_not_fra_code);
    assert!(tx.check_fee());

    tx.add_operation(invalid_nonconfidential_fee_too_little);
    assert!(tx.check_fee());

    tx.add_operation(invalid_destination_not_black_hole);
    assert!(tx.check_fee());
}
