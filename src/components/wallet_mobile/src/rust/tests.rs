use super::*;

#[test]
pub fn test() {
    let kp = new_keypair();
    let b64 = public_key_to_base64(kp.get_pk_ref());
    let pk = rs_public_key_from_base64(&b64).unwrap();
    dbg!(pk);
}

#[test]
fn t_keypair_conversion() {
    let kp = new_keypair();
    let b64 = public_key_to_base64(kp.get_pk_ref());
    let be32 = public_key_to_bech32(kp.get_pk_ref());
    rs_public_key_from_base64(&b64).unwrap();
    rs_public_key_from_bech32(&be32).unwrap();
}

#[test]
fn t_keypair_encryption() {
    let key_pair = "hello world".to_string();
    let password = "12345".to_string();
    let enc = encryption_pbkdf2_aes256gcm(key_pair.clone(), password.clone());
    let dec_key_pair = decryption_pbkdf2_aes256gcm(enc, password);
    assert_eq!(key_pair, dec_key_pair);
}

#[test]
fn t_create_keypair_from_secret() {
    let kp = new_keypair();
    let sk_str = serde_json::to_string(&kp.get_sk()).unwrap();
    let kp1 = create_keypair_from_secret(sk_str).unwrap();
    let kp_str = serde_json::to_string(&kp).unwrap();
    let kp1_str = serde_json::to_string(&kp1).unwrap();
    assert_eq!(kp_str, kp1_str);
}

#[test]
fn test_asset_rules_to_str() {
    let mut ar = AssetRules {
        rules: Default::default(),
    };
    ar.rules.max_units = Some(10000000000_u64);
    let actual_serialized_json = serde_json::to_string(&ar.rules).unwrap();
    let expected_serialized_json = r#"{"transferable":true,"updatable":false,"transfer_multisig_rules":null,"max_units":"10000000000","decimals":6}"#.to_string();
    assert_eq!(actual_serialized_json, expected_serialized_json);
}

#[test]
fn test_asset_rules_from_str() {
    use ledger::data_model::AssetRules as PlatformAssetRules;
    let mut ar = AssetRules {
        rules: Default::default(),
    };
    let amt = 10000000000_u64;
    ar.rules.max_units = Some(amt);
    let actual_serialized_json = serde_json::to_string(&ar.rules).unwrap();
    let expected_serialized_json = r#"{"transferable":true,"updatable":false,"transfer_multisig_rules":null,"max_units":"10000000000","decimals":6}"#.to_string();
    assert_eq!(actual_serialized_json, expected_serialized_json);

    let res: PlatformAssetRules =
        serde_json::from_str::<PlatformAssetRules>(&expected_serialized_json).unwrap();
    assert_eq!(res.max_units.unwrap(), amt);
}

#[test]
fn test_asset_rules_from_str_null_max_units() {
    use ledger::data_model::AssetRules as PlatformAssetRules;
    let mut ar = AssetRules {
        rules: Default::default(),
    };
    let amt = 10000000000_u64;
    ar.rules.max_units = Some(amt);
    let actual_serialized_json = r#"{"transferable":true,"updatable":false,"transfer_multisig_rules":null,"max_units":null,"decimals":6}"#.to_string();

    let res: PlatformAssetRules =
        serde_json::from_str::<PlatformAssetRules>(&actual_serialized_json).unwrap();
    assert_eq!(res.max_units, None);
}

#[test]
fn test_asset_rules_from_str_empty_str_max_units() {
    use ledger::data_model::AssetRules as PlatformAssetRules;
    let mut ar = AssetRules {
        rules: Default::default(),
    };
    let amt = 10000000000_u64;
    ar.rules.max_units = Some(amt);
    let actual_serialized_json = r#"{"transferable":true,"updatable":false,"transfer_multisig_rules":null,"max_units":"","decimals":6}"#.to_string();

    let res: PlatformAssetRules =
        serde_json::from_str::<PlatformAssetRules>(&actual_serialized_json).unwrap();
    assert_eq!(res.max_units, None);
}
