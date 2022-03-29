use crate::rust::*;

/// Create a default set of asset rules. See class description for defaults.
#[no_mangle]
pub extern "C" fn findora_ffi_asset_rules_new() -> *mut AssetRules {
    Box::into_raw(Box::new(AssetRules::new()))
}

/// Adds an asset tracing policy.
/// @param {TracingPolicy} policy - Tracing policy for the new asset.
#[no_mangle]
pub extern "C" fn findora_ffi_asset_rules_add_tracing_policy(
    ar: &AssetRules,
    policy: &TracingPolicy,
) -> *mut AssetRules {
    Box::into_raw(Box::new(ar.clone().add_tracing_policy(policy)))
}

/// Set a cap on the number of units of this asset that can be issued.
/// @param {BigInt} max_units - Maximum number of units that can be issued.
#[no_mangle]
pub extern "C" fn findora_ffi_asset_rules_set_max_units(
    ar: &AssetRules,
    max_units: u64,
) -> *mut AssetRules {
    Box::into_raw(Box::new(ar.clone().set_max_units(max_units)))
}

/// Transferability toggle. Assets that are not transferable can only be transferred by the asset
/// issuer.
/// @param {boolean} transferable - Boolean indicating whether asset can be transferred.
#[no_mangle]
pub extern "C" fn findora_ffi_asset_rules_set_transferable(
    ar: &AssetRules,
    transferable: bool,
) -> *mut AssetRules {
    Box::into_raw(Box::new(ar.clone().set_transferable(transferable)))
}

/// The updatable flag determines whether the asset memo can be updated after issuance.
/// @param {boolean} updatable - Boolean indicating whether asset memo can be updated.
/// @see {@link module:Findora-Wasm~TransactionBuilder#add_operation_update_memo|add_operation_update_memo} for more information about how to add
/// a memo update operation to a transaction.
#[no_mangle]
pub extern "C" fn findora_ffi_asset_rules_set_updatable(
    ar: &AssetRules,
    updatable: bool,
) -> *mut AssetRules {
    Box::into_raw(Box::new(ar.clone().set_updatable(updatable)))
}

/// Co-signature rules. Assets with co-signatue rules require additional weighted signatures to
/// be transferred.
/// @param {SignatureRules} multisig_rules - Co-signature restrictions.
#[no_mangle]
pub extern "C" fn findora_ffi_asset_rules_set_transfer_multisig_rules(
    ar: &AssetRules,
    multisig_rules: &SignatureRules,
) -> *mut AssetRules {
    Box::into_raw(Box::new(
        ar.clone()
            .set_transfer_multisig_rules(multisig_rules.clone()),
    ))
}

/// Set the decimal number of asset. Return error string if failed, otherwise return changed asset.
/// #param {Number} decimals - The number of decimals used to set its user representation.
/// Decimals should be 0 ~ 255.
#[no_mangle]
pub extern "C" fn findora_ffi_asset_rules_set_decimals(
    ar: &AssetRules,
    decimals: u8,
) -> *mut AssetRules {
    if let Ok(rules) = ar.clone().set_decimals(decimals) {
        Box::into_raw(Box::new(rules))
    } else {
        std::ptr::null_mut()
    }
}
