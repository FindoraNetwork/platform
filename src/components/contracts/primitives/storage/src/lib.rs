#![deny(warnings)]
#![allow(missing_docs)]

pub mod hash;
pub mod types;

#[cfg(test)]
mod tests;

pub use parking_lot::RwLock;
pub use paste;
pub use ruc::{d, Result, RucResult};
pub use serde::{de::DeserializeOwned, Serialize};
pub use std::borrow::{Borrow, BorrowMut};
pub use std::ops::{Deref, DerefMut};
pub use std::sync::Arc;
pub use storage::store::traits::StatelessStore;

const DB_SEPARATOR: &'static str = "_";

/// An instance of a storage in a module.
pub trait StorageInstance {
    /// Prefix of a module to isolate it from other modules.
    fn module_prefix() -> &'static str;

    /// Prefix given to a storage to isolate from other storages in the module.
    const STORAGE_PREFIX: &'static str;
}

/// Generate a new type for [`fp_storage::types::StorageValue`],
/// [`fp_storage::types::StorageMap`] and [`fp_storage::types::StorageDoubleMap`].
///
/// Useful for creating a *storage-like* struct for test and migrations.
///
///```
/// # use fp_storage::generate_storage;
///
/// // generate a storage value with type u32.
/// generate_storage!(Prefix, StorageValueName => Value<u32>);
///
/// // generate a storage map from String to u32
/// generate_storage!(
///    Prefix, StorageMapName => Map<String, u32>
/// );
///
/// // generate a double map from `(u32, u32)` to `Vec<u8>`
/// generate_storage!(
///    Prefix, StorageDoubleMapName => DoubleMap<
///        (u32, u32),
///        (u32, u32),
///        Vec<u8>
///    >
/// );
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! generate_storage {
    ($module:ident, $name:ident => Value<$value:ty>) => {
        $crate::paste::paste! {
            $crate::generate_storage!(@GENERATE_INSTANCE_STRUCT $module, $name);
            pub type $name = $crate::types::StorageValue<
                [<$name Instance>],
                $crate::hash::Sha256,
                $value,
            >;
        }
    };
    ($module:ident, $name:ident<$t:ident : $bounds:tt> => Value<$value:ty>) => {
        $crate::paste::paste! {
            $crate::generate_storage!(@GENERATE_INSTANCE_STRUCT $module, $name);
            #[allow(type_alias_bounds)]
            pub type $name<$t : $bounds> = $crate::types::StorageValue<
                [<$name Instance>],
                $crate::hash::Sha256,
                $value,
            >;
        }
    };
    ($module:ident, $name:ident => Map<$key:ty, $value:ty>) => {
        $crate::paste::paste! {
            $crate::generate_storage!(@GENERATE_INSTANCE_STRUCT $module, $name);
            pub type $name = $crate::types::StorageMap<
                [<$name Instance>],
                $crate::hash::Sha256,
                $key,
                $value,
            >;
        }
    };
    ($module:ident, $name:ident<$t:ident : $bounds:tt> => Map<$key:ty, $value:ty>) => {
        $crate::paste::paste! {
            $crate::generate_storage!(@GENERATE_INSTANCE_STRUCT $module, $name);
            #[allow(type_alias_bounds)]
            pub type $name<$t : $bounds> = $crate::types::StorageMap<
                [<$name Instance>],
                $crate::hash::Sha256,
                $key,
                $value,
            >;
        }
    };
    ($module:ident, $name:ident => DoubleMap<$key1:ty, $key2:ty, $value:ty>) => {
        $crate::paste::paste! {
            $crate::generate_storage!(@GENERATE_INSTANCE_STRUCT $module, $name);
            pub type $name = $crate::types::StorageDoubleMap<
                [<$name Instance>],
                $crate::hash::Sha256,
                $key1,
                $key2,
                $value,
            >;
        }
    };
    (
        $module:ident,
        $name:ident<$t:ident : $bounds:tt>
        => DoubleMap<$key1:ty, $key2:ty, $value:ty>) => {
        $crate::paste::paste! {
            $crate::generate_storage!(@GENERATE_INSTANCE_STRUCT $module, $name);
            #[allow(type_alias_bounds)]
            pub type $name<$t : $bounds> = $crate::types::StorageDoubleMap<
                [<$name Instance>],
                $crate::hash::Sha256,
                $key1,
                $key2,
                $value,
            >;
        }
    };

    // helper used in all arms.
    (@GENERATE_INSTANCE_STRUCT $module:ident, $name:ident) => {
        $crate::paste::paste! {
            pub struct [<$name Instance>];
            impl $crate::StorageInstance for [<$name Instance>] {
                fn module_prefix() -> &'static str { stringify!($module) }
                const STORAGE_PREFIX: &'static str = stringify!($name);
            }

            impl $crate::StatelessStore for [<$name Instance>] {}
        }
    };
}
