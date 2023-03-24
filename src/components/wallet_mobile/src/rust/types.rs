use credentials::{
    CredIssuerPublicKey as PlatformCredIssuerPublicKey,
    CredIssuerSecretKey as PlatformCredIssuerSecretKey,
    CredUserPublicKey as PlatformCredUserPublicKey,
    CredUserSecretKey as PlatformCredUserSecretKey,
};
use std::ops::{Deref, DerefMut};
use zei::noah_api::keys::{KeyPair as NoahXfrKeyPair, PublicKey as NoahXfrPublicKey};
use zei::noah_api::xfr::structs::OpenAssetRecord as NoahOpenAssetRecord;

////////////////////////////////////////////////////////////////////////////////

pub struct XfrPublicKey(NoahXfrPublicKey);

impl From<NoahXfrPublicKey> for XfrPublicKey {
    fn from(v: NoahXfrPublicKey) -> XfrPublicKey {
        XfrPublicKey(v)
    }
}

impl Deref for XfrPublicKey {
    type Target = NoahXfrPublicKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for XfrPublicKey {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct XfrKeyPair(NoahXfrKeyPair);

impl From<NoahXfrKeyPair> for XfrKeyPair {
    fn from(v: NoahXfrKeyPair) -> XfrKeyPair {
        XfrKeyPair(v)
    }
}

impl Deref for XfrKeyPair {
    type Target = NoahXfrKeyPair;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for XfrKeyPair {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct OpenAssetRecord(NoahOpenAssetRecord);

impl From<NoahOpenAssetRecord> for OpenAssetRecord {
    fn from(v: NoahOpenAssetRecord) -> OpenAssetRecord {
        OpenAssetRecord(v)
    }
}

impl Deref for OpenAssetRecord {
    type Target = NoahOpenAssetRecord;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for OpenAssetRecord {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct CredIssuerPublicKey(PlatformCredIssuerPublicKey);

impl From<PlatformCredIssuerPublicKey> for CredIssuerPublicKey {
    fn from(v: PlatformCredIssuerPublicKey) -> CredIssuerPublicKey {
        CredIssuerPublicKey(v)
    }
}

impl Deref for CredIssuerPublicKey {
    type Target = PlatformCredIssuerPublicKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CredIssuerPublicKey {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct CredUserPublicKey(PlatformCredUserPublicKey);

impl From<PlatformCredUserPublicKey> for CredUserPublicKey {
    fn from(v: PlatformCredUserPublicKey) -> CredUserPublicKey {
        CredUserPublicKey(v)
    }
}

impl Deref for CredUserPublicKey {
    type Target = PlatformCredUserPublicKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CredUserPublicKey {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct CredIssuerSecretKey(PlatformCredIssuerSecretKey);

impl From<PlatformCredIssuerSecretKey> for CredIssuerSecretKey {
    fn from(v: PlatformCredIssuerSecretKey) -> CredIssuerSecretKey {
        CredIssuerSecretKey(v)
    }
}

impl Deref for CredIssuerSecretKey {
    type Target = PlatformCredIssuerSecretKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CredIssuerSecretKey {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct CredUserSecretKey(PlatformCredUserSecretKey);

impl From<PlatformCredUserSecretKey> for CredUserSecretKey {
    fn from(v: PlatformCredUserSecretKey) -> CredUserSecretKey {
        CredUserSecretKey(v)
    }
}

impl Deref for CredUserSecretKey {
    type Target = PlatformCredUserSecretKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CredUserSecretKey {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
