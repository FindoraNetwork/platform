//!
//! Generate mnemonic and restore keypair from it.
//!
//! Separating mnemonic to a standalone library is needed by tests.
//!

use {
    bech32::{self, FromBase32, ToBase32},
    bip0039::{Count, Language, Mnemonic},
    ed25519_dalek_bip32::{DerivationPath, ExtendedSecretKey},
    ruc::*,
    zei::{
        serialization::ZeiFromToBytes,
        xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey},
    },
};

/// Randomly generate a 12words-length mnemonic.
#[inline(always)]
pub fn generate_mnemonic_default() -> String {
    Mnemonic::generate_in(Language::English, Count::Words12).into_phrase()
}

/// Generate mnemonic with custom length and language.
/// - @param `wordslen`: acceptable value are one of [ 12, 15, 18, 21, 24 ]
/// - @param `lang`: acceptable value are one of [ "en", "zh", "zh_traditional", "fr", "it", "ko", "sp", "jp" ]
#[inline(always)]
pub fn generate_mnemonic_custom(wordslen: u8, lang: &str) -> Result<String> {
    let w = match wordslen {
        12 => Count::Words12,
        15 => Count::Words15,
        18 => Count::Words18,
        21 => Count::Words21,
        24 => Count::Words24,
        _ => {
            return Err(eg!(
                "Invalid words length, only 12/15/18/21/24 can be accepted."
            ));
        }
    };

    let l = check_lang(lang).c(d!())?;

    Ok(Mnemonic::generate_in(l, w).into_phrase())
}

// do the real restore operation.
macro_rules! restore_keypair_from_mnemonic {
    ($phrase: expr, $l: expr, $p: expr, $bip: tt) => {
        check_lang($l)
            .c(d!())
            .and_then(|l| Mnemonic::from_phrase_in(l, $phrase).map_err(|e| eg!(e)))
            .map(|m| m.to_seed(""))
            .and_then(|seed| {
                DerivationPath::$bip($p.coin, $p.account, $p.change, $p.address)
                    .map_err(|e| eg!(e))
                    .map(|dp| (seed, dp))
            })
            .and_then(|(seed, dp)| {
                ExtendedSecretKey::from_seed(&seed)
                    .map_err(|e| eg!(e))?
                    .derive(&dp)
                    .map_err(|e| eg!(e))
            })
            .and_then(|kp| {
                XfrSecretKey::zei_from_bytes(&kp.secret_key.to_bytes()[..])
                    .map_err(|e| eg!(e))
            })
            .map(|sk| sk.into_keypair())
    };
}

/// Use this struct to express a Bip44/Bip49 path.
pub struct BipPath {
    coin: u32,
    account: u32,
    change: u32,
    address: u32,
}

impl BipPath {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn new(coin: u32, account: u32, change: u32, address: u32) -> Self {
        BipPath {
            coin,
            account,
            change,
            address,
        }
    }
}

/// Restore the XfrKeyPair from a mnemonic with a default bip44-path,
/// that is "m/44'/917'/0'/0/0" ("m/44'/coin'/account'/change/address").
#[inline(always)]
pub fn restore_keypair_from_mnemonic_default(phrase: &str) -> Result<XfrKeyPair> {
    const FRA: u32 = 917;
    restore_keypair_from_mnemonic!(phrase, "en", BipPath::new(FRA, 0, 0, 0), bip44)
        .c(d!())
}

/// Restore the XfrKeyPair from secret key,
#[inline(always)]
pub fn restore_keypair_from_seckey_base64(sec: &str) -> Result<XfrKeyPair> {
    base64::decode_config(sec, base64::URL_SAFE)
        .c(d!())
        .and_then(|sec| {
            XfrSecretKey::zei_from_bytes(&sec)
                .c(d!())
                .map(|sec| sec.into_keypair())
        })
}

/// Restore the XfrKeyPair from a mnemonic with custom params,
/// in bip44 form.
#[inline(always)]
pub fn restore_keypair_from_mnemonic_bip44(
    phrase: &str,
    lang: &str,
    path: &BipPath,
) -> Result<XfrKeyPair> {
    restore_keypair_from_mnemonic_bip44_inner(phrase, lang, path).c(d!())
}

#[inline(always)]
fn restore_keypair_from_mnemonic_bip44_inner(
    phrase: &str,
    lang: &str,
    path: &BipPath,
) -> Result<XfrKeyPair> {
    restore_keypair_from_mnemonic!(phrase, lang, path, bip44).c(d!())
}

/// Restore the XfrKeyPair from a mnemonic with custom params,
/// in bip49 form.
#[inline(always)]
pub fn restore_keypair_from_mnemonic_bip49(
    phrase: &str,
    lang: &str,
    path: &BipPath,
) -> Result<XfrKeyPair> {
    restore_keypair_from_mnemonic_bip49_inner(phrase, lang, path).c(d!())
}

#[inline(always)]
fn restore_keypair_from_mnemonic_bip49_inner(
    phrase: &str,
    lang: &str,
    path: &BipPath,
) -> Result<XfrKeyPair> {
    restore_keypair_from_mnemonic!(phrase, lang, path, bip49).c(d!())
}

// check and generate a Language object from its string value.
#[inline(always)]
fn check_lang(lang: &str) -> Result<Language> {
    match lang {
        "en" => Ok(Language::English),
        "zh" => Ok(Language::SimplifiedChinese),
        "zh_traditional" => Ok(Language::TraditionalChinese),
        "fr" => Ok(Language::French),
        "it" => Ok(Language::Italian),
        "ko" => Ok(Language::Korean),
        "sp" => Ok(Language::Spanish),
        "jp" => Ok(Language::Japanese),
        _ => Err(eg!("Unsupported language")),
    }
}

/////////////////////////////////////////////////////////////////

/// Convert a XfrPublicKey to base64 human-readable address
#[inline(always)]
pub fn public_key_to_base64(key: &XfrPublicKey) -> String {
    base64::encode_config(ZeiFromToBytes::zei_to_bytes(key), base64::URL_SAFE)
}

/// Restore a XfrPublicKey from base64 human-readable address
#[inline(always)]
pub fn public_key_from_base64(pk: &str) -> Result<XfrPublicKey> {
    base64::decode_config(pk, base64::URL_SAFE)
        .c(d!())
        .and_then(|bytes| XfrPublicKey::zei_from_bytes(&bytes).c(d!()))
}

/// Convert a XfrPublicKey to bech32 human-readable address
#[inline(always)]
pub fn public_key_to_bech32(key: &XfrPublicKey) -> String {
    bech32enc(&XfrPublicKey::zei_to_bytes(key))
}

/// Restore a XfrPublicKey to bech32 human-readable address
#[inline(always)]
pub fn public_key_from_bech32(addr: &str) -> Result<XfrPublicKey> {
    bech32dec(addr)
        .c(d!())
        .and_then(|bytes| XfrPublicKey::zei_from_bytes(&bytes).c(d!()))
}

#[inline(always)]
fn bech32enc<T: AsRef<[u8]> + ToBase32>(input: &T) -> String {
    bech32::encode("fra", input.to_base32()).unwrap()
}

#[inline(always)]
fn bech32dec(input: &str) -> Result<Vec<u8>> {
    bech32::decode(input)
        .c(d!())
        .and_then(|(_, data)| Vec::<u8>::from_base32(&data).c(d!()))
}

/////////////////////////////////////////////////////////////////

#[cfg(test)]
#[allow(missing_docs)]
mod test {
    use {super::*, rand_core::SeedableRng};

    #[test]
    fn t_generate_mnemonic() {
        ["en", "zh", "zh_traditional", "fr", "it", "ko", "sp", "jp"]
            .iter()
            .for_each(|lang| {
                [12, 15, 18, 21, 24].iter().for_each(|wordslen| {
                    let phrase = generate_mnemonic_custom(*wordslen, lang).unwrap();
                    let path = BipPath {
                        coin: 917,
                        account: rand::random::<u32>() % 100,
                        change: rand::random::<u32>() % 100,
                        address: rand::random::<u32>() % 100,
                    };
                    assert_eq!(*wordslen as usize, phrase.split(' ').count());

                    pnk!(restore_keypair_from_mnemonic_bip44_inner(
                        &phrase, lang, &path
                    ));
                    pnk!(restore_keypair_from_mnemonic_bip49_inner(
                        &phrase, lang, &path
                    ));
                })
            });
    }

    #[test]
    fn t_generate_mnemonic_bad() {
        assert!(generate_mnemonic_custom(12, "xx").is_err());
        assert!(generate_mnemonic_custom(11, "zh").is_err());
        assert!(generate_mnemonic_custom(11, "xx").is_err());
    }

    fn new_keypair() -> XfrKeyPair {
        let mut small_rng = rand_chacha::ChaChaRng::from_entropy();
        XfrKeyPair::generate(&mut small_rng)
    }

    #[test]
    fn t_converts() {
        let pk = new_keypair().get_pk();
        assert_eq!(pk, pnk!(public_key_from_base64(&public_key_to_base64(&pk))));
        assert_eq!(pk, pnk!(public_key_from_bech32(&public_key_to_bech32(&pk))));
    }
}
