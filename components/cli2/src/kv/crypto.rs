use argon2::{self, Config};
use blake2::crypto_mac::{Mac, NewMac};
use blake2::Blake2b;
use chacha20poly1305::aead::{Aead, NewAead};
use chacha20poly1305::{ChaCha20Poly1305, Nonce}; // Or `XChaCha20Poly1305`
use rand::{thread_rng, Rng};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use snafu::{ResultExt, Snafu};
use std::marker::PhantomData;
use zeroize::{Zeroize, Zeroizing};

#[derive(Snafu, Debug)]
pub enum CryptoError {
  #[snafu(display("Failed decrypting key. Check provided password."))]
  KeyDecryptionError,
  #[snafu(display("Failed decrypting ciphertext component."))]
  DecryptionError,
  #[snafu(display("HMAC Validation Failed"))]
  HMACValidation,
  #[snafu(display("Deserilization of {} component failed", component))]
  Deserialization {
    source: serde_json::error::Error,
    component: &'static str,
  },
}
type Result<T, E = CryptoError> = std::result::Result<T, E>;

#[derive(Zeroize)]
#[zeroize(drop)]
struct Key {
  pub encryption: [u8; 32],
  pub hmac: [u8; 32],
}

impl Key {
  pub fn new_from_password(password: &[u8]) -> (Key, [u8; 32]) {
    let mut salt = [0_u8; 32];
    thread_rng().fill(&mut salt);
    let key = Self::from_password(password, &salt[..]);
    (key, salt)
  }

  pub fn from_password(password: &[u8], salt: &[u8]) -> Key {
    let mut config = Config::default();
    config.hash_length = 64;

    let hash =
      Zeroizing::new(argon2::hash_raw(password, salt, &config).expect("Failure Hashing Password"));

    let mut key = Key { encryption: [0; 32],
                        hmac: [0; 32] };
    key.encryption.copy_from_slice(&hash[..32]);
    key.hmac.copy_from_slice(&hash[32..]);
    key
  }
}

/// Internal method for `Pair`
///
/// Provides the HMAC of two concatenated byte strings.
fn hmac_pair(clear: &[u8], encrypted: &[u8], nonce: &[u8], key: &Key) -> [u8; 64] {
  assert!(nonce.len() == 12);
  let mut hasher = Blake2b::new_varkey(&key.hmac[..]).unwrap();
  hasher.update(clear);
  hasher.update(encrypted);
  hasher.update(nonce);
  let result = hasher.finalize();
  let code_bytes = result.into_bytes();
  let mut ret = [0_u8; 64];
  ret.copy_from_slice(&code_bytes[..]);

  ret
}

#[derive(Serialize, Deserialize)]
pub struct MixedPair<Clear, Encrypted> {
  clear: String,
  clear_phantom: PhantomData<Clear>,
  encrypted: Vec<u8>,
  encrypted_phantom: PhantomData<Encrypted>,
  chacha_nonce: [u8; 12],
  salt: [u8; 32],
  /// Due to serde derive limitations, this needs to be a `Vec<u8>`, but it is
  /// effectively a `[u8;64]`.
  hmac: Vec<u8>,
}

impl<Clear, Encrypted> MixedPair<Clear, Encrypted>
  where Clear: Serialize + DeserializeOwned + 'static,
        Encrypted: Serialize + DeserializeOwned + 'static
{
  /// Verifies the data, using the user-provided password
  pub fn verify(&self, password: &[u8]) -> bool {
    let key = Key::from_password(password, &self.salt[..]);
    assert!(self.hmac.len() == 64);
    let mut hasher = Blake2b::new_varkey(&key.hmac[..]).unwrap();
    hasher.update(self.clear.as_bytes());
    hasher.update(&self.encrypted[..]);
    hasher.update(&self.chacha_nonce[..]);
    hasher.verify(&self.hmac[..]).is_ok()
  }

  /// Produces a new, packed, `Pair` with the provided data and user provided password.
  pub fn pack(clear: Clear, encrypted: &Encrypted, password: &[u8]) -> Self {
    let (key, salt) = Key::new_from_password(password);
    let clear = serde_json::to_string(&clear).expect("JSON serialization failed");
    let encrypted = serde_json::to_string(encrypted).expect("JSON serialization failed");
    let mut nonce = [0_u8; 12];
    thread_rng().fill(&mut nonce[..]);

    let encryption_key = chacha20poly1305::Key::from_slice(&key.encryption[..]);
    let cipher = ChaCha20Poly1305::new(encryption_key);
    let encrypted = cipher.encrypt(Nonce::from_slice(&nonce[..]), encrypted.as_bytes())
                          .expect("Encryption failed");

    let hmac = hmac_pair(clear.as_bytes(), &encrypted[..], &nonce[..], &key);

    MixedPair { clear,
                clear_phantom: PhantomData,
                encrypted,
                encrypted_phantom: PhantomData,
                chacha_nonce: nonce,
                salt,
                hmac: hmac.to_vec() }
  }

  /// Returns the clear-text component, without verifying the hmac
  pub fn clear_no_verify(&self) -> Result<Clear> {
    serde_json::from_str(&self.clear).context(Deserialization { component: "cleartext" })
  }

  /// Returns the clear-text component, while verifying the hmac
  pub fn clear(&self, password: &[u8]) -> Result<Clear> {
    if self.verify(password) {
      self.clear_no_verify()
    } else {
      Err(CryptoError::HMACValidation)
    }
  }

  /// Validates, decrypts, and returns the encrypted component
  pub fn encrypted(&self, password: &[u8]) -> Result<Encrypted> {
    if self.verify(password) {
      let key = Key::from_password(password, &self.salt[..]);
      let encryption_key = chacha20poly1305::Key::from_slice(&key.encryption[..]);
      let cipher = ChaCha20Poly1305::new(encryption_key);
      let plaintext = Zeroizing::new(cipher.decrypt(Nonce::from_slice(&self.chacha_nonce[..]),
                                                    &self.encrypted[..])
                                           .map_err(|_| CryptoError::DecryptionError)?);
      serde_json::from_slice(&plaintext[..]).context(Deserialization { component: "ciphertext" })
    } else {
      Err(CryptoError::HMACValidation)
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn pack_and_verify() {
    let pair = MixedPair::pack("Clear".to_string(),
                               &"Encrypted".to_string(),
                               "password".as_bytes());
    assert!(pair.verify("password".as_bytes()));
  }
  #[test]
  fn pack_unpack() -> Result<()> {
    let clear = "Clear".to_string();
    let encrypted = "Encrypted".to_string();
    let pair = MixedPair::pack(clear.clone(), &encrypted, "password".as_bytes());

    assert!(pair.clear("password".as_bytes())? == clear);
    assert!(pair.encrypted("password".as_bytes())? == encrypted);
    Ok(())
  }
}
