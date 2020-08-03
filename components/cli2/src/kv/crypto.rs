use super::HasTable;
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
pub struct Key {
  pub encryption: [u8; 32],
  pub hmac: [u8; 32],
}

impl Key {
  pub fn random() -> Self {
    let mut encryption = [0_u8; 32];
    thread_rng().fill(&mut encryption);
    let mut hmac = [0_u8; 32];
    thread_rng().fill(&mut hmac);

    Key { encryption, hmac }
  }

  pub fn encrypt(&self, password: &[u8]) -> EncryptedKey {
    let mut salt = [0_u8; 32];
    thread_rng().fill(&mut salt);
    let mut nonce = [0_u8; 12];
    thread_rng().fill(&mut nonce);

    let config = Config::default();
    let hash = argon2::hash_raw(password, &salt[..], &config).expect("Failure Hashing Password");

    let mut blob = Zeroizing::new([0_u8; 64]);
    blob[..32].copy_from_slice(&self.encryption[..]);
    blob[32..].copy_from_slice(&self.hmac[..]);

    let key = chacha20poly1305::Key::from_slice(&hash[..]);
    let cipher = ChaCha20Poly1305::new(key);
    let ciphertext = cipher.encrypt(Nonce::from_slice(&nonce[..]), &blob[..])
                           .expect("Encryption failure");

    EncryptedKey { salt,
                   nonce,
                   blob: ciphertext }
  }
}

#[derive(Ord, PartialOrd, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default)]
pub struct KeyName(pub String);

impl HasTable for EncryptedKey {
  const TABLE_NAME: &'static str = "encryption_keys";
  type Key = KeyName;
}

#[derive(Serialize, Deserialize)]
pub struct EncryptedKey {
  salt: [u8; 32],
  nonce: [u8; 12],
  blob: Vec<u8>,
}

impl EncryptedKey {
  pub fn decrypt(&self, password: &[u8]) -> Result<Key> {
    let config = Config::default();
    let hash =
      argon2::hash_raw(password, &self.salt[..], &config).expect("Failure Hashing Password");

    let key = chacha20poly1305::Key::from_slice(&hash[..]);
    let cipher = ChaCha20Poly1305::new(key);
    let plaintext = Zeroizing::new(cipher.decrypt(Nonce::from_slice(&self.nonce[..]),
                                                  &self.blob[..])
                                         .map_err(|_| CryptoError::KeyDecryptionError)?);
    let mut key = Key { encryption: [0_u8; 32],
                        hmac: [0_u8; 32] };
    key.encryption.copy_from_slice(&plaintext[..32]);
    key.hmac.copy_from_slice(&plaintext[32..]);

    Ok(key)
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
pub struct Pair<Clear, Encrypted> {
  clear: String,
  clear_phantom: PhantomData<Clear>,
  encrypted: Vec<u8>,
  encrypted_phantom: PhantomData<Encrypted>,
  nonce: [u8; 12],
  /// Due to serde derive limitations, this needs to be a `Vec<u8>`, but it is
  /// effectively a `[u8;64]`.
  hmac: Vec<u8>,
}

impl<Clear, Encrypted> Pair<Clear, Encrypted>
  where Clear: Serialize + DeserializeOwned + 'static,
        Encrypted: Serialize + DeserializeOwned + 'static
{
  /// Verifies the data, using the provided key
  pub fn verify(&self, key: &Key) -> bool {
    assert!(self.hmac.len() == 64);
    let mut hasher = Blake2b::new_varkey(&key.hmac[..]).unwrap();
    hasher.update(self.clear.as_bytes());
    hasher.update(&self.encrypted[..]);
    hasher.update(&self.nonce[..]);
    hasher.verify(&self.hmac[..]).is_ok()
  }

  /// Produces a new, packed, `Pair` with the provided data and key.
  pub fn pack(clear: Clear, encrypted: Encrypted, key: &Key) -> Self {
    let clear = serde_json::to_string(&clear).expect("JSON serialization failed");
    let encrypted = serde_json::to_string(&encrypted).expect("JSON serialization failed");
    let mut nonce = [0_u8; 12];
    thread_rng().fill(&mut nonce[..]);

    let encryption_key = chacha20poly1305::Key::from_slice(&key.encryption[..]);
    let cipher = ChaCha20Poly1305::new(encryption_key);
    let encrypted = cipher.encrypt(Nonce::from_slice(&nonce[..]), encrypted.as_bytes())
                          .expect("Encryption failed");

    let hmac = hmac_pair(clear.as_bytes(), &encrypted[..], &nonce[..], key);

    Pair { clear,
           clear_phantom: PhantomData,
           encrypted,
           encrypted_phantom: PhantomData,
           nonce,
           hmac: hmac.to_vec() }
  }

  /// Returns the clear-text component, without verifying the hmac
  pub fn clear_no_verify(&self) -> Result<Clear> {
    serde_json::from_str(&self.clear).context(Deserialization { component: "cleartext" })
  }

  /// Returns the clear-text component, while verifying the hmac
  pub fn clear(&self, key: &Key) -> Result<Clear> {
    if self.verify(key) {
      self.clear_no_verify()
    } else {
      Err(CryptoError::HMACValidation)
    }
  }

  /// Validates, decrypts, and returns the encrypted component
  pub fn encrypted(&self, key: &Key) -> Result<Encrypted> {
    if self.verify(key) {
      let encryption_key = chacha20poly1305::Key::from_slice(&key.encryption[..]);
      let cipher = ChaCha20Poly1305::new(encryption_key);
      let plaintext = Zeroizing::new(cipher.decrypt(Nonce::from_slice(&self.nonce[..]),
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
  fn encrypt_decrypt_key() -> Result<()> {
    let key = Key::random();
    let encrypted_key = key.encrypt(b"A Secure Password");
    let decrypted_key = encrypted_key.decrypt(b"A Secure Password")?;

    assert_eq!(key.encryption, decrypted_key.encryption);
    assert_eq!(key.hmac, decrypted_key.hmac);
    Ok(())
  }
  #[test]
  fn pack_and_verify() {
    let key = Key::random();
    let pair = Pair::pack("Clear".to_string(), "Encrypted".to_string(), &key);
    assert!(pair.verify(&key))
  }
  #[test]
  fn pack_unpack() -> Result<()> {
    let key = Key::random();
    let clear = "Clear".to_string();
    let encrypted = "Encrypted".to_string();
    let pair = Pair::pack(clear.clone(), encrypted.clone(), &key);

    assert!(pair.clear(&key)? == clear);
    assert!(pair.encrypted(&key)? == encrypted);
    Ok(())
  }
}
